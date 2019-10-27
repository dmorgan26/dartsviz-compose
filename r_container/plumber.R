
library(dplyr)
library(tidyr)
library(sqldf)
library(RPostgreSQL)


## Loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")


## Try to connect to postgres server every 5 seconds for a minute
## Allows time for postgres container to be set up and populated

for (i in 1:12) {
  
  connect_to_db <- tryCatch(
    {
      message("Attempting to reach postgres service")
      
      con <- dbConnect(drv, dbname = "dartsviz-db",
                       host = "db", port = 5432,
                       user = "postgres", password = "postgres")
      
    },
    error=function(cond) {
      message("Could not reach service, retrying every 5 seconds for one minute")
      message(paste(12 - i, "attempts remaining"))
      Sys.sleep(5)
      return(NA)
    },
    warning=function(cond) {
      
      return(NULL) 
    },
    finally={
      
    }
  )    
  
}


## Pull in the data required to generate the following stats for each player:

## Average number of darts required to win a leg
## Double conersion %
## Double conversion when pressure is on (opponent on a low score)
## Double conversion with no pressure

darts_to_win_leg_query <- dbGetQuery(con, "SELECT   t.visit_sequence 
                                                    , t.dart_sequence
                                                    , t.is_home
                                                    , l.leg_id
                                                    , p.name

                                      FROM throws AS t
                                      
                                      INNER JOIN players AS p
                                      ON t.player_id = p.player_id
                                      
                                      INNER JOIN legs AS l
                                      ON l.leg_id = t.leg_id
                                      
                                      WHERE ((t.ring_id = 2 AND t.score_before = t.segment * 2) OR (t.segment = 50 AND t.score_before = 50))
                                     ")

darts_to_win_leg <- darts_to_win_leg_query %>%
  mutate(darts_thrown = ifelse(visit_sequence %% 2 > 0
                               , (((visit_sequence-1)/2)*3) + dart_sequence # If visit_sequence is odd, darts_thrown = total darts thrown in the leg divided by 2 plus dart_sequence of final visit
                               , ifelse(is_home == TRUE, ((((visit_sequence-1)/2)+0.5)*3) + dart_sequence #If visit_sequence is even, total_darts depends on who threw first
                                        , ((((visit_sequence-1)/2)-0.5)*3) + dart_sequence))) %>%
  
  group_by(name) %>%
  summarise(mean_darts_thrown = mean(darts_thrown))

####################

double_success_query <- dbGetQuery(con, "SELECT   t.player_id
                                                  , p.name
                                              
                                                  , SUM(CASE WHEN ((t.score_before = 50) 
                                                  	    OR (t.score_before <= 40 AND MOD(t.score_before, 2) =0)) THEN 1 ELSE 0 END 
                                                  	 ) 
                                                  	 AS winning_opportunities
                                                  	
                                                  , SUM (CASE WHEN ((t.ring_id = 2 AND t.score_before = t.segment * 2) 
                                                  	            OR (t.segment = 50 AND t.score_before = 50)) THEN 1	ELSE 0 END
                                                  	     ) 
                                                  	     AS winning_shots
                                              
                                              FROM throws AS t
                                              
                                              INNER JOIN legs AS l
                                              	ON t.leg_id = l.leg_id
                                              
                                              INNER JOIN players AS p
                                                ON p.player_id = t.player_id
                                                
                                              INNER JOIN completed_matches AS cm
                                                ON cm.match_id = l.match_id
                                              
                                               GROUP BY    t.player_id
                                                          , p.name")

double_success <-   double_success_query %>%
  mutate(conversion = (winning_shots/winning_opportunities)*100) %>%
  select(name, conversion)

##############

winning_opportunities <- dbGetQuery(con, "SELECT 	  throw_id
                                              	, player_id
                                              	, leg_id
                                              	, segment
                                              	, ring_id
                                              	, score_before
                                              
                                              FROM throws AS t
                                              
                                              WHERE (t.score_before = 50) OR (t.score_before <= 40 AND MOD(t.score_before, 2) = 0)
                                    ")

eligible_players <- dbGetQuery(con, "SELECT 	  player_id
                                              	
                                              	, SUM(CASE WHEN (t.score_before = 50) OR (t.score_before <= 40 AND MOD(t.score_before, 2) = 0) THEN 1 ELSE 0
                                              	 END) AS is_winning_opportunity
                                              
                                              FROM throws AS t
                                              
                                              GROUP BY player_id
                                              HAVING SUM(CASE WHEN (t.score_before = 50) OR (t.score_before <= 40 AND MOD(t.score_before, 2) = 0) THEN 1 ELSE 0
                                              	 END) >= 50
                                    ")

eligible_player_winning_opportunities <- winning_opportunities %>%
  inner_join(eligible_players, by="player_id") %>%
  select(throw_id, player_id, leg_id, segment, ring_id, score_before)

throws <- dbGetQuery(con, "SELECT * FROM throws")

potential_final_throws <- sqldf::sqldf('SELECT  wo.throw_id AS shooting_player_throw_id
                                    ,wo.player_id AS shooting_player_id
                                
                                    ,t.throw_id AS opponent_throw_id
                                    ,t.player_id AS opponent_player_id
                                    ,t.score_before - (t.segment*t.ring_id) AS opponent_waiting_on


                            FROM eligible_player_winning_opportunities AS wo
                            INNER join throws AS t
                              ON (t.leg_id = wo.leg_id)
                              AND (t.player_id != wo.player_id)
                              AND (t.throw_id IN (wo.throw_id-1, wo.throw_id-2, wo.throw_id-3))
                              ', drv ="SQLite")

## The greatest opponent_throw_id from the visit before the winning opportunity is the opponent's last dart thrown

throw_ids <- eligible_player_winning_opportunities %>%
  inner_join(potential_final_throws, by=c("throw_id"="shooting_player_throw_id")) %>%
  group_by(throw_id) %>%
  summarise(opponents_last_throw_id = max(opponent_throw_id))

## Now that we have the opponent_throw_ id we can calculate the score that the opponent was sitting on for each 'winning_opporunity' for the throwing player
## If the opponent was sat on fewer than 60 points then the shot is marked as 'is_pressure'
## Average conversion of shots where is_pressure = 0 and is_pressure = 1 is calculated for each player

players <- dbGetQuery(con, "SELECT * FROM players")

pressure_df <- throw_ids %>%
  inner_join(throws, by=c("throw_id"="throw_id")) %>%
  inner_join(throws, by=c("opponents_last_throw_id"="throw_id")) %>%
  mutate(opponent_waiting_on = score_before.y - (segment.y * ring_id.y)) %>%
  mutate(is_winning_shot = ifelse((ring_id.x == 2 & score_before.x == (segment.x*2)) | (segment.x == 50 & score_before.x == 50), 1, 0)) %>%
  inner_join(players, by=c("player_id.x"="player_id")) %>%
  select(throw_id, name, is_winning_shot, opponent_waiting_on) %>%
  mutate(is_pressure = ifelse(opponent_waiting_on <= 60, 1, 0)) %>%
  group_by(name, is_pressure) %>%
  summarise(conversion = mean(is_winning_shot)*100) %>%
  pivot_wider(names_from = is_pressure, values_from = conversion)

colnames(pressure_df) <- c("name","no_pressure_conversion","under_pressure_conversion")

######################################

## Create the final data frame ##

stats_df <- darts_to_win_leg %>%
  inner_join(double_success, by = "name") %>%
  inner_join(pressure_df, by = "name") %>%
  mutate(api_name = gsub(" ", "", tolower(name), fixed = TRUE))

stats_df$name <- trimws(stats_df$name, which="right")

#* @get /player_stats
#* @param player_name
get_player_stats <- function(player_name){
  
  #create the prediction data frame
  player_stats <- subset(stats_df, api_name == player_name)
  
  # create the prediction
  player_stats
}



