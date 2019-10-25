#' Create Pressure Dataset
#'
#' @return data.frame showing the average conversion of winning opportunities when their shot is under pressure or not
#' @export
#' @import dplyr
#' @importFrom sqldf sqldf
#'
#' @examples create_pressure_effect_dataset()
create_pressure_effect_dataset <- function() {

  ## Identify all potential leg winning shots and calulate the opponents score. When the opponent is waiting on <= 60 points
  ## then the potential leg winning shot is a "pressure" situation (ie the player knows that they are likely to lose if the player shooting doesn't hit).

  ## Does this increased pressure change the success %age of the shooter?

  winning_opportunities <- throws %>%
    mutate(is_winning_opportunity = ifelse((score_before == 50) | (score_before <= 40 & score_before %% 2 == 0), 1, 0)) %>%
    filter(is_winning_opportunity == 1) %>%
    select(throw_id, player_id, leg_id, segment, ring_id, score_before, is_winning_opportunity)

  # Only players with 50 winning opportunities are included

  eligible_players <- winning_opportunities %>%
    group_by(player_id) %>%
    summarise(opportunities = sum(is_winning_opportunity)) %>%
    filter(opportunities >= 50)


  winning_opportunities_subset <- winning_opportunities[winning_opportunities$player_id %in% eligible_players$player_id,]

  ## For each winning opportunity, inner joins the darts that satisfy all of the conditions:
  ##
  ##      1) thrown in the relevant leg
  ##      2) thrown by the other player
  ##      3) throw no more than 3 darts before the winning opportunity dart
  ##
  ## One of these darts has to be the opponent's last visit


  potential_final_throws <- sqldf('SELECT  wo.throw_id AS shooting_player_throw_id
                                    ,wo.player_id AS shooting_player_id
                                    ,wo.is_winning_opportunity
                                    ,t.throw_id AS opponent_throw_id
                                    ,t.player_id AS opponent_player_id
                                    ,t.score_before - (t.segment*t.ring_id) AS opponent_waiting_on


                            FROM winning_opportunities_subset AS wo
                            INNER join throws AS t
                              ON (t.leg_id = wo.leg_id)
                              AND (t.player_id != wo.player_id)
                              AND (t.throw_id IN (wo.throw_id-1, wo.throw_id-2, wo.throw_id-3))
                              ')

  ## The greatest opponent_throw_id from the visit before the winning opportunity is the opponent's last dart thrown

  throw_ids <- winning_opportunities_subset %>%
    inner_join(potential_final_throws, by=c("throw_id"="shooting_player_throw_id")) %>%
    group_by(throw_id) %>%
    summarise(opponents_last_throw_id = max(opponent_throw_id))

  ## Now that we have the opponent_throw_did we can calculate the score that the opponent was sitting on for each 'winning_opporunity' for the throwing player
  ## If the opponent was sat on fewer than 60 points then the shot is marked as 'is_pressure'
  ## Average conversion of shots where is_pressure = 0 and is_pressure = 1 is calculated for each player

  pressure_df <- throw_ids %>%
    inner_join(throws, by=c("throw_id"="throw_id")) %>%
    inner_join(throws, by=c("opponents_last_throw_id"="throw_id")) %>%
    mutate(opponent_waiting_on = score_before.y - (segment.y * ring_id.y)) %>%
    mutate(is_winning_shot = ifelse((ring_id.x == 2 & score_before.x == (segment.x*2)) | (segment.x == 50 & score_before.x == 50), 1, 0)) %>%
    inner_join(players, by=c("player_id.x"="player_id")) %>%
    select(throw_id, name, is_winning_shot, opponent_waiting_on) %>%
    mutate(is_pressure = ifelse(opponent_waiting_on <= 60, 1, 0))

    #%>%
    #group_by(name, is_pressure) %>%
    #summarise(conversion = mean(is_winning_shot))


  assign("pressure_effect_dataset", pressure_df, envir = .GlobalEnv)

}
