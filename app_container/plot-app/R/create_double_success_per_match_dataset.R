#' Transform Dataset To Calculate Conversion Percentage of Leg Winning Opportunities
#'
#' @return data.frame showing the conversion percentage of each player on match winning shots in each match
#' @export
#' @import dplyr
#'
#' @examples create_double_success_per_match_dataset()
create_double_success_per_match_dataset <- function() {

  ## For each player in each match, compute the proportion of times that they converted a leg winning opportunity

  double_conversion <- throws %>%
    inner_join(legs, by = c("leg_id" = "leg_id")) %>%
    mutate(is_winning_opportunity = ifelse((score_before == 50) | (score_before <= 40 & score_before %% 2 == 0), 1, 0)) %>%
    mutate(is_winning_shot = ifelse((ring_id == 2 & score_before == (segment*2)) | (segment == 50 & score_before == 50), 1, 0)) %>%
    group_by(match_id, player_id) %>%
    summarise(winning_opportunities = sum(is_winning_opportunity), winning_shots = sum(is_winning_shot)) %>%
    inner_join(players, by = c("player_id" = "player_id")) %>%
    inner_join(completed_matches, by = c("match_id" = "match_id")) %>%
    mutate(conversion = (winning_shots/winning_opportunities)*100) %>%
    select(match_id, name, conversion)

  double_conversion$conversion[is.na(double_conversion$conversion)] <- 0

  assign("double_conversion_dataset", double_conversion, envir = .GlobalEnv)

}
