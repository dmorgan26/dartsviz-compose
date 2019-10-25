#' Transform Dataset To Calculate Numer of Darts to Win a Leg
#'
#' @return data.frame showing the number of darts required to win each of the legs that a player won
#' @export
#' @import dplyr
#'
#' @examples create_darts_to_complete_leg_dataset()
create_darts_to_complete_leg_dataset <- function() {

  ## Transform dataset to show the winner and the number of darts throw by the winner for each leg_id
  ##
  ## A leg winning dart is defined as:
  ##
  ##            1) A successful double hit where (segment*2) == score_before (to exclude cases where player hits a double in error)
  ##            2) A bullseye hit (where segment == 50 and score_before == 50)
  ##

  darts_thrown_to_win_leg <- throws %>%
    filter((ring_id == 2 & score_before == (segment*2)) | segment == 50 & score_before == 50) %>%
    inner_join(players, by = c("player_id" = "player_id")) %>%
    inner_join(legs, by = c("leg_id" = "leg_id")) %>%
    inner_join(completed_matches, by = c("match_id" = "match_id")) %>%
    mutate(darts_thrown = ifelse(visit_sequence %% 2 > 0
                                 , (((visit_sequence-1)/2)*3) + dart_sequence # If visit_sequence is odd, darts_thrown = total darts thrown in the leg divided by 2 plus dart_sequence of final visit
                                 , ifelse(is_home == TRUE, ((((visit_sequence-1)/2)+0.5)*3) + dart_sequence #If visit_sequence is even, total_darts depends on who threw first
                                          , ((((visit_sequence-1)/2)-0.5)*3) + dart_sequence))) %>%

    select(leg_id, name, darts_thrown)


  assign("darts_to_win_leg_dataset", darts_thrown_to_win_leg, envir = .GlobalEnv)

}
