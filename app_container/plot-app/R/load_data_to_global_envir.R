#' Assign packaged datasets to global environment
#'
#' @return Four tibbles containing raw datasets for throws, legs, matches and players
#' @export
#'
#'
#'
#' @examples
#' load_data_to_global_envir()
load_data_to_global_envir <- function() {

  assign("throws", throws, envir = .GlobalEnv)
  assign("legs", legs, envir = .GlobalEnv)
  assign("completed_matches", completed_matches, envir = .GlobalEnv)
  assign("players", players, envir = .GlobalEnv)

}
