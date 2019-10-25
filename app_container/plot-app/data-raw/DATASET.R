## Load datasets ready to attach to package

legs <- readr::read_csv("C:/Users/Dan/Desktop/cleaned_darts_data/data/legs.csv")
completed_matches <- readr::read_csv("C:/Users/Dan/Desktop/cleaned_darts_data/data/matches.csv")
throws <- readr::read_csv("C:/Users/Dan/Desktop/cleaned_darts_data/data/throws.csv")
players <- readr::read_csv("C:/Users/Dan/Desktop/cleaned_darts_data/data/players.csv")


usethis::use_data(legs, overwrite = TRUE)
usethis::use_data(completed_matches, overwrite = TRUE)
usethis::use_data(throws, overwrite = TRUE)
usethis::use_data(players, overwrite = TRUE)


