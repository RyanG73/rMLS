#' A function to pull fivethirtyeight's match list and probabilities
#'
#' This function allows me to test setup
#' @param fte_matches, defaults to 2017,2020
#' @keywords fivethirtyeight, fte, matches
#' @export
#' @examples \dontrun{fte_matches()}
#' @importFrom magrittr %>%
#' @export %>%

fte_matches <- function(start_season=2017,end_season=2020){
  matches <- readr::read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv")
  matches <- matches %>%
    dplyr::filter(league == 'Major League Soccer') %>%
    dplyr::filter(season >= start_season) %>%
    dplyr::filter(season <= end_season)
  return(matches)
}
