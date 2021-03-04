#' A function to pull fivethirtyeight's global club rankings
#'
#' This function allows me to test setup
#' @param fte_rankings, defaults to 2017,2020
#' @keywords fivethirtyeight, fte, rankings
#' @export
#' @examples \dontrun{fte_rankings()}
#' @importFrom magrittr %>%
#' @export %>%

fte_rankings <- function(){
  rankings <- readr::read_csv("https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv")
  rankings <- rankings %>%
    dplyr::filter(league == 'Major League Soccer')
  return(rankings)
}
