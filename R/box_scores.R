#' A function to find box scores
#'
#' @param box_scores, no defaults
#' @keywords box, box_scores, scores
#' @importFrom magrittr "%>%"
#' @export
#' @examples \dontrun{box_scores()}
#' box_scores()


box_scores <- function(){
  URL <- "https://fbref.com/en/matches/5256f2a9/Columbus-Crew-New-York-City-FC-March-1-2020-Major-League-Soccer"
  URL <- "https://fbref.com/en/matches/b4151f72/Tampa-Bay-Mutiny-Columbus-Crew-September-18-1997-Major-League-Soccer"
  html_doc <- URL %>% xml2::read_html()
  table <- html_doc %>% rvest::html_nodes("table")
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  table <- table[[1]]
}
