#' A function to find fixture results
#'
#' @param fixtures, defaults are start=1996 and end=2021
#' @keywords fixtures, results, schedule
#' @importFrom magrittr "%>%"
#' @export
#' @examples \dontrun{fixtures()}
#' fixtures()

fixtures <- function(start_season=1996,end_season=2021){
  sea <- rMLS::seasons$season_id
  sea[is.na(sea)] <- ""
  total <- tibble::tibble()
  for(s in sea){
    URL <- paste0("https://fbref.com/en/comps/22/",s,"/schedule/Major-League-Soccer-Scores-and-Fixtures")
    html_doc <- URL %>% xml2::read_html()
    if (s == "11006") {table <- html_doc %>% rvest::html_nodes("#sched_11006_1")}
    else {table <- html_doc %>% rvest::html_nodes("#sched_all")}
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    total <- plyr::rbind.fill(total,df)
  }
  total <- total %>%
    dplyr::filter(Day != "Day") %>%
    dplyr::filter(Date != "")
  total$Date <- lubridate::ymd(total$Date)
  total$Score <- gsub("\\s*\\([^\\)]+\\)","",as.character(total$Score))
  total$Score <- stringr::str_trim(total$Score, side = c("both"))
  total$away_score <- substr(total$Score,3,3)
  total$home_score <- substr(total$Score,1,1)
  total$Attendance <- sapply(total$Attendance, gsub, pattern=",", replacement="")
  suppressWarnings({total$Attendance <- sapply(total$Attendance,as.numeric)})
  total$Match.Report <- NULL
  total <- total %>% dplyr::rename(home_xGoals = xG,away_xGoals = xG.1)
  start <- lubridate::ymd(start_season, truncated = 2L)
  end <- lubridate::ymd(end_season+1, truncated = 2L)
  total <- total %>%
    dplyr::filter(Date >= start) %>%
    dplyr::filter(Date <= end)
  closeAllConnections()
  return(total)
}
