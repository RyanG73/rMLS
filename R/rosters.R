#' A function to find fixture results
#'
#' @param rosters, defaults are start=1996 and end=2021
#' @keywords rosters,roster,team
#' @importFrom magrittr "%>%"
#' @export
#' @examples \dontrun{rosters()}
#' rosters()

rosters <- function(start_season=1996,end_season=2020,teamid=NULL){
  total <- tibble::tibble()
  team_table <- rMLS::team_info %>% dplyr::filter(team_est_year <= 2020)
  team_table$team_est_year <- ifelse(team_table$team_est_year < start_season,start_season,team_table$team_est_year)
  if (!is.null(teamid)) {
    team_table <- team_table %>% filter(team_id == teamid)
  }
  for(i in 1:nrow(team_table)){
    id <- team_table[[i,3]]
    team <- team_table[[i,2]]
    start_season <- team_table[[i,17]]
    print(paste("Gathering results for",team))
    for(s in start_season:end_season){
      URL <- paste0("https://fbref.com/en/squads/",id,"/",s,"/")
      html_doc <- URL %>% xml2::read_html()
      table <- html_doc %>% rvest::html_nodes("table")
      try(table <- table[[1]],next)
      table1 <- table %>% rvest::html_table()
      df <- as.data.frame(table1)
      names(df) <- paste0(names(df), df[1, ])
      names(df) <- stringr::str_replace_all(names(df), "%", "pct")
      names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
      df <- df[-1,]
      pg <- xml2::read_html(URL)
      all <- rvest::html_attr(rvest::html_nodes(pg, "a"), "href")
      all <- tibble::as_tibble(all)
      all <- all %>%
        dplyr::filter(stringr::str_detect(value, '^/en/players/')) %>%
        dplyr::filter(nchar(value) >= 13) %>%
        dplyr::filter(stringr::str_detect(value, 'country',negate = T)) %>%
        dplyr::filter(stringr::str_detect(value, 'matchlogs',negate = T))
      output <- plyr::count(all[1:100,], vars = "value") %>%
        dplyr::mutate(player_name = substr(value,22,nchar(value))) %>%
        dplyr::mutate(player_name = stringr::str_replace_all(player_name, "-", " ")) %>%
        dplyr::mutate(player_id = substr(value,13,20))
      output$value <- NULL
      output$freq <- NULL
      output$season <- s
      output$squad <- team
      total <- dplyr::bind_rows(total,output)
      Sys.sleep(1)
    }
  }
  return(total)
}
