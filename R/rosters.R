#' A function to find fixture results
#'
#' @param rosters, defaults are start=1996 and end=2021
#' @keywords rosters,roster,team
#' @importFrom magrittr "%>%"
#' @export
#' @examples \dontrun{rosters()}
#' rosters()

rosters <- function(start_season=1996,end_season=2021){
  total <- tibble::tibble()
  for(id in start_season:end_season){
    URL <- paste0("https://fbref.com/en/squads/529ba333/",id,"/")
    html_doc <- URL %>% xml2::read_html()
    table <- html_doc %>% rvest::html_nodes("table")
    table <- table[[1]]
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
    output$season <- id
    output$squad <- 'Columbus'
    total <- dplyr::bind_rows(total,output)
    closeAllConnections()
    print(id)
  }
  return(total)
}
