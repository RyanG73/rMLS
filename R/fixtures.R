#' A function to find fixture results
#'
#' @param fixtures, defaults are start=1996 and end=2021
#' @keywords fixtures, results, schedule
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' library(rMLS)
#' fixtures(start_season=2008,end_season=2020)

fixtures <- function(start_season=1996,end_season=2021){
  season <- rMLS::seasons %>% dplyr::filter(Season >= start_season) %>% dplyr::filter(Season <= end_season)
  sea <- season$season_id
  sea[is.na(sea)] <- ""
  total <- tibble::tibble()
  for(s in sea){
    URL <- paste0("https://fbref.com/en/comps/22/",s,"/schedule/Major-League-Soccer-Scores-and-Fixtures")
    html_doc <- URL %>% xml2::read_html()
    if (s == "") {table <- html_doc %>% rvest::html_nodes("#sched_11006_1")}
    else {table <- html_doc %>% rvest::html_nodes("#sched_all")}
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    df <- df %>%
      dplyr::filter(Day != "Day") %>%
      dplyr::filter(Date != "")
    pg <- xml2::read_html(URL)
    all <- rvest::html_attr(rvest::html_nodes(pg, "a"), "href")
    all <- tibble::as_tibble(all)
    all <- all %>%
      dplyr::filter(stringr::str_detect(value, '^/en/matches/')) %>%
      dplyr::filter(nchar(value) >= 25)
    all <- dplyr::distinct(all)
    all <- all %>% dplyr::mutate(game_id = substr(value,13,20))
    all <- all %>% dplyr::mutate(game_url = paste0('https://fbref.com/',value))
    all$value <- NULL
    if (nrow(all) < 1) {all <- data.frame(matrix(ncol=2,nrow=1, dimnames=list(NULL, names(all))))}
    df <- tibble::rownames_to_column(df)
    all <- tibble::rownames_to_column(all)
    df <- dplyr::left_join(df,all,by='rowname')
    df$rowname <- NULL
    total <- plyr::rbind.fill(total,df)
  }
  total$Date <- lubridate::ymd(total$Date)
  total$Score <- gsub("\\s*\\([^\\)]+\\)","",as.character(total$Score))
  total$Score <- gsub("[(]","",as.character(total$Score))
  total$Score <- gsub("[)]","",as.character(total$Score))
  total$Score <- stringr::str_trim(total$Score, side = c("both"))
  total$away_score <- substr(total$Score,3,3)
  total$home_score <- substr(total$Score,1,1)
  total$Attendance <- sapply(total$Attendance, gsub, pattern=",", replacement="")
  suppressWarnings({total$Attendance <- sapply(total$Attendance,as.numeric)})
  total$Match.Report <- NULL
  x <- c( "xG","xG.1")
  total[x[!(x %in% colnames(total))]] = NA
  total <- total %>% dplyr::rename(home_xGoals = xG,away_xGoals = xG.1)
  start <- lubridate::ymd(start_season, truncated = 2L)
  end <- lubridate::ymd(end_season+1, truncated = 2L)
  total <- total %>%
    dplyr::filter(Date >= start) %>%
    dplyr::filter(Date <= end)
  closeAllConnections()
  total$Home <- dplyr::recode(total$Home, "Houston" = "Houston Dynamo","Seattle"="Seattle Sounders FC",
                              "CF Montréal"="CF Montreal","Orlando City"="Orlando City SC",
                              "Los Angeles FC"="Los Angeles FC","FC Dallas"="FC Dallas",
                              "NY Red Bulls"="New York Red Bulls","D.C. United"="D.C. United",
                              "Nashville"="Nashville SC","Chicago"="Chicago Fire FC",
                              "Miami"="Inter Miami CF","Columbus"="Columbus Crew SC" ,
                              "Vancouver"="Vancouver Whitecaps FC","Sporting KC"="Sporting Kansas City",
                              "NYCFC"="New York City FC","Toronto FC"="Toronto FC","San Jose"="San Jose Earthquakes",
                              "Minnesota"="Minnesota United FC","Atlanta"="Atlanta United FC",
                              "New England"="New England Revolution","Philadelphia"="Philadelphia Union",
                              "Colorado"="Colorado Rapids","Portland"="Portland Timbers",
                              "LA Galaxy"="LA Galaxy","Real Salt Lake"="Real Salt Lake",
                              "FC Cincinnati"="FC Cincinnati","Austin FC"="Austin FC",
                              "Montreal"="CF Montreal","KC Wizards"="Sporting Kansas City",
                              "MetroStars"="New York Red Bulls","Dallas"="FC Dallas",
                              "KC Wiz"="Sporting Kansas City","Minnesota Utd"="Minnesota United FC",
                              "Atlanta Utd"="Atlanta United FC","Chicago Fire"="Chicago Fire FC",
                              "Inter Miami"="Inter Miami CF","Columbus SC"="Columbus Crew SC",
                              "Columbus Crew"="Columbus Crew SC","Montreal Impact"="CF Montreal",
                              "Dallas Burn"="FC Dallas")
  total$Away <- dplyr::recode(total$Away, "Houston" = "Houston Dynamo","Seattle"="Seattle Sounders FC",
                              "CF Montréal"="CF Montreal","Orlando City"="Orlando City SC",
                              "Los Angeles FC"="Los Angeles FC","FC Dallas"="FC Dallas",
                              "NY Red Bulls"="New York Red Bulls","D.C. United"="D.C. United",
                              "Nashville"="Nashville SC","Chicago"="Chicago Fire FC",
                              "Miami"="Inter Miami CF","Columbus"="Columbus Crew SC" ,
                              "Vancouver"="Vancouver Whitecaps FC","Sporting KC"="Sporting Kansas City",
                              "NYCFC"="New York City FC","Toronto FC"="Toronto FC","San Jose"="San Jose Earthquakes",
                              "Minnesota"="Minnesota United FC","Atlanta"="Atlanta United FC",
                              "New England"="New England Revolution","Philadelphia"="Philadelphia Union",
                              "Colorado"="Colorado Rapids","Portland"="Portland Timbers",
                              "LA Galaxy"="LA Galaxy","Real Salt Lake"="Real Salt Lake",
                              "FC Cincinnati"="FC Cincinnati","Austin FC"="Austin FC",
                              "Montreal"="CF Montreal","KC Wizards"="Sporting Kansas City",
                              "MetroStars"="New York Red Bulls","Dallas"="FC Dallas",
                              "KC Wiz"="Sporting Kansas City","Minnesota Utd"="Minnesota United FC",
                              "Atlanta Utd"="Atlanta United FC","Chicago Fire"="Chicago Fire FC",
                              "Inter Miami"="Inter Miami CF","Columbus SC"="Columbus Crew SC",
                              "Columbus Crew"="Columbus Crew SC","Montreal Impact"="CF Montreal",
                              "Dallas Burn"="FC Dallas")
  total$Home <- ifelse((total$Date < '2019-01-01')&(total$Home == 'Inter Miami CF'),'Miami Fusion',total$Home)
  total$Away <- ifelse((total$Date < '2019-01-01')&(total$Away == 'Inter Miami CF'),'Miami Fusion',total$Away)
  total <- total %>% dplyr::left_join(dplyr::select(rMLS::team_info,team_name,team_id),by=c("Home" = "team_name"))
  total <- total %>% dplyr::left_join(dplyr::select(rMLS::team_info,team_name,team_id),by=c("Away" = "team_name"),suffix = c("","away"))
  total <- total %>% dplyr::rename(home_team_id = team_id,away_team_id = team_idaway)
  suppressWarnings({total$home_score <- as.numeric(total$home_score)})
  #total$home_score <- ifelse(is.na(total$home_score),0,total$home_score)
  suppressWarnings({total$away_score <- as.numeric(total$away_score)})
  #total$away_score <- ifelse(is.na(total$away_score),0,total$away_score)
  suppressWarnings({total$home_xGoals <- as.numeric(total$home_xGoals)})
  suppressWarnings({total$away_xGoals <- as.numeric(total$away_xGoals)})
  return(total)
}
