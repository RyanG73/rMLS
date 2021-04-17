#' A function to find box scores
#'
#' @param box_scores, no defaults
#' @keywords box, box_scores, scores
#' @importFrom magrittr "%>%"
#' @export
#' @examples \dontrun{box_scores()}
#' box_scores()

box_scores <- function(data=NULL,row=1){
  game_id = data$game_id[row]
  home_id = data$home_team_id[row]
  away_id = data$away_team_id[row]
  URL <- paste0("https://fbref.com/en/matches/",game_id)
  html_doc <- URL %>% xml2::read_html()
  # 1 "#stats_",home_id,"_summary"
  one <- c("VarPlayer", "number", "VarNation", "VarPos", "VarAge", "VarMin",
           "PerformanceGls", "PerformanceAst", "PerformancePK", "PerformancePKatt",
           "PerformanceSh", "PerformanceSoT", "PerformanceCrdY", "PerformanceCrdR",
           "PerformanceTouches", "PerformancePress", "PerformanceTkl", "PerformanceInt",
           "PerformanceBlocks", "ExpectedxG", "ExpectednpxG", "ExpectedxA",
           "SCASCA", "SCAGCA", "PassesCmp", "PassesAtt", "PassesTwoCmppct",
           "PassesProg", "CarriesCarries", "CarriesProg", "DribblesSucc",
           "DribblesAtt")
  one <- tibble::as_tibble(sapply(one, function(x) character()))
  name <- paste0("#stats_",home_id,"_summary")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "Var.2#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$number <- as.integer(df$number)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(one))
  df <- df[sort(c(col.num))]
  one <- plyr::rbind.fill(one,df)
  if (nrow(one)>0) {one$team_id <- home_id} else {one$team_id  <- character()}
  # 2 "stats_529ba333_passing"
  two <- c("VarPlayer", "Varnumber", "VarNation", "VarPos", "VarAge",
           "VarMin", "TotalCmp", "TotalAtt", "TotalTwoCmppct", "TotalTotDist",
           "TotalPrgDist", "ShortCmp", "ShortAtt", "ShortTwoCmppct", "MediumCmp",
           "MediumAtt", "MediumTwoCmppct", "LongCmp", "LongAtt", "LongTwoCmppct",
           "VarAst", "VarxA", "VarKP", "Var", "VarPPA", "VarCrsPA", "VarProg")
  two <- tibble::as_tibble(sapply(two, function(x) character()))
  name <- paste0("#stats_",home_id,"_passing")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$Varnumber <- as.integer(df$Varnumber)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(VarPos != "")
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(two))
  df <- df[sort(c(col.num))]
  two <- plyr::rbind.fill(two,df)
  if (nrow(two)>0) {two$team_id <- home_id} else {two$team_id  <- character()}
  # 3 "stats_529ba333_passing_types"
  three <- c("VarPlayer", "Varnumber", "VarNation", "VarPos", "VarAge",
             "VarMin", "VarAtt", "PassTypesLive", "PassTypesDead", "PassTypesFK",
             "PassTypesTB", "PassTypesPress", "PassTypesSw", "PassTypesCrs",
             "PassTypesCK", "CornerKicksIn", "CornerKicksOut", "CornerKicksStr",
             "HeightGround", "HeightLow", "HeightHigh", "BodyPartsLeft", "BodyPartsRight",
             "BodyPartsHead", "BodyPartsTI", "BodyPartsOther", "OutcomesCmp",
             "OutcomesOff", "OutcomesOut", "OutcomesInt", "OutcomesBlocks")
  three <- tibble::as_tibble(sapply(three, function(x) character()))
  name <- paste0("#stats_",home_id,"_passing_types")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$Varnumber <- as.integer(df$Varnumber)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(three))
  df <- df[sort(c(col.num))]
  three <- plyr::rbind.fill(three,df)
  if (nrow(three)>0) {three$team_id <- home_id} else {three$team_id  <- character()}

  # 4 "stats_529ba333_defense"
  four <- c("VarPlayer", "Varnumber", "VarNation", "VarPos", "VarAge",
            "VarMin", "TacklesTkl", "TacklesTklW", "TacklesDef rd", "TacklesMid rd",
            "TacklesAtt rd", "VsDribblesTkl", "VsDribblesAtt", "VsDribblesTklpct",
            "VsDribblesPast", "PressuresPress", "PressuresSucc", "Pressurespct",
            "PressuresDef rd", "PressuresMid rd", "PressuresAtt rd", "BlocksBlocks",
            "BlocksSh", "BlocksShSv", "BlocksPass", "VarInt", "VarTklInt",
            "VarClr", "VarErr")
  four <- tibble::as_tibble(sapply(four, function(x) character()))
  name <- paste0("#stats_",home_id,"_defense")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$Varnumber <- as.integer(df$Varnumber)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(four))
  df <- df[sort(c(col.num))]
  four <- plyr::rbind.fill(four,df)
  if (nrow(four)>0) {four$team_id <- home_id} else {four$team_id  <- character()}

  # 5 "stats_529ba333_possession"
  five <- c("VarPlayer", "Varnumber", "VarNation", "VarPos", "VarAge",
            "VarMin", "TouchesTouches", "TouchesDef Pen", "TouchesDef rd",
            "TouchesMid rd", "TouchesAtt rd", "TouchesAtt Pen", "TouchesLive",
            "DribblesSucc", "DribblesAtt", "DribblesSuccpct", "DribblesnumberPl",
            "DribblesMegs", "CarriesCarries", "CarriesTotDist", "CarriesPrgDist",
            "CarriesProg", "Carries", "CarriesCPA", "CarriesMis", "CarriesDis",
            "ReceivingTarg", "ReceivingRec", "ReceivingRecpct", "ReceivingProg")
  five <- tibble::as_tibble(sapply(five, function(x) character()))
  name <- paste0("#stats_",home_id,"_possession")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$Varnumber <- as.integer(df$Varnumber)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(five))
  df <- df[sort(c(col.num))]
  five <- plyr::rbind.fill(five,df)
  if (nrow(five)>0) {five$team_id <- home_id} else {five$team_id  <- character()}

  # 6 "stats_529ba333_misc"
  six <- c("VarPlayer", "Varnumber", "VarNation", "VarPos", "VarAge",
           "VarMin", "PerformanceCrdY", "PerformanceCrdR", "PerformanceTwoCrdY",
           "PerformanceFls", "PerformanceFld", "PerformanceOff", "PerformanceCrs",
           "PerformanceInt", "PerformanceTklW", "PerformancePKwon", "PerformancePKcon",
           "PerformanceOG", "PerformanceRecov", "AerialDuelsWon", "AerialDuelsLost",
           "AerialDuelsWonpct")
  six <- tibble::as_tibble(sapply(six, function(x) character()))
  name <- paste0("#stats_",home_id,"_misc")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$Varnumber <- as.integer(df$Varnumber)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(six))
  df <- df[sort(c(col.num))]
  six <- plyr::rbind.fill(six,df)
  if (nrow(six)>0) {six$team_id <- home_id} else {six$team_id  <- character()}

  # 1 "#stats_",home_id,"_summary"
  one2 <- c("VarPlayer", "number", "VarNation", "VarPos", "VarAge", "VarMin",
            "PerformanceGls", "PerformanceAst", "PerformancePK", "PerformancePKatt",
            "PerformanceSh", "PerformanceSoT", "PerformanceCrdY", "PerformanceCrdR",
            "PerformanceTouches", "PerformancePress", "PerformanceTkl", "PerformanceInt",
            "PerformanceBlocks", "ExpectedxG", "ExpectednpxG", "ExpectedxA",
            "SCASCA", "SCAGCA", "PassesCmp", "PassesAtt", "PassesTwoCmppct",
            "PassesProg", "CarriesCarries", "CarriesProg", "DribblesSucc",
            "DribblesAtt")
  one2 <- tibble::as_tibble(sapply(one2, function(x) character()))
  name <- paste0("#stats_",away_id,"_summary")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "Var.2#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$number <- as.integer(df$number)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(one2))
  df <- df[sort(c(col.num))]
  one2 <- plyr::rbind.fill(one2,df)
  if (nrow(one2)>0) {one2$team_id <- away_id} else {one2$team_id  <- character()}
  # 2 "stats_529ba333_passing"
  two2 <- c("VarPlayer", "Varnumber", "VarNation", "VarPos", "VarAge",
           "VarMin", "TotalCmp", "TotalAtt", "TotalTwoCmppct", "TotalTotDist",
           "TotalPrgDist", "ShortCmp", "ShortAtt", "ShortTwoCmppct", "MediumCmp",
           "MediumAtt", "MediumTwoCmppct", "LongCmp", "LongAtt", "LongTwoCmppct",
           "VarAst", "VarxA", "VarKP", "Var", "VarPPA", "VarCrsPA", "VarProg")
  two2 <- tibble::as_tibble(sapply(two2, function(x) character()))
  name <- paste0("#stats_",away_id,"_passing")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$Varnumber <- as.integer(df$Varnumber)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(two2))
  df <- df[sort(c(col.num))]
  two2 <- plyr::rbind.fill(two2,df)
  if (nrow(two2)>0) {two2$team_id <- away_id} else {two2$team_id  <- character()}

  # 3 "stats_529ba333_passing_types"
  three2 <- c("VarPlayer", "Varnumber", "VarNation", "VarPos", "VarAge",
             "VarMin", "VarAtt", "PassTypesLive", "PassTypesDead", "PassTypesFK",
             "PassTypesTB", "PassTypesPress", "PassTypesSw", "PassTypesCrs",
             "PassTypesCK", "CornerKicksIn", "CornerKicksOut", "CornerKicksStr",
             "HeightGround", "HeightLow", "HeightHigh", "BodyPartsLeft", "BodyPartsRight",
             "BodyPartsHead", "BodyPartsTI", "BodyPartsOther", "OutcomesCmp",
             "OutcomesOff", "OutcomesOut", "OutcomesInt", "OutcomesBlocks")
  three2 <- tibble::as_tibble(sapply(three2, function(x) character()))
  name <- paste0("#stats_",away_id,"_passing_types")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$Varnumber <- as.integer(df$Varnumber)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(three2))
  df <- df[sort(c(col.num))]
  three2 <- plyr::rbind.fill(three2,df)
  if (nrow(three2)>0) {three2$team_id <- away_id} else {three2$team_id  <- character()}

  # 4 "stats_529ba333_defense"
  four2 <- c("VarPlayer", "Varnumber", "VarNation", "VarPos", "VarAge",
            "VarMin", "TacklesTkl", "TacklesTklW", "TacklesDef rd", "TacklesMid rd",
            "TacklesAtt rd", "VsDribblesTkl", "VsDribblesAtt", "VsDribblesTklpct",
            "VsDribblesPast", "PressuresPress", "PressuresSucc", "Pressurespct",
            "PressuresDef rd", "PressuresMid rd", "PressuresAtt rd", "BlocksBlocks",
            "BlocksSh", "BlocksShSv", "BlocksPass", "VarInt", "VarTklInt",
            "VarClr", "VarErr")
  four2 <- tibble::as_tibble(sapply(four2, function(x) character()))
  name <- paste0("#stats_",away_id,"_defense")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$Varnumber <- as.integer(df$Varnumber)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(four2))
  df <- df[sort(c(col.num))]
  four2 <- plyr::rbind.fill(four2,df)
  if (nrow(four2)>0) {four2$team_id <- away_id} else {four2$team_id  <- character()}

  # 5 "stats_529ba333_possession"
  five2 <- c("VarPlayer", "Varnumber", "VarNation", "VarPos", "VarAge",
            "VarMin", "TouchesTouches", "TouchesDef Pen", "TouchesDef rd",
            "TouchesMid rd", "TouchesAtt rd", "TouchesAtt Pen", "TouchesLive",
            "DribblesSucc", "DribblesAtt", "DribblesSuccpct", "DribblesnumberPl",
            "DribblesMegs", "CarriesCarries", "CarriesTotDist", "CarriesPrgDist",
            "CarriesProg", "Carries", "CarriesCPA", "CarriesMis", "CarriesDis",
            "ReceivingTarg", "ReceivingRec", "ReceivingRecpct", "ReceivingProg")
  five2 <- tibble::as_tibble(sapply(five2, function(x) character()))
  name <- paste0("#stats_",away_id,"_possession")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$Varnumber <- as.integer(df$Varnumber)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(five2))
  df <- df[sort(c(col.num))]
  five2 <- plyr::rbind.fill(five2,df)
  if (nrow(five2)>0) {five2$team_id <- away_id} else {five2$team_id  <- character()}

  # 6 "stats_529ba333_misc"
  six2 <- c("VarPlayer", "Varnumber", "VarNation", "VarPos", "VarAge",
           "VarMin", "PerformanceCrdY", "PerformanceCrdR", "PerformanceTwoCrdY",
           "PerformanceFls", "PerformanceFld", "PerformanceOff", "PerformanceCrs",
           "PerformanceInt", "PerformanceTklW", "PerformancePKwon", "PerformancePKcon",
           "PerformanceOG", "PerformanceRecov", "AerialDuelsWon", "AerialDuelsLost",
           "AerialDuelsWonpct")
  six2 <- tibble::as_tibble(sapply(six2, function(x) character()))
  name <- paste0("#stats_",away_id,"_misc")
  table <- html_doc %>% rvest::html_nodes(name)
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "#", "number")
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$Varnumber <- as.integer(df$Varnumber)})
  if (nrow(df)>0) {df <- df %>% dplyr::filter(VarPos != "")}
  #df <- df %>% dplyr::filter(!is.na(number))
  col.num <- which(colnames(df) %in% names(six2))
  df <- df[sort(c(col.num))]
  six2 <- plyr::rbind.fill(six2,df)
  if (nrow(six2)>0) {six2$team_id <- away_id} else {six2$team_id  <- character()}

  final <- merge(one, two, by =0, all = TRUE, suffixes = c("tbl1","tbl2"),sort=FALSE)
  final$Row.names <- NULL
  final <- merge(final, three, by =0, all = TRUE,suffixes = c("","tbl3"),sort=FALSE)
  final$Row.names <- NULL
  final <- merge(final, four, by =0, all = TRUE,suffixes = c("","tbl4"),sort=FALSE)
  final$Row.names <- NULL
  final <- merge(final, five, by =0, all = TRUE,suffixes = c("","tbl5"),sort=FALSE)
  final$Row.names <- NULL
  final <- merge(final, six, by =0, all = TRUE,suffixes = c("","tbl6"),sort=FALSE)
  final$Row.names <- NULL

  final2 <- merge(one2, two2, by =0, all = TRUE,suffixes = c("","tbl8"),sort=FALSE)
  final2$Row.names <- NULL
  final2 <- merge(final2, three2, by =0, all = TRUE,suffixes = c("","tbl9"),sort=FALSE)
  final2$Row.names <- NULL
  final2 <- merge(final2, four2, by =0, all = TRUE,suffixes = c("","tbl10"),sort=FALSE)
  final2$Row.names <- NULL
  final2 <- merge(final2, five2, by =0, all = TRUE,suffixes = c("","tbl11"),sort=FALSE)
  final2$Row.names <- NULL
  final2 <- merge(final2, six2, by =0, all = TRUE,suffixes = c("","tbl12"),sort=FALSE)
  final2$Row.names <- NULL
  l1 <- c("player_name", "number", "country", "position", "age", "minutes",
          "goals", "assists", "penalties_converted", "penalties_attempted",
          "shots", "shots_on_target", "yellow_cards", "red_cards",
          "touches", "pressures", "tackles", "interceptions",
          "blocks", "expected_goals", "non_penalty_expected_goals", "expected_assists",
          "shot_creating_actions", "goal_creating_actions", "passes_completed", "passes_attempted", "pass_completion_pct",
          "progressive_passes", "carries", "progressive_carries", "dribbles_successful",
          "dribbles_attempted","team_id")
  l2 <- c("player_name", "number", "country", "position", "age", "minutes"
          , "passes_completed", "passes_attempted", "pass_completion_pct", "pass_distance",
          "progressive_pass_distance", "short_passes_completed", "short_passes_attempted", "short_pass_completion_pct", "medium_passes_completed",
          "medium_passes_attempted", "medium_pass_completion_pct", "long_passes_completed", "long_passes_attempted", "long_pass_completion_pct",
          "assists", "expected_assosts", "key_passes", "passes_into_attacking_third", "passes_into_18box", "crosses_into_18box", "progressive_passes","team_id")
  l3 <- c("player_name", "number", "country", "position", "age", "minutes", "passes_attempted",
          "live_ball_passes", "dead_ball_passes", "free_kick_passes",
          "through_ball_passes", "passes_under_press", "pitch_wide_passes", "crosses",
          "corner_kicks", "corner_kicks_inswinging", "corner_kicks_outswinging", "corner_kicks_straight",
          "ground_passes", "low_passes", "high_passes", "left_foot_passes", "right_foot_passes",
          "head_passes", "throw_in_passes", "other_passes", "passes_completed",
          "passes_offside", "passes_out_of_bounds", "passes_intercepted", "passes_blocked","team_id")
  l4 <- c("player_name", "number", "country", "position", "age", "minutes",
          "tackles", "tackles_won", "tackles_def_third", "tackles_mid_third",
          "tackles_att_third", "dribblers_tackles_won", "dribbler_tackles_attempted", "dribbler_tackles_pct",
          "dribbler_tackles_lost", "pressure_attempted", "pressures_successful", "pressure_success_pct",
          "pressures_def_third", "pressures_mid_third", "pressures_att_third", "def_blocks",
          "blocked_shots", "blocked_shots_on_target", "blocked_passes", "def_interceptions", "tackles_plus_interceptions",
          "clearances", "def_errors","team_id")
  l5 <- c("player_name", "number", "country", "position", "age", "minutes",
          "touches", "touches_def_18box", "touches_def_third",
          "touches_mid_third", "touches_att_third", "touches_att_18box", "touches_live_ball",
          "dribbles_successful", "dribbles_attempted", "dribbles_success_pxt", "dribbles_past_players",
          "nutmegs", "carries", "carry_distance", "progressive_carry_distance",
          "progressive_carries", "carries_into_att_third", "carries_into_18box", "carries_missed", "carries_lost",
          "pass_targets", "passes_received", "passes_received_pct", "progressive_passes_received","team_id")
  l6 <- c("player_name", "number", "country", "position", "age", "minutes",
          "yellow_cards", "red_cards", "second_yellow_cards",
          "fouls_committed", "fouls_drawn", "offsides", "crosses",
          "interceptions", "tackles_won", "penalties_won", "penalties_converted",
          "own_goals", "loose_ball_recoveries", "aerials_won", "aerials_lost",
          "aerial_win_pct","team_id")
  l7 <- c("player_name", "number", "country", "position", "age", "minutes",
          "goals", "assists", "penalties_converted", "penalties_attempted",
          "shots", "shots_on_target", "yellow_cards", "red_cards",
          "touches", "pressures", "tackles", "interceptions",
          "blocks", "expected_goals", "non_penalty_expected_goals", "expected_assists",
          "shot_creating_actions", "goal_creating_actions", "passes_completed", "passes_attempted", "pass_completion_pct",
          "progressive_passes", "carries", "progressive_carries", "dribbles_successful",
          "dribbles_attempted","team_id")
  l8 <- c("player_name", "number", "country", "position", "age", "minutes"
          , "passes_completed", "passes_attempted", "pass_completion_pct", "pass_distance",
          "progressive_pass_distance", "short_passes_completed", "short_passes_attempted", "short_pass_completion_pct", "medium_passes_completed",
          "medium_passes_attempted", "medium_pass_completion_pct", "long_passes_completed", "long_passes_attempted", "long_pass_completion_pct",
          "assists", "expected_assosts", "key_passes", "passes_into_attacking_third", "passes_into_18box", "crosses_into_18box", "progressive_passes","team_id")
  l9 <- c("player_name", "number", "country", "position", "age", "minutes", "passes_attempted",
          "live_ball_passes", "dead_ball_passes", "free_kick_passes",
          "through_ball_passes", "passes_under_press", "pitch_wide_passes", "crosses",
          "corner_kicks", "corner_kicks_inswinging", "corner_kicks_outswinging", "corner_kicks_straight",
          "ground_passes", "low_passes", "high_passes", "left_foot_passes", "right_foot_passes",
          "head_passes", "throw_in_passes", "other_passes", "passes_completed",
          "passes_offside", "passes_out_of_bounds", "passes_intercepted", "passes_blocked","team_id")
  l10 <- c("player_name", "number", "country", "position", "age", "minutes",
           "tackles", "tackles_won", "tackles_def_third", "tackles_mid_third",
           "tackles_att_third", "dribblers_tackles_won", "dribbler_tackles_attempted", "dribbler_tackles_pct",
           "dribbler_tackles_lost", "pressure_attempted", "pressures_successful", "pressure_success_pct",
           "pressures_def_third", "pressures_mid_third", "pressures_att_third", "def_blocks",
           "blocked_shots", "blocked_shots_on_target", "blocked_passes", "def_interceptions", "tackles_plus_interceptions",
           "clearances", "def_errors","team_id")
  l11 <- c("player_name", "number", "country", "position", "age", "minutes",
           "touches", "touches_def_18box", "touches_def_third",
           "touches_mid_third", "touches_att_third", "touches_att_18box", "touches_live_ball",
           "dribbles_successful", "dribbles_attempted", "dribbles_success_pxt", "dribbles_past_players",
           "nutmegs", "carries", "carry_distance", "progressive_carry_distance",
           "progressive_carries", "carries_into_att_third", "carries_into_18box", "carries_missed", "carries_lost",
           "pass_targets", "passes_received", "passes_received_pct", "progressive_passes_received","team_id")
  l12 <- c("player_name", "number", "country", "position", "age", "minutes",
           "yellow_cards", "red_cards", "second_yellow_cards",
           "fouls_committed", "fouls_drawn", "offsides", "crosses",
           "interceptions", "tackles_won", "penalties_won", "penalties_converted",
           "own_goals", "loose_ball_recoveries", "aerials_won", "aerials_lost",
           "aerial_win_pct","team_id")
  final_names <- c(l1,l2,l3,l4,l5,l6)
  names(final) <- final_names
  final_names2 <- c(l7,l8,l9,l10,l11,l12)
  names(final2) <- final_names2
  final <-  final[!duplicated(colnames(final))]
  final2 <-  final2[!duplicated(colnames(final2))]
  final <- dplyr::bind_rows(final,final2,.id=NULL)
  final$country <- substr(final$country, nchar(final$country) - (3 - 1), nchar(final$country))
  final$age <- substr(final$age,1,2)
  final <- final %>% dplyr::relocate(team_id, .before = 2)
  if (nrow(final)>0) {final$game_id <- game_id} else {final$game_id  <- character()}
  final <- final %>% dplyr::relocate(game_id, .before = 2)
  final[7:124]<- sapply(final[7:124], gsub, pattern=",", replacement="")
  suppressWarnings({final[7:124] <- sapply(final[7:124],as.numeric)})
  final <- final %>% dplyr::relocate(team_id, .before = 2)
  final <- final %>% dplyr::inner_join(dplyr::select(rMLS::team_info,team_id,team_name),r,by='team_id')
  final <- final %>% dplyr::relocate(team_name, .before = "team_id")
  `%nin%` = Negate(`%in%`)
  if("Round" %nin% colnames(data)){data$Round <- "Regular Season"}
  final <- dplyr::inner_join(dplyr::select(data,game_id,Date,Round),final,by='game_id')
  final <- final %>% dplyr::relocate(Date, .before = "number")
  final <- final %>% dplyr::relocate(Round, .before = "number")
  return(final)
}
