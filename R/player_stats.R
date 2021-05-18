#' A function to find individual player statistics
#'
#' This function pulls individual player statistics using the Football-Reference player ID as an argument. An ID must be specified as there are no default arguments. Player ID values can be found using the playerid_lookup() function, where you can supply a name to generate a tibble of possible players.
#' @param ID = Football Reference ID
#' @keywords player, stats, statistics, player_stats
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' library(rMLS)
#' player_stats(id ='e0cd04e0')


player_stats <- function(id=NULL){
  URL <- paste0("https://fbref.com/en/players/",id,"/")
  html_doc <- URL %>% xml2::read_html()
  # 1 "#stats_standard_dom_lg"
  one <- c("VarSeason", "VarAge", "VarSquad", "VarCountry", "VarComp",  "VarLgRank", "PlayingTimeMP", "PlayingTimeStarts", "PlayingTimeMin",  "PlayingTimes", "PerformanceGls", "PerformanceAst", "PerformanceGPK",  "PerformancePK", "PerformancePKatt", "PerformanceCrdY", "PerformanceCrdR",  "PerMinutesGls", "PerMinutesAst", "PerMinutesGA", "PerMinutesGPK",  "PerMinutesGAPK", "ExpectedxG", "ExpectednpxG", "ExpectedxA",  "ExpectednpxGxA", "PerMinutesxG", "PerMinutesxA", "PerMinutesxGxA",  "PerMinutesnpxG", "PerMinutesnpxGxA", "VarMatches")
  one <- tibble::as_tibble(sapply(one, function(x) character()))
  table <- html_doc %>% rvest::html_nodes("#stats_standard_dom_lg")
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$VarAge <- as.integer(df$VarAge)})
  df <- df %>% dplyr::filter(!is.na(VarAge)) %>% dplyr::arrange(VarAge)
  one <- plyr::rbind.fill(one,df)

  # 2 "#stats_shooting_dom_lg"
  two <- c("VarSeason", "VarAge", "VarSquad", "VarCountry", "VarComp",  "VarLgRank", "Varninetys", "StandardGls", "StandardSh", "StandardSoT",  "StandardSoTpct", "StandardShninety", "StandardSoTninety", "StandardGSh",  "StandardGSoT", "StandardDist", "StandardFK", "StandardPK", "StandardPKatt",  "ExpectedxG", "ExpectednpxG", "ExpectednpxGSh", "ExpectedGxG",  "ExpectednpGxG", "VarMatches")
  two <- tibble::as_tibble(sapply(two, function(x) character()))
  table <- html_doc %>% rvest::html_nodes("#stats_shooting_dom_lg")
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$VarAge <- as.integer(df$VarAge)})
  df <- df %>% dplyr::filter(!is.na(VarAge)) %>% dplyr::arrange(VarAge)
  two <- plyr::rbind.fill(two,df)

  # 3"#stats_passing_dom_lg"
  three <- c("VarSeason", "VarAge", "VarSquad", "VarCountry", "VarComp",  "VarLgRank", "Varninetys", "TotalCmp", "TotalAtt", "TotalCmppct",  "TotalTotDist", "TotalPrgDist", "ShortCmp", "ShortAtt", "ShortCmppct",  "MediumCmp", "MediumAtt", "MediumCmppct", "LongCmp", "LongAtt",  "LongCmppct", "VarAst", "VarxA", "VarAxA", "VarKP", "Var", "VarPPA",  "VarCrsPA", "VarProg", "VarMatches")
  three <- tibble::as_tibble(sapply(three, function(x) character()))
  table <- html_doc %>% rvest::html_nodes("#stats_passing_dom_lg")
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$VarAge <- as.integer(df$VarAge)})
  df <- df %>% dplyr::filter(!is.na(VarAge)) %>% dplyr::arrange(VarAge)
  three <- plyr::rbind.fill(three,df)

  # 4 "#stats_passing_types_dom_lg"
  four <- c("VarSeason", "VarAge", "VarSquad", "VarCountry", "VarComp",  "VarLgRank", "Varninetys", "VarAtt", "PassTypesLive", "PassTypesDead",  "PassTypesFK", "PassTypesTB", "PassTypesPress", "PassTypesSw",  "PassTypesCrs", "PassTypesCK", "CornerKicksIn", "CornerKicksOut",  "CornerKicksStr", "HeightGround", "HeightLow", "HeightHigh",  "BodyPartsLeft", "BodyPartsRight", "BodyPartsHead", "BodyPartsTI",  "BodyPartsOther", "OutcomesCmp", "OutcomesOff", "OutcomesOut",  "OutcomesInt", "OutcomesBlocks", "VarMatches")
  four <- tibble::as_tibble(sapply(four, function(x) character()))
  table <- html_doc %>% rvest::html_nodes("#stats_passing_types_dom_lg")
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$VarAge <- as.integer(df$VarAge)})
  df <- df %>% dplyr::filter(!is.na(VarAge)) %>% dplyr::arrange(VarAge)
  four <- plyr::rbind.fill(four,df)

  # 5 "#stats_gca_dom_lg"
  five <- c("VarSeason", "VarAge", "VarSquad", "VarCountry", "VarComp",  "VarLgRank", "Varninetys", "SCASCA", "SCASCAninety", "SCATypesPassLive",  "SCATypesPassDead", "SCATypesDrib", "SCATypesSh", "SCATypesFld",  "SCATypesDef", "GCAGCA", "GCAGCAninety", "GCATypesPassLive",  "GCATypesPassDead", "GCATypesDrib", "GCATypesSh", "GCATypesFld",  "GCATypesDef", "GCATypesOG", "VarMatches")
  five <- tibble::as_tibble(sapply(five, function(x) character()))
  table <- html_doc %>% rvest::html_nodes("#stats_gca_dom_lg")
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$VarAge <- as.integer(df$VarAge)})
  df <- df %>% dplyr::filter(!is.na(VarAge)) %>% dplyr::arrange(VarAge)
  five <- plyr::rbind.fill(five,df)

  # 6 ,"#stats_defense_dom_lg"
  six <- c("VarSeason", "VarAge", "VarSquad", "VarCountry", "VarComp",  "VarLgRank", "Varninetys", "TacklesTkl", "TacklesTklW", "TacklesDef rd",  "TacklesMid rd", "TacklesAtt rd", "VsDribblesTkl", "VsDribblesAtt",  "VsDribblesTklpct", "VsDribblesPast", "PressuresPress", "PressuresSucc",  "Pressurespct", "PressuresDef rd", "PressuresMid rd", "PressuresAtt rd",  "BlocksBlocks", "BlocksSh", "BlocksShSv", "BlocksPass", "VarInt",  "VarTklInt", "VarClr", "VarErr", "VarMatches")
  six <- tibble::as_tibble(sapply(six, function(x) character()))
  table <- html_doc %>% rvest::html_nodes("#stats_defense_dom_lg")
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$VarAge <- as.integer(df$VarAge)})
  df <- df %>% dplyr::filter(!is.na(VarAge)) %>% dplyr::arrange(VarAge)
  six <- plyr::rbind.fill(six,df)

  # 7 "#stats_possession_dom_lg"
  seven <- c("VarSeason", "VarAge", "VarSquad", "VarCountry", "VarComp",  "VarLgRank", "Varninetys", "TouchesTouches", "TouchesDef Pen",  "TouchesDef rd", "TouchesMid rd", "TouchesAtt rd", "TouchesAtt Pen",  "TouchesLive", "DribblesSucc", "DribblesAtt", "DribblesSuccpct",  "DribblesPl", "DribblesMegs", "CarriesCarries", "CarriesTotDist",  "CarriesPrgDist", "CarriesProg", "Carries", "CarriesCPA", "CarriesMis",  "CarriesDis", "ReceivingTarg", "ReceivingRec", "ReceivingRecpct",  "ReceivingProg", "VarMatches")
  seven <- tibble::as_tibble(sapply(seven, function(x) character()))
  table <- html_doc %>% rvest::html_nodes("#stats_possession_dom_lg")
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$VarAge <- as.integer(df$VarAge)})
  df <- df %>% dplyr::filter(!is.na(VarAge)) %>% dplyr::arrange(VarAge)
  seven <- plyr::rbind.fill(seven,df)

  # 8 "#stats_playing_time_dom_lg"
  eight <- c("VarSeason", "VarAge", "VarSquad", "VarCountry", "VarComp",  "VarLgRank", "PlayingTimeMP", "PlayingTimeMin", "PlayingTimeMnMP",  "PlayingTimeMinpct", "PlayingTimeninetys", "StartsStarts", "StartsMnStart",  "StartsCompl", "SubsSubs", "SubsMnSub", "SubsunSub", "TeamSuccessPPM",  "TeamSuccessonG", "TeamSuccessonGA", "TeamSuccess", "TeamSuccessninety",  "TeamSuccessOnOff", "TeamSuccessxGonxG", "TeamSuccessxGonxGA",  "TeamSuccessxGxG", "TeamSuccessxGxGninety", "TeamSuccessxGOnOff",  "VarMatches")
  eight <- tibble::as_tibble(sapply(eight, function(x) character()))
  table <- html_doc %>% rvest::html_nodes("#stats_playing_time_dom_lg")
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$VarAge <- as.integer(df$VarAge)})
  df <- df %>% dplyr::filter(!is.na(VarAge)) %>% dplyr::arrange(VarAge)
  eight <- plyr::rbind.fill(eight,df)

  # 9 "#stats_misc_dom_lg"
  nine <- c("VarSeason", "VarAge", "VarSquad", "VarCountry", "VarComp",  "VarLgRank", "Varninetys", "PerformanceCrdY", "PerformanceCrdR",  "PerformanceTwoCrdY", "PerformanceFls", "PerformanceFld", "PerformanceOff",  "PerformanceCrs", "PerformanceInt", "PerformanceTklW", "PerformancePKwon",  "PerformancePKcon", "PerformanceOG", "PerformanceRecov", "AerialDuelsWon",  "AerialDuelsLost", "AerialDuelsWonpct", "VarMatches")
  nine <- tibble::as_tibble(sapply(nine, function(x) character()))
  table <- html_doc %>% rvest::html_nodes("#stats_misc_dom_lg")
  table1 <- table %>% rvest::html_table()
  df <- as.data.frame(table1)
  names(df) <- paste0(names(df), df[1, ])
  names(df) <- stringr::str_replace_all(names(df), "%", "pct")
  names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
  names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
  names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
  df <- df[-1,]
  suppressWarnings({df$VarAge <- as.integer(df$VarAge)})
  df <- df %>% dplyr::filter(!is.na(VarAge)) %>% dplyr::arrange(VarAge)
  nine <- plyr::rbind.fill(nine,df)


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
  final <- merge(final, seven, by =0, all = TRUE,suffixes = c("","tbl7"),sort=FALSE)
  final$Row.names <- NULL
  final <- merge(final, eight, by =0, all = TRUE,suffixes = c("","tbl8"),sort=FALSE)
  final$Row.names <- NULL
  final <- merge(final, nine, by =0, all = TRUE,suffixes = c("","tbl9"),sort=FALSE)
  final$Row.names <- NULL
  #final <- tibble::add_column(final, player_name = rMLS::playerid_lookup(id)[[1,3]], .before = 1)
  l1 <- c('season','age','squad','country','league','league_rank','matches_played','starts','minutes','90s','goals','assists','non_pen_goals','pen_goals','pen_attempted','yellow_cards','red_cards','goals_per90','assists_per90','goals_assists_per90','non_penalty_goals_per90','goals_assists_non_penalty_per90','expected_goals','non_penalty_expected_goals','expected_assists','non_penalty_expected_goals_plus_assists','expected_goals_per90','expected_assists_per90','expected_goals_plus_assists_per90','non_penalty_expected_goals_per90','non_penalty_expected_goals_plus_assists_per90','matches')
  l2 <- c('season','age','squad','country','league','league_rank','90s','goals','shots','shots_on_target','shots_on_target_pct','shots_per90','shots_on_target_per90','goals_per_shot','goals_per_shot_on_target','shot_distance','shots_free_kicks','pen_goals','pen_attempted','expected_goals','non_penalty_expected_goals','non_penalty_expected_goals_per_shot','goals_minus_expected_goals','non_pen_goals_minus_non_pen_expected_goals','matches')
  l3 <- c('season','age','squad','country','league','league_rank','90s','passes_completed','passes_attempted','pass_completion_pct','total_pass_distance','progressive_pass_distance','short_passes_completed','short_passes_att','short_pass_completion_pct','medium_passes_completed','medium_passes_att','medium_pass_completion_pct','long_passes_completed','long_passes_att','long_pass_completion_pct','assists','expected_assists','assists_minus_expected_assists','assisted_shots','passes_completed_final_third','passes_completed_into_18box','crosses_completed_into_18box','progressive_passes','matches')
  l4 <- c('season','age','squad','country','league','league_rank','90s','passes_attempted','live_ball_passes','dead_ball_passes','free_kick_passes','through_ball_passes','passes_under_press','cross_field_passes','crosses','corner_kicks','inswing_corners','outswing_corners','straight_corners','ground_passes','low_passes','high_passes','left_foot_passes','right_foot_passes','header_passes','throw_ins','other_body_part_passes','passes_completed','pass_attempts_offside','pass_attempts_out_of_bounds','pass_attempts_intercepted','pass_attempts_blocked','matches')
  l5 <- c('season','age','squad','country','league','league_rank','90s','shot_creating_actions','shot_creating_actions_per90','live_ball_pass_to_shot','dead_ball_pass_to_shot','dribbles_to_shot','shots_to_shot','fouls_to_shot','defense_to_shot','goal_creating_actions','goal_creating_actions_per90','live_ball_pass_to_goal','dead_ball_pass_to_goal','dribbles_to_goal','shots_to_goal','fouls_to_goal','defense_to_goal','action_to_own_goal','matches')
  l6 <- c('season','age','squad','country','league','league_rank','90s','tackles','tackles_won','tackles_def_third','tackles_mid_third','tackles_att_third','dribbler_tackles','dribbler_tackle_attempts','tackle_pct','dribbled_past','pressures_attempted','pressures_successful','pressure_success_pct','pressures_def_third','pressures_mid_third','pressures_att_third','toal_blocks','shots_blocked','shots_blocked_on_target','passes_blocked','interceptions','tackles_plus_interceptions','clearances','defensive_errors','matches')
  l7 <- c('season','age','squad','country','league','league_rank','90s','touches','defensive_penalty_area_touches','touches_def_third','touches_mid_third','touches_att_third','touches_att_pen_area','touches_live_ball','dribbles_successful','dribbles_attempted','dribble_pct','players_dribbled_past','nutmegs','carries','carries_distance','carries_progressive_distance','carries_progressive_count','carries_into_att_third','carries_into_18box','carries_missed','carries_lost','reception_targets','receptions','reception_completion_pct','progressive_receptions','matches')
  l8 <- c('season','age','squad','country','league','league_rank','matches_played','minutes_played','minutes_played_per_match','minutes_pct','90s','starts','minutes_per_start','complete_matches','sub_appearances','minutes_per_sub','unused_sub','points_per_match','goals_for_on_pitch','goals_against_on_pitch','plus_minus','plus_minus_per90','net_on_off','team_expected_goals_for_on_pitch','team_expected_goals_against_on_pitch','team_expected_goals_plus_minus','team_expected_goals_plus_minus_per90','net_on_off_expected_goals','matches')
  l9 <- c('season','age','squad','country','league','league_rank','90s','yellow_cards','red_cards','second_yellow_cards','fouls_committed','fouls_drawn','offsides','crosses','interceptions','tackles_won','penalties_won','penalties_conceded','own_goals','loose_ball_recoveries','aerials_won','aerials_lost','aerials_won_pct','matches')
  final_names <- c(l1,l2,l3,l4,l5,l6,l7,l8,l9)
  names(final) <- final_names
  final <-  final[!duplicated(colnames(final))]
  final$matches <- NULL
  final$age <- as.numeric(final$age)
  final$plus_minus <- as.numeric(final$plus_minus)
  #if (nrow(final)>0) {final$age <- sapply(final$age,as.numeric)}
  #if (nrow(final)>0) {final$plus_minus <- sapply(final$plus_minus,as.numeric)}
  final$league_rank <- gsub("[^0-9.-]", "", final$league_rank)
  final[6:180]<- sapply(final[6:180], gsub, pattern=",", replacement="")
  suppressWarnings({final[6:180] <- sapply(final[6:180],as.numeric)})
  final$country <- substr(final$country, nchar(final$country) - (3 - 1), nchar(final$country))
  final$league <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",final$league)
  final$league <- stringr::str_trim(final$league, side = c("both"))
  final <- final %>% dplyr::mutate(player_id = id) %>% dplyr::relocate(player_id, .before = "season")
  return(final)
}
