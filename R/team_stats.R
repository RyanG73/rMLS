#' A function to find team statistics
#'
#' @param team_stats, no defaults
#' @keywords team, stats, statistics, team_stats
#' @importFrom magrittr "%>%"
#' @export
#' @examples \dontrun{team_stats()}
#' team_stats()

team_stats <- function(start_season=1996,end_season=2020,teamid=NULL){
  total <- tibble::tibble()
  season_table <- rMLS::seasons
  season_table <- season_table %>% dplyr::filter(Season != 2021) %>%
    dplyr::filter(Season >= start_season) %>%
    dplyr::filter(Season <= end_season)
  if (!is.null(teamid)) {
    season_table <- season_table %>% dplr::filter(team_id == teamid)
  }
  for(i in 1:nrow(season_table)){
    id <- season_table[[i,7]]
    season <- season_table[[i,1]]
    URL <- paste0("https://fbref.com/en/comps/22/",id,"/",season,"-Major-League-Soccer-Stats")
    html_doc <- URL %>% xml2::read_html()
    # 1 "#stats_squads_standard_for"
    one <- c("VarSquad", "Var Pl", "VarAge", "VarPoss", "PlayingTimeMP",
             "PlayingTimeStarts", "PlayingTimeMin", "PlayingTimes", "PerformanceGls",
             "PerformanceAst", "PerformanceGPK", "PerformancePK", "PerformancePKatt",
             "PerformanceCrdY", "PerformanceCrdR", "PerMinutesGls", "PerMinutesAst",
             "PerMinutesGA", "PerMinutesGPK", "PerMinutesGAPK", "ExpectedxG",
             "ExpectednpxG", "ExpectedxA", "ExpectednpxGxA", "PerMinutesxG",
             "PerMinutesxA", "PerMinutesxGxA", "PerMinutesnpxG", "PerMinutesnpxGxA")
    one <- tibble::as_tibble(sapply(one, function(x) character()))
    table <- html_doc %>% rvest::html_nodes("#stats_squads_standard_for")
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    names(df) <- paste0(names(df), df[1, ])
    names(df) <- stringr::str_replace_all(names(df), "%", "pct")
    names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
    df <- df[-1,]
    one <- plyr::rbind.fill(one,df)
    # 2 "#stats_squads_keeper_for"
    two <- c("VarSquad", "Var Pl", "PlayingTimeMP", "PlayingTimeStarts",
             "PlayingTimeMin", "PlayingTimeninetys", "PerformanceGA", "PerformanceGAninety",
             "PerformanceSoTA", "PerformanceSaves", "PerformanceSavepct",
             "PerformanceW", "PerformanceD", "PerformanceL", "PerformanceCS",
             "PerformanceCSpct", "PenaltyKicksPKatt", "PenaltyKicksPKA", "PenaltyKicksPKsv",
             "PenaltyKicksPKm", "PenaltyKicksSavepct")
    two <- tibble::as_tibble(sapply(two, function(x) character()))
    table <- html_doc %>% rvest::html_nodes("#stats_squads_keeper_for")
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    names(df) <- paste0(names(df), df[1, ])
    names(df) <- stringr::str_replace_all(names(df), "%", "pct")
    names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
    names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
    df <- df[-1,]
    two <- plyr::rbind.fill(two,df)
    # 3 "#stats_squads_keeper_adv_for"
    three <- c("VarSquad", "Var Pl", "Varninetys", "GoalsGA", "GoalsPKA",
               "GoalsFK", "GoalsCK", "GoalsOG", "ExpectedPSxG", "ExpectedPSxGSoT",
               "ExpectedPSxGplusminus", "Expectedninety", "LaunchedCmp", "LaunchedAtt",
               "LaunchedCmppct", "PassesAtt", "PassesThr", "PassesLaunchpct",
               "PassesAvgLen", "GoalKicksAtt", "GoalKicksLaunchpct", "GoalKicksAvgLen",
               "CrossesOpp", "CrossesStp", "CrossesStppct", "SweeperOPA", "SweeperOPAninety",
               "SweeperAvgDist")
    three <- tibble::as_tibble(sapply(three, function(x) character()))
    table <- html_doc %>% rvest::html_nodes("#stats_squads_keeper_adv_for")
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    names(df) <- paste0(names(df), df[1, ])
    names(df) <- stringr::str_replace_all(names(df), "-", "_plus_minus")
    names(df) <- stringr::str_replace_all(names(df), "%", "pct")
    names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
    names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
    df <- df[-1,]
    three <- plyr::rbind.fill(three,df)
    # 4 "#stats_squads_shooting_for"
    four <- c("VarSquad", "Var Pl", "Varninetys", "StandardGls", "StandardSh",
              "StandardSoT", "StandardSoTpct", "StandardShninety", "StandardSoTninety",
              "StandardGSh", "StandardGSoT", "StandardDist", "StandardFK",
              "StandardPK", "StandardPKatt", "ExpectedxG", "ExpectednpxG",
              "ExpectednpxGSh", "ExpectedGplusminusxG", "ExpectednpGplusminusxG")
    four <- tibble::as_tibble(sapply(four, function(x) character()))
    table <- html_doc %>% rvest::html_nodes("#stats_squads_shooting_for")
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    names(df) <- paste0(names(df), df[1, ])
    names(df) <- stringr::str_replace_all(names(df), "-", "_plus_minus")
    names(df) <- stringr::str_replace_all(names(df), "%", "pct")
    names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
    names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
    df <- df[-1,]
    four <- plyr::rbind.fill(four,df)
    # 5 "#stats_squads_passing_for"
    five <- c("VarSquad", "Var Pl", "Varninetys", "TotalCmp", "TotalAtt",
              "TotalCmppct", "TotalTotDist", "TotalPrgDist", "ShortCmp", "ShortAtt",
              "ShortCmppct", "MediumCmp", "MediumAtt", "MediumCmppct", "LongCmp",
              "LongAtt", "LongCmppct", "VarAst", "VarxA", "VarAplusminusxA",
              "VarKP", "Var", "VarPPA", "VarCrsPA", "VarProg")
    five <- tibble::as_tibble(sapply(five, function(x) character()))
    table <- html_doc %>% rvest::html_nodes("#stats_squads_passing_for")
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    names(df) <- paste0(names(df), df[1, ])
    names(df) <- stringr::str_replace_all(names(df), "-", "_plus_minus")
    names(df) <- stringr::str_replace_all(names(df), "%", "pct")
    names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
    names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
    df <- df[-1,]
    five <- plyr::rbind.fill(five,df)
    # 6 "#stats_squads_passing_types_for"
    six <- c("VarSquad", "Var Pl", "Varninetys", "VarAtt", "PassTypesLive",
             "PassTypesDead", "PassTypesFK", "PassTypesTB", "PassTypesPress",
             "PassTypesSw", "PassTypesCrs", "PassTypesCK", "CornerKicksIn",
             "CornerKicksOut", "CornerKicksStr", "HeightGround", "HeightLow",
             "HeightHigh", "BodyPartsLeft", "BodyPartsRight", "BodyPartsHead",
             "BodyPartsTI", "BodyPartsOther", "OutcomesCmp", "OutcomesOff",
             "OutcomesOut", "OutcomesInt", "OutcomesBlocks")
    six <- tibble::as_tibble(sapply(six, function(x) character()))
    table <- html_doc %>% rvest::html_nodes("#stats_squads_passing_types_for")
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    names(df) <- paste0(names(df), df[1, ])
    names(df) <- stringr::str_replace_all(names(df), "-", "_plus_minus")
    names(df) <- stringr::str_replace_all(names(df), "%", "pct")
    names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
    names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
    df <- df[-1,]
    six <- plyr::rbind.fill(six,df)
    # 7 "#stats_squads_gca_for"
    seven <- c("VarSquad", "Var Pl", "Varninetys", "SCASCA", "SCASCAninety",
              "SCATypesPassLive", "SCATypesPassDead", "SCATypesDrib", "SCATypesSh",
              "SCATypesFld", "SCATypesDef", "GCAGCA", "GCAGCAninety", "GCATypesPassLive",
              "GCATypesPassDead", "GCATypesDrib", "GCATypesSh", "GCATypesFld",
              "GCATypesDef", "GCATypesOG")
    seven <- tibble::as_tibble(sapply(seven, function(x) character()))
    table <- html_doc %>% rvest::html_nodes("#stats_squads_gca_for")
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    names(df) <- paste0(names(df), df[1, ])
    names(df) <- stringr::str_replace_all(names(df), "-", "_plus_minus")
    names(df) <- stringr::str_replace_all(names(df), "%", "pct")
    names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
    names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
    df <- df[-1,]
    seven <- plyr::rbind.fill(seven,df)
    # 8 "#stats_squads_defense_for"
    eight <- c("VarSquad", "Var Pl", "Varninetys", "TacklesTkl", "TacklesTklW",
               "TacklesDef rd", "TacklesMid rd", "TacklesAtt rd", "VsDribblesTkl",
               "VsDribblesAtt", "VsDribblesTklpct", "VsDribblesPast", "PressuresPress",
               "PressuresSucc", "Pressurespct", "PressuresDef rd", "PressuresMid rd",
               "PressuresAtt rd", "BlocksBlocks", "BlocksSh", "BlocksShSv",
               "BlocksPass", "VarInt", "VarTklInt", "VarClr", "VarErr")
    eight <- tibble::as_tibble(sapply(eight, function(x) character()))
    table <- html_doc %>% rvest::html_nodes("#stats_squads_defense_for")
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    names(df) <- paste0(names(df), df[1, ])
    names(df) <- stringr::str_replace_all(names(df), "-", "_plus_minus")
    names(df) <- stringr::str_replace_all(names(df), "%", "pct")
    names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
    names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
    df <- df[-1,]
    eight <- plyr::rbind.fill(eight,df)
    # 9 "#stats_squads_possession_for"
    nine <- c("VarSquad", "Var Pl", "VarPoss", "Varninetys", "TouchesTouches",
              "TouchesDef Pen", "TouchesDef rd", "TouchesMid rd", "TouchesAtt rd",
              "TouchesAtt Pen", "TouchesLive", "DribblesSucc", "DribblesAtt",
              "DribblesSuccpct", "DribblesPl", "DribblesMegs", "CarriesCarries",
              "CarriesTotDist", "CarriesPrgDist", "CarriesProg", "Carries",
              "CarriesCPA", "CarriesMis", "CarriesDis", "ReceivingTarg", "ReceivingRec",
              "ReceivingRecpct", "ReceivingProg")
    nine <- tibble::as_tibble(sapply(nine, function(x) character()))
    table <- html_doc %>% rvest::html_nodes("#stats_squads_possession_for")
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    names(df) <- paste0(names(df), df[1, ])
    names(df) <- stringr::str_replace_all(names(df), "-", "_plus_minus")
    names(df) <- stringr::str_replace_all(names(df), "%", "pct")
    names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
    names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
    df <- df[-1,]
    nine <- plyr::rbind.fill(nine,df)
    # 10 "#stats_squads_playing_time_for"
    ten <- c("VarSquad", "Var Pl", "VarAge", "PlayingTimeMP", "PlayingTimeMin",
             "PlayingTimeMnMP", "PlayingTimeMinpct", "PlayingTimeninetys",
             "StartsStarts", "StartsMnStart", "StartsCompl", "SubsSubs", "SubsMnSub",
             "SubsunSub", "TeamSuccessPPM", "TeamSuccessonG", "TeamSuccessonGA",
             "TeamSuccessplusminus", "TeamSuccessplusminusninety", "TeamSuccessxGonxG",
             "TeamSuccessxGonxGA", "TeamSuccessxGxGplusminus", "TeamSuccessxGxGplusminusninety")
    ten <- tibble::as_tibble(sapply(ten, function(x) character()))
    table <- html_doc %>% rvest::html_nodes("#stats_squads_playing_time_for")
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    names(df) <- paste0(names(df), df[1, ])
    names(df) <- stringr::str_replace_all(names(df), "-", "_plus_minus")
    names(df) <- stringr::str_replace_all(names(df), "%", "pct")
    names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
    names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
    df <- df[-1,]
    ten <- plyr::rbind.fill(ten,df)
    # 11 "#stats_squads_misc_for"
    eleven <- c("VarSquad", "Var Pl", "Varninetys", "PerformanceCrdY", "PerformanceCrdR",
                "PerformanceTwoCrdY", "PerformanceFls", "PerformanceFld", "PerformanceOff",
                "PerformanceCrs", "PerformanceInt", "PerformanceTklW", "PerformancePKwon",
                "PerformancePKcon", "PerformanceOG", "PerformanceRecov", "AerialDuelsWon",
                "AerialDuelsLost", "AerialDuelsWonpct")
    eleven <- tibble::as_tibble(sapply(eleven, function(x) character()))
    table <- html_doc %>% rvest::html_nodes("#stats_squads_misc_for")
    table1 <- table %>% rvest::html_table()
    df <- as.data.frame(table1)
    names(df) <- paste0(names(df), df[1, ])
    names(df) <- stringr::str_replace_all(names(df), "-", "_plus_minus")
    names(df) <- stringr::str_replace_all(names(df), "%", "pct")
    names(df) <- stringr::str_replace_all(names(df), "90", "ninety")
    names(df) <- stringr::str_replace_all(names(df), "2C", "TwoC")
    names(df) <- gsub("[[:punct:]]|[[:digit:]]|^http:\\/\\/.*|^https:\\/\\/.*","",names(df))
    df <- df[-1,]
    eleven <- plyr::rbind.fill(eleven,df)
    # COMBINE
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
    final <- merge(final, ten, by =0, all = TRUE,suffixes = c("","tbl10"),sort=FALSE)
    final$Row.names <- NULL
    final <- merge(final, eleven, by =0, all = TRUE,suffixes = c("","tbl11"),sort=FALSE)
    final$Row.names <- NULL
    l1 <- c("squad", "players_used", "avg_age", "possession", "matches_played",
            "starts", "minutes", "90s", "goals",
            "assists", "non_penalty_goals", "penalty_goals", "penalties_attempted",
            "yellow_cards", "red_cards", "goals_per90", "assists_per90",
            "goals_and_assists_per90", "non_penalty_goals_per90", "non_penalty_goals_and_assists_per90", "expected_goals",
            "expected_non_penalty_goals", "expected_assists", "expected_non_penalty_goals_and_assists", "expected_goals_per90",
            "expected_assists_per90", "expected_goals_and_assists_per90", "expected_non_penalty_goals_per90", "expected_non_penalty_goals_and_assists_per90")
    l2 <- c("squad", "keepers_used", "matches_played", "keeper_starts",
            "keeper_minutes", "90s", "goals_against", "goals_against_per90",
            "shots_on_target_against", "saves", "save_pct",
            "wins", "draws", "losses", "clean_sheets",
            "clean_sheet_pct", "penalties_attempted_against", "penalties_converted_against", "penalties_saved",
            "penalties_missed", "penalty_save_pct")
    l3 <- c("squad", "keepers_used", "90s", "goals_against", "penalties_attempted_against",
            "free_kick_goals_against", "corner_kick_goals_against", "own_goals_against", "expected_goals_against", "expected_goals_against_per_shot_on_target",
            "expected_goals_against_minus_goals_allowed", "expected_goals_against_minus_goals_allowed_per90", "keeper_long_passes_completed", "keeper_long_passes_attempted",
            "keeper_long_passes_completion_pct", "keeper_passes_attempted", "keeper_throws_attempted", "keeper_long_pass_pct",
            "keeper_avg_pass_length", "goal_kicks_attempted", "goal_kick_long_pct", "goal_kick_avg_length",
            "crosses_attempted_against", "keeper_crosses_stopped", "keeper_crosses_stop_pct", "keeper_actions_outside18", "keeper_actions_outside18_per90",
            "keeper_actions_avg_distance_from_goal")
    l4 <- c("squad", "players_used", "90s", "goals", "shots",
            "shots_on_target", "shots_on_target_pct", "shots_per90", "shots_on_target_per90",
            "goals_per_shot", "goals_per_shot_on_target", "avg_distance_shot", "shots_free_kick",
            "penalties_converted", "penalties_attempted", "expected_goals", "expected_non_penalty_goals",
            "expected_non_penalty_goals_per_shot", "goals_minus_expected_goals", "non_penalty_goals_minus_expected_goals")
    l5 <- c("squad", "players_used", "90s", "passes_completed", "passes_attempted",
            "pass_completion_ct", "pass_total_distance", "pass_progressive_distance", "short_passes_completed", "short_passes_attempted",
            "short_pass_completion_pct", "medium_passes_completed", "medium_passes_attempted", "medium_pass_completion_pct", "long_passes_completed",
            "long_passes_attempted", "long_pass_completion_pct", "assists", "expected_assists", "assists_minus_expected_assists",
            "key_passes", "passes_attacking_third", "passes_into_18box", "crosses_into_18box", "progressive_passes")
    l6 <- c("squad", "players_used", "90s", "passes_attempted",
            "passes_live_ball","passes_dead_ball", "passes_free_kick", "passes_through_ball", "passes_under_press",
            "passes_pitch_wide", "crosses", "corner_kicks", "corners_inswinging",
            "corners_outswinging", "corners_straight", "passes_ground_level", "passes_low",
            "passes_high", "passes_left_foot", "passes_right_foot", "passes_head",
            "passes_throw_in", "passes_other_body_part", "passes_completed", "passes_offside",
            "passes_out_of_bounds", "passes_intercepted", "passes_blocked")
    l7 <- c("squad", "players_used", "90s", "shot_creating_actions", "shot_creating_actions_per90",
            "shot_creating_passes_live_ball", "shot_creating_passes_dead_ball", "shot_creating_Dribbles", "shot_creating_rebounds",
            "shot_creating_fouls_drawn", "shot_creating_defensive_actions", "goal_creating_actions", "goal_creating_actions_per90", "goal_creating_passes_live_ball",
            "goal_creating_passes_dead_ball", "goal_creating_dribbles", "goal_creating_rebounds", "goal_creating_fouls_drawn",
            "goal_creating_defensive_actions", "goal_creating_own_goals_for")
    l8 <- c("squad", "players_used", "90s", "tackles", "tackles_won",
            "tackles_defensive_third", "tackles_middle_third", "tackles_attacking_third", "dribbler_tackles_success",
            "dribbler_tackles_attemmpted", "dribbler_tackle_success_pct", "tackles_missed", "pressures_applied",
            "pressure_success", "pressure_success_pct", "pressures_defensive_third", "pressures_middle_third",
            "pressures_attacking_third", "def_total_blocks", "def_shots_blocked", "def_shots_on_target_blocked",
            "def_passes_blocked", "def_interceptions", "def_tackles_plus_interceptions", "def_clearances", "def_errors")
    l9 <- c("squad", "players_used", "possession_pct", "90s", "touches_total",
            "touches_def_pen", "touches_def_third", "touches_middle_third", "touches_attacking_third",
            "touches_attacking_pen", "touches_live_ball", "dribbles_successful", "dribbles_attempted",
            "dribbles_successful_pct", "players_dribbled_past", "nutmegs", "carry_count",
            "carry_distance", "carry_progressive_distance", "carry_progressive_count", "carry_into_attacking_third",
            "carries_into_18box", "loose_balls_missed", "carry_lost", "targeted_by_pass", "passes_received",
            "pass_received_pct", "progressive_passes_received")
    l10 <- c("squad", "players_used", "avg_age", "matches_played", "minutes_played",
             "minutes_played_per_match", "minutes_played_pct", "90s",
             "starts", "starter_avg_minutes", "complete_matches", "subs_used", "sub_avg_time",
             "unused_subs", "points_per_match", "goals", "goals_against",
             "goal_difference", "goal_difference_per90", "expected_goals",
             "expected_goals_against", "expected_goal_difference", "expected_goal_difference_per90")
    l11 <- c("squad", "players_used", "90s", "yellow_cards", "red_cards",
             "second_yellow_cards", "fouls_committed", "fouls_drawn", "offsides",
             "crosses", "interceptions", "tackles_won", "penalties_won",
             "penalties_conceded", "own_goals", "loose_ball_recoveries", "aerials_won",
             "aerials_lost", "aerial_win_pct")
    final_names <- c(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11)
    names(final) <- final_names
    final <-  final[!duplicated(colnames(final))]
    final[2:216]<- sapply(final[2:216], gsub, pattern=",", replacement="")
    suppressWarnings({final[2:216] <- sapply(final[2:216],as.numeric)})
    final <- tibble::add_column(final, season = season, .before = 2)
    final$season <- season
    total <- dplyr::bind_rows(total,final)
    Sys.sleep(1)
  }
  oldValue <- c("Atlanta","Chicago","Colorado","Columbus","Houston","Miami","Minnesota"
                ,"Montreal","Nashville","New England","NY Red Bulls","NYCFC",
                "Orlando City","Philadelphia","Portland","San Jose","Seattle","Sporting KC",
                "Vancouver","KC Wizards","MetroStars","Dallas","KC Wiz")
  newValue <- c("Atlanta United FC","Chicago Fire FC","Colorado Rapids","Columbus Crew SC",
                "Houston Dynamo","Inter Miami CF","Minnesota United FC","CF Montreal","Nashville SC",
                "New England Revolution","New York Red Bulls","New York City FC",
                "Orlando City SC","Philadelphia Union","Portland Timbers","San Jose Earthquakes",
                "Seattle Sounders FC","Sporting Kansas City","Vancouver Whitecaps FC","Sporting Kansas City",
                "New York Red Bulls","FC Dallas","Sporting Kansas City")
  suppressWarnings({total$squad <- plyr::mapvalues(total$squad, oldValue, newValue)})
  total$squad <- ifelse((total$squad == "Inter Miami CF") & (total$season < 2019),"defunct Miami",total$squad)
  return(total)
}
