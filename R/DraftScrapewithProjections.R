#' Scraping Projections from variety of sources for FF Draft
#' @param season Specific NFL season
#' @export
#' @examples
#' Draftprojections<-DraftScrapewithProjections(2022)
#'
DraftScrapewithProjections<-function(season){

  #Mappers#
  Depthchartmapper<- c("QB","RB","WR","TE","K","PK","DST")
  Teams<-data.frame(abbreviation = rep(c("SF",  "GB",  "ARI",  "NE",  "TEN",  "DAL",  "CHI",
                                         "SEA",  "CLE",  "NO",  "PIT",  "TB",  "KC",  "BAL",
                                         "LAC",  "NYJ",  "LA",  "ATL",  "HOU",  "IND",  "BUF",
                                         "MIN",  "LV",  "WAS",  "NYG",  "DEN",  "MIA",  "CIN",
                                         "CAR",  "PHI",  "DET",  "JAX",  "LAR")),
                    team = rep(c("San Francisco 49ers",  "Green Bay Packers",  "Arizona Cardinals",
                                 "New England Patriots",  "Tennessee Titans",  "Dallas Cowboys",  "Chicago Bears",
                                 "Seattle Seahawks",  "Cleveland Browns",  "New Orleans Saints",  "Pittsburgh Steelers",
                                 "Tampa Bay Buccaneers",  "Kansas City Chiefs",  "Baltimore Ravens",
                                 "Los Angeles Chargers",  "New York Jets",  "Los Angeles Rams",  "Atlanta Falcons",
                                 "Houston Texans",  "Indianapolis Colts",  "Buffalo Bills",  "Minnesota Vikings",
                                 "Las Vegas Raiders",  "Washington Football Team",  "New York Giants",  "Denver Broncos",
                                 "Miami Dolphins",  "Cincinnati Bengals",  "Carolina Panthers",  "Philadelphia Eagles",
                                 "Detroit Lions",  "Jacksonville Jaguars",  "Los Angeles Rams")))
  AbbrevationMapper<-data.frame(team = rep(c("TBB","ATL","BUF","CAR","CIN","DET","HOU",
                                             "IND","TEN","WAS","KCC","NEP","NOS","NYG",
                                             "LAR","LVR","CHI","CLE","JAC","MIA","NYJ",
                                             "PHI","PIT","ARI","LAC","SEA","BAL","GBP",
                                             "DEN","MIN","SFO","DAL")),
                                Team = rep(c("TB","ATL","BUF","CAR","CIN","DET","HOU","IND",
                                             "TEN","WAS","KC","NE","NO","NYG","LA","LV","CHI",
                                             "CLE","JAX","MIA","NYJ","PHI","PIT","ARI","LAC",
                                             "SEA","BAL","GB","DEN","MIN","SF","DAL")))

  PositionMapperBase<- c("QB","WR","RB","TE","K","PK")

  DSTMapper<-data.frame(team = rep(c("BUF","IND","MIA","NE",
                                     "NYJ", "CIN","CLE","TEN",
                                     "JAX","PIT","DEN","KC",
                                     "LV", "LAC","SEA","DAL",
                                     "NYG","PHI","ARI","WAS",
                                     "CHI","DET","GB","MIN",
                                     "TB","ATL","CAR","LAR",
                                     "NO","SF","BAL","HOU")),
                        def_name = rep(c("Buffalo Bills","Indianapolis Colts","Miami Dolphins",
                                         "New England Patriots","New York Jets","Cincinnati Bengals",
                                         "Cleveland Browns","Tennessee Titans", "Jacksonville Jaguars",
                                         "Pittsburgh Steelers","Denver Broncos","Kansas City Chiefs",
                                         "Las Vegas Raiders","Los Angeles Chargers","Seattle Seahawks",
                                         "Dallas Cowboys","New York Giants","Philadelphia Eagles",
                                         "Arizona Cardinals","Washington FootballTeam","Chicago Bears",
                                         "Detroit Lions","Green Bay Packers","Minnesota Vikings",
                                         "Tampa Bay Buccaneers","Atlanta Falcons","Carolina Panthers",
                                         "Los Angeles Rams","New Orleans Saints","San Francisco 49ers",
                                         "Baltimore Ravens","Houston Texans")))

  TeamConf<-data.frame(team_abbr = rep(c("BUF","CIN","HOU","IND",
                                         "TEN","KC","NE","LV",
                                         "CLE","JAX","MIA","NYJ",
                                         "PIT","LAC", "BAL", "DEN",
                                         "TB","ATL", "CAR", "DET",
                                         "WAS","NO", "NYG", "LAR",
                                         "CHI","PHI", "ARI", "SEA",
                                         "GB", "MIN", "SF", "DAL")),
                       Conference = rep(c("AFC", "AFC", "AFC", "AFC",
                                          "AFC", "AFC", "AFC", "AFC",
                                          "AFC", "AFC", "AFC", "AFC",
                                          "AFC", "AFC", "AFC", "AFC",
                                          "NFC", "NFC", "NFC", "NFC",
                                          "NFC", "NFC", "NFC", "NFC",
                                          "NFC", "NFC", "NFC", "NFC",
                                          "NFC", "NFC", "NFC", "NFC")),
                       Division = rep(c("AFC East", "AFC North", "AFC South", "AFC South",
                                        "AFC South", "AFC West", "AFC East", "AFC West",
                                        "AFC North", "AFC South", "AFC East", "AFC East",
                                        "AFC North", "AFC West", "AFC North", "AFC West",
                                        "NFC South", "NFC South", "NFC South", "NFC North",
                                        "NFC East", "NFC South", "NFC East", "NFC West",
                                        "NFC North", "NFC East", "NFC West", "NFC West",
                                        "NFC North", "NFC North", "NFC West", "NFC East")))



  #Scrape from FFAnalytics#
  my_scrape <- suppressMessages(scrape_data(src = c("ESPN", "FantasyPros", "FantasySharks",
                                                    "NumberFire", "FantasyFootballNerd", "NFL","CBS"),
                                            pos = c("QB", "RB", "WR", "TE", "DST", "K"),
                                            season = {season}, week = 0))

  #Taking out individual Stats from FFAnalytics#
  my_scrapeDF<-suppressMessages(quiet(full_join(
    do.call(cbind.data.frame, my_scrape$K),
    full_join(
      do.call(cbind.data.frame, my_scrape$DST),
      full_join(
        do.call(cbind.data.frame, my_scrape$TE),
        full_join(
          do.call(cbind.data.frame, my_scrape$WR),
          full_join(
            do.call(cbind.data.frame, my_scrape$QB),
            do.call(cbind.data.frame, my_scrape$RB))))))) %>%
      filter(data_src !="NA"))

  DF1<- suppressMessages(quiet(my_scrapeDF[, !names(my_scrapeDF) %in% c("pos", "src_id")] %>%
                                 add_player_info() %>%
                                 mutate(team = team.x) %>%
                                 unite( "name", first_name:last_name, sep=" ")))
  DF2<-DF1[, !names(DF1) %in% c("team.y", "age", "exp", "player","team.x","first_name","last_name","chg","tm",
                                "fg_miss", "xp_miss", "xp_att",
                                "site_ci_low", "site_ci_high", "opp_team", "opp_team_rank", "ranks_pos", "...27",
                                "draftkings_site_points", "yahoo_site_points", "games",  "site_fppg", "...23", "return_tds",
                                "kick_ret_yds", "...26", "ranks_ovr", "rec_yds_g",
                                "rec_avg", "...17", "two_pts", "rush_avg", "...19",  "pass_ints",
                                "...36", "pass_yds_g", "pass_rate",  "...29",
                                "...37","opp", "...14","...35")]

  CleanDF<-left_join(DF2,AbbrevationMapper, by = "team")

  AvgProjectsDF<-CleanDF %>%
    group_by(id, name, position, team) %>%
    summarise_all("mean", na.rm=TRUE)

  AvgProjectsDF<-AvgProjectsDF[, !names(AvgProjectsDF) %in%
                                 c("data_src", "opp", "Team", "site_pts")]

  AvgProjectsDF[is.na(AvgProjectsDF)]<-0

  #Scoring#
  AvgProjectsDF<-AvgProjectsDF %>%
    mutate(Standard =.04*.data$pass_yds +4*.data$pass_tds +-2*.data$pass_int +
             .1*(.data$rush_yds + .data$rec_yds) +6*(.data$rush_tds + .data$rec_tds) +
             -2*(.data$fumbles_lost) +3*.data$fg+.data$xp+
             .data$dst_fum_force +.data$dst_fum_rec+.data$dst_int*2+
             .data$dst_td*6+.data$dst_safety*2+
             if_else(.data$position == "DST",(
               if_else(.data$dst_pts_allowed<6, 10,
                       if_else(.data$dst_pts_allowed<20, 7,
                               if_else(.data$dst_pts_allowed<34, 4,
                                       if_else(.data$dst_pts_allowed < 99, 0,0)))))
               ,0),
           HalfPPR = .data$Standard + (.5*.data$rec),
           PPR = .data$Standard + .data$rec,
           Season = season)

  ProjectionsFinal<-full_join(AvgProjectsDF, AbbrevationMapper, by="team") %>%
    mutate(team = Team)
  ProjectionsFinal<-ProjectionsFinal[, !names(ProjectionsFinal) %in% "Team"]
  ProjectionsFinal<-moveMe(ProjectionsFinal, c("Season"), "first")
  ProjectionsFinal<-ProjectionsFinal[, !names(ProjectionsFinal) %in%
                                       c("id","gsis_id")]

  ProjectionsFinal<-ProjectionsFinal[order(
    ProjectionsFinal$position,-ProjectionsFinal$HalfPPR), ] %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    filter(team != "NA") %>%
    elect("Season","name","position","team",
          "pass_yds","pass_tds","pass_att","pass_comp","pass_int",
          "pass_250_yds", "pass_300_yds","pass_350_yds","pass_2029_tds",
          "pass_3039_tds", "pass_4049_tds", "pass_50_tds", "sacks",

          "rec","rec_yds","rec_tds","rec_tgt","rec_rz_tgt","rec_50_yds", "rec_100_yds",
          "rec_150_yds","rec_200_yds","rec_09_tds", "rec_1019_tds",
          "rec_2029_tds", "rec_3039_tds", "rec_4049_tds", "rec_50_tds",

          "rush_yds","rush_tds","rush_att","rush_50_yds","rush_100_yds",
          "rush_09_tds", "rush_1019_tds", "rush_2029_tds", "rush_3039_tds",
          "rush_4049_tds", "rush_50_tds",

          "fumbles_lost",

          "fg","fg_att","xp","fg_att_0019","fg_0019", "fg_att_2029","fg_2029",
          "fg_att_3039","fg_3039", "fg_att_4049","fg_4049", "fg_att_50","fg_50",

          "dst_sacks", "dst_int","dst_fum_rec","dst_fum_force", "dst_td","dst_safety","dst_pts_allowed", "dst_yds_allowed",
          "dst_yds_199","dst_yds_299","dst_yds_349","dst_yds_399",
          "dst_yds_449","dst_yds_499","dst_yds_549","dst_yds_550",
          "dst_pts_6","dst_pts_13","dst_pts_17","dst_pts_20","dst_pts_27","dst_pts_34","dst_pts_45",
          "dst_tackles", "dst_pts_allowed_g", "dst_pass_yds_allowed", "dst_rush_yds_allowed",
          "dst_avg_yds_allowed",

          "Standard","HalfPPR","PPR")


  ProjectionsFinal

}
