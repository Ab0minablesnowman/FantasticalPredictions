#' Scraping Projections from variety of sources
#' @param season Specific NFL season
#' @param week Specific Week within NFL Season
#' @export
ScrapewithProjections<-function(season, week){

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
  my_scrape <- suppressMessages(scrape_data(src = c("ESPN", "FantasyPros", "FantasySharks", "FFToday",
                                                    "NumberFire", "FantasyFootballNerd", "NFL","CBS"),
                                            pos = c("QB", "RB", "WR", "TE", "DST", "K"),
                                            season = {season}, week = {week}))

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

  my_scrapeMapper<-quiet(unique((my_scrapeDF %>%
                                   filter(data_src == "NFL") %>%
                                   select(id, player, pos, team)) %>%
                                  mutate(player = ifelse(is.na(player), team, player),
                                         pos = ifelse(is.na(pos),"DST", pos))))
  appendids<-my_scrapeMapper %>%
    filter(pos == "DST") %>%
    mutate(gsis_id = id,
           name = player) %>%
    select(id, gsis_id, name)
  appendposition<-my_scrapeMapper %>%
    filter(pos == "DST") %>%
    mutate(gsis_id = id,
           name = player) %>%
    select(id, gsis_id, name)

  Depthchart<- nflreadr::load_depth_charts() %>%
    filter(season == {season}, week == {week}, position %in% Depthchartmapper) %>%
    select(gsis_id, full_name, team, position, depth_team)

  DF<- suppressMessages(quiet(my_scrapeDF[, !names(my_scrapeDF) %in% c("pos", "src_id")] %>%
                                add_player_info() %>%
                                mutate(team = team.x) %>%
                                unite( "Name", first_name:last_name, sep=" ")))
  DF<-DF[, !names(DF) %in% c("team.y", "age", "exp", "player","team.x","first_name","last_name","chg","tm",
                             "fg_miss", "xp_miss", "xp_att", "fg_0019", "fg_2029", "fg_3039", "fg_4049", "fg_50",
                             "site_ci_low", "site_ci_high", "opp_team", "opp_team_rank", "ranks_pos", "...27",
                             "draftkings_site_points", "yahoo_site_points", "games", "fg_long", "fg_att_2029",
                             "fg_att_3039", "fg_att_4049", "fg_att_50", "site_fppg", "...23", "return_tds",
                             "dst_tackles", "dst_pts_allowed_g", "dst_pass_yds_allowed", "dst_rush_yds_allowed",
                             "dst_avg_yds_allowed", "rec_09_tds", "rec_1019_tds", "rec_2029_tds", "rec_3039_tds",
                             "rec_4049_tds", "rec_50_tds", "kick_ret_yds", "...26", "ranks_ovr", "rec_yds_g",
                             "rec_avg", "...17", "two_pts", "rush_avg", "...19", "pass_09_tds", "pass_1019_tds",
                             "pass_2029_tds", "pass_3039_tds", "pass_4049_tds", "pass_50_tds", "sacks", "pass_ints",
                             "...36", "pass_yds_g", "pass_rate", "rush_09_tds", "rush_1019_tds", "rush_2029_tds",
                             "rush_3039_tds", "rush_4049_tds", "rush_50_tds", "rec_50_yds", "rec_100_yds", "...29",
                             "...37","opp", "...14","...35")]
  FantasyIDforScrapes<-load_ff_playerids() %>%
    filter(position %in% Depthchartmapper, gsis_id !="NA") %>%
    mutate(id = mfl_id) %>%
    select(id,gsis_id, name)

  AddedFantasyIDforScrapse<-rbind(FantasyIDforScrapes, appendids)

  DF<-left_join(AddedFantasyIDforScrapse, DF,  by = "id") %>%
    filter(data_src != "NA")

  CleanDF<-left_join(DF,AbbrevationMapper, by = "team")

  AvgProjectsDF<-CleanDF %>%
    group_by(id, gsis_id, name, position, team) %>%
    summarise_all("mean", na.rm=TRUE)

  AvgProjectsDF<-AvgProjectsDF[, !names(AvgProjectsDF) %in%
                                 c("Name","data_src", "opp", "Team", "site_pts")]

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
           Week = week,
           Season = season)

  ProjectionsFinal<-full_join(AvgProjectsDF, AbbrevationMapper, by="team") %>%
    mutate(team = Team)
  ProjectionsFinal<-ProjectionsFinal[, !names(ProjectionsFinal) %in% "Team"]
  ProjectionsFinal<-moveMe(ProjectionsFinal, c("Season", "Week"), "first")
  ProjectionsFinal<-ProjectionsFinal[, !names(ProjectionsFinal) %in%
                                       c("id","gsis_id")]

  ProjectionsFinal<-ProjectionsFinal[order(
    ProjectionsFinal$position,-ProjectionsFinal$HalfPPR), ]

  #MyOwnScrapes for Lines#
  tr_url<-glue::glue(
    "http://gridirongames.com/nfl-weekly-betting-lines/?Year={season}&Week={week}")
  theurl <- getURL(tr_url,.opts = list(ssl.verifypeer = FALSE) )
  tables <- readHTMLTable(theurl)
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  tables2<- tables[[which.max(n.rows)]]
  colnames(tables2)<-c("First",
                       "Spread",
                       "Moneyline",
                       "Total",
                       "Unknown")
  Finaltable<-tables2 %>%
    filter(tables2$Spread != "Spread")
  Finaltable<-Finaltable %>%
    mutate(Teams = ifelse(grepl('^F', First),Spread,First),
           NewSpread = ifelse(Spread==Teams,Moneyline, Spread),
           NewTotal = ifelse(grepl('^u', Total),Total,Unknown))
  Finaltable$NewTotal<-str_sub(Finaltable$NewTotal, 2)
  Finaltable$NewTotal<-as.numeric(Finaltable$NewTotal)
  Finaltable$NewSpread<-as.numeric(Finaltable$NewSpread)
  Finaltable<-Finaltable %>%
    mutate(Vegas_Implied_Score = ((Finaltable$NewTotal*.5)-(Finaltable$NewSpread/2)),
           team = Teams) %>%
    select(team, NewSpread, NewTotal, Vegas_Implied_Score)


  #Joining final product#
  ProjectionsFinal$team<-ifelse(ProjectionsFinal$team == "LA","LAR", ProjectionsFinal$team)
  ProjectionsFinal<-full_join(ProjectionsFinal,Finaltable, by = "team") %>%
    filter(team != "NA")

  #Opponent#
  SchedulesForScrapes<-load_schedules({season}) %>%
    mutate(Week = week) %>%
    select(home_team, away_team, Week)

  SchedulesForScrapes<-SchedulesForScrapes %>%
    filter(Week == {week}) %>%
    select(home_team, away_team, Week)


  SchedulesForScrapes1<-rep(1:nrow(SchedulesForScrapes), 2)
  SchedulesForScrapes2<- SchedulesForScrapes[SchedulesForScrapes1, ]
  SchedulesForScrapes3<-SchedulesForScrapes2

  SchedulesForScrapes3[17:32,1]<-SchedulesForScrapes2[17:32,2]
  SchedulesForScrapes3[17:32,2]<-SchedulesForScrapes2[17:32,1]

  SchedulesForScrapes4 <- SchedulesForScrapes3%>%
    mutate(team = home_team,
           opponent = away_team,
           Week = week) %>%
    select(Week, team, opponent)

  SchedulesForScrapesFinal <- SchedulesForScrapes4%>%
    filter(Week == {week}) %>%
    mutate(team = ifelse(team=="LA","LAR",team),
           opponent = ifelse(opponent=="LA","LAR",opponent))

  ProjectionsFinal <- full_join(ProjectionsFinal, SchedulesForScrapesFinal)


  #From Stats for Defensive Strength#
  #Prep#
  Schedules<-load_schedules({season})

  PBPStats <- load_pbp({season})

  PBPStatsOpp<-PBPStats %>%
    select(season, week,posteam,home_team,away_team,game_id) %>%
    mutate(
      recent_team = posteam,
      Opponent = ifelse(posteam==home_team,away_team,home_team))%>%
    filter(season=={season})

  PBPStatsOpp2<-PBPStatsOpp %>%
    mutate(recent_team = ifelse(recent_team == "LA","LAR",recent_team),
           Opponent = ifelse(Opponent == "LA","LAR",Opponent))

  PBPStatsHA<-PBPStats %>%
    filter(season_type == "REG") %>%
    select(away_team, home_team, week, away_score, home_score)
  PBPStats2<-unique(PBPStatsHA)

  PBPStatshome <- PBPStats2%>%
    mutate(recent_team=home_team,
           finalscore = home_score) %>%
    select(week, recent_team, finalscore)

  PBPStatsaway <- PBPStats2%>%
    mutate(recent_team=away_team,
           finalscore = away_score) %>%
    select(week, recent_team, finalscore)

  FinalScores<-full_join(PBPStatshome, PBPStatsaway, by=c("week","recent_team","finalscore")) %>%
    mutate(team = ifelse(recent_team == "LA","LAR",recent_team),
           Week = week)%>%
    group_by(team) %>%
    summarise(Avg_Team_Score = mean(finalscore))

  PlayerStatsOriginal <- nflreadr::load_player_stats({season},"offense") %>%
    mutate(recent_team = ifelse(recent_team == "LA","LAR",recent_team))

  DepthCharts <- nflreadr::load_depth_charts() %>%
    filter(week == max(week), formation == "Offense"|formation=="Special Teams") %>%
    mutate(depth_position = paste(position,
                                  depth_team,sep=""),
           player = full_name,
           player_id = gsis_id) %>%
    select(player, player_id, position)

  #Snapcounts#
  AllFantasyID<-load_ff_playerids()
  FantasyIDforPFR<-load_ff_playerids() %>%
    select(pfr_id,gsis_id)
  SnapCounts <- load_snap_counts() %>%
    filter(season == {season}) %>%
    mutate(pfr_id = pfr_player_id)
  SnapCounts<-left_join(SnapCounts,FantasyIDforPFR,"pfr_id") %>%
    mutate(player_id = gsis_id,
           week = substring(SnapCounts$game_id,6,7)) %>%
    select(week, player_id,game_id, offense_snaps, offense_pct)
  SnapCounts$week<-as.numeric(as.character(SnapCounts$week))

  SnapCounts<-full_join(SnapCounts, DepthCharts, "player_id")
  SnapCounts<-SnapCounts %>%
    filter(game_id!="#NA", player!="#NA")
  SnapCounts<-unique(SnapCounts)

  #Stats#
  CurrentRoster<- nflreadr::load_rosters({season}) %>%
    mutate(player_id = gsis_id)
  PositionMapper<-CurrentRoster %>%
    filter(position %in% PositionMapperBase) %>%
    select(player_id,position, full_name)


  PlayerStats<-left_join(PlayerStatsOriginal, PositionMapper, by = "player_id") %>%
    filter(fantasy_points_ppr>(-2), player_name!='#NA',position!='#NA')

  PlayerStats2<-full_join(PlayerStats, PBPStatsOpp2, by=c("season", "week", "recent_team"
  ))

  Stats<-full_join(PlayerStats2,SnapCounts, by=c("player_id","week","game_id")) %>%
    group_by(player_id) %>%
    ungroup() %>%
    filter(player_name!="#NA") %>%
    mutate(position = position.x)

  Stats<-Stats[, !names(Stats) %in% c("position.x", "position.y")]

  StatsByOpponent<-Stats %>%
    mutate(Fumbles = sack_fumbles+rushing_fumbles+receiving_fumbles,
           Fumbles_Lost = sack_fumbles_lost+rushing_fumbles_lost+receiving_fumbles_lost) %>%
    group_by(player_id, full_name, position, recent_team, season, week, Opponent) %>%
    summarise_all("mean") %>%
    ungroup() %>%
    group_by(player_id, full_name, position,recent_team, season, week) %>%
    select(completions, attempts, passing_yards, passing_tds, interceptions, sacks, sack_yards,
           passing_air_yards, passing_yards_after_catch, passing_first_downs, passing_epa,
           passing_2pt_conversions, carries, rushing_yards, rushing_tds,
           rushing_first_downs, rushing_epa, rushing_2pt_conversions, receptions, targets,
           receiving_yards, receiving_tds, receiving_air_yards, receiving_yards_after_catch,
           receiving_first_downs, receiving_epa, receiving_2pt_conversions, target_share,
           Fumbles, Fumbles_Lost, fantasy_points_ppr, offense_snaps, offense_pct, Opponent)

  StatsByOpponent<-unique(StatsByOpponent)
  StatsByOpponentPosition<-StatsByOpponent %>%
    group_by(Opponent, position, season) %>%
    summarise_all("mean") %>%
    ungroup() %>%
    group_by(position) %>%
    mutate(DefensiverankingPosition= ave(fantasy_points_ppr, position, FUN=rank),
           AvgPPRagainst = fantasy_points_ppr,
           opponent = Opponent) %>%
    ungroup() %>%
    group_by(opponent, position, season) %>%
    select(AvgPPRagainst, DefensiverankingPosition)


  #Combine Ranking and projections#
  ProjectionsFinalFinal <- suppressMessages(left_join(ProjectionsFinal,StatsByOpponentPosition))

  ProjectionsFinalFinal<-ProjectionsFinalFinal[, !names(ProjectionsFinalFinal) %in% "season"]

  ProjectionsFinaltheThird<- left_join(ProjectionsFinalFinal, FinalScores, by="team")
  #Final Product#
  ProjectionsFinaltheThird

}
