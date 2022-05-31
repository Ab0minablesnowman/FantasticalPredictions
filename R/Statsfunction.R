#' Stats during NFL season from nflVerse Packages
#' @param seasons Specific NFL season
#' @param stat_type Type of Stats ("players", "defense_by_position","defense_by_depthposition","teams")
#' @export

Statsfunction<- function (seasons, stat_type = c("players", "defense_by_position","defense_by_depthposition","teams")){
  options(digits = 2)
  #Mappers#
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

  #Prep#
  Schedules<-load_schedules({seasons})

  PBPStats <- load_pbp({seasons}) %>%
    select(season, week,posteam,home_team,away_team,game_id) %>%
    mutate(
      recent_team = posteam,
      Opponent = ifelse(posteam==home_team,away_team,home_team))%>%
    filter(season=={seasons})

  PlayerStatsOriginal <- nflreadr::load_player_stats({seasons},"offense") %>%
    filter(season_type == "REG")

  DepthCharts <- nflreadr::load_depth_charts() %>%
    filter(week == max(week), formation == "Offense"|formation=="Special Teams") %>%
    mutate(depth_position = paste(position,
                                  depth_team,sep=""),
           player = full_name,
           player_id = gsis_id) %>%
    select(player, player_id, position)
  #removed depth position for select - causing issues#


  #Snapcounts#
  AllFantasyID<-load_ff_playerids()
  FantasyIDforPFR<-load_ff_playerids() %>%
    select(pfr_id,gsis_id)
  SnapCounts <- load_snap_counts() %>%
    filter(season == {seasons}) %>%
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
  CurrentRoster<- nflreadr::load_rosters({seasons}) %>%
    mutate(player_id = gsis_id)
  PositionMapper<-CurrentRoster %>%
    filter(position %in% PositionMapperBase) %>%
    select(player_id,position, full_name)


  PlayerStats<-left_join(PlayerStatsOriginal, PositionMapper, by = "player_id") %>%
    filter(fantasy_points_ppr>(-2), player_name!='#NA',position!='#NA')

  PlayerStats2<-full_join(PlayerStats, PBPStats, by=c("season", "week", "recent_team"
  ))

  Stats<-full_join(PlayerStats2,SnapCounts, by=c("player_id","week","game_id")) %>%
    group_by(player_id) %>%
    ungroup() %>%
    filter(player_name!="#NA") %>%
    mutate(position = position.x)

  Stats<-Stats[, !names(Stats) %in% c("position.x", "position.y")] %>%
    mutate(Fumbles = sack_fumbles+rushing_fumbles+receiving_fumbles,
           Fumbles_Lost = sack_fumbles_lost+rushing_fumbles_lost+receiving_fumbles_lost)

  Stats<-Stats %>%
    mutate(Standard =.04*.data$passing_yards +4*.data$passing_tds +-2*.data$interceptions +
             .1*(.data$rushing_yards + .data$receiving_yards) +6*(.data$rushing_tds + .data$receiving_tds) +
             -2*(.data$Fumbles_Lost),
           HalfPPR = .data$Standard + (.5*.data$receptions),
           PPR = .data$Standard + .data$receptions)

  #Stat Choices#
  if (stat_type == "players"){
    SeasonStats<-Stats %>%
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
             Fumbles, Fumbles_Lost, Standard, HalfPPR, PPR, offense_snaps, offense_pct, Opponent)

    SeasonStats<-unique(SeasonStats)

    output<-SeasonStats
  }
  if (stat_type == "defense_by_position"){
    StatsByOpponent<-Stats %>%
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
             Fumbles, Fumbles_Lost, Standard, HalfPPR, PPR, offense_snaps, offense_pct, Opponent)

    StatsByOpponent<-unique(StatsByOpponent)
    StatsByOpponentPosition<-StatsByOpponent%>%
      group_by(Opponent, position, season) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      ungroup() %>%
      group_by(position) %>%
      mutate(DefensiverankingPosition= ave(HalfPPR, position, FUN=rank),
             Forcedfumbles = Fumbles,
             Fumblesrecovered = Fumbles_Lost) %>%
      ungroup() %>%
      group_by(Opponent, position, season) %>%
      select(completions, attempts, passing_yards, passing_tds, interceptions, sacks, sack_yards,
             passing_air_yards, passing_yards_after_catch, passing_first_downs, passing_epa,
             passing_2pt_conversions, carries, rushing_yards, rushing_tds,
             rushing_first_downs, rushing_epa, rushing_2pt_conversions, receptions,
             receiving_yards, receiving_tds, receiving_air_yards, receiving_yards_after_catch,
             receiving_first_downs, receiving_epa, receiving_2pt_conversions,  Forcedfumbles, Fumblesrecovered,
             Standard, HalfPPR, PPR, DefensiverankingPosition)

    StatsByOpponentPosition<-StatsByOpponentPosition %>%
      rename_with(.fn = ~paste0(., "_against"),
                  .cols = c("completions", "attempts",
                            "passing_yards", "passing_tds",
                            "interceptions","passing_air_yards",
                            "passing_yards_after_catch",
                            "passing_first_downs", "passing_epa",
                            "passing_2pt_conversions", "carries", "rushing_yards",
                            "rushing_tds","rushing_first_downs", "rushing_epa",
                            "rushing_2pt_conversions", "receptions","receiving_yards",
                            "receiving_tds", "receiving_air_yards", "receiving_yards_after_catch",
                            "receiving_first_downs", "receiving_epa", "receiving_2pt_conversions",
                            "Standard", "HalfPPR", "PPR")) %>%
      mutate(across(c(1:29),.fns = ~./max(Stats$week)))

    output<-StatsByOpponentPosition
  }
  if (stat_type == "defense_by_depthposition"){
    StatsByOpponentdepth<-Stats %>%
      group_by(player_id, full_name, position, recent_team, season, week, Opponent, depth_position) %>%
      summarise_all("mean") %>%
      ungroup() %>%
      group_by(player_id, full_name, position,depth_position,recent_team, season, week) %>%
      select(completions, attempts, passing_yards, passing_tds, interceptions, sacks, sack_yards,
             passing_air_yards, passing_yards_after_catch, passing_first_downs, passing_epa,
             passing_2pt_conversions, carries, rushing_yards, rushing_tds,
             rushing_first_downs, rushing_epa, rushing_2pt_conversions, receptions, targets,
             receiving_yards, receiving_tds, receiving_air_yards, receiving_yards_after_catch,
             receiving_first_downs, receiving_epa, receiving_2pt_conversions, target_share,
             Fumbles, Fumbles_Lost, Standard, HalfPPR, PPR, offense_snaps, offense_pct, Opponent)

    StatsByOpponentDepthPosition<-StatsByOpponentdepth %>%
      group_by(Opponent, position,depth_position, season) %>%
      summarise_all("mean") %>%
      ungroup() %>%
      group_by(position) %>%
      mutate(DefensiverankingDepthPosition= ave(HalfPPR, depth_position, FUN=rank),
             Forcedfumbles = Fumbles,
             Fumblesrecovered = Fumbles_Lost) %>%
      ungroup() %>%
      group_by(Opponent, position, depth_position,season) %>%
      select(completions, attempts, passing_yards, passing_tds, interceptions, sacks, sack_yards,
             passing_air_yards, passing_yards_after_catch, passing_first_downs, passing_epa,
             passing_2pt_conversions, carries, rushing_yards, rushing_tds,
             rushing_first_downs, rushing_epa, rushing_2pt_conversions, receptions,
             receiving_yards, receiving_tds, receiving_air_yards, receiving_yards_after_catch,
             receiving_first_downs, receiving_epa, receiving_2pt_conversions,  Forcedfumbles, Fumblesrecovered,
             Standard, HalfPPR, PPR, DefensiverankingDepthPosition)

    StatsByOpponentDepthPosition<-StatsByOpponentDepthPosition %>%
      rename_with(.fn = ~paste0(., "_against"),
                  .cols = c("completions", "attempts",
                            "passing_yards", "passing_tds",
                            "interceptions","passing_air_yards", "passing_yards_after_catch",
                            "passing_first_downs", "passing_epa",
                            "passing_2pt_conversions", "carries", "rushing_yards",
                            "rushing_tds","rushing_first_downs", "rushing_epa",
                            "rushing_2pt_conversions", "receptions","receiving_yards",
                            "receiving_tds", "receiving_air_yards", "receiving_yards_after_catch",
                            "receiving_first_downs", "receiving_epa", "receiving_2pt_conversions",
                            "Standard", "HalfPPR", "PPR"))

    output<-StatsByOpponentDepthPosition
  }
  if (stat_type == "teams"){
    Teamstats<-Stats %>%
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
             Fumbles, Fumbles_Lost, Standard, HalfPPR, PPR, offense_snaps, offense_pct, Opponent)

    Teamstats2<-Teamstats %>%
      group_by(recent_team, position, season) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      group_by(position) %>%
      mutate(TeamRanking = ave(HalfPPR, position, FUN=rank)) %>%
      ungroup() %>%
      group_by(recent_team, position, season) %>%
      select(completions, attempts, passing_yards, passing_tds, interceptions, sacks, sack_yards,
             passing_air_yards, passing_yards_after_catch, passing_first_downs, passing_epa,
             passing_2pt_conversions, carries, rushing_yards, rushing_tds,
             rushing_first_downs, rushing_epa, rushing_2pt_conversions, receptions,
             receiving_yards, receiving_tds, receiving_air_yards, receiving_yards_after_catch,
             receiving_first_downs, receiving_epa, receiving_2pt_conversions,  Fumbles, Fumbles_Lost,
             Standard, HalfPPR, PPR, TeamRanking) %>%
      mutate(across(c(1:29),.fns = ~./max(Stats$week)))

    output<-Teamstats2
  }

  output
}
