#' Removing many columns from initial scraping.
#' Must use "DraftScrapewithProjections" prior to this
#' @param DraftProjections An output from the \link{DraftScrapewithProjections} function
#' @export

CleanProjections<-function(DraftProjections){
  DraftprojectionsLessNoise<-{DraftProjections}[, !names({DraftProjections}) %in%
                                                 c("dst_yds_199","dst_yds_299","dst_yds_349","dst_yds_399",
                                                   "dst_yds_449","dst_yds_499","dst_yds_549","dst_yds_550",
                                                   "dst_pts_6","dst_pts_13","dst_pts_17","dst_pts_20","dst_pts_27",
                                                   "dst_pts_34","dst_pts_45","rec_150_yds","rec_200_yds", "pass_250_yds",
                                                   "pass_300_yds","pass_350_yds","rush_50_yds","rush_100_yds",
                                                   "dst_tackles", "dst_pts_allowed_g", "dst_pass_yds_allowed", "dst_rush_yds_allowed",
                                                   "dst_avg_yds_allowed","dst_sacks", "dst_int","dst_fum_rec","dst_fum_force", "dst_td",
                                                   "dst_safety", "rec_09_tds", "rec_1019_tds", "rec_2029_tds", "rec_3039_tds",
                                                   "rec_4049_tds", "rec_50_tds","pass_09_tds", "pass_1019_tds",
                                                   "rush_09_tds", "rush_1019_tds", "rush_2029_tds", "rush_3039_tds",
                                                   "rush_4049_tds", "rush_50_tds", "rec_50_yds", "rec_100_yds",
                                                   "fg_att_0019","pass_2029_tds", "pass_3039_tds", "pass_4049_tds", "pass_50_tds", "sacks",
                                                   "fg_0019", "fg_2029", "fg_3039", "fg_4049", "fg_50","fg_long", "fg_att_2029",
                                                   "fg_att_3039", "fg_att_4049", "fg_att_50","dst_pts_allowed", "dst_yds_allowed",
                                                   "fg","fg_att","xp","rec_rz_tgt")]
  DraftprojectionsLessNoise<-DraftprojectionsLessNoise[order(
    -DraftprojectionsLessNoise$HalfPPR), ] %>%
    select("Season","name","position","team",
           "pass_yds","pass_tds","pass_att","pass_comp","pass_int",
           "rec","rec_yds","rec_tds","rec_tgt",
           "rush_yds","rush_tds","rush_att","fumbles_lost",
           "Standard","HalfPPR","PPR")

  DraftprojectionsLessNoise
}
