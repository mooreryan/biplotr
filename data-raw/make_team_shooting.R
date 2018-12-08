shooting<-"/Users/moorer/projects/nba_stats/data_files/team_shooting_2018_2019_for_r.tsv"

team_shooting_df <- read.table(shooting, header = T, sep = "\t")
team_shooting_mat <- as.matrix(team_shooting_df[, 4:ncol(team_shooting_df)])
rownames(team_shooting_mat) <- team_shooting_df$Team

usethis::use_data(team_shooting_df, team_shooting_mat)
