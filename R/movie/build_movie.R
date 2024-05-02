
message('Pre Game')
title_page()
starting_lineups()
league_table_pre()
manager_faceoff()

message('Halftime / Fulltime')
league_table_post()
halftime_stats()
player_leader('fulltime')
ratings_list('fulltime')
fulltime_stats()
#added_time(1)
#added_time(2)

message('Match Base')
match_base()
crest_overlay('A')
crest_overlay('B')

message('Minutes')
cl <- makeCluster(active_cores)
registerDoParallel(cl)
foreach(key = unique(stat_times$KEY)) %dopar% {
  source('R/setup.R')
  temp_times <- stat_times %>% subset(KEY==key)
  for(i in seq_along(temp_times$period)) with(temp_times %>% slice(i),minute_base(period,time))
}

message('Transactions')
foreach(key = unique(trx_frames$KEY)) %dopar% {
  source('R/setup.R')
  temp_trx <- trx_frames %>% subset(KEY==key) %>% left_join(time_prog,by='IDX')
  for(i in temp_trx %>% pull(ORD)) trx_export(i)
}

message('Clock')
foreach(key = unique(time_base$KEY)) %dopar% {
  source('R/setup.R')
  this_clock <- time_base[time_base$KEY==key,]
  for(i in this_clock$IDX) clock_overlay(this_clock %>% subset(IDX==i))
}
stopCluster(cl)

message('Key Moments')
for(i in key_moments$IDX){ 
    key_base(i,TRUE)
}

message('Lineups')
for(i in unique(lineup_times$TRX)) lineup_base(i)

message('Goals')
for(i in seq(sum(match_file$state=='Goal'))) goal_overlay(i)

# message('Subs')
# for(i in seq(match_file %>% subset(state=='Substitution') %>% select(period,time) %>% unique() %>% nrow())) sub_overlay(i)

message('All layers built')
