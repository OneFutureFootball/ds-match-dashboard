
message('Title Page')
title_page()

message('Starting Lineup')
starting_lineups()

message('League Table Pre')
league_table_pre()

message('League Table Post')
league_table_post()

message('Halftime Stats')
halftime_stats()

message('Fulltime Stats')
fulltime_stats()

message('Match Base')
match_base()

message('Minutes')
cl <- makeCluster(active_cores)
registerDoParallel(cl)
foreach(key = unique(stat_times$KEY)) %dopar% {
  source('R/setup.R')
  temp_times <- stat_times %>% subset(KEY==key)
  for(i in seq_along(temp_times$period)) with(temp_times %>% slice(i),minute_base(period,time))
}

message('Key Moments')
foreach(key = unique(key_moments$KEY)) %dopar% {
  source('R/setup.R')
  temp_moments <- key_moments %>% subset(KEY==key)
  for(i in temp_moments$IDX){ 
    key_base(i,TRUE)
  }
}

message('Transactions')
foreach(key = unique(time_prog$KEY)) %dopar% {
  source('R/setup.R')
  temp_trx <- time_prog %>% subset(KEY==key)
  for(i in temp_trx$IDX){
    trx_export(i,'possession')
    trx_export(i,'result')
  }
  for(i in temp_trx$IDX){
    trx_export(i,'action')
  }
}

message('Clock')
foreach(key = unique(time_base$KEY)) %dopar% {
  source('R/setup.R')
  this_clock <- time_base[time_base$KEY==key,]
  for(i in this_clock$IDX) clock_overlay(this_clock %>% subset(IDX==i))
}
stopCluster(cl)

message('Lineups')
for(i in unique(lineup_times$TRX)) lineup_base(i)

message('Goals')
for(i in seq(sum(match_file$state=='Goal'))) goal_overlay(i)

# message('Subs')
# for(i in seq(match_file %>% subset(state=='Substitution') %>% select(period,time) %>% unique() %>% nrow())) sub_overlay(i)

message('All layers built')
