message('Building frames')
message(nrow(frame_index))

cl <- makeCluster(active_cores)
registerDoParallel(cl)

foreach(idx = unique(frame_index$KEY)) %dopar% {
    source('R/setup.R')
    frame_index <- fromJSON('output/frame_index.json')
    if(!'card'%in%names(frame_index)) frame_index <- frame_index %>% mutate(card=NA_character_)
    if(!'overlay'%in%names(frame_index)) frame_index <- frame_index %>% mutate(overlay=NA_character_)
    assign('frame_index',frame_index,envir = .GlobalEnv)
    this_list <- subset(frame_index,KEY==idx)
    success <- FALSE
    # Define a counter to limit the number of retries
    retry_limit <- 5
    retry_count <- 0
    while (!success && retry_count < retry_limit) {
        retry_count <- retry_count + 1
        tryCatch({
            build_frames(this_list$IDX)
            success <- TRUE  # If no error, set success to TRUE
        }, error = function(e) {
            message(paste("Attempt ", retry_count, " on Core ",idx," failed with error:", e$message))
        })
    }
    
    if (!success) {
        stop("All attempts to run build_frames failed.")
    }
    # for(i in this_list$IDX) build_frame(i)
    for(i in this_list$IDX){
        frame <- this_list %>% subset(IDX==i)
        for(j in seq(frame$REP)){
            file.copy(paste0('output/layers/99/Frame_',str_pad(i,5,pad='0'),'.png'),
                      paste0('output/frames/',str_pad(i,5,pad='0'),'_',str_pad(j,4,pad='0'),'.png'),
                      overwrite=TRUE)
        }
        #file.remove(paste0('output/layers/99/Frame_',str_pad(i,5,pad='0'),'.png'))
    }
}
stopCluster(cl)

# for(i in frame_index %>% drop_na(overlay) %>% pull(IDX)){
#     build_frame(i, force=TRUE)
#     frame <- frame_index %>% subset(IDX==i)
#     for(j in seq(frame$REP)){
#         file.copy(paste0('output/layers/99/Frame_',str_pad(i,5,pad='0'),'.png'),
#                   paste0('output/frames/',str_pad(i,5,pad='0'),'_',str_pad(j,4,pad='0'),'.png'),
#                   overwrite=TRUE)
#     }
#     #file.remove(paste0('output/layers/99/Frame_',str_pad(i,5,pad='0'),'.png'))
# } 



message('All frames built')

message('Building chunks')
chunk_index <- frame_index %>% 
    select(IDX,match_state,period,secs,next_shot,prev_shot,next_shots,prev_shots,REP) %>% 
    mutate(period = replace_na(period,0),
           secs = replace_na(secs,0),
           next_shots = replace_na(next_shots,0),
           prev_shots = replace_na(prev_shots,0),
           next_shot = replace_na(next_shot,0),
           prev_shot = replace_na(prev_shot,0),
           prev_rep = lag(REP),
           CHUNK = cumsum(REP!=prev_rep|is.na(prev_rep)|secs==next_shots|secs==(prev_shots+1)|secs==next_shot|secs==(prev_shot+1))) %>% 
    group_by(CHUNK) %>% 
    summarise(START_PER = min(period),
              START_SEC = min(secs),
              END_SEC = max(secs),
              FIRST = min(IDX), 
              LAST = max(IDX), 
              N = n(), 
              REP = mean(REP), 
              TOT = REP*N) %>% 
    ungroup() %>% 
    mutate(PROG = cumsum(TOT),
           KEY = ceiling(PROG/max(PROG)*active_cores)) %>% 
    arrange(FIRST)
chunk_index %>% toJSON() %>% write(file='output/chunk_index.json')
cl <- makeCluster(active_cores)
registerDoParallel(cl)
foreach(idx = unique(frame_index$KEY)) %dopar% {
    source('R/setup.R')
    this_key <- chunk_index %>% subset(KEY==idx)
    for(i in this_key$CHUNK){
        this_chunk <- this_key %>% subset(CHUNK==i)
        this_list <- paste0('output/layers/99/Frame_',str_pad(seq(this_chunk$FIRST,this_chunk$LAST,by=1),5,pad='0'),'.png')
        av::av_encode_video(this_list,
                            framerate = 30/this_chunk$REP,
                            output = paste0('output/chunks/',str_pad(i,5,pad='0'),'.mp4'))
    }
}
stopCluster(cl)

message('Shot animations')
shown_shots <- time_prog %>% 
    subset(action%in%c('SHOT','PENALTY')) %>% 
    inner_join(frame_index %>% select(period,secs,match_state,FIDX=IDX),by=c('period','time'='secs')) %>% 
    subset(match_state%in%c('build_up','reaction')) %>% 
    select(IDX,period,time,action,outcome,match_state,FIDX) %>% 
    mutate(RN = row_number()-1,
           KEY = 1 + RN%%active_cores)
shown_shots %>% toJSON() %>% write('output/shown_shots.json')
cl <- makeCluster(active_cores)
# clusterEvalQ(cl,{
#     library(tidyverse)
#     library(png)
#     library(ggimage)
#     library(gganimate)
#     library(jsonlite)
#     library(magick)
# })
clusterExport(cl, c('shot_animation','shown_shots','frame_index','chunk_index','time_prog','pitch_transform'))
registerDoParallel(cl)
foreach(shot_idx = shown_shots$IDX,
        .packages = c('tidyverse','png','ggimage','gganimate','jsonlite','magick','parallel','foreach')) %dopar% {
    shot_animation(shot_idx)
}
stopCluster(cl)

message('Final Movie')
all_chunks <- list.files('output/chunks',full.names=TRUE)
writeLines(str_replace(paste0('file \'',all_chunks,'\''),'output/',''),'output/chunks.txt')
output_file <- paste0('output/broadcast/S',match_details$season_no,
                      '_R',str_pad(match_details$round_no,2,pad='0'),
                      '_',match_details$home_short_name,'v',match_details$away_short_name,'.mp4')
if(file.exists(output_file)) file.remove(output_file)
system(paste0("ffmpeg -f concat -safe 0 -i output/chunks.txt -c copy ",output_file),
       ignore.stdout = TRUE,
       ignore.stderr = TRUE)
message(Sys.time())

home_goals <- match_file %>% 
    subset(state=='Goal' & possession=='A') %>% 
    select(period,time) %>% 
    left_join(frame_index %>% mutate(CUM = cumsum(REP)),by=c('period'='period','time'='secs')) %>% 
    mutate(PREV = CUM - REP,
           SECS = round(PREV/30,2)) %>% 
    pull(SECS) %>% 
    paste(collapse='+')

away_goals <- match_file %>% 
    subset(state=='Goal' & possession=='B') %>% 
    select(period,time) %>% 
    left_join(frame_index %>% mutate(CUM = cumsum(REP)),by=c('period'='period','time'='secs')) %>% 
    mutate(PREV = CUM - REP,
           SECS = round(PREV/30,2)) %>% 
    pull(SECS) %>% 
    paste(collapse='+')

whistles <- match_file %>% 
    subset(state!='Substitution') %>% 
    group_by(period) %>% 
    arrange(period,time) %>% 
    mutate(next_time = lead(time)) %>% 
    subset(period > 0) %>% 
    subset(time==0 | (!is.na(time) & is.na(next_time))) %>% 
    left_join(frame_index %>% 
                  mutate(CUM = cumsum(REP)) %>% 
                  group_by(period,secs) %>% 
                  slice(1),
              by=c('period'='period','time'='secs')) %>% 
    mutate(PREV = CUM - REP,
           SECS = round(PREV/30,2)) %>% 
    pull(SECS) %>% 
    paste(collapse='+')

#system(paste0('python3 ',here(),'/R/movie/add_audio.py ',whistles,' ',home_goals,' ',away_goals))
