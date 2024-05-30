message('Building frames')
message(nrow(frame_index))

cl <- makeCluster(active_cores)
registerDoParallel(cl)

foreach(idx = unique(frame_index$KEY)) %dopar% {
    source('R/setup.R')
    frame_index <- fromJSON('output/frame_index.json')
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

cl <- makeCluster(active_cores)
registerDoParallel(cl)
foreach(idx = unique(frame_index$KEY)) %dopar% {
    source('R/setup.R')
    this_key <- frame_index %>% subset(KEY==idx)
    for(i in this_key$IDX){
        this_list <- list.files('output/frames',pattern=paste0(str_pad(i,5,pad='0'),'_'),full.names=TRUE)
        av::av_encode_video(this_list,
                            framerate = 30,
                            output = paste0('output/chunks/',str_pad(i,5,pad='0'),'.mp4'))
    }
}
stopCluster(cl)

# for(i in time_prog %>% subset(action%in%c('SHOT','PENALTY')) %>% nrow() %>% seq()) shot_animation(i)

all_chunks <- list.files('output/chunks',full.names=TRUE)

message('Final Movie')
av::av_encode_video(all_chunks,
                    framerate = 30,
                    output = paste0('output/broadcast/S',match_details$season_no,
                                    '_R',str_pad(match_details$round_no,2,pad='0'),
                                    '_',match_details$home_short_name,'v',match_details$away_short_name,'.mp4'),
                    verbose = FALSE)

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
