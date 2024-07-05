input <- fromJSON('input/match_output.json')
this_period <- 2
this_time <- c(68,20); this_time <- 60*this_time[1] + this_time[2] - (45*60*(this_period-1))

input %>% subset(time>=this_time) %>% 
    subset(period==this_period) %>% 
    select(period,time,match_time,state,full_name,action,outcome,x,y,X,Y,xg) %>% 
    head(10) %>% 
    print()

temp <- input %>% 
    group_by(period,match_time) %>% 
    mutate(N = n()) %>% 
    ungroup() %>% 
    mutate(
        Y = case_when(
            period==1 & match_time=='00:10' ~ 69,
            period==1 & match_time=='12:17' ~ 21,
            period==1 & match_time=='42:10' ~ 50,
            period==2 & match_time=='42:10' ~ 31,
            TRUE ~ Y
        ),
        X = case_when(
            period==1 & match_time=='00:06' ~ 39,
            period==1 & match_time=='12:12' ~ 23,
            period==1 & match_time=='12:17' ~ 120-27,
            TRUE ~ X
        )
    )
temp %>% toJSON(pretty=TRUE) %>% 
    write(file='input/match_output.json')
source('R/support/data_prep.R')

#for(i in list.files('output/layers/04',full.names=TRUE)) file.remove(i)
#for(i in list.files('output/layers/99',full.names=TRUE)) file.remove(i)
#for(i in list.files('output/frames',full.names=TRUE)) file.remove(i)

affected_frames <- frame_index %>% 
    subset(period==1 & secs%in%(12*60 + 13:19))
for(i in affected_frames$IDX){
    this_frame <- affected_frames %>% subset(IDX==i)
    this_trx <- trx_frames %>% 
        subset(period==this_frame$period) %>% 
        subset(timestamp==this_frame$secs)
    if(nrow(this_trx) > 0) trx_export(this_trx$ORD,TRUE)
    build_frame(i,TRUE)
    frame <- frame_index %>% subset(IDX==i)
    for(j in seq(this_frame$REP)){
        file.copy(paste0('output/layers/99/Frame_',str_pad(i,5,pad='0'),'.png'),
                  paste0('output/frames/',str_pad(i,5,pad='0'),'_',str_pad(j,4,pad='0'),'.png'),
                  overwrite=TRUE)
    }
    this_list <- list.files('output/frames',pattern=paste0(str_pad(i,5,pad='0'),'_'),full.names=TRUE)
    av::av_encode_video(this_list,
                        framerate = 30,
                        output = paste0('output/chunks/',str_pad(i,5,pad='0'),'.mp4'))
    this_shot <- time_prog %>% 
        subset(period==this_frame$period) %>% 
        subset(time==this_frame$secs) %>% 
        subset(action%in%c('SHOT','PENALTY'))
    if(nrow(this_shot) > 0) shot_animation(this_shot$IDX)
}
all_chunks <- list.files('output/chunks',full.names=TRUE)

message('Final Movie')
av::av_encode_video(all_chunks,
                    framerate = 30,
                    output = paste0('output/broadcast/S',match_details$season_no,
                                    '_R',str_pad(match_details$round_no,2,pad='0'),
                                    '_',match_details$home_short_name,'v',match_details$away_short_name,'.mp4'),
                    verbose = FALSE)
message(Sys.time())
