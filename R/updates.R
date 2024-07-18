input <- fromJSON('input/match_output.json')
this_period <- 2
this_time <- c(77,10); this_time <- 60*this_time[1] + this_time[2] - (45*60*(this_period-1))

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
        YC = case_when(
            period==1 & match_time=='33:46' ~ 23,
            period==1 & match_time=='40:40' ~ 31,
            period==2 & match_time=='64:15' ~ 20
        ),
        Y = ifelse(is.na(YC),Y,YC),
        XC = case_when(
            period==1 & match_time=='33:46' ~ 112,
            period==1 & match_time=='40:40' ~ 84,
            period==2 & match_time=='77:25' ~ 77
        ),
        X = ifelse(is.na(XC),X,XC)
    )
temp %>% toJSON(pretty=TRUE) %>% 
    write(file='input/match_output.json')
source('R/support/data_prep.R')

# for(i in list.files('output/layers/04',full.names=TRUE)) file.remove(i)
# for(i in list.files('output/layers/99',full.names=TRUE)) file.remove(i)
# for(i in list.files('output/frames',full.names=TRUE)) file.remove(i)
# for(i in list.files('output/chunks',full.names=TRUE)) file.remove(i)

affected_times <- temp %>% 
    subset(!is.na(YC) | !is.na(XC)) %>% 
    select(idx,match_time,period,time,X,Y) %>%
    left_join(trx_frames,by=c('period','time'='timestamp')) %>% 
    select(period,match_time,IDX,ORD)
if(exists('cross_join')){
    affected_times <- affected_times %>% cross_join(data.frame(ADD = seq(-1,3)))
}else{
    affected_times <- affected_times %>% left_join(data.frame(ADD = seq(-1,3)),by=character())
}
affected_times <- affected_times %>% 
    mutate(IDX = IDX + ADD) %>% 
    left_join(trx_frames,by='IDX',suffix=c('_old','')) %>% 
    group_by(period,ORD_old) %>% 
    summarise(
        first_time = min(timestamp),
        last_time = max(timestamp),
        first_clock = min(match_time),
        last_clock = max(match_time),
        .groups='drop'
    )
affected_times %>% print()

affected_frames <- frame_index %>% 
    left_join(affected_times,by='period') %>% 
    drop_na(secs) %>% 
    subset(secs>=first_time & secs<=last_time)
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
