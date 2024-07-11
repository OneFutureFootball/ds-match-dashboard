shot_animation <- function(shot_idx){
    dir.create(paste0('output/gifs/',Sys.getpid()),showWarnings=FALSE)
    for(i in list.files(paste0('output/gifs/',Sys.getpid()),full.names=TRUE)) file.remove(i)
    input <- time_prog %>% 
        mutate(next_x = lead(ball_x),
               next_y = lead(ball_y),
               next_position = lead(position)) %>% 
        subset(IDX==shot_idx)
    
    goal_width <- data.frame(GY = seq(30,50,by=0.1)) %>% 
        rowwise() %>% 
        mutate(GK = max(0.01,abs(GY-40))) %>% 
        ungroup()
    
    shot_target <- input %>% 
        mutate(
            DIST = sqrt((ifelse(possession=='A',120,0)-ball_x)^2 + (40-ball_y)^2),
            ANGLE = abs(ifelse(possession=='A',
                               atan((ball_y - 40)/(120-ball_x)),
                               atan((ball_y - 40)/ball_x)))/pi*180,
            GX = ifelse(possession=='A',120,0))
    
    if(exists('cross_join'))  shot_target <- shot_target %>% cross_join(goal_width)
    if(!exists('cross_join')) shot_target <- shot_target %>% left_join(goal_width,by=character())
    
    shot_target <- shot_target %>% 
        mutate(
            YW = GK*case_when(
                outcome%in%c('off target','blocked') & abs(ball_y - 40) > 4  & abs(GY-40) > abs(ball_y-40)+0.5 ~ 0,
                outcome%in%c('off target','blocked') & abs(ball_y - 40) <= 4 & abs(GY-40) > 6 ~ 0,
                outcome%in%c('off target','blocked') ~ 1,
                outcome%in%c('saved','goal') & abs(GY-40)>3.9 ~ 0,
                outcome%in%c('saved') & next_state=='Corner' ~ case_when(
                    sign(GY-40)!=sign(next_y-40) ~ 0,
                    TRUE ~ 1
                ),
                outcome=='saved' ~ 1,
                outcome=='post' ~ case_when(
                    abs(GY-40)>4.5 ~ 0,
                    abs(GY-36)<=0.5 ~ 40,
                    abs(GY-44)<=0.5 ~ 40,
                    next_state=='Corner' & sign(GY-40)!=sign(next_y-40) ~ 0,
                    TRUE ~ 1
                ),
                ANGLE < 60 ~ 1,
                abs(ball_y - 40) < 4 ~ 1,
                ball_y > 40 ~ 1 - (GY - 36)/8,
                ball_y < 40 ~ (GY - 36)/8
            )
        ) %>% 
        sample_n(1,weight = YW) %>% 
        mutate(
            EX = pitch_transform(GX,'X'),
            EY = pitch_transform(GY,'Y'),
            DIST = sqrt((ball_x - GX)^2 + (ball_y - GY)^2),
            TIME = case_when(
                technique == 'head' ~ DIST / rnorm(1,12,1),
                TRUE ~ DIST / rnorm(1,21,2),
            ),
            TIME = case_when(
                TIME > 0.8 ~ 0.8,
                TIME < 0.1 ~ 0.1,
                TRUE ~ TIME
            ),
            DELAY = case_when(
                technique%in%c('volley','head') ~ 0.01,
                TRUE ~ 0.95 - TIME
            )
        ) %>% 
        select(possession,technique,X,Y,EX,EY,DIST,TIME,DELAY,ball_x,ball_y,next_x,next_y,GX,GY)
    
    
    if(input$outcome%in%c('saved')){
        save_point <- shot_target %>% 
            mutate(
                SX = case_when(
                    possession=='A' ~ case_when(
                        input$state=='Free Kick' ~ 119.6,
                        ball_x >= 114 ~ 119.6,
                        ball_x >= 102 ~ 119.6 - runif(1)*3,
                        TRUE ~ 119.6 - runif(1)*8
                    ),
                    possession=='B' ~ case_when(
                        input$state=='Free Kick' ~ 0.4,
                        ball_x <= 6  ~ 0.4,
                        ball_x <= 18 ~ 0.4 + runif(1)*3,
                        TRUE ~ 0.4 + runif(1)*8
                    )
                ),
                SP = (SX - ball_x)/(GX - ball_x),
                SY = round(SP*(GY - ball_y) + ball_y,1),
                OY = round((ifelse(possession=='A',122,-2) - ball_x)/(GX - ball_x)*(GY - ball_y) + ball_y,1)
            ) %>% 
            rename(TY = GY)
        
        def_point <- save_point %>% 
            mutate(ball_x = SX,
                   ball_y = SY,
                   X = pitch_transform(SX,'X'),
                   Y = pitch_transform(SY,'Y')) %>% select(ball_x,ball_y,X,Y) %>% mutate(technique = 'deflection',STEP = 2)
        
        if(exists('cross_join')) end_options <- save_point %>% cross_join(goal_width)
        if(!exists('cross_join')) end_options <- save_point %>% left_join(goal_width,by=character())
        
        end_point <- end_options %>% 
            mutate(
                KEEP = case_when(
                    input$next_state=='Corner' ~ case_when(
                        sign(GY-40)!=sign(next_y-40) ~ FALSE,
                        sign(ball_y-40)!=sign(SY-40) & abs(GY-40) > abs(OY-40) ~ FALSE,
                        TRUE ~ TRUE
                    ),
                    input$outcome=='saved' & input$next_position=='GK' ~ GY==SY,
                    input$outcome=='post' & input$next_state=='Goal Kick' ~ case_when(
                        sign(ball_y-40)!=sign(SY-40) & abs(GY-40) > abs(OY-40) ~ FALSE,
                        abs(TY-40) < 3.5 & abs(GY-OY) > 1 ~ FALSE,
                        TRUE ~ TRUE
                    ),
                    TRUE ~ TRUE
                ),
                DC = (max(GK) - GK)/(max(GK)-min(GK)),
                EC = GK/max(GK),
                OL = abs(SY-40)/4,
                OL = ifelse(OL>1,1,OL),
                WGT = OL*DC^2 + (1-OL)*sqrt(EC)
            ) %>% 
            subset(KEEP) %>% 
            sample_n(1,weight = WGT) %>% 
            mutate(NX = ifelse(possession=='A',122,-2)) %>% 
            rename(NY = GY) %>% 
            mutate(
                NX = case_when(
                    input$next_state=='Keeper Possession' ~ SX,
                    str_detect(input$next_state,'Possession') ~ input$next_x,
                    TRUE ~ NX
                ),
                NY = case_when(
                    input$next_state=='Keeper Possession' ~ SY,
                    str_detect(input$next_state,'Possession') ~ input$next_y,
                    TRUE ~ NY
                )
            )
    }else if(input$outcome%in%c('blocked','post')){
        block_pct <- data.frame(BP = (2:18)/20)
        if(input$outcome=='post') block_pct <- data.frame(BP=1)
        if(exists('cross_join')) block_point <- shot_target %>% cross_join(block_pct)
        if(!exists('cross_join')) block_point <- shot_target %>% left_join(block_pct,by=character())
        assign('temp_block',block_point,envir=.GlobalEnv)
        block_point <- block_point %>% 
            mutate(
                BX = round(BP*(GX - ball_x) + ball_x,1),
                BY = round(BP*(GY - ball_y) + ball_y,1),
                SD = sqrt((GX - ball_x)^2 + (GY - ball_y)^2),
                BD = SD*BP,
                NY = case_when(
                    input$next_state=='Corner' ~ rnorm(n(),GY,3),
                    TRUE ~ next_y
                ),
                NY = case_when(
                    input$next_state!='Corner' ~ NY,
                    max(NY) < 40 | min(NY) > 40 ~ rnorm(n(),40,4),
                    TRUE ~ NY
                ),
                KEEP = case_when(
                    input$next_state=='Corner' & sign(next_y-40)!=sign(NY-40) ~ FALSE,
                    TRUE ~ TRUE
                ),
                NX = case_when(
                    input$next_state=='Corner' ~ ifelse(possession=='A',122,-2),
                    TRUE ~ next_x
                ),
                ED = sqrt((BX - NX)^2 + (BY - NY)^2),
                TD = ED + BD,
                WG = 1/TD*case_when(abs(NY-40)<=4 ~ 0.2, TRUE ~ 1)
            ) %>% 
            subset(KEEP) %>% 
            subset((SD - BD > 5)|max(SD - BD) < 5) %>% 
            sample_n(1)
        
        def_point <- block_point %>% 
            mutate(ball_x = BX,
                   ball_y = BY,
                   X = pitch_transform(BX,'X'),
                   Y = pitch_transform(BY,'Y')) %>% select(ball_x,ball_y,X,Y) %>% mutate(technique = 'deflection',STEP = 2)
        
        end_point <- block_point
    }else if(input$outcome%in%c('off target','goal')){
        def_point <- shot_target %>% 
            mutate(ball_x = GX,
                   ball_y = GY,
                   X = pitch_transform(GX,'X'),
                   Y = pitch_transform(GY,'Y')) %>% select(ball_x,ball_y,X,Y) %>% mutate(technique = 'deflection',STEP=2)
        end_point <- shot_target %>% 
            mutate(NX = GX,
                   NY = GY)
    }
    
    
    
    anim_start <- input %>% select(technique,ball_x,ball_y,X,Y) %>% mutate(STEP=0)
    shot_start <- input %>% select(technique,ball_x,ball_y,X,Y) %>% mutate(STEP=1)
    def_point <- def_point
    shot_end <- end_point %>% 
        mutate(ball_x = NX,
               ball_y = NY,
               X = pitch_transform(NX,'X'),
               Y = pitch_transform(NY,'Y')) %>% select(technique,ball_x,ball_y,X,Y) %>% mutate(STEP=3)
    anim_end <- shot_end %>% mutate(STEP = 4)
    
    all_points <- anim_start %>% 
        bind_rows(shot_start) %>% 
        bind_rows(def_point) %>% 
        bind_rows(shot_end) %>% 
        bind_rows(anim_end) %>% 
        mutate(DIST = sqrt((ball_x - lead(ball_x))^2 + (ball_y - lead(ball_y))^2))
    
    total_distance <- sum(all_points$DIST,na.rm=TRUE)
    shot_speed <- rnorm(1,case_when(
        input$technique=='head' ~ 12,
        input$technique=='volley' ~ 23,
        TRUE ~ 20
    ),2)
    def_speed <- rnorm(1,case_when(
        input$technique=='head' ~ 8,
        input$technique=='volley' ~ 16,
        TRUE ~ 15
    ),2)
    shot_time <- (all_points %>% subset(STEP==1) %>% pull(DIST))/shot_speed
    def_time <- tail(c(0,(all_points %>% subset(STEP==2) %>% pull(DIST))/def_speed),1)
    total_time <- shot_time + def_time
    
    if(total_time > 0.9){
        time_warp <- 0.9/total_time
        spare_time <- 0.1
    }else{
        time_warp <- 1
        spare_time <- 1 - total_time
    }
    
    plot_input <- all_points %>% 
        mutate(
            TIME = case_when(
                STEP==0 ~ 0,
                STEP==1 ~ ifelse(input$technique%in%c('head','volley'),0,spare_time/2),
                STEP==2 ~ shot_time*time_warp,
                STEP==3 ~ def_time*time_warp,
                STEP==4 ~ ifelse(input$technique%in%c('head','volley'),spare_time,spare_time/2)
            ),
            TIME = cumsum(TIME),
            START = STEP<=1) %>% 
        select(-c(technique,STEP,DIST)) %>% 
        unique()
    
    
    plot_output <- plot_input %>% 
        mutate(
            ANG = atan((max(Y[TIME==0])-max(Y[TIME==max(TIME)]))/(max(X[TIME==0])-max(X[TIME==max(TIME)]))),
            X = case_when(
                input$action=='PENALTY' ~ X,
                input$state=='Free Kick' ~ X,
                START ~ X + ifelse(input$possession=='A',1,-1)*17*abs(cos(ANG)),
                TRUE ~ X),
            Y = case_when(
                input$action=='PENALTY' ~ Y,
                input$state=='Free Kick' ~ Y,
                START ~ Y + ifelse(input$ball_y > 40,-1,1)*17*abs(sin(ANG)),
                TRUE ~ Y)
        ) %>% 
        unique() %>% 
        ggplot(mapping = aes(x=X,y=Y)) +
        geom_image(mapping=aes(image='images/icons/ball.png'),
                   size=0.018) +
        transition_time(TIME) +
        theme_void() +
        enter_appear() +
        exit_disappear() +
        coord_cartesian(xlim = c(0,1920), ylim=c(0,1080))
    
    anim <- animate(plot_output, 
                    nframes = 30, 
                    fps = 30,
                    height=1080,
                    width=1920,
                    units='px', 
                    renderer = file_renderer(dir = paste0("output/gifs/",Sys.getpid()), 
                                             prefix = paste0("GIF_",input$period,'_',str_pad(input$time,4,pad='0'),'_'), 
                                             overwrite = TRUE),
                    bg='transparent')
    
    gif_list <- data.frame(filename=list.files(paste0('output/gifs/',Sys.getpid()),full.names=TRUE)) %>% 
        mutate(period = as.numeric(sapply(filename,function(x) str_split(x,'_')[[1]][2])),
               time   = as.numeric(sapply(filename,function(x) str_split(x,'_')[[1]][3])),
               REP   = as.numeric(str_replace(sapply(filename,function(x) str_split(x,'_')[[1]][4]),'.png','')))
    
    new_frames <- frame_index %>% select(IDX,period,secs,REP,match_state) %>% uncount(REP) %>% 
        group_by(IDX) %>% 
        mutate(REP = row_number()) %>% 
        inner_join(gif_list,by=c('period','secs'='time','REP')) %>% 
        mutate(output = paste0('output/frames/',str_pad(IDX,5,pad='0'),'_',str_pad(REP,4,pad='0'),'.png'),
               frame = paste0('output/layers/99/Frame_',str_pad(IDX,5,pad='0'),'.png')) %>% 
        subset(match_state%in%c('build_up','reaction'))
    
    for(i in seq_along(new_frames$output)){
        this_output <- new_frames %>% slice(i)
        if(file.exists(this_output$output)) image_read(this_output$frame) %>% 
            image_composite(image_read(this_output$filename)) %>% 
            image_write(this_output$output)
    }
    
    frame_list <- new_frames$output
    av::av_encode_video(frame_list,
                        framerate = 30,
                        output = paste0('output/chunks/',str_pad(unique(new_frames$IDX),5,pad='0'),'.mp4'),
                        verbose = FALSE)
    
    result_frames <- frame_index %>% select(IDX,period,secs,REP,match_state,trx) %>% uncount(REP) %>% 
        subset(trx == paste0('Trx_',unique(new_frames$period),'_',str_pad(unique(new_frames$secs)+1,4,pad='0'),'.png')) %>% 
        subset(IDX > unique(new_frames$IDX)) %>% 
        subset(input$outcome!='goal') %>% 
        group_by(IDX) %>% mutate(REP = row_number()) %>% 
        mutate(output = paste0('output/frames/',str_pad(IDX,5,pad='0'),'_',str_pad(REP,4,pad='0'),'.png'),
               frame = paste0('output/layers/99/Frame_',str_pad(IDX,5,pad='0'),'.png')) %>% 
        subset(match_state%in%c('build_up','reaction')) %>% ungroup()
    
    for(j in seq_along(result_frames$output)){
        this_output <- result_frames %>% slice(j)
        if(file.exists(this_output$output)) image_read(this_output$frame) %>% 
            image_composite(image_read(tail(new_frames$filename,1))) %>% 
            image_write(this_output$output)
    }
    for(k in unique(result_frames$IDX)){
        result_list <- result_frames %>% 
            subset(IDX==k) %>% 
            pull(output)
        av::av_encode_video(result_list,
                            framerate = 30,
                            output = paste0('output/chunks/',str_pad(k,5,pad='0'),'.mp4'),
                            verbose = FALSE)
    }
    
    
    
    
    for(i in list.files(paste0('output/gifs/',Sys.getpid()),full.names=TRUE)) file.remove(i)
    file.remove(paste0('output/gifs/',Sys.getpid()))
    return(plot_input)
}