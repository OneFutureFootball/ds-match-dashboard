shot_animation <- function(shot_idx){
    file.create(paste0('output/',shot_idx,'.json'))
    dir.create(paste0('output/gifs/',Sys.getpid()),showWarnings=FALSE)
    for(i in list.files(paste0('output/gifs/',Sys.getpid()),full.names=TRUE)) file.remove(i)
    #KICK AT 25M/S
    #HEAD AT 10M/S
    input <- time_prog %>% 
        mutate(next_x = lead(ball_x),
               next_y = lead(ball_y),
               next_position = lead(position)) %>% 
        subset(IDX==shot_idx)
    
    goal_width <- data.frame(GY = seq(30,50,by=0.1)) %>% 
        mutate(GK = abs(GY-40)+0.1)
    end_point <- input %>% 
        mutate(
            DIST = sqrt((ifelse(possession=='A',120,0)-ball_x)^2 + (40-ball_y)^2),
            ANGLE = abs(ifelse(possession=='A',
                               atan((ball_y - 40)/(120-ball_x)),
                               atan((ball_y - 40)/ball_x)))/pi*180,
            GX = case_when(
                outcome%in%c('post','goal') ~ ifelse(possession=='A',120,0),
                outcome=='saved' ~ ifelse(possession=='A',
                                          120 - runif(n())*2,
                                          0 + runif(n())*2),
                outcome%in%c('off target','blocked') ~ ifelse(possession=='A',122,-2),
                TRUE ~ 60
            ),
            GX = case_when(
                possession=='A' & GX < ball_x ~ (ball_x + 120)/2,
                possession=='B' & GX > ball_x ~ (ball_x)/2,
                TRUE ~ GX
            ))
    
    if(exists('cross_join'))  end_point <- end_point %>% cross_join(goal_width)
    if(!exists('cross_join')) end_point <- end_point %>% left_join(goal_width,by=character())
    
    end_point <- end_point %>% 
        mutate(
            YW = GK*case_when(
                outcome%in%c('off target','blocked') & abs(ball_y - 40) > 5  & abs(GY-40) > abs(ball_y-40) ~ 0,
                outcome%in%c('off target','blocked') & abs(ball_y - 40) <= 5 & abs(GY-40) > 6 ~ 0,
                outcome%in%c('off target','blocked') ~ 1,
                outcome%in%c('saved','goal') & abs(GY-40)>4 ~ 0,
                outcome=='saved' & next_state=='Corner' & sign(GY-40)!=sign(next_y-40) ~ 0,
                outcome=='saved' ~ 1,
                outcome=='post' & abs(GY-40)>4.5 ~ 0,
                outcome=='post' & abs(GY-36)<=0.5 ~ 40,
                outcome=='post' & abs(GY-44)<=0.5 ~ 40,
                outcome=='post' ~ 1,
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
                technique == 'head' ~ DIST / 10,
                TRUE ~ DIST / 25,
            ),
            TIME = ifelse(TIME > 1,0.8,TIME),
            DELAY = 1 - TIME
        ) %>% 
        select(possession,technique,X,Y,EX,EY,TIME,DELAY,ball_x,ball_y,next_x,next_y,GX,GY)
    
    if(input$outcome=='blocked'){
        pct <- (4:16)*5 / 100
        val <- sapply(pct,function(x){
            end_point$next_y = end_point$next_y + ifelse(input$next_state%in%c('Corner','Goal Kick'),rnorm(1,0,2),0)
            A = with(end_point,sqrt((next_x - ball_x)^2 + (next_y - ball_y)^2))
            C = with(end_point,sqrt((ifelse(possession=='A',120,0)-ball_x)^2 + (ball_y-40)^2))
            Z = sqrt(A^2 + C^2)*x
            return(Z)
        })
        DEF <- ifelse(input$next_state=='Corner',
                      0.4 + runif(1)*0.5,
                      pct[order(val)][1])
        
        mid_point <- end_point %>% 
            mutate(
                DX = ball_x*DEF + GX*(1-DEF),
                DY = ball_y*DEF + GY*(1-DEF),
                MX = pitch_transform(DX,'X'),
                MY = pitch_transform(DY,'Y'),
                DIST = sqrt((ball_x - DX)^2 + (ball_y - DY)^2),
                TIME = case_when(
                    technique == 'head' ~ DIST / 12,
                    TRUE ~ DIST / 30,
                ),
                TIME = DEF
            )
        
        end_point <- end_point %>% 
            mutate(EX = pitch_transform(ifelse(input$next_state=='Corner',
                                               GX,
                                               next_x),'X'),
                   EGY = ifelse(input$next_state=='Corner',
                                GY + runif(1)*5,
                                next_y),
                   EGY = ifelse(input$next_state=='Corner' & sign(EGY-40) != sign(next_y),
                                40 + runif(1)*4*sign(next_y-40),
                                EGY),
                   EY = pitch_transform(EGY,'Y'),
                   TIME = 1)
        
        MD <- sqrt((input$X - mid_point$MX)^2 + (input$Y - mid_point$MY)^2)
        ED <- sqrt((end_point$EX - mid_point$MX)^2 + (end_point$EY - mid_point$MY)^2)
        mid_point$TIME <- MD/(MD + ED)*0.7 + 0.3
        
        plot_input <- input %>% select(X,Y) %>% mutate(TIME=0) %>% 
            bind_rows(input %>% select(X,Y) %>% mutate(TIME=0.3)) %>% 
            bind_rows(mid_point %>% select(X=MX,Y=MY,TIME)) %>% 
            bind_rows(end_point %>% mutate(TIME=1) %>% select(X=EX,Y=EY,TIME))
    }else if(input$outcome=='saved' & input$next_position!='GK'){
        DEF <- 0.9
        
        mid_point <- end_point %>% 
            mutate(
                MX = pitch_transform(GX,'X'),
                MY = pitch_transform(GY,'Y'),
                DIST = sqrt((ball_x - GX)^2 + (ball_y - GY)^2),
                TIME = case_when(
                    technique == 'head' ~ DIST / 10,
                    TRUE ~ DIST / 25,
                ),
                TIME = 0.9
            )
        
        end_point <- end_point %>% 
            mutate(EX = pitch_transform(ifelse(input$next_state=='Corner',
                                               ifelse(possession=='A',122,-2),
                                               next_x),'X'),
                   EGY = ifelse(input$next_state=='Corner',
                                GY + runif(1)*5,
                                next_y),
                   EGY = ifelse(input$next_state=='Corner' & sign(EGY-40) != sign(next_y),
                                40 + runif(1)*4*sign(next_y-40),
                                EGY),
                   EY = pitch_transform(EGY,'Y'),
                   TIME = 1)
        
        MD <- sqrt((input$X - mid_point$MX)^2 + (input$Y - mid_point$MY)^2)
        ED <- sqrt((end_point$EX - mid_point$MX)^2 + (end_point$EY - mid_point$MY)^2)
        mid_point$TIME <- MD/(MD + ED)*0.7 + 0.3
        
        plot_input <- input %>% select(X,Y) %>% mutate(TIME=0) %>% 
            bind_rows(input %>% select(X,Y) %>% mutate(TIME=0.3)) %>% 
            bind_rows(mid_point %>% select(X=MX,Y=MY,TIME)) %>% 
            bind_rows(end_point %>% mutate(TIME=1) %>% select(X=EX,Y=EY,TIME))
        
    }else{
        plot_input <- input %>% select(X,Y) %>% mutate(TIME=0) %>% 
            bind_rows(end_point %>% mutate(TIME=DELAY) %>% select(X,Y,TIME)) %>% 
            bind_rows(end_point %>% mutate(TIME=1) %>% select(X=EX,Y=EY,TIME))
    }
    
    plot_input <- plot_input %>% 
        mutate(
            ANG = atan((max(Y[TIME==0])-max(Y[TIME==1]))/(max(X[TIME==0])-max(X[TIME==1]))),
            X = case_when(
                input$action=='PENALTY' ~ X,
                TIME==0 | (X==max(X[TIME==0]) & Y==max(Y[TIME==0])) ~ X + ifelse(input$possession=='A',1,-1)*17*abs(cos(ANG)),
                TRUE ~ X),
            Y = case_when(
                input$action=='PENALTY' ~ Y,
                TIME==0 | (Y==max(Y[TIME==0]) & Y==max(Y[TIME==0])) ~ Y + ifelse(input$ball_y > 40,-1,1)*17*abs(sin(ANG)),
                TRUE ~ Y)
        )
    
    base_image <- readPNG(paste0('output/layers/04/Trx_',input$period,'_',str_pad(input$time,4,pad='0'),'.png'))
    plot_output <- plot_input %>% 
        unique() %>% 
        ggplot(mapping = aes(x=X,y=Y)) +
        # background_image(readPNG('output/layers/01/Match.png')) +
        # background_image(base_image) +
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
        mutate(output = paste0('output/frames/',str_pad(IDX,5,pad='0'),'_',str_pad(REP,4,pad='0'),'.png')) %>% 
        subset(match_state%in%c('build_up','reaction'))

    new_frames %>% toJSON() %>% write(paste0('output/',shot_idx,'.json'))
    for(i in seq_along(new_frames$output)){
        this_output <- new_frames %>% slice(i)
        if(file.exists(this_output$output)) image_read(this_output$output) %>% 
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
        mutate(output = paste0('output/frames/',str_pad(IDX,5,pad='0'),'_',str_pad(REP,4,pad='0'),'.png')) %>% 
        subset(match_state%in%c('build_up','reaction')) %>% ungroup()
    
    for(j in seq_along(result_frames$output)){
        this_output <- result_frames %>% slice(j)
        if(file.exists(this_output$output)) image_read(this_output$output) %>% 
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
    
    #SAVED BUT NOT GK POSSESSION = BLOCKED WITH FIXED X
    
    file.remove(paste0('output/',shot_idx,'.txt'))
}