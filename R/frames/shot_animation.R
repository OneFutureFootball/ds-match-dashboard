shot_animation <- function(shot_idx){
    for(i in list.files('output/gifs',full.names=TRUE)) file.remove(i)
    #KICK AT 25M/S
    #HEAD AT 10M/S
    input <- time_prog %>% 
        mutate(next_x = lead(ball_x),
               next_y = lead(ball_y)) %>% 
        subset(action%in%c('SHOT','PENALTY')) %>% 
        slice(shot_idx)
    
    goal_width <- data.frame(GY = seq(30,50,by=0.1)) %>% 
        mutate(GK = abs(GY-40)+0.1)
    end_point <- input %>% 
        mutate(
            DIST = sqrt((ifelse(possession=='A',120,0)-ball_x)^2 + (40-ball_y)^2),
            ANGLE = abs(ifelse(possession=='A',
                               atan((ball_y - 40)/(120-ball_x)),
                               atan((ball_y - 40)/ball_x)))/pi*180,
            GX = case_when(
                outcome=='goal' ~ ifelse(possession=='A',120,0),
                outcome=='saved' ~ ifelse(possession=='A',120 - runif(n())*2,0 + runif(n())*2),
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
            TIME = ifelse(TIME > 1,1,TIME),
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
        DEF <- pct[order(val)][1]
        
        mid_point <- end_point %>% 
            mutate(
                DX = ball_x*DEF + GX*(1-DEF),
                DY = ball_y*DEF + GY*(1-DEF),
                MX = pitch_transform(DX,'X'),
                MY = pitch_transform(DY,'Y'),
                DIST = sqrt((ball_x - DX)^2 + (ball_y - DY)^2),
                TIME = case_when(
                    technique == 'head' ~ DIST / 10,
                    TRUE ~ DIST / 25,
                ),
                TIME = 0.2 + DEF*0.8
            )
        
        end_point <- end_point %>% 
            mutate(EX = pitch_transform(next_x,'X'),
                   EY = pitch_transform(next_y,'Y'),
                   TIME = 1)
        
        plot_input <- input %>% select(X,Y) %>% mutate(TIME=0) %>% 
            bind_rows(input %>% select(X,Y) %>% mutate(TIME=0.2)) %>% 
            bind_rows(mid_point %>% select(X=MX,Y=MX,TIME)) %>% 
            bind_rows(end_point %>% mutate(TIME=1) %>% select(X=EX,Y=EY,TIME))
    }else if(input$outcome=='saved' & input$next_position!='GK'){
        DEF <- 1
        
        mid_point <- end_point %>% 
            mutate(
                DX = ball_x*DEF + GX*(1-DEF),
                DY = ball_y*DEF + GY*(1-DEF),
                MX = pitch_transform(DX,'X'),
                MY = pitch_transform(DY,'Y'),
                DIST = sqrt((ball_x - DX)^2 + (ball_y - DY)^2),
                TIME = case_when(
                    technique == 'head' ~ DIST / 10,
                    TRUE ~ DIST / 25,
                ),
                TIME = 0.2 + DEF*0.8
            )
        
        end_point <- end_point %>% 
            mutate(EX = pitch_transform(next_x,'X'),
                   EY = pitch_transform(next_y,'Y'),
                   TIME = 1)
        
        plot_input <- input %>% select(X,Y) %>% mutate(TIME=0) %>% 
            bind_rows(input %>% select(X,Y) %>% mutate(TIME=0.2)) %>% 
            bind_rows(mid_point %>% select(X=MX,Y=MX,TIME)) %>% 
            bind_rows(end_point %>% mutate(TIME=1) %>% select(X=EX,Y=EY,TIME))
        
    }else{
        plot_input <- input %>% select(X,Y) %>% mutate(TIME=0) %>% 
            bind_rows(end_point %>% mutate(TIME=DELAY) %>% select(X,Y,TIME)) %>% 
            bind_rows(end_point %>% mutate(TIME=1) %>% select(X=EX,Y=EY,TIME))
    }
    
    
    
    
    
    plot_output <- plot_input %>% 
        ggplot(mapping = aes(x=X,y=Y)) +
        geom_image(mapping=aes(image='images/icons/ball.png'),
                   size=0.018) +
        transition_time(TIME) +
        theme_void() +
        coord_cartesian(xlim = c(0,1920), ylim=c(0,1080))
    
    anim <- animate(plot_output, 
                    nframes = 30, 
                    fps = 30,
                    height=1080,
                    width=1920,
                    units='px', 
                    renderer = file_renderer(dir = "output/gifs", 
                                             prefix = paste0("GIF_",input$period,'_',str_pad(input$time,4,pad='0'),'_'), 
                                             overwrite = TRUE),
                    bg='transparent')
    
    gif_list <- data.frame(filename=list.files('output/gifs',full.names=TRUE)) %>% 
        mutate(period = as.numeric(sapply(filename,function(x) str_split(x,'_')[[1]][2])),
               time   = as.numeric(sapply(filename,function(x) str_split(x,'_')[[1]][3])),
               REP   = as.numeric(str_replace(sapply(filename,function(x) str_split(x,'_')[[1]][4]),'.png','')))
    
    new_frames <- frame_index %>% select(IDX,period,secs,REP,match_state) %>% uncount(REP) %>% 
        group_by(IDX) %>% mutate(REP = row_number()) %>% 
        inner_join(gif_list,by=c('period','secs'='time','REP')) %>% 
        mutate(output = paste0('output/frames/',str_pad(IDX,5,pad='0'),'_',str_pad(REP,4,pad='0'),'.png')) %>% 
        subset(match_state%in%c('build_up','reaction'))
    if(nrow(new_frames)==0) return(NULL)
    for(i in seq_along(new_frames$output)){
        this_output <- new_frames %>% slice(i)
        if(file.exists(this_output$output)) image_read(this_output$output) %>% 
            image_composite(image_read(this_output$filename)) %>% 
            image_write(this_output$output)
    }
    
    message(unique(new_frames$IDX))
    
    frame_list <- new_frames$output
    av::av_encode_video(frame_list,
                        framerate = 30,
                        output = paste0('output/chunks/',str_pad(unique(new_frames$IDX),5,pad='0'),'.mp4'),
                        verbose = FALSE)
    for(i in list.files('output/gifs',full.names=TRUE)) file.remove(i)
    
    #SAVED BUT NOT GK POSSESSION = BLOCKED WITH FIXED X
}