penalty_gif <- function(trx){
    horizon <- 700
    height <- 250
    net <- 15
    pre <- 150
    frames <- 10
    pause <- 10
    this_pen <- match_file %>% subset(action=='PENALTY') %>% slice(trx)
    outcome <- this_pen$outcome
    PID <- this_pen$ID
    PGK <- all_lineups %>% 
        subset(period<this_pen$period | (period==this_pen$period & time<=this_pen$time)) %>% 
        subset(team_id!=this_pen$team_id) %>% 
        subset(position=='GK') %>% 
        tail(1) %>% 
        pull(ID)
    idx <- paste0(this_pen$period,'_',str_pad(this_pen$time,4,pad='0'))
    
    
    if(outcome%in%c('goal','saved')){
        x_range <- seq(960 - height*3 + 50,
                       960 + height*3 - 50)
        y_range <- seq(horizon+20, horizon+height-50)
    } 
    
    if(outcome=='post'){
        miss_side <- sample(c('left','right','crossbar'),1,
                            prob = c(1,1,3))
        if(miss_side=='left'){
            x_range <- seq(960 - height*3 - 40, 
                           960 - height*3 + 40)
            y_range <- seq(horizon+20, horizon + height-50)
        }
        if(miss_side=='right'){
            x_range <- seq(960 + height*3 - 40, 
                           960 + height*3 + 40)
            y_range <- seq(horizon+20, horizon + height-50)
        }
        if(miss_side=='crossbar'){
            x_range <- seq(960 - height*3 + 50,
                           960 + height*3 - 50)
            y_range <- seq(horizon + height - 40,
                           horizon + height + 40)
        }
    } 
    
    if(outcome=='off target'){
        miss_side <- sample(c('left','right','high','topleft','topright'),1,
                            prob = c(3,3,8,1,1))
        if(miss_side=='left'){
            x_range <- seq(960 - height*3 - 40, 
                           960 - height*3 - 100)
            y_range <- seq(horizon+20, horizon + height-50)
        }
        if(miss_side=='right'){
            x_range <- seq(960 + height*3 + 40, 
                           960 + height*3 + 100)
            y_range <- seq(horizon+20, horizon + height-50)
        }
        if(miss_side=='high'){
            x_range <- seq(960 - height*3 + 50,
                           960 + height*3 - 50)
            y_range <- seq(horizon + height + 40,
                           horizon + height + 100)
        }
        if(miss_side=='topleft'){
            x_range <- seq(960 - height*3 - 40, 
                           960 - height*3 - 100)
            y_range <- seq(horizon + height + 40,
                           horizon + height + 100)
        }
        if(miss_side=='topright'){
            x_range <- seq(960 + height*3 + 40, 
                           960 + height*3 + 100)
            y_range <- seq(horizon + height + 40,
                           horizon + height + 100)
        }
    } 
    
    ball <- data.frame(time = 1,
                       x = 960, 
                       y = horizon - 2*height + 50) %>% 
        bind_rows(data.frame(time = frames+1,
                             x = sample(x_range,1,
                                        prob = abs(x_range - 960)/(height*3-60)),
                             y = sample(y_range,1))) %>% 
        bind_rows(data.frame(time = 2:(frames))) %>% 
        mutate(
            start_x = max(x[time==1]),
            end_x = max(x[time==frames+1]),
            start_y = max(y[time==1]),
            end_y = max(y[time==frames+1]),
            end_weight = (time-2)/(frames-1),
            end_weight = ifelse(end_weight<0,0,end_weight),
            x = start_x*(1-end_weight) + end_x*end_weight,
            y = start_y*(1-end_weight) + end_y*end_weight
        ) %>% 
        arrange(time)
    
    
    for(i in unique(ball$time)){
        this_ball <- ball %>% subset(time==i)
        plot_output <- ggplot() +
            coord_cartesian(xlim = c(0,1920),
                            ylim = c(0,1080)) +
            theme_void() +
            geom_rect(mapping = aes(xmin=-1000,xmax=3000,
                                    ymin=horizon,ymax=2000),
                      fill='white',colour=NA) +
            geom_rect(mapping = aes(xmin=-1000,xmax=3000,
                                    ymin=-1000,ymax=horizon),
                      fill='darkgreen',colour=NA) +
            geom_segment(mapping = aes(x = 960 + height*c(-3,3,-3),
                                       xend=c(960 + height*c(-3,3,3)),
                                       y = horizon + height*c(0,0,1),
                                       yend = horizon + height),
                         colour='black', linewidth=3, lineend='round') +
            geom_segment(mapping = aes(x = 960 - height*3,
                                       xend=960 + height*3,
                                       y = seq(horizon,horizon+height,length.out=net),
                                       yend = seq(horizon,horizon+height,length.out=net)),
                         colour='black', linewidth=0.1, lineend='round') +
            geom_segment(mapping = aes(x = seq(960 - height*3,960 + height*3, length.out=net*3),
                                       xend=seq(960 - height*3,960 + height*3, length.out=net*3),
                                       y = horizon,
                                       yend = horizon+height),
                         colour='black',linewidth=0.1, lineend='round') +
            geom_point(mapping = aes(x=960, y = horizon - 2*height), colour='white', pch=19, size=8) +
            geom_image(this_ball,
                       mapping = aes(x=x, y = y, image='images/icons/ball.png'),size=0.12 - (i-2)/(frames)*0.08)
        orig_plot <- plot_output
        if(i == 1) plot_output <- plot_output +
            geom_image(mapping = aes(x = 960 - 400, y=540 - 170, 
                                     image = paste0('https://1ff-data.s3.ap-southeast-2.amazonaws.com/player_cards/',
                                                    match_details$season_no,'/',PID,'.png')),
                       size = 0.25) +
            geom_image(mapping = aes(x = 960 + 400, y=540 + 170, 
                                     image = paste0('https://1ff-data.s3.ap-southeast-2.amazonaws.com/player_cards/',
                                                    match_details$season_no,'/',PGK,'.png')),
                       size = 0.25) +
            geom_shape(mapping = aes(x = 960 - 60*c(-1,1,1,-1),
                                     y = 540 - 60*c(-1,-1,1,1)),
                       radius = 0.01,
                       fill='#CA6FFF') +
            geom_text(mapping = aes(x = 960,y = 540,label='VS'),
                      family='Montserrat-Bold',colour='white',size=12, hjust=0.5, vjust=0.5)
        REP <- 0
        if(i==frames+1) REP <- seq(0,pause-1)
        if(i==1) REP <- seq(0,pre-1)
        if(i==2) REP <- seq(0,pause*6)
        png_out <- paste0('output/layers/10/Penalty_',
                          str_pad(idx,5,pad='0'),'_',
                          str_pad(i,3,pad='0'),'_',
                          str_pad(0,3,pad='0'),'.png')
        ggsave(png_out,
               plot_output,
               height = 1080,width=1920,
               units='px', dpi=300)
        # Hold on no cards and ball on the spot for a bit before taking the shot
        # orig_out <- paste0('output/layers/10/Penalty_',
        #                    str_pad(idx,5,pad='0'),'_',
        #                    str_pad(i,3,pad='0'),'_',
        #                    str_pad(0,3,pad='0'),'.png')
        # if(i==1) 
        for(j in REP[REP>0]) file.copy(png_out,
                                str_replace(png_out,
                                            paste0('_',str_pad(0,3,pad='0'),'.png'),
                                            paste0('_',str_pad(j,3,pad='0'),'.png')),
                                overwrite=TRUE)
    }
    output_pngs <- grep(list.files('output/layers/10',pattern=str_pad(idx,5,pad='0'),full.names=TRUE),pattern='png',value=TRUE)
    av::av_encode_video(output_pngs,
                        framerate = 30,
                        output = paste0('output/layers/10/Penalty_',str_pad(idx,5,pad='0'),'.mp4'))
    for(i in output_pngs) file.remove(i)
}