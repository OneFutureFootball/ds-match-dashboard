this_period <- 1
this_time <- c(48,41); this_time <- 60*this_time[1] + this_time[2] - (45*60*(this_period-1))

match_file %>% subset(time>=this_time) %>% 
    subset(period==this_period) %>% 
    subset(match_time!='02:00') %>% 
    select(period,match_time,full_name,action,outcome,ball_x,ball_y,xg) %>% 
    head(10) %>% 
    print()

match_file %>% 
    mutate(
        ball_y = case_when(
            TRUE ~ ball_y
        ),
        ball_x = case_when(
            period==1 & match_time=='48:41' ~ 81,
            period==1 & match_time=='48:45' ~ 57,
            TRUE ~ ball_x
        )
    ) %>% 
    group_by(period) %>% 
    mutate(prev_x = lag(ball_x),
           prev_y = lag(ball_y),
           prev_x = ifelse(state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick'),NA_real_,prev_x),
           prev_y = ifelse(state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick'),NA_real_,prev_y),
           prev_x2 = ifelse(is.na(prev_x),NA_real_,lag(ball_x,2)),
           prev_y2 = ifelse(is.na(prev_y),NA_real_,lag(ball_y,2)),
           prev_x2 = ifelse(prev_state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick'),NA_real_,prev_x2),
           prev_y2 = ifelse(prev_state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick'),NA_real_,prev_y2),
           next_x = case_when(
               action=='SHOT' ~ 60 + sign(ball_x - 60)*60,
               state=='Goal' ~ NA_real_,
               next_state=='Corner' ~ 60 + sign(ball_x - 60)*60,
               TRUE ~ lead(ball_x)
           ),
           next_y = case_when(
               action=='SHOT' ~ 40,
               state=='Goal' ~ NA_real_,
               next_state=='Corner' & action!='SHOT' & sign(lead(ball_y)-40)==1 ~ runif(n(),min = min(c(44,max(c(ball_y-20,44)))), max=max(c(44,min(c(ball_y+20,80))))),
               next_state=='Corner' & action!='SHOT' & sign(lead(ball_y)-40)!=1 ~ runif(n(),min = min(c(44,max(c(ball_y-20,44)))), max=max(c(44,min(c(ball_y+20,80))))),
               TRUE ~ lead(ball_y)
           )) %>% 
    ungroup() %>% 
    toJSON(pretty=TRUE) %>% 
    write(file='output/match_file.json')
match_file <- fromJSON('output/match_file.json')
source('R/support/data_prep.R')



time_prog %>%
    subset(abs(idx - time_prog %>% subset(period==this_period & time==this_time) %>% pull(idx))<=5) %>% 
    left_join(fromJSON('data/player_identity.json') %>% select(ID,number),by='ID',suffix=c('','_ply')) %>% 
    ggplot() +
    background_image(readPNG('images/pitch.png')) +
    theme_void() +
    geom_point(mapping = aes(x=X,y=Y, colour=factor(team_id)), size=6) +
    geom_text(mapping = aes(x=X,y=Y-15, label=paste0(full_name,'\n',match_time,' (',round(ball_x),',',round(ball_y),')')), 
              colour='black', size=4,vjust=1,lineheight=0.7) +
    geom_text(mapping = aes(x=X,y=Y, label=number), colour='red', size=4) +
    coord_cartesian(xlim = c(9,457),
                    ylim = c(114,702)) +
    scale_colour_manual(values = team_colours, guide='none')
