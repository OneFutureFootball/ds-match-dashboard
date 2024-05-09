goal_overlay <- function(idx){
    
    this_goal <- match_file %>% 
        subset(state=='Goal') %>% 
        group_by(ID) %>% 
        mutate(TOT = n(),
               N = row_number()) %>% 
        ungroup() %>% 
        select(ID,TOT,N,period,time,team_id,full_name,minute,prev_xz,prev_yz, prev_technique, prev_state, prev_action) %>% 
        left_join(teams,by='team_id') %>% 
        left_join(players %>% select(ID,flag),by='ID') %>% 
        slice(idx)
    
    if(nrow(this_goal)==0) return(NULL)
    
    goal_moments <- key_moments %>% 
        subset(str_detect(state,'Goal')) %>% 
        group_by(period) %>% 
        mutate(prev_time = lag(time),
               goal_time = case_when(
                   is.na(prev_time) ~ time,
                   time-prev_time>1 ~ time,
                   TRUE ~ NA_real_)) %>% 
        fill(goal_time,.direction='down') %>% 
        subset(period==this_goal$period & goal_time==this_goal$time)
    
    this_assist <- this_goal %>% 
        select(period,time) %>% 
        left_join(match_file %>% 
                      mutate(action = tolower(action)) %>% 
                      subset(action!='MOVE' & !(action=='DRIBBLE' & outcome=='completed')) %>% 
                      mutate(last_outcome=lag(outcome),
                             last_action=lag(action),
                             last_team=lag(team),
                             last_player=lag(full_name),
                             last_position=lag(position),
                             next_time = lead(time)) %>%
                      subset(outcome=='goal' & state!='Free Kick' & action!='PENALTY' &
                                 (((str_detect(last_action,'pass')|last_action%in%c('THROUGH BALL', 'CROSS', 'THROW IN', 'GOAL KICK')) & 
                                       team==last_team & 
                                       last_outcome%in%c('completed', 'incomplete') & last_player!=full_name) | (action=='SHOT' & last_action=='SHOT' & last_player!=full_name))) %>% 
                      mutate(time = next_time) %>% 
                      select(idx, time, period, minute, full_name=last_player, possession, team, position=last_position) %>% 
                      mutate(stat='assist'),by=c('period','time')) %>% 
        mutate(full_name = replace_na(full_name,'Unassisted'))
    
    goal_description <- ifelse(file.exists('input/goal_body.json'),
                               this_goal %>% left_join(fromJSON('input/goal_body.json'),by=c('period','time')) %>% pull(body),
                               this_goal %>% 
                                   mutate(goal_type=case_when(
                                       prev_action=='PENALTY' ~ 'penalty',
                                       prev_state=='Free Kick' ~ 'freekick',
                                       TRUE ~ ""
                                   )) %>% 
                                   left_join(fromJSON('data/player_identity.json') %>% select(full_name,foot),by=c('full_name'='full_name')) %>% 
                                   mutate(body = case_when(
                                       goal_type=='penalty' ~ 'Penalty',
                                       goal_type=='freekick' ~ 'Free Kick',
                                       prev_technique=='foot' ~ ifelse(runif(n())<=0.2,ifelse(foot=='Left','Right foot','Left foot'),paste(foot,'foot')),
                                       prev_technique=='volley' ~ ifelse(runif(n())<=0.2,ifelse(foot=='Left','Right foot volley','Left foot volley'),paste(foot,'foot volley')),
                                       prev_technique=='head' ~ 'Header')) %>% pull(body))
    
    goal_zone <- case_when(
        goal_description=='Penalty' ~ NA_character_,
        this_goal$prev_xz==5 & this_goal$prev_yz==2 ~ 'Inside the box',
        TRUE ~ 'Outside the box'
    )
    
    season_goals <- fromJSON('input/players_season.json') %>% 
        subset(ID==this_goal$ID & statistic=='Goals') %>% 
        pull(value) - this_goal$TOT + this_goal$N
    season_text <- paste0(toOrdinal::toOrdinal(season_goals),' goal this season')
    career_goals <- tail(c(0,fromJSON('data/historical_goals.json') %>% 
                               subset(ID==this_goal$ID) %>% 
                               pull(goals)),1) + season_goals
    career_text <- paste0(toOrdinal::toOrdinal(career_goals),' career goal')
    match_text <- NULL
    if(this_goal$N > 1) match_text <- paste0(toOrdinal::toOrdinal(this_goal$N),' goal this game.')
    goal_text <- sample(c(career_text, season_text, match_text),1)
    
    player_card <- paste0('https://1ff-data.s3.ap-southeast-2.amazonaws.com/player_cards/',match_details$season_no,'/',this_goal$ID,'.png')
    
    player_bio <- fromJSON('data/player_identity.json') %>% 
        subset(ID==this_goal$ID) %>% 
        left_join(teams %>% select(team_id,medium_name),by='team_id') %>% 
        left_join(player_trainers %>% select(ID=player_id,display_name),by='ID') %>% 
        rename(Club = medium_name,
               Country = nationality,
               Age = age,
               Height = height,
               Weight = weight,
               Foot = foot,
               `Top Trainer`=display_name) %>% 
        select(Club,Country,Age,Height,Weight,Foot,`Top Trainer`) %>% 
        gather('column','value') %>% 
        mutate(IDX = row_number(),
               X = 320,
               Y = 870 - 95*IDX) %>% 
        drop_na(value)
    
    goal_info <- this_goal %>% rename(Minute = minute) %>% select(Minute) %>% gather('column','value') %>% mutate(value=paste0(value,"'")) %>% 
        bind_rows(data.frame(column='Finish',value=goal_description)) %>% 
        bind_rows(data.frame(column='Location',value=goal_zone)) %>% 
        bind_rows(data.frame(column='Assist By',value=this_assist$full_name)) %>% 
        bind_rows(data.frame(column='Season Goals',value=as.character(season_goals))) %>% 
        bind_rows(data.frame(column='Career Goals',value=as.character(career_goals))) %>% 
        mutate(IDX = row_number(),
               X = 1600,
               Y = 840 - 95*IDX)
        
        for(i in seq_along(goal_moments$state)){
            this_moment <- goal_moments %>% slice(i)
            
            plot_output <- ggplot() +
                coord_cartesian(xlim = c(0,1920),
                                ylim = c(0,1080)) +
                theme_void()
            
            if(this_moment$state=='Goal_1') plot_output <- plot_output + geom_image(mapping = aes(x=960,y=540,image='images/goal_1.png'),size=0.8)
            if(this_moment$state=='Goal_2') plot_output <- plot_output + geom_image(mapping = aes(x=960,y=540,image='images/goal_2.png'),size=0.8)
            if(!str_detect(this_moment$state,'_')) plot_output <- plot_output + geom_image(mapping = aes(x=960,y=540,image='images/goal_background.png'),size=0.8)
            if(this_moment$state=='Goal') plot_output <- plot_output + geom_image(mapping = aes(x=960,y=540,image=player_card),size=0.4)
            if(this_moment$state=='Goal Text') plot_output <- plot_output + 
                background_image(readPNG('images/goal_text.png')) +
                geom_image(mapping = aes(x = 960, y=540,image=player_card), size=0.445) +
                geom_text(player_bio,
                          mapping = aes(x = X, y=Y+15, label=column),
                          family = 'Montserrat-ExtraBold', size=6, hjust=0.5, vjust=0.5, colour='white') +
                geom_text(player_bio,
                          mapping = aes(x = X, y=Y-15, label=value),
                          family = 'Montserrat-Medium', size=6, hjust=0.5, vjust=0.5, colour='white') +
                geom_text(goal_info,
                          mapping = aes(x = X, y=Y-15, label=value),
                          family = 'Montserrat-Medium', size=6, hjust=0.5, vjust=0.5, colour='white') +
                geom_text(goal_info,
                          mapping = aes(x = X, y=Y+15, label=column),
                          family = 'Montserrat-ExtraBold', size=6, hjust=0.5, vjust=0.5, colour='white')

            ggsave(paste0('output/layers/07/Overlay_',this_moment$period,'_',str_pad(this_moment$time,4,pad='0'),'.png'),
                   plot_output,
                   height=1080, width=1920, units='px', dpi=300)
            
        }
    
    #      +
    #     geom_text(this_assist, mapping = aes(x=960,y=250,label=paste0('Assisted by\n',toupper(full_name))),
    #               family='Montserrat-Bold', colour='white', size=8, hjust=0.5, vjust=1, lineheight=0.4)
    # if(frame==4) plot_output <- plot_output + 
    #     geom_image(mapping = aes(x=960,y=680,image=player_card),size=0.32) +
    #     geom_shape(mapping = aes(x = 960 + 400*c(-1,1,1,-1),
    #                              y = 220 + 100*c(-1,-1,1,1)),
    #                fill='black', alpha=0.7, radius = 0.023) +
    #     geom_text(this_assist, mapping = aes(x=960,y=300,label=paste0('Assisted by\n',toupper(full_name))),
    #               family='Montserrat-Bold', colour='white', size=8, hjust=0.5, vjust=1, lineheight=0.4) +
    #     geom_text(mapping = aes(x=960,y=140,label=goal_text),
    #               family='Montserrat-Bold', colour='white', size=8, hjust=0.5, vjust=0, lineheight=0.4)
    
}