fixture <- fromJSON(list.files('data',pattern='fixture',full.names=TRUE))
teams <- fromJSON('data/team_list.json') %>% 
    mutate(crest = paste0('https://assets.1ff.com/crests/',short_name,'-256.png')) %>% 
    select(team_id = internal_id, short_name, medium_name, crest)
match_details <- fixture %>% subset(match_id%in%fromJSON('input/match.json')$match_id)
players <- fromJSON('data/player_map.json')
if(file.exists('graphics/broadcast/player_trainers.json')){
    player_trainers <- fromJSON('graphics/broadcast/player_trainers.json')
}else{
    player_trainers <- players %>% mutate(display_name=NA_character_)
}


home_colours <- NULL
away_colours <- NULL
home_banner <- readPNG(paste0('images/banners/',match_details$home_short_name,'-Home.png'))
away_banner <- readPNG(paste0('images/banners/',match_details$away_short_name,'-Away.png'))
home_colour <- data.frame(RED = as.numeric(home_banner[,,1]),
                          GRN = as.numeric(home_banner[,,2]),
                          BLU = as.numeric(home_banner[,,3])) %>% 
    group_by(RED,GRN,BLU) %>% 
    count() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    head(1) %>% 
    mutate(RGB = paste0('#',
                        str_pad(as.hexmode(round(RED*255)),2,pad='0'),
                        str_pad(as.hexmode(round(GRN*255)),2,pad='0'),
                        str_pad(as.hexmode(round(BLU*255)),2,pad='0'))) %>% 
    pull(RGB)
away_colour <- data.frame(RED = as.numeric(away_banner[,,1]),
                          GRN = as.numeric(away_banner[,,2]),
                          BLU = as.numeric(away_banner[,,3])) %>% 
    group_by(RED,GRN,BLU) %>% 
    count() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    head(1) %>% 
    mutate(RGB = paste0('#',
                        str_pad(as.hexmode(round(RED*255)),2,pad='0'),
                        str_pad(as.hexmode(round(GRN*255)),2,pad='0'),
                        str_pad(as.hexmode(round(BLU*255)),2,pad='0'))) %>% 
    pull(RGB)
kit_home <- rep('white',12); names(kit_home) <- 1:12
kit_home[names(kit_home%in%c())] <- 'black'

kit_away <- rep('black',12); names(kit_away) <- 1:12
kit_away[names(kit_away%in%c())] <- 'white'

kit_colours <- c(kit_home[names(kit_home)==this_match$home],
                 kit_away[names(kit_away)==this_match$away])
names(kit_colours) <- paste0('kit_',names(kit_colours))
team_colours <- c(home_colour,away_colour)
names(team_colours) <- c(match_details$home_id, match_details$away_id)
text_colours <- sapply(team_colours,
                       function(x) data.frame(colour=x) %>% 
                           mutate(
                               red = col2rgb(colour)[1,]/255,
                               green = col2rgb(colour)[2,]/255,
                               blue = col2rgb(colour)[3,]/255,
                               brightness = 0.2126*red + 0.7152*green + 0.0722*blue,
                               text_colour = ifelse(brightness>=0.70,'#150928','#FFFFFF')
                           ) %>% 
                           pull(text_colour))
text_colours <- c(text_colours,text_colours)
names(text_colours) <- c(match_details$home_short_name,match_details$away_short_name,match_details$home_id, match_details$away_id)
text_colours <- c(text_colours,kit_colours)

match_file <- fromJSON('input/match_output.json') %>% 
    mutate(outcome = ifelse(outcome=='wayward','off target',outcome)) %>% 
    left_join(teams %>% select(team_id,short_name),by='team_id') %>% 
    left_join(fromJSON('data/player_identity.json') %>% select(ID,number),by='ID') %>% 
    mutate(
        ball_x = replace_na(X,108),
        ball_y = replace_na(Y,40)
    ) %>% 
    select(-c(X,Y)) %>% 
    group_by(period) %>% 
    mutate(
        ball_x=ifelse(possession=='B',120-ball_x,ball_x),
        ball_y=ifelse(possession=='B',80-ball_y,ball_y),
        prev_state = lag(state),
        prev_player = lag(ID),
        prev_outcome = lag(outcome),
        prev_outcome2 = lag(outcome,2),
        prev_position = 'GK',
        next_state = lead(state),
        prev_x = lag(ball_x),
        prev_y = lag(ball_y),
        prev_x = ifelse(state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick')|prev_outcome=='blocked'|(prev_outcome=='saved' & position!='GK'),NA_real_,prev_x),
        prev_y = ifelse(state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick')|prev_outcome=='blocked'|(prev_outcome=='saved' & position!='GK'),NA_real_,prev_y),
        prev_x2 = ifelse(is.na(prev_x),NA_real_,lag(ball_x,2)),
        prev_y2 = ifelse(is.na(prev_y),NA_real_,lag(ball_y,2)),
        prev_number = ifelse(state%in%c('Kickoff','Throw In','Corner','Free Kick','Keeper Possession','Goal Kick'),NA_real_, lag(number)),
        prev_number2 = ifelse(is.na(prev_number),NA_real_, lag(number,2)),
        prev_short_name = ifelse(state%in%c('Kickoff','Throw In','Corner','Free Kick','Keeper Possession','Goal Kick'),NA_character_, lag(short_name)),
        prev_short_name2 = ifelse(is.na(prev_number),NA_character_, lag(short_name,2)),
        prev_x2 = ifelse(prev_state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick')|prev_outcome2=='blocked'|(prev_outcome2=='saved' & prev_position!='GK'),NA_real_,prev_x2),
        prev_y2 = ifelse(prev_state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick')|prev_outcome2=='blocked'|(prev_outcome2=='saved' & prev_position!='GK'),NA_real_,prev_y2),
        next_x = case_when(
            action%in%c('penalty','shoot') ~ 60 + sign(ball_x - 60)*60,
            state=='Goal' ~ NA_real_,
            next_state=='Corner' ~ 60 + sign(ball_x - 60)*60,
            TRUE ~ lead(ball_x)
        ),
        next_y = case_when(
            action%in%c('penalty','shoot') ~ 40,
            state=='Goal' ~ NA_real_,
            next_state=='Corner' & action!='shoot' & sign(lead(ball_y)-40)==1 ~ runif(n(),min = min(c(44,max(c(ball_y-20,44)))), max=max(c(44,min(c(ball_y+20,80))))),
            next_state=='Corner' & action!='shoot' & sign(lead(ball_y)-40)!=1 ~ runif(n(),min = min(c(44,max(c(ball_y-20,44)))), max=max(c(44,min(c(ball_y+20,80))))),
            TRUE ~ lead(ball_y)),
        prev_team = lag(team_id),
        prev_team2= lag(team_id,2),
        next_team = lead(team_id)
    ) %>% 
    ungroup() %>% 
    mutate(
        action = case_when(
            str_detect(action,'pass') ~ 'PASS',
            action%in%c('through ball','cross') ~ 'PASS',
            action=='move' ~ 'DRIBBLE',
            action=='carry' ~ 'DRIBBLE',
            state=='Goal' ~ 'GOAL',
            state=='Corner' ~ 'CORNER',
            state=='Free Kick' & action=='shoot' ~ 'SHOT',
            state=='Free Kick' & action=='pass' ~ 'FREE KICK',
            state=='Goal Kick' ~ 'GOAL KICK',
            action=='shoot' ~ 'SHOT',
            TRUE ~ toupper(action)
        )) %>%
    left_join(fromJSON('data/player_identity.json') %>% select(ID,last_name),by='ID') %>% 
    left_join(fromJSON('data/player_identity.json') %>% select(oth_ID=ID,oth_last_name=last_name),by='oth_ID') %>% 
    arrange(period,time) %>% 
    mutate(last_name = toupper(ifelse(is.na(last_name),full_name,last_name)),
           oth_last_name = toupper(ifelse(is.na(oth_last_name),oth_full_name,oth_last_name)),
           LAB = ifelse(is.na(action),NA_character_,last_name),
           next_action = lead(action),
           next_state = lead(state),
           next_card = lead(card_given),
           prev_action = lag(action),
           prev_state = lag(state),
           prev_technique = lag(technique),
           prev_time = lag(time),
           next_time = lead(time),
           next_time2= lead(time,2),
           prev_xz = lag(x),
           prev_yz = lag(y),
           time = case_when(
               state=='Goal' & time==prev_time ~ time+1,
               state=='Free Kick' & next_time-time<=1 ~ time-2,
               state=='Free Kick' & next_time-time<=2 ~ time-1,
               TRUE ~ time))
match_file %>% toJSON() %>% write(file='output/match_file.json')



all_ratings <- fromJSON('input/ratings_timestamp.json') %>% 
    mutate(value = ifelse(period==1 & time<=5,0,replace_na(value,0)),
           PCT = value/100) %>% 
    subset(statistic=='Player Rating') %>% 
    left_join(players,by=c('ref'='ID')) %>% 
    arrange(period,time,desc(value),full_name) %>% 
    left_join(teams,by='team_id')

all_stats <- data.frame(statistic = c('Shots','Shots On Target','Expected Goals','Final Third Entries','Fouls Committed','Yellow Cards','Red Cards','Possession %','Goals')) %>% 
    left_join(fromJSON('input/stats_timestamp.json'),by='statistic') %>% 
    subset(!(period==1 & time==0)) %>% 
    rename(team_id = ref) %>% 
    mutate(possession = ifelse(team_id==match_details$home_id, 'A', 'B')) %>% 
    group_by(team_id, period, time) %>% 
    mutate(IDX = row_number()) %>% 
    mutate(display = ifelse(statistic=='Yellow Cards',
                            paste0(sum(ifelse(statistic=='Yellow Cards',value,0)),'/',sum(ifelse(statistic=='Red Cards',value,0))),
                            display),
           statistic = ifelse(statistic=='Yellow Cards','YELLOW/RED CARDS',toupper(statistic))) %>% 
    subset(statistic!='RED CARDS') %>% 
    group_by(statistic, period, time) %>% 
    mutate(RANK = min_rank(-value),
           Y = case_when(statistic=='GOALS' ~ 1030,
                         TRUE ~ ifelse(statistic=='POSSESSION %',15,0) + 310 - 40*IDX),
           X = case_when(statistic=='GOALS' ~ ifelse(possession=='A',
                                                     960 - 40, 
                                                     960 + 40),
                         TRUE ~ 135 + ifelse(possession=='A',-140,140)),
           X1 = 135 + ifelse(possession=='A',-170,170),
           X2 = ifelse(possession=='A',
                       X1 + 340*value,
                       X1 - 340*value),
           KEEP = !statistic%in%c('GOALS')) %>% 
    group_by(statistic,period,time,value) %>% 
    mutate(N = n()) %>% 
    left_join(teams,by='team_id')

all_lineups <- fromJSON('input/match_formations.json') %>% 
    left_join(fromJSON('input/match_lineups.json') %>% select(ID,number,last_name),by='ID') %>% 
    mutate(period = ifelse(period==0,1,period),
           last_name = toupper(last_name),
           time = round(time)) %>% 
    arrange(period,time)

lineup_times <- all_lineups %>% 
    select(period,time) %>% 
    unique() %>% 
    mutate(TRX = row_number())
if(exists('cross_join')){
    lineup_times <- lineup_times %>% cross_join(data.frame(team_id = c(match_details$home_id,match_details$away_id)))
}else{
    lineup_times <- lineup_times %>% left_join(data.frame(team_id = c(match_details$home_id,match_details$away_id)),by=character())
}
lineup_times <- lineup_times %>% 
    uncount(11) %>% 
    group_by(period,time,team_id) %>% 
    mutate(IDX = row_number()) %>% 
    left_join(all_lineups %>% group_by(period,time,team_id) %>% mutate(IDX=row_number())
              ,by=c('period','time','team_id','IDX')) %>% 
    select(period,time,team_id,TRX,IDX,team_class,ID,full_name,position,side,number,last_name) %>% 
    mutate(last_name = ifelse(is.na(last_name),toupper(full_name),last_name)) %>% 
    group_by(period,time,team_id) %>% 
    mutate(FULL = sum(!is.na(position)),
           FULL = ifelse(FULL==0,NA,FULL)) %>% 
    group_by(team_id,IDX) %>% 
    fill(FULL,.direction='down') %>% 
    subset(!(FULL==10 & IDX==11)) %>% 
    group_by(team_id,IDX) %>% 
    fill(c(team_class,ID,full_name,position,side,number,last_name),.direction='down')


time_base <- match_file %>% 
    subset(period>0) %>% 
    group_by(period) %>% 
    arrange(desc(time)) %>% 
    slice(1) %>% 
    select(period,time) %>% 
    mutate(time = ceiling(time)+1) %>% 
    uncount(time) %>% 
    group_by(period) %>% 
    mutate(secs = row_number()-1,
           mins = floor(secs/60),
           next_secs = lead(secs)) %>% 
    ungroup() %>% 
    mutate(IDX = row_number(),
           TIME = case_when(
               period==1 & secs <= (45*60) ~ paste0(floor(secs/60),':',str_pad(secs%%60,2,pad='0')),
               period==1 ~ paste0('45+',floor(secs/60)-45,':',str_pad(secs%%60,2,pad='0')),
               period==2 & secs <= (45*60) ~ paste0(45+floor(secs/60),':',str_pad(secs%%60,2,pad='0')),
               period==2 ~ paste0('90+',floor(secs/60)-45,':',str_pad(secs%%60,2,pad='0')),
               period==3 & secs <= (15*60) ~ paste0(90+floor(secs/60),':',str_pad(secs%%60,2,pad='0')),
               period==3 ~ paste0('105+',floor(secs/60)-15,':',str_pad(secs%%60,2,pad='0')),
               period==3 & secs <= (15*60) ~ paste0(105+floor(secs/60),':',str_pad(secs%%60,2,pad='0')),
               period==4 ~ paste0('120+',floor(secs/60)-15,':',str_pad(secs%%60,2,pad='0'))
           ),
           KEY = 1 + IDX%%active_cores)

key_moments <- match_file %>% 
    mutate(prev_time = lag(time),
           prev_mt = lag(match_time)) %>% 
    mutate(old_time = time,
           time = ifelse(state%in%c('Corner','Free Kick'),prev_time+2,time)) %>% 
    group_by(period) %>% 
    arrange(time) %>% 
    mutate(next_time = lead(time),
           next_state = lead(state),
           prev_time2 = lag(time,2),
           prev_role = lag(oth_role),
           prev_role2 = lag(oth_role,2),
           prev_team = lag(team_id)) %>% 
    mutate(
        time = case_when(
            !is.na(card_given) & !action%in%c('shoot','penalty') ~ time,
            state%in%c('Corner','Free Kick') ~ old_time,
            TRUE ~ time),
        next_time = case_when(
            state=='Substitution' & oth_role=='injury' ~ round(old_time),
            state=='Substitution' & prev_role=='injury' & time==prev_time & team_id==prev_team ~ NA_real_,
            state=='Substitution' & prev_role2=='injury' & time==prev_time2 & team_id==prev_team ~ NA_real_,
            TRUE ~ next_time),
        time = case_when(
            state=='Substitution' & oth_role=='injury' ~ ceiling((prev_time + time)/2),
            state=='Substitution' & prev_role=='injury' & time==prev_time & team_id==prev_team ~ NA_real_,
            state=='Substitution' & prev_role2=='injury' & time==prev_time2 & team_id==prev_team ~ NA_real_,
            TRUE ~ time)) %>% 
    fill(c(time,next_time),.direction='down') %>% 
    ungroup() %>% 
    arrange(period,time) %>% 
    subset(state%in%c('Substitution','Kickoff','Corner')|
               action%in%c('SHOT','PENALTY')|
               !is.na(card_given)) %>% 
    select(period,time,prev_time,next_time,old_time,next_state,possession,state,action,outcome,position,ID,full_name,last_name,oth_ID,oth_position, oth_full_name,oth_role,oth_team,team_id, card_given) %>% 
    mutate(
        live_label = case_when(
            action=='KICKOFF' ~ action,
            action=='SHOT' ~ 'SHOT',
            state=='Substitution' & oth_role=='injury' ~ 'INJURY',
            state=='Substitution' ~ paste0(' > ',toupper(oth_full_name)),
            state=='Corner' ~ 'CORNER',
            action=='PENALTY' ~ 'PENALTY',
            card_given=='red' ~ 'RED CARD',
            card_given=='yellow' ~ 'YELLOW CARD',
            card_given=='yellow-red' ~ 'SECOND YELLOW CARD'
        ),
        post_label = case_when(
            action=='SHOT' ~ paste0(case_when(
                outcome=='goal' ~ 'GOAL',
                outcome=='saved' ~ 'SHOT SAVED',
                outcome=='blocked' ~ 'SHOT BLOCKED',
                TRUE ~ 'SHOT OFF TARGET'
            ),ifelse(next_state=='Corner',' - CORNER','')),
            action=='PENALTY' ~ case_when(
                outcome=='goal' ~ 'PENALTY GOAL',
                outcome=='saved'~ 'PENALTY SAVED',
                TRUE ~ 'PENALTY MISSED',
            ),
            state=='Substitution' ~ paste0(' > ',toupper(oth_full_name)),
            TRUE ~ live_label
        ),
        team_id = case_when(
            action%in%c('SHOT','PENALTY') ~ team_id,
            !is.na(card_given) ~ ifelse(team_id==match_details$home_id,match_details$away_id,match_details$home_id),
            TRUE ~ team_id
        ),
        full_name = case_when(
            action=='SHOT' ~ full_name,
            action=='PENALTY' ~ full_name,
            !is.na(card_given) ~ oth_full_name,
            TRUE ~ full_name
        ),
        position = case_when(
            action=='SHOT' ~ position,
            action=='PENALTY' ~ position,
            !is.na(card_given) ~ oth_position,
            TRUE ~ position
        ),
        IDX = row_number()
    ) %>% 
    group_by(period,time) %>% 
    mutate(shot = sum(ifelse(action%in%c('SHOT','PENALTY'),1,0))) %>% 
    ungroup() %>% 
    subset(!(state=='Corner' & shot>0)) %>% 
    subset(!(state=='Substitution' & is.na(full_name)))

key_moments <- key_moments %>% 
    bind_rows(key_moments %>%
                  drop_na(card_given) %>% 
                  subset(action%in%c('SHOT','PENALTY')) %>% 
                  mutate(time = prev_time + 8,
                         live_label = case_when(
                             card_given=='red' ~ 'RED CARD',
                             card_given=='yellow' ~ 'YELLOW CARD',
                             card_given=='yellow-red' ~ 'SECOND YELLOW CARD'
                         ),
                         team_id = ifelse(team_id==match_details$home_id,match_details$away_id,match_details$home_id),
                         post_label = live_label,
                         full_name = oth_full_name,
                         position = oth_position)) %>% 
    bind_rows(key_moments %>% 
                  subset(action=='PENALTY') %>% 
                  mutate(state='PENALTY',
                         action='CONCEDED',
                         live_label = 'PENALTY CONCEDED',
                         team_id = ifelse(team_id==match_details$home_id,match_details$away_id,match_details$home_id),
                         post_label = live_label,
                         full_name = oth_full_name,
                         position = oth_position)) %>% 
    #First Goal overlay is 10 seconds after the goal
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+10)) %>% 
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+0, state='Goal_2')) %>% 
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+1, state='Goal_1')) %>% 
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+2, state='Goal_2')) %>% 
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+3, state='Goal_1')) %>% 
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+4, state='Goal_2')) %>% 
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+5, state='Goal_1')) %>% 
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+6, state='Goal_2')) %>% 
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+7, state='Goal_1')) %>% 
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+8, state='Goal_2')) %>% 
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+9, state='Goal_1')) %>% 
    bind_rows(match_file %>% subset(state=='Goal') %>% select(period,time,state,possession,team_id,last_name) %>% mutate(time = time+11, state='Goal Text')) %>% 
    arrange(period,time,desc(state)) %>% 
    mutate(
        MIN = case_when(
            period==1 ~ paste0(ifelse(time<=60*45,ceiling(time/60),paste0('45+',ceiling(time/60)-45)),"'"),
            period==2 ~ paste0(ifelse(time<=60*45,ceiling(time/60)+45,paste0('90+',ceiling(time/60)-45)),"'"),
            period==3 ~ paste0(ifelse(time<=60*15,ceiling(time/60)+90,paste0('105+',ceiling(time/60)-15)),"'"),
            period==4 ~ paste0(ifelse(time<=60*15,ceiling(time/60)+105,paste0('120+',ceiling(time/60)-15)),"'")
        ),
        IDX = row_number(),
        KEY = 1 + IDX%%active_cores,
        time = round(time),
        time = case_when(
            str_detect(state,'Goal') ~ time,
            state=='PENALTY' ~ prev_time + 3,
            action=='PENALTY' & !str_detect(live_label,'CARD') ~ time - 2,
            TRUE ~ time),
        next_time = round(case_when(
            state%in%c('Corner','Free Kick','Substition') ~ old_time,
            next_time - time < 5 ~ next_time,
            TRUE ~ time + 3 + 4*runif(n())))
    ) %>% 
    arrange(period,time) %>% 
    left_join(teams,by='team_id')

stat_times <- all_stats %>% 
    ungroup() %>% 
    select(period,time) %>% 
    unique() %>% 
    bind_rows(data.frame(period=1,time=0)) %>% 
    arrange(period,time) %>% 
    mutate(KEY = row_number()%%active_cores + 1)

trx_list <- time_base %>% 
    inner_join(match_file %>% subset(state!='Substitution'),by=c('period'='period','secs'='time')) %>% 
    select(period,secs,IDX,possession) %>% 
    mutate(KEY = 1 + row_number()%%active_cores)

pitch_transform <- function(x,coord='X') ifelse(coord=='X',
                                                420 + (1500-420)*x/120,
                                                140 + (805-140)*x/80)

time_prog <- match_file %>% 
    subset(state!='Substitution') %>%
    subset(period>0) %>% 
    mutate(old_time = time,
           time = case_when(
               action=='PENALTY' ~ time - 10,
               TRUE ~ time)) %>% 
    ungroup() %>% 
    mutate(
        X = sapply(ball_x,function(x) pitch_transform(x,'X')),
        Y = sapply(ball_y,function(x) pitch_transform(x,'Y')),
        X2= sapply(prev_x,function(x) pitch_transform(x,'X')),
        Y2= sapply(prev_y,function(x) pitch_transform(x,'Y')),
        X3= sapply(prev_x2,function(x) pitch_transform(x,'X')),
        Y3= sapply(prev_y2,function(x) pitch_transform(x,'Y')),
        X4= sapply(next_x,function(x) pitch_transform(x,'X')),
        Y4= sapply(next_y,function(x) pitch_transform(x,'Y')),
        IDX = row_number(),
        KEY = 1 + IDX%%active_cores)

trx_frames <- time_prog %>% 
    select(IDX,period,time,old_time,crest=possession,team_id,state,action,technique,outcome,full_name,card_given) %>% 
    group_by(period) %>% 
    arrange(period,time) %>% 
    mutate(next_time = lead(time),
           next_action=lead(action),
           prev_time = lag(time),
           prev_action=lag(action),
           prev_state = lag(state),
           next_poss = lead(crest),
           next_card = lead(card_given),
           next_state = lead(state)) %>% 
    ungroup() %>% 
    mutate(
        action_time = case_when(
            is.na(prev_time) ~ 1,
            action=='PENALTY' ~ old_time,
            TRUE ~ time),
        possession_time = case_when(
            state=='Kickoff' ~ time,
            technique=='head' ~ NA_real_,
            action=='PENALTY' ~ time,
            prev_action=='PENALTY' ~ time-1,
            state%in%c('Free Kick') & str_detect(card_given,'red') & time-prev_time >=11 ~ time - 10,
            state%in%c('Free Kick','Corner','Goal Kick','Keeper Possession','Throw In') & time-prev_time > 7 ~ prev_time + 5,
            time - prev_time<=1 ~ NA_real_,
            time - prev_time<=3 ~ time-1,
            time - prev_time==4 ~ time-2,
            time - prev_time==5 ~ time-2,
            TRUE ~ ceiling(0.4*prev_time + 0.6*time)
        ),
        result_time = case_when(
            state=='Kickoff' & next_time - time > 3 ~ time+2,
            state=='Kickoff' ~ NA_real_,
            action=='PENALTY' & outcome!='goal' ~ next_time-2,
            action=='PENALTY' & outcome=='goal' ~ next_time,
            next_state=='Goal' ~ NA_real_,
            next_state=='Free Kick' & str_detect(next_card,'red') ~ time + 2,
            next_state%in%c('Free Kick','Corner','Goal Kick','Keeper Possession','Throw In') & next_time-time > 7 ~ time + 3,
            next_time - time==1 ~ NA_real_,
            next_time - time<=4 ~ time+1,
            next_time - time<=6 ~ time+2,
            TRUE ~ ceiling(0.7*time + 0.3*next_time)
        )
    ) %>% 
    gather('type','timestamp',c(action_time,possession_time,result_time)) %>% 
    mutate(type = str_replace(type,'_time',''),
           crest = ifelse(type=='result',NA_character_,crest)) %>% 
    arrange(period,timestamp) %>% 
    group_by(period) %>% 
    fill(c(crest),.direction='down') %>% 
    ungroup() %>% 
    drop_na(timestamp) %>% 
    select(IDX,type,period,timestamp,old_time,crest,action) %>% 
    arrange(IDX,period,timestamp) %>% 
    group_by(period) %>% 
    mutate(next_time = lead(timestamp),
           prev_time = lag(timestamp)) %>% 
    subset(action=='PENALTY'|is.na(next_time)|is.na(prev_time)|(timestamp < next_time & timestamp > prev_time)) %>% 
    ungroup() %>% 
    mutate(ORD = row_number(),
           KEY = 1 + ORD%%active_cores)

key_moments <- key_moments %>% 
    left_join(
        trx_frames %>% 
            select(IDX,period,old_time,type,timestamp) %>% 
            mutate(type = paste0(type,'_time')) %>% 
            spread(type,timestamp) %>% 
            select(period,old_time,action_time,result_time,possession_time),
        by=c('period'='period','old_time'='old_time')) %>% 
    group_by(period,time) %>% 
    mutate(
        time = case_when(
            is.na(action_time) ~ min(time),
            action=='PENALTY' & !str_detect(live_label,'CARD') ~ possession_time,
            TRUE ~ time
        ),
        next_time = case_when(
            is.na(result_time) ~ min(next_time),
            action=='SHOT' ~ result_time,
            action=='PENALTY' ~ result_time,
            TRUE ~ next_time
        )) %>% 
    ungroup() %>% 
    select(-c(action_time,result_time)) %>% 
    arrange(period,time) %>% 
    mutate(IDX=row_number())
