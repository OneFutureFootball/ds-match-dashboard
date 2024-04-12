fixture <- fromJSON(list.files('data',pattern='fixture',full.names=TRUE))

if(file.exists('output/match_file.json')){
    match_file <- fromJSON('output/match_file.json')
}else{
    match_file <- fromJSON('input/match_output.json') %>% 
        add_xy() %>% 
        mutate(action = case_when(
            str_detect(action,'pass') ~ 'PASS',
            state=='Goal' ~ 'GOAL',
            state=='Corner' ~ 'CORNER',
            state=='Free Kick' & action=='shoot' ~ 'FREE KICK',
            state=='Free Kick' & action=='pass' ~ 'FREE KICK',
            state=='Goal Kick' ~ 'GOAL KICK',
            action=='shoot' ~ 'SHOT',
            TRUE ~ toupper(action)
        )) %>%
        left_join(fromJSON('data/player_identity.json') %>% select(ID,last_name),by='ID') %>% 
        mutate(last_name = toupper(ifelse(is.na(last_name),full_name,last_name)),
               LAB = ifelse(is.na(action),NA_character_,paste(last_name,action,sep='\n')),
               next_action = lead(action),
               next_state = lead(state),
               prev_action = lag(action),
               prev_state = lag(state),
               prev_technique = lag(technique),
               prev_time = lag(time),
               next_time = lead(time),
               next_time2= lead(time,2),
               prev_xz = lag(x),
               prev_yz = lag(y),
               time = ifelse(state=='Goal' & time==prev_time,time+1,time))
    match_file %>% toJSON() %>% write(file='output/match_file.json')
}

match_details <- fixture %>% subset(match_id%in%match_file$match_id)
players <- fromJSON('data/player_map.json')

teams <- fromJSON('data/team_list.json') %>% 
    mutate(crest = paste0('https://assets.1ff.com/crests/',short_name,'-256.png')) %>% 
    select(team_id = internal_id, short_name, medium_name, crest)

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
team_colours <- c(home_colour,away_colour)
names(team_colours) <- c(match_details$home_id, match_details$away_id)
text_colours <- sapply(team_colours,
                       function(x) data.frame(colour=x) %>% 
                           mutate(
                               red = col2rgb(colour)[1,]/255,
                               green = col2rgb(colour)[2,]/255,
                               blue = col2rgb(colour)[3,]/255,
                               brightness = 0.2126*red + 0.7152*green + 0.0722*blue,
                               text_colour = ifelse(brightness < 1 & brightness>=0.70,'#150928','#FFFFFF')
                           ) %>% 
                           pull(text_colour))
text_colours <- c(text_colours,text_colours)
names(text_colours) <- c(match_details$home_short_name,match_details$away_short_name,match_details$home_id, match_details$away_id)

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
           Y = case_when(statistic=='GOALS' ~ 1000,
                         TRUE ~ 486 - 58*IDX),
           X = case_when(statistic=='GOALS' ~ ifelse(possession=='A',
                                                     960 - 40, 
                                                     960 + 40),
                         TRUE ~ 960 + ifelse(possession=='A',-310,310)),
           X1 = 960 + ifelse(possession=='A',-360,360),
           X2 = ifelse(possession=='A',
                       X1 + 720*value,
                       X1 - 720*value),
           KEEP = !statistic%in%c('GOALS')) %>% 
    group_by(statistic,period,time,value) %>% 
    mutate(N = n()) %>% 
    left_join(teams,by='team_id')

all_lineups <- fromJSON('input/match_formations.json') %>% 
    left_join(fromJSON('input/match_lineups.json') %>% select(ID,number,last_name),by='ID') %>% 
    mutate(period = ifelse(period==0,1,period),
           last_name = toupper(last_name))
lineup_times <- all_lineups %>% 
    select(period,time) %>% 
    unique() %>% 
    mutate(TRX = row_number()) %>% 
    left_join(data.frame(team_id = c(match_details$home_id,match_details$away_id)),by=character()) %>% 
    uncount(11) %>% 
    group_by(period,time,team_id) %>% 
    mutate(IDX = row_number()) %>% 
    left_join(all_lineups %>% group_by(period,time,team_id) %>% mutate(IDX=row_number())
              ,by=c('period','time','team_id','IDX')) %>% 
    select(period,time,team_id,TRX,IDX,team_class,ID,full_name,position,side,number,last_name) %>% 
    mutate(last_name = ifelse(is.na(last_name),toupper(full_name),last_name)) %>% 
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
    mutate(time = ifelse(state%in%c('Substitution','Corner','Free Kick'),prev_time+2,time),
           match_time = ifelse(state%in%c('Substitution','Corner','Free Kick'),prev_mt,match_time)) %>% 
    group_by(period) %>% 
    arrange(time) %>% 
    mutate(next_time = lead(time),
           next_state = lead(state)) %>% 
    ungroup() %>% 
    arrange(period,time) %>% 
    subset(state%in%c('Substitution','Kickoff','Corner')|
               action%in%c('SHOT','PENALTY')|
               !is.na(card_given)) %>% 
    select(period,time,match_time,next_time,next_state,possession,state,action,outcome,position,full_name,last_name,oth_position, oth_full_name,oth_role,oth_team,team_id, card_given) %>% 
    mutate(
        live_label = case_when(
            action=='KICKOFF' ~ action,
            action=='SHOT' ~ 'SHOT',
            state=='Substitution' ~ paste0('SUBBED IN for ',oth_position,' ',toupper(oth_full_name),ifelse(oth_role=='injury',' (INJ)','')),
            state=='Corner' ~ 'CORNER',
            action=='PENALTY' ~ paste0('PENALTY - ',oth_position,' ',toupper(oth_full_name),ifelse(oth_role=='foul',' (FOUL)',' (OTH)')),
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
            action=='PENALTY' ~ paste0(case_when(
                outcome=='goal' ~ 'PENALTY GOAL - ',
                outcome=='saved'~ 'PENALTY SAVED - ',
                TRUE ~ 'PENALTY MISSED - ',
            ),toupper(oth_full_name),ifelse(oth_role=='foul',' (FOUL)',' (OTH)')),
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
    subset(!(state=='Corner' & shot>0))

key_moments <- key_moments %>% 
    bind_rows(key_moments %>%
                  drop_na(card_given) %>% 
                  subset(action%in%c('SHOT','PENALTY')) %>% 
                  mutate(time = time - 0.5,
                         live_label = case_when(
                             card_given=='red' ~ 'RED CARD',
                             card_given=='yellow' ~ 'YELLOW CARD',
                             card_given=='yellow-red' ~ 'SECOND YELLOW CARD'
                         ),
                         team_id = ifelse(team_id==match_details$home_id,match_details$away_id,match_details$home_id),
                         post_label = live_label,
                         full_name = oth_full_name,
                         position = oth_position)) %>% 
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
    arrange(period,time) %>% 
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
        next_time = round(case_when(
            next_time - time < 5 ~ next_time,
            state%in%c('Corner','Free Kick','Substition') ~ next_time,
            TRUE ~ time + 3 + 4*runif(n())))
    ) %>% 
    left_join(teams,by='team_id')

stat_times <- all_stats %>% 
    ungroup() %>% select(period,time) %>% 
    unique() %>% mutate(KEY = row_number()%%active_cores + 1) %>% subset(!(period==1 & time==0))

trx_list <- time_base %>% 
    inner_join(match_file %>% subset(state!='Substitution'),by=c('period'='period','secs'='time')) %>% 
    select(period,secs,IDX) %>% 
    mutate(KEY = 1 + row_number()%%active_cores)


time_prog <- match_file %>% 
    subset(period>0) %>% 
    mutate(time = case_when(
        action=='PENALTY' ~ prev_time+1,
        TRUE ~ time)) %>% 
    subset(!state%in%c('Substitution')) %>% 
    mutate(LAB = str_replace(LAB,'\nMOVE','')) %>% 
    ungroup() %>% 
    mutate(X = 233 + (ball_y-40)/40*224,
           Y = 408 + (ball_x-60)/60*294,
           X2= 233 + (prev_y-40)/40*224,
           Y2= 408 + (prev_x-60)/60*294,
           X3= 233 + (prev_y2-40)/40*224,
           Y3= 408 + (prev_x2-60)/60*294,
           X4= 233 + (next_y-40)/40*224,
           Y4= 408 + (next_x-60)/60*294,
           IDX = row_number(),
           KEY = 1 + IDX%%active_cores)
