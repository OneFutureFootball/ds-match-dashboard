active_cores <- 8
normal <- 30
slow <- 1

pre_match <- data.frame(base = 'output/layers/title_page.png', REP = 7*normal) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup.png', REP = 10*normal)) %>% 
    bind_rows(data.frame(base='output/layers/league_table_pre.png', REP = 10*normal)) %>% 
    bind_rows(data.frame(base='output/layers/manager_faceoff.png', REP = 6*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup.png', REP = 8*normal)) %>% 
    bind_rows(data.frame(base='output/layers/league_table_pre.png', REP = 8*normal)) %>% 
    bind_rows(data.frame(base='output/layers/manager_faceoff.png', REP = 4*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup.png', REP = 8*normal)) %>% 
    bind_rows(data.frame(base='output/layers/league_table_pre.png', REP = 8*normal)) %>% 
    bind_rows(data.frame(base='output/layers/manager_faceoff.png', REP = 4*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup.png', REP = 8*normal)) %>% 
    bind_rows(data.frame(base='output/layers/league_table_pre.png', REP = 8*normal)) %>% 
    bind_rows(data.frame(base='output/layers/manager_faceoff.png', REP = 4*normal)) %>% 
    bind_rows(data.frame(base='output/layers/title_page.png', REP = 3*normal))

goals <- match_file %>% subset(state=='Goal') %>% select(period,time) %>% mutate(goal=1) %>% rename(secs=time)
pens <- match_file %>% subset(next_action=='PENALTY') %>% select(period,time) %>% mutate(pen=1) %>% rename(secs=time)
high_xg <- match_file %>% 
    subset(!(action%in%c('SHOT','PENALTY') & outcome!='goal' & lag(position)=='GK')) %>% 
    drop_na(xg) %>% arrange(desc(xg)) %>% group_by(period) %>% 
    mutate(IDX = row_number()) %>% ungroup() %>% mutate(RANK = row_number()) %>% subset(IDX<=4|RANK<=10) %>% 
    arrange(IDX,RANK) %>% head(10) %>% select(period,time) %>% arrange(period,time) %>% 
    mutate(shot=1) %>% rename(secs=time)
kickoffs <- match_file %>% subset(state=='Kickoff') %>% select(period,time) %>% mutate(kickoff=1) %>% rename(secs=time)
subs <- lineup_times %>% ungroup() %>% select(period,time) %>% unique() %>% mutate(time = round(time), sub=1) %>% rename(secs=time) %>% slice(-1)
injs <- key_moments %>% subset(oth_role=='injury') %>% select(period,time,next_time) %>% mutate(inj=1,time = round(time), next_time = round(next_time)) %>% rename(secs=time,next_inj=next_time)
trxs <- trx_list %>% select(period,secs,possession) %>% mutate(trxx=1)
crests <- trx_frames %>% select(period,timestamp,crest) %>% rename(secs=timestamp) %>% mutate(crest=paste0('crest_',crest,'.png'))
reds <- match_file %>% drop_na(card_given) %>% subset(str_detect(card_given,'red'))

frame_index <- time_base %>% 
    arrange(period,secs) %>% 
    mutate(filename = paste0('_',period,'_',str_pad(secs,4,pad='0'),'.png'),
           base = 'output/layers/01/Match.png') %>% 
    left_join(data.frame(minute = list.files('output/layers/02')) %>% mutate(filename=str_replace(minute,'Minute','')),by='filename') %>% 
    left_join(data.frame(key = list.files('output/layers/03')) %>% mutate(filename=str_replace(key,'Key','')),by='filename') %>% 
    left_join(data.frame(trx = list.files('output/layers/04')) %>% mutate(filename=str_replace(trx,'Trx','')),by='filename') %>% 
    left_join(data.frame(lineup = list.files('output/layers/05')) %>% mutate(filename=str_replace(lineup,'Lineup','')),by='filename') %>% 
    left_join(data.frame(card = list.files('output/layers/08')) %>% mutate(filename=str_replace(card,'Card','')),by='filename') %>% 
    left_join(data.frame(text = list.files('output/layers/09')) %>% mutate(filename=str_replace(text,'Text','')),by='filename') %>% 
    left_join(crests,by=c('period','secs')) %>% 
    fill(c(minute,key,trx,text,lineup,card),.direction='down') %>% 
    left_join(goals,by=c('period','secs')) %>% 
    left_join(pens,by=c('period','secs')) %>% 
    left_join(high_xg,by=c('period','secs')) %>% 
    left_join(kickoffs,by=c('period','secs')) %>% 
    left_join(subs,by=c('period','secs')) %>% 
    left_join(injs,by=c('period','secs')) %>% 
    left_join(trxs,by=c('period','secs')) %>% 
    mutate(next_goal = ifelse(goal==1,secs,NA),
           prev_goal = ifelse(goal==1,secs,NA),
           next_pen = ifelse(pen==1,secs,NA),
           prev_pen = ifelse(pen==1,secs,NA),
           next_shot = ifelse(shot==1,secs,NA),
           prev_shot = ifelse(shot==1,secs,NA),
           prev_kickoff = ifelse(kickoff==1,secs,NA),
           next_kickoff = ifelse(kickoff==1,secs,NA),
           next_sub = ifelse(sub==1,secs,NA),
           prev_sub = ifelse(sub==1,secs,NA),
           prev_inj = ifelse(inj==1,secs,NA),
           prev_inj_end = ifelse(inj==1,next_inj,NA),
           next_trx = ifelse(trxx==1,secs,NA)) %>% 
    group_by(period) %>% 
    fill(c(next_goal,next_pen, next_shot, next_kickoff, next_trx, next_sub), .direction='up') %>% 
    fill(c(prev_goal,prev_pen, prev_shot, prev_kickoff, prev_sub, prev_inj, prev_inj_end, crest), .direction='down') %>% 
    mutate(
        match_state = case_when(
            secs>=prev_inj & secs<=prev_inj_end ~ 'injury',
            secs<next_pen & next_pen - secs <= 12 ~ 'build_up',
            secs>=prev_pen & secs-prev_pen <= 6 ~ 'reaction',
            secs>=prev_pen & secs<next_goal & next_goal-prev_pen < 75 ~ 'trx',
            secs==prev_goal ~ 'overlay',
            secs<=prev_goal+11 ~ 'overlay',
            secs>=prev_kickoff & secs - prev_kickoff <= 10 ~ 'kickoff',
            secs<next_goal & next_goal - secs <= 12 ~ 'build_up',
            secs<next_shot & next_shot - secs <= 12 ~ 'build_up',
            secs> prev_goal & next_kickoff - secs <= 60 & secs - prev_goal > 11 ~ NA_character_,
            secs>=prev_shot & secs - prev_shot <= 6 ~ 'reaction',
        ),
        delay = next_trx - secs,
        overlay = paste0('output/layers/07/Overlay_',period,'_',str_pad(secs,4,pad='0'),'.png'),
        overlay = ifelse(file.exists(overlay),overlay,NA_character_)
    )

frame_index <- frame_index %>% 
    mutate(prev_state = ifelse(is.na(match_state),NA,secs),
           next_state = ifelse(is.na(match_state),NA,secs)) %>% 
    group_by(period) %>% 
    fill(c(prev_state,overlay),.direction='down') %>% 
    fill(c(next_state),.direction='up') %>% 
    mutate(overlay = ifelse(match_state=='overlay',overlay,NA_character_),
           elapsed = secs - prev_state,
           remaining = next_state - secs,
           match_state = replace_na(match_state,'show_lineup'),
           lineup = ifelse(!match_state%in%c('show_lineup','injury'),NA_character_,lineup),
           card = ifelse(!match_state%in%c('show_lineup','injury'),NA_character_,card),
           trx = ifelse(match_state%in%c('show_lineup','injury'),NA_character_,trx)
    ) %>% 
    left_join(key_moments %>% ungroup() %>% select(period,secs=time,state) %>% unique(),by=c('period','secs')) %>% 
    ungroup() %>% 
    left_join(match_file %>% select(period,secs=time,oth_role),by=c('period','secs')) %>% 
    mutate(REP = case_when(
        paste(period,secs)%in%with(reds,paste(period,time)) ~ 5*normal,
        match_state=='overlay' & state%in%c('Goal_1','Goal_2') ~ 0.4*normal,
        match_state=='overlay' & state=='Goal' ~ 8*normal,
        match_state=='overlay' & state=='Goal Text' ~ 12*normal,
        secs==next_sub & oth_role=='injury' ~ 5*normal,
        secs==next_sub ~ 2*normal,
        secs==0 ~ 5*normal,
        match_state=='injury' ~ normal / slow,
        match_state%in%c('kickoff','build_up','reaction') ~ normal / slow,
        TRUE ~ 1
    ))



frame_index <- pre_match %>% 
    bind_rows(frame_index %>% subset(period==1)) %>% 
    bind_rows(frame_index %>% subset(period==1) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/halftime_stats.png', REP = 30*normal)) %>% 
    # bind_rows(frame_index %>% subset(period==1) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/halftime_ratings.png', REP = 8*normal)) %>% 
    # bind_rows(frame_index %>% subset(period==1) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/halftime_player.png', REP = 6*normal)) %>% 
    bind_rows(frame_index %>% subset(period==2)) %>% 
    bind_rows(frame_index %>% subset(period==2) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/fulltime_stats.png', REP = 15*normal)) %>% 
    bind_rows(frame_index %>% subset(period==2) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/fulltime_ratings.png', REP = 10*normal)) %>% 
    bind_rows(frame_index %>% subset(period==2) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/fulltime_player.png', REP = 10*normal)) %>% 
    bind_rows(frame_index %>% subset(period==2) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/league_table_post.png', REP = 25*normal)) %>% 
    group_by(period) %>% 
    fill(c(filename, minute, key, trx),.direction='down') %>%
    ungroup() %>% 
    mutate(IDX = row_number()) %>% 
    mutate(KEY = 1 + IDX%%active_cores)

# frame_index <- frame_index %>%
#     mutate(
#         match_state = case_when(
#             IDX%in%5431:5432 ~ 'show_lineup',
#             TRUE ~ match_state
#         ),
#         REP = case_when(
#             IDX%in%5431:5432 ~ 1,
#             TRUE ~ REP
#         )
#     ) %>%
#     mutate(new_lineup = lineup) %>%
#     group_by(period) %>%
#     fill(new_lineup,.direction = 'down') %>%
#     mutate(lineup = ifelse(match_state=='show_lineup' & is.na(lineup),new_lineup,lineup)) %>%
#     select(-new_lineup)

frame_index %>% 
    toJSON(pretty = TRUE) %>%
    write('output/frame_index.json')

preview_index <- frame_index %>% 
    drop_na(match_state) %>% 
    subset(!match_state%in%c('show_lineup','overlay')) %>% 
    mutate(prev_secs = lag(secs),
           GROUP = cumsum(is.na(prev_secs)|secs != prev_secs+1))

# for(i in unique(preview_index$GROUP)){
# }
