active_cores <- 7
normal <- 30
slow <- 1

pre_match <- data.frame(base = 'output/layers/title_page.png', REP = 7*normal) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup_A.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup_B.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/league_table_pre.png', REP = 6*normal)) %>% 
    bind_rows(data.frame(base='output/layers/manager_faceoff.png', REP = 6*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup_A.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup_B.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/league_table_pre.png', REP = 6*normal)) %>% 
    bind_rows(data.frame(base='output/layers/manager_faceoff.png', REP = 4*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup_A.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup_B.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/league_table_pre.png', REP = 6*normal)) %>% 
    bind_rows(data.frame(base='output/layers/manager_faceoff.png', REP = 4*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup_A.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/starting_lineup_B.png', REP = 5*normal)) %>% 
    bind_rows(data.frame(base='output/layers/league_table_pre.png', REP = 6*normal)) %>% 
    bind_rows(data.frame(base='output/layers/manager_faceoff.png', REP = 4*normal)) %>% 
    bind_rows(data.frame(base='output/layers/title_page.png', REP = 3*normal))

goals <- match_file %>% subset(state=='Goal') %>% select(period,time) %>% mutate(goal=1) %>% rename(secs=time)
pens <- key_moments %>% subset(state=='PENALTY') %>% select(period,time) %>% mutate(pen=1) %>% rename(secs=time)
high_xg <- match_file %>% 
    mutate(prev_position = lag(position)) %>% 
    subset(!(action%in%c('SHOT','PENALTY') & outcome!='goal' & prev_position=='GK')) %>% 
    drop_na(xg) %>% 
    mutate(xg = ifelse(time>=2700,xg+0.1,xg)) %>% 
    arrange(outcome!='goal',desc(xg)) %>% group_by(period) %>% 
    mutate(IDX = row_number()) %>% ungroup() %>% mutate(RANK = row_number()) %>% subset(IDX<=4|RANK<=8) %>% 
    arrange(IDX,RANK) %>% head(8) %>% select(period,time) %>% arrange(period,time) %>% 
    mutate(shot=1) %>% rename(secs=time)
selected_shots <- high_xg
while(nrow(selected_shots) < min(12,nrow(match_file %>% drop_na(xg)))){
    other_shots <- match_file %>% 
        group_by(period) %>% 
        mutate(PL=max(time)) %>% 
        drop_na(xg) %>% 
        left_join(selected_shots %>% select(period,secs,shot),by=c('period','time'='secs')) %>% 
        mutate(prev_shot = time*shot,
               next_shot = time*shot) %>% 
        fill(prev_shot,.direction='down') %>% 
        fill(next_shot,.direction='up') %>% 
        mutate(
            prev_shot = replace_na(prev_shot,0),
            next_shot = ifelse(is.na(next_shot),PL,next_shot),
            old_delay = next_shot - prev_shot,
               new_delay_prev = time - prev_shot,
               new_delay_next = next_shot - time) %>% 
        mutate(
            old_delay = ifelse(old_delay < 0, old_delay + max(match_file$time[match_file$period==1]),old_delay),
            new_delay_prev = ifelse(new_delay_prev < 0, new_delay_prev + max(match_file$time[match_file$period==1]),new_delay_prev),
            new_delay_next = ifelse(new_delay_next < 0, new_delay_next + max(match_file$time[match_file$period==1]),new_delay_next),
            new_delay = ifelse(new_delay_prev > new_delay_next,new_delay_prev,new_delay_next),
            improvement = old_delay - new_delay
        ) %>% 
        subset(is.na(shot)) %>% 
        arrange(desc(improvement)) %>% 
        head(1)
    selected_shots <- selected_shots %>% 
        bind_rows(other_shots %>% select(period,secs=time) %>% mutate(shot=1)) %>% 
        arrange(period,secs)
}

kickoffs <- match_file %>% subset(state=='Kickoff') %>% select(period,time) %>% mutate(kickoff=1) %>% rename(secs=time)
corners <- match_file %>% subset(state=='Corner') %>% select(period,time) %>% mutate(corner=1) %>% rename(secs=time)
subs <- lineup_times %>% ungroup() %>% select(period,time) %>% unique() %>% mutate(time = round(time), sub=1) %>% rename(secs=time) %>% slice(-1)
injs <- key_moments %>% subset(oth_role=='injury') %>% select(period,time,next_time) %>% mutate(inj=1,time = round(time), next_time = round(next_time)) %>% rename(secs=time,next_inj=next_time)
reds <- key_moments %>% drop_na(card_given) %>% subset(str_detect(card_given,'red')) %>% mutate(red=1) %>% rename(secs=time) %>% select(period,secs,red)
trxs <- trx_list %>% select(period,secs,possession) %>% mutate(trxx=1)
crests <- trx_frames %>% select(period,timestamp,crest) %>% rename(secs=timestamp) %>% mutate(crest=paste0('crest_',crest,'.png'))

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
    left_join(reds,by=c('period','secs')) %>% 
    left_join(selected_shots,by=c('period','secs')) %>% 
    left_join(kickoffs,by=c('period','secs')) %>% 
    left_join(corners,by=c('period','secs')) %>% 
    left_join(subs,by=c('period','secs')) %>% 
    left_join(injs,by=c('period','secs')) %>% 
    left_join(trxs,by=c('period','secs')) %>% 
    mutate(next_goal = ifelse(goal==1,secs,NA),
           prev_goal = ifelse(goal==1,secs,NA),
           next_pen = ifelse(pen==1,secs,NA),
           prev_pen = ifelse(pen==1,secs,NA),
           prev_red = ifelse(red==1,secs,NA),
           next_shot = ifelse(shot==1,secs,NA),
           prev_shot = ifelse(shot==1,secs,NA),
           prev_kickoff = ifelse(kickoff==1,secs,NA),
           next_kickoff = ifelse(kickoff==1,secs,NA),
           next_corner = ifelse(corner==1,secs,NA),
           next_sub = ifelse(sub==1,secs,NA),
           prev_sub = ifelse(sub==1,secs,NA),
           prev_inj = ifelse(inj==1,secs,NA),
           prev_inj_end = ifelse(inj==1,next_inj,NA),
           next_trx = ifelse(trxx==1,secs,NA)) %>% 
    group_by(period) %>% 
    fill(c(next_goal,next_pen, next_shot, next_kickoff, prev_red, next_corner, next_trx, next_sub), .direction='up') %>% 
    fill(c(prev_goal,prev_pen, prev_shot, prev_kickoff, prev_sub, prev_inj, prev_inj_end, crest), .direction='down') %>% 
    mutate(
        match_state = case_when(
            secs>=prev_inj & secs<=prev_inj_end ~ 'injury',
            secs==prev_pen ~ 'overlay',
            secs==prev_red ~ 'overlay',
            secs<next_pen & next_pen - secs <= 12 & is.na(next_corner) ~ 'build_up',
            secs<next_pen & next_pen - secs <= 12 & (next_corner - secs > 12|next_corner - secs <= 3) ~ 'build_up',
            secs>=prev_pen & secs-prev_pen <= 6 ~ 'reaction',
            secs>=prev_pen & secs<next_goal & next_goal-prev_pen < 75 ~ 'trx',
            secs==prev_goal ~ 'overlay',
            secs<=prev_goal+11 ~ 'overlay',
            secs>=prev_kickoff & secs - prev_kickoff <= 10 ~ 'kickoff',
            secs<next_goal & next_goal - secs <= 12 & is.na(next_corner) ~ 'build_up',
            secs<next_shot & next_shot - secs <= 12 & is.na(next_corner) ~ 'build_up',
            secs<next_goal & next_goal - secs <= 12 & (next_corner - secs > 12|next_corner - secs <= 3) ~ 'build_up',
            secs<next_shot & next_shot - secs <= 12 & (next_corner - secs > 12|next_corner - secs <= 3) ~ 'build_up',
            secs> prev_goal & next_kickoff - secs <= 60 & secs - prev_goal > 11 ~ NA_character_,
            secs>=prev_shot & secs - prev_shot <= 6 ~ 'reaction',
            secs==2700 ~ 'overlay'
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
        paste(period,secs)%in%with(reds,paste(period,secs)) ~ 5*normal,
        paste(period,secs)%in%with(pens,paste(period,secs)) ~ 5*normal,
        match_state=='overlay' & state%in%c('Goal_1','Goal_2') ~ 0.3*normal,
        match_state=='overlay' & state=='Goal' ~ 5*normal,
        match_state=='overlay' & state=='Goal Text' ~ 15*normal,
        secs==next_sub & oth_role=='injury' ~ 5*normal,
        secs==next_sub ~ 3*normal,
        secs==0 ~ 4*normal,
        match_state=='overlay' ~ 3*normal,
        match_state=='injury' ~ normal / slow,
        match_state%in%c('kickoff','build_up','reaction') ~ normal / slow,
        TRUE ~ 1
    ))



frame_index <- pre_match %>% 
    bind_rows(frame_index %>% subset(period==1)) %>% 
    bind_rows(frame_index %>% subset(period==1) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/halftime_stats.png', REP = 12*normal)) %>% 
    bind_rows(frame_index %>% subset(period==1) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/halftime_ratings.png', REP = 8*normal)) %>%
    bind_rows(frame_index %>% subset(period==1) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/halftime_player.png', REP = 6*normal)) %>%
    bind_rows(frame_index %>% subset(period==1) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/halftime_stats.png', REP = 8*normal)) %>% 
    bind_rows(frame_index %>% subset(period==2)) %>% 
    bind_rows(frame_index %>% subset(period==2) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/fulltime_stats.png', REP = 15*normal)) %>% 
    bind_rows(frame_index %>% subset(period==2) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/fulltime_ratings.png', REP = 10*normal)) %>% 
    bind_rows(frame_index %>% subset(period==2) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/fulltime_player.png', REP = 10*normal)) %>% 
    bind_rows(frame_index %>% subset(period==2) %>% tail(1) %>% mutate(match_state = 'overlay', overlay='output/layers/league_table_post.png', REP = 25*normal)) %>% 
    group_by(period) %>% 
    fill(c(filename, minute, key, trx),.direction='down') %>%
    mutate(trx = ifelse(match_state%in%c('show_lineup','injury'),NA_character_,trx)) %>% 
    ungroup() %>% 
    mutate(IDX = row_number()) %>% 
    mutate(KEY = 1 + IDX%%active_cores)

frame_index %>% 
    toJSON(pretty = TRUE) %>%
    write('output/frame_index.json')

preview_index <- frame_index %>% 
    drop_na(match_state) %>% 
    subset(!match_state%in%c('show_lineup','overlay')) %>% 
    mutate(prev_secs = lag(secs),
           GROUP = cumsum(is.na(prev_secs)|secs != prev_secs+1))

for(i in unique(preview_index$GROUP)){
    temp_preview <- subset(preview_index,GROUP==i)
    
    temp_output <- temp_preview %>% 
        select(period,secs) %>% 
        inner_join(time_prog,by=c('period'='period','secs'='time')) %>% 
        mutate(RN = row_number()) %>% 
        ggplot() +
        theme_void() +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        background_image(readPNG('output/layers/01/Match.png')) +
        geom_segment(mapping = aes(x=X,y=Y,xend=X2,yend=Y2,colour=factor(team_id)),linewidth=0.4) +
        geom_point(mapping = aes(x=X,y=Y,fill=factor(team_id),colour=short_name),size=3,pch=21) +
        geom_text(mapping = aes(x=X,y=Y,label=number,colour=short_name),
                  family='Montserrat-Medium',hjust=0.5,vjust=0.5,size=3.5) +
        geom_text(mapping = aes(x=X,y=Y+20,label=RN),
                  family='Montserrat-Medium',hjust=0.5,vjust=0.5,size=2.5,colour='black') +
        scale_fill_manual(values = team_colours,guide='none') + 
        scale_colour_manual(values = c(team_colours,text_colours[1:2]),guide='none')
    dir.create('output/preview',showWarnings=FALSE)
    ggsave(paste0('output/preview/',min(temp_preview$period),'_',str_pad(min(temp_preview$secs),4,pad='0'),'.png'),
           temp_output,
           height=1080,width=1920,
           units='px',dpi=300)
    
}
