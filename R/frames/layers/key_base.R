key_base <- function(idx,live=TRUE){
    key_prog <- key_moments %>% 
        subset(IDX<=idx) %>%
        subset(!state%in%c('Goal_1','Goal_2','Goal Text')) %>%
        #Undo the shift of 10 seconds to the first goal overlay
        mutate(time = ifelse(state=='Goal',time-10,time),
               MXT = max(time[period==max(period)]),
               MPR = max(period)) %>% 
        rowwise() %>% 
        mutate(
            full_name = ifelse(live & IDX==idx & state=='Substitution' & oth_role=='injury',oth_full_name,full_name),
            position = ifelse(live & IDX==idx & state=='Substitution' & oth_role=='injury',oth_position,position),
            LABEL = case_when(
                IDX==idx ~ ifelse(live,live_label,post_label),
                TRUE ~ post_label),
            CODE = case_when(
                str_detect(LABEL,'GOAL') ~ 'GOAL',
                state=='Substitution' & period==MPR & time==MXT ~ 'DARK',
                TRUE ~ LABEL)) %>% 
        ungroup() %>% 
        mutate(GIDX = cumsum(ifelse(state=='Goal',0,1)),
               X = 0,
               Y = 879 - 46*(max(GIDX) + 1 - GIDX)) %>% 
        mutate(logo = ifelse(state=='Substitution',paste0('images/icons/subon',ifelse(oth_role=='injury','injury',''),'.png'),NA_character_))
    
    reds <- key_moments %>% 
        subset(str_detect(live_label,'RED|SECOND'))
    
    this_moment <- subset(key_prog,IDX==idx)
    if(nrow(this_moment)==0) return(NULL)
    topup <- key_moments %>% subset(period==this_moment$period & time==this_moment$time) %>% summarise(TOPUP = sum(oth_role%in%c('red card','injury'))) %>% pull(TOPUP)
    
    if(!is.na(this_moment$card_given)) card_overlay(this_moment$period,this_moment$time)
    if(this_moment$state=='Substitution') card_overlay(this_moment$period,this_moment$time)
    
    key_prog <- key_prog %>% 
        subset(state!='Goal') %>% 
        tail(11)
    
    if(live){
        goals <- key_moments %>% 
            subset(outcome=='goal') %>%
            subset(!str_detect(live_label,'CARD|CONCED')) %>% 
            subset(IDX<idx)
    }else{
        goals <- key_moments %>% 
            subset(outcome=='goal') %>%
            subset(!str_detect(live_label,'CARD|CONCED')) %>% 
            subset(IDX<=idx)
        
    }
    goals <- goals %>% 
        group_by(team_id) %>% 
        mutate(IDX = row_number(),
               N = n()) %>%
        ungroup() %>% 
        mutate(XT = 960 + ifelse(possession=='A',-20,20),
               XP = 960 + ifelse(possession=='A',-70,70),
               Y = 982 - ifelse(max(N),-3,0) - (IDX-1)*ifelse(max(N)>=5,18,23))
    
    pers <- key_prog %>% tail(1) %>% pull(period)
    mins <- key_prog %>% tail(1) %>% mutate(MIN = floor(ifelse(live,time,next_time)/60)) %>% pull(MIN)
    
    agg_goals <- data.frame(team_id=c(8,4),X=960 - 140*c(-1,1),Y=c(1090,1090),display=c(4,3))
    plot_output <- ggplot() +
        # background_image(readPNG('output/layers/01/Match.png')) +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        theme_void() +
        scale_alpha_continuous(range = c(0.7,1),guide='none') +
        geom_text(all_stats %>% ungroup() %>% subset(statistic=='GOALS') %>% select(team_id,X,Y) %>% 
                      unique() %>% left_join(goals %>% group_by(team_id) %>% summarise(value=n()),by='team_id') %>% 
                      mutate(display=replace_na(value,0)),
                  mapping = aes(x=X,y=Y,label=display),
                  hjust = 0.5, vjust=0.5, family='Montserrat-ExtraBold', size=16, colour='white') +
        # geom_text(agg_goals,
        #           mapping = aes(x=X,y=Y,label=paste0('(',display,')')),
        #           hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=7, colour='white') +
        geom_text(mapping = aes(x=960, y=1030, label='–'),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Black', size=8, colour='white') +
        geom_rect(
            key_prog %>% subset(period==this_moment$period & time==this_moment$time & this_moment$state=='Substitution'),
            mapping = aes(xmin = X - 10, xmax = X + 310,
                          ymin = Y - 23, ymax = Y + 23),
            fill='white',colour=NA
        ) +
        geom_image(key_prog %>% subset(str_detect(crest,this_match$home_name)),
                   mapping = aes(x=X-27,y=Y,image=str_replace(crest,'-256','-60'), alpha=IDX), 
                   size=0.024) +
        geom_image(key_prog %>% subset(str_detect(crest,this_match$away_name)),
                   mapping = aes(x=X-27,y=Y,image=str_replace(crest,'-256','-white-60'), alpha=IDX), 
                   size=0.030) +
        geom_text(key_prog, mapping = aes(x=X+40,y=Y, label=MIN, alpha=IDX, colour=CODE),
                  hjust = 1, vjust=0.5, family='Montserrat-ExtraBold', size=4) +
        geom_text(key_prog, mapping = aes(x=X+50,y=Y+10, label=toupper(full_name), alpha=IDX, colour=CODE),
                  hjust = 0, vjust=0.5, family='Montserrat-Bold', size=4) +
        geom_text(key_prog, mapping = aes(x=X+50,y=Y-10, label=LABEL, alpha=IDX, colour=CODE),
                  hjust = 0, vjust=0.5, family='Montserrat-Regular', size=4) +
        geom_text(goals %>% subset(possession=='A'),mapping = aes(x=XT,y=Y,label=MIN),
                  hjust = 1, vjust=0.5, family='Montserrat-Bold', size=4, colour='white') +
        geom_text(goals %>% subset(possession=='A'),mapping = aes(x=XP,y=Y,label=toupper(last_name)),
                  hjust = 1, vjust=0.5, family='Montserrat-Medium', size=4, colour='white') +
        geom_text(goals %>% subset(possession=='B'),mapping = aes(x=XT,y=Y,label=MIN),
                  hjust = 0, vjust=0.5, family='Montserrat-Bold', size=4, colour='white') +
        geom_text(goals %>% subset(possession=='B'),mapping = aes(x=XP,y=Y,label=toupper(last_name)),
                  hjust = 0, vjust=0.5, family='Montserrat-Medium', size=4, colour='white') +
        geom_image(reds,
                   mapping = aes(x=960 + 90*ifelse(oth_team=='A',-1,1),
                                 y=1010,
                                 image='images/icons/cardred.png'),
                   size=0.016) +
        geom_image(key_prog %>% drop_na(logo),
                   mapping = aes(x=X+290,y=Y,image=logo, alpha=IDX),
                   size=0.025) +
        scale_colour_manual(values = c(text_colours, 'GOAL'='#5CED73', 'YELLOW CARD'='#FFF380', 'RED CARD'='#FF6955','SECOND YELLOW CARD'='#FF6955', 'DARK'='#150928'), na.value='white',guide='none')
    
    if(!(topup>0 & !this_moment$oth_role%in%c('red card','injury') & live)){
        
        ggsave(paste0('output/layers/03/Key_',pers,'_',str_pad(ifelse(live,this_moment$time,this_moment$next_time),4,pad='0'),'.png'),
               plot_output,
               height=1080,
               width=1920,
               units='px',
               dpi=300)
    }
    if(replace_na(this_moment$oth_role,'')=='injury') card_overlay(this_moment$period,ifelse(live,this_moment$time,this_moment$next_time))
    if(is.na(this_moment$post_label)) return(NULL)
    if(this_moment$post_label == this_moment$live_label & topup==0) return(NULL)
    if(live) key_base(idx,FALSE)
}