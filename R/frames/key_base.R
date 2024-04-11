key_base <- function(idx,live=TRUE){
    key_prog <- key_moments %>% 
        subset(IDX<=idx) %>%
        subset(!state%in%c('Goal_1','Goal_2','Goal Text')) %>%
        mutate(time = ifelse(state=='Goal',time-10,time)) %>% 
        rowwise() %>% 
        mutate(
            LABEL = case_when(
                IDX==idx ~ ifelse(live,live_label,post_label),
                TRUE ~ post_label),
            CODE = ifelse(str_detect(LABEL,'GOAL'),'GOAL',LABEL)) %>% 
        ungroup() %>% 
        mutate(GIDX = cumsum(ifelse(state=='Goal',0,1)),
               X = 680,
               Y = 868 - 60*(max(GIDX) + 1 - GIDX)) %>% 
        mutate(logo = ifelse(state=='Substitution',paste0('images/subon',ifelse(oth_role=='injured','injury',''),'.png'),NA_character_))
    
    this_moment <- subset(key_prog,IDX==idx)
    if(nrow(this_moment)==0) return(NULL)
    
    key_prog <- key_prog %>% 
        subset(state!='Goal') %>% 
        tail(6)
    
    if(live){
        goals <- key_moments %>% 
            subset(outcome=='goal') %>%
            subset(!str_detect(live_label,'CARD')) %>% 
            subset(IDX<idx) %>% 
            group_by(team_id) %>% 
            mutate(IDX = row_number()) %>%
            mutate(XT = 960 + ifelse(possession=='A',-20,20),
                   XP = 960 + ifelse(possession=='A',-70,70),
                   Y = 975 - IDX*25)
    }else{
        goals <- key_moments %>% 
            subset(outcome=='goal') %>%
            subset(!str_detect(live_label,'CARD')) %>% 
            subset(IDX<=idx) %>% 
            group_by(team_id) %>% 
            mutate(IDX = row_number()) %>%
            mutate(XT = 960 + ifelse(possession=='A',-20,20),
                   XP = 960 + ifelse(possession=='A',-70,70),
                   Y = 975 - IDX*25)
        
    }
    pers <- key_prog %>% tail(1) %>% pull(period)
    mins <- key_prog %>% tail(1) %>% mutate(MIN = floor(ifelse(live,time,next_time)/60)) %>% pull(MIN)
    plot_output <- ggplot() +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        theme_void() +
        scale_alpha_continuous(range = c(0.6,1),guide='none') +
        #background_image(readPNG(paste0('output/layers/01/Match.png'))) +
        geom_text(all_stats %>% ungroup() %>% subset(statistic=='GOALS') %>% select(team_id,X,Y) %>% unique() %>% left_join(goals %>% group_by(team_id) %>% summarise(value=n()),by='team_id') %>% mutate(display=replace_na(value,0)),
                  mapping = aes(x=X,y=Y,label=display),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Medium', size=14, colour='white') +
        geom_text(mapping = aes(x=960, y=1000, label='—'),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=4, colour='white') +
        geom_image(key_prog,mapping = aes(x=X-30,y=Y,image=crest), size=0.04) +
        geom_text(key_prog, mapping = aes(x=X+40,y=Y+10, label=MIN, alpha=IDX, colour=CODE),
                  hjust = 0, vjust=0.5, family='Montserrat-ExtraBold', size=5) +
        geom_text(key_prog, mapping = aes(x=X+140,y=Y+10, label=toupper(position), alpha=IDX, colour=CODE),
                  hjust = 1, vjust=0.5, family='Montserrat-Medium', size=4) +
        geom_text(key_prog, mapping = aes(x=X+150,y=Y+10, label=toupper(full_name), alpha=IDX, colour=CODE),
                  hjust = 0, vjust=0.5, family='Montserrat-Medium', size=6) +
        geom_text(key_prog, mapping = aes(x=X+150,y=Y-15, label=LABEL, alpha=IDX, colour=CODE),
                  hjust = 0, vjust=0.5, family='Montserrat-Medium', size=4) +
        geom_text(goals %>% subset(possession=='A'),mapping = aes(x=XT,y=Y,label=MIN),
                  hjust = 1, vjust=0.5, family='Montserrat-Bold', size=4, colour='white') +
        geom_text(goals %>% subset(possession=='A'),mapping = aes(x=XP,y=Y,label=toupper(last_name)),
                  hjust = 1, vjust=0.5, family='Montserrat-Medium', size=4, colour='white') +
        geom_text(goals %>% subset(possession=='B'),mapping = aes(x=XT,y=Y,label=MIN),
                  hjust = 0, vjust=0.5, family='Montserrat-Bold', size=4, colour='white') +
        geom_text(goals %>% subset(possession=='B'),mapping = aes(x=XP,y=Y,label=toupper(last_name)),
                  hjust = 0, vjust=0.5, family='Montserrat-Medium', size=4, colour='white') +
        geom_image(key_prog %>% drop_na(logo),
                   mapping = aes(x=X+600,y=Y,image=logo, alpha=IDX),
                   size=0.03) +
        scale_colour_manual(values = c(text_colours, 'GOAL'='#5CED73', 'YELLOW CARD'='#FFF380', 'RED CARD'='#FF6955','SECOND YELLOW CARD'='#FF6955'), na.value='white',guide='none') + 
        scale_fill_manual(values = team_colours,guide='none')
    
    ggsave(paste0('output/layers/03/Key_',pers,'_',str_pad(ifelse(live,this_moment$time,this_moment$next_time),4,pad='0'),'.png'),
           plot_output,
           height=1080,
           width=1920,
           units='px',
           dpi=300)
    
    if(is.na(this_moment$post_label)) return(NULL)
    if(this_moment$post_label == this_moment$live_label) return(NULL)
    if(live) key_base(idx,FALSE)
}