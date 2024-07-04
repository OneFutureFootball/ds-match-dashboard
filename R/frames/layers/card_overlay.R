card_overlay <- function(pers,secs){
    cards <- key_moments %>% 
        drop_na(card_given) %>% 
        subset(period<=pers) %>% 
        subset(!(period==pers & time>secs)) %>% 
        mutate(ID = oth_ID)
    
    reds <- cards %>% subset(str_detect(card_given,'red'))
    yels <- cards %>% subset(!str_detect(card_given,'red'))
    
    injs <- key_moments %>% 
        subset(oth_role=='injury') %>% 
        subset(period==pers) %>% 
        subset(time <= secs & next_time > secs)
    
    redc <- key_moments %>%
        subset(state=='Substitution') %>% 
        subset(!str_detect(oth_role,'balance|tactical')) %>% 
        subset(str_detect(oth_role,'red')) %>% 
        subset(period==pers) %>% 
        subset(time <= secs & next_time > secs)
    
    lineup <- lineup_times %>%
        subset(period<=pers) %>% 
        subset(!(period==pers & time>secs)) %>% 
        group_by(team_id,IDX) %>% 
        slice(n()) %>% 
        lineup_location() %>%
        mutate(
            Y = (Y-50)*1.25 + 50,
            X = ifelse(team_class=='B',(100-X/2),X/2)*120/100,
            Y = ifelse(team_class=='B',Y,(100-Y))*80/100,
            X = sapply(X,function(x) pitch_transform(x,'X')),
            Y = sapply(Y,function(x) pitch_transform(x,'Y')),
        )
    
    prev_lineup <- lineup_times %>%
        subset(period<=pers) %>% 
        subset(!(period==pers & time>=secs)) %>% 
        group_by(team_id,IDX) %>% 
        slice(n()) %>% 
        lineup_location() %>%
        mutate(
            Y = (Y-50)*1.25 + 50,
            X = ifelse(team_class=='B',(100-X/2),X/2)*120/100,
            Y = ifelse(team_class=='B',Y,(100-Y))*80/100,
            X = sapply(X,function(x) pitch_transform(x,'X')),
            Y = sapply(Y,function(x) pitch_transform(x,'Y')),
        )
    
    yels <- lineup %>% 
        inner_join(yels %>% select(ID,card_given),by='ID')
    
    injs <- lineup %>% 
        inner_join(injs %>% select(ID=oth_ID,oth_role),by='ID')
    
    if(secs>0|pers>1){
        subs <- lineup_times %>%
        subset(period<=pers) %>% 
        subset(!(period==pers & time>=secs)) %>% 
        group_by(team_id,IDX) %>% 
        slice(n()) %>% 
        lineup_location() %>%
        mutate(
            Y = (Y-50)*1.25 + 50,
            X = ifelse(team_class=='B',(100-X/2),X/2)*120/100,
            Y = ifelse(team_class=='B',Y,(100-Y))*80/100,
            X = sapply(X,function(x) pitch_transform(x,'X')),
            Y = sapply(Y,function(x) pitch_transform(x,'Y')),
        ) %>% 
        inner_join(key_moments %>%
                       subset(state=='Substitution') %>% 
                       subset(str_detect(oth_role,'balance|tactical')) %>% 
                       subset(period==pers) %>% 
                       subset(time <= secs & next_time > secs) %>% select(ID=oth_ID,oth_role),by='ID')
    }else{
        subs <- NULL
    }
    redc <- lineup %>% 
        inner_join(redc %>% select(ID=oth_ID,card_given),by='ID')
    
    dots <- yels %>% 
        bind_rows(injs) %>% 
        bind_rows(subs)
    
    #if(dots %>% bind_rows(reds) %>% nrow() == 0) return(NULL)
    
    plot_output <- ggplot() + 
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) + 
        theme_void()
    
    if(dots %>% bind_rows(reds) %>% nrow() > 0) plot_output <- plot_output +
        geom_point(dots,
                   mapping = aes(x=X+25*ifelse(is.na(card_given),-1,1),
                                 y=Y-25),
                   pch=19,size=3,colour='white') +
        geom_image(yels,
                   mapping = aes(x=X+25,
                                 y=Y-25,
                                 image='images/icons/cardyellow.png'),
                   size=0.016) +
        geom_image(injs,
                   mapping = aes(x=X-25,
                                 y=Y-25,
                                 image='images/icons/suboninjury.png'),
                   size=0.022) +
        geom_image(subs,
                   mapping = aes(x=X-25,
                                 y=Y-25,
                                 image='images/icons/subon.png'),
                   size=0.022) +
        geom_image(reds,
                   mapping = aes(x=960 + 90*ifelse(oth_team=='A',-1,1),
                                 y=1010,
                                 image='images/icons/cardred.png'),
                   size=0.016)
    
    ggsave(paste0('output/layers/08/Card_',pers,'_',str_pad(round(secs),4,pad='0'),'.png'),
           plot_output,
           height=1080, width=1920, 
           dpi=300, units='px')
}