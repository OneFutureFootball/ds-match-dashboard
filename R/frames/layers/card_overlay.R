card_overlay <- function(pers,secs){
    cards <- key_moments %>% 
        drop_na(card_given) %>% 
        subset(period<=pers) %>% 
        subset(!(period==pers & time>secs)) %>% 
        mutate(ID = oth_ID)
    
    reds <- cards %>% subset(str_detect(card_given,'red'))
    
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
        ) %>% 
        inner_join(cards %>% select(ID,card_given),by='ID')
    
    plot_output <- ggplot() + 
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) + 
        theme_void() +
        geom_point(lineup,
                   mapping = aes(x=X+25,
                                 y=Y-25),
                   pch=19,size=3,colour='white') +
        geom_image(lineup,
                   mapping = aes(x=X+25,
                                 y=Y-25,
                                 image='images/icons/cardyellow.png'),
                   size=0.016) +
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