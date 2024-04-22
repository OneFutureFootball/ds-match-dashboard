card_overlay <- function(pers,secs){
    cards <- key_moments %>% 
        drop_na(card_given) %>% 
        subset(period<=pers) %>% 
        subset(!(period==pers & time>secs)) %>% 
        mutate(ID = oth_ID)
    
    lineup <- lineup_times %>%
        subset(period<=pers) %>% 
        subset(!(period==pers & time>secs)) %>% 
        group_by(team_id,IDX) %>% 
        slice(n()) %>% 
        lineup_location() %>%
        mutate(
            X = ifelse(team_class=='B',(100-X/2),X/2),
            Y = ifelse(team_class=='B',(100-Y),Y),
            X = 415 + (X-50)/100*570,
            Y = 230 + (Y-50)/100*550
        ) %>% 
        inner_join(cards %>% select(ID,card_given),by='ID')
    
    plot_output <- ggplot() + 
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) + 
        theme_void() +
        geom_image(lineup,
                   mapping = aes(x=Y+15,
                                 y=X+15,
                                 image=paste0('images/icons/card',card_given,'.png')),
                   size=0.01)
    
    ggsave(paste0('output/layers/08/Card_',pers,'_',str_pad(round(secs),4,pad='0'),'.png'),
           plot_output,
           height=1080, width=1920, 
           dpi=300, units='px')
}