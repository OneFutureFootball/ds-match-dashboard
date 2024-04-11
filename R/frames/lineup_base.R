lineup_base <- function(trx){
    
    lineups <- lineup_times %>%
        subset(TRX==trx) %>% 
        lineup_location() %>%
        mutate(
            X = ifelse(team_class=='B',(100-X/2),X/2),
            Y = ifelse(team_class=='B',(100-Y),Y),
            X = 415 + (X-50)/100*570,
            Y = 230 + (Y-50)/100*550
        )
    
    plot_output <- ggplot() + 
        geom_image(mapping = aes(x=230, y=410, image='images/pitch.png'), size=0.401) +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) + 
        theme_void() +
        geom_image(mapping = aes(x=404, y=702, image=paste0('images/banners/',
                                                            teams %>% subset(team_id==match_details$away_id) %>% pull(short_name),
                                                            '-Away.png')),size=0.108) +
        geom_image(mapping = aes(x=57, y=117, image=paste0('images/banners/',
                                                           teams %>% subset(team_id==match_details$home_id) %>% pull(short_name),
                                                           '-Home.png')),size=0.108) +
        geom_point(lineups,
                   mapping = aes(x=Y,
                                 y=X,
                                 fill=factor(team_id)),
                   pch=21, size=3) +
        geom_text(lineups,
                  mapping = aes(x=Y,
                                y=X,
                                label=number,
                                colour=factor(team_id)),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=4.5) +
        geom_text(lineups,
                  mapping = aes(x=Y,
                                y=X - 25,
                                label=last_name),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=3.8, colour='#150928') +
        scale_colour_manual(values = c(text_colours), na.value='white',guide='none') + 
        scale_fill_manual(values = team_colours,guide='none'); plot_output
    ggsave(paste0('output/layers/05/Lineup_',unique(lineups$period),'_',str_pad(unique(round(lineups$time)),4,pad='0'),'.png'),
           plot_output,
           height=1080, width=1920, 
           dpi=300, units='px')
}
