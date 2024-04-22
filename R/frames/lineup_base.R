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
    
    card_overlay(unique(lineups$period),unique(lineups$time))
    
    
    subs <- match_file %>% 
        subset(state=='Substitution') %>% 
        mutate(time = ifelse(match_id==3012 & period==2 & time<250,218,time)) %>% 
        subset(period<=unique(lineups$period)) %>% 
        subset(!(period==unique(lineups$period) & time>unique(lineups$time))) %>% 
        select(team_id,possession,ID,oth_role,oth_ID) %>% 
        gather('sub','ID',c(ID,oth_ID)) %>% 
        mutate(
            sub = ifelse(sub=='oth_ID','off','on'),
            icon = case_when(
                oth_role=='injury' & sub=='off' ~ 'images/icons/suboninjury.png',
                TRUE ~ 'images/icons/subon.png'
            )
        ) %>% 
        group_by(team_id,sub) %>% 
        mutate(IDX = row_number() - 0.5,
               Y = 238 + 250*ifelse(possession=='A',1,-1),
               X = 410 + 65*ifelse(possession=='A',-1,1)*IDX) %>% 
        left_join(fromJSON('data/player_identity.json') %>% select(ID,first_name,last_name,number),by='ID') %>% 
        left_join(teams %>% select(team_id,short_name),by='team_id') %>% 
        ungroup() %>% 
        mutate(last_name = toupper(ifelse(is.na(last_name),first_name,last_name)))
    
    home_subs <- subs %>% subset(possession=='A' & sub=='off') %>% nrow()
    away_subs <- subs %>% subset(possession=='B' & sub=='off') %>% nrow()
    
    plot_output <- ggplot() + 
        geom_image(mapping = aes(x=230, y=410, image='images/pitch.png'), size=0.401) +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) + 
        theme_void() +
        geom_image(mapping = aes(x=404, y=702, image=paste0('images/banners/',
                                                            teams %>% subset(team_id==match_details$away_id) %>% pull(short_name),
                                                            '-Away.png')),size=0.108) +
        geom_image(mapping = aes(x=56, y=117, image=paste0('images/banners/',
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
        geom_point(lineups %>%
                       select(ID,X,Y) %>%
                       inner_join(subs %>% select(ID,icon),by='ID'),
                   mapping = aes(x=Y-18,
                                 y=X+18),
                   pch=19,size=1.3,colour='white') +
        geom_image(lineups %>%
                       select(ID,X,Y) %>%
                       inner_join(subs %>% select(ID,icon),by='ID'),
                   mapping = aes(x=Y-18,
                                 y=X+18,
                                 image=icon),
                   size=0.014) +
        scale_colour_manual(values = c(text_colours), na.value='white',guide='none') + 
        scale_fill_manual(values = team_colours,guide='none')
    
    ggsave(paste0('output/layers/05/Lineup_',unique(lineups$period),'_',str_pad(unique(round(lineups$time)),4,pad='0'),'.png'),
           plot_output,
           height=1080, width=1920, 
           dpi=300, units='px')
}