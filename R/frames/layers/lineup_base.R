lineup_base <- function(trx){
    
    lineups <- lineup_times %>%
        subset(TRX==trx) %>% 
        lineup_location() %>%
        mutate(
            Y = (Y-50)*1.25 + 50,
            X = ifelse(position=='GK',3,X),
            X = ifelse(team_class=='B',(100-X/2),X/2)*120/100,
            Y = ifelse(team_class=='B',Y,100-Y)*80/100,
            X = sapply(X,function(x) pitch_transform(x,'X')),
            Y = sapply(Y,function(x) pitch_transform(x,'Y')),
        ) %>% 
        left_join(teams %>% select(team_id,short_name,crest) %>% rename(kit=crest),by='team_id') %>% 
        mutate(
            kit = case_when(
                position=='GK' ~ paste0('images/kits/gk/',short_name,'-GK.png'),
                TRUE ~ paste0('images/kits/',ifelse(team_class=='A','home','away'),'/',short_name,'.png')
            ),
            WHITE = case_when(
                position=='GK' & team_id==2 ~ TRUE,
                position=='GK' & team_id==5 ~ TRUE,
                TRUE ~ FALSE
            ),
            BLACK = case_when(
                position=='GK' & team_id==6 ~ TRUE,
                position=='GK' & team_id==10~ TRUE,
                TRUE ~ FALSE
            ),
            FILL = case_when(
                WHITE|BLACK ~ FALSE,
                position=='GK' ~ FALSE,
                team_class=='A' & team_id%in%c(1,6,7,9,10) ~ TRUE,
                team_class=='B' & team_id%in%c(1,6,7,9,10,12) ~ TRUE,
                TRUE ~ FALSE
            ))
    
    card_overlay(unique(lineups$period),unique(lineups$time))
    
    
    subs <- key_moments %>% 
        subset(state=='Substitution') %>% 
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
        )
    
    fill_colours <- team_colours; fill_colours[names(fill_colours)==1] <- 'white'
    number_colours <- text_colours; number_colours[names(number_colours)==1] <- '#ee202e'
    
    plot_output <- ggplot() + 
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) + 
        theme_void() +
        geom_image(lineups,
                   mapping = aes(x=X,
                                 y=Y,
                                 image=kit),
                   size=0.06) +
        geom_text(lineups %>% subset(!FILL & !WHITE & !BLACK),
                  mapping = aes(x=X,
                                y=Y,
                                label=number,
                                colour=paste0('kit_',team_id)),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=5.7) +
        geom_text(lineups %>% subset(WHITE),
                  mapping = aes(x=X,
                                y=Y,
                                label=number),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=5.7, colour='white') +
        geom_text(lineups %>% subset(BLACK),
                  mapping = aes(x=X,
                                y=Y,
                                label=number),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=5.7, colour='black') +
        geom_rect(lineups %>% subset(FILL),
                  mapping = aes(xmin=X-15,xmax=X+15,
                                ymin=Y-10,ymax=Y+10,
                                fill=factor(team_id)),
                  colour='transparent',alpha=0.6) +
        geom_text(lineups %>% subset(FILL),
                  mapping = aes(x=X,
                                y=Y,
                                label=number,
                                colour=factor(team_id)),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=5.7) +
        geom_rect(lineups,
                  mapping = aes(xmin = X - 5.7*nchar(last_name),
                                xmax = X + 5.7*nchar(last_name),
                                ymin = Y - 50 - 10,
                                ymax = Y - 50 + 10,
                                group = ID),
                  fill='black', colour=NA, alpha=0.7) +
        geom_text(lineups,
                  mapping = aes(x=X,
                                y=Y - 50,
                                label=last_name),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=3.5, colour='white') +
        geom_point(lineups %>%
                       select(ID,X,Y) %>%
                       inner_join(subs %>% select(ID,icon),by='ID'),
                   mapping = aes(x=X-25,
                                 y=Y-25),
                   pch=19,size=3,colour='white') +
        geom_image(lineups %>%
                       select(ID,X,Y) %>%
                       inner_join(subs %>% select(ID,icon),by='ID'),
                   mapping = aes(x=X-25,
                                 y=Y-25,
                                 image=icon),
                   size=0.027) +
        scale_colour_manual(values = number_colours, na.value='white',guide='none') + 
        scale_fill_manual(values = fill_colours,guide='none')
    
    ggsave(paste0('output/layers/05/Lineup_',unique(lineups$period),'_',str_pad(unique(round(lineups$time)),4,pad='0'),'.png'),
           plot_output,
           height=1080, width=1920, 
           dpi=300, units='px')
}
