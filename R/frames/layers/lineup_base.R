lineup_base <- function(trx){
    
    lineups <- lineup_times %>%
        subset(TRX==trx) %>% 
        lineup_location() %>%
        mutate(
            Y = (Y-50)*1.25 + 50,
            X = ifelse(team_class=='B',(100-X/2),X/2)*120/100,
            Y = ifelse(team_class=='B',Y,100-Y)*80/100,
            X = sapply(X,function(x) pitch_transform(x,'X')),
            Y = sapply(Y,function(x) pitch_transform(x,'Y')),
        ) %>% 
        left_join(teams %>% select(team_id,short_name,crest) %>% rename(kit=crest),by='team_id') %>% 
        mutate(kit = case_when(
            position=='GK' ~ str_replace(str_replace(kit,'crests','kits'),'-256','-GK'),
            TRUE ~ str_replace(str_replace(kit,'crests','kits'),'-256',paste0('-shirt-',ifelse(team_class=='A','Home','Away'),'-small'))))
    
    # card_overlay(unique(lineups$period),unique(lineups$time))
    
    
    subs <- match_file %>% 
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

    plot_output <- ggplot() + 
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) + 
        theme_void() +
        geom_image(lineups,
                   mapping = aes(x=X,
                                 y=Y,
                                 image=kit),
                   size=0.075) +
        geom_text(lineups %>% subset(!(position!='GK' & team_class=='B' & team_id%in%c(6,9))),
                  mapping = aes(x=X,
                                y=Y+5,
                                label=number,
                                colour=paste0('kit_',team_id)),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=7) +
        geom_rect(lineups %>% subset(position!='GK' & team_class=='B' & team_id%in%c(6,9)),
                   mapping = aes(xmin=X-20,xmax=X+20,
                                 ymin=Y-10,ymax=Y+20,
                                 fill=factor(team_id)),
                   colour='transparent',alpha=0.6) +
        geom_text(lineups %>% subset(position!='GK' & team_class=='B' & team_id%in%c(6,9)),
                  mapping = aes(x=X,
                                y=Y+5,
                                label=number,
                                colour=factor(team_id)),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=7) +
        geom_rect(lineups,
                  mapping = aes(xmin = X - 6.8*nchar(last_name),
                                xmax = X + 7*nchar(last_name),
                                ymin = Y - 55 - 11,
                                ymax = Y - 55 + 9,
                                group = ID),
                  fill='black', colour=NA, alpha=0.7) +
        geom_text(lineups,
                  mapping = aes(x=X,
                                y=Y - 55,
                                label=last_name),
                  hjust = 0.5, vjust=0.5, family='Montserrat-Bold', size=4, colour='white') +
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
        scale_colour_manual(values = c(text_colours), na.value='white',guide='none') + 
        scale_fill_manual(values = team_colours,guide='none')
    
    ggsave(paste0('output/layers/05/Lineup_',unique(lineups$period),'_',str_pad(unique(round(lineups$time)),4,pad='0'),'.png'),
           plot_output,
           height=1080, width=1920, 
           dpi=300, units='px')
}
