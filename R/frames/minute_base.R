minute_base <- function(pers,mins){
    stats <- all_stats %>% subset(period==pers & time==mins)
    if(nrow(stats)==0) stats <- all_stats %>% 
            subset(period==1 & time==1) %>% 
            mutate(value=0,
                   display=case_when(
                       statistic=='EXPECTED GOALS' ~ '0.00',
                       statistic=='POSSESSION %' ~ '0%',
                       TRUE ~ '0'))
    
    ratings <- all_ratings %>% subset(period==pers & time==mins) %>% 
        arrange(desc(value)) %>% 
        head(6) %>% 
        mutate(IDX = row_number(),
               Y = 640 - 100*IDX,
               X = 1380) %>% 
        mutate(value = ifelse(period==1 & time<=5,0,value),
               PCT = ifelse(period==1 & time<=5,0,PCT),
               short_name = sapply(full_name,function(x) paste0(substring(x,1,1),'.',str_replace(x,str_split(x,' ')[[1]][1],''))),
               full_name = ifelse(nchar(full_name)>=22,short_name,full_name))

    plot_output <- ggplot() +
        #background_image(readPNG('output/layers/01/Match.png')) + 
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        theme_void() + 
        geom_image(ratings, 
                   mapping = aes(x=X + 120, y=Y, image=circle_crop(profileImage)), 
                   size=0.07) +
        geom_rect(ratings, 
                  mapping = aes(xmin=X + 180,
                                xmax=X + 180 + 360*PCT,
                                ymin=Y - 18 - 12,
                                ymax=Y - 18 + 12,
                                fill=factor(team_id)),
                  colour='white', linewidth=0.1) +
        geom_text(ratings, 
                  mapping = aes(x=X + 180,
                                y=Y + 18,
                                label=value),
                  family='Montserrat-ExtraBold', colour='white', size=8, hjust=0, vjust=0.5) +
        geom_text(ratings, 
                  mapping = aes(x=X + 230,
                                y=Y + 18,
                                label=toupper(full_name)),
                  family='Montserrat-Medium', colour='white', size=6, hjust=0, vjust=0.5) + 
        geom_rect(stats %>% subset(statistic=='POSSESSION %'),
                  mapping = aes(xmin = X1, 
                                xmax = X2,
                                ymin = Y - 28,
                                ymax = Y + 28,
                                fill = factor(team_id)),
                  colour='white', linewidth=0.1) +
        geom_shape(stats %>% 
                       subset(KEEP & RANK==1 & N==1 & value>0 & !str_detect(statistic,'CARDS|POSS')) %>% 
                       mutate(REP = 4) %>% 
                       uncount(REP) %>% 
                       group_by(statistic) %>% 
                       mutate(ROW = row_number(),
                              X = X + ifelse(ROW%in%c(1,4),-50,50),
                              Y = Y + ifelse(ROW%in%c(1,2),25,-25)),
                   mapping = aes(x = X,
                                 y = Y,
                                 fill=factor(team_id),
                                 group=statistic),
                   radius = 0.006) +
        geom_text(stats %>% subset(KEEP & ((RANK==1 & N==1)|statistic=='POSSESSION %') & value>0 & !str_detect(statistic,'CARDS')),
                  mapping = aes(x=X,y=Y,label=display, colour=factor(team_id)),
                  family='Montserrat-Medium',hjust=0.5,vjust=0.5, size=7) +
        geom_text(stats %>% subset(KEEP & !(((RANK==1 & N==1)|statistic=='POSSESSION %') & value>0 & !str_detect(statistic,'CARDS'))),
                  mapping = aes(x=X,y=Y,label=display),
                  family='Montserrat-Medium',hjust=0.5,vjust=0.5, size=7, colour='white') +
        scale_colour_manual(values = c(text_colours, 'GOAL'='#5CED73', 'YELLOW CARD'='#FFF380', 'RED CARD'='#FF6955','SECOND YELLOW CARD'='#FF6955'), na.value='white', guide='none') + 
        scale_fill_manual(values = team_colours, guide='none')
    
    
    ggsave(paste0('output/layers/02/Minute_',pers,'_',str_pad(mins*60,4,pad='0'),'.png'),
           plot_output,
           height=1080,width=1920,
           dpi=300,units='px')
}