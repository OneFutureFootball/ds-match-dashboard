match_base <- function(){
  plot_output <- ggplot() +
    #Main purple background
    background_image(readPNG('images/background.png')) +
    #1FF logo
    geom_image(mapping = aes(x=1580, y=1070, image='images/S3 Logo.png'),size=0.12) +
    geom_image(mapping = aes(x=70, y=980, image=paste0('images/managers/',this_match$home_name,'-small.png')),size=0.2) +
    geom_image(mapping = aes(x=385, y=980, image=paste0('images/managers/',this_match$away_name,'-small.png')),size=0.2) +
    geom_text(mapping = aes(x=227,y=980,label='VS'),family='Montserrat-Black',colour='white',hjust=0.5,vjust=0.5,size=10) +
    #Clock + scoreboard
    geom_shape(mapping = aes(x = 960 + 320*c(-1,1,1,-1),
                            y = c(849,849,1035,1035)),
              fill='#150928', colour='white',linewidth=0.3,radius=0.01) +
    geom_rect(mapping = aes(xmin = 960 - 320,
                            xmax = 960 + 320,
                            ymax = 966,
                            ymin = 1035),
              fill='#150928', colour='white',linewidth=0.3) +
    geom_rect(mapping = aes(xmin=960 - 76,
                            xmax=960 + 76,
                            ymin=966,
                            ymax=1035),
              fill='#150928',colour='white',linewidth=0.3) +
    geom_image(all_stats %>% 
                 ungroup() %>% 
                 select(possession,short_name) %>% 
                 unique() %>% 
                 mutate(X=ifelse(possession=='A',960-198,960+198),
                        possession=ifelse(possession=='A','Home','Away')),
               mapping = aes(x=X,y=1001,image=paste0('images/banners/',short_name,'-',possession,'.png')),
               size=0.204) +
    geom_shape(mapping = aes(x=960 + 100*c(-1,-1,1,1),
                             y=1075 + 30*c(1,-1,-1,1)),
               radius=0.014, fill='#17006b', colour='white',linewidth=0.2) +
    #Pitch Map
    geom_rect(mapping = aes(xmin = 230 - 285,
                            xmax = 230 + 285,
                            ymax = 838,
                            ymin = -18),
              fill='#150928', colour='white',linewidth=0.3) +
    geom_image(mapping = aes(x=230, y=410, image='images/pitch.png'), size=0.4) +
    # Key Moments
    geom_shape(mapping = aes(x=c(min(all_stats$X) - 80,max(all_stats$X) + 80,max(all_stats$X) + 80,min(all_stats$X) - 80),
                            y=c(max(all_stats$Y[all_stats$KEEP]) + 50,max(all_stats$Y[all_stats$KEEP]) + 50,max(all_stats$Y[all_stats$KEEP]) + 410,max(all_stats$Y[all_stats$KEEP]) + 410)),
              fill='#150928',colour='white',linewidth=0.2,radius=0.01) +
    geom_segment(mapping = aes(x = 600, xend = 1320,
                               y = 478 + (1:5)*60,
                               yend = 478 + (1:5)*60),
                 colour='white', linewidth=0.2) +
    #Stats background
    geom_shape(mapping = aes(x = c(min(all_stats$X) - 80,max(all_stats$X) + 80,max(all_stats$X) + 80,min(all_stats$X) - 80),
                            y=c(min(all_stats$Y) - 40,min(all_stats$Y) - 40,max(all_stats$Y[all_stats$KEEP]) + 40,max(all_stats$Y[all_stats$KEEP]) + 40)),
              fill='#150928',colour='white',linewidth=0.2,radius=0.01) +
    geom_text(all_stats %>% 
                ungroup() %>% 
                select(statistic,Y,KEEP) %>% 
                unique() %>% 
                subset(KEEP),
              mapping = aes(x=960,y=ifelse(statistic=='POSSESSION %',Y+50,Y),label=statistic),
              family='Montserrat-ExtraBold',colour='white',hjust=0.5,vjust=0.5, size=6) +
    geom_segment(all_stats %>% 
                   ungroup() %>% 
                   select(IDX,Y) %>% 
                   unique() %>% 
                   subset(IDX%in%c(4,6)),
                 mapping = aes(x=min(all_stats$X) - 50, 
                               xend=max(all_stats$X) + 50, 
                               y=Y-29, yend=Y-29),
                 linewidth=0.3, colour='white') +
    #Ratings background
    geom_shape(mapping = aes(x = c(1430,1950,1950,1430),
                            y=c(-18,-18,660,660)),
              fill='#150928',colour='white',linewidth=0.2, radius=0.01) +
    geom_text(mapping = aes(x=1690,
                            y=620,
                            label='PLAYER RATINGS'),
              family='Montserrat-Bold',colour='white', hjust=0.5, vjust=0.5, size=8) +
    geom_rect(data.frame(IDX=1:6) %>% 
                mutate(Y = 640 - 100*IDX,
                       X = 1380), 
              mapping = aes(xmin=X + 180,
                            xmax=X + 180 + 360,
                            ymin=Y - 18 - 12,
                            ymax=Y - 18 + 12),
              colour='white', fill='transparent', linewidth=0.2) +
    geom_point(data.frame(IDX=1:6) %>% 
                mutate(Y = 640 - 100*IDX,
                       X = 1500), 
              mapping = aes(x=X,
                            y=Y),
              colour='white', pch=19, size=8.5) +
    #Team Kits
    geom_image(mapping = aes(x=1800, y=910, image=paste0('images/kits/away/',match_details$away_short_name,'.png')), size=0.27) +
    geom_image(mapping = aes(x=1580, y=860, image=paste0('images/kits/home/',match_details$home_short_name,'.png')), size=0.27) +
    geom_image(mapping = aes(x=230, y=783, image=paste0('images/banners/adboard/',match_details$home_short_name,'.png')), size=0.475) +
    coord_cartesian(xlim = c(0,1920), ylim = c(0,1080)) +
    theme_void() +
    theme(legend.position = 'none') + 
    scale_colour_manual(values = c(text_colours, 'GOAL'='#5CED73', 'YELLOW CARD'='#FFF380', 'RED CARD'='#FF6955','SECOND YELLOW CARD'='#FF6955'), na.value='white') + 
    scale_fill_manual(values = team_colours)
  

  ggsave('output/layers/01/Match.png',
         plot_output,
         height=1080,
         width=1920,
         dpi=300,
         units='px')
}