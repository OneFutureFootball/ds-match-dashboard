match_base <- function(){
  plot_output <- ggplot() +
    #Main purple background
    background_image(readPNG('images/background.png')) +
    #1FF logo
    geom_image(mapping = aes(x=120, y=980, image='images/S3 Logo.png'),size=0.2) +
    #Clock + scoreboard
    geom_rect(mapping = aes(xmin=960 - 77,
                            xmax=960 + 77,
                            ymin=964,
                            ymax=1035),
              fill='#150928',colour=NA,linewidth=0.2) +
    geom_rect(mapping = aes(xmin = 960 - 320,
                            xmax = 960 + 320,
                            ymax = 974,
                            ymin = 974 - 125),
              fill='#150928', colour=NA) +
    geom_image(all_stats %>% 
                 ungroup() %>% 
                 select(possession,short_name) %>% 
                 unique() %>% 
                 mutate(X=ifelse(possession=='A',960-205,960+205),
                        possession=ifelse(possession=='A','Home','Away')),
               mapping = aes(x=X,y=1000,image=paste0('images/banners/',short_name,'-',possession,'.png')),
               size=0.216) +
    geom_shape(mapping = aes(x=960 + 100*c(-1,-1,1,1),
                             y=1075 + 30*c(1,-1,-1,1)),
               radius=0.014, fill='#17006b', colour=NA) +
    #Pitch Map
    geom_rect(mapping = aes(xmin = 230 - 285,
                            xmax = 230 + 285,
                            ymax = 838,
                            ymin = -18),
              fill='#150928', colour=NA) +
    geom_image(mapping = aes(x=230, y=410, image='images/pitch.png'), size=0.4) +
    #Current transaction
    geom_shape(mapping = aes(x=230 + 200*c(-1,-1,1,1),
                             y=40 + 40*c(1,-1,-1,1)),
               radius=0.017, fill='#17006b', colour=NA) +
    # Key Moments
    geom_rect(mapping = aes(xmin=min(all_stats$X) - 80,
                            xmax=max(all_stats$X) + 80,
                            ymin=max(all_stats$Y[all_stats$KEEP]) + 50,
                            ymax=max(all_stats$Y[all_stats$KEEP]) + 410),
              fill='#150928',colour=NA) +
    geom_segment(mapping = aes(x = 600, xend = 1320,
                               y = 478 + (1:5)*60,
                               yend = 478 + (1:5)*60),
                 colour='white', linewidth=0.2) +
    #Stats background
    geom_rect(mapping = aes(xmin=min(all_stats$X) - 80,
                            xmax=max(all_stats$X) + 80,
                            ymin=min(all_stats$Y) - 40,
                            ymax=max(all_stats$Y[all_stats$KEEP]) + 40),
              fill='#150928',colour=NA) +
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
    geom_rect(mapping = aes(xmin=1430,
                            xmax=1950,
                            ymin=-18,
                            ymax=600),
              fill='#150928',colour=NA) +
    geom_rect(data.frame(IDX=1:6) %>% 
                mutate(Y = 640 - 100*IDX,
                       X = 1380), 
              mapping = aes(xmin=X + 180,
                            xmax=X + 180 + 360,
                            ymin=Y - 18 - 12,
                            ymax=Y - 18 + 12),
              colour='white', fill='transparent', linewidth=0.1) +
    #Team Kits
    geom_image(mapping = aes(x=1830, y=850, image=paste0('images/kits/away/',match_details$away_short_name,'.png')), size=0.3) +
    geom_image(mapping = aes(x=1550, y=850, image=paste0('images/kits/home/',match_details$home_short_name,'.png')), size=0.3) +
    geom_image(mapping = aes(x=230, y=785, image=paste0('images/banners/adboard/',match_details$home_short_name,'.png')), size=0.48) +
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