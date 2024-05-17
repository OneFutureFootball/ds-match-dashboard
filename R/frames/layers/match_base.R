match_base <- function(){
  
  pitch_image <- readPNG('images/backgrounds/pitch.png')
  plot_output <- ggplot() +
    #Main purple background
    coord_cartesian(xlim = c(0,1920), ylim = c(0,1080)) +
    theme_void() +
    #background_image(readPNG('images/backgrounds/background.png')) +
    theme(panel.background = element_rect(fill='#260455',colour=NA)) +
    geom_image(mapping = aes(x=125 - 100, y=1020, image=paste0('images/managers/',this_match$home_name,'-small.png')),size=0.14) +
    geom_image(mapping = aes(x=125 + 100, y=1020, image=paste0('images/managers/',this_match$away_name,'-small.png')),size=0.14) +
    geom_image(mapping = aes(x=125,y=1020,image='images/icons/ball.png'),size=0.06) +
    #Clock + scoreboard
    geom_shape(mapping = aes(x = 960 + 280*c(-1,1,1,-1),
                             y = 975 + 53*c(-1,-1,1,1)),
               fill='#150928', colour='white',linewidth=0.3,radius=0.01) +
    geom_rect(mapping = aes(xmin = 960 - 320,
                            xmax = 960 + 320,
                            ymax = 996,
                            ymin = 1065),
              fill='#150928', colour='white',linewidth=0.3) +
    geom_rect(mapping = aes(xmin=960 - 76,
                            xmax=960 + 76,
                            ymin=996,
                            ymax=1065),
              fill='#150928',colour='white',linewidth=0.3) +
    geom_image(all_stats %>% 
                 ungroup() %>% 
                 select(possession,short_name) %>% 
                 unique() %>% 
                 mutate(X=ifelse(possession=='A',960-198,960+198),
                        possession=ifelse(possession=='A','Home','Away')),
               mapping = aes(x=X,y=1030,image=paste0('images/banners/',short_name,'-',possession,'.png')),
               size=0.204) +
    geom_shape(mapping = aes(x=960 + 100*c(-1,-1,1,1),
                             y=1096 + 22*c(1,-1,-1,1)),
               radius=0.010, fill='#17006b', colour='white',linewidth=0.2) +
    #Pitch Map
    annotation_custom(rasterGrob(pitch_image),
                      xmin=360,xmax=1530,
                      ymin=90,ymax=870) +
    # Key Moments
    geom_rect(mapping = aes(xmin=-50,xmax=300,
                            ymin=350,ymax=900),
              fill='#150928',colour='white',linewidth=0.2) +
    geom_segment(mapping = aes(x = -45, xend = 295,
                               y = 350 + (1:11)*46,
                               yend = 350 + (1:11)*46),
                 colour='white', linewidth=0.2) +
    #Stats background
    geom_rect(mapping = aes(xmin=-50,xmax=300,
                            ymin=-20,ymax=330),
              fill='#150928',colour='white',linewidth=0.2) +
    geom_image(all_stats %>% 
                 ungroup() %>% 
                 select(possession,short_name) %>% 
                 unique() %>% 
                 mutate(X = 125 + 130*ifelse(possession=='A',-1,1),
                        possession=ifelse(possession=='A','Home','Away')),
               mapping = aes(x=X,y=310,image=paste0('images/banners/',short_name,'-',possession,'.png')),
               size=0.06) +
    geom_text(all_stats %>%
                ungroup() %>%
                select(statistic,Y,KEEP) %>%
                unique() %>%
                subset(KEEP),
              mapping = aes(x=125,y=ifelse(statistic=='POSSESSION %',Y+30,Y),label=statistic),
              family='Montserrat-ExtraBold',colour='white',hjust=0.5,vjust=0.5, size=4) +
    #Ratings background
    geom_rect(mapping = aes(xmin=1600, xmax=1980,
                            ymin=-20,ymax=900),
              fill='#150928',colour='white',linewidth=0.2) +
    geom_rect(data.frame(IDX=1:10) %>%
                mutate(Y = 900 - 85*IDX,
                       X = 1600),
              mapping = aes(xmin=X + 90,
                            xmax=X + 90 + 280,
                            ymin=Y - 12 - 10,
                            ymax=Y - 12 + 10),
              colour='white', fill='transparent', linewidth=0.2) +
    geom_point(data.frame(IDX=1:10) %>%
                 mutate(Y = 900 - 85*IDX,
                        X = 1640),
               mapping = aes(x=X,
                             y=Y),
               colour='white', pch=19, size=6.5) +
    #Titles
    geom_text(mapping = aes(x=c(125,125,1790),
                            y=c(900,330,900) - 20,
                            label=c('KEY MOMENTS','MATCH STATS','PLAYER RATINGS')),
              family='Montserrat-Bold',colour='white', hjust=0.5, vjust=0.5, size=5) +
    #Team Kits
    geom_image(mapping = aes(x=1780 + 90, y=1030, image=paste0('images/kits/away/',match_details$away_short_name,'.png')), size=0.15) +
    geom_image(mapping = aes(x=1780 - 90, y=1030, image=paste0('images/kits/home/',match_details$home_short_name,'.png')), size=0.15) +
    #Ad Boards
    geom_rect(mapping = aes(xmin=338,xmax=1553,
                            ymin=855 - 27,
                            ymax=855 + 27),
              fill='#150928',colour=NA) +
    geom_rect(mapping = aes(xmin=338,xmax=1553,
                            ymin=100 - 27,
                            ymax=100 + 27),
              fill='#150928',colour=NA) +
    geom_rect(mapping = aes(xmin=338,xmax=338 + 54,
                            ymin=100 - 27,
                            ymax=855 + 27),
              fill='#150928',colour=NA) +
    geom_rect(mapping = aes(xmin=1499,xmax=1499 + 54,
                            ymin=100 - 27,
                            ymax=855 + 27),
              fill='#150928',colour=NA) +
    geom_image(mapping = aes(x=1450, y=855, 
                             image=paste0('images/banners/',
                                          teams %>% subset(team_id==match_details$away_id) %>% pull(short_name),
                                          '-Away.png')),size=0.12) +
    geom_image(mapping = aes(x=440, y=855, 
                             image=paste0('images/banners/',
                                          teams %>% subset(team_id==match_details$home_id) %>% pull(short_name),
                                          '-Home.png')),size=0.12) +
    geom_shape(mapping = aes(x=520 + 30*c(0, 0, 1),
                             y=c(845,865, 855)),
               fill=team_colours[names(team_colours)==this_match$home],colour=NA) +
    geom_shape(mapping = aes(x=1365 - 30*c(0, 0, 1),
                             y=c(845,865, 855)),
               fill=team_colours[names(team_colours)==this_match$away],colour=NA) +
    geom_image(mapping = aes(x=c(708,1182),
                             y=c(100,100),
                             image='images/logos/1FF-Logo-Mono-Rev.png'),
               size=0.09) +
    geom_image(mapping = aes(x=c(945),
                             y=c(855),
                             image='images/logos/S3 Logo.png'),
               size=0.07) +
    geom_image(mapping = aes(x=c(1526),
                             y=c(475),
                             image='images/logos/1FF-White.png'),
               size=0.65,angle=90) +
    geom_image(mapping = aes(x=c(365),
                             y=c(475),
                             image='images/logos/1FF-White.png'),
               size=0.65,angle=270) +
    geom_image(mapping = aes(x=c(708,1182,945,470,1420), y=c(855,855,100,100,100), image=paste0('images/banners/adboard/',match_details$home_short_name,'.png')), size=0.22) +
    geom_image(mapping = aes(x=1526, y=c(255,690), image=paste0('images/banners/adboard/',match_details$home_short_name,'.png')), size=0.22*5.25, angle=90) +
    geom_image(mapping = aes(x=365, y=c(255,690), image=paste0('images/banners/adboard/',match_details$home_short_name,'.png')), size=0.22*5.25, angle=270) +
    theme(legend.position = 'none')
  
  
  ggsave('output/layers/01/Match.png',
         plot_output,
         height=1080,
         width=1920,
         dpi=300,
         units='px')
}