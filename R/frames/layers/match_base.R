match_base <- function(){
  plot_output <- ggplot() +
    #Main purple background
    coord_cartesian(xlim = c(0,1920), ylim = c(0,1080)) +
    theme_void() +
    # background_image(readPNG('images/backgrounds/background.png')) +
    theme(panel.background = element_rect(fill='#260455',colour=NA)) +
    geom_image(mapping = aes(x=135 - 100, y=1020, image=paste0('images/managers/',this_match$home_name,'-small.png')),size=0.14) +
    geom_image(mapping = aes(x=135 + 100, y=1020, image=paste0('images/managers/',this_match$away_name,'-small.png')),size=0.14) +
    geom_image(mapping = aes(x=135,y=1020,image='images/icons/ball.png'),size=0.06) +
    # #Clock + scoreboard
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
               mapping = aes(x=X,y=1031,image=paste0('images/banners/',short_name,'-',possession,'.png')),
               size=0.204) +
    geom_shape(mapping = aes(x=960 + 100*c(-1,-1,1,1),
                             y=1096 + 22*c(1,-1,-1,1)),
               radius=0.010, fill='#17006b', colour='white',linewidth=0.2) +
    #Pitch Map
    geom_rect(mapping = aes(xmin = 420 + (-1:20)*(1500-420)/20,
                            xmax = 420 + (0:21)*(1500-420)/20,
                            ymin = 140 - 40, ymax = 805 + 40,
                            group = 0:21,fill=factor((0:21)%%2)),
              colour=NA) +
    scale_fill_manual(values = c('0'='#6D9F30','1'='#56892C'),guide='none') +
    geom_segment(mapping = aes(x=c(420,1500,960), xend=c(420,1500,960), y=140, yend=805),
                 colour='white',linewidth=0.6, lineend='round') +
    geom_segment(mapping = aes(x=420, xend=1500, y=c(140,805), yend=c(140,805)),
                 colour='white',linewidth=0.6, lineend='round') +
    geom_segment(mapping = aes(x=c(420,420,420,420,1500,1500,1500,1500),xend=c(474,474,582,582,1446,1446,1338,1338),
                               y=c(389.375,555.625,655.375,289.625,389.375,555.625,655.375,289.625),yend=c(389.375,555.625,655.375,289.625,389.375,555.625,655.375,289.625)),
                 colour='white',linewidth=0.6, lineend='round') +
    geom_segment(mapping = aes(x=c(474,582,1446,1338),xend=c(474,582,1446,1338),
                               y=c(555.625,289.625,555.625,289.625),yend=c(389.375,655.375,389.375,655.375)),
                 colour='white',linewidth=0.6, lineend='round') +
    geom_arc(mapping = aes(x0=960,y0=472.5,r=261.25/3,start=0,end=2*pi),
             colour='white',linewidth=0.6, lineend='round') +
    geom_arc(mapping = aes(x0=1392,y0=472.5,r=261.25/3,start=3.85,end=5.6),
             colour='white',linewidth=0.6, lineend='round') +
    geom_arc(mapping = aes(x0=528,y0=472.5,r=261.25/3,start=0.7,end=2.45),
             colour='white',linewidth=0.6, lineend='round') +
    geom_arc(mapping = aes(x0=420,y0=805,r=15,start=pi/2,end=pi),
             colour='white',linewidth=0.6, lineend='round') +
    geom_arc(mapping = aes(x0=1500,y0=805,r=15,start=3*pi/2,end=pi),
             colour='white',linewidth=0.6, lineend='round') +
    geom_arc(mapping = aes(x0=420,y0=140,r=15,start=0,end=pi/2),
             colour='white',linewidth=0.6, lineend='round') +
    geom_arc(mapping = aes(x0=1500,y0=140,r=15,start=3*pi/2,end=2*pi),
             colour='white',linewidth=0.6, lineend='round') +
    geom_point(mapping = aes(x=c(528,960,1392), y=472.5),
               colour='white',size=1.2) +
    geom_point(mapping = aes(x=c(420,420,1500,1500), y=c(140,805,140,805)),
               colour='white',size=0.1) +
    geom_segment(mapping = aes(x=c(rep(420,8),rep(1500,8),420-25,1500+25),
                               xend=c(rep(420,8)-25,rep(1500,8)+25,420-25,1500+25),
                               y=c(rep(seq(439.25,
                                           505.75,length.out=8),times=2),
                                   439.25,439.25),
                               yend=c(rep(seq(439.25,
                                              505.75,length.out=8),times=2),
                                      505.75,505.75)),
                 colour='white',linewidth=0.3, lineend='round') +
    # Key Moments
    geom_rect(mapping = aes(xmin=-50,xmax=320,
                            ymin=350,ymax=900),
              fill='#150928',colour='white',linewidth=0.2) +
    geom_segment(mapping = aes(x = -40, xend = 310,
                               y = 350 + (1:11)*46,
                               yend = 350 + (1:11)*46),
                 colour='white', linewidth=0.2) +
    #Stats background
    geom_rect(mapping = aes(xmin=-50,xmax=320,
                            ymin=-20,ymax=330),
              fill='#150928',colour='white',linewidth=0.2) +
    geom_image(all_stats %>% 
                 ungroup() %>% 
                 select(possession,short_name) %>% 
                 unique() %>% 
                 mutate(X = 135 + 140*ifelse(possession=='A',-1,1),
                        possession=ifelse(possession=='A','Home','Away')),
               mapping = aes(x=X,y=310,image=paste0('images/banners/',short_name,'-',possession,'.png')),
               size=0.065) +
    geom_text(all_stats %>%
                ungroup() %>%
                select(statistic,Y,KEEP) %>%
                unique() %>%
                subset(KEEP),
              mapping = aes(x=135,y=ifelse(statistic=='POSSESSION %',Y+30,Y),label=statistic),
              family='Montserrat-ExtraBold',colour='white',hjust=0.5,vjust=0.5, size=4) +
    #Ratings background
    geom_rect(mapping = aes(xmin=1600, xmax=1970,
                            ymin=-20,ymax=900),
              fill='#150928',colour='white',linewidth=0.2) +
    geom_rect(data.frame(IDX=1:10) %>%
                mutate(Y = 900 - 85*IDX,
                       X = 1600),
              mapping = aes(xmin=X + 90,
                            xmax=X + 90 + 270,
                            ymin=Y - 12 - 10,
                            ymax=Y - 12 + 10),
              colour='white', fill='transparent', linewidth=0.2) +
    geom_point(data.frame(IDX=1:10) %>%
                 mutate(Y = 900 - 85*IDX,
                        X = 1645),
               mapping = aes(x=X,
                             y=Y),
               colour='white', pch=19, size=6.5) +
    #Titles
    geom_text(mapping = aes(x=c(135,135,1785),
                            y=c(900,330,900) - 20,
                            label=c('KEY MOMENTS','MATCH STATS','PLAYER RATINGS')),
              family='Montserrat-Bold',colour='white', hjust=0.5, vjust=0.5, size=5) +
    #Team Kits
    geom_image(mapping = aes(x=1785 + 85, y=1030, image=paste0('images/kits/away/',match_details$away_short_name,'.png')), size=0.14) +
    geom_image(mapping = aes(x=1785 - 85, y=1030, image=paste0('images/kits/home/',match_details$home_short_name,'.png')), size=0.14) +
    #Ad Boards
    geom_rect(mapping = aes(xmin=360,xmax=1560,
                            ymin=845 - 27,
                            ymax=845 + 27),
              fill='#150928',colour=NA) +
    geom_rect(mapping = aes(xmin=360,xmax=1560,
                            ymin=100 - 27,
                            ymax=100 + 27),
              fill='#150928',colour=NA) +
    geom_rect(mapping = aes(xmin=360,xmax=360 + 10,
                            ymin=100 - 27,
                            ymax=845 + 27),
              fill='#150928',colour=NA) +
    geom_rect(mapping = aes(xmin=1560,xmax=1560-10,
                            ymin=100 - 27,
                            ymax=845 + 27),
              fill='#150928',colour=NA) +
    geom_rect(mapping = aes(xmin=360,xmax=1560,
                            ymin=100 - 28,
                            ymax=845 + 27),
              fill=NA,colour='white',linewidth=0.2) +
    geom_image(mapping = aes(x=1460, y=845,
                             image=paste0('images/banners/',
                                          teams %>% subset(team_id==match_details$away_id) %>% pull(short_name),
                                          '-Away.png')),size=0.15) +
    geom_image(mapping = aes(x=460, y=845,
                             image=paste0('images/banners/',
                                          teams %>% subset(team_id==match_details$home_id) %>% pull(short_name),
                                          '-Home.png')),size=0.15) +
    geom_shape(mapping = aes(x=555 + 30*c(0, 0, 1),
                             y=c(835,855, 845)),
               fill=team_colours[names(team_colours)==this_match$home],colour=NA) +
    geom_shape(mapping = aes(x=1365 - 30*c(0, 0, 1),
                             y=c(835,855, 845)),
               fill=team_colours[names(team_colours)==this_match$away],colour=NA) +
    geom_image(mapping = aes(x=960 + 230*c(-1,1),y=c(100,100),image='images/logos/1FF-Logo-Mono-Rev.png'),size=0.09) +
    geom_image(mapping = aes(x=c(960),y=c(845),image='images/logos/S3 Logo.png'),size=0.07) +
    geom_image(mapping = aes(x=960 + c(-210,210,0,-460,460), y=c(845,845,100,100,100), image=paste0('images/banners/adboard/',match_details$home_short_name,'.png')), size=0.22) +
    # geom_image(mapping = aes(x=1526, y=c(255,690), image=paste0('images/banners/adboard/',match_details$home_short_name,'.png')), size=0.22*5.25, angle=90) +
    # geom_image(mapping = aes(x=365, y=c(255,690), image=paste0('images/banners/adboard/',match_details$home_short_name,'.png')), size=0.22*5.25, angle=270) +
    theme(legend.position = 'none')
  
  
  ggsave('output/layers/01/Match.png',
         plot_output,
         height=1080,
         width=1920,
         dpi=300,
         units='px')
}