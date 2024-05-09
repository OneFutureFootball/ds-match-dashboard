trx_export <- function(time_idx){
  
  frame <- trx_frames %>% 
    subset(ORD == time_idx)
  
  frame_ord <- frame %>% 
    pull(IDX)
  
  status <- frame %>% 
    pull(type)
  
  time_stamp <- time_prog %>% 
    subset(IDX==frame_ord)
  
  if(is.na(time_stamp$next_team)) return(NULL)
  
  time_stamp <- time_stamp %>% 
    mutate(time = trx_frames %>% subset(IDX==frame_ord & type==status) %>% pull(timestamp))
  
  if(status%in%c('action','result') & time_stamp$action%in%c('SHOT','PENALTY')) time_stamp <- time_stamp %>% mutate(X4 = 233, Y4=ifelse(possession=='A',704,112))
  
  if(status%in%c('action','result')) time_stamp <- time_stamp %>% 
    mutate(
      ANG = atan((X4 - X)/(Y4 - Y)),
      DIS = sqrt((X4 - X)^2 + (Y4 - Y)^2),
      XS = sign(X4 - X),
      YS = sign(Y4 - Y),
      LSX = X + 17*XS*abs(sin(ANG)),
      LSY = Y + 17*YS*abs(cos(ANG)),
      GAP = case_when(DIS<150 ~ 50,DIS>600 ~ 200,TRUE ~ DIS/3),
      LEX = X + GAP*XS*abs(sin(ANG)),
      LEY = Y + GAP*YS*abs(cos(ANG)))
  
  time_stamp <- time_stamp %>% 
    mutate(
      RANG = atan((X - X2)/(Y - Y2)),
      RXS = sign(X2 - X),
      RYS = sign(Y2 - Y),
      RX = X + 17*ifelse(prev_action%in%c('MOVE','CARRY','DRIBBLE'),-RXS,RXS)*abs(sin(RANG)),
      RY = Y + 17*ifelse(prev_action%in%c('MOVE','CARRY','DRIBBLE'),-RXS,RXS)*abs(cos(RANG)),
      RX = case_when(
        state%in%c('Keeper Possession','Free Kick','Goal Kick','Kickoff') ~ X,
        state%in%c('Throw In') ~ X - 17*sign(ball_y-40),
        state=='Corner' ~ X - 17*sign(ball_y-40)*sin(pi/4),
        TRUE ~ RX
      ),
      RY = case_when(
        state%in%c('Keeper Possession','Free Kick','Goal Kick','Kickoff') ~ Y + 17*ifelse(possession=='A',1,-1),
        state%in%c('Throw In') ~ Y,
        state=='Corner' ~ Y - 17*sign(ball_x-40)*sin(pi/4),
        TRUE ~ RY
      )
    )
  
  plot_output <- ggplot() +
    coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
    theme_void() +
    #Ball + Status
    geom_segment(time_stamp,
                 mapping = aes(x = X3,y = Y3,
                               xend = X2,yend=Y2,
                               colour=factor(prev_team2)),
                 linewidth=0.4,alpha=0.3) +
    geom_segment(time_stamp,
                 mapping = aes(x = X,y = Y,
                               xend = X2,yend=Y2,
                               colour=factor(prev_team)),
                 linewidth=0.4, alpha=0.6) +
    geom_point(time_stamp,
               mapping = aes(x=X3,
                             y=Y3,
                             fill=factor(prev_team2),
                             colour=prev_short_name2
               ),
               colour='white',pch=21,size=6.5,alpha=0.3) +
    geom_point(time_stamp,
               mapping = aes(x=X2,
                             y=Y2,
                             fill=factor(prev_team),
                             colour=prev_short_name),
               colour='white',pch=21,size=6.5,alpha=0.6) +
    geom_point(time_stamp,
               mapping = aes(x=X,
                             y=Y,
                             fill=factor(team_id),
                             colour=short_name),
               colour='white',pch=21,size=8) +
    geom_text(time_stamp,
              mapping = aes(x=X3,
                            y=Y3,
                            label=prev_number2,
                            colour=factor(prev_short_name2)),
              family='Montserrat-Medium',hjust=0.5,vjust=0.5,size=3.5,alpha=0.3) +
    geom_text(time_stamp,
              mapping = aes(x=X2,
                            y=Y2,
                            label=prev_number,
                            colour=factor(prev_short_name)),
              family='Montserrat-Medium',hjust=0.5,vjust=0.5,size=3.5,alpha=0.6) +
    geom_text(time_stamp,
              mapping = aes(x=X,
                            y=Y,
                            label=number,
                            colour=factor(short_name)),
              family='Montserrat-Medium',hjust=0.5,vjust=0.5,size=3.5) +
    scale_fill_manual(values = team_colours,guide='none') + 
    scale_colour_manual(values = c(team_colours,text_colours[1:2]),guide='none')
  
  if(!'LEX'%in%names(time_stamp)) time_stamp$LEX <- NA

  if(status=='possession'){
    plot_output <- plot_output +
      geom_image(time_stamp,
                 mapping = aes(x=RX, y=RY,
                               image='images/icons/ball.png'),
                 size=0.018)
  }
  if(status=='action'){
    if(!is.na(time_stamp$LEX)) plot_output <- plot_output +
        geom_image(time_stamp,
                   mapping = aes(x=ifelse(next_action=='PENALTY',RX,LSX), 
                                 y=ifelse(next_action=='PENALTY',RY,LSY),
                                 image='images/icons/ball.png'),
                   size=0.018)
  }
  if(status=='result'){
    plot_output <- plot_output +
      geom_segment(time_stamp,
                   mapping = aes(x=LSX,y=LSY,xend=X4,yend=Y4, colour=factor(team_id)),
                   linewidth=0.4) +
      geom_image(time_stamp,
                 mapping = aes(x=X4, y=Y4,
                               image='images/icons/ball.png'),
                 size=0.018)
  }
  ggsave(paste0('output/layers/04/Trx_',time_stamp$period,'_',str_pad(time_stamp$time,4,pad='0'),'.png'),
         plot_output,
         height=1080,width=1920,
         units='px',dpi=100)
  
  trx_text(time_idx)
}