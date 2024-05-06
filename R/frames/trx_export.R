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
  
  if(!is.na(time_stamp$action)) time_stamp <- time_stamp %>% 
    mutate(
      action = case_when(
        is.na(action) ~ NA_character_,
        status=='possession' & state%in%c('Free Kick','Corner','Throw In','Goal Kick','Kickoff') ~ toupper(state),
        status=='possession' ~ NA_character_,
        status=='result' ~ case_when(
          action%in%c('SHOT','PENALTY') & outcome=='goal' ~ 'GOAL',
          action%in%c('SHOT','PENALTY') & next_state=='Corner' ~ paste0(toupper(outcome),' - CORNER'),
          action%in%c('SHOT','PENALTY') ~ toupper(replace_na(outcome,'')),
          next_state=='Corner' & team_id==next_team ~ ifelse(action=='PASS','CORNER WON',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - CORNER WON')),
          next_state=='Corner' & team_id!=next_team ~ ifelse(action=='PASS','OPPO. CORNER',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - OPPO. CORNER')),
          next_state=='Throw In' & team_id==next_team ~ ifelse(action=='PASS','THROW IN WON',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - THROW IN WON')),
          next_state=='Throw In' & team_id!=next_team ~ ifelse(action=='PASS','OPPO. THROW IN',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - OPPO. THROW IN')),
          outcome=='turnover' ~ ifelse(action=='PASS','TURNOVER',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - TURNOVER')),
          outcome=='intercepted' ~ ifelse(action=='PASS','INTERCEPTED',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - INTERCEPTED')),
          next_state=='Free Kick' & team_id==next_team ~ ifelse(action=='PASS','FREE KICK WON',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - FREE KICK WON')),
          next_state=='Free Kick' & team_id!=next_team ~ ifelse(action=='PASS','OPPO. FREE KICK',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - OPPO. FREE KICK')),
          next_state=='Goal Kick' & team_id==next_team ~ ifelse(action=='PASS','GOAL KICK WON',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - GOAL KICK WON')),
          next_state=='Goal Kick' & team_id!=next_team ~ ifelse(action=='PASS','OPPO. GOAL KICK',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - OPPO. GOAL KICK')),
          state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick') ~ toupper(state),
          TRUE ~ str_replace(action,'PASS','')
        ),
        status=='action' ~ case_when(
          state%in%c('Corner','Throw In','Goal Kick','Kickoff') ~ toupper(state),
          TRUE ~ str_replace(action,'PASS','')
        )
      )
    )
  
  
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
      LEY = Y + GAP*YS*abs(cos(ANG)),
      LEX = ifelse(action=='SHOT' & DIS<150,NA_real_,LEX))
  time_stamp <- time_stamp %>% 
    mutate(
      RANG = atan((X - X2)/(Y - Y2)),
      RXS = sign(X2 - X),
      RYS = sign(Y2 - Y),
      RX = X + 17*RXS*abs(sin(RANG)),
      RY = Y + 17*RYS*abs(cos(RANG)),
      RX = case_when(
        state%in%c('Keeper Possession','Free Kick','Goal Kick','Kickoff') ~ X,
        state%in%c('Throw In') ~ X - 17*sign(Y-40),
        state=='Corner' ~ X - sign(Y-40)*17*sin(pi/4),
        TRUE ~ RX
      ),
      RY = case_when(
        state%in%c('Keeper Possession','Free Kick','Goal Kick','Kickoff') ~ Y + 17*ifelse(possession=='A',1,-1),
        state%in%c('Throw In') ~ Y,
        state=='Corner' ~ Y + 17*sin(pi/4),
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
  if(status%in%c('action','result') & !time_stamp$action%in%c('MISCONTROL') & !time_stamp$next_action%in%c('PENALTY') & !is.na(time_stamp$LEX)){
    if(!is.na(time_stamp$LEX)) plot_output <- plot_output +
        geom_image(time_stamp,
                   mapping = aes(x=LSX, y=LSY,
                                 image='images/icons/ball.png'),
                   size=0.018)
  }else{
    plot_output <- plot_output +
      geom_image(time_stamp,
                 mapping = aes(x=RX, y=RY,
                               image='images/icons/ball.png'),
                 size=0.018)
  }
  
  ggsave(paste0('output/layers/04/Trx_',time_stamp$period,'_',str_pad(time_stamp$time,4,pad='0'),'.png'),
         plot_output,
         height=1080,width=1920,
         units='px',dpi=100)
  
  trx_text(time_idx)
}