add_xy <- function(input){

  zone_boundaries <- data.frame(
    x = rep(1:5,times=3),
    y = rep(rev(1:3),each=5),
    xmin = rep(c(0,18,40,80,102),times=3),
    xmax = rep(c(18,40,80,102,120),times=3),
    ymin = rep(c(0,18,62),each=5),
    ymax = rep(c(18,62,80),each=5)
  )
  
  input %>%
    left_join(zone_boundaries,by=c('x','y')) %>%
    group_by(period) %>% 
    mutate(
      next_xmin = lead(xmin),
      next_xmax = lead(xmax),
      next_ymin = lead(ymin),
      next_ymax = lead(ymax),
      prev_xmin = lag(xmin),
      prev_xmax = lag(xmax),
      prev_ymin = lag(ymin),
      prev_ymax = lag(ymax),
      RANDX = case_when(
        x==1 & position%in%c('LW','ST','RW') ~ runif(n())^0.4,
        x==2 & position%in%c('LW','ST','RW') ~ runif(n())^0.6,
        x==3 & position%in%c('LW','ST','RW') ~ runif(n())^0.8,
        x==1 & position%in%c('CAM')          ~ runif(n())^0.6,
        x==2 & position%in%c('CAM')          ~ runif(n())^0.8,
        x==1 & position%in%c('LM','CM','RM') ~ runif(n())^0.8,
        x==5 & position%in%c('LM','CM','RM') ~ 1 - runif(n())^0.8,
        x==4 & position%in%c('CDM')          ~ 1 - runif(n())^0.8,
        x==5 & position%in%c('CDM')          ~ 1 - runif(n())^0.6,
        x==3 & position%in%c('LB','CB','RB') ~ 1 - runif(n())^0.8,
        x==4 & position%in%c('LB','CB','RB') ~ 1 - runif(n())^0.6,
        x==5 & position%in%c('LB','CB','RB') ~ 1 - runif(n())^0.4,
        x==1 & position%in%c('GK')           ~ 1 - runif(n())^0.9,
        x==2 & position%in%c('GK')           ~ 1 - runif(n())^0.8,
        x==3 & position%in%c('GK')           ~ 1 - runif(n())^0.6,
        TRUE ~ runif(n())),
      RANDY = case_when(
        position=='GK' ~ rnorm(n(),0.5,0.1),
        x==1 & y==2 & position%in%c('LW','LM','LB') ~ 1 - runif(n())^0.6,
        x==2 & y==2 & position%in%c('LW','LM','LB') ~ 1 - runif(n())^0.6,
        x==3 & y==2 & position%in%c('LW','LM','LB') ~ 1 - runif(n())^0.8,
        x==4 & y==2 & position%in%c('LW','LM','LB') ~ 1 - runif(n())^0.6,
        x==5 & y==2 & position%in%c('LW','LM','LB') ~ 1 - runif(n())^0.4,
        x==1 & y==2 & position%in%c('RW','RM','RB') ~ runif(n())^0.6,
        x==2 & y==2 & position%in%c('RW','RM','RB') ~ runif(n())^0.6,
        x==3 & y==2 & position%in%c('RW','RM','RB') ~ runif(n())^0.8,
        x==4 & y==2 & position%in%c('RW','RM','RB') ~ runif(n())^0.6,
        x==5 & y==2 & position%in%c('RW','RM','RB') ~ runif(n())^0.4,
        TRUE ~ runif(n())),
      PREV = lag(state),
      prev_action = lag(action),
      prev_team = lag(team_id),
      NEXT = lead(state),
      ball_x = case_when(
        PREV=='Kickoff' ~ round(RANDX*(60-xmin)+xmin,2),
        state=='Kickoff' ~ 60,
        action=='penalty' ~ 108,
        state=='Goal Kick' ~ 18,
        state%in%c('Goal','Corner') ~ 120,
        team_id==prev_team & prev_action%in%c('miscontrol','move','dribble') & xmin<next_xmin ~ round(0.8*xmax + 0.2*(RANDX*(xmax - xmin)+xmin),2),
        team_id==prev_team & prev_action%in%c('miscontrol','move','dribble') & xmin>next_xmin ~ round(0.8*xmin + 0.2*(RANDX*(xmax - xmin)+xmin),2),
        # team_id!=prev_team & prev_action%in%c('miscontrol','move','dribble') & xmin<next_xmin ~ round(0.8*xmax + 0.2*(RANDX*(xmax - xmin)+xmin),2),
        # team_id!=prev_team & prev_action%in%c('miscontrol','move','dribble') & xmin>next_xmin ~ round(0.8*xmin + 0.2*(RANDX*(xmax - xmin)+xmin),2),
        action%in%c('miscontrol','move','dribble') & xmin<next_xmin ~ round(0.8*xmax + 0.2*(RANDX*(xmax - xmin)+xmin),2),
        action%in%c('miscontrol','move','dribble') & xmin>next_xmin ~ round(0.8*xmin + 0.2*(RANDX*(xmax - xmin)+xmin),2),
        TRUE ~ round(RANDX*(xmax-xmin)+xmin,2)),
      ball_y = case_when(
        state%in%c('Kickoff','Goal','Goal Kick') ~ 40,
        action=='penalty' ~ 40,
        state%in%c('Throw In','Corner') & y==1 ~ 80,
        state%in%c('Throw In','Corner') & y==3 ~ 0,
        team_id==prev_team & prev_action%in%c('miscontrol','move','dribble') & ymin<next_ymin ~ round(0.8*ymax + 0.2*(RANDY*(ymax - ymin)+ymin),2),
        team_id==prev_team & prev_action%in%c('miscontrol','move','dribble') & ymin>next_ymin ~ round(0.8*ymin + 0.2*(RANDY*(ymax - ymin)+ymin),2),
        # team_id!=prev_team & prev_action%in%c('miscontrol','move','dribble') & xmin<next_xmin ~ round(0.8*xmax + 0.2*(RANDX*(xmax - xmin)+xmin),2),
        # team_id!=prev_team & prev_action%in%c('miscontrol','move','dribble') & xmin>next_xmin ~ round(0.8*xmin + 0.2*(RANDX*(xmax - xmin)+xmin),2),
        action%in%c('miscontrol','move','dribble') & ymin<next_ymin ~ round(0.8*ymax + 0.2*(RANDY*(ymax - ymin)+ymin),2),
        action%in%c('miscontrol','move','dribble') & ymin>next_ymin ~ round(0.8*ymin + 0.2*(RANDY*(ymax - ymin)+ymin),2),
        TRUE ~ round(RANDY*(ymax-ymin)+ymin,2))
    ) %>%
    mutate(ball_x=ifelse(possession=='B',120-ball_x,ball_x),
           ball_y=ifelse(possession=='B',80-ball_y,ball_y),
           prev_state = lag(state),
           next_state = lead(state),
           prev_team = lag(team_id),
           prev_team2= lag(team_id,2),
           prev_x = lag(ball_x),
           prev_y = lag(ball_y),
           prev_x = ifelse(state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick'),NA_real_,prev_x),
           prev_y = ifelse(state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick'),NA_real_,prev_y),
           prev_x2 = ifelse(is.na(prev_x),NA_real_,lag(ball_x,2)),
           prev_y2 = ifelse(is.na(prev_y),NA_real_,lag(ball_y,2)),
           prev_x2 = ifelse(prev_state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick'),NA_real_,prev_x2),
           prev_y2 = ifelse(prev_state%in%c('Kickoff','Throw In','Corner', 'Free Kick', 'Keeper Possession', 'Goal Kick'),NA_real_,prev_y2),
           next_x = case_when(
             action=='shoot' ~ 60 + sign(ball_x - 60)*60,
             state=='Goal' ~ NA_real_,
             next_state=='Corner' ~ 60 + sign(ball_x - 60)*60,
             TRUE ~ lead(ball_x)
           ),
           next_y = case_when(
             action=='shoot' ~ 40,
             state=='Goal' ~ NA_real_,
             next_state=='Corner' & action!='shoot' & sign(lead(ball_y)-40)==1 ~ runif(n(),min = min(c(44,max(c(ball_y-20,44)))), max=max(c(44,min(c(ball_y+20,80))))),
             next_state=='Corner' & action!='shoot' & sign(lead(ball_y)-40)!=1 ~ runif(n(),min = min(c(44,max(c(ball_y-20,44)))), max=max(c(44,min(c(ball_y+20,80))))),
             TRUE ~ lead(ball_y)
           )) %>%
    select(-c(xmin,xmax,ymin,ymax,RANDX,RANDY,PREV,prev_state)) %>%
    return()
}

