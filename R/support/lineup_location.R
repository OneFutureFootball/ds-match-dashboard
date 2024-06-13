lineup_location <- function(input){
  LR_POS <- data.frame(N=1:3) %>% 
    left_join(data.frame(ORD=1:3),by=character()) %>% 
    subset(ORD<=N) %>% 
    mutate(SIDE = case_when(
      N==1 ~ 'C',
      N==2 & ORD==1 ~ 'L',
      N==2 & ORD==2 ~ 'R',
      N==3 & ORD==1 ~ 'L',
      N==3 & ORD==2 ~ 'C',
      N==3 & ORD==3 ~ 'R'
    ))
  
  lpos <- c('LB','LM','LW')
  rpos <- c('RB','RM','RW')
  input %>%
    mutate(
      LEFT  = side=='L',
      RIGHT = side=='R'
    ) %>% 
    #Count how many players in each position
    mutate(PORD = case_when(position=='GK'~1,
                            position=='CB'~2,
                            position=='LB'~3,
                            position=='RB'~3,
                            position=='CDM'~4,
                            position=='CM'~5,
                            position=='LM'~6,
                            position=='RM'~6,
                            position=='CAM'~7,
                            position=='LW'~8,
                            position=='RW'~8,
                            position=='ST'~9,
                            is.na(position)~10,
                            TRUE~11)) %>% 
    arrange(team_class, PORD,desc(LEFT),RIGHT) %>% 
    group_by(team_class, PORD) %>% 
    mutate(TOT=n(),
           ORD=row_number()) %>% 
    left_join(LR_POS,by=c('TOT'='N','ORD'='ORD')) %>%
    select(-c(LEFT,RIGHT,TOT)) %>% 
    ungroup() %>%
    group_by(team_class) %>% 
    mutate(OB = sum(position%in%c('LB','RB'),na.rm=TRUE),
           CB = sum(position=='CB',na.rm=TRUE),
           DM = sum(position=='CDM',na.rm=TRUE),
           OM = sum(position%in%c('LM','RM'),na.rm=TRUE),
           CM = sum(position=='CM',na.rm=TRUE),
           AM = sum(position=='CAM',na.rm=TRUE),
           ST = sum(position=='ST',na.rm=TRUE),
           WW = sum(position%in%c('LW','RW'),na.rm=TRUE),
           SUB = sum(is.na(position))
    ) %>%
    #Set X,Y Coordinate based on the makeup of the formation.
    #Pitch goes from (0,0) in bottom-left corner to (100,100) in top-left corner
    mutate(
      X = case_when(position=='CM' & AM==DM & ORD==2 & CM>2~ 52,
                    position=='CM' & DM==0 & ORD==2 & CM>2 ~ 47,
                    position=='CM' & AM==0 & ORD==2 & CM>2 ~ 58,
                    position=='CM' & AM==DM ~ 56,
                    position=='CM' & AM>DM & AM==1 & ORD==2 & CM>2 ~ 46,
                    position=='CM' & AM>DM & AM==1 & CM==1 ~ 46,
                    position=='CM' & AM>DM & AM==1 & CM==2 ~ 50,
                    position=='CM' & AM>DM & AM==2 & CM==2 ~ 46,
                    position=='CM' & AM>DM & AM==2 & CM==1 ~ 46,
                    position=='CM' & DM>AM ~ 61,
                    position=='CM' ~ 55,
                    position=='GK' ~ 5,
                    position=='CB' & CB>2 & ORD==2 & DM > 0 ~ 19,
                    position=='CB' & CB>2 & ORD==2 ~ 21,
                    position=='CB' & DM==2 & CB==2 ~ 21,
                    position=='CB' ~ 23,
                    position%in%c('LM','RM') & OB>0  ~60,
                    position%in%c('LM','RM') & OB==0 ~51,
                    position%in%c('LB','RB') & OM==0 ~32,
                    position%in%c('LB','RB') & OM>0  ~30,
                    position=='ST' & AM==0 & WW==0 ~ 84,
                    position=='ST' ~ 88,
                    position%in%c('LW','RW') & CM==2 & DM>AM ~ 81,
                    position%in%c('LW','RW') & AM==2 ~ 83,
                    position%in%c('LW','RW') ~ 81,
                    position=='CAM' & AM==2 ~ 64,
                    position=='CAM' & ST>0 & DM==0 ~ 68,
                    position=='CAM' & ST>0 & DM>1 & CM==2 ~ 70,
                    position=='CAM' & ST>0 & DM==1 & CM==1 ~ 73,
                    position=='CAM' & ST>0 ~ 67,
                    position=='CAM' & ST==0~ 72,
                    position=='CDM' & CM==2 & AM>0 ~ 39,
                    position=='CDM' & CM==2 ~ 43,
                    position=='CDM' & CM==1 & AM==1 ~ 38,
                    position=='CDM' & CM==0 & AM==0 ~ 47,
                    position=='CDM' & CM==0 ~ 44,
                    position=='CDM' ~ 40,
                    is.na(position) ~ -25,
                    TRUE~0),
      Y = case_when(position=='GK'~50,
                    position=='CAM' & AM==1 & DM==1 & CM==1 ~ 53,
                    position=='CAM' & AM==1~50,
                    position=='CAM' & AM==2 & CM%in%c(1,3) ~ (ORD-1.5)*20+50,
                    position=='CAM' & AM==2 ~ (ORD-1.5)*20+50,
                    position=='CDM' & AM==1 & DM==1 & CM==1 ~ 47,
                    position=='CDM' & DM==1 ~ 50,
                    position=='CDM' & DM==2 & CM%in%c(1,3) ~ (ORD-1.5)*24+50,
                    position=='CDM' & DM==2 ~ (ORD-1.5)*23+50,
                    position=='ST' & ST==1~50,
                    position=='ST' & ST==2 & AM==1 & CM==1 & DM==1 ~ (ORD-1.5)*20+50,
                    position=='ST' & ST==2 ~ (ORD-1.5)*17+50,
                    position=='CM' & CM==1~50,
                    position=='CM' & CM==2 & DM==0 & AM==0 ~ (ORD-1.5)*22+50,
                    position=='CM' & CM==2 & OM==0 ~ (ORD-1.5)*29+50,
                    position=='CM' & CM==2 & DM==AM ~ (ORD-1.5)*30+50,
                    position=='CM' & CM==2 ~ (ORD-1.5)*23+50,
                    position=='CM' & CM>2 & OM==0 ~ (ORD-2)*20+50,
                    position=='CM' & CM>2 & OM>0 ~ (ORD-2)*15+50,
                    position=='CB' & CB==2~(ORD-1.5)*25+50,
                    position=='CB' & CB>2 & OB==0 ~ (ORD-2)*23+50,
                    position=='CB' & CB>2 & OB>0 ~ (ORD-2)*17+50,
                    position=='LW' & ST==2 ~ 25,
                    position=='LW' & AM==2 ~ 25,
                    position=='LW' ~ 29,
                    position=='RW' & ST==2 ~ 75,
                    position=='RW' & AM==2 ~ 71,
                    position=='RW' ~ 71,
                    position=='LB' ~ 20,
                    position=='RB' ~ 80,
                    position=='LM' & DM==AM & CM==2 ~ 20,
                    position=='RM' & DM==AM & CM==2 ~ 80,
                    position=='LM' & CM==3 ~ 20,
                    position=='RM' & CM==3 ~ 80,
                    position=='LM' ~ 22,
                    position=='RM' ~ 78,
                    is.na(position) ~ -15+20*ORD,
                    TRUE~0)
    ) %>% 
    select(-c(OB,CB,DM,OM,CM,AM,ST,WW,SUB)) %>% 
    ungroup() %>% 
    return()
}
