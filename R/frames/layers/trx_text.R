trx_text <- function(time_idx){
  
  frame <- trx_frames %>% 
    subset(ORD == time_idx)
  
  frame_ord <- frame %>% 
    pull(IDX)
  
  status <- frame %>% 
    pull(type)
  
  time_stamp <- time_prog %>% 
    subset(IDX==frame_ord)
  
  if(is.na(time_stamp$next_team)) return(NULL)
  
  time_stamp <- time_stamp %>% mutate(time = trx_frames %>% subset(IDX==frame_ord & type==status) %>% pull(timestamp))
  
  if(!is.na(time_stamp$action)) time_stamp <- time_stamp %>% 
    left_join(teams %>% select(team_id,medium_name),by='team_id') %>% 
    mutate(
        team_id = case_when(
            next_state=='Free Kick' & str_detect(next_card,'red') ~ ifelse(possession=='A',this_match$away,this_match$home),
            TRUE ~ team_id
        ),
        short_name = case_when(
            next_state=='Free Kick' & str_detect(next_card,'red') ~ ifelse(possession=='A',this_match$away_name,this_match$home_name),
            TRUE ~ short_name
        ),
        action = case_when(
        is.na(action) ~ NA_character_,
        status=='possession' & action=='PENALTY' ~ 'PENALTY',
        status=='possession' & state%in%c('Free Kick','Corner','Throw In','Goal Kick','Kickoff') ~ toupper(state),
        status=='possession' ~ NA_character_,
        status=='result' ~ case_when(
          action%in%c('SHOT') & outcome=='goal' ~ 'GOAL',
          action%in%c('SHOT') & next_state=='Corner' ~ paste0(toupper(outcome),' - CORNER'),
          action%in%c('SHOT') & outcome=='goal' ~ 'PENALTY GOAL',
          action%in%c('SHOT') & next_state=='Corner' ~ paste0('PENALTY - ',toupper(outcome),' - CORNER'),
          action=='PENALTY' ~ paste0('PENALTY - ',toupper(outcome)),
          action=='SHOT' & state=='Free Kick' ~ paste0('FREE KICK SHOT - ',toupper(replace_na(outcome,''))),
          action=='SHOT' ~ paste0('SHOT - ',toupper(outcome)),
          next_state=='Corner' & team_id==next_team ~ ifelse(action=='PASS','CORNER WON',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - CORNER WON')),
          next_state=='Corner' & team_id!=next_team ~ ifelse(action=='PASS','CORNER CONCEDED',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - CORNER CONCEDED')),
          next_state=='Throw In' & team_id==next_team ~ ifelse(action=='PASS','THROW IN WON',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - THROW IN WON')),
          next_state=='Throw In' & team_id!=next_team ~ ifelse(action=='PASS','OUT OF PLAY',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - OUT OF PLAY')),
          outcome%in%c('turnover','lost possession') ~ ifelse(action=='PASS','TURNOVER',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - TURNOVER')),
          outcome=='intercepted' ~ ifelse(action=='PASS','INTERCEPTED',paste0(ifelse(state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick'),toupper(state),action),' - INTERCEPTED')),
          next_action=='PENALTY' ~ 'PENALTY CONCEDED',
          next_state=='Free Kick' & str_detect(next_card,'red') ~ 'RED CARD',
          next_state=='Free Kick' & team_id==next_team ~ 'FREE KICK WON',
          next_state=='Free Kick' & team_id!=next_team ~ 'FREE KICK CONCEDED',
          next_state=='Goal Kick' & team_id==next_team ~ 'GOAL KICK WON',
          next_state=='Goal Kick' & team_id!=next_team ~ 'GOAL KICK CONCEDED',
          state%in%c('Corner','Throw In','Goal Kick','Kickoff','Free Kick') ~ toupper(state),
          TRUE ~ str_replace(action,'PASS','')
        ),
        status=='action' ~ case_when(
          state%in%c('Corner','Throw In','Goal Kick','Kickoff') ~ toupper(state),
          state=='Free Kick' & action=='PASS' ~ 'FREE KICK - PASS',
          state=='Free Kick' & action=='PENALTY' ~ 'PENALTY',
          state=='Free Kick' & action=='SHOT' ~ 'FREE KICK - SHOT',
          TRUE ~ str_replace(action,'PASS','')
        )
      ),
      last_name = case_when(
        status=='possession' & action=='PENALTY CONCEDED' ~ oth_last_name,
        status=='result' & next_action=='PENALTY' ~ oth_last_name,
        status=='result' & next_state=='Free Kick' & str_detect(next_card,'red') ~ next_oth_last,
        status!='result' ~ last_name,
        is.na(action) ~ last_name,
        str_detect(action,'WON|CONCEDED') ~ toupper(medium_name),
        TRUE ~ last_name
      )
    )
  
  plot_output <- ggplot() +
    coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
    theme_void() +
    #Current transaction
    geom_shape(mapping = aes(x=960 + 240*c(-1,-1,1,1),
                             y=20 + 40*c(1,-1,-1,1),
                             fill=factor(time_stamp$team_id)),
               radius=0.017, colour='white',linewidth=0.2) +
    #Status
    geom_text(time_stamp,
              mapping = aes(x=960,y = 20 + 5, label=last_name, colour=short_name),
              hjust = 0.5, vjust=0, family='Montserrat-Bold', size=9) + 
    geom_text(time_stamp %>% drop_na(action),
              mapping = aes(x=960,y = 20 - 5, label=action, colour=short_name),
              hjust = 0.5, vjust=1, family='Montserrat-Bold', size=5) + 
    scale_fill_manual(values = team_colours,guide='none') + 
    scale_colour_manual(values = c(team_colours,text_colours[1:2]),guide='none')

  ggsave(paste0('output/layers/09/Text_',time_stamp$period,'_',str_pad(time_stamp$time,4,pad='0'),'.png'),
         plot_output,
         height=1080,width=1920,
         units='px',dpi=100)
}