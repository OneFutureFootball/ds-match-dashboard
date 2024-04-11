trx_export <- function(time_idx,status='action'){
  
  time_stamp <- time_prog %>% 
    subset(IDX==time_idx)
  
  if(status=='posession' & time_stamp$prev_time >= time_stamp$time-1) return(NULL)
  if(status=='possession' & is.na(time_stamp$prev_time)) return(NULL)
  if(status=='result' & time_stamp$next_time <= time_stamp$time+3) return(NULL)
  if(status=='result' & is.na(time_stamp$next_time)) return(NULL)
  
  time_stamp <- time_stamp %>% 
    mutate(
      LAB = case_when(
        status=='possession' & state%in%c('Free Kick','Corner','Throw In','Goal Kick') ~ paste0(last_name,'\n',toupper(state)),
        status=='possession' ~ last_name,
        status=='result' ~ case_when(
          action%in%c('SHOT','PENALTY') & outcome=='goal' ~ LAB,
          action%in%c('SHOT','PENALTY') ~ paste0(LAB,' (',toupper(time_stamp$outcome),')'),
          next_state=='Corner' ~ paste0(LAB,' (CORNER)'),
          next_state=='Throw In' ~ paste0(LAB,' (OUT OF PLAY)'),
          outcome=='turnover' ~ paste0(LAB,' (TURNOVER)'),
          outcome=='intercepted' ~ paste0(LAB,' (INTERCEPTED)'),
          TRUE ~ str_replace(LAB,'\nPASS','')
        ),
        status=='action' ~ LAB
      ),
      time = case_when(
        status=='action' ~ time,
        status=='possession' ~ case_when(
          time - prev_time == 1 ~ time,
          time - prev_time <= 4 ~ time-1,
          state%in%c('Free Kick','Corner','Throw In','Goal Kick') ~ ceiling(0.4*prev_time + 0.6*time),
          TRUE ~ ceiling(0.3*prev_time + 0.7*time)
        ),
        status=='result' ~ case_when(
          next_time - time == 1 ~ time,
          next_time - time <= 4 ~ time+1,
          TRUE ~ floor(0.7*time + 0.3*next_time)
        )
      ))
  
  plot_output <- ggplot() +
    coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
    theme_void() +
    #Ball + Status
    geom_segment(time_stamp,
                 mapping = aes(x = X,y = Y,
                               xend = X2,yend=Y2,
                               colour=factor(prev_team)),
                 linewidth=0.4, alpha=0.6) +
    geom_segment(time_stamp,
                 mapping = aes(x = X3,y = Y3,
                               xend = X2,yend=Y2,
                               colour=factor(prev_team2)),
                 linewidth=0.4,alpha=0.3) +
    geom_point(time_stamp,
               mapping = aes(x=X,
                             y=Y,
                             fill=factor(team_id)),
               colour='white',pch=21,size=6) +
    geom_point(time_stamp,
               mapping = aes(x=X2,
                             y=Y2,
                             fill=factor(prev_team)),
               colour='white',pch=21,size=6,alpha=0.6) +
    geom_point(time_stamp,
               mapping = aes(x=X3,
                             y=Y3,
                             fill=factor(prev_team2)),
               colour='white',pch=21,size=6,alpha=0.3) +
    geom_text(time_stamp,
              mapping = aes(x=230,y = 67, label=LAB),
              hjust = 0.5, vjust=1, family='Montserrat-Bold', size=6, colour='white') + 
    scale_fill_manual(values = team_colours,guide='none') + 
    scale_colour_manual(values = team_colours,guide='none')
  
  if(status=='result') plot_output <- plot_output +
    geom_segment(time_stamp,
                 mapping = aes(x=X, y=Y,
                               xend = 0.6*X + 0.4*X4, 
                               yend=0.6*Y + 0.4*Y4,
                               colour=factor(team_id)),
                 arrow = arrow(length=unit(0.01,'npc')), 
                 linewidth=0.4, lineend='round', linejoin = 'round')
  
  ggsave(paste0('output/layers/04/Trx_',time_stamp$period,'_',str_pad(time_stamp$time,4,pad='0'),'.png'),
         plot_output,
         height=1080,width=1920,
         units='px',dpi=100)
}