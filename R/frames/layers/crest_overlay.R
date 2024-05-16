crest_overlay <- function(poss){
    plot_input <- match_details %>% 
        mutate(team_id = ifelse(poss=='A',home_id,away_id)) %>% 
        left_join(teams,by='team_id') %>% 
        mutate(crest = str_replace(crest,'256',ifelse(poss=='A','60','white-60')))
    
    
    ggsave(paste0('output/layers/crest_',poss,'.png'),
           ggplot() +
               theme_void() +
               coord_cartesian(xlim = c(0,1920), ylim=c(0,1080)) +
               geom_image(plot_input,
                          mapping = aes(x=960 + 300*ifelse(poss=='A',-1,1),y = 20, image = crest),
                          size=ifelse(poss=='A',0.055,0.07)),
           height=1080,width=1920,
           units='px')
    
}