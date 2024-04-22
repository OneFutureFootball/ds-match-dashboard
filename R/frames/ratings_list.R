ratings_list <- function(ref){
    ST <- data.frame(img = list.files('graphics',pattern=ifelse(ref=='fulltime','lineup_ratings','halftime_lineup'),recursive=TRUE,full.names=TRUE)) %>% 
        mutate(X = 960,
               Y = 540)
    ggplot() +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        theme_void() +
        geom_image(ST,mapping = aes(x=X,y=Y,image=img),
                   size=0.85)
    ggsave(paste0('output/layers/',ref,'_ratings.png'),
           height=1080,
           width=1920,
           units='px',dpi=300)
    
}