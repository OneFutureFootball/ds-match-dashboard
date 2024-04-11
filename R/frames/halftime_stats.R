halftime_stats <- function(){
    
    ST <- data.frame(img = list.files('graphics',pattern='stats_ht.png',recursive=TRUE,full.names=TRUE)) %>% 
        mutate(X = 960,
               Y = 540)
    ggplot() +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        theme_void() +
        geom_image(ST,mapping = aes(x=X,y=Y,image=img),
                   size=0.85)
    ggsave('output/layers/halftime_stats.png',
           height=1080,
           width=1920,
           units='px',dpi=300)
    
}
