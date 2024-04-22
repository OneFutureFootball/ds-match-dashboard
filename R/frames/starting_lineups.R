starting_lineups <- function(){
    
    LU <- data.frame(img = list.files('graphics',pattern='lineup.png',recursive=TRUE,full.names=TRUE)) %>% 
        mutate(travel = ifelse(str_detect(img,'home'),'home','away'),
               X = 960 + 451.5*ifelse(travel == 'home',-1,1),
               Y = 560)
    
    image_write(image_crop(image_read(LU %>% subset(travel=='home') %>% pull(img)),'954x900+126+98'),'images/temp_home.png')
    image_write(image_crop(image_read(LU %>% subset(travel=='away') %>% pull(img)),'954x900+0+98'),'images/temp_away.png')
    LU <- LU %>% 
        mutate(img = case_when(
            travel=='home' ~ 'images/temp_home.png',
            travel=='away' ~ 'images/temp_away.png'
        ))
    
    ggplot() +
        background_image(readPNG('output/layers/title_page.png')) +
        geom_rect(mapping = aes(xmin=-1000,xmax=3000,
                                ymin=-1000,ymax=2000),
                  fill='black',alpha=0.8) +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        theme_void() +
        geom_image(LU,mapping = aes(x=X,y=Y,image=img),
                   size=0.76)
    ggsave('output/layers/starting_lineup.png',
           height=1080,
           width=1920,
           units='px',dpi=300)
        
}