match_preview <- function(home=TRUE){
    plot_output <- ggplot() + 
        background_image(readPNG('output/layers/title_page.png')) +
        background_image(readPNG('images/overlays/black_overlay.png')) +
        coord_cartesian(xlim=c(0,1920),
                        ylim=c(0,1080)) +
        geom_image(mapping = aes(x=1100,y=540,image=(list.files('graphics/pre_game',pattern=paste0('preview_',ifelse(home,'home','away')),full.names=TRUE))),
                   size = 1.4) +
        geom_image(mapping = aes(x=250,y=540,image=paste0('images/managers/',ifelse(home,this_match$home_name,this_match$away_name),'.png')),
                   size = 0.5) +
        theme_void()
    ggsave(paste0('output/layers/match_preview_',ifelse(home,'A','B'),'.png'),
           plot_output,
           height=1080,width=1920,units='px')
}
#match_preview(FALSE)