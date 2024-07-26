previous_match <- function(){
    
    stat_file <- list.files('graphics/prev_game',pattern='stats',full.names=TRUE)
    summ_file <- list.files('graphics/prev_game',pattern='summa',full.names=TRUE)
    
    plot_output <- ggplot() +
        background_image(readPNG('output/layers/title_page.png')) + 
        geom_rect(mapping = aes(xmin=-1000,xmax=3000,
                                ymin=-1000,ymax=2000),
                  fill='black',alpha=0.8) +
        coord_cartesian(xlim=c(0,1920),ylim=c(0,1080)) +
        theme_void() +
        geom_image(mapping = aes(x=480, y=460, image=stat_file),size=0.75) +
        geom_image(mapping = aes(x=1440, y=460, image=summ_file),size=0.75) +
        geom_shape(mapping = aes(x = 960 + 600*c(-1,1,1,-1),
                                 y = 1000 + 80*c(-1,-1,1,1)),
                   fill='#CA6FFF',colour='white',radius=0.035) +
        annotate('text',x = 960, y=1000, label = 'PREVIOUS MEETING\n',
                 family='Montserrat-ExtraBold', size=18, colour='#150928', hjust=0.5, vjust=0.5, lineheight=0.2) +
        annotate('text',x = 960, y=1000, label = paste0('\n',case_when(
            is.na(prev_match$final_type) ~ paste0('ROUND ',prev_match$round_no),
            prev_match$final_type=='qualifying final' ~ 'QUALIFYING FINAL',
            prev_match$final_type=='semi_final' ~ 'SEMI FINAL',
            prev_match$final_type=='final' ~ 'FINAL')),
            family='Montserrat-ExtraBold', size=12, colour='#150928', hjust=0.5, vjust=0.5, lineheight=0.4)
    
    ggsave(paste0('output/layers/prev_match.png'),
           plot_output,
           height=1080,
           width=1920,
           units='px',dpi=300)
    
}