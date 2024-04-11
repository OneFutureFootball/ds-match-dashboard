build_frame <- function(idx){
    
    frame <- frame_index %>% subset(IDX==idx)
    
    FRAME <- image_read(frame$base)
    
    if(!is.na(frame$filename)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/06/Clock',frame$filename)))
    if(!is.na(frame$minute)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/02/',frame$minute)))
    if(!is.na(frame$key)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/03/',frame$key)))
    # if(!is.na(frame$trx_3) & is.na(frame$lineup)) FRAME <- FRAME %>% image_composite(image_transparent(image_read(paste0('output/layers/04/',frame$trx_3)),color='none',fuzz = 0.50))
    # if(!is.na(frame$trx_2) & is.na(frame$lineup)) FRAME <- FRAME %>% image_composite(image_transparent(image_read(paste0('output/layers/04/',frame$trx_2)),color='none',fuzz = 0.75))
    if(!is.na(frame$trx)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/04/',frame$trx)))
    if(!is.na(frame$lineup)) FRAME <- FRAME %>% image_composite(image_read(paste0('output/layers/05/',frame$lineup)))
    if(!is.na(frame$overlay)){
        FRAME <- FRAME %>% image_composite(image_read('images/black_overlay.png'))
        FRAME <- FRAME %>% image_composite(image_read(frame$overlay))
    }
    image_write(FRAME,paste0('output/layers/99/Frame_',str_pad(frame$IDX,5,pad='0'),'.png'))
    
}