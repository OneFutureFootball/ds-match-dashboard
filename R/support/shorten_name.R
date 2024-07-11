shorten_name <- function(name){
    first_initial <- substring(name,1,1)
    remainder <- paste(str_split(name,' ')[[1]][-1],collapse=' ')
    output <- paste0(first_initial,'. ',remainder)
    return(output)
}