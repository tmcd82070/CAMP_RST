getRiverPassage <- function(stem){
  
  #   stem <- l3Folder
  
  #   ---- Identify the different type of passage runs.  
  files <- list.files(stem)
  
  if(length(files) > 0){
    ls_passageB <- files[grep('lifestage_passage_table.csv',files)]
    passageB <- files[grep('passage_table.csv',files)]         
    
    #   ---- Open up the files one-by-one, and suck out the passage results.  
    openTheseB <- unique(data.frame(file=c(ls_passageB,passageB),stringsAsFactors=FALSE))
    rownames(openTheseB) <- NULL
    for(l in 1:nrow(openTheseB)){
      if(substr(openTheseB$file[l],nchar(openTheseB$file[l]) - 26,nchar(openTheseB$file[l]) - 26 + 3) == 'life'){
        openTheseB$type[l] <- 'life'
      } else if(substr(openTheseB$file[l],nchar(openTheseB$file[l]) - 20,nchar(openTheseB$file[l]) - 20 + 2) == 'run'){
        openTheseB$type[l] <- 'run'
      } else {
        openTheseB$type[l] <- 'summary'
      }
    }
    
    bigDFB <- getTheData(openThese=openTheseB,stem=stem)  
    return(bigDFB)
  } 
}