buff.days <- function(beg.buff,end.buff,df){
  
  # calculate beginning buffer
  n.totVec <- df$n.tot
  b <- 1
  while(n.totVec[b] == 0 & !is.na(n.totVec[b])){
    b <- b + 1
  }
  
  if((b - beg.buff) >= 0){
    beg.date <- df$batchDate[b - beg.buff]   # b is the index identifying the first day of non-zero data.
  } else {
    beg.date <- df$batchDate[1]
  }
  
  # calculate ending buffer
  n.totVec <- df3[order(df$batchDate,decreasing=TRUE),]$n.tot
  e <- 1
  while(n.totVec[e] == 0 | is.na(n.totVec[e])){
    e <- e + 1
  }
  
  if((e - end.buff) >= 0){
    end.date <- df$batchDate[nrow(df3) - e + 1 + end.buff]   # e is the first zero or NA after non-zero data
  } else {
    end.date <- df$batchDate[nrow(df3)]
  }
  
  df.small <- df[df$batchDate >= beg.date & df$batchDate <= end.date,]
  
  df.small
}  