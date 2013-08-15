F.plot.lifestages <- function( df, file.root, plot.pies=FALSE ){
#
#   Produce a plot of the lifestages.
#
#   df = a table with lifestages down the rows and runs across the columns.
#   plot.pies = If TRUE, pie charts are produced
#

#   Get passage columns and totals row indices
pass.cols <- grep( "passage", names(df) )
tot.row <- grep( "Total", df$LifeStage )

#   Get totals
tots <- df[ tot.row, pass.cols ]

#   Pare down the data frame to just passage columns, no totals
df2 <- df[ -tot.row, pass.cols ]

#   drop the runs with 0 fish. 
df2 <- df2[ , tots > 0 ]

#   drop the lifestages with no fish
tots <- apply( df2, 1, sum )
df2 <- df2[ tots > 0, ]

if( nrow(df2) == 0 ){
    cat("F.plot.lifestages - ALL ZEROS\n")
    return("ZEROS")    
}

ls.names <- rownames(df2)
ls.names[ ls.names == "YOY (young of the year)" ] <- "YOY"
ls.names[ ls.names == "Not recorded" ] <- "Missing"
run.names <- strsplit(names(df2), ".", fixed=T)
run.names <- unlist(lapply( run.names, function(x){x[1]} ))
#run.names[ run.names == "Late fall"] <- "Late\nFall"

if( plot.pies ){

    file.list <- NULL
    for( j in 1:ncol(df2) ){

        #   If file=NA, a pdf graphing device is assumed to be open already.
        if( !is.na(file.root) ){
            #   Shut down all graphics devices
            graphics.off()
        
            #   ---- Open PNG device
            out.pass.graphs <- paste(file.root, "_", run.names[j], ".png", sep="")
            if(file.exists(out.pass.graphs)){
                file.remove(out.pass.graphs)
            }
            tryCatch({png(file=out.pass.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(file=out.pass.graphs)})  # produces hi-res graphs unless there's an error, then uses default png settings
            file.list <- c(file.list, out.pass.graphs)
        }

        y <- df2[,j]
        y <- y / sum(y)
        gt0 <- y > 0
        y <- y[gt0]
        labs <- ls.names[gt0]
 
        
        pie( y, labs, main=run.names[j] )
    
    
        if( !is.na(file.root) ){
            dev.off(dev.cur())
        }
    }
    
}  else {
        #   If file=NA, a pdf graphing device is assumed to be open already.
        file.list <- NULL
        if( !is.na(file.root) ){
            #   Shut down all graphics devices
            graphics.off()
        
            #   ---- Open PNG device
            out.pass.graphs <- paste(file.root, "_lifestage_barchart.png", sep="")
            if(file.exists(out.pass.graphs)){
                file.remove(out.pass.graphs)
            }
            tryCatch({png(file=out.pass.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(file=out.pass.graphs)})  # produces hi-res graphs unless there's an error, then uses default png settings
            file.list <- c(file.list, out.pass.graphs)
        }


        n.plots <- ncol(df2)
    
        layout.mat <- matrix(1,n.plots+1,2)
        layout.wid <- c(1,20)
        layout.hgt <- c(rep(3,n.plots),2)
        layout.mat[,2]<-2:(n.plots+2)
        layout(layout.mat, widths=layout.wid, heights=layout.hgt)

        #   Y axis label in margin
        par(mar=c(0,0,0,0))
        plot(c(0,1),c(0,1),type="n",xlab="",ylab="",xaxt="n",yaxt="n", bty="n")
        text(.65,.55,"Proportion of Catch", cex=2, srt=90)
         
        #   the barplots 
        for( j in 1:n.plots ){
            y <- df2[,j]
            y <- y / sum(y)
            par(mar=c(0,4.1,1.5,2.1))

            tmp<-barplot( y, ylim=c(0,1.35), xaxt="n", col="orange", yaxt="n" )
            axis( 2, at=c(0,.25,.5,.75,1) )
            text( max(tmp)+.4*(tmp[2]-tmp[1]), 1.15, run.names[j], adj=1, cex=2)
            
            
            n <- formatC(round(df2[1,j]),big.mark=",",digits=8)
            n <- gsub(" ", "", n)
            axis( 1, at=tmp[1], labels=paste("n=",n,sep=""), tick=F, line=-1 )
            for( i in 2:length(tmp)){
                n <- formatC(round(df2[i,j]),big.mark=",",digits=8)
                n <- gsub(" ", "", n)
                axis( 1, at=tmp[i], labels=n, tick=F, line=-1 )
            }
        }
        

        #   Write out the ls.names along the bottom
        par(mar=c(0,4.1,1,2.1))
        tmp<-barplot( y, ylim=c(0,1), xaxt="n", yaxt="n", col=0, border=0 )
        for( j in 1:length(tmp)){
            text( tmp[j], .95, ls.names[j], srt=90, adj=1, cex=1.75 )
        }


        if( !is.na(file.root) ){
            dev.off(dev.cur())
        }

}

file.list

}
