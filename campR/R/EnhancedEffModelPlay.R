



require(EnvCovDBpostgres)

min.date <- "2001-01-01"
max.date <- "2004-12-31"

#   ---- Convert to POSIXlt.
min.date <- as.POSIXlt(min.date,format="%Y-%m-%d",tz="America/Los_Angeles")
max.date <- as.POSIXlt(max.date,format="%Y-%m-%d",tz="America/Los_Angeles")

#   ---- Get covariate data of interest.  
df <- queryEnvCovDB("jmitchell","jmitchell","2001-01-01","2003-12-31",11,type="D",plot=TRUE)

#   ---- Fit a simple smoothing spline.  
m1 <- smooth.spline(df[!is.na(df$flow_cfs),]$date,df[!is.na(df$flow_cfs),]$flow_cfs,cv=TRUE)
m2 <- smooth.spline(df[!is.na(df$temp_c),]$date,df[!is.na(df$temp_c),]$temp_c,cv=TRUE)

#   ---- Kinda make a fake dataset with random and missing values.
p <- 0.25
nd <- seq(min.date,max.date,by="day")
nd <- nd + runif(length(nd),min=0,max=60*60*12)
nd <- nd[sample(length(nd),p*length(nd),replace=FALSE)]
nd <- nd[order(nd)]

#   ---- Based on our smoothing spline, get predicted values based on our 'catch' data.  
p1 <- predict(m1,as.numeric(nd))
p2 <- predict(m2,as.numeric(nd))

#   ---- Take a look at what we have.  
par(mfrow=c(2,1))
plot(df$date,df$flow_cfs,pch=19,cex=0.5)
#lines(m1,col="red",lwd=2)
lines(p1,col="blue",lwd=1)

plot(df$date,df$temp_c,pch=19,cex=0.5)
#lines(m2,col="red",lwd=2)
lines(p2,col="blue",lwd=1)
par(mfrow=c(1,1))
