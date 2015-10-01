



require(rgdal)

projAEA  <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
projLatLong  <- "+init=epsg:4326"
dsn <- '//LAR-FILE-SRV/Data/BTPD/Product/WY/Rough Draft June 1 2015'


# make google earth shapefiles for lyman


# all towns

StShp <- readOGR(dsn,"Interim_WY_Clipped_Towns") 
WyTownsLatLon <- spTransform(StShp,CRS(projLatLong))
setwd(paste0(dsn,"/"))
writeOGR(WyTownsLatLon,'WyTownsLatLon.KML','WyTownsLatLon',driver="KML",overwrite_layer=TRUE) # save kml file 

# carissa-special towns

CarShp <- readOGR(dsn,"carissaSample")
CarissaTownsLatLon <- spTransform(CarShp,CRS(projLatLong))
setwd(paste0(dsn,"/"))
writeOGR(CarissaTownsLatLon,'CarissaTownsLatLon.KML','CarissaTownsLatLon',driver="KML",overwrite_layer=TRUE) # save kml file 