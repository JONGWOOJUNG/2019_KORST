#190725

getwd()
setwd("C:/Users/jongu/OneDrive/바탕 화면/190725")


source("base.R")
source("subway.R")
source("zone_loc.R")
source("building.R")

## Global vars to Analysis :
## main_df <- data.frame() # main passage data
## weekday_df <- data.frame() # weekday passage data (subset of main)
## weekend_df <- data.fram() # weekend passage data (subset of main)
# zones_df <- data.frame() # zones and locataion of zones (X, Y)
# zones_loc <- SpatialPointsDataFrame() # Projected(EPSG:5186, Korea Central 2010) zones
# zones_noloc <- data.frame() # zones that have no location
# rest_bikes <- data.frame() # rest bike real-time data
# rest_bikes_q <- data.frame() # rest_bikes with coordinates
# zones_subway_1ndist <- data.frame() # Distance of nearest subway station

# Read and bind data as one dataframe
ddar_2018 <- read.csv("서울특별시 공공자전거 대여이력 정보_2018년_1분기1.csv")
ddar_2018t <- read.csv("서울특별시 공공자전거 대여이력 정보_2018년_2분기_1.csv")
ddar_2018 <- rbind(ddar_2018, ddar_2018t)
ddar_2018t <- read.csv("서울특별시 공공자전거 대여이력 정보_2018년_2분기_2.csv")
ddar_2018 <- rbind(ddar_2018, ddar_2018t)
ddar_2018t <- read.csv("서울특별시 공공자전거 대여이력 정보_2018년_2분기_3.csv")
ddar_2018 <- rbind(ddar_2018, ddar_2018t)
ddar_2018t <- read.csv("서울특별시 공공자전거 대여이력 정보_2018년_2분기_4.csv")
ddar_2018 <- rbind(ddar_2018, ddar_2018t)

rm(ddar_2018t)

## Convert to data.frame and make easy to use
main_df <- as.data.frame(ddar_2018)
colnames(main_df) <- c("BIKE_NUM", "DEP_TIME", "DEP_ZONE_CODE", "DEP_ZONE_NAME", "DEP_HOLDER_NUM", "ARR_TIME", "ARR_ZONE_CODE", "ARR_ZONE_NAME", "ARR_HOLDER_NUM", "USE_MINUTES", "USE_METERS")
main_df$DEP_TIME <- strptime(main_df$DEP_TIME, "'%Y-%m-%d %H:%M:%S'")
main_df$ARR_TIME <- strptime(main_df$ARR_TIME, "'%Y-%m-%d %H:%M:%S'")
main_df$DEP_ZONE_CODE <- gsub("'", "", main_df$DEP_ZONE_CODE)
main_df$ARR_ZONE_CODE <- gsub("'", "", main_df$ARR_ZONE_CODE)
rm(ddar_2018)
main_df <- refine_df(main_df)
 
# summary(main_df)
# head(main_df, 10)

## make copy and split main data by Weekday/Weekend
weekend_df <- subset(main_df, (format(main_df$DEP_TIME, "%w") == 0 | format(main_df$DEP_TIME, "%w") == 1))
weekday_df <- subset(main_df, (format(main_df$DEP_TIME, "%w") != 0 & format(main_df$DEP_TIME, "%w") != 1))

#####################################################################################################################################

# ## Rest Bike (from realtime data)
# rest_bikes <- read.csv("./data/rest_bikes.csv", fileEncoding = 'euc-kr')
# rest_bikes$CODE <- as.character(rest_bikes$CODE)
# 
# #rest_bikes <- left_join(rest_bikes, zones_df[c('CODE', 'X', 'Y')], by = 'CODE') # merge with location
# rest_bikes$TIME <- strptime(rest_bikes$TIME, "%Y-%m-%d %H-%M")
# rest_bikes_q <- subset(rest_bikes, rest_bikes$X > 35 & rest_bikes$Y > 125)
# rest_bikes_q <- subset(rest_bikes_q, as.numeric(format(rest_bikes_q[,"TIME"], "%M")) == 0)


## zone locations
# Open zone locataion file and merge to zone
library(dplyr)
zones_loc <- read.csv("zones_loc.csv")
zones_loc$CODE <- as.character(zones_loc$CODE)
zones <- extract_zones(main_df)
zones_loc <- left_join(zones, zones_loc[c('CODE', 'X', 'Y')], by = 'CODE')
zones_noloc <- zones_loc[is.na(zones_loc$X),]
zones_loc <- zones_loc[!zones_loc$CODE %in% zones_noloc$CODE, ]
zones_noloc <- rbind(zones_noloc, zones_loc[zones_loc$X < 126 | zones_loc$Y < 36, ])
zones_loc <- zones_loc[!zones_loc$CODE %in% zones_noloc$CODE, ]
rm(zones)
zones_df <- zones_loc
zones_df$CODE <- as.character(zones_df$CODE)
coordinates(zones_loc) <- c("X", "Y")
proj4string(zones_loc)  <- CRS("+init=epsg:4326")

#library(rgdal)
#library(leaflet.extras)
seoul_shp = readOGR("TL_SCCO_SIG.shp")
seoul_center <- c(mean(seoul_shp@bbox[1,]), mean(seoul_shp@bbox[2,]))

## Subway
### READ DATA
library(readr)

read.csv.any <- function(text, sep = "", ...) {
  encoding <- as.character(guess_encoding(text)[1,1])
  setting <- as.character(tools::file_ext(text))
  if(sep != "" | !(setting  %in% c("csv", "txt")) ) setting <- "custom"
  separate <- list(csv = ",", txt = "\n", custom = sep)
  result <- read.table(text, sep = separate[[setting]], fileEncoding = encoding, ...)
  return(result)
}

subway_df <- read.csv.any("subway_loc.csv", header = TRUE)
subway_df <- subway_df[c(1, 2, 8, 9)]
colnames(subway_df) <- c("SUB_CODE", "SUB_NAME", "X", "Y")
subway_loc <- subway_df
subway_loc <- subway_loc[subway_loc$X > 36 & !is.na(subway_loc$X),]
coordinates(subway_loc) <- c("X", "Y")

zones_subway_1ndist <- read.csv.any("subway_da.csv", header = TRUE)
colnames(zones_subway_1ndist) <- c("CODE", "SUB_CODE", "DISTANCE")
zones_subway_1ndist <- zones_subway_1ndist[!duplicated(zones_subway_1ndist$CODE),]