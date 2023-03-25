#install.packages('rstac')

library(rstac)
library(sf)
library(raster)
library(stringr)
library(maps)
library(mapdata)

s_obj <- stac("https://explorer.digitalearth.africa/stac")

bbox <- c(37.26, -3.16, 37.42, -2.97)
bbox_sp <- as(extent(37.26, 37.42, -3.16, -2.97), 'SpatialPolygons')
crs(bbox_sp) <- "+proj=longlat +datum=WGS84 +no_defs"



url <- stac("https://explorer.digitalearth.africa/stac") %>%
  stac_search(collections = "ndvi_anomaly",
              bbox = bbox, datetime = "2023-01-01/2023-12-31", limit=40) %>%
  get_request() %>% assets_select(asset_names=c('ndvi_mean')) %>% assets_url() 

https <- "https://deafrica-services.s3.af-south-1.amazonaws.com"
url <- paste0(https, gsub( "s3://deafrica-services", "", url))

df_list <- list()
for (i in 1:length(url)){
  temp <- raster(url[i])
  temp <- projectRaster(temp,crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  date <- names(temp)
  date <- as.numeric(str_sub(date, 23,29))
  
  temp <- as.data.frame(temp,xy=TRUE,na.rm=TRUE)
  colnames(temp) <- c("x","y","ndvi")
  temp[,'date'] = date
  df_list[[i]] <- temp
}

print(df_list[1])

# ras = raster(url[1])
# ras <- projectRaster(ras,crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
# 
# date <- names(ras)
# 
# date <- str_sub(date, 23,29)
# date <- as.numeric(date)
# 
# df <- as.data.frame(ras,xy=TRUE,na.rm=TRUE)
# colnames(df) <- c("x","y","ndvi")
# df[,'date'] = date


bbox_sp <- spTransform(bbox_sp, crs("+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
b <- crop(raster(url[1]), bbox_sp)
plot(b)

a <- crop(raster(url[2]), bbox_sp)
plot(a)

c <- merge(a,b)
plot(c)
plot(b)
c



