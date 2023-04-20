
library(shiny)
library(leaflet)
library(rgdal)
library(rstac)
library(sf)
library(raster)
library(stringr)
library(maps)
library(mapdata)
library(tidyverse)
library(mgcv)
library(parallel)
library(dplyr)

bbox <- c(37.01, -3.43, 37.76, -2.82)

## Load crop mask

url_cm <- stac("https://explorer.digitalearth.africa/stac") %>%
  stac_search(collections = "crop_mask",
              bbox = bbox, datetime = "2019-01-01") %>%
  get_request() %>% assets_select(asset_names=c('mask')) %>% assets_url() 

https_cm <- "https://deafrica-services.s3.af-south-1.amazonaws.com"
url_cm <- paste0(https_cm, gsub( "s3://deafrica-services", "", url_cm))

temp_cm <- aggregate(raster(url_cm[1]), 100) %>% projectRaster(crs=crs("+proj=longlat +datum=WGS84 +no_defs"))

## Load ndvi

url_ndvi <- stac("https://explorer.digitalearth.africa/stac") %>%
  stac_search(collections = "ndvi_anomaly",
              bbox = bbox, datetime = "2019-01-01") %>%
  get_request() %>% assets_select(asset_names=c('ndvi_mean')) %>% assets_url() 

https_ndvi <- "https://deafrica-services.s3.af-south-1.amazonaws.com"
url_ndvi <- paste0(https_ndvi, gsub( "s3://deafrica-services", "", url_ndvi))

temp_ndvi <- aggregate(raster(url_ndvi[1]), 100) %>% projectRaster(crs=crs("+proj=longlat +datum=WGS84 +no_defs"))


# Mask NDVI to crop mask

cropland <- temp_cm >0.2
cropland[cropland==FALSE] <- NA
plot(cropland)
cropland <- resample(cropland, temp_ndvi)
cropland <- crop(cropland, temp_ndvi)

ndvi_masked <- mask(temp_ndvi, cropland)
plot(temp_ndvi)
plot(ndvi_masked)

# Remove layers from memory

rm(temp_cm)
rm(temp_ndvi)

# Garbage collect

gc()
