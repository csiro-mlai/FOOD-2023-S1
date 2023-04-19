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

bbox <- c(37.26, -3.16, 37.42, -2.97)

url <- stac("https://explorer.digitalearth.africa/stac") %>%
  stac_search(collections = "crop_mask",
             bbox = bbox, datetime = "2019-01-01") %>%
  get_request() %>% assets_select(asset_names=c('mask')) %>% assets_url() 

https <- "https://deafrica-services.s3.af-south-1.amazonaws.com"
url <- paste0(https, gsub( "s3://deafrica-services", "", url))

temp <- aggregate(raster(url[1]), 100) %>% projectRaster(crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
plot(temp)
print(values(temp))

cropland <- temp > 0.6
print(values(cropland))
plot(cropland)
