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

s_obj <- stac("https://explorer.digitalearth.africa/stac")

bbox <- c(37.26, -3.16, 37.42, -2.97)
bbox_sp <- as(extent(37.26, 37.42, -3.16, -2.97), 'SpatialPolygons')
crs(bbox_sp) <- "+proj=longlat +datum=WGS84 +no_defs"



url <- stac("https://explorer.digitalearth.africa/stac") %>%
  stac_search(collections = "ndvi_anomaly",
              bbox = bbox, datetime = "2019-01-01/2022-12-31") %>%
  get_request() %>% items_fetch() %>% assets_select(asset_names=c('ndvi_mean')) %>% assets_url()

https <- "https://deafrica-services.s3.af-south-1.amazonaws.com"
url <- paste0(https, gsub( "s3://deafrica-services", "", url))

star_time <- Sys.time()

raster_to_df <- function(tif){try({
  temp <- aggregate(raster(tif), 100, fun = 'mean') %>% projectRaster(crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
  date <- names(temp)
  year <- as.numeric(str_sub(date, 23,26))
  month <- as.numeric(str_sub(date, 28,29))
  temp <- as.data.frame(temp,xy=TRUE,na.rm=TRUE)
  colnames(temp) <- c("x","y","ndvi")
  temp[,'year'] <- year
  temp[,'month'] <- month
  temp})
  }

#df_list <- list()
cl <- makeCluster(getOption("cl.cores", 12))
clusterEvalQ(cl, expr = library(raster))
clusterEvalQ(cl, expr = library(stringr))
clusterEvalQ(cl, expr = library(dplyr))

df_list <- parLapply(cl, url, raster_to_df)

df_filter <- df_list
for (i in length(df_list):1)
  if (class(df_list[[i]]) == 'try-error')
    df_filter <- df_filter[-i]

rm(df_list)
df <- bind_rows(df_filter)
stopCluster(cl)

end_time <- Sys.time()

run_time <- end_time - star_time
print(run_time)

head(df)
tail(df)
names(df)

f <- ndvi ~ 
  s(year, bs = "cr", k = 4) +
  s(month, bs = "cc", k=12) +
  s(x,y, bs = "gp", k = 50) +
  ti(month, year, bs = c("cc", "cr"), k = c(12,4)) +
  ti(x, y, year, d = c(2, 1), bs = c("gp", "cr"),
     k  = c(50, 4), m=list(2,NA))

gam_ex <- bam(f, data = df, discrete = TRUE, nthreads = 8, rho=0.8)

plot(gam_ex)
summary(gam_ex)
