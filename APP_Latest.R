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
library(rnaturalearth)
library(rnaturalearthdata)

# get sp data from file
pgsp = pgsp = ne_countries(continent="africa")

ui <- fluidPage(
  tabsetPanel(type = "tabs",
              tabPanel("Map",
                       fluidRow(
                         column(2,
                                verticalLayout(
                                  selectInput('providerName', NULL, choices = providers),
                                  selectInput('countries', NULL, choices = sort(as.character(pgsp@data$admin) %>% unique))
                                )
                         ),
                         column(10, leafletOutput("map", height = 900))
                       ),
                       textOutput("boundary"),
              ),
              tabPanel("Dashboard",
                       br(),
                       span(style = "font-weight: 100; font-size: 16px; width: 100%;
               color: black;", "This progress could take a little while to load. The loading 
                   symbol further down the page shows that something is happening!"),
                       br(),
                       shinycssloaders::withSpinner(
                         plotOutput("plot", height=1400, width = 800))
              )
  )
)

server <- function(input, output, session) {
  # create a reactive variable for rgn
  rgn <- reactiveValues()
  
  output$map <- renderLeaflet({
    leaflet() #%>% amap()
  })
  
  observe({
    mapdata <- subset(pgsp, admin == input$countries)
    rgn$bbox <- mapdata@bbox %>% as.vector() # use $ to modify the reactive value
    leafletProxy("map", session) %>% clearShapes() %>%
      addProviderTiles(input$providerName, layerId = "tiles") %>%
      flyToBounds(rgn$bbox[1], rgn$bbox[2], rgn$bbox[3], rgn$bbox[4]) %>%
      addPolygons(data = mapdata, fill = T, fillColor = 'gold', color = 'red')
  })
  
  # use rgn as a reactive value in a separate reactive context
  output$boundary <- renderText({
    # get the current value of rgn
    bbox <- rgn$bbox
    # return a string with the bounding box coordinates
    paste("The bounding box is:", paste(bbox, collapse = ", "))
  })
  
  output$plot <- renderPlot({  
    star_time <- Sys.time()
    
    # Load ndvi data
    list_ndvi <- function(bbox){
      url_ndvi <- stac("https://explorer.digitalearth.africa/stac") %>%
        stac_search(collections = "ndvi_anomaly",
                    bbox = bbox, datetime = "2019-01-01/2022-12-31") %>%
        get_request() %>% items_fetch() %>% assets_select(asset_names=c('ndvi_mean')) %>% assets_url()
      
      https <- "https://deafrica-services.s3.af-south-1.amazonaws.com"
      url_ndvi <- paste0(https, gsub( "s3://deafrica-services", "", url_ndvi))
      url_ndvi
    }
    
    ## Load crop mask
    list_cm <- function(bbox){
      url_cm <- stac("https://explorer.digitalearth.africa/stac") %>%
        stac_search(collections = "crop_mask",
                    bbox = bbox, datetime = "2019-01-01",limit=2) %>%
        get_request() %>% assets_select(asset_names=c('mask')) %>% assets_url() 
      
      https <- "https://deafrica-services.s3.af-south-1.amazonaws.com"
      url_cm <- paste0(https, gsub( "s3://deafrica-services", "", url_cm))
      url_cm
    }
    
    crop_to_sf <- function(tif_cm){
      temp_cm <- aggregate(raster(tif_cm), 1000) %>% projectRaster(crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
      cropland <- temp_cm >0.2
      cropland[cropland==FALSE] <- NA
      crop_sp <- rasterToPolygons(cropland,dissolve = TRUE)
      crop_sf <- st_as_sf(crop_sp)
      rm(temp_cm)
      rm(cropland)
      rm(crop_sp)
      gc()
      crop_sf
    }
    
    ## mask
    filter <- function(tif_ndvi){try({
      temp_ndvi <- aggregate(raster(tif_ndvi), 300) %>% projectRaster(crs=crs("+proj=longlat +datum=WGS84 +no_defs"))

      #cropland <- crop(crop_sp, temp_ndvi)
      
      ndvi_masked <- mask(temp_ndvi, crop_sf)
      names(ndvi_masked) <- names(temp_ndvi)
      
      # Remove layers from memory
      rm(temp_cm)
      rm(temp_ndvi)
      gc()
      
      ndvi_masked})
    }
    
    raster_to_df <- function(ndvi_masked){try({
      date <- names(ndvi_masked)
      year <- as.numeric(str_sub(date, 23,26))
      month <- as.numeric(str_sub(date, 28,29))
      temp <- as.data.frame(ndvi_masked,xy=TRUE,na.rm=TRUE)
      colnames(temp) <- c("x","y","ndvi")
      temp[,'year'] <- year
      temp[,'month'] <- month
      temp})
    }
    
    cl <- makeCluster(getOption("cl.cores", 13))
    clusterEvalQ(cl, expr = library(rstac))
    clusterEvalQ(cl, expr = library(raster))
    clusterEvalQ(cl, expr = library(stringr))
    clusterEvalQ(cl, expr = library(dplyr))
    clusterEvalQ(cl, expr = library(sf))
    
    bbox <- list(rgn$bbox)
    print(bbox)
    tif_ndvi <- parLapply(cl,bbox,list_ndvi)[[1]]
    tif_cm <- parLapply(cl,bbox,list_cm)[[1]]
    print(tif_cm)
    
    crop_sf <- parLapply(cl,tif_cm,crop_to_sf)
    crop_sf <- do.call(rbind, crop_sf)
    print(crop_sf)
    rm(tif_cm)
    gc()
    
    #plot(crop_sp)
    assign("crop_sf", crop_sf, envir = globalenv())
    clusterExport(cl, "crop_sf")
    ndvi_masked <- parLapply(cl,tif_ndvi,filter)
    #rm(crop_sf)
    #gc()
    #print(ndvi_masked)
    df_list <- parLapply(cl, ndvi_masked, raster_to_df)
    #print(df_list)
    
    df_filter <- df_list
    for (i in length(df_list):1)
      if (class(df_list[[i]]) == 'try-error')
        df_filter <- df_filter[-i]
    
    rm(df_list)
    gc()
    df <- bind_rows(df_filter)
    #print(length(df))
    
    ## Random sampling
    
    # n_row <- nrow(df) %/% 2
    # sample_rows <- sample(seq_len(nrow(df)), size = n_row)  
    # df <- df[sample_rows, ]  
    
    #print(df)
    
    stopCluster(cl)
    
    
    f <- ndvi ~
      s(year, bs = "cr", k = 4) +
      s(month, bs = "cc", k=12) +
      s(x,y, bs = "gp", k = 50) +
      ti(month, year, bs = c("cc", "cr"), k = c(12,4)) +
      ti(x, y, year, d = c(2, 1), bs = c("gp", "cr"),
         k  = c(50, 4), m=list(2,NA))
    
    gam_ex <- bam(f, data = df, discrete = TRUE, nthreads = 12, rho=0.8)
    
    end_time <- Sys.time()
    
    run_time <- end_time - star_time
    
    print(run_time)
    
    
    plot(gam_ex)
    summary(gam_ex)
  })
}

shinyApp(ui, server)
