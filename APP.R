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

# get sp data from file
pgsp = rgdal::readOGR('ne_10m_admin_0_countries.shp', encoding = 'utf-8')

ui <- fluidPage(
  tabsetPanel(type = "tabs",
              tabPanel("Map",
                       fluidRow(
                         column(2,
                                verticalLayout(
                                  selectInput('providerName', NULL, choices = providers),
                                  selectInput('countries', NULL, choices = sort(as.character(pgsp@data$ADMIN) %>% unique))
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
    mapdata <- subset(pgsp, ADMIN == input$countries)
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
                    bbox = bbox, datetime = "2019-01-01") %>%
        get_request() %>% assets_select(asset_names=c('mask')) %>% assets_url() 
      
      https <- "https://deafrica-services.s3.af-south-1.amazonaws.com"
      url_cm <- paste0(https, gsub( "s3://deafrica-services", "", url_cm))
      url_cm
    }
    
    crop_to_sp <- function(tif_cm){
      temp_cm <- aggregate(raster(tif_cm), 300) %>% projectRaster(crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
      cropland <- temp_cm >0.2
      cropland[cropland==FALSE] <- NA
      crop_sp <- rasterToPolygons(cropland)
      rm(temp_cm)
      rm(cropland)
      gc()
      crop_sp
    }
    
    ## mask
    filter <- function(tif_ndvi){try({
      temp_ndvi <- aggregate(raster(tif_ndvi), 100) %>% projectRaster(crs=crs("+proj=longlat +datum=WGS84 +no_defs"))

      cropland <- crop(crop_sp, temp_ndvi)
      
      ndvi_masked <- mask(temp_ndvi, cropland)
      names(ndvi_masked) <- names(temp_ndvi)
      #plot(temp_ndvi)
      #plot(ndvi_masked)
      
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
    
    cl <- makeCluster(getOption("cl.cores", 12))
    clusterEvalQ(cl, expr = library(rstac))
    clusterEvalQ(cl, expr = library(raster))
    clusterEvalQ(cl, expr = library(stringr))
    clusterEvalQ(cl, expr = library(dplyr))
    
    bbox <- list(rgn$bbox)
    print(bbox)
    tif_ndvi <- parLapply(cl,bbox,list_ndvi)[[1]]
    tif_cm <- parLapply(cl,bbox,list_cm)[[1]]
    print(tif_ndvi)
    
    crop_sp <- parLapply(cl,tif_cm,crop_to_sp)
    crop_sp <- do.call(rbind, crop_sp)
    rm(tif_cm)
    #plot(crop_sp)
    ndvi_masked <- parLapply(cl,tif_ndvi,filter)
    #print(ndvi_masked)
    df_list <- parLapply(cl, ndvi_masked, raster_to_df)
    print(df_list)
    
    df_filter <- df_list
    for (i in length(df_list):1)
      if (class(df_list[[i]]) == 'try-error')
        df_filter <- df_filter[-i]
    
    rm(df_list)
    df <- bind_rows(df_filter)
    #print(df)
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
  })
}

shinyApp(ui, server)
