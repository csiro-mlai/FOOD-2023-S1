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
library(ggplot2)
library(visreg)
library(gridExtra)
library(grid)
library(mapview)
library(leafpm)
library(shinythemes)

# get sp data from file
pgsp = ne_countries(continent="africa")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-notification {
        font-size: 24px;
        text-align: center;
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        padding: 20px;
        border-radius: 5px;
        z-index: 1051;
      }
    "))
  ),
  theme = shinytheme("slate"),
  
  tagList(
    img(src = "logo.png", style = "display: block; margin-left: 0; margin-right: auto; width: 240px; height: 100px;"),
    
    div(
      id = "carousel-example",
      class = "carousel slide",
      `data-ride` = "carousel",
      tags$ol(
        class = "carousel-indicators",
        tags$li(`data-target` = "#carousel-example", `data-slide-to` = "0", class = "active"),
        tags$li(`data-target` = "#carousel-example", `data-slide-to` = "1"),
        tags$li(`data-target` = "#carousel-example", `data-slide-to` = "2")
      ),
      tags$div(
        class = "carousel-inner",
        role = "listbox",
        tags$div(
          class = "item active",
          img(src = "1.jpg", alt = "First slide", style = "width: 1200px; height: 436px; display: block; margin-left: auto; margin-right: auto;")
        ),
        tags$div(
          class = "item",
          img(src = "2.jpg", alt = "Second slide", style = "width: 1200px; height: 436px; display: block; margin-left: auto; margin-right: auto;")
        ),
        tags$div(
          class = "item",
          img(src = "3.jpg", alt = "Third slide", style = "width: 1200px; height: 436px; display: block; margin-left: auto; margin-right: auto;")
        )
      ),
      tags$a(class = "left carousel-control", `data-slide` = "prev", href = "#carousel-example", role = "button",
             tags$span(class = "glyphicon glyphicon-chevron-left", `aria-hidden` = "true"),
             tags$span(class = "sr-only", "Previous")
      ),
      tags$a(class = "right carousel-control", `data-slide` = "next", href = "#carousel-example", role = "button",
             tags$span(class = "glyphicon glyphicon-chevron-right", `aria-hidden` = "true"),
             tags$span(class = "sr-only", "Next")
      )
    ),
    navbarPage(NULL,
               tabPanel("MainPage",  
                        h1("Welcome to the Main Page!"),
                        p("This application aims to analyze the NDVI trends in a specific area. With this application, 
                          you can select any region or country in Africa and the system will fit its trend with its average NDVI data over the last four years and present it in various formats."),
                        h3("The application contains the following main functions:"),
                        p("1. Two modes of regional selection: free choice of a rectangular region and a specific African country."),
                        p("2. Intuitive map display with automatic jumping to the selected area."),
                        p("3. Four outputs (reflecting trends in NDVI data with different variables) and a brief description of the outputs."),
                        h1("Terminology explanation"),
                        h3("NDVI"),
                        p("The normalized difference vegetation index (NDVI) is a simple graphical indicator that can be used to analyze remote sensing measurements, often from a space platform, 
                          assessing whether or not the target being observed contains live green vegetation"),
                        h3("Mean NDVI"),
                        p("Digital Earth Africa’s Mean NDVI and Anomalies service provides a monthly estimate of vegetation health, and how this compares to the long-term average conditions. 
                          This is determined by measuring how different the vegetation health is each month compared to the long-term mean for that month, measured in standard deviations. 
                          The service is designed to give users insight into how much the current vegetation conditions have changed relative to historic conditions. It can show improvement, maintenance or deterioration of the vegetation at each 30 x 30 m pixel across the continent. 
                          The long-term baseline condition is measured based on the period from 1984 to 2020 and is available through the NDVI Climatology layer."),
                        p("If the NDVI anomaly is positive this indicates that the vegetation is greener than the average conditions, usually suggesting an increased rainfall in a region. A negative anomaly indicates that the vegetation may be stressed due to drier conditions. 
                          The anomaly can also indicate the extent, intensity, and impact of drought conditions when compared to the long-term mean. An abrupt and significant negative anomaly can occur following a fire. Examining the anomaly against the NDVI Climatology can confirm the impact of weather, 
                          such as rainfall patterns, on the areas observed."),
                        h1("For use and instructions, please click Map")
               ),
               
               tabPanel("Map",
                        fluidRow(
                          column(2,
                                 verticalLayout(
                                   selectInput('providerName', NULL, choices = providers),
                                   selectInput('countries', NULL, choices = sort(as.character(pgsp@data$admin) %>% unique)),
                                   radioButtons("operation", label = "Choose an operation:",
                                                choices = c("The whole country", "Free Selection"), selected = "Addition"),
                                   tags$h4("Instruction for use"),
                                   tags$p("1. Select mode（Free or Whole）"),
                                   tags$p("2. Crop the region of interest or select the country name"),
                                   tags$p("3. Click on the Dashboard"),
                                   tags$p("4. Waiting for results to be generated"),
                                   tags$p("Please do not choose an area that is too small and not covered with vegetation. Choose an agricultural area of moderate size if possible")
                                 )
                          ),
                          column(10, leafletOutput("map", height = 800))
                        ),
                        textOutput("boundary"),
               ),
               tabPanel("Dashboard",
                        br(),
                        span(style = "font-weight: 100; font-size: 16px; width: 100%;
            color: white;", "This progress could take a little while to load. The loading 
                symbol further down the page shows that something is happening!"),
                        br(),
                        shinycssloaders::withSpinner(
                          plotOutput("plot_1", height = 1400, width = 800)
                        ),
                        div(style = "font-weight: 100; font-size: 16px; width: 100%; color: white;", 
                            HTML("These two images separately display the annual and monthly changes of the Normalized Difference Vegetation Index (NDVI) over time. They show the variation trend of the vegetation index within a certain period. In these images, the horizontal axis represents the year and the month, respectively, while the vertical axis represents the Normalized Difference Vegetation Index.")
                        ),
                        shinycssloaders::withSpinner(
                          plotOutput("plot_2", height = 800, width = 800)
                        ),
                        div(style = "font-weight: 100; font-size: 16px; width: 100%; color: white;", 
                            HTML("This image shows the distribution of the spatial average Normalized Difference Vegetation Index. It displays the distribution of the vegetation index within a given region. In this image, the horizontal axis represents longitude, while the vertical axis represents latitude.")
                        ),
                        shinycssloaders::withSpinner(
                          plotOutput("plot_3", height = 1000, width = 1000)
                        ),
                        div(style = "font-weight: 100; font-size: 16px; width: 100%; color: white;", 
                            HTML("This image presents the spatial vegetation index distribution for each year. It is a composite image composed of multiple subplots, where each subplot displays the vegetation index distribution for a specific year. In this image, the horizontal axis represents longitude, and the vertical axis represents latitude.")
                        )
               ),
               
    )
  )
)

server <- function(input, output, session) {
  # create a reactive variable
  rgn <- reactiveValues()
  
  output$map <- renderLeaflet({
    mapview()@map %>% 
      addPmToolbar(drawOptions = pmDrawOptions(allowSelfIntersection = FALSE),
                   toolbarOptions=pmToolbarOptions(
        drawMarker = FALSE,
        drawPolygon = FALSE,
        drawPolyline = FALSE,
        drawCircle = FALSE,
        drawRectangle = TRUE)) %>% 
      setView(0.0, 25, zoom = 5)
  })
  
  observeEvent(c(input$num1, input$num2, input$operation), {
    if (input$operation == "The whole country") {
      observe({
        mapdata <- subset(pgsp, admin == input$countries)
        rgn$bbox <- mapdata@bbox %>% as.vector() # use $ to modify the reactive value
        leafletProxy("map", session) %>% clearShapes() %>%
          addProviderTiles(input$providerName, layerId = "tiles") %>%
          flyToBounds(rgn$bbox[1], rgn$bbox[2], rgn$bbox[3], rgn$bbox[4]) %>%
          addPolygons(data = mapdata, fill = T, fillColor = 'gold', color = 'red')
      })
    } 
    else {
      observeEvent(input$map_draw_new_feature, {
        bbox <- input$map_draw_new_feature$geometry$coordinates
        bbox <- as.numeric(c(bbox[[1]][[1]][[1]],bbox[[1]][[1]][[2]],bbox[[1]][[3]][[1]],bbox[[1]][[3]][[2]]))
        rgn$bbox <- bbox
      })
    }
    })
    
  # use rgn as a reactive value in a separate reactive context
  output$boundary <- renderText({
    # get the current value of rgn
    bbox <- rgn$bbox
    # return a string with the bounding box coordinates
    paste("The bounding box is:", paste(bbox, collapse = ", "))
  })
  
  output$plot_1 <- renderPlot({  
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
    
    crop_to_sf <- function(tif_cm){
      temp_cm <- aggregate(raster(tif_cm), res) %>% projectRaster(crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
      assign("temp_cm", temp_cm, envir = globalenv())
      cropland <- temp_cm > 0.2
      cropland[cropland==FALSE] <- NA
      #print(cropland)
      
      if (!all(is.na(cropland[]))) {
        crop_sp <- rasterToPolygons(cropland, dissolve = TRUE)
        crop_sf <- st_as_sf(crop_sp)
        rm(temp_cm)
        rm(cropland)
        rm(crop_sp)
        gc()
        print(crop_sf)
        #print(st_crs(crop_sf))
        # Transform CRS if necessary
        if (!is.na(st_crs(crop_sf))) {
          print("return")
          return(crop_sf)
        } else {
          print("delete")
          rm(crop_sf)
          gc()
          NULL
        }
        
      } else {
        rm(temp_cm)
        rm(cropland)
        gc()
        NULL
      }

    }
    
    ## mask
    filter <- function(tif_ndvi){try({
      temp_ndvi <- aggregate(raster(tif_ndvi), res) %>% projectRaster(crs=crs("+proj=longlat +datum=WGS84 +no_defs"))
      
      #cropland <- crop(crop_sp, temp_ndvi)
      # crs1 <- st_crs(temp_ndvi)
      # crs2 <- st_crs(crop_sf)
      # temp_ndvi <- st_transform(temp_ndvi, crs2)
      
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
    
    bbox <- list(rgn$bbox)
    tif_cm <- lapply(bbox,list_cm)[[1]]
    print(tif_cm)
    
    len_cm <- length(tif_cm)
    if (len_cm >= 1 & len_cm <= 5) {
      res <- 300
    } else if (len_cm > 5 & len_cm <= 10) {
      res <- 800
    } else if (len_cm > 10 & len_cm <= 15) {
      res <- 1400
    } else if (len_cm > 15 & len_cm <= 20) {
      res <- 1900
    } else if (len_cm > 20 & len_cm <= 25) {
      res <- 2500
    } else {
      res <- 3000
    }
    print(res)
    crop_sf <- lapply(tif_cm,crop_to_sf)
    crop_sf <- Filter(Negate(is.null), crop_sf)
    crop_sf <- do.call(rbind, crop_sf)
    assign("crop_sf", crop_sf, envir = globalenv())
    print(crop_sf)
    #plot(crop_sf)
    
    cl <- makeCluster(getOption("cl.cores", 13))
    clusterEvalQ(cl, expr = library(rstac))
    clusterEvalQ(cl, expr = library(raster))
    clusterEvalQ(cl, expr = library(stringr))
    clusterEvalQ(cl, expr = library(dplyr))
    clusterEvalQ(cl, expr = library(sf))
    clusterExport(cl, "crop_sf")
    clusterExport(cl, "res")
    
    bbox <- list(rgn$bbox)
    print(bbox)
    tif_ndvi <- parLapply(cl,bbox,list_ndvi)[[1]]
    gc()
    
    ndvi_masked <- parLapply(cl,tif_ndvi,filter)
    #rm(crop_sf)
    df_list <- parLapply(cl, ndvi_masked, raster_to_df)
    
    df_filter <- df_list
    for (i in length(df_list):1)
      if (class(df_list[[i]]) == 'try-error')
        df_filter <- df_filter[-i]
    
    rm(df_list)
    gc()
    df <- bind_rows(df_filter)
    #print(length(df))
    
    ## Random sampling
    
    n_row <- nrow(df) %/% 4
    sample_rows <- sample(seq_len(nrow(df)), size = n_row)
    df <- df[sample_rows, ]
    
    print(df)
    
    stopCluster(cl)
    
    f <- ndvi ~
      s(year, bs = "cr", k = 4) +
      s(month, bs = "cc", k=12) +
      s(x,y, bs = "gp", k = 50) +
      ti(month, year, bs = c("cc", "cr"), k = c(12,4)) +
      ti(x, y, year, d = c(2, 1), bs = c("gp", "cr"),
         k  = c(50, 4), m=list(2,NA))
    
    # gam_ex <- bam(f, data = df, discrete = TRUE, nthreads = 12, rho=0.8)
    
    fit_gam <- function(f, data) {
      tryCatch(
        {
          gam_ex <- bam(f, data = df, discrete = TRUE, nthreads = 12, rho = 0.8)
          return(gam_ex)
        },
        error = function(e) {
          showNotification("Error: Insufficient amount of data to fit the model. Please select a larger area, thanks!", 
                           duration = NULL, 
                           closeButton = TRUE,
                           type = "error")
          return(NULL)
        }
      )
    }
    gam_ex <- fit_gam(f, df)
    
    end_time <- Sys.time()
    
    run_time <- end_time - star_time
    
    print(run_time)
    
    plot1 <- visreg(gam_ex, "year", gg=TRUE) + theme(axis.text = element_text(size = 16)) + ggtitle("Yearly Changes")
    plot2 <- visreg(gam_ex, "month", gg=TRUE) + theme(axis.text = element_text(size = 16)) + ggtitle("Monthly Changes")
    plot3 <- visreg2d(gam_ex, "x", "y", plot.type="gg") + theme(axis.text = element_text(size = 16)) + ggtitle("Spatial average")
    assign("plot3", plot3, envir = globalenv())
    
    years <- unique(df$year)
    
    # Create a list to store each plot
    plot_list <- list()
    
    # Loop through each year and create a separate plot
    for (i in seq_along(years)) {
      
      # Subset the data for the current year
      df_year <- df[df$year == years[i], ]
      
      # Plot the visreg2d for the current year and add it to the plot list
      plot_list[[i]] <- visreg2d(gam_ex, "x", "y",
                                 cond = list(year = years[i]),
                                 main = paste0("Year: ", years[i]),plot.type="gg",heights = c(5, 5, 10, 10)) + theme(axis.text = element_text(size = 16)) + ggtitle(years[i])
    }
    
    plot4 <- grid.arrange(grobs = plot_list, ncol = 2)
    assign("plot4", plot4, envir = globalenv())
    
    grid.arrange(plot1,plot2)
  })
  
  output$plot_2 <- renderPlot({
    grid.draw(plot3)
  })
  
  output$plot_3 <- renderPlot({
    grid.draw(plot4)
  })
  
}

shinyApp(ui, server)
