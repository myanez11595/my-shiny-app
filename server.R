#   ____________________________________________________________________________
#   Server   
####
library(leaflet.providers)
library(leaflet.extras)
library(osmdata)
library(shiny)
library(leaflet)
library(sp)
library(plotly)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
#library(ggmap)
library(xts)
library(shinyjs)
library(jsonlite)
library(urltools)
library(utils)
library(rvest)
library(stringr)
library(xml2)
library(selectr)
library(purrr)
library(RColorBrewer)
library(DT)
library(shinyBS)
library(terra)
library(data.table)


shinyServer(function(input, output) {
    

  
##  ............................................................................
##  Neighborhood browser                                                    ####
    
##  ............................................................................
##  Map chart                                                               ####

    # ---------------------------
    # create color scheme for map
    # Calcular los bins para la paleta 
    #ref=unique(HEX$C_sept23)
    #bins <- c(min(ref), min(ref) + sd(ref), 
    #          min(ref) + 2*sd(ref), min(ref) + 3*sd(ref),
    #          min(ref) + 4*sd(ref), min(ref) + 5*sd(ref),
    #          min(ref) + 6*sd(ref), min(ref) + 7*sd(ref),
    #          min(ref) + 8*sd(ref), min(ref) + 9*sd(ref),
    #          min(ref) + 10*sd(ref), min(ref) + 11*sd(ref),
    #          min(ref) + 12*sd(ref), min(ref) + 13*sd(ref),
    #          min(ref) + 14*sd(ref), max(ref))
    
    # Crear la paleta de colores
    #pal <- colorBin("YlOrRd", domain = ref, bins = rev(bins),
    #                na.color = "transparent")
    palC1 <- colorNumeric(c("green","yellow", "red"), (min(HEX$C_sept23)-1):(max(HEX$C_sept23)+1))
    palC2 <- colorNumeric(c("green","yellow", "red"), (min(HEX$C_mar24)-1):(max(HEX$C_mar24)+1))
    palC3 <- colorNumeric(c("green","yellow", "red"), (min(HEX$C_jul24)-1):(max(HEX$C_jul24)+1))
    palD1 <- colorNumeric(c("green","yellow", "red"), (min(HEX$D_sept23)-1):(max(HEX$D_sept23)+1))
    palD2 <- colorNumeric(c("green","yellow", "red"), (min(HEX$D_mar24)-1):(max(HEX$D_mar24)+1))
    palD3 <- colorNumeric(c("green","yellow", "red"), (min(HEX$D_jul24)-1):(max(HEX$D_jul24)+1))
    palT1 <- colorNumeric(c("green","yellow", "red"), (min(HEX$T_sept23)-1):(max(HEX$T_sept23)+1))
    palT2 <- colorNumeric(c("green","yellow", "red"), (min(HEX$T_mar24)-1):(max(HEX$T_mar24)+1))
    palT3 <- colorNumeric(c("green","yellow", "red"), (min(HEX$T_jul24)-1):(max(HEX$T_jul24)+1))
    output$map <- renderLeaflet({
      leaflet() %>%
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palT1(HEX$T_sept23), popup = ~paste0("Terrenos : ",round(HEX$T_sept23,2),"$/m2"), group = "Terrenos sept 2023") %>%
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palT2(HEX$T_mar24), popup = ~paste0("Terrenos : ",round(HEX$T_mar24,2),"$/m2"), group = "Terrenos mar 2024") %>% 
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palT3(HEX$T_jul24), popup = ~paste0("Terrenos : ",round(HEX$T_jul24,2),"$/m2"), group = paste0("Terrenos jul 2024","<br>","Costo del m2 Casas")) %>% 
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palC1(HEX$C_sept23), popup = ~paste0("Casas : ",round(HEX$C_sept23,2),"$/m2"), group = "Casas sept 2023") %>%
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palC2(HEX$C_mar24), popup = ~paste0("Casas : ",round(HEX$C_mar24,2),"$/m2"), group = "Casas mar 2024") %>% 
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palC3(HEX$C_jul24), popup = ~paste0("Casas : ",round(HEX$C_jul24,2),"$/m2"), group = paste0("Casas jul 2024","<br>","Costo del m2 Departamentos")) %>% 
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palD1(HEX$D_sept23), popup = ~paste0("Departamentos : ",round(HEX$D_sept23,2),"$/m2"), group = "Departamentos sept 2023") %>%
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palD2(HEX$D_mar24), popup = ~paste0("Departamentos : ",round(HEX$D_mar24,2),"$/m2"), group = "Departamentos mar 2024") %>%
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palD3(HEX$D_jul24), popup = ~paste0("Departamentos : ",round(HEX$D_jul24,2),"$/m2"), group = "Departamentos jul 2024") %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
        setView(-78.5, -0.2, 11) %>% 
        addLayersControl(overlayGroups = c("Terrenos sept 2023", "Terrenos mar 2024",paste0("Terrenos jul 2024","<br>","Costo del m2 Casas"),"Casas sept 2023","Casas mar 2024",paste0("Casas jul 2024","<br>","Costo del m2 Departamentos"),"Departamentos sept 2023","Departamentos mar 2024","Departamentos jul 2024"), position="topright",options = layersControlOptions(collapsed = F)) %>% 
        hideGroup(c("Terrenos mar 2024",paste0("Terrenos jul 2024","<br>","Costo del m2 Casas"),"Casas sept 2023","Casas mar 2024",paste0("Casas jul 2024","<br>","Costo del m2 Departamentos"),"Departamentos sept 2023","Departamentos mar 2024","Departamentos jul 2024")) %>%
        htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:rigth\">Costo del m2 Terrenos</label>');
        }
        
    ") 
        #addRasterImage(DEPARTAMENTOS_tif, colors=pal, opacity = 0.6) %>%
        
    })
 
    palC4 <- colorNumeric(c("green","yellow", "red"), (min(HEX$C_jul24)-1):(max(HEX$C_jul24)+1))
  
    palD4<- colorNumeric(c("green","yellow", "red"), (min(HEX$D_jul24)-1):(max(HEX$D_jul24)+1))
  
    palT4 <- colorNumeric(c("green","yellow", "red"), (min(HEX$T_jul24)-1):(max(HEX$T_jul24)+1))
    
    palC_av <- colorNumeric(c("green","yellow", "red"), (min(HEX$c_cost[!is.na(HEX$c_cost)])-1):(max(HEX$c_cost[!is.na(HEX$c_cost)])+1))
    
    palD_av <- colorNumeric(c("green","yellow", "red"), (min(HEX$d_cost[!is.na(HEX$d_cost)])-1):(max(HEX$d_cost[!is.na(HEX$d_cost)])+1))
    
    palT_av <- colorNumeric(c("green","yellow", "red"), (min(HEX$t_cost[!is.na(HEX$t_cost)])-1):(max(HEX$t_cost[!is.na(HEX$t_cost)])+1))
    
    casas_df = reactive({
      
    HEX %>%
        filter(C_jul24 > input$casas[1] & C_jul24 < input$casas[2])
      
    })
    
    terrenos_df = reactive({
      
      HEX %>%
        filter(T_jul24 > input$terrenos[1] & T_jul24 < input$terrenos[2])
      
    })
    
    departamentos_df = reactive({
      
      HEX %>%
        filter(D_jul24 > input$departamentos[1] & D_jul24 < input$departamentos[2])
      
    })
    
    casas_avr = reactive({
      
      HEX[!is.na(HEX$c_cost),] %>%
        filter(c_cost > input$casas_av[1] & c_cost < input$casas_av[2])
      
    })
    
    terrenos_avr = reactive({
      
      HEX[!is.na(HEX$t_cost),] %>%
        filter(t_cost > input$terrenos_av[1] & t_cost < input$terrenos_av[2])
      
    })
    
    departamentos_avr = reactive({
      
      HEX[!is.na(HEX$d_cost),] %>%
        filter(d_cost > input$departamentos_av[1] & d_cost < input$departamentos_av[2])
      
    })

    setmap = leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
      setView(-78.5, -0.2, 11) %>% 
      addLayersControl(overlayGroups = c("Terrenos jul 2024", "Casas jul 2024",paste0("Departamentos jul 2024","<br>","<br>","Costo Promedio de Inmuebles"),"Terrenos promedio","Casas promedio","Departamentos promedio"), position="topright",options = layersControlOptions(collapsed = F)) %>% 
      hideGroup(c("Casas jul 2024",paste0("Departamentos jul 2024","<br>","<br>","Costo Promedio de Inmuebles"),"Casas promedio","Terrenos promedio","Departamentos promedio")) %>% 
      htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:rigth\">Costo del m2</label>');
        }
        
    ") 
    output$map2 <- renderLeaflet({
      setmap
      #addRasterImage(DEPARTAMENTOS_tif, colors=pal, opacity = 0.6) %>%
      
    })
    
    observeEvent(input$casas, {
      leafletProxy("map2") %>%
        clearShapes() %>%
        addPolygons(
          data = casas_df(), 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palC4(C_jul24), popup = ~paste0("Casas : ",round(C_jul24,2),"$/m2"), group = "Casas jul 2024")
    })
    
    observeEvent(input$terrenos, {
      leafletProxy("map2") %>%
        clearShapes() %>%
        addPolygons(
          data = terrenos_df(), 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palT4(T_jul24), popup = ~paste0("Terrenos : ",round(T_jul24,2),"$/m2"), group = "Terrenos jul 2024") 
    })
    
    observeEvent(input$departamentos, {
      leafletProxy("map2") %>%
        clearShapes() %>%
        addPolygons(
          data = departamentos_df(), 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palD4(D_jul24), popup = ~paste0("Departamentos : ",round(D_jul24,2),"$/m2"), group = paste0("Departamentos jul 2024","<br>","<br>","Costo Promedio de Inmuebles julio"))
    })
    
    observeEvent(input$casas_av, {
      leafletProxy("map2") %>%
        clearShapes() %>%
        addPolygons(
          data = casas_avr(), 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palC_av(c_cost), popup = ~paste0("Casas : ",round(c_cost,0),"$","<br>",
                                                             "Dimensión : ",c_dim, "m2","<br>",
                                                             "Cuartos : ",c_room,"<br>",
                                                             "Baños : ",c_bat), group = "Casas promedio")
      })
    
    observeEvent(input$terrenos_av, {
      leafletProxy("map2") %>%
        clearShapes() %>%
        addPolygons(
          data = terrenos_avr(), 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palT_av(t_cost), popup = ~paste0("Terrenos : ",round(t_cost,0),"$","<br>",
                                                             "Dimensión : ",t_dim, "m2"), group = "Terrenos promedio") 
    })
    
    observeEvent(input$departamentos_av, {
      leafletProxy("map2") %>%
        clearShapes() %>%
        addPolygons(
          data = departamentos_avr(), 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palD_av(d_cost), popup = ~paste0("Departamentos : ",round(d_cost,0),"$","<br>",
                                                             "Dimensión : ",d_dim, "m2","<br>",
                                                             "Cuartos : ",d_room,"<br>",
                                                             "Baños : ",d_bat), group = "Departamentos promedio")
    })
    
    output$map3 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = BORDER,color="black",weight = 2, fillOpacity = 0) %>% 
        addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
        setView(-78.5, -0.2, 11)
      
    })
    
    values <- reactiveValues(
      CTcomparisonTable = data.table::data.table(
        Opción1 = "", 
        Servicio = c("Estación de Bus", "Centros Educativos", "Atención Médica", "Botaderos de Basura", "Tiendas", "Zona Industrial", "Estaciones de Policias", "Centro de la ciudad", "Porcentaje de Gentrificación", "Porcentaje de Crecimiento Económico"), 
        Opción2 = ""
      )
    )
    table1=reactiveVal()
    table2=reactiveVal()
    observe({
      click <- input$map3_click
      if(is.null(click)) return()
      
      leafletProxy("map3") %>%
        clearMarkers() %>%
        addMarkers(lng=click$lng, lat=click$lat)
      clickPoint <- st_as_sf(data.frame(lon = click$lng, lat = click$lat), coords = c("lon", "lat"), crs = 4326)
      clickPoint=terra::vect(clickPoint)
      BUS_Value <- paste(terra::extract(BUS_STOP, clickPoint)$Layer_1, "min") 
      EDUCATION_Value <- paste(terra::extract(EDUCATION, clickPoint)$Layer_1, "min") 
      HEALTHCAR_Value <- paste(terra::extract(HEALTHCAR, clickPoint)$Layer_1, "min") 
      LANDFILL_Value <- paste(terra::extract(LANDFILL, clickPoint)$Layer_1, "min") 
      SHOPS_Value <- paste(terra::extract(SHOPS, clickPoint)$Layer_1, "min") 
      INDUSTRIES_Value <- paste(terra::extract(INDUSTRIES, clickPoint)$Layer_1, "min") 
      #UNSAFE_Value <- terra::extract(UNSAFE, clickPoint)$PLGRSD2
      POLICESTATION_Value <- paste(terra::extract(POLICESTATION, clickPoint)$Layer_1, "min") 
      DOWNTOWN_Value <- paste(terra::extract(DOWNTOWN, clickPoint)$Layer_1, "min") 
      
      #url <- paste0("https://nominatim.openstreetmap.org/reverse?format=json&lat=", click$lat, "&lon=", click$lng)
      #contenido <- readLines(url, warn = FALSE, encoding="UTF-8")
      #texto <- paste(contenido, collapse = "")
      #datos <- jsonlite::fromJSON(texto, "UTF-8")
      #direccion <- datos$display_name
     
      #setnames(CTcomparisonTable,"First_Observe",direccion)
      clickPoint <- st_as_sf(data.frame(lon = click$lng, lat = click$lat), coords = c("lon", "lat"), crs = 4326)

      data=st_intersection(HEX,clickPoint)
      
      GENTRIFICATION=paste(round(data$gentrificacion,2),"%")
      ECONOMIC_GROWTH=paste(round(data$crecimiento_economico,2),"%")
      
      values$CTcomparisonTable <- data.table::data.table(
        Opción1 = c(BUS_Value, EDUCATION_Value, HEALTHCAR_Value, LANDFILL_Value, SHOPS_Value, INDUSTRIES_Value, POLICESTATION_Value, DOWNTOWN_Value, GENTRIFICATION, ECONOMIC_GROWTH),
        Servicio = c("Estación de Bus", "Centros Educativos", "Atención Médica", "Botaderos de Basura", "Tiendas", "Zona Industrial", "Estaciones de Policias", "Centro de la ciudad", "Porcentaje de Gentrificación", "Porcentaje de Crecimiento Económico"),
        Opción2 = values$CTcomparisonTable$Opción2
      )
      
      fechas <- as.Date(c("2023-09-23", "2024-01-01", "2024-03-01", "2024-05-01", "2024-07-01"))
      # Crear una secuencia de precios correspondientes a las columnas del data frame
      precios_departamentos <- c(data$D_sept23, data$D_ene24, data$D_mar24, data$D_may24, data$D_jul24)
      precios_casas <- c(data$C_sept23, data$C_ene24, data$C_mar24, data$C_may24, data$C_jul24)
      precios_terrenos <- c(data$T_sept23, data$T_ene24, data$T_mar24, data$T_may24, data$T_jul24)
      # 
      tab=plot_ly(x = fechas) %>%
        add_lines(y = precios_departamentos, name = "Departamentos") %>%
        add_lines(y = precios_casas, name = "Casas") %>%
        add_lines(y = precios_terrenos, name = "Terrenos") %>%
        layout(title = "Precios de Propiedades",
               xaxis = list(title = "Fecha"),
               yaxis = list(title = "Precio"),
               hovermode = "closest")
      table1(tab)
    })
    
    output$map4 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = BORDER,color="black",weight = 2, fillOpacity = 0) %>% 
        addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
        setView(-78.5, -0.2, 11)
    })
    
    observe({
      click <- input$map4_click
      if(is.null(click)) return()
      
      leafletProxy("map4") %>%
        clearMarkers() %>%
        addMarkers(lng=click$lng, lat=click$lat)
      
      clickPoint <- st_as_sf(data.frame(lon = click$lng, lat = click$lat), coords = c("lon", "lat"), crs = 4326)
      clickPoint=terra::vect(clickPoint)
      BUS_Value <- paste(terra::extract(BUS_STOP, clickPoint)$Layer_1, "min") 
      EDUCATION_Value <- paste(terra::extract(EDUCATION, clickPoint)$Layer_1, "min") 
      HEALTHCAR_Value <- paste(terra::extract(HEALTHCAR, clickPoint)$Layer_1, "min") 
      LANDFILL_Value <- paste(terra::extract(LANDFILL, clickPoint)$Layer_1, "min") 
      SHOPS_Value <- paste(terra::extract(SHOPS, clickPoint)$Layer_1, "min") 
      INDUSTRIES_Value <- paste(terra::extract(INDUSTRIES, clickPoint)$Layer_1, "min") 
      #UNSAFE_Value <- terra::extract(UNSAFE, clickPoint)$PLGRSD2
      POLICESTATION_Value <- paste(terra::extract(POLICESTATION, clickPoint)$Layer_1, "min") 
      DOWNTOWN_Value <- paste(terra::extract(DOWNTOWN, clickPoint)$Layer_1, "min") 
      
      #url <- paste0("https://nominatim.openstreetmap.org/reverse?format=json&lat=", click$lat, "&lon=", click$lng)
      #contenido <- readLines(url, warn = FALSE)
      #texto <- paste(contenido, collapse = "")
      #datos <- jsonlite::fromJSON(texto)
      #direccion <- datos$display_name

      #setnames(CTcomparisonTable,"Second_Observe",direccion)
      clickPoint <- st_as_sf(data.frame(lon = click$lng, lat = click$lat), coords = c("lon", "lat"), crs = 4326)

      data=st_intersection(HEX,clickPoint)
    
      GENTRIFICATION=paste(round(data$gentrificacion,2),"%")
      ECONOMIC_GROWTH=paste(round(data$crecimiento_economico,2),"%")
      
      # Update the values in the table
      values$CTcomparisonTable <- data.table::data.table(
        Opción1 = values$CTcomparisonTable$Opción1,
        Servicio = c("Estación de Bus", "Centros Educativos", "Atención Médica", "Botaderos de Basura", "Tiendas", "Zona Industrial", "Estaciones de Policias", "Centro de la ciudad", "Porcentaje de Gentrificación", "Porcentaje de Crecimiento Económico"),
        Opción2 = c(BUS_Value, EDUCATION_Value, HEALTHCAR_Value, LANDFILL_Value, SHOPS_Value, INDUSTRIES_Value, POLICESTATION_Value, DOWNTOWN_Value, GENTRIFICATION, ECONOMIC_GROWTH)
      )
      
      fechas <- as.Date(c("2023-09-23", "2024-01-01", "2024-03-01", "2024-05-01", "2024-07-01"))
      # Crear una secuencia de precios correspondientes a las columnas del data frame
      precios_departamentos <- c(data$D_sept23, data$D_ene24, data$D_mar24, data$D_may24, data$D_jul24)
      precios_casas <- c(data$C_sept23, data$C_ene24, data$C_mar24, data$C_may24, data$C_jul24)
      precios_terrenos <- c(data$T_sept23, data$T_ene24, data$T_mar24, data$T_may24, data$T_jul24)
      # 
      tab=plot_ly(x = fechas) %>%
        add_lines(y = precios_departamentos, name = "Departamentos") %>%
        add_lines(y = precios_casas, name = "Casas") %>%
        add_lines(y = precios_terrenos, name = "Terrenos") %>%
        layout(title = "Precios de Propiedades",
               xaxis = list(title = "Fecha"),
               yaxis = list(title = "Precio"),
               hovermode = "closest")
      table2(tab)
    })
    
    output$CTcomparisonTable <- DT::renderDataTable({
      DT::datatable(values$CTcomparisonTable, options = list(pageLength = 12,columnDefs=list(
        list(width='40%', targets=1),
        list(width='20%', targets=2),
        list(width='40%', targets=3)
      )
                                                             ))
       
    })
    
    output$lineplot1 <- renderPlotly({
      table1()
    })
    output$lineplot2 <- renderPlotly({
      table2()
    })
    
#### Mapa de predicciones ####    
    
    palDEG <- colorFactor(
      palette = c("darkolivegreen1", "darkolivegreen3", "green4", "yellow", "goldenrod3", "chocolate4", "red1"),
      levels = 1:7
    )
    clasesurbanas= data.frame(clases=c("Centros urbanos","Zonas densamente pobladas","Zonas semidensamente pobladas","Zonas suburbanas","Villas","Zonas rurales dispersas","Zonas rurales muy dispersas"), id=c(7,6,5,4,3,2,1))
    output$map_pred <- renderLeaflet({
      leaflet() %>%
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palDEG(HEX$DEG2005), popup = ~clasesurbanas$clases, group = "Clasificación Urbana 2005") %>%
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palDEG(HEX$DEG2015), popup = ~clasesurbanas$clases, group = "Clasificación Urbana 2015") %>% 
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palDEG(HEX$DEG2025), popup = ~clasesurbanas$clases, group = "Clasificación Urbana 2025") %>%
        addPolygons(
          data = HEX, 
          stroke = TRUE, fillOpacity = 0.3, smoothFactor = 0.5,
          color = "black", opacity = 0.3,weight=0.2,
          fillColor = ~palDEG(HEX$DEG2035), popup = ~clasesurbanas$clases, group = "Clasificación Urbana 2035") %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
        setView(-78.5, -0.2, 11) %>% 
        leaflet::addLegend("bottomright",
                  colors = c("#FF0000", "#8B4513", "#CD950C", "#FFFF00", "#008B00", "#A2CD5A","#CAFF70"),
                  labels = c("Centros urbanos","Zonas densamente pobladas","Zonas semidensamente pobladas","Zonas suburbanas","Villas","Zonas rurales dispersas","Zonas rurales muy dispersas"),
                  title = "Urbanización",
                  opacity = 0.3) %>%
        addLayersControl(overlayGroups = c("Clasificación Urbana 2005", "Clasificación Urbana 2015","Clasificación Urbana 2025","Clasificación Urbana 2035"), position="topright",options = layersControlOptions(collapsed = F)) %>% 
        hideGroup(c("Clasificación Urbana 2015","Clasificación Urbana 2025","Clasificación Urbana 2035")) %>%
        htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:rigth\">Urbanización</label>');
        }
        
    ") 
      #addRasterImage(DEPARTAMENTOS_tif, colors=pal, opacity = 0.6) %>%
      
    })
    
})
