
## Interactive Map

mapdata <- reactive({
  switch(input$agglev,
         Region = DATA_Region,
         District = DATA_District, 
         County = DATA_County,
         Subcounty = DATA_Subcounty,
         Parish = DATA,
         stop("Unknown level of aggregation"))
})


observe({
  
  if(nzchar(input$topic)) {
    ch <- cens_vars_list[[input$topic]]
    updateSelectInput(session, "color", choices = ch, 
                      selected = if(input$topic == "Population Composition") "POP" else NULL) 
  }
  
})

observe({
 
  if(nzchar(input$topic) && nzchar(input$color)) {
   ch <- cens_vars_list[[input$topic]]
   hp <- attr(ch, "HasPerc")
   if(isTRUE(hp[match(input$color, unattrib(ch))])) {
     updateRadioButtons(session, "unit", 
                        choices = c("Number", "Percent", "Number per Km2"), 
                        inline = TRUE, selected = "Percent")
   } else {
     updateRadioButtons(session, "unit", 
                        choices = c("Number", "Number per Km2"), 
                        inline = TRUE, selected = "Number")
     
   }
  }
  
})

# Create the map

output$map <- renderLeaflet({
  
  leaflet() %>% # DATA
    addProviderTiles('CartoDB.Positron', group = "CartoDB Positron") %>%
    addProviderTiles('CartoDB.DarkMatter', group = "CartoDB DarkMatter") %>%
    addTiles(group="Open Street Map", attribution = '&copy; Sebastian Krantz 2021 | Open Street Map') %>%
    addProviderTiles('OpenTopoMap', group = "Open Topo Map") %>%
    addTiles(urlTemplate = 'https://mts1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}', group="Google Maps", attribution = '&copy; Sebastian Krantz 2021 | Maps by Google') %>%
    addTiles(urlTemplate = 'https://mts1.google.com/vt/lyrs=p&x={x}&y={y}&z={z}', group="Google Terrain", attribution = '&copy; Sebastian Krantz 2021 | Maps by Google') %>%
    addTiles(urlTemplate = 'https://mts1.google.com/vt/lyrs=s,h&x={x}&y={y}&z={z}', group="Google Hybrid", attribution = '&copy; Sebastian Krantz 2021 | Maps by Google') %>%
    addTiles(urlTemplate = 'https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}', group="Google Satellite", attribution = '&copy; Sebastian Krantz 2021 | Maps by Google') %>%
    addProviderTiles('Esri.WorldStreetMap', group = "Esri World Street Map") %>% 
    addProviderTiles('Esri.WorldTopoMap', group = "Esri World Topo Map") %>% 
    addProviderTiles('Esri.WorldImagery', group = "Esri World Imagery") %>% 
    addProviderTiles('Esri.OceanBasemap', group = "Esri Ocean and Rivers") %>% 
    addProviderTiles('NASAGIBS.ViirsEarthAtNight2012', group = "NASA Nightlights 2012") %>%
    setView(lng = 32.5, lat = 1.4, zoom = 8) %>% # lng = 0, lat = 20, zoom = 3
    addScaleBar(position = "bottomright", options = scaleBarOptions(maxWidth = 150)) %>%
    addMeasure(
      position = "bottomright",
      primaryLengthUnit = "meters",
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479") %>%
    addEasyButton(easyButton(
      icon="fa-globe", title="Zoom to Level 3",
      onClick=JS("function(btn, map){ map.setZoom(3); }"))) %>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    addLayersControl(baseGroups = c("CartoDB Positron", "CartoDB DarkMatter", 
                                    "Open Street Map", "Open Topo Map",
                                    "Google Maps", "Google Terrain",
                                    "Google Hybrid", "Google Satellite", 
                                    "Esri World Street Map",
                                    "Esri World Topo Map",
                                    "Esri World Imagery",
                                    "Esri Ocean and Rivers", 
                                    "NASA Nightlights 2012"),
                     overlayGroups = "Choropleth Layer") 
  
})


# adding slider delay https://stackoverflow.com/questions/32235525/delay-on-sliderinput
input_opacity <-  debounce(reactive(input$opacity), 600)

INPUTS <- reactive({
  if(nzchar(input$topic) && nzchar(input$color)) {
  data <- mapdata()
  colvar <- if(input$unit == "Percent") paste0(input$color, "_P") else input$color
  colorData <- .subset2(data, colvar)
  if(input$unit == "Number per Km2") colorData <- colorData / .subset2(data, "AREA")
    
  CFUN <- identity
  if (length(colorData) && fndistinct(colorData) < 8L) { 
      pal <- colorFactor(input$palette, qF(colorData))
  } else {
    if(input$discbreaks) {
      if (input$logcolor) {
        pal <- colorBin(input$palette, colorData, 
                        bins = log_breaks(n = 7, base = 10)(colorData), pretty = FALSE)
      } else {
        pal <- colorBin(input$palette, colorData, bins = 7, pretty = TRUE)
      }
    } else {
      if (input$logcolor) {
        colorData <- replace_Inf(log10(colorData))
        pal <- colorNumeric(input$palette, colorData)
        CFUN <- function(x) 10^x
      } else {
        pal <- colorNumeric(input$palette, colorData)    
      }
    }
  }
  list(topic = input$topic, color = input$color, unit = input$unit, popup = genpopup(data, colvar),
       colvar = colvar, colorData = colorData, pal = pal, CFUN = CFUN, data = data, agglev = input$agglev)
  } else NULL
  
}) %>% debounce(500) # 300
  
observe({
  
 IN <- INPUTS()
 
 if(!is.null(IN)) {
 
 leafletProxy("map", data = IN$data) %>% 
   clearShapes() %>% clearControls() %>%
  addPolygons(
    popup = IN$popup,
    stroke = input$stroke,
    weight = 0.5,
    opacity = input_opacity(),
    color = "grey", # pal(colorData), # NA, # "black"
    fillColor = IN$pal(IN$colorData), # log(POP)
    fillOpacity = input_opacity(),
    smoothFactor = 0.2,  # 0.1
    noClip = TRUE,
    highlightOptions = list(
      stroke = input$stroke,
      weight = 1,
      opacity = 1,
      color = "white",
      # dashArray = "",
      fillOpacity = 1, 
      bringToFront = TRUE
    ),
    label = paste0("<b>", .subset2(IN$data, IN$agglev), "</b>: ", round(IN$CFUN(IN$colorData), 2)) %>% lapply(HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    group = "Choropleth Layer"
  ) %>%
    addLegend(pal = IN$pal, values = IN$colorData, opacity = 0.9, # input_opacity(),
              labFormat = labelFormat(transform = IN$CFUN),
              title = if(IN$topic == "Composite Indices") "Index [0 - 1]" else
                switch(IN$unit, Number = "Population", Percent = "Percent", "Number per Km2" = "Pop. per Km2", stop("Unknown unit")),
              position = "bottomleft") # , group = "Choropleth Layer"
 
 }
 
})


