library(shiny)
library(shinybusy)
library(crosstalk) #
#source(broker)

# Define server logic required to draw a histogram
function(input, output, session) {
  broker<-getBroker()
  
  
  
  # Tables box ----
  # TODO: unfix data
  exampleTibble <- tibble::tibble(
    source = NA,
    url = NA,
    title = NA,
    resources = NA
  )
  
  # status (Reactive Values "object" to store current status)
  status <- reactiveValues(
    selectedSite = '',
    selectedEv = NULL,
    selectedSiteInfo = NULL
  )
  
  # TODO: unfix data
  datasets <- reactiveValues(
    # per ora lascio esempi. Poi sono da mettere tutti a NULL
    tblEVsData = exampleTibble,
    tblOtherResData = exampleTibble,
    tblOtherRepoData = exampleTibble
  )
  
  # reactive to store the actual data selected by the user in each "datasets" list
  theDataset<-reactiveValues(
    datasetEv = NULL,
    datasetRes = NULL,
    datasetOther = NULL
  )
  
  theSelectedTab <- reactive({
    tabs<-c("EVs","OtherRes","OtherRepo")
    res<-which(tabs==input$resultsTabContainer)
    if(length(res)>0)
      return(res)
    return(0)
  })
  
  # return a list
  currentDatasetMetadata <- reactive({
    seltab<-theSelectedTab()
    if(seltab==1)
      return(theDataset$datasetEv$metadata)
    if(seltab==2)
      return(theDataset$datasetRes$metadata)
    # if(seltab==4)
    #   return(theDataset$datasetOther)
  })
  
  currentDataset <- reactive({
    # according to the selected tab, the current dataset is one of the three slots
    # of the RV theDataset. The following expression returns the right one.
    
    seltab<-theSelectedTab()
    if(seltab==1)
      return(theDataset$datasetEv$dataset)
    if(seltab==2)
      return(theDataset$datasetRes$dataset)
    if(seltab==4)
      return(theDataset$datasetOther)
    #nameDatasetForTheSelectedTab<-names(theDataset)[theSelectedTab()]
    #theDataset[[nameDatasetForTheSelectedTab]]
  })
  
  # show/hide elements according to reactive condition
  observe({
    # TODO: check if it is currently possible to hide the tab box
    # 
    # if(is.null(currentDataset()))
    #   hideTab(inputId = "datasetLists", target = "table", session = session)
    # else
    #   showTab(inputId = "datasetLists", target = "table", session = session)
    
    shinyjs::toggle(id = "saveDataset", condition = !is.null(currentDataset()))
    updateTextInput(inputId = "saveCurrentDatasetName",
                    value = fname<-stringr::str_replace_all(currentDatasetMetadata()$datasetname, " ", "_")
    )
  })
  
  observeEvent(input$saveCurrentDataset,{
    message("saving dataset")
    res<-saveDataset(datasetfilename = input$saveCurrentDatasetName,
                dataset = currentDataset(),
                metadataList = currentDatasetMetadata()
                )
    session$sendCustomMessage(type = 'testmessage',
                              message = res)
  })
  
  
  currentDatasetType<-reactive({
    # here we examine the dataset type and the geometry type
    getDatasetObjectTechInfo(currentDataset())
  })
  
  # TODO: add a slider to control time for rasterTS
  rasterTSLayer<-reactive({
    return(1)
  })
  
  # SHOW METADATA
  output$selectedDatasetMD<-DT::renderDataTable({
    DT::datatable(data = currentDatasetMetadata() %>% 
                    dplyr::as_tibble() %>% 
                    t(),
      escape = FALSE,
      selection = "single",
      caption = htmltools::tags$caption(
        style = 'text-align: center;',
        htmltools::h3(paste0(
          'Dataset metadata'
        ))
      ),
      filter = 'top'
    )
  })
  # -- START REMOVE THIS AFTER DEVELOPMENT
  output$debug <- renderText({
    #currentDataset()
    #print(currentDatasetType())
    #result <- jsonlite::prettify(jsonlite::toJSON(currentDatasetMetadata(), 
     #                                             auto_unbox = TRUE), 4)
    #result<-x$metadata %>% dplyr::as_tibble() %>% t()
    #result <- jqr::jq(jsonlite::toJSON(currentDatasetMetadata(), auto_unbox = TRUE))
    
    #return(result)
    # browser()
    
    # cat(paste("selected tab is #", input$resultsTabContainer))
    # 
    # cat('<br>\nRows on the current current EVsData table:\n\n')
    # cat(input$tableEVsData_rows_current, sep = ', ')
    # cat('\n\nAll rows:\n\n')
    # cat(input$tableEVsData_rows_all, sep = ', ')
    # cat('\n\nSelected rows:\n\n')
    # cat(input$tableEVsData_rows_selected, sep = ', ')
    # 
    # cat('\nRows on the current current EVsData table:\n\n')
    # cat(input$tableOtherResData_rows_current, sep = ', ')
    # cat('\n\nAll rows:\n\n')
    # cat(input$tableOtherResData_rows_all, sep = ', ')
    # cat('\n\nSelected rows:\n\n')
    #print(input$tableOtherResData_rows_selected)
    # 
    # cat('\nRows on the current current EVsData table:\n\n')
    # cat(input$tableOtherRepoData_rows_current, sep = ', ')
    # cat('\n\nAll rows:\n\n')
    # cat(input$tableOtherRepoData_rows_all, sep = ', ')
    # cat('\n\nSelected rows:\n\n')
    # cat(input$tableOtherRepoData_rows_selected, sep = ', ')
    
    # cat('<br>user selected EV dataset of class')
    # cat(class(theDataset$datasetEV ))
    # 
    # cat('<br>user selected Other res dataset of class')
    # cat(class(theDataset$datasetRes ))
  })
  # -- END REMOVE THIS AFTER DEVELOPMENT
  
  observeEvent(input$site, {
    message("ev site$ev:", input$site)
    status$selectedSite <- input$site
  })
  
  observeEvent(input$ev, {
    message("ev input$ev:", input$ev)
    status$selectedEv <- input$ev
  })
  
  observeEvent(status$selectedSite, {
    # TODO: check if broker can be used as a reactive to avoid duplication of selection status
    broker$setSite(status$selectedSite)
    status$selectedEv=NULL#broker$EVsList()[1]
    
    
    
    # reset theDataset slots
    theDataset$datasetEv = NULL
    theDataset$datasetRes = NULL
    theDataset$datasetOther = NULL
    
    # reset tbl selection
    # tpr<-DT::dataTableProxy("tableOtherRepoData",session = session)
    DT::selectRows(DT::dataTableProxy("tableEVsData",session = session), NULL)
    DT::selectRows(DT::dataTableProxy("tableOtherResData",session = session), NULL)
    DT::selectRows(DT::dataTableProxy("tableOtherRepoData",session = session), NULL)
    
    
    freezeReactiveValue(input, "ev")
    updateSelectizeInput(
      session,
      "ev",
    choices = broker$EVsList(),
      server = TRUE,
      selected = NULL
    )
    # updateSelectizeInput(
    #   session,
    #   "ev",
    #   choices = broker$EVsList(),
    #   server = TRUE,
    #   selected = "<select one>"
    # )
    
    shinybusy::show_modal_spinner(text="fetching site data")
    #withProgress(message="fetching data", {
    x<-broker$getInfo_Site() # first call requires some time.
    # TODO: add a "progress" to notify this is loading
    #})
    shinybusy::remove_modal_spinner()
    # load and present site info in panel
    output$siteinfo <- renderUI(tagList(
      a(x$val_title, href=x$val_uri, target="_blank"),
      div(renderTable(x$tbl_generalInfo %>% dplyr::as_tibble() %>% 
                        dplyr::select(geoBonBiome, biogeographicalRegion) %>% 
                        tidyr::unnest(cols=c("geoBonBiome"))))#,
      # p("Yearly avg precipitation: ", x$val_precipitation, "[", units::deparse_unit(x$val_precipitation), "]"),
      # p("Biome", x$val_geoBonBiome),
      # p("Biogeographical Region:", x$val_biogeographicalRegion),
      # div("habitats within the site", class="scroll", renderTable(x$tbl_eunisHabitats, striped = T, colnames = F))
    ))
    status$selectedSiteInfo<-x
    
    shinybusy::show_modal_spinner(text="fetching data")
    datasets$tblOtherResData <- broker$getOtherResData()
    datasets$tblOtherRepoData <- broker$getOtherRepoData()
    shinybusy::remove_modal_spinner()
    
  })
  
  observeEvent(status$selectedEv,{
    req(status$selectedEv)
    
    broker$setEv(status$selectedEv)
    message(status$selectedEv)
    
    # load and present ev info in panel
    shinybusy::show_modal_spinner(text="fetching EVs data")
    #withProgress(message="fetching data", {
    x<-broker$getEv()
    datasets$tblEVsData <- broker$getEVsData()
    # datasets$tblOtherResData <- broker$getOtherResData()
    # datasets$tblOtherRepoData <- broker$getOtherRepoData()
    #})
    shinybusy::remove_modal_spinner()
    #browser()
    output$EVinfo <- renderUI(tagList(
      a(x$name, href=x$webpage),
      renderTable(x %>% dplyr::select(type, domain, uom), rownames = TRUE),
      p(x$description)
    ))
  })
  
  # # set the numbers in infoboxes
  # observeEvent(datasets$tblOtherRepoData, {
  #   
  # })
  
  output$info_box_EVsData <- renderUI({
    infoBox(
      "Concerning selected EVs",
      paste0(nrow(datasets$tblEVsData), " dataset(s)"),
      icon = icon("table"),
      color = "olive"
    )
  })
  
  output$info_box_OtherResData <- renderUI({
    infoBox(
      "Relating to the selected site (structured)",
      paste0(nrow(datasets$tblOtherResData), " dataset(s)"),
      icon = icon("table"),
      color = "olive"
    )
  })
  
  output$info_box_OtherRepoData <- renderUI({
    infoBox(
      "Relating to the selected site (unstructured)",
      paste0(nrow(datasets$tblOtherRepoData), " dataset(s)"),
      icon = icon("table"),
      color = "olive"
    )
  })
  
  output$tableEVsData <- DT::renderDataTable({
    DT::datatable(
      datasets$tblEVsData,
      escape = FALSE,
      selection = "single",
      caption = htmltools::tags$caption(
        style = 'text-align: center;',
        htmltools::h3(paste0(
          'Contains resources related directly with selected EV and site'
        ))
      ),
      filter = 'top'
    )
  })
  
  output$tableOtherResData <- DT::renderDataTable({
    DT::datatable(
      datasets$tblOtherResData,
      escape = FALSE,
      selection = "single",
      caption = htmltools::tags$caption(
        style = 'text-align: center;',
        htmltools::h3(paste0(
          'Includes resources related to the selected site, collected from domain-specific repositories (e.g. GBIF, iNaturalist, OBIS)'
        ))
      ),
      filter = 'top'
    )
  })
  
  output$tableOtherRepoData <- DT::renderDataTable({
    DT::datatable(
      datasets$tblOtherRepoData,
      escape = FALSE,
      selection = "single",
      caption = htmltools::tags$caption(
        style = 'text-align: center;',
        htmltools::h3(paste0(
          'Includes resources associated with the selected site, collected from non-structured or general-purpose repositories (e.g. DEIMS-SDR, Pangaea, Zenodo)'
        ))
      ),
      filter = 'top'
    )
  })
  
  # events on result lists (dataset selection)
  observeEvent(input$tableEVsData_cell_clicked, {
    shinybusy::show_modal_spinner(text="loading dataset...please wait")
    info <- input$tableEVsData_cell_clicked
    selectedRows <- input$tableEVsData_rows_selected
    message("selected row is", info$row)
    if(any(selectedRows>0) && ! is.null(info) && !is.null(info$row) && info$row > 0 && info$row<=3 ){
      #getEVsDatasetResultList() %>% .[row_id,]
      dataset <- broker$getActualDataset_EVrelated(info$row)
      theDataset$datasetEv <- dataset
    } else {
      theDataset$datasetEv <- NULL
    }
    shinybusy::remove_modal_spinner()

  })

  observeEvent(input$tableOtherResData_cell_clicked, {
    info <- input$tableOtherResData_cell_clicked
    proxy<-DT::dataTableProxy("tableOtherResData")
    selectedRows <- input$tableOtherResData_rows_selected
    
    message("selected row is", info$row)
    if(any(selectedRows>0) && ! is.null(info) && !is.null(info$row) && info$row > 0 && info$row<=3 ){
      # per assegnare al RV che contiene i dataset correntemente selezionati dall'utente nelle 3 tabelle di risultato
      # theDataset$datasetRes<-broker$getActualDataset_OtherRes(info$row)
      # altrimenti prendere al volo dalla broker e assegnare a una variabile locale a questo observe event.
      # come segue:
      dataset <- broker$getActualDataset_OtherRes(info$row)
      message("assign datasetRes")
      theDataset$datasetRes <- dataset
    } else {
      theDataset$datasetRes <- NULL
    }
    
    #

  })
  
  observeEvent(input$tableOtherRepoData_cell_clicked, {
    # at the moment we do not do anything here.
    # we can consider downloading something e.g. for pangaea, using the panagaear functions
    
    #info = input$tableOtherRepoData_cell_clicked$row
    # info is a list(row = row_index, col = column_index, value = cell_value)
    # broker$
    #theDataset$datasetOther <- dataset
    #
    #
  })
  
  
  
  
  # Visualization box ----
  # TODO: unfix data
  # if(F){
  # chla <- ReLTER::get_sos_obs(
  #   sosURL = "http://getit.lteritalia.it/observations/service",
  #   procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72097",
  #   foi = c("http://www.get-it.it/sensors/getit.lteritalia.it/sensors/foi/SSF/SP/4326/45.9547/8.63403"),
  #   show_map = FALSE
  # ) %>% dplyr::slice(1:55)
  # }
  # shared_data <- SharedData$new(
  #   sf::st_as_sf(
  #     chla,
  #     coords = c("lat", "lon"),
  #     crs = 4326
  #   )
  # )
  
  # TODO: adapt also to EV datasets
  output$map <- leaflet::renderLeaflet({
    
    
    # chla_map <- chla %>%
    #   dplyr::select(foiLabel:lat) %>%
    #   unique() %>%
    #   sf::st_as_sf(coords = c("lat", "lon"), crs = 4326)
    
    l<-leaflet::leaflet() %>%
      leaflet::addProviderTiles(
        "CartoDB.Positron",
        options = leaflet::providerTileOptions(opacity = 0.99)
      )
    
    # add site polygon as polylines
    l <- l %>% leaflet::addPolylines(data=status$selectedSiteInfo$geometry)
    
    # switch leaflet method on sf + type (points etc) , raster + type (single or mmultiple layers)
    typ<-currentDatasetType()
    
    if("vector" %in% typ$datasetType){
      
      # warning(typ$vectorType)
      
      if(typ$vectorType == "POINT")
        l <- l %>% leaflet::addMarkers(data = currentDataset())
      if(typ$vectorType == "POLYGON")
        l <- l %>% leaflet::addPolygons(data=currentDataset())
    }
    if("raster" %in% typ$datasetType){
      l <- l %>% leaflet::addRasterImage(x=currentDataset(), opacity=.8)
    }
    if("rasterTS" %in% typ$datasetType){
      l <- l %>% leaflet::addRasterImage(x=currentDataset()[[rasterTSLayer()]], opacity=.8)
    }
    l
    
  })
  
  # TODO: adapt also to EV datasets
  #attributes(chla)$uri
  output$tbl <- DT::renderDT({
    res=NULL
    
    if("data.frame" %in% currentDatasetType()$datasetType){
      res<-DT::datatable(
              data = currentDataset(),
              escape = FALSE,
              filter = 'top',
              options = list(scrollX = TRUE)
            )
    }
    return(res)
    # chla |>
    #   dplyr::select(!(foiLabel:lat)) |>
    #   DT::datatable(
    #     escape = FALSE,
    #     filter = 'top',
    #     options = list(scrollX = TRUE),
    #     colnames = colnames(chla %>% dplyr::select(!(foiLabel:lat))) %>% lapply(FUN=function(x){
    #       if("units" %in% class(chla[[x]])) {
    #         n = which(names(chla) == x)
    #         sprintf(
    #           "<a title='%s - %s' href='%s' target='_blank'>%s</a>",
    #           units::deparse_unit(chla[[x]]),
    #           attributes(chla)$uri[[n]],
    #           attributes(chla)$uri[[n]],
    #           x
    #         )
    #       } else x
    #     })
    #   )
  }, server = FALSE)
  
  # TODO: unfix data
  # output$plot <- plotly::renderPlotly({
  #   chla |>
  #     dplyr::select(!(foiLabel:lat)) |>
  #     plotly::plot_ly(
  #       type = 'scatter',
  #       mode = 'lines'
  #     ) %>%
  #     plotly::add_trace(
  #       y = ~Maximum_depth_below_surface_of_the_water_body,
  #       x = ~chla_fluorescence_component,
  #       name = paste0('Chlorophyll a [', units::deparse_unit(chla$chla_fluorescence_component), "]")
  #     ) %>%
  #     plotly::add_trace(
  #       y = ~Maximum_depth_below_surface_of_the_water_body,
  #       x = ~green_algae_chla_fluorescence_component,
  #       name = paste0('Green algae [', units::deparse_unit(chla$green_algae_chla_fluorescence_component), "]")
  #     ) %>%
  #     plotly::add_trace(
  #       y = ~Maximum_depth_below_surface_of_the_water_body,
  #       x = ~bluegreen_algae_chla_fluorescence_component,
  #       name = paste0('Blue-Green algae [', units::deparse_unit(chla$bluegreen_algae_chla_fluorescence_component), "]")
  #     ) %>%
  #     plotly::add_trace(
  #       y = ~Maximum_depth_below_surface_of_the_water_body,
  #       x = ~diatom_algae_chla_fluorescence_component,
  #       name = paste0('Diatom algae [', units::deparse_unit(chla$diatom_algae_chla_fluorescence_component), "]")
  #     ) %>%
  #     plotly::add_trace(
  #       y = ~Maximum_depth_below_surface_of_the_water_body,
  #       x = ~crypto_algae_chla_fluorescence_component,
  #       name = paste0('Crypto algae [', units::deparse_unit(chla$crypto_algae_chla_fluorescence_component), "]")
  #     ) %>%
  #     plotly::add_trace(
  #       y = ~Maximum_depth_below_surface_of_the_water_body,
  #       x = ~Concentration_of_organic_matter_in_water_bodies,
  #       name = 'Organic matter'
  #     ) %>%
  #     plotly::add_trace(
  #       y = ~Maximum_depth_below_surface_of_the_water_body,
  #       x = ~water_temp,
  #       xaxis = "x2",
  #       name = paste0('Water temperature [', units::deparse_unit(chla$water_temp), "]")
  #     ) %>%
  #     plotly::layout(
  #       yaxis = list(
  #         autorange = "reversed",
  #         title = paste0(
  #           'Maximum depth below surface of the water body [',
  #           units::deparse_unit(chla$Maximum_depth_below_surface_of_the_water_body),
  #           ']'
  #         )
  #       ),
  #       xaxis = list(
  #         title = paste0('Chla fluorescence component [', units::deparse_unit(chla$chla_fluorescence_component), ']')
  #       ),
  #       xaxis2 = list(
  #         overlaying = "x",
  #         anchor = "y",
  #         side = "top",
  #         showticklabels = TRUE,
  #         title = paste0('temperature [', units::deparse_unit(chla$water_temp), ']')
  #       )
  #     )
  # })
}
