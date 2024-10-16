library(shiny)
library(shinybusy)
library(crosstalk) #
#source(broker)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # status (Reactive Values "object" to store current status)
  status <- reactiveValues(
    selectedSite = '',
    selectedEv = NULL,
    site_info = NULL
  )
  
  # Tables box ----
  # TODO: unfix data
  exampleTibble <- tibble::tibble(
    source = NA,
    url = NA,
    title = NA,
    resources = NA
  )
  
  # TODO: unfix data
  datasets <- reactiveValues(
    # per ora lascio esempi. Poi sono da mettere tutti a NULL
    tblEVsData = exampleTibble,
    tblOtherResData = exampleTibble,
    tblOtherRepoData = exampleTibble
  )
  
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
    
    freezeReactiveValue(input, "ev")
    updateSelectizeInput(
      session,
      "ev",
      choices = NULL,
      server = TRUE,
      selected = NULL
    )
    updateSelectizeInput(
      session,
      "ev",
      choices = broker$EVsList(),
      server = TRUE,
      selected = ""
    )
    
    shinybusy::show_modal_spinner(text="fetching data")
    #withProgress(message="fetching data", {
    x<-broker$getInfo_Site() # first call requires some time.
    # TODO: add a "progress" to notify this is loading
    #})
    shinybusy::remove_modal_spinner()
    # load and present site info in panel
    output$siteinfo <- renderUI(tagList(
      a(x$val_title, href=x$val_uri, target="_blank"),
      div(renderTable(t(x$tbl_generalInfo %>% dplyr::as_data_frame())))#,
      # p("Yearly avg precipitation: ", x$val_precipitation, "[", units::deparse_unit(x$val_precipitation), "]"),
      # p("Biome", x$val_geoBonBiome),
      # p("Biogeographical Region:", x$val_biogeographicalRegion),
      # div("habitats within the site", class="scroll", renderTable(x$tbl_eunisHabitats, striped = T, colnames = F))
    ))
    
  })
  
  observeEvent(status$selectedEv,{
    req(status$selectedEv)
    
    broker$setEv(status$selectedEv)
    message(status$selectedEv)
    
    # load and present ev info in panel
    shinybusy::show_modal_spinner(text="fetching data")
    #withProgress(message="fetching data", {
    x<-broker$getEv()
    datasets$tblEVsData <- broker$getEVsData()
    datasets$tblOtherResData <- broker$getOtherResData()
    datasets$tblOtherRepoData <- broker$getOtherRepoData()
    #})
    shinybusy::remove_modal_spinner()
    #browser()
    output$EVinfo <- renderUI(tagList(
      a(x$name, href=x$webpage),
      renderTable(x %>% dplyr::select(type, domain, uom), rownames = TRUE),
      p(x$description)
    ))
  })
  
  # set the numbers in infoboxes
  observeEvent(datasets$tblOtherRepoData, {
    
  })
  
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
      caption = htmltools::tags$caption(
        style = 'text-align: center;',
        htmltools::h3(paste0(
          'Contains resources related with selected EV and site'
        ))
      ),
      filter = 'top',
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          "copy",
          list(
            extend = "collection",
            text = 'Use selected dataset in my VRE session',
            action = DT::JS("function ( e, dt, node, config ) {
                                    alert( 'Button activated' );
                                }")
          )
        )
      )
    )
  })
  
  output$tableOtherResData <- DT::renderDataTable({
    DT::datatable(
      datasets$tblOtherResData,
      escape = FALSE,
      caption = htmltools::tags$caption(
        style = 'text-align: center;',
        htmltools::h3(paste0(
          'Contains resources related with selected site and collected from GBIF, iNaturalist, and OBIS'
        ))
      ),
      filter = 'top',
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          # "copy",
          list(
            extend = "collection",
            text = 'Use selected dataset in my VRE session',
            action = DT::JS("function ( e, dt, node, config ) {
                                    alert( 'Button activated' );
                                }")
          )
        )
      )
    )
  })
  
  output$tableOtherRepoData <- DT::renderDataTable({
    DT::datatable(
      datasets$tblOtherRepoData,
      escape = FALSE,
      caption = htmltools::tags$caption(
        style = 'text-align: center;',
        htmltools::h3(paste0(
          'Contains resources related with selected site and collected from DEIMS-SDR, Pangaea, and Zenodo'
        ))
      ),
      filter = 'top',
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          # "copy",
          list(
            extend = "collection",
            text = 'Use selected dataset in my VRE session',
            action = DT::JS("function ( e, dt, node, config ) {
                                    alert( 'Button activated' );
                                }")
          )
        )
      )
    )
  })
  
  # -- START REMOVE THIS AFTER DEVELOPMENT
  output$debug <- renderPrint({
    cat('\nRows on the current current EVsData table:\n\n')
    cat(input$tableEVsData_rows_current, sep = ', ')
    cat('\n\nAll rows:\n\n')
    cat(input$tableEVsData_rows_all, sep = ', ')
    cat('\n\nSelected rows:\n\n')
    cat(input$tableEVsData_rows_selected, sep = ', ')
    
    cat('\nRows on the current current EVsData table:\n\n')
    cat(input$tableOtherResData_rows_current, sep = ', ')
    cat('\n\nAll rows:\n\n')
    cat(input$tableOtherResData_rows_all, sep = ', ')
    cat('\n\nSelected rows:\n\n')
    cat(input$tableOtherResData_rows_selected, sep = ', ')
    
    cat('\nRows on the current current EVsData table:\n\n')
    cat(input$tableOtherRepoData_rows_current, sep = ', ')
    cat('\n\nAll rows:\n\n')
    cat(input$tableOtherRepoData_rows_all, sep = ', ')
    cat('\n\nSelected rows:\n\n')
    cat(input$tableOtherRepoData_rows_selected, sep = ', ')
  })
  # -- END REMOVE THIS AFTER DEVELOPMENT
  
  
  # Visualization box ----
  # TODO: unfix data
  chla <- ReLTER::get_sos_obs(
    sosURL = "http://getit.lteritalia.it/observations/service",
    procedure = "http://www.get-it.it/sensors/getit.lteritalia.it/procedure/noOwnerDeclared/noModelDeclared/noSerialNumberDeclared/1286194C-A5DF-11DF-8ED7-1602DFD72097",
    foi = c("http://www.get-it.it/sensors/getit.lteritalia.it/sensors/foi/SSF/SP/4326/45.9547/8.63403"),
    show_map = FALSE
  ) %>% dplyr::slice(1:55)
  
  # shared_data <- SharedData$new(
  #   sf::st_as_sf(
  #     chla,
  #     coords = c("lat", "lon"),
  #     crs = 4326
  #   )
  # )
  
  # TODO: unfix data
  output$map <- leaflet::renderLeaflet({
    chla_map <- chla %>%
      dplyr::select(foiLabel:lat) %>%
      unique() %>%
      sf::st_as_sf(coords = c("lat", "lon"), crs = 4326)
    
    leaflet::leaflet() %>%
      leaflet::addProviderTiles(
        "CartoDB.Positron",
        options = leaflet::providerTileOptions(opacity = 0.99)
      ) %>%
      leaflet::addMarkers(
        data = chla_map,
        popup = paste0(
          "<b>Sensor name: </b>",
          "<br>",
          "<a href='",
          chla_map$procedureID,
          "' target='_blank'>",
          chla_map$procedureName,
          "</a>",
          "<br>",
          "<b>Sensor coordinates: </b>",
          "<br>",
          chla_map$geometry
        )
      )
  })
  
  # TODO: unfix data
  attributes(chla)$uri
  output$tbl <- DT::renderDT({
    chla |>
      dplyr::select(!(foiLabel:lat)) |>
      DT::datatable(
        escape = FALSE,
        filter = 'top',
        options = list(scrollX = TRUE),
        colnames = colnames(chla %>% dplyr::select(!(foiLabel:lat))) %>% lapply(FUN=function(x){
          if("units" %in% class(chla[[x]])) {
            n = which(names(chla) == x)
            sprintf(
              "<a title='%s - %s' href='%s' target='_blank'>%s</a>",
              units::deparse_unit(chla[[x]]),
              attributes(chla)$uri[[n]],
              attributes(chla)$uri[[n]],
              x
            )
          } else x
        })
      )
  }, server = FALSE)
  
  # TODO: unfix data
  output$plot <- plotly::renderPlotly({
    chla |>
      dplyr::select(!(foiLabel:lat)) |>
      plotly::plot_ly(
        type = 'scatter',
        mode = 'lines'
      ) %>%
      plotly::add_trace(
        y = ~Maximum_depth_below_surface_of_the_water_body,
        x = ~chla_fluorescence_component,
        name = paste0('Chlorophyll a [', units::deparse_unit(chla$chla_fluorescence_component), "]")
      ) %>%
      plotly::add_trace(
        y = ~Maximum_depth_below_surface_of_the_water_body,
        x = ~green_algae_chla_fluorescence_component,
        name = paste0('Green algae [', units::deparse_unit(chla$green_algae_chla_fluorescence_component), "]")
      ) %>%
      plotly::add_trace(
        y = ~Maximum_depth_below_surface_of_the_water_body,
        x = ~bluegreen_algae_chla_fluorescence_component,
        name = paste0('Blue-Green algae [', units::deparse_unit(chla$bluegreen_algae_chla_fluorescence_component), "]")
      ) %>%
      plotly::add_trace(
        y = ~Maximum_depth_below_surface_of_the_water_body,
        x = ~diatom_algae_chla_fluorescence_component,
        name = paste0('Diatom algae [', units::deparse_unit(chla$diatom_algae_chla_fluorescence_component), "]")
      ) %>%
      plotly::add_trace(
        y = ~Maximum_depth_below_surface_of_the_water_body,
        x = ~crypto_algae_chla_fluorescence_component,
        name = paste0('Crypto algae [', units::deparse_unit(chla$crypto_algae_chla_fluorescence_component), "]")
      ) %>%
      plotly::add_trace(
        y = ~Maximum_depth_below_surface_of_the_water_body,
        x = ~Concentration_of_organic_matter_in_water_bodies,
        name = 'Organic matter'
      ) %>%
      plotly::add_trace(
        y = ~Maximum_depth_below_surface_of_the_water_body,
        x = ~water_temp,
        xaxis = "x2",
        name = paste0('Water temperature [', units::deparse_unit(chla$water_temp), "]")
      ) %>%
      plotly::layout(
        yaxis = list(
          autorange = "reversed",
          title = paste0(
            'Maximum depth below surface of the water body [',
            units::deparse_unit(chla$Maximum_depth_below_surface_of_the_water_body),
            ']'
          )
        ),
        xaxis = list(
          title = paste0('Chla fluorescence component [', units::deparse_unit(chla$chla_fluorescence_component), ']')
        ),
        xaxis2 = list(
          overlaying = "x",
          anchor = "y",
          side = "top",
          showticklabels = TRUE,
          title = paste0('temperature [', units::deparse_unit(chla$water_temp), ']')
        )
      )
  })
}
