library(shiny)
library(shinybusy) # 
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
  exampleTibble <- tibble::tibble(
    title = c("titolo1", "titolo2", "titolo3"),
    url = c("url1/doi1", "url2/doi2", "url3/doi3"),
    resType = c("Zenodo", "Pangaea", "iNat")
  )
  datasets <- reactiveValues(
    # per ora lascio esempi. Poi sono da mettere tutti a NULL
    tblEVsData = exampleTibble,
    tblOtherResData = exampleTibble,
    tblOtherRepoData = exampleTibble |>
      dplyr::select(
        Title = title,
        `Resource link` = url,
        `Resource repo` = resType
      )
    
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
      div(renderTable(t(x$tbl_generalInfo %>% as_data_frame())))#,
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
      #datasets$tblEVsData <- 
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
      "Numero di dataset che contribuiscono alla EV selezionata", 
      nrow(datasets$tblEVsData), 
      icon = icon("table"),
      color = "olive"
    )
  })
  output$info_box_OtherResData <- renderUI({
    infoBox(
      "Numero di dataset accessori",
      nrow(datasets$tblOtherResData), 
      icon = icon("table"),
      color = "olive"
    )
  })
  output$info_box_OtherRepoData <- renderUI({
    infoBox(
      "Numero di dataset che rispondono alle parole chiave nome sito (e variabile) in altri repo",
      nrow(datasets$tblOtherRepoData), 
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
  
  # Visualization box ----
  occ <- ReLTER::get_site_speciesOccurrences(
    deimsid = paste0("https://deims.org/", selected_site),
    list_DS = "gbif",
    exclude_inat_from_gbif = TRUE,
    show_map = FALSE,
    limit = 500
  )
  shared_data <- SharedData$new(occ$gbif) 
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(shared_data) |>
      leaflet::addProviderTiles(
        "CartoDB.Positron",
        options = leaflet::providerTileOptions(opacity = 0.99)) |>
      leaflet::addMarkers()
  })
  output$tbl <- DT::renderDT({
    DT::datatable(
      shared_data,
      escape = FALSE,
      filter = 'top'
    )
  }, server = FALSE)
}
