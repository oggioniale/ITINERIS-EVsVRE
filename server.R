library(shiny)
library(shinybusy) # 
#source(broker)

# Define server logic required to draw a histogram
function(input, output, session) {

  # status (Reactive Values "object" to store current status)
  status <- reactiveValues(
    selectedSite = '',
    selectedEv = NULL,
    site_info=NULL
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
    #})
    shinybusy::remove_modal_spinner()
      #browser()
    output$EVinfo <- renderUI(tagList(
      a(x$name, href=x$webpage),
      renderTable(x %>% dplyr::select(type, domain, uom), rownames = TRUE),
      p(x$description)
    ))
  })
  
  # Tables box ----
  exampleTibble <- tibble::tibble(
    title = c("titolo1", "titolo2", "titolo3"),
    url = c("url1/doi1", "url2/doi2", "url3/doi3"),
    resType = c("Zenodo", "Pangaea", "iNat")
  )
  output$tableEVsData <- DT::renderDataTable({
    tblEVsData <- exampleTibble
    DT::datatable(
      tblEVsData,
      escape = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        'Tabella - ', htmltools::em(paste0(
          'caption text'
        ))
      ),
      filter = 'top'
    )
  })
  output$tableOtherResData <- DT::renderDataTable({
    tblOtherResData <- exampleTibble
    DT::datatable(
      tblOtherResData,
      escape = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        'Tabella - ', htmltools::em(paste0(
          'caption text'
        ))
      ),
      filter = 'top'
    )
  })
  output$tableOtherRepoData <- DT::renderDataTable({
    tblOtherRepoData <- exampleTibble |>
      dplyr::select(
        Title = title,
        `Resource link` = url,
        `Resource repo` = resType
      )
    DT::datatable(
      tblOtherRepoData,
      escape = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        'Tabella - ', htmltools::em(paste0(
          'caption text'
        ))
      ),
      filter = 'top'
    )
  })
}
