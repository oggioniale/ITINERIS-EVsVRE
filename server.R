library(shiny)
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
    status$selectedSite <- input$site
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
    
    x<-broker$getInfo_Site() # first call requires some time. 
    # TODO: add a "progress" to notify this is loading
    
    output$siteinfo <- renderUI(tagList(
      a(x$val_title, href=x$val_uri, target="_blank"),
      p("Yearly avg precipitation: ", x$val_precipitation, "[", units::deparse_unit(x$val_precipitation), "]"),
      p("Biome", x$val_geoBonBiome),
      p("Biogeographical Region:", x$val_biogeographicalRegion),
      div("habitats within the site", class="scroll", renderTable(x$tbl_eunisHabitats, striped = T, colnames = F))
      
    ))
    
  })
  
}
