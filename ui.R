library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(leaflet)

# Define UI for application that draws a histogram

fluidPage(
  dashboardPage(
    skin = "blue",
    # collapse_sidebar = TRUE,
    header = dashboardHeader(
      title = tagList(
        tags$span(class = "logo-lg", "EVs VRE"), 
        tags$img(src = "images/Logo_ITINERIS_globo-01.png")), 
      # fixed = FALSE,
      # enable_rightsidebar = TRUE,
      # rightSidebarIcon = "gears",
      tags$li(class ="dropdown", 
              id="containerItinerisLogo",
              tags$a(
                href = "https://itineris.cnr.it",
                tags$img(id="itinerisLogo", src = "images/Logo_ITINERIS.png"),
                style = "margin:0;padding-top:2px;padding-bottom:2px;padding-left:10px;padding-right:10px;",
                target = "_blank"
              )
      )#,
      # tags$li(class = "dropdown",
      #         actionButton("help", "Give me an overview", style="margin-right: 10px; margin-top: 8px; color: #fff; background-color: #0069D9; border-color: #0069D9")
      # )
    ),
    sidebar = dashboardSidebar(
      collapsed = TRUE,
      sidebarMenu(
        menuItem("Fixed Station", tabName = "fixed", icon = icon("map-marked-alt", lib = "font-awesome"))
      )
    ),
    body = dashboardBody(
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css")
      ),
      tabItems(
        tabItem(
          tabName = "fixed",
          fluidRow(
            box(
              width = 4,
              title = "Select eLTER site and EV", 
              closable = FALSE, 
              status = "info", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              enable_sidebar = TRUE,
              column(12,
                     # Input: eLTER site
                     div(HTML("<h4><b>eLTER site</b></h4>")),
                     selectizeInput(
                       #selectInput(
                       inputId = "site",
                       label = "Select an eLTER site",
                       multiple = FALSE,
                       choices = broker$siteList(),
                       selected = NULL#"f30007c4-8a6e-4f11-ab87-569db54638fe"
                     ),
                     # Input: EV
                     div(HTML("<hr><h4><b>EVs</b></h4>")),
                     selectizeInput(
                       #selectInput(
                       inputId = "ev",
                       label = HTML("Select an EV"), 
                       multiple = FALSE,
                       choices = broker$EVsList(),
                       selected = ""
                     ),
                     actionButton("sendFile", "Explore", icon = icon("file-upload")),
              )
            ),
            box(
              width = 8,
              title = "Info box", 
              closable = FALSE, 
              status = "info", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              enable_sidebar = TRUE,
              infoBox("Numero di dataset che contribuiscono alla EV selezionata", 4, icon = icon("credit-card")),
              infoBox("Numero di dataset accessori", 2, icon = icon("credit-card")),
              infoBox("Numero di dataset che rispondono alle parole chiave nome sito e variabile in repo quali: DEIMS, Zenodo, Pangea, B2Share", 7, icon = icon("credit-card")),
              "Inserire: 1. info al sito con nome, PID (DEIMS.ID) linkabile; 2. info della EV selezionata con nome completo EV, link alla pagina; 3. tabella con dataset(s) riferiti alla EV selezionata, dataset(s) accessori (es. iNat, GBIF, ecc.) e dataset che sono presenti in repo non strutturati (es. DEIMS, Zenodo, Pangea, B2Share) i campi di questa tabella dovrabbero essere titolo dataset, sorgente, PID. Infine solo i dataset(s) che possono essere visibili nel pannello sottostante dovrebbero essere cliccabili."
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "Visualization",
              closable = FALSE,
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              enable_sidebar = TRUE,
              tabBox(
                width = 12,
                # side = "right",
                # height = "250px",
                selected = "Map",
                tabPanel("Map", "Tab content 1"),
                tabPanel("Table", "Tab content 2"),
                tabPanel("Chart", "Note that when side=right, the tab order is reversed.")
              )
            )
          )
        )
      )
    )
  )
)

