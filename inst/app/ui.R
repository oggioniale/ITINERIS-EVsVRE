library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(leaflet)

# Define UI for application that draws a histogram

fluidPage(
  dashboardPage(
    # collapse_sidebar = TRUE,
    header = dashboardHeader(
      title = tagList(
        tags$head(tags$script(src = "message-handler.js"),tags$style(HTML('.navbar {
                                background-color: #FFFFFF !important;
                              }
                              .logo {
                                background-color: #FFF !important;
                                color: #595959 !important;
                              }
                              .sidebar-toggle {
                                background-color: #FFF !important;
                                color: #595959 !important;
                              }
                              .skin-blue .main-header .navbar .sidebar-toggle:hover {
                                background-color: #00a54f !important;
                                color: #595959 !important;
                              }
                              .skin-blue .sidebar-menu > li.active > a, .skin-blue .sidebar-menu > li:hover > a {
                                background-color: #3d403e !important;
                                color: #FFF !important;
                                border-left-color: #00a54f !important;
                              }
                              .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
                                background-color:#3d403e !important;
                              }
                              .box.box-info {
                                border-top-color: #00a54f !important;
                              }
                              .nav-tabs-custom > .nav-tabs > li.active {
                                border-top-color: #00a54f !important;
                              }
                              .alert-info, .bg-aqua, .callout.callout-info, .label-info, .modal-info .modal-body {
                                background-color: #00a54f !important;
                              }
                              '))),
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
    # sidebar = dashboardSidebar(disable = TRUE),
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
            box( # site info
              width = 6,
              title = "eLTER site", 
              closable = FALSE, 
              status = "info", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              enable_sidebar = TRUE,
              selectizeInput(
                #selectInput(
                inputId = "site",
                label = HTML("select site"),
                multiple = FALSE,
                choices = ITINERIS.EVsVRE:::sites$deimsUUID %>%
                  magrittr::set_names(
                    ITINERIS.EVsVRE:::sites$name
                  ),#,#broker$siteList(),
                selected = NULL
              ),
              uiOutput("siteinfo")
            ),
            box( # EV info
              width = 6,
              title = "Essential Variable (EV)", 
              closable = FALSE, 
              status = "info", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              enable_sidebar = TRUE,
              selectizeInput(
                inputId = "ev",
                label = HTML("select EV"),
                multiple = FALSE,
                choices = list("<select one>"=""),#broker$EVsList(),
                selected = NULL
              ),
              uiOutput("EVinfo")
            ),
            box(
              width = 12,
              title = "Datasets found", 
              closable = FALSE, 
              status = "info", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              enable_sidebar = TRUE,
              uiOutput("info_box_EVsData"),
              uiOutput("info_box_OtherResData"),
              uiOutput("info_box_OtherRepoData"),
              tabBox(
                id="resultsTabContainer",
                width = 12,
                selected = "EVs",
                tabPanel(
                  "Concerning selected EVs",
                  value="EVs",
                  DT::dataTableOutput("tableEVsData")
                ),
                tabPanel(
                  "Relating to the selected site (structured)",
                  value="OtherRes",
                  DT::dataTableOutput("tableOtherResData")
                ),
                tabPanel(
                  "Relating to the selected site (unstructured)",
                  value="OtherRepo",
                  DT::dataTableOutput("tableOtherRepoData")
                )
              )
            )
          ),
          # # -- START REMOVE THIS AFTER DEVELOPMENT
          # fluidRow(
          #   box(
          #     width = 12,
          #     title="debug info",
          #     status="info",
          #     shiny::textOutput("debug")
          #   )
          # ),
          # # -- END REMOVE THIS AFTER DEVELOPMENT
          fluidRow(
            box(
              width = 12,
              title = "Dataset visualisation",
              closable = FALSE,
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              enable_sidebar = TRUE,
              tabBox(
                id="datasetLists",
                width = 12,
                # side = "right",
                # height = "250px",
                selected = "Map",
                tabPanel(
                  title="Map",
                  leaflet::leafletOutput("map")
                ),
                tabPanel(
                  title="Table",
                  DT::dataTableOutput("tbl")
                ),
                tabPanel(
                  title="Metadata",
                  DT::dataTableOutput("selectedDatasetMD")
                )#,
                # tabPanel(
                #   "Chart",
                #   plotly::plotlyOutput("plot")
                # )
              )
            )
          ),
          fluidRow(
            
            box(
              id="saveDataset",
              width = 12,
              title="Save dataset in my workspace",
              status="info",
              shiny::textInput("saveCurrentDatasetName",label="R object file name:"),
              shiny::actionButton("saveCurrentDataset",label = "save dataset R object in my workspace"),
              
            )
          ),
          fluidRow(
            box(
              shiny::actionButton("stopApp",label = "Stop the application"),
            )
          )
          #
        )
      )
    )
  )
)

