library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(
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
                tags$a(
                  href = "https://itineris.cnr.it",
                  tags$img(src = "images/Logo_ITINERIS.png"),
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
                sidebar_width = 25,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  tags$p(".."),
                  tags$p(tags$b(".."))
                ),
                column(12,
                       # Input: eLTER site
                       div(HTML("<h4><b>eLTER site</b></h4>")),
                       selectInput(
                         inputId = "site",
                         label = "Select an eLTER site",
                         multiple = FALSE,
                         choices = sites_list,
                         selected = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe"
                       ),
                       # Input: EV
                       div(HTML("<hr><h4><b>EVs</b></h4>")),
                       selectInput(
                         inputId = "ev",
                         label = HTML("Select an EV"), 
                         multiple = FALSE,
                         choices = EVs_list,
                         selected = "https://gcos.wmo.int/en/essential-climate-variables/lakes"
                       ),
                       actionButton("sendFile", "Explore", icon = icon("file-upload")),
                )
              ),
              box(
                width = 8,
                title = "Info about selection", 
                closable = FALSE, 
                status = "info", 
                solidHeader = FALSE, 
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 25,
                sidebar_start_open = FALSE,
                sidebar_content = tagList(
                  tags$p(".."),
                  tags$p(".."),
                  tags$p(tags$b(".."))
                )
              )
            ),
            fluidRow(
              # box(
              #   width = 12,
              #   title = "Visualization",
              #   closable = FALSE,
              #   status = "info",
              #   solidHeader = FALSE,
              #   collapsible = TRUE,
              #   enable_sidebar = TRUE,
              #   sidebar_width = 25,
              #   sidebar_start_open = FALSE,
              #   sidebar_content = tagList(
              #     tags$p(".."),
              #     tags$p(tags$b(".."))
              #   )
              # ),
              box(
                width = 12,
                  title = "Visualization",
                  closable = FALSE,
                  status = "info",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  enable_sidebar = TRUE,
                  sidebar_width = 25,
                  sidebar_start_open = FALSE,
                  sidebar_content = tagList(
                    tags$p(".."),
                    tags$p(tags$b(".."))
                  ),
                tabBox(width = 12,
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
)
