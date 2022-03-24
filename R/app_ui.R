#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    #### external resources ####
    golem_add_external_resources(),
    prompter::use_prompt(),
    shinybusy::add_busy_bar(timeout = 500),
    
    # Your application UI logic 
    fluidPage(
      
      #### theme ####
      theme = shinythemes::shinytheme("paper"),
      
      #### title ####
      
      titlePanel(
        title = "canPlotR", ## later reduce text size
        windowTitle = "canPlotR"
      ),
      
      #### main page ####
      
      fluidRow(
        
        #### plot data elements ####
        
        column(
          width = 6,
          wellPanel(
            tags$h5("Plot Data"),
            tabsetPanel(
              
              #### upload and select data ####
              tabPanel(
                title = "1. Upload",
                
                ## plot name
                prompter::add_prompt(
                  textInput(inputId = "plotid",
                            label = "Plot Name",
                            placeholder = "plot", 
                            width = "100%"),
                  position = "right",
                  size = "large",
                  message = "Enter the name for this plot here. 
                         This name will be used as the title, and for 
                         exporting the exported plot."),
                
                ## select data set
                
                selectInput("Choose data source", 
                            inputId = "input_data_type",
                            choices = c(
                              "Upload User Data" = 1,
                              "Example Data" = 2
                            )),
                uiOutput("data_ui")
              ),
              
              #### choose columns to plot ####
              tabPanel(
                title = "2. Choose columns",
                
                ## choose x and y
                fluidRow(
                  column(
                    width = 6,
                    uiOutput("chooseX")
                  ),
                  column(
                    width = 6,
                    uiOutput("chooseY")
                  )
                ),
                
                ## whether to convert to factor
                fluidRow(
                  column(
                    width = 6,
                    uiOutput("formatfactorx"),
                    uiOutput("transX")
                  ),
                  column(
                    width = 6,
                    uiOutput("formatfactory"),
                    uiOutput("transY")
                  )
                )
              ),
              
              #### choose type of plot ####
              tabPanel(
                title = "3. Choose Plot",
                
                fluidRow(
                  
                  ## choose the type of geom
                  column(
                    width = 12,
                    ## input$geompoint = geom_point
                    shinyWidgets::switchInput(
                      inputId = "geompoint",
                      label = "points",
                      value = T,
                      labelWidth = 300
                    ) |> prompter::add_prompt(
                      position = "top",
                      message = "Display individual data points (geom_point), suitable for continuous x and y variables.",
                      size = "medium"
                    ),
                    
                    ## input$geomline = geom_line
                    shinyWidgets::switchInput(
                      inputId = "geomline",
                      label = "line",
                      value = F,
                      labelWidth = 300
                    ) |> prompter::add_prompt(
                      position = "top",
                      message = "Connect data points with a line (geom_line), suitable for data witha continuous y variable.",
                      size = "medium"
                    ),
                    
                    ## input$geomboxplot = geom_boxplot
                    shinyWidgets::switchInput(
                      inputId = "geomboxplot",
                      label = "box plot",
                      value = F,
                      labelWidth = 300
                    ) |> prompter::add_prompt(
                      position = "top",
                      message = "Display a summary of the y variable using box plots (geom_boxplot), suitable for categorical x and continuous y variables.",
                      size = "medium"
                    )
                  )
                )
              )
            )
          )
        ),
        
        #### main plot output ####
        
        column(
          width = 6,
          fluidRow(
            actionButton(
              inputId = "makeplot",
              label = "Update Plot",
              icon = icon("chart-bar"),
              class = "btn-primary"
            )
          ),
          fluidRow(
            plotOutput("plot")
          )
        )
        
      ),
      
      fluidRow(
        
        #### plot appearance ####
        
        column(
          width = 6,
          wellPanel(
            tags$h5("Plot Appearance"),
            tabsetPanel(
              tabPanel(
                title = "appearance 1",
                "appearance 1"
              ),
              tabPanel(
                title = "apperance 2",
                "appearance 2"
              )
            )
          )
        ),
        
        #### statistics ####
        column(
          width = 6,
          wellPanel(
            tags$h5("Statistics"),
            tabsetPanel(
              tabPanel(
                title = "statistics 1",
                "statistics 1"
              ),
              tabPanel(
                title = "statistics 2",
                "statistics 2"
              )
            )
          )
        )
      ),
      
      fluidRow(
        #### data preview ####
        column(
          width = 6,
          reactable::reactableOutput("datatable")
        ),
        
        #### tutorial/about ####
        column(
          width = 6,
          wellPanel(
            tabsetPanel(
              tabPanel(
                title = "How to use",
                "tutorial"
              ),
              tabPanel(
                title = "About",
                "about"
              ),
              tabPanel(
                title = "debug",
                textOutput("debug")
              )
            )
          )
        )
      )
      
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'canGraphR'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

