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
    
    ## Application UI 
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
                title = "1. Select Data",
                
                ## plot name
                
                fluidRow(
                  textInput(inputId = "plotid",
                            label = "Plot Name",
                            placeholder = "plot", 
                            width = "100%") |> 
                    prompter::add_prompt(
                      position = "right",
                      size = "large",
                      message = "Enter the name for this plot here. 
                         This name will be used as the title, and for 
                         exporting the exported plot.")
                ),

                
                ## select data set
                fluidRow(
                  selectInput("Choose data source", 
                              inputId = "input_data_type",
                              choices = c(
                                "Upload User Data" = 1,
                                "Example Data" = 2
                              )),
                  uiOutput("data_ui")
                ),

                #### choose columns to plot ####
                
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
                
                #### asfactor and transformations ####
                fluidRow(
                  column(
                    width = 6,
                    
                    ## format x as factor
                    checkboxInput(
                      inputId = "x_asfactor",
                      label = "Format as categorical",
                      value = F
                    ) |> prompter::add_prompt(
                      position = "right",
                      size = "large",
                      message = "If the column you selected as the x variable 
                      conists of only numbers, it will be assumed to be a 
                      continuous variable. Select this box if you want to format 
                      the x-variable as a categorical variable (factor). If the 
                      variable is already detected as categorical, nothing will 
                      happen."
                    ),
                    
                    ## select transformation for numeric x variable
                    shinyWidgets::pickerInput(
                      inputId = "xtrans",
                      label = "x-axis transformation",
                      choices = trans_continuous,
                      selected = 1
                    ),
                    
                    ## reorder categorical x, hide in dropdown since sometimes it's huge
                    uiOutput("orderX") |> 
                      prompter::add_prompt(
                        position = "right",
                        size = "large",
                        message = "Click to open a menu which allows you to 
                        reorder the categories. Drag to re-order. Click on an
                        item to select it to reorder multiple items at a time."
                      )
                  ),
                  column(
                    width = 6,
                    
                    ## format y as factor
                    checkboxInput(
                      inputId = "y_asfactor",
                      label = "Format as categorical",
                      value = F
                    ) |> prompter::add_prompt(
                      position = "right",
                      size = "large",
                      message = "If the column you selected as the y variable 
                      conists of only numbers, it will be assumed to be a 
                      continuous variable. Select this box if you want to format 
                      the x-variable as a categorical variable (factor). If the 
                      variable is already detected as categorical, nothing will 
                      happen."
                    ),
                    
                    ## select transformation for numeric y variable
                    shinyWidgets::pickerInput(
                      inputId = "ytrans",
                      label = "y-axis transformation",
                      choices = trans_continuous,
                      selected = 1
                    ),
                    
                    ## reorder categorical x, hide in dropdown since sometimes it's huge
                    uiOutput("orderY")|> 
                      prompter::add_prompt(
                        position = "right",
                        size = "large",
                        message = "Click to open a menu which allows you to 
                        reorder the categories. Drag to re-order. Click on an
                        item to select it to reorder multiple items at a time."
                      )
                  )
                )
              ),
              
              tabPanel(
                title = "2. Stratify Data",
                uiOutput(outputId = "color_var")

              ),
              
              tabPanel(
                title = "3. NA",
                
              )
            )
          )
        ),
        
        #### main plot output ####
        
        column(
          width = 6,
          
          ## remove update plot button dependency until testing with large datasets
          # fluidRow(
          #   actionButton(
          #     inputId = "makeplot",
          #     label = "Update Plot",
          #     icon = icon("chart-bar"),
          #     class = "btn-primary"
          #   )
          # ),
          
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
                title = "Plot Type",
                
                #### choose type of plot ####
                
                fluidRow(
                  
                  ## choose the type of geom
                  
                  column(
                    width = 12,
                    
                    ## continuous x and y
                    
                    fluidRow(
                      p(strong("Continuous x & y")),
                      ## inputId$geompoint = geom_point
                      shinyWidgets::switchInput(
                        inputId = "geompoint",
                        label = "points",
                        value = T,
                        labelWidth = 200
                      ) |> prompter::add_prompt(
                        position = "top",
                        message = "Display individual data points (geom_point), 
                      suitable for most plots with continuous x and y variables.",
                        size = "medium"
                      ),
                      
                      ## input$bin2d = geom_bin2d
                      shinyWidgets::switchInput(
                        inputId = "geombin2d",
                        label = "density heatmap",
                        value = F,
                        labelWidth = 200
                      ) |> prompter::add_prompt(
                        position = "top",
                        message = "Display a density heatmap binning the data into 
                      rectangles (geom_bin2d), suitable for continuous x and y 
                      plots with a large number of data points.",
                        size = "medium"
                      ),
                      
                      ## input$geomdensity = geom_density2d
                      shinyWidgets::switchInput(
                        inputId = "geomdensity",
                        label = "density contours",
                        value = F,
                        labelWidth = 200
                      ) |> prompter::add_prompt(
                        position = "top",
                        message = "Display a binned contour plot (geom_density2d),
                      suitable for continuous x and y plots with a large number
                      of data points.",
                        size = "medium"
                      ),
                      
                      ## input$geomdensity = geom_density2d
                      shinyWidgets::switchInput(
                        inputId = "geomdensityfilled",
                        label = "filled density contours",
                        value = F,
                        labelWidth = 200
                      ) |> prompter::add_prompt(
                        position = "top",
                        message = "Display a binned contour with filled bands 
                      (geom_density2d_filled), suitable for continuous x and y plots 
                      with a large number of data points.",
                        size = "medium"
                      ),
                      
                    ),
                    
                    ## categorical x, continuous y
                    fluidRow(
                      p(strong("Categorical x, continuous y")),
                      ## input$geomboxplot = geom_boxplot
                      shinyWidgets::switchInput(
                        inputId = "geomboxplot",
                        label = "box plot",
                        value = F,
                        labelWidth = 200
                      ) |> prompter::add_prompt(
                        position = "top",
                        message = "Display a summary of the y variable using 
                        box plots (geom_boxplot), suitable for categorical x and 
                        continuous y variables.",
                        size = "medium"
                      ),
                      
                      ## input$geomviolin = geom_violin
                      shinyWidgets::switchInput(
                        inputId = "geomviolin",
                        label = "violin plot",
                        value = F,
                        labelWidth = 200
                      ) |> prompter::add_prompt(
                        position = "top",
                        message = "Display a summary of the y variable using 
                        violin plots (geom_violin), suitable for categorical x and 
                        continuous y variables.",
                        size = "medium"
                      ),
                      
                      ## input$dotplot = geom_dotplot
                      shinyWidgets::switchInput(
                        inputId = "geomdotplot",
                        label = "dot plot",
                        value = F,
                        labelWidth = 200
                      ) |> prompter::add_prompt(
                        position = "top",
                        message = "Display individal y datapoints, suitable for categorical x and 
                        continuous y variables.",
                        size = "medium"
                      )
                      
                    )
                    
                  )
                )
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
            tags$h5("statistics"),
            tabsetPanel(
              tabPanel(
                title = "statistics 1",
                "statistics 1"
              ),
              tabPanel(
                title = "Summary Statistics",
                reactable::reactableOutput("datasummary")
              )
            )
          )
        )
      ),
      
      fluidRow(
        #### data preview ####
        ## later move the data table to a better location
        
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

#### common functions and variables ####
## transform or reorder x and y (numeric vs factor)
trans_continuous <- c(
  "none" = "identity",
  "reverse",
  "log10",
  "log2",
  "natural log" = "log",
  "sqrt",
  "exp",
  "logit",
  "probit",
  "date",
  "time hms" = "hms",
  "time POSIX" = "time"
)