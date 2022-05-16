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
    shinyWidgets::useSweetAlert(theme = "minimal"),

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
                  mod_data_load_ui("data_load_1")
                )
              ),

              tabPanel(
                title = "2. Choose Variables",

                #### choose columns to plot ####
                mod_choose_plotxy_ui("choose_plotxy_1")

              ),

              #### statistics and regression ####
              tabPanel(
                title = "3. Regression",
                mod_regression_ui("regression_1")
              )
            )
          )
        ),

        #### main plot output ####

        column(
          width = 6,

                    # make resizable with shinyjqui
          # With mouse interactions attached, the corresponding interaction
          # states, e.g. position of draggable, size of resizable, selected of
          # selectable and order of sortable, will be sent to server side in the
          # form of input$<id>_<state>. The default values can be overridden by
          # setting the shiny option in the options parameter. Please see the
          # vignette Introduction to shinyjqui for more details.

          fluidRow(
            plotOutput("plot") |>
              shinyjqui::jqui_resizable()
          ),


          ## export dropdown menu
          fluidRow(
            shinyWidgets::dropdown(
              label = "Export Figure",

              ## export file type
              shinyWidgets::pickerInput(
                inputId = "export_filetype",
                label = "Filetype",
                choices = sort(c("png", "pdf", "jpeg", "bmp", "svg", "eps", "tex", "tiff")), # can expand to more filetypes supported by ggsave
                selected = "png"
              ),

              ## export resolution
              shinyWidgets::sliderTextInput(
                inputId = "export_resolution",
                label = "Resolution",
                choices = c(36, 72, 100, 200, 300, 600),
                selected = "72"
              ) |> prompter::add_prompt(
                size = "medium",
                position = "right",
                message = "Select the desired resolution (pixels per inch) of
                the exported figure."
              ),

              ## export size
              # also a button to get size from the current preview
              fluidRow(

                column(
                  width = 4,
                  numericInput(
                    inputId = "export_width",
                    label = "Width:",
                    value = 800,
                    min = 0,
                    max = 50000,
                    step = 10,
                    width = "50%"
                  )
                ),

                column(
                  width = 4,
                  numericInput(
                    inputId = "export_height",
                    label = "Height:",
                    value = 500,
                    min = 0,
                    max = 50000,
                    step = 10,
                    width = "50%"
                  )
                ),

                column(
                  width = 4,
                  actionButton(
                    inputId = "export_getdimensions",
                    label = "Get preview dimensions"
                  ) |> prompter::add_prompt(
                    size = "medium",
                    position = "right",
                    message = "Click this button to retrieve the dimensions of the
                    preview figure. The dimensions are automatically adjusted
                    for the resolution you selected above."
                  )
                )

              ),

              ## download button
              downloadButton(
                "plot_download",
                label = "Export plot",
                class = "btn-success"
              )

            )
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
                title = "Plot Appearance",

                ## color data points with continuous variables
                fluidRow(

                  p(strong("Color with continuous variables")),

                  p("Colour your data points with a continuous variable from your
                    data. This will only show on plot types with points (e.g.
                    point plots and dot plots). If you've already selected a
                    grouping color variable, nothing will show."),

                  shinyWidgets::pickerInput(
                    inputId = "color_numeric_var",
                    label = NULL,
                    choices = "none",
                    selected = 1
                  )
                )
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
                tableOutput("debug"),
                textOutput("debug2")
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
# trans_continuous <- c(
#   "none" = "identity",
#   "reverse",
#   "log10",
#   "log2",
#   "natural log" = "log",
#   "sqrt",
#   "exp",
#   "logit",
#   "probit",
#   "date",
#   "time hms" = "hms",
#   "time POSIX" = "time"
# )
