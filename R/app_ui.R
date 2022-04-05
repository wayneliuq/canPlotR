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
                            placeholder = "plot name",
                            value = "plot",
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
                  selectInput( "Choose data source",
                              inputId = "input_data_type",
                              choices = c(
                                "Upload Data" = 1,
                                "Example Data" = 2
                              ),
                            selected = 1),

                  ## upload box for user data
                  conditionalPanel(
                    condition = "input.input_data_type == 1",
                    fileInput(inputId = "data_user",
                              label = "Upload your data",
                              accept = c(".csv", ".txt", ".xls", ".xlsx"),
                              placeholder = "csv, txt, xls, and xlsx files") |>
                      prompter::add_prompt(
                        position = "right",
                        message = "Supported filetypes include 'xls', 'xlsx', 'csv', and 'txt'.
						                      By default, only the first sheet of Excel spreadsheets will be loaded.",
                        size = "large")
                  ),

                  ## prompt that example data is loaded
                  conditionalPanel(
                    condition = "input.input_data_type == 2",
                    renderText("Example data loaded.")
                  ),

                  ## action button bound to data loading
                  actionButton(inputId = "data_load",
                               label = "Load Data!",
                               class = "btn-success") |>
                    prompter::add_prompt(
                      position = "right",
                      message = "Click here to load the data you selected.",
                      size = "medium"
                    )
                ),

                #### choose columns to plot ####

                ## choose x and y
                fluidRow(
                  hr(),
                  p(strong("Choose x and y variables")),

                  column(
                    width = 6,
                    selectInput(inputId = "xvar",
                                label = "Choose x variable",
                                choices = "No data loaded!",
                                selected = 1,
                                selectize = F,
                                multiple = F,
                                size = 4)
                  ),
                  column(
                    width = 6,
                    selectInput(inputId = "yvar",
                                label = "Choose y variable",
                                choices = "No data loaded!",
                                selected = 1,
                                selectize = F,
                                multiple = F,
                                size = 4)
                  )
                ),

                #### asfactor and transformations ####
                fluidRow(
                  column(
                    width = 6,

                    ## format x as factor
                    conditionalPanel(
                      condition = "output.xvar_isnumeric",
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
                        the x-variable as a categorical variable (factor)."
                      )
                    ),

                    ## select transformation for numeric x variable
                    shinyWidgets::pickerInput(
                      inputId = "xtrans",
                      label = "x-axis transformation",
                      choices = trans_continuous,
                      selected = 1
                    ),

                    # reorder categorical x, hide in dropdown since sometimes it's huge
                    conditionalPanel(
                      condition = "output.xvar_isfactor",
                      shinyWidgets::dropdown(
                        status = "primary",
                        label = "re-order x categories",
                        shinyjqui::orderInput(
                          inputId = "xorder",
                          label = NULL,
                          items = c("NA"),
                          item_class = "primary"
                        )
                      ) |> prompter::add_prompt(
                             position = "right",
                             size = "large",
                             message = "Click to open a menu which allows you to
                             reorder the categories. Drag to re-order."
                           )
                    )

                  ),

                  column(
                    width = 6,

                    ## format y as factor
                    conditionalPanel(
                      condition = "output.yvar_isnumeric",
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
                        the x-variable as a categorical variable (factor)."
                      )
                    ),

                    ## select transformation for numeric y variable
                    shinyWidgets::pickerInput(
                      inputId = "ytrans",
                      label = "y-axis transformation",
                      choices = trans_continuous,
                      selected = 1
                    ),

                    ## reorder categorical x, hide in dropdown since sometimes it's huge
                    conditionalPanel(
                      condition = "output.yvar_isfactor",
                      shinyWidgets::dropdown(
                        status = "primary",
                        label = "re-order y categories",
                        shinyjqui::orderInput(
                          inputId = "yorder",
                          label = NULL,
                          items = c("NA"),
                          item_class = "primary"
                        )
                      ) |> prompter::add_prompt(
                             position = "right",
                             size = "large",
                             message = "Click to open a menu which allows you to
                             reorder the categories. Drag to re-order."
                           )
                    )
                  )
                )
              ),

              tabPanel(
                title = "2. Group and Split Data",

                ## split by color (factor)

                fluidRow(
                  p(strong("Group data by color")),

                  p("Group your data by a variable, which will be coerced to a
                  categorical variable. Some plots, such as filled density plots,
                  cannot be grouped."),

                  shinyWidgets::pickerInput(
                    inputId = "color_factor_var",
                    label = NULL,
                    choices = "none",
                    multiple = F,
                    selected = 1
                  ),

                  conditionalPanel(
                    condition = "input.color_factor_var !== 'none'",
                    shinyWidgets::dropdown(
                      status = "primary",
                      label = "re-order grouping variable",
                      shinyjqui::orderInput(
                        inputId = "color_factor_var_order",
                        label = NULL,
                        items = c("NA"),
                        item_class = "primary"
                      )
                    ) |> prompter::add_prompt(
                      position = "right",
                      size = "large",
                      message = "You can re-order the grouping variable by opening
                      this dropdown menu and dragging the items to the desired order."
                    )
                  )
                ),

                ## split by facet

                fluidRow(

                  p(strong("Split plot")),

                  p("Split your data into panels (facets) by categorical
                  variables. Numeric variables will be coerced to categories."),

                  column(
                    width = 6,
                    p(em("horizontal split (columns)")),

                    shinyWidgets::pickerInput(
                      inputId = "facet_hvar",
                      choices = "none",
                      selected = 1
                    ),

                    conditionalPanel(
                      condition = "input.facet_hvar !== 'none'",
                      shinyWidgets::dropdown(
                        status = "primary",
                        label = "re-order horizontal panels",
                        size = "sm",
                        shinyjqui::orderInput(
                          inputId = "facet_hvar_order",
                          items = "NA",
                          label = NULL
                        )
                      ) |> prompter::add_prompt(
                        position = "right",
                        size = "large",
                        message = "You can re-order the horizontal panel by opening
                        this dropdown menu and dragging the items to the desired order."
                      )
                    )

                  ),

                  column(
                    width = 6,
                    p(em("vertical split (rows)")),

                    shinyWidgets::pickerInput(
                      inputId = "facet_vvar",
                      choices = "none",
                      selected = 1
                    ),

                    conditionalPanel(
                      condition = "input.facet_vvar !== 'none'",
                      shinyWidgets::dropdown(
                        status = "primary",
                        label = "re-order vertical panels",
                        size = "sm",
                        shinyjqui::orderInput(
                          inputId = "facet_vvar_order",
                          items = "NA",
                          label = NULL
                        )
                      ) |> prompter::add_prompt(
                        position = "right",
                        size = "large",
                        message = "You can re-order the vertical panel by opening
                        this dropdown menu and dragging the items to the desired order."
                      )
                    )

                  )
                )

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

              ## export resolution
              shinyWidgets::pickerInput(
                inputId = "export_filetype",
                label = "Filetype",
                choices = c("png", "pdf"), # can expand to more filetypes supported by ggsave
                selected = 1
              ),

              ## export size
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

                    ),

                    ## categorical x & y
                    fluidRow(

                      p(strong("Categorical x & y")),

                      shinyWidgets::switchInput(
                        inputId = "geomcount",
                        label = "counts plot",
                        value = F,
                        labelWidth = 200
                      ) |> prompter::add_prompt(
                        position = "top",
                        message = "Display the number of items corresponding to a matrix of
                        categorical x and y variables.",
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
