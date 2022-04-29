#' choose_plotxy UI Function
#'
#' @description Shiny module which allows the user to choose
#' x and y variables for plotting, and associated server logic.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_choose_plotxy_ui <- function(id){
  ns <- NS(id)
  tagList(

    #### choose x and y variables to plot ####
    fluidRow(
      hr(),
      p(strong("Choose x and y variables")),

      column(
        width = 6,
        selectInput(inputId = ns("xvar"),
                    label = "Choose x variable",
                    choices = "No data loaded!",
                    selected = 1,
                    selectize = F,
                    multiple = F,
                    size = 4)
      ),
      column(
        width = 6,
        selectInput(inputId = ns("yvar"),
                    label = "Choose y variable",
                    choices = "No data loaded!",
                    selected = 1,
                    selectize = F,
                    multiple = F,
                    size = 4)
      )
    ),

    #### factoring, ordering, and transformation of x and y ####
    fluidRow(
      column(
        width = 6,

        ## select transformation for numeric x variable
        shinyWidgets::pickerInput(
          inputId = ns("xtrans"),
          label = "x-axis transformation",
          choices = transformContinuous(),
          selected = 1
        ),

        ## format x as factor
        conditionalPanel(
          condition = "output.xvar_isnumeric",
          checkboxInput(
            inputId = ns("x_asfactor"),
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

        # reorder categorical x, hide in dropdown since sometimes it's huge
        conditionalPanel(
          condition = "output.xvar_isfactor",
          shinyWidgets::dropdown(
            status = "primary",
            label = "re-order x categories",
            shinyjqui::orderInput(
              inputId = ns("xorder"),
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

        ## select transformation for numeric y variable
        shinyWidgets::pickerInput(
          inputId = ns("ytrans"),
          label = "y-axis transformation",
          choices = transformContinuous(),
          selected = 1
        )

      )
    ),

    #### split by color (factor) ####

    fluidRow(
      p(strong("Group data by color")),

      p("Group your data by a variable, which will be coerced to a
                  categorical variable. Some plots, such as filled density plots,
                  cannot be grouped."),

      shinyWidgets::pickerInput(
        inputId = ns("color_factor_var"),
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
            inputId = ns("color_factor_var_order"),
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

    #### split data by facet ####

    fluidRow(

      p(strong("Split plot")),

      p("Split your data into panels (facets) by categorical
                  variables. Numeric variables will be coerced to categories."),

      column(
        width = 6,
        p(em("horizontal split (columns)")),

        shinyWidgets::pickerInput(
          inputId = ns("facet_hvar"),
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
              inputId = ns("facet_hvar_order"),
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
          inputId = ns("facet_vvar"),
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

  )
}

#' choose_plotxy Server Functions
#'
#' @noRd
mod_choose_plotxy_server <- function(id, 
                                     data_load_btn,
                                     data_vars,
                                     data_vars_numeric,
                                     x_factorlevels,
                                     color_factorlevels,
                                     facet_h_factorlevels,
                                     facet_v_factorlevels){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #### observer which updates x and y column choices ####
    ## to-do: bind observers to data loading
    observe({
      if (tryCatch(isTruthy(data_vars_numeric()), error = function(e) F)) {
        updateSelectInput(session,
                          inputId = "xvar",
                          choices = data_vars(), 
                          selected = 1)
        
        updateSelectInput(session,
                          inputId = "yvar",
                          choices = data_vars_numeric(), ## here
                          selected = 1)
      } else if (isTruthy(data_vars() |> tryCatch(error = function(e) F))) { 
        shinyWidgets::sendSweetAlert(
          title = "No numeric variables detected",
          type = "error",
          text = "This dashboard requires at least one numeric variable, as only
        numeric variables are able to be plotted on the y-axis. Please double-
        check and upload a new set of data where at least one column only
        consist of numbers."
        )
      } else {
        shinyWidgets::sendSweetAlert(
          title = "Data error",
          type = "error",
          text = "Something went wrong with the data you uploaded and columns of
        data could not be detected. Please make sure your file is in the correct
        format and upload a new file."
        )
      }
    }) |> bindEvent(data_load_btn()) # bindEvent(input$data_load)
    
    #### observer which updates color_factor and facet choices
    observe({
      shinyWidgets::updatePickerInput(
        session,
        inputId = "color_factor_var",
        choices = c("none", data_vars())
      )
      
      shinyWidgets::updatePickerInput(
        session,
        inputId = "facet_hvar",
        choices = c("none", data_vars())
      )
      
      shinyWidgets::updatePickerInput(
        session,
        inputId = "facet_vvar",
        choices = c("none", data_vars())
      )
      
    })
    
    #### observers that update factor levels for ordering ####
    observe({
      shinyjqui::updateOrderInput(
        session,
        inputId = "xorder",
        items = x_factorlevels()
      )
    })
    
    observe({
      shinyjqui::updateOrderInput(
        session,
        inputId = "color_factor_var_order",
        items = color_factorlevels()
      )
    })
    
    observe({
      shinyjqui::updateOrderInput(
        session,
        inputId = "facet_hvar_order",
        items = facet_h_factorlevels()
      )
    })
    
    observe({
      shinyjqui::updateOrderInput(
        session,
        inputId = "facet_vvar_order",
        items = facet_v_factorlevels()
      )
    })


    #### return values ####
    returnVals <- reactiveValues(
      xvar = reactive(input$xvar),
      yvar = reactive(input$yvar),
      xtrans = reactive(input$xtrans),
      x_asfactor = reactive(input$x_asfactor),
      xorder = reactive(input$xorder),
      ytrans = reactive(input$ytrans),
      color_factor_var = reactive(input$color_factor_var),
      color_factor_var_order = reactive(input$color_factor_var_order),
      facet_hvar = reactive(input$facet_hvar),
      facet_hvar_order = reactive(input$facet_hvar_order),
      facet_vvar = reactive(input$facet_vvar),
      facet_vvar_order = reactive(input$facet_vvar_order)
    )

    return(returnVals)

  })
}

## To be copied in the UI
# mod_choose_plotxy_ui("choose_plotxy_1")

## To be copied in the server
# mod_choose_plotxy_server("choose_plotxy_1")
