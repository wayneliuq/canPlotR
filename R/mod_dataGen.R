#' dataGen UI Function
#'
#' @description This is a module which allows the user to generate and download
#' a randomly generated dataframe of various sizes to test various functions
#' of canPlotR.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dataGen_ui <- function(id){
  ns <- NS(id)
  tagList(

    #### UI: dataGen settings input ####
    column(
      width = 8,

      ## seed
      numericInput(
        inputId = ns("dataGen_seed"),
        value = runif(1, 1, 1e9) |> as.integer(),
        label = "Seed",
        width = "100%"
      ) |> prompter::add_prompt(
        size = "medium",
        position = "right",
        message = "The seed used to generate the random dataframe. using
          the same seed should result in an identical table, if all other
          settings are identical."
      )
    ),

    column(
      width = 4,
      actionButton(
        inputId = ns("dataGen_seed_randbutton"),
        label = "Randomize",
        width = "100%"
      )
    ),

    column(
      width = 12,
      sliderInput(
        inputId = ns("dataGen_nrows"),
        label = "Rows of data",
        value = 100,
        min = 100L,
        max = 100000L,
        step = 100,
      )
    )

  )
}

#' dataGen Server Functions
#'
#' @noRd
mod_dataGen_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### observers that update UI ####
    observe({
      updateNumericInput(
        inputId = "dataGen_seed",
        value = runif(1, 1, 1e9) |> as.integer()
      )
    }) |> bindEvent(input$dataGen_seed_randbutton)
    
    
    #### dataGen function ####
    dataGen_df <- reactive({
      set.seed(input$dataGen_seed);
      tibble(
        id = 1:input$dataGen_nrow,
        gender = sample(
          c(rep("man", sample(1:20, 1)),
            rep("woman", sample(1:20, 1)),
            rep("nonbinary", sample(1:6, 1))),
          size = input$dataGen_nrow,
          replace = T
        )
      )
    })
    
    
  })
}

## To be copied in the UI
# mod_dataGen_ui("dataGen_1")

## To be copied in the server
# mod_dataGen_server("dataGen_1")
