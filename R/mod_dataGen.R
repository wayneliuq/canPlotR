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

    ## the module
    fluidRow(
      p(strong("Seed")),
      #### UI: dataGen settings input ####
      column(
        width = 8,
        numericInput(
          inputId = ns("dataGen_seed"),
          value = as.integer(Sys.time()),
          label = NULL,
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
        session,
        inputId = "dataGen_seed",
        value = sample(0:9, 10, replace = T) |> paste(collapse = "") |> as.numeric()
      )
    }) |> bindEvent(input$dataGen_seed_randbutton)


  })
}

## To be copied in the UI
# mod_dataGen_ui("dataGen_1")

## To be copied in the server
# mod_dataGen_server("dataGen_1")
