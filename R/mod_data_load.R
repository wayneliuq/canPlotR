#' data_load UI Function
#'
#' @description A shiny module which contains the UI for uploading
#' user data or selecting example datasets. The module server passes
#' inputs from the UI to the final app.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_load_ui <- function(id){

  ns <- NS(id)

  #### use_example_data ####
  tagList(
    ## specify which example dataset to use
    conditionalPanel(
      condition  = "input.use_example_data",
      ns = ns,
      selectInput(
        inputId = ns("example_dataset"),
        label = "Select example dataset",
        choices = c(
          "Example dose-response" = 1,
          "Hair Eye Color" = 2
        )
      )
    ),

    #### upload box for user data
    conditionalPanel(
      condition = "input.use_example_data == false",
      ns = ns,
      fileInput(inputId = ns("data_user"),
                label = "Upload your data",
                accept = c(".csv", ".txt", ".xls", ".xlsx"),
                placeholder = "csv, txt, xls, and xlsx files") |>
        prompter::add_prompt(
          position = "right",
          message = "Supported filetypes include 'xls', 'xlsx', 'csv', and 'txt'.
						                      By default, only the active sheet of Excel spreadsheets will be loaded.",
          size = "large")
    ),

    ## action button bound to data loading
    actionButton(inputId = ns("data_load"),
                 label = "Load Data!",
                 class = "btn-success") |>
      prompter::add_prompt(
        position = "right",
        message = "Click here to load the data you selected.",
        size = "medium"
      ),

    ## use example data
    checkboxInput(
      inputId = ns("use_example_data"),
      label = "Use example data",
      value = F
    )
  )


}

#' data_load Server Functions
#'
#' @noRd
mod_data_load_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## send module UI inputs to the server
    returnVals <- reactiveValues(
      data_load_btn = reactive(input$data_load),
      data_user_path = reactive(input$data_user$datapath),
      use_example_data = reactive(input$use_example_data),
      example_dataset = reactive(input$example_dataset)
    )

    return(returnVals)

  })

}

## To be copied in the UI
# mod_data_load_ui("data_load_1")

## To be copied in the server
# mod_data_load_server("data_load_1")
