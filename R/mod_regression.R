#' regression UI Function
#'
#' @description A shiny Module for generating plot elements and statistics for
#' regression.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regression_ui <- function(id){
  ns <- NS(id)
  tagList(

    #### regression UI (for numeric x and y only)####
    conditionalPanel(
      condition = "output.xvar_isnumeric",

      fluidRow(
        p(strong("Regression for continuous y variables")),

        shinyWidgets::pickerInput(
          inputId = ns("regression_conty"),
          label = "Select regression model:",
          choices = c(
            "none",
            "linear regression" = "lm",
            "local polynomial (smooth)" = "loess",
            "quadratic" = "loess_quadratic",
            "cubic" = "loess_cubic",
            "logistic regression" = "glm_logistic"
          )
        )
      )
    )

  )
}

#' regression Server Functions
#'
#' @noRd
mod_regression_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### perform regression on split data ####
    data_do_split <- reactive({
      if (any(grepl("Factor", colnames(data_do())))) {
        data_do() |>
          group_by(
            color_factor_var_formdf(),
            facet_hvar_formdf(),
            facet_vvar_formdf()
          ) |>
          group_split()
      } else {data_do()}
    })

    #### function to perform and predict regression ####
    ifListLapply <- function(x, fct, ...) {
      if ("list" %in% class(x)) {
        lapply(FUN = fct, x, ...) |> suppressWarnings()
      } else {fct(x, ...)  |> suppressWarnings()}
    }

    ## perform model fitting
    regr_model <- reactive({

      if (input$regression_conty == "lm") {
        ifListLapply(x = data_do_split(),
                     fct = lm,
                     formula = y ~ x)
      } else if (input$regression_conty == "loess") {
        ifListLapply(x = data_do_splot(),
                     fct = loess,
                     formula = y ~ x)
      }

    })

    ## perform predictions
    regr_predict <- reactive({
      if (input$regression_conty == "lm") {
        ifListLapply(x = regr_model(),
                     fct = predict,
                     se.fit = T)
      } else if (input$regression_conty == "loess") {
        ifListLapply(x = regr_model(),
                     fct = predict,
                     se = T)
      }
    })

    ## mapply predictions to data frame
    

    #### deprecated below here ####
    ## regression formula
#     regr_formula <- reactive({
#       if (any(grepl("Factor", colnames(data_do())))) {
#         paste0(
#           "y ~ x + ",
#           (grep("Factor", colnames(data_do()), value = T) |> paste(collapse = " + "))) |> as.formula()
#       } else {
#         y ~ x
#       }
#     })
#
#     #### regression model ####
#     regr_model <- reactive({
#
#       if (input$regression_conty == "lm") {
#         lm(regr_formula(), data = data_do())
#       }
#
#     })
#
#
#   })
# }

## To be copied in the UI
# mod_regression_ui("regression_1")

## To be copied in the server
# mod_regression_server("regression_1")
