#' regression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_regression_ui <- function(id){
  ns <- NS(id)
  tagList(

    #### choose type of regression ####
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
            "quadratic" = "quadratic",
            "cubic" = "cubic",
            "logistic" = "glm_binomial"
          )
        )
      )
    )

  )
}

#' regression Server Functions
#'
#' @noRd
mod_regression_server <- function(
  id,
  data_do,
  xtrans,
  ytrans
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### filter based on axis transformation ####
    ## later build some error handling in case there is nothing left after filter
    regr_data_filterx <- reactive({
      if (xtrans() %in% c("log10", "log2", "log")) {
        data_do()[x > 0]
      } else if (xtrans() %in% c("sqrt")) {
        data_do()[x >= 0]
      } else if (xtrans() %in% c("logit", "probit")) {
        data_do()[x > 0 & x < 1]
      } else {data_do()}
    })

    regr_data_filtery <- reactive({
      if (ytrans() %in% c("log10", "log2", "log")) {
        regr_data_filterx()[y > 0]
      } else if (ytrans() %in% c("sqrt")) {
        regr_data_filterx()[y >= 0]
      } else if (ytrans() %in% c("logit", "probit") | input$regression_conty %in% c("glm_binomial")) {
        regr_data_filterx()[y > 0 & y < 1]
      } else {regr_data_filterx()}
    })

    #### regression model formula ####
    formula_x <- function() {
      if (xtrans() %in% c("log10", "log2", "log", "sqrt", "exp")) {
        paste0(xtrans(), "(x)")
      } else if (xtrans() == "logit") {
        "qlogis(x)"
      } else if (xtrans() == "probit") {
        "qnorm(x)"
      } else {
        "x"
      }
    }

    formula_y <- function() {
      if (ytrans() %in% c("log10", "log2", "log", "sqrt", "exp")) {
        paste0(ytrans(), "(y)")
      } else if (ytrans() == "logit") {
        "qlogis(y)"
      } else if (ytrans() == "probit") {
        "qnorm(y)"
      } else {
        "y"
      }
    }

    regr_formula <- reactive({
      if (input$regression_conty == "quadratic") {
        paste0(
          formula_y(),
          " ~ poly(",
          formula_x(),
          ", degree = 2, raw = TRUE)"
        ) |> as.formula()
      } else if (input$regression_conty == "cubic") {
        paste0(
          formula_y(),
          " ~ poly(",
          formula_x(),
          ", degree = 3, raw = TRUE)"
        ) |> as.formula()
      } else {
          paste0(
            formula_y(),
            " ~ ",
            formula_x()
          ) |> as.formula()
      }

    })

    #### model fitting function####
    regr_model_fct <- function(dat, formula) {
      if (input$regression_conty %in% c("lm", "quadratic", "cubic")) {

        lm(
          formula = formula,
          data = dat
        )

      } else if (input$regression_conty == "loess") {

        loess(
          formula = formula,
          data = dat,
          span = 0.5
        )

      } else if (input$regression_conty == "glm_binomial") {

        glm(
          formula = formula,
          data = dat,
          family = binomial
        )

      }
    }

    #### find the factors that split the regression ####
    ## list all factors
    regr_factors <- reactive({
      setdiff(regr_data_filtery() |> colnames(), c("x", "y"))
    })

    ## true or false, any factors found?
    data_anyFactor <- reactive({
      any(grep("Factor", colnames(regr_data_filtery())))
    })

    #### split data based on factors ####
    data_split <- reactive({
      if (input$regression_conty != "none") {
        if (data_anyFactor()) {
          split(regr_data_filtery(), by = regr_factors(), keep.by = T)
        } else {
          regr_data_filtery()
        }
      } else {NULL}
    })

    #### model fitting ####
    regr_modellist <- reactive({
      if (input$regression_conty != "none" & !is.null(data_split())) {
        if (data_anyFactor()) {
          lapply(
            data_split(),
            regr_model_fct,
            formula = regr_formula()
          )
        } else {
          regr_model_fct(dat = data_split(), formula = regr_formula())
        }
      } else {NULL}
    })

    #### table for plotting regression results
    # example_dr[, list(x = seq(
    #   from = min(Viability),
    #   to = max(Viability),
    #   length.out = 100
    # )), by = .(Cell, Compound)]
    regrdf <- reactive({

      if (input$regression_conty != "none" & !is.null(regr_modellist())) {

        nrow = 100 ## later set higher or make it tunable?

        if (data_anyFactor()) {

          df <- rep(data.table(0), length(data_split()))

          for (i in seq_along(data_split())) {
            df[[i]] <- data_split()[[i]][, list(
              x = seq(
                from = x |> transvar(fct = xtrans()) |> min(),
                to = x |> transvar(fct = xtrans()) |> max(),
                length.out = nrow
              ) |> inversetrans(fct = xtrans()),
              y = 0,
              .SD[1]
            ), .SDcols = patterns("Factor")]

            if (input$regression_conty == "loess") {
              predi <- predict(regr_modellist()[[i]], newdata = df[[i]], se = T)

              df[[i]][, `:=`(
                y = inversetrans(fct = ytrans(), predi$fit),
                y_setop = seFind(fct = max, fit = predi$fit, se = predi$se.fit, trans = ytrans()),
                t_sebot = seFind(fct = min, fit = predi$fit, se = predi$se.fit, trans = ytrans())
              )]
            } else {
              predi <- predict(regr_modellist()[[i]], newdata = df[[i]], se.fit = T)

              df[[i]][, `:=`(
                y = inversetrans(fct = ytrans(), predi$fit),
                y_setop = seFind(fct = max, fit = predi$fit, se = predi$se.fit, trans = ytrans()),
                t_sebot = seFind(fct = min, fit = predi$fit, se = predi$se.fit, trans = ytrans())
              )]
            }

          }

          df <- rbindlist(df)

        } else {
          df <- data_split()[, list(
            x = seq(
              from = x |> transvar(fct = xtrans()) |> min(),
              to = x |> transvar(fct = xtrans()) |> max(),
              length.out = nrow
            ) |> inversetrans(fct = xtrans()),
            y = 0,
            .SD[1]
          ), .SDcols = patterns("")]

          if (input$regression_conty == "loess") {
            predi <- predict(regr_modellist(), newdata = df, se = T)

            df[, `:=`(
              y = inversetrans(fct = ytrans(), predi$fit),
              y_setop = seFind(fct = max, fit = predi$fit, se = predi$se.fit, trans = ytrans()),
              t_sebot = seFind(fct = min, fit = predi$fit, se = predi$se.fit, trans = ytrans())
            )]
          } else {
            predi <- predict(regr_modellist(), newdata = df, se.fit = T)

            df[, `:=`(
              y = inversetrans(fct = ytrans(), predi$fit),
              y_setop = seFind(fct = max, fit = predi$fit, se = predi$se.fit, trans = ytrans()),
              t_sebot = seFind(fct = min, fit = predi$fit, se = predi$se.fit, trans = ytrans())
            )]
          }

        }

        return(df)

      } else {
        data.table()
      }

    })


    #### return Values ####
    returnVals = reactiveValues(
      regrdf = regrdf,
      regression_conty = reactive(input$regression_conty)
    )

    return(returnVals)

  })
}

## To be copied in the UI
# mod_regression_ui("regression_1")

## To be copied in the server
# mod_regression_server("regression_1")
