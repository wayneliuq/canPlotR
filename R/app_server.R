#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  #### module servers ####

  ## ui inputs for loading user data or choosing example dataset
  mod_data_load <- mod_data_load_server("data_load_1")

  mod_choose_plotxy <- mod_choose_plotxy_server(
    "choose_plotxy_1",
    data_load_btn = mod_data_load$data_load_btn,
    x_factorlevels = x_factorlevels_default,
    data_vars = data_vars,
    data_vars_numeric = data_vars_numeric,
    color_factorlevels = color_factorlevels_default,
    facet_h_factorlevels = facet_h_factorlevels_default,
    facet_v_factorlevels = facet_v_factorlevels_default

  )

  mod_regression <- mod_regression_server(
    "regression_1",
    data_do = data_do,
    xtrans = mod_choose_plotxy$xtrans,
    ytrans = mod_choose_plotxy$ytrans
  )
  #### final output plot ####
  ## this is the ggplot2 function which will render the final plot

  final_ggplot <- reactive({
    ggplot(
      data = data_do(),
      mapping = aes(x = !!xvar_plot(),
                    y = y)
    ) +

      # custom geoms
      geomcust_bin2d() +
      geomcust_densityfilled() +
      geomcust_density() +
      geomcust_boxplot() +
      geomcust_violin() +
      geomcust_dotplot() +
      geomcust_point() +

      # regression geoms
      geom_regression() +

      # title
      labs(title = input$plotid,
           fill = legend_lab(),
           color = legend_lab()) +

      #x-axis label
      xlab(mod_choose_plotxy$xvar()) +

      #y-axis label
      ylab(mod_choose_plotxy$yvar()) +

      # transform the x and y axis depending on user input
      x_scale_trans() +
      y_scale_trans() +

      # facets
      facet_cust() +

      #theme, to be customized
      theme_bw()
  })

  output$plot <- renderPlot(
    final_ggplot(),
    res = 72,
    alt = "Drag the bottom right corner to resize the plot"
  )

  #### download ggplot handler ####
  output$plot_download <- downloadHandler(
    filename = function() {
      paste0(
        Sys.Date(),
        "_",
        input$plotid,
        ".",
        input$export_filetype
      )
    },

    content = function(file) {
      ggsave(
        filename = file,
        plot = final_ggplot(),
        device = input$export_filetype,
        scale = 1,
        width = 500, # calculate from dpi later
        height = 600,
        units = "px",
        dpi = as.integer(input$export_resolution)
      )
    }
  )

  #### misc labels ####
  ## legend titles
  # automatically take the column title
  # planned feature: allow user to customize it
  legend_lab <- reactive({
    first(c(mod_choose_plotxy$color_factor_var(), input$color_numeric_var))
  })

  #### customize type of plot ####
  ## mapping depending on whether color or fill needs to be changed

  colorvar_catch <- reactive({
      if (mod_choose_plotxy$color_factor_var() == "none" & input$color_numeric_var == "none") {
        F
      } else {T}
  })

  colorvar_levels <- reactive({
    if (isTruthy(mod_choose_plotxy$color_factor_var_order())) {
      mod_choose_plotxy$color_factor_var_order()
    } else {
      color_factorlevels_default()
    }
  })

  aes_cust_colour <- reactive({
    if (colorvar_catch()) {
      if (mod_choose_plotxy$color_factor_var() == "none") {
        aes(colour = colorNumeric)
      } else {
        aes(colour = factor(colorFactor, levels = colorvar_levels()))
      }
    } else {aes()}
  })

  aes_cust_fill <- reactive({
    if (colorvar_catch()) {
      if (mod_choose_plotxy$color_factor_var() == "none") {
        aes(fill = colorNumeric)
      } else {
        aes(fill = factor(colorFactor, levels = colorvar_levels()))
      }
    } else {aes()}
  })

  aes_cust_colourfill <- reactive({
    if (colorvar_catch()) {
      if (mod_choose_plotxy$color_factor_var() == "none") {
        aes(colour = colorNumeric,
            fill = colorNumeric)
      } else {
        aes(colour = factor(colorFactor, levels = colorvar_levels()),
            fill = factor(colorFactor, levels = colorvar_levels()))
      }
    } else {aes()}
  })

  ## individual geom functions

  ## continuous x & y
  geomcust_point <- reactive({
    if (input$geompoint) {
      geom_point(
        mapping = aes_cust_colour()
      ) #color (set color)
    }
  })

  geomcust_bin2d <- reactive({
    if (input$geombin2d) {
      geom_bin2d(bins = 25,
                 mapping = aes_cust_fill()) #fill (set fill)
    }
  })

  geomcust_density <- reactive({
    if (input$geomdensity) {
      geom_density2d(
        mapping = aes_cust_colour()
      ) #color (set color)
    }
  })

  geomcust_densityfilled <- reactive({
    if (input$geomdensityfilled) {
      geom_density2d_filled(
        mapping = aes()
      ) #this plot cannot be colored/grouped
    }
  })

  ## categorical x, continuous y
  geomcust_boxplot <- reactive({
    if (input$geomboxplot) {
      geom_boxplot(
        mapping = aes_cust_fill(),
        alpha = 0.5
      ) #fill is shading, color is border (set fill)
    }
  })

  geomcust_violin <- reactive({
    if (input$geomviolin) {
      geom_violin(scale = "area",
                  mapping = aes_cust_fill(),
                  alpha = 0.5) #fill is shading, color is border (set fill)
    }
  })

  geomcust_dotplot <- reactive({
    if (input$geomdotplot) {
      geom_dotplot(
        binaxis = "y",
        stackdir = "center",
        binwidth = 0.01 * diff(range(data_do()$y)),
        mapping = aes_cust_colourfill(),
        position = position_dodge(0.85)
      ) #fill is shading, color is border (set both)
    }
  })

  #### faceting data by user ####
  ## only facet_grid is planned to be supported

  ## get custom levels
  facet_hvar_levels <- reactive({
    if (isTruthy(mod_choose_plotxy$facet_hvar_order())) {
      mod_choose_plotxy$facet_hvar_order()
    } else {
      facet_h_factorlevels_default()
    }
  })

  facet_vvar_levels <- reactive({
    if (isTruthy(mod_choose_plotxy$facet_vvar_order())) {
      mod_choose_plotxy$facet_vvar_order()
    } else {
      facet_v_factorlevels_default()
    }
  })

  ## make facet_cust function

  facet_cust <- reactive({

    if (data_do()$facetHFactor |> is.null() & data_do()$facetVFactor |> is.null()) {
      facet_grid(
        cols = NULL,
        rows = NULL
      )
    } else if (data_do()$facetVFactor |> is.null()) {
      facet_grid(
        cols = facetHFactor |> factor(levels = facet_hvar_levels()) |> vars(),
        scales = "fixed"
      )
    } else if (data_do()$facetHFactor |> is.null()) {
      facet_grid(
        rows = facetVFactor |> factor(levels = facet_vvar_levels()) |> vars(),
        scales = "fixed"
      )
    } else {
      facet_grid(
        cols = facetHFactor |> factor(levels = facet_hvar_levels()) |> vars(),
        rows = facetVFactor |> factor(levels = facet_vvar_levels()) |> vars(),
        scales = "fixed"
      )
    }

  })

  #### regression ####

  # how to get x from y value with `uniroot` functions
  #  findInt <- function(model, value) {
  #   function(x) {
  #    predict(model, data.frame(x=x), type="response") - value
  #   }
  #  }
  #
  #   uniroot(findInt(drmod, 0.5), range(pull(example_dr, x)))$root |>
  #     tryCatch(error = function(e) warning("Out of bounds"))

  ## get variables for regression df

  TruthNoneOrNull <- function(x) {
    if (x != "none") {
      get(!!x) |> expr()
    } else NULL
  }

  color_factor_var_formdf <- reactive({
    TruthNoneOrNull(mod_choose_plotxy$color_factor_var())
  })# |> bindEvent(mod_choose_plotxy$color_factor_var())

  color_numeric_var_formdf <- reactive({
    TruthNoneOrNull(input$color_numeric_var)
  })# |> bindEvent(input$color_numeric_var)

  facet_hvar_formdf <- reactive({
    TruthNoneOrNull(mod_choose_plotxy$facet_hvar())
  })# |> bindEvent(mod_choose_plotxy$facet_hvar())

  facet_vvar_formdf <- reactive({
    TruthNoneOrNull(mod_choose_plotxy$facet_vvar())
  })# |> bindEvent(mod_choose_plotxy$facet_vvar())

  ## regression df

  data_do <- reactive({

    data_get()[, list(
      x = mod_choose_plotxy$xvar() |> get() |> tryCatch(error = function(e) 1),
      y = mod_choose_plotxy$yvar() |> get() |> tryCatch(error = function(e) 1),
      colorNumeric = color_numeric_var_formdf() |> eval(),
      colorFactor = color_factor_var_formdf() |> eval(),
      facetHFactor = facet_hvar_formdf() |> eval(),
      facetVFactor = facet_vvar_formdf() |> eval()
    )]

  })

  #### regression logics ####

  ## geom_line for drawing the regression predicted values

  geom_regression <- reactive({

    if (mod_regression$regression_conty() != "none") {

      if (data_do()$colorFactor |> is.null() |> suppressWarnings()) {
        geom_line(
          mapping = aes(
            y = y
          ),
          data = mod_regression$regrdf()
        )
      } else {
        geom_line(
          mapping = aes(
            y = y,
            color = colorFactor
          ),
          data = mod_regression$regrdf()
        )
      }

    }

  })

  #### load data ####

  ## get the real data
  ## data is bound to button since reading data can be slow
  data_get <- reactive({

    if (!mod_data_load$use_example_data()) {

      fread(mod_data_load$data_user_path())

    } else if (mod_data_load$example_dataset() == 1) {
      data("example_dr", envir = environment()); example_dr
    } else if (mod_data_load$example_dataset() == 2) {
      data("example_ChickWeight", envir = environment()); example_HairEye
    } else {
      data.table()
    }
  }) |>
    bindEvent(mod_data_load$data_load_btn())# bindEvent(input$data_load)

  #### summarise data ####
  ## later modify group_by so it incorporates facets, and mappings e.g. (color/fill)
  ## also include summary statistics where both variables are factors
  ## and where both variables are numeric

  ## to be updated later

  # data_summary <- reactive({
  #
  #   if (
  #     (xvar_iscategorical()) |>
  #     tryCatch(error = function(e) F)
  #   ) {
  #     data_get() |>
  #       group_by("x" = get(mod_choose_plotxy$xvar())) |> ## need to fix so summary displays the x variable name
  #       summarise(
  #         count = n(),
  #         mean = mean(get(mod_choose_plotxy$yvar()), na.rm = T),
  #         median = median(get(mod_choose_plotxy$yvar()), na.rm = T),
  #         "geometric_mean" = geomean(get(mod_choose_plotxy$yvar()), na.rm = T),
  #         variance = var(get(mod_choose_plotxy$yvar()), na.rm = T),
  #         "standard_deviation" = sd(get(mod_choose_plotxy$yvar()), na.rm = T),
  #         "standard_error_of_mean" = sd(get(mod_choose_plotxy$yvar()), na.rm = T) / sqrt(n()),
  #         "median_absolute_deviation" = mad(get(mod_choose_plotxy$yvar()), na.rm = T)
  #       )
  #   } else {
  #     tibble(error = "error")
  #   }
  #
  # })

  # output$datasummary <- reactable::renderReactable({
  #   reactable::reactable(data_summary(),
  #                        showPageSizeOptions = T,
  #                        pageSizeOptions = c(10, 25, 50, 100),
  #                        resizable = T,
  #                        defaultPageSize = 10)
  # })

  #### show data table preview ####
  output$datatable <- reactable::renderReactable({
    reactable::reactable(data_get(),
                         showPageSizeOptions = T,
                         pageSizeOptions = c(10, 25, 50, 100, 250, 500),
                         resizable = T,
                         defaultPageSize = 10)
  })

  #### check whether variables are numeric ####
  # error occurs before mod_choose_plotxy$xvar() is initialized, e.g. before user loads data
  # or switches to tab to select xvar and yvar
  # need to optimize script to avoid using tryCatch as it is computationally expensive

  xvar_isnumeric <- reactive({
    (data_get()[, mod_choose_plotxy$xvar() |> get() |> is.numeric()]) |>
      tryCatch(error = function(e) {F})
  })

  yvar_isnumeric <- reactive({
    (data_get()[, mod_choose_plotxy$yvar() |> get() |> is.numeric()]) |>
      tryCatch(error = function(e) {F})
  })

  xvar_iscategorical <- reactive({
    !xvar_isnumeric() | mod_choose_plotxy$x_asfactor()
  })

  # yvar_iscategorical <- reactive({
  #   !yvar_isnumeric() | input$y_asfactor
  # })

  ## send it to browser to update inputs
  output$xvar_isnumeric <- reactive(xvar_isnumeric())
  output$yvar_isnumeric <- reactive(yvar_isnumeric())
  output$xvar_isfactor <- reactive({xvar_iscategorical()})
  # output$yvar_isfactor <- reactive({yvar_iscategorical()})

  outputOptions(output, "xvar_isnumeric", suspendWhenHidden = F)
  outputOptions(output, "yvar_isnumeric", suspendWhenHidden = F)
  outputOptions(output, "xvar_isfactor", suspendWhenHidden = F)
  # outputOptions(output, "yvar_isfactor", suspendWhenHidden = F)

  #### update UI for coloring/splitting data ####

  ## add UI element to choose separating data
  data_vars <- reactive({
    (data_get() |> colnames()) # |>
    #  tryCatch(error = function(e) character(0))
  })

  ## separate numeric variables
  ## or else it will be too confusing for users, coloring vs splitting data

  data_vars_numeric <- reactive({
    data_get()[, names(.SD), .SDcols = is.numeric] #|>
      #tryCatch(error = function(e) character(0))
  })

  observe({

    shinyWidgets::updatePickerInput(
      session,
      inputId = "color_numeric_var",
      choices = c("none", data_vars_numeric())
    )

  })


  #### format x or y as factors and choose order ####

  ## function for ggplot mapping as factors
  xorder_catch <- reactive({
    (if (identical(length(mod_choose_plotxy$xorder()), length(x_factorlevels_default()))) {
      mod_choose_plotxy$xorder()
    } else {
      x_factorlevels_default()
    }) # |> tryCatch(error = function(e) x_factorlevels_default())
  })

  ## format x and y variables as ggplot mapping objects
  # If your wrapper has a more specific interface with named arguments,
  # you need "enquote and unquote":
  # scatter_by <- function(data, x, y) {
  #   x <- enquo(x)
  #   y <- enquo(y)
  #
  #   ggplot(data) + geom_point(aes(!!x, !!y))

  xvar_plot <- reactive({

    if (xvar_iscategorical()) {

      factor(
        x,
        levels = xorder_catch()
      ) |> expr()

    } else {

      x |> expr()

    }

  })


  #### numeric variable transformation ####
  # allow the user to set the scale transformation for
  # numeric x and y

  # TO BE IMPLEMENTED: check for variable type to determine which
  # transformations will succeed

  # this is copied from app_ui, maybe need to find a way to pull list directly from app_ui.R
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
    "time hms" = "hms"
  )

  x_scale_trans <- reactive({
    if (!xvar_iscategorical()) {
      scale_x_continuous(trans = mod_choose_plotxy$xtrans())
    }
  })

  y_scale_trans <- reactive({
    # if (!yvar_iscategorical()) {
      scale_y_continuous(trans = mod_choose_plotxy$ytrans())
    # }
  })

  ## if variable is factor, do not allow selection of transformation
  ## or user will be confused
  ## fixes are needed here, cannot apply transformation to numeric values
  observe({
    disabled_choices <- trans_continuous != "identity"

    if (xvar_iscategorical()) {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "xtrans",
        choices = trans_continuous,
        choicesOpt = list(disabled = disabled_choices,
                          style = ifelse(disabled_choices,
                                         yes = "color: rgba(119, 119, 119, 0.5);",
                                         no = ""))

      )
    } else {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "xtrans",
        choices = trans_continuous
      )
    }

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "ytrans",
        choices = trans_continuous
      )

  })

  #### get the factor levels of variables ####
  GetColLevelsCatch <- function(dat, col, error_output) {
    dat[[col]] |> factor() |> levels() |>
      tryCatch(error = function(e) error_output)
  }

  x_factorlevels_default <- reactive({
    GetColLevelsCatch(data_do(), "x", "NA")
  })

  color_factorlevels_default <- reactive({
    GetColLevelsCatch(data_do(), "colorFactor", "NA")
  })

  facet_h_factorlevels_default <- reactive({
    GetColLevelsCatch(data_do(), "facetHFactor", "NA")

  })

  facet_v_factorlevels_default <- reactive({
    GetColLevelsCatch(data_do(), "facetVFactor", "NA")
  })
  #### reorder factor levels observer ####

  # observe({
  #         shinyjqui::updateOrderInput(
  #           session,
  #           inputId = "xorder",
  #           items = x_factorlevels_default()
  #         )
  # })# |> bindEvent(mod_choose_plotxy$xvar())

  # observe({
  #   shinyjqui::updateOrderInput(
  #     session,
  #     inputId = "color_factor_var_order",
  #     items = color_factorlevels_default()
  #   )
  # })# |> bindEvent(mod_choose_plotxy$color_factor_var())

  # observe({
  #   shinyjqui::updateOrderInput(
  #     session,
  #     inputId = "facet_hvar_order",
  #     items = facet_h_factorlevels_default()
  #   )
  # })# |> bindEvent(mod_choose_plotxy$facet_hvar())
  #
  # observe({
  #   shinyjqui::updateOrderInput(
  #     session,
  #     inputId = "facet_vvar_order",
  #     items =facet_v_factorlevels_default()
  #   )
  # })# |> bindEvent(mod_choose_plotxy$facet_vvar())

  #### debug console ####
  output$debug <- renderTable({
    mod_regression$regrdf()
  })

  output$debug2 <- renderText({
  })

  #### session end scripts ####
  # session$onSessionEnded(function() {
  #
  #   ## remove uploaded data
  #   if (!is.null(input$data_user)) {
  #     file.remove(input$data_user$datapath)
  #   }
  #
  #   ## remove temporary files
  #   if (dir.exists(tempdir())) {
  #     unlink(tempdir(), recursive = T)
  #   }
  # })
}
