#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  #### final output plot ####
  # this is the ggplot2 function which will render the final plot

  final_ggplot <- reactive({
    ggplot(
      data = data_get(),
      mapping = aes(x = !!xvar_plot(),
                    y = !!yvar_plot())
    ) +

      # custom geoms
      geomcust_bin2d() +
      geomcust_densityfilled() +
      geomcust_density() +
      geomcust_boxplot() +
      geomcust_violin() +
      geomcust_dotplot() +
      geomcust_point() +
      geomcust_count() +

      # title
      labs(title = input$plotid,
           fill = legend_lab(),
           color = legend_lab()) +

      #x-axis label
      xlab(input$xvar) +

      #y-axis label
      ylab(input$yvar) +

      # transform the x and y axis depending on user input
      x_scale_trans() +
      y_scale_trans() +

      # facets
      facet_cust()+

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
    first(c(input$color_factor_var, input$color_numeric_var))
  })

  #### customize type of plot ####
  ## mapping depending on whether color or fill needs to be changed

  colorvar_catch <- reactive({
    if (isTruthy(input$color_factor_var) & isTruthy(input$color_numeric_var) |>
      tryCatch(error = function(e) F)) {
        if (input$color_factor_var == "none" & input$color_numeric_var == "none") {
          F
        } else {T}
      }
  })

  colorvar_levels <- reactive({
    if (isTruthy(input$color_factor_var_order)) {
      input$color_factor_var_order
    } else {
      color_factorlevels_default()
    }
  })

  aes_cust_colour <- reactive({
    if (colorvar_catch()) {
      if (input$color_factor_var == "none") {
        aes(colour = get(input$color_numeric_var))
      } else {
        aes(colour = factor(get(input$color_factor_var), levels = colorvar_levels()))
      }
    } else {aes()}
  })

  aes_cust_fill <- reactive({
    if (colorvar_catch()) {
      if (input$color_factor_var == "none") {
        aes(fill = get(input$color_numeric_var))
      } else {
        aes(fill = factor(get(input$color_factor_var), levels = colorvar_levels()))
      }
    } else {aes()}
  })

  aes_cust_colourfill <- reactive({
    if (colorvar_catch()) {
      if (input$color_factor_var == "none") {
        aes(colour = get(input$color_numeric_var),
            fill = get(input$color_numeric_var))
      } else {
        aes(colour = factor(get(input$color_factor_var), levels = colorvar_levels()),
            fill = factor(get(input$color_factor_var), levels = colorvar_levels()))
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
      geom_bin2d(bins = 40,
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
        binwidth = 0.01 * (max(data_get() |> select(rlang::sym(input$yvar)) |> unlist()) -
                             min(data_get() |> select(rlang::sym(input$yvar)) |> unlist())),
        mapping = aes_cust_colourfill(),
        position = position_dodge(0.85)
      ) #fill is shading, color is border (set both)
    }
  })
  ## categorical x & y
  geomcust_count <- reactive({
    if (input$geomcount) {
      geom_count(
        mapping = aes_cust_colourfill(),
        position = position_dodge(0.85)
      )
    }
  })

  #### faceting data by user ####
  ## only facet_grid is planned to be supported

  ## get custom levels
  facet_hvar_levels <- reactive({
    if (isTruthy(input$facet_hvar_order)) {
      input$facet_hvar_order
    } else {
      facet_h_factorlevels_default()
    }
  })

  facet_vvar_levels <- reactive({
    if (isTruthy(input$facet_vvar_order)) {
      input$facet_vvar_order
    } else {
      facet_v_factorlevels_default()
    }
  })

  ## make facet_cust function
  facet_hvar_catch <- reactive({
    if (isTruthy(input$facet_hvar) |> tryCatch(error = function(e) F)) {
      if (input$facet_hvar != "none") {
        input$facet_hvar |>
          get() |>
          factor(levels = facet_hvar_levels()) |>
          vars()
      } else {NULL}
    } else {NULL}
  })

  facet_vvar_catch <- reactive({
    if (isTruthy(input$facet_vvar) |> tryCatch(error = function(e) F)) {
      if (input$facet_vvar != "none") {
        input$facet_vvar |>
          get() |>
          factor(levels = facet_vvar_levels()) |>
          vars()
      } else {NULL}
    } else {NULL}
  })

  facet_cust <- reactive({
    facet_grid(
      cols = facet_hvar_catch(),
      rows = facet_vvar_catch(),
      scales = "fixed"
    )
  })

  #### load data ####

  ## get the real data
  ## data is bound to button since reading data can be slow
  data_get <- reactive({

    if (input$input_data_type == 1) {

      ## find the file type of the user uploaded data

      if (tools::file_ext(input$data_user$datapath) == "xls") {
        readxl::read_xls(input$data_user$datapath)
      } else if (tools::file_ext(input$data_user$datapath) == "xlsx") {
        readxl::read_xlsx(input$data_user$datapath)
      } else if (tools::file_ext(input$data_user$datapath) %in% c("txt", "csv")) {
        readr::read_delim(input$data_user$datapath)
      }

    } else if (input$input_data_type == 2) {
      data("example_dr", envir = environment()); example_dr
    } else {
        tibble()
    }
  }) |>
    bindEvent(input$data_load)

  #### summarise data ####
  ## later modify group_by so it incorporates facets, and mappings e.g. (color/fill)
  ## also include summary statistics where both variables are factors
  ## and where both variables are numeric
  data_summary <- reactive({

    if (
      (xvar_iscategorical() & !yvar_iscategorical()) |>
      tryCatch(error = function(e) F)
    ) {
      data_get() |>
        group_by("x" = get(input$xvar)) |> ## need to fix so summary displays the x variable name
        summarise(
          count = n(),
          mean = mean(get(input$yvar), na.rm = T),
          median = median(get(input$yvar), na.rm = T),
          "geometric_mean" = geomean(get(input$yvar), na.rm = T),
          variance = var(get(input$yvar), na.rm = T),
          "standard_deviation" = sd(get(input$yvar), na.rm = T),
          "standard_error_of_mean" = sd(get(input$yvar), na.rm = T) / sqrt(n()),
          "median_absolute_deviation" = mad(get(input$yvar), na.rm = T)
        )
    } else {
      tibble(error = "error")
    }

  })

  output$datasummary <- reactable::renderReactable({
    reactable::reactable(data_summary(),
                         showPageSizeOptions = T,
                         pageSizeOptions = c(10, 25, 50, 100),
                         resizable = T,
                         defaultPageSize = 10)
  })

  #### show data table preview ####
  output$datatable <- reactable::renderReactable({
    reactable::reactable(data_get(),
                         showPageSizeOptions = T,
                         pageSizeOptions = c(10, 25, 50, 100, 250, 500),
                         resizable = T,
                         defaultPageSize = 10)
  })

  #### check whether variables are numeric ####
  # error occurs before input$xvar is initialized, e.g. before user loads data
  # or switches to tab to select xvar and yvar
  # need to optimize script to avoid using tryCatch as it is computationally expensive

  xvar_isnumeric <- reactive({
    (data_get() |> select(rlang::sym(input$xvar)) |> unlist() |> is.numeric()) |>
      tryCatch(error = function(e) {F})
  })

  yvar_isnumeric <- reactive({
    (data_get() |> select(rlang::sym(input$yvar)) |> unlist() |> is.numeric()) |>
      tryCatch(error = function(e) {F})
  })

  xvar_iscategorical <- reactive({
    !xvar_isnumeric() | input$x_asfactor
  })

  yvar_iscategorical <- reactive({
    !yvar_isnumeric() | input$y_asfactor
  })

  ## send it to browser to update inputs
  output$xvar_isnumeric <- reactive(xvar_isnumeric())
  output$yvar_isnumeric <- reactive(yvar_isnumeric())
  output$xvar_isfactor <- reactive({xvar_iscategorical()})
  output$yvar_isfactor <- reactive({yvar_iscategorical()})

  outputOptions(output, "xvar_isnumeric", suspendWhenHidden = F)
  outputOptions(output, "yvar_isnumeric", suspendWhenHidden = F)
  outputOptions(output, "xvar_isfactor", suspendWhenHidden = F)
  outputOptions(output, "yvar_isfactor", suspendWhenHidden = F)

  #### update UI for coloring/splitting data ####

  ## add UI element to choose separating data
  data_vars <- reactive({
    (data_get() |> colnames()) |>
      tryCatch(error = function(e) character(0))
  })

  ## separate numeric variables
  ## or else it will be too confusing for users, coloring vs splitting data

  data_vars_numeric <- reactive({
    data_get() |> select(where(is.numeric)) |> colnames() |>
      tryCatch(error = function(e) character(0))
  })

  observe({
    shinyWidgets::updatePickerInput(
      session,
      inputId = "color_factor_var",
      choices = c("none", data_vars())
    )

    shinyWidgets::updatePickerInput(
      session,
      inputId = "color_numeric_var",
      choices = c("none", data_vars_numeric())
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

  }) |> bindEvent(data_vars())


  #### update column selection ####
  # bind it to data load button to increase efficiency
  observe({
    if (tryCatch(is.character(data_vars()), error = function(e) F)) {
      updateSelectInput(session,
                        inputId = "xvar",
                        choices = data_vars(),
                        selected = 1)

      updateSelectInput(session,
                        inputId = "yvar",
                        choices = data_vars(),
                        selected = 1)
            }
    }) |> bindEvent(input$data_load)

  #### format x or y as factors and choose order ####

  ## function for ggplot mapping as factors
  xorder_catch <- reactive({
    (if (identical(length(input$xorder), length(x_factorlevels_default()))) {
      input$xorder
    } else {
      x_factorlevels_default()
    }) # |> tryCatch(error = function(e) x_factorlevels_default())
  })

  yorder_catch <- reactive({
    (if (identical(length(input$yorder), length(y_factorlevels_default()))) {
      input$yorder
    } else {
      y_factorlevels_default()
    }) # |> tryCatch(error = function(e) y_factorlevels_default())
  })

  ## format x and y variables as ggplot mapping objects
  ## separate input$xvar and input$yvar for later greater modularity (e.g. adding mappings)
  # If your wrapper has a more specific interface with named arguments,
  # you need "enquote and unquote":
  # scatter_by <- function(data, x, y) {
  #   x <- enquo(x)
  #   y <- enquo(y)
  #
  #   ggplot(data) + geom_point(aes(!!x, !!y))

  xvar_plot <- reactive({
    (if (xvar_iscategorical()) {
      factor(
        get(input$xvar),
        levels = xorder_catch()
      ) |> expr()
    } else {
      get(input$xvar) |> expr()
    })
  })

  yvar_plot <- reactive({
    (if (yvar_iscategorical()) {
      factor(
        get(input$yvar),
        levels = yorder_catch()
      ) |> expr()
    } else {
      get(input$yvar) |> expr()
    })
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
    "time hms" = "hms",
    "time POSIX" = "time"
  )

  x_scale_trans <- reactive({
    if (!xvar_iscategorical()) {
      scale_x_continuous(trans = input$xtrans)
    }
  })

  y_scale_trans <- reactive({
    if (!yvar_iscategorical()) {
      scale_y_continuous(trans = input$ytrans)
    }
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

    if (yvar_iscategorical()) {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "ytrans",
        choices = trans_continuous,
        choicesOpt = list(disabled = disabled_choices,
                          style = ifelse(disabled_choices,
                                         yes = "color: rgba(119, 119, 119, 0.5);",
                                         no = ""))

      )
    } else {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "ytrans",
        choices = trans_continuous
      )
    }
  })

  #### get the factor levels of variables ####
  x_factorlevels_default <- reactive({
    (data_get() |> select(rlang::sym(input$xvar)) |> unlist() |> factor() |> levels()) |>
      tryCatch(error = function(e) "NA")
  })

  y_factorlevels_default <- reactive({
    (data_get() |> select(rlang::sym(input$yvar)) |> unlist() |> factor() |> levels()) |>
      tryCatch(error = function(e) "NA")
  })

  color_factorlevels_default <- reactive({
    (data_get() |> select(rlang::sym(input$color_factor_var)) |> unlist() |> factor() |> levels()) |>
      tryCatch(error = function(e) "NA")
  })

  facet_h_factorlevels_default <- reactive({
    (data_get() |> select(rlang::sym(input$facet_hvar)) |> unlist() |> factor() |> levels()) |>
      tryCatch(error = function(e) "NA")
  })

  facet_v_factorlevels_default <- reactive({
    (data_get() |> select(rlang::sym(input$facet_vvar)) |> unlist() |> factor() |> levels()) |>
      tryCatch(error = function(e) "NA")
  })
  #### reorder factor levels observer ####

  observe({
          shinyjqui::updateOrderInput(
            session,
            inputId = "xorder",
            items = x_factorlevels_default()
          )
  }) |> bindEvent(input$xvar)

  observe({
    shinyjqui::updateOrderInput(
      session,
      inputId = "yorder",
      items = y_factorlevels_default()
    )
  }) |> bindEvent(input$yvar)

  observe({
    shinyjqui::updateOrderInput(
      session,
      inputId = "color_factor_var_order",
      items = color_factorlevels_default()
    )
  }) |> bindEvent(input$color_factor_var)

  observe({
    shinyjqui::updateOrderInput(
      session,
      inputId = "facet_hvar_order",
      items = facet_h_factorlevels_default()
    )
  }) |> bindEvent(input$facet_hvar)

  observe({
    shinyjqui::updateOrderInput(
      session,
      inputId = "facet_vvar_order",
      items =facet_v_factorlevels_default()
    )
  }) |> bindEvent(input$facet_vvar)

  #### debug console ####
  output$debug <- renderText({

    paste0("unlist as.numeric: ", as.numeric(unlist(input$plot_size)))

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
