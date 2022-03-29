#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  #### common functions ####
  
  ## check whether variables are numeric
  ##need fixes here
  xvar_isnumeric <- reactive({
    if (!exists("input$xvar")) {
      F
    } else {
      data_get() |> select(input$xvar) |> unlist() |> is.numeric()
    }
  })
  
  yvar_isnumeric <- reactive({
    if (!exists("input$yvar")) {
      F
    } else {
      data_get() |> select(input$yvar) |> unlist() |> is.numeric()
    }
  })
  
  ## get the factor levels of variables
  x_factorlevels_default <- reactive({
    data_get() |> select(input$xvar) |> unlist() |> factor() |> levels()
  })
  
  y_factorlevels_default <- reactive({
    data_get() |> select(input$yvar) |> unlist() |> factor() |> levels()
  })
  
  #### final output plot ####
  # this is the ggplot2 function which will render the final plot
  
  output$plot <- renderPlot(
    ggplot(
      data = data_get(), 
      mapping = aes_cust()
    ) +

      # custom geom
      geomcust_boxplot() +
      geomcust_line() +
      geomcust_point() +

      # title
      labs(title = input$plotid) +
      
      #x-axis label
      xlab(input$xvar) +
      
      #y-axis label
      ylab(input$yvar) +
      
      # transform the x and y axis depending on user input
      x_scale_trans() +
      y_scale_trans() +
      
      #theme, to be customized
      theme_bw()
  ) #|> 
  # plot only updates when button is pressed
  # remove dependency until large data testing
  # bindEvent(input$makeplot)

  #### load data ####
  ## file import
  ## if user select to upload, show the upload box
  output$data_ui <- renderUI({
    if (input$input_data_type == 1) {
      prompter::add_prompt(
        fileInput(inputId = "data_user",
                  label = "Upload your data",
                  accept = c(".csv", ".txt", ".xls", ".xlsx")),
        position = "right",
        message = "Supported filetypes include 'xls', 'xlsx', 'csv', and 'txt'. By default, only the first sheet of Excel spreadsheets will be loaded.",
        size = "large"
      )
      
    } else if (input$input_data_type == 2) {
      renderText("Example dose-response data loaded.")
    }
  })
  
  ## get the real data
  data_get <- reactive({
    ## later fix so no data won't generate error in reactable output
    ## user uploaded data
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
      tibble(A = c(1,2,3), B = c(5,6,7))
    }
  })
  
  #### show data table preview ####
  output$datatable <- reactable::renderReactable({
    
    reactable::reactable(data_get(),
                         showPageSizeOptions = T,
                         pageSizeOptions = c(10, 25, 50, 100, 250, 500),
                         resizable = T,
                         defaultPageSize = 25)
  })
  
  #### choose columns ####

  ## choose x and y
  output$chooseX <- renderUI({
    varSelectInput(inputId = "xvar",
                label = "Choose x variable",
                data = data_get(),
                selectize = F,
                size = 4)
  })
  
  output$chooseY <- renderUI({
    varSelectInput(inputId = "yvar",
                   label = "Choose y variable",
                   data = data_get(),
                   selectize = F,
                   size = 4)
  })
  
  #### format x or y as factors ####
 
  ## function for ggplot mapping as factors
  
  ## conditionally set ggplot aes() mapping as factors
  # this has the advantage of easily incorporating the z variable for drawing heatmaps??
  
  aes_cust <- reactive({
    if ((input$x_asfactor & input$y_asfactor) | (!xvar_isnumeric() & !yvar_isnumeric())) {
      aes(x = factor(get(input$xvar), levels = input$xorder), y = factor(get(input$yvar), levels = input$yorder))
    } else if ((input$x_asfactor | !xvar_isnumeric()) & yvar_isnumeric() & !input$y_asfactor) {
      aes(x = factor(get(input$xvar), levels = input$xorder), y = get(input$yvar))
    } else if ((input$y_asfactor | !yvar_isnumeric()) & !input$x_asfactor & xvar_isnumeric()) {
      aes(x = get(input$xvar), y = factor(get(input$yvar), levels = input$yorder))
    } else {
      aes(x = get(input$xvar), y = get(input$yvar))
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
    "time hms" = "hms",
    "time POSIX" = "time"
  )
  
  x_scale_trans <- reactive({
    if (xvar_isnumeric() & isFALSE(input$x_asfactor)) {
      scale_x_continuous(trans = input$xtrans)
    }
  })
  
  y_scale_trans <- reactive({
    if (yvar_isnumeric() & isFALSE(input$y_asfactor)) {
      scale_y_continuous(trans = input$ytrans)
    }
  })
  
  ## if variable is factor, do not allow selection of transformation
  ## or user will be confused
  # observe(!xvar_isnumeric() | input$x_asfactor) |>
  #   bindEvent({
  #     disabled_choices <- trans_continuous == "identity"
  #     shinyWidgets::updatePickerInput(
  #       session = session,
  #       inputId = "xtrans",
  #       choices = "none",
  #       choicesOpt = list(
  #         disabled = disabled_choices,
  #         style = ifelse(disabled_choices,
  #                        yes = "color: rgba(119, 119, 119, 0.5);",
  #                        no = "")
  #       )
  #     )
  #   })
  # 
  
  observeEvent((!xvar_isnumeric() | input$x_asfactor), {
    disabled_choices <- trans_continuous != "identity"
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "xtrans",
      choices = trans_continuous,
      choicesOpt = list(
        disabled = disabled_choices,
        style = ifelse(disabled_choices,
                       yes = "color: rgba(119, 119, 119, 0.5);",
                       no = "")
      )
    )
  }, ignoreInit = T)
  # 
  # observeEvent((!yvar_isnumeric() | input$y_asfactor), {
  #   disabled_choices <- "none"
  #   shinyWidgets::updatePickerInput(
  #     session = session,
  #     inputId = "ytrans",
  #     choices = "none",
  #     choicesOpt = list(
  #       disabled = disabled_choices,
  #       style = ifelse(disabled_choices,
  #                      yes = "color: rgba(119, 119, 119, 0.5);",
  #                      no = "")
  #     )
  #   )
  # }, ignoreInit = T)
  
  #### reorder x and y variable ui ####
  output$orderX <- renderUI({
    if (!xvar_isnumeric | input$x_asfactor) {
      sortable::rank_list(
        text = "Category order",
        labels = x_factorlevels_default(),
        input_id = "xorder",
        options = sortable::sortable_options(multiDrag = T)
      )
    } else {"Not application for continuous x variable"}
  })
  
  output$orderY <- renderUI({
    if (!yvar_isnumeric | input$y_asfactor) {
      sortable::rank_list(
        text = "Category order",
        labels = y_factorlevels_default(),
        input_id = "yorder",
        options = sortable::sortable_options(multiDrag = T)
      )
    } else {"Not applicable for continuous y variable"}
  })

  #### customize type of plot ####

  ## individual geom functions
  geomcust_point <- reactive({
    if (input$geompoint) {
      geom_point()
    }
  })
  
  geomcust_line <- reactive({
    if(input$geomline) {
      geom_line()
    }
  })
  
  geomcust_boxplot <- reactive({
    if(input$geomboxplot) {
      geom_boxplot()
    }
  })
  
  #### debug console ####
  output$debug <- renderText({
    input$xvar
    exists("input$xvar")
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
