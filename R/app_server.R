#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  #### plot elements as reactive expressions ####
  
 
  
  #### final output plot ####
  # this is the ggplot2 function which will render the final plot
  
  output$plot <- renderPlot(
    ggplot(
      data_trans(), 
      aes(x = get(input$xvar),
          y = get(input$yvar))
    ) +
      geom_point() +
      
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
  ) |> 
    # plot only updates when button is pressed
    bindEvent(input$makeplot)

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
    }
  })
  
  #### show data table preview ####
  output$datatable <- reactable::renderReactable({
    
    reactable::reactable(data_trans(),
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
  # for numeric columns, allow the user to optionally 
  # format these columns as factors
  
  output$formatfactorx <- renderUI({
    if (data_get() |> 
        dplyr::select(input$xvar) |> 
        unlist() |> 
        is.numeric()) {
      
      prompter::add_prompt(
        checkboxInput(
          inputId = "x_asfactor",
          label = "Format as factor",
          value = FALSE
        ),
        position = "right",
        size = "large",
        message = "The x variable was detected as a continuous (numeric) variable. Check this box to format it as a categorical variable (a.k.a factor)"
      )

    }
  })
  
  output$formatfactory <- renderUI({
    if (data_get() |> 
        dplyr::select(input$yvar) |> 
        unlist() |> 
        is.numeric()) {
      
      prompter::add_prompt(
        checkboxInput(
          inputId = "y_asfactor",
          label = "Format as factor",
          value = FALSE
        ),
        position = "right",
        size = "large",
        message = "The y variable was detected as a continuous (numeric) variable. Check this box to format it as a categorical variable (a.k.a factor)"
      )
      
    }
  })
  
  ## mutate data as factors
  ## ?dplyr_data_masking to see data masking
  ## example_dr |> dplyr::mutate("{a}" := factor(.data[[a]]))
  
    data_trans <- reactive({
    if (!exists("input$x_asfactor") | !exists("input$y_asfactor")) {
      data_get()
    } else if (input$x_asfactor & input$y_asfactor) {
      data_get() |> 
        mutate(
          "{input$xvar}" := factor(.data[[input$xvar]]),
          "{input$yvar}" := factor(.data[[input$yvar]])
        )
    } else if (input$x_asfactor) {
      data_get() |> 
        mutate(
          "{input$xvar}" := factor(.data[[input$xvar]])
        )
    } else if (input$y_asfactor) {
      data_get() |> 
        mutate(
          "{input$yvar}" := factor(.data[[input$yvar]])
        )
    } else {
      data_get()
    }
  })
  
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
  
  #### numeric variable transformation ####
  # allow the user to set the scale transformation for 
  # numeric x and y
  
  # TO BE IMPLEMENTED: check for variable type to determine which
  # transformations will succeed
  
  x_scale_trans <- reactive({
    if (data_trans() |> 
        select(input$xvar) |> 
        unlist() |> 
        is.numeric()) {
      scale_x_continuous(trans = input$xtrans)
    }
  })
  
  y_scale_trans <- reactive({
    if (data_trans() |> 
        select(input$yvar) |> 
        unlist() |> 
        is.numeric()) {
      scale_y_continuous(trans = input$ytrans)
    }
  })
  
  output$transX <- renderUI({
    if (data_trans() |> 
        select(input$xvar) |> 
        unlist() |> 
        is.numeric()) {
      selectInput(inputId = "xtrans",
                  label = "x-axis transformation",
                  choices = trans_continuous,
                  selected = 1)
    } else if (data_trans() |> 
               select(input$xvar) |> 
               unlist() |> 
               is.factor()) {
      sortable::rank_list(
        text = "Category order",
        labels = "test",
        input_id = "xorder",
        options = sortable::sortable_options(multiDrag = T)
      )
    }
  })
  
  output$transY <- renderUI({
    if (data_trans() |> 
        select(input$yvar) |> 
        unlist() |> 
        is.numeric()) {
      selectInput(inputId = "ytrans",
                  label = "y-axis transformation",
                  choices = trans_continuous,
                  selected = 1)
    } else if (data_trans() |> 
               select(input$yvar) |> 
               unlist() |> 
               is.factor()) {
      sortable::rank_list(
        text = "y-axis order",
        labels = "test",
        input_id = "yorder",
        options = sortable::sortable_options(multiDrag = T)
      )
    }
  })

  #### debug console ####
  output$debug <- renderText({
    is.null(input$x_asfactor)
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
