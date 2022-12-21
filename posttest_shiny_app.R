# Working directory -------------------------------------------
setwd("C:/Users/Maximilian/Documents/Repositories/Postttest-Probability-Shiny-App")

# Packages ----------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(reactlog)
library(DT)
library(plotly)
#library(thematic)
library(riskyr)

# Global options ----------------------------------------------
options(shiny.reactlog = TRUE)
#thematic_shiny()

# function source ---------------------------------------------
source("multiple_post_probs.R")

# Global functions --------------------------------------------
# define function to create dynamic input names for tabs
# with consistent naming sheme name_id
shinyInput <- function(name, id) paste(name, id, sep = "_")

### Themeselector as input -------------------------------

themeSelector2 <- function() {
  div(
    div(
      selectInput("shinytheme-selector", "Choose a theme",
        c("default", shinythemes:::allThemes()),
        selectize = FALSE
      )
    ),
    tags$script(
      "$('#shinytheme-selector')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'bootstrap';
        curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
        } else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
    )
  )
}

### Data Table NA display options ----------------------------
rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"
)
### Reactive plotly resizing workaround ----------------------

# Ui ---------------------------------------------------------
ui <- fluidPage(
  withMathJax(),
  tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
  navbarPage("Navbar",
  ## PPC tab ----------------------------------------------------
    tabPanel("PPC",                  
      titlePanel("Posttest Probability Calculator"),
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            id = "tabs",
            # Static Test 1 tab
            tabPanel(
              title = "Test 1",
              value = "Test1",

              # Content of Test 1 tab :
              helpText("You can enter your relevant 
                                        test characteristics"),
              numericInput("sens_1",
                        label = h4("Sensitvitiy"),
                        value = 0.5,
                        min = 0,
                        max = 1,
                        step = 0.1
                      ),
               numericInput("spec_1",
                          label = h4("Specificity"),
                          value = 0.5,
                          min = 0,
                          max = 1,
                          step = 0.1
                        ),
              numericInput("br",
                          label = h4("Base rate"),
                          value = 0.5,
                          min = 0,
                          max = 1,
                          step = 0.1
                        ),
              selectInput("result_1",
                label = h4("Test result"),
                choices = list(
                  "Positive" = "pos",
                  "Negative" = "neg"
                ),
                selected = "Positive"
              ),
              selectInput("method",
                label = h4("Method"),
                choices = list(
                  "Fast" = "fast",
                  "Detail" = "detail"
                ),
                selected = "Fast"
              ),
              actionButton("add", "Add test",
                icon = icon("plus-circle")
              ),
              br(),
              br(),
              actionButton("calc", "Calculate posttest probability",
                icon = icon("calculator")
              ),
              themeSelector2(),
            )
          )
        ),

        # Main Panel for Output
        mainPanel(
          tabsetPanel(
            id = "output_tabs",
            tabPanel(
              title = "Text",
              p(
                "The final posttest probability of disease is",
                textOutput("post_prob")
              ),
              textOutput("track"),
            ),
            tabPanel(
              title = "Table",
              dataTableOutput("test"),
              br(),
              radioButtons(
                inputId = "filetype",
                label = "Select filetype:",
                choices = c("csv", "tsv"),
                selected = "csv"
              ),
              br(),
              downloadButton("download_data", "Download data")
            ),
            tabPanel(
              title = "Plot",
              plotlyOutput(
                outputId = "roc_plot",
                width = "auto",
                height = "auto"
              )
            )
          )
        )
      )
    ),
    ## 2x2 tab -------------------------------------------------------
    tabPanel("2x2",
      titlePanel("2x2 Table Display"),
      sidebarLayout(
        sidebarPanel(
              title = "2 x 2",
              value = "2x2_sidebar",
              # Content
              helpText("You can enter your relevant 
                                        test characteristics"),
              h4("True positives"),
              radioButtons("type_inp_tp", label = NULL,
                           choices = c("Numeric", "Sliders"),
                           selected = "Numeric",
                           inline = TRUE),
              uiOutput("tp_UI"),
              h4("False positives"),
              radioButtons("type_inp_fp", label = NULL,
                           choices = c("Numeric", "Sliders"),
                           selected = "Numeric",
                           inline = TRUE),
              uiOutput("fp_UI"),
              h4("True negatives"),
              radioButtons("type_inp_tn", label = NULL,
                           choices = c("Numeric", "Sliders"),
                           selected = "Numeric",
                           inline = TRUE),
              uiOutput("tn_UI"),
              h4("False negatives"),
              radioButtons("type_inp_fn", label = NULL,
                           choices = c("Numeric", "Sliders"),
                           selected = "Numeric",
                           inline = TRUE),
              uiOutput("fn_UI"),
              actionButton("go_2x2", "Go",
                icon = icon("calculator")
              )
        ),

        # Main Panel for Output
        mainPanel(
          tabsetPanel(
            id = "2x2_output_tabs",
            tabPanel(
              title = "2x2 Table",
              tableOutput("table_2x2_out"),
              br(),
              uiOutput("sens_2x2_out"), 
              br(),
              br(),
              uiOutput("spec_2x2_out"),
              br(),
              br(),
              uiOutput("br_2x2_out")
            ),
            tabPanel(
              title = "Colorized Table",
              plotOutput("col_table_out"),
              p("Created using the", 
                a("riskyr", href = "https://cran.r-project.org/web/packages/riskyr/index.html"),  "package")
            ),
            tabPanel(
              title = "Tree Plot",
              plotOutput("tree_plot_out")
            ),
            tabPanel(
              title = "Area Plot",
              plotOutput("area_plot_out")
            )
          )
        )
      )
    ),
    tabPanel("customize plot colors"),
    tabPanel("customize naming sheme"),
    tabPanel("About")
  )
)

# Server -----------------------------------------------------
server <- function(input, output, session) {
## PPC contents --------------------------------------------
  
  ### Tab creation -------------------------------------------
  # Define counter for Tests
  rv <- reactiveValues(counter = 1L)

  # Increase test and button counter
  observeEvent(input$add,
    {
      rv$counter <- rv$counter + 1L
      btn$counter <- btn$counter + 1L
    },
    priority = 10,
    label = "Count Increaser"
  )
  # jump to new tab
  observeEvent(input$add, {
    updateTabsetPanel(
      session, "tabs",
      shinyInput("Test", rv$counter)
    )
  })

    # Create new Test tab
  observeEvent(input$add, {
    appendTab(
      inputId = "tabs",
      tabPanel(
        title = paste("Test", rv$counter),
        value = shinyInput("Test", rv$counter),

        # Content of new tabs:
        helpText("You can enter your relevant 
                                test characteristics"),
        numericInput(shinyInput("sens", rv$counter),
          label = h4("Sensitvitiy"),
          value = 0.5,
          min = 0,
          max = 1,
          step = 0.1
        ),
        numericInput(shinyInput("spec", rv$counter),
          label = h4("Specificity"),
          value = 0.5,
          min = 0,
          max = 1,
          step = 0.1
        ),
        selectInput(shinyInput("result", rv$counter),
          label = h4("Test result"),
          choices = list(
            "Positive" = "pos",
            "Negative" = "neg"
          ),
          selected = "Positive"
        ),

        # Remove button only shows on tab with
        # highest index
        conditionalPanel(
          condition = "output.max_tab",
          actionButton(shinyInput("remove_btn", btn$counter),
            "Remove",
            icon = icon("minus-circle")
          )
        )
      )
    )
  })

  ### Tab deletion ---------------------------------------------
  # reactive condition for display of remove button
  output$max_tab <- reactive({
    req(rv$counter > 1L)
    identical(input$tabs, shinyInput("Test", rv$counter))
  })

  # Keep max_tab active?
  outputOptions(output, "max_tab", suspendWhenHidden = FALSE)

  # Reactive tab naming for remove button:
  current.tab <- eventReactive(input$tabs, {
    # don't accidentally remove Test1 tab:
    if (!identical(input$tabs, "Test1")) {
      input$tabs
    } else {
      NULL
    }
  })

  # reactive button counter and exhaust vector:
  btn <- reactiveValues(counter = 1L) # only goes up
  exhaust <- c(1)

  # observer for remove button
  observe({
    # Create observe events for all existing remove buttons
    # that have not yet been created (exhaust)
    lapply((1:btn$counter)[-exhaust], function(x) {
      observeEvent(input[[paste("remove_btn", x, sep = "_")]],
        {
          req(input[[paste("remove_btn", x, sep = "_")]] == 1)
          rv$counter <- rv$counter - 1L

          removeTab(
            inputId = "tabs",
            target = current.tab()
          )
          print("Dead")
        },
        once = TRUE,
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      exhaust <<- append(exhaust, x)
    })
  })

  ### Debugging ------------------------------------------------

  # observer for debugging REMOVE BEFORE FINAL
  output$track <- renderText({
    paste("index", rv$counter, "Button", btn$counter)
  })

  ### Post Probability calculation -------------------------------

  # Create reactive vectors of test parameters that only trigger
  # on calc button press

  sens_list <- eventReactive(input$calc, {
    sensis <- sapply(1:rv$counter, function(x) {
      input[[paste0("sens_", x)]]
    })
  })

  spec_list <- eventReactive(input$calc, {
    specis <- sapply(1:rv$counter, function(x) {
      input[[paste0("spec_", x)]]
    })
  })

  res_list <- eventReactive(input$calc, {
    resis <- sapply(1:rv$counter, function(x) {
      input[[paste0("result_", x)]]
    })
  })

  br_reactive <- eventReactive(input$calc, {
    input$br
  })

  # Create dataframe for use in outputs
  test_data <- eventReactive(input$calc, {
    multiple_post_prob(sens_list(), spec_list(), br_reactive(),
      res_list(),
      method = input$method
    )
  })

  ### Outputs --------------------------------------------------

  # Create datatable for output
  output$test <- renderDataTable(
    {
      datatable(
        test_data() %>%
          mutate(across(where(is.numeric), round, 4)),
        style = "bootstrap"
      )
    },
    options = list(rowCallback = JS(rowCallback))
  )

  # Create download of dataframe
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("posttest_probability.", input$filetype)
    },
    content = function(file) {
      if (input$filetype == "csv") {
        write_csv(test_data(), file)
      }
      if (input$filetype == "tsv") {
        write_tsv(test_data(), file)
      }
    }
  )

  # Create ROC plot for fun

  plot1 <- eventReactive(input$calc, {
    plot1 <- ggplot(test_data(), aes(
      x = 1 - Specificity,
      y = Sensitivity,
      color = Result,
      label = LR
    )) +
      geom_point() +
      xlim(0, 1) +
      ylim(0, 1) +
      labs(title = "ROC plot", color = "Test result\n") +
      scale_color_manual(
        labels = c("Positive", "Negative"),
        values = c("blue", "red")
      ) +
      geom_abline(slope = 1, intercept = 0, colour = "gray75")
  })

  observeEvent(input$dimension, {
    output$roc_plot <- renderPlotly({
      ggplotly(
        p = plot1(),
        tooltip = c("x", "y"),
        width = (0.8 * as.numeric(input$dimension[1])),
        height = as.numeric(input$dimension[2])
      )
    })
  })

  observeEvent(input$calc, {
    print(res_list())
  })

## 2x2 -------------------------------------------------------
 ### Render Input UI -----------------------------------------
  # render UI based on radio Buttons 
  output$tp_UI <- renderUI({ 

    # keep value when switching
    value <- isolate(input$tp)

    if (input$type_inp_tp == "Numeric") {
        numericInput("tp",
                      label = NULL,
                      value = value,
                      min = 1,
                      max = 25000,
                      step = 1
                    )
      }
    else if (input$type_inp_tp == "Sliders") {
        sliderInput("tp",
                    label = NULL,
                    value = value,
                    min = 1,
                    max = 25000,
                    step = 1
                  )
      }
  })
  output$fp_UI <- renderUI({
    
    # keep value when switching
    value <- isolate(input$fp)

    if (input$type_inp_fp == "Numeric") {
        numericInput("fp",
            label = NULL,
            value = value,
            min = 1,
            max = 25000,
            step = 1
          )
    }
    else if (input$type_inp_fp == "Sliders") {
        sliderInput("fp",
                    label = NULL,
                    value = value,
                    min = 1,
                    max = 25000,
                    step = 1
                    )
      }
  })
  output$tn_UI <- renderUI({

    # keep value when switching
    value <- isolate(input$tn)

    if (input$type_inp_tn == "Numeric") {
        numericInput("tn",
            label = NULL,
            value = value,
            min = 1,
            max = 25000,
            step = 1
          )
    }
    else if (input$type_inp_tn == "Sliders") {
        sliderInput("tn",
                    label = NULL,
                    value = value,
                    min = 1,
                    max = 25000,
                    step = 1
                    )
      }
  })
  output$fn_UI <- renderUI({
    
    # keep value when switching
    value <- isolate(input$fn)

    if (input$type_inp_fn == "Numeric") {
        numericInput("fn",
            label = NULL,
            value = value,
            min = 1,
            max = 25000,
            step = 1
          )
    }
    else if (input$type_inp_fn == "Sliders") {
        sliderInput("fn",
                    label = NULL,
                    value = value,
                    min = 1,
                    max = 25000,
                    step = 1
                    )
    }
  })
  ### Calculations -------------------------------------
  # Calculate sensitivity
  sens_2x2 <- reactive({
    input$tp / (input$tp + input$fn)
  })
  # Calculate specificity
  spec_2x2 <- reactive({
    input$tn / (input$tn + input$fp)
  })
  # Calculate baserate
  br_2x2 <- reactive({
    (input$tp + input$fn) / (input$tp + input$fn + input$tn + input$fp)
  })
  # Calculate total
  total <- reactive({
    input$tp + input$fn + input$tn + input$fp
  })
  # Create table 
  table_2x2 <- reactive({
    
    all_pos <- c(input$tp, input$fn, input$tp + input$fn)
    all_neg <- c(input$fp, input$tn, input$fp + input$tn)
    row_names <- c("Positive Test", "Negative Test", "Total")

    df <- data.frame(cbind(row_names, all_pos, all_neg)) %>%
          mutate(horizontal = as.integer(all_pos) + as.integer(all_neg)) %>%
          "colnames<-"(c("", "True Criterion", "False Criterion", "Total"))
  })


  ### Debugging --------------------------------------------------
  output$sens_2x2_out <- renderUI({

    withMathJax(paste0("The sensitivity of the test is: ",
                       "\\(\\frac{TP}{TP + FN} = ",
                       round(sens_2x2(), 4), "\\)"))
  })
  output$spec_2x2_out <- renderUI({
        withMathJax(paste0("The specificity of the test is: ",
                       "\\(\\frac{TN}{TN + FP} = ",
                       round(spec_2x2(), 4), "\\)"))
  })
  output$br_2x2_out <- renderUI({
        withMathJax(paste0("The base rate of the true criterion is: ",
                       "\\(\\frac{TP + FN}{TP + FN + TN + FP} = ",
                       round(br_2x2(), 4), "\\)"))
  })
  output$table_2x2_out <- renderTable({

    table_2x2()
  })

  # Plot outputs
  output$tree_plot_out <- renderPlot({

    plot_prism(prev = br_2x2(),
               sens = sens_2x2(),
               spec = spec_2x2(),
               N = total(),
               p_scale = TRUE,
               p_lwd = 2,
               main = NULL
    )
  })
  output$area_plot_out <- renderPlot({

    plot_area(prev = br_2x2(),
               sens = sens_2x2(),
               spec = spec_2x2(),
               N = total(),
               main = NULL
    )
  })
  output$col_table_out <- renderPlot({

    plot_tab(prev = br_2x2(),
               sens = sens_2x2(),
               spec = spec_2x2(),
               N = total(),
               main = NULL
    )
  })

}
# run App ----------------------------------------------------

shinyApp(ui = ui, server = server)
