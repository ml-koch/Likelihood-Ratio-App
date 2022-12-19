# Working directory -------------------------------------------
setwd("C:/Users/maxim/Documents/R/Postttest Probability Shiny App")

# Packages ----------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(reactlog)
library(DT)
library(plotly)
library(thematic)

# Global options ----------------------------------------------
options(shiny.reactlog = TRUE)
thematic_shiny()

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
  titlePanel("Posttest Probability Calculator"),
  
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(id = "tabs",
                  # Static Test 1 tab
                  tabPanel(title = "Test 1",
                           value = "Test1",
                           
                           # Content of Test 1 tab :
                           helpText("You can enter your relevant 
                                    test characteristics"),
                           
                           numericInput("sens_1", label = h4("Sensitvitiy"),
                                        value = 0.5,
                                        min = 0,
                                        max = 1,
                                        step = 0.1),
                           
                           numericInput("spec_1", label = h4("Specificity"),
                                        value = 0.5,
                                        min = 0,
                                        max = 1,
                                        step = 0.1),
                           
                           selectInput("result_1", label = h4("Test result"),
                                       choices = list("Positive" = "pos",
                                                      "Negative" = "neg"),
                                       selected = "Positive"),
                           
                           numericInput("br", label = h4("Base rate"),
                                        value = 0.5,
                                        min = 0,
                                        max = 1,
                                        step = 0.1),
                           
                           
                           selectInput("method", label = h4("Method"),
                                       choices = list("Fast" = "fast",
                                                      "Detail" = "detail"),
                                       selected = "Fast"),
                           
                           actionButton("add", "Add test",
                                        icon = icon("plus-circle")),
                           
                           br(),
                           br(),
                 
                           actionButton("calc", "Calculate posttest probability",
                                        icon = icon("calculator")),
                           
                           themeSelector2(),
                           
                           )
                  
                  )
      
    ),
    
    # Main Panel for Output
    mainPanel(
      
      tabsetPanel(id = "output_tabs",
        
        tabPanel(title = "Text",
          p("The final posttest probability of disease is",
            textOutput("post_prob")),
          
          textOutput("track"), ),
        
        tabPanel(title = "Table",
          dataTableOutput("test"),
          br(),
          radioButtons(inputId = "filetype",
                       label = "Select filetype:",
                       choices = c("csv", "tsv"),
                       selected = "csv"),
          br(),
          downloadButton("download_data", "Download data")
          ),
        
        tabPanel(title = "Plot",
                 
                 plotlyOutput(outputId = "roc_plot",
                              width = "auto")
          
          )
        
        )
      
      )
    
  )
  
)

# Server -----------------------------------------------------
server <- function(input, output, session) {
  
  ## Tab creation -------------------------------------------
  # Define counter for Tests
  rv <- reactiveValues(counter = 1L)
  
  # Increase test and button counter
  observeEvent(input$add, {
    
    rv$counter <- rv$counter + 1L
    btn$counter <- btn$counter + 1L
    
  }, priority = 10,
  label = "Count Increaser")
    
  observeEvent(input$add, {
    updateTabsetPanel(session, "tabs",
                      shinyInput("Test", rv$counter))
  })
  
  # jump to new tab 
  
  # Create new Test tab
  observeEvent(input$add, {
    appendTab(inputId = "tabs",
              tabPanel(title = paste("Test", rv$counter),
                       value = shinyInput("Test", rv$counter),
                       
                       # Content of new tabs:
                       helpText("You can enter your relevant 
                                test characteristics"),
                                
                       numericInput(shinyInput("sens", rv$counter),
                                    label = h4("Sensitvitiy"),
                                    value = 0.5,
                                    min = 0,
                                    max = 1,
                                    step = 0.1),
                                
                       numericInput(shinyInput("spec", rv$counter),
                                    label = h4("Specificity"),
                                    value = 0.5,
                                    min = 0,
                                    max = 1,
                                    step = 0.1),
                                
                       selectInput(shinyInput("result", rv$counter),
                                   label = h4("Test result"),
                                   choices = list("Positive" = "pos",
                                                  "Negative" = "neg"),
                                   selected = "Positive"),
                       
                       # Remove button only shows on tab with
                       # highest index
                       conditionalPanel(
                         condition = "output.max_tab",
                         actionButton(shinyInput("remove_btn", btn$counter),
                                      "Remove",
                                      icon = icon("minus-circle")))
                       
                       
              )
    )
  })
  
  ## Tab deletion ---------------------------------------------
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
    }
    else {
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
      observeEvent(input[[paste("remove_btn", x, sep = "_")]], {
        
        req(input[[paste("remove_btn", x, sep = "_")]] == 1)
        rv$counter <- rv$counter - 1L
        
        removeTab(inputId = "tabs",
                  target = current.tab())
        print("Dead")
        
      }, once = TRUE, ignoreNULL = TRUE,
      ignoreInit = TRUE)
      
      exhaust <<- append(exhaust, x)
    })
  })
        
  
  ## Debugging ------------------------------------------------
  
  # observer for debugging REMOVE BEFORE FINAL
  output$track <- renderText({
    
    paste("index", rv$counter, "Button", btn$counter)
    
  })
  
  
  ## Post Probability calculation -------------------------------
  
  # Create reactive vectors of test parameters that only trigger
  # on calc button press
  
  sens_list <- eventReactive(input$calc, {
    
    sensis <- sapply(1:rv$counter, function(x)
      input[[paste0("sens_", x)]])
    
  })
  
  spec_list <- eventReactive(input$calc, {
    
    specis <- sapply(1:rv$counter, function(x)
      input[[paste0("spec_", x)]])
    
  })
  
  res_list <- eventReactive(input$calc, {
    
    resis <- sapply(1:rv$counter, function(x)
      input[[paste0("result_", x)]])
    
  })
  
  br_reactive <- eventReactive(input$calc, {
    
    input$br
    
  })
  
  # Create dataframe for use in outputs
  test_data <- eventReactive(input$calc, {
    
    multiple_post_prob(sens_list(), spec_list(), br_reactive(),
                       res_list(), method = input$method)
    
  })
  
  # Outputs --------------------------------------------------
  
  # Create datatable for output
  output$test <- renderDataTable({
    
    datatable(test_data() %>%
                mutate(across(where(is.numeric), round, 4)),
              style = "bootstrap")
    
  }, options = list(rowCallback = JS(rowCallback)))
  
  # Create download of dataframe
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("posttest_probability.", input$filetype)
    },
    content = function(file) {
      if(input$filetype == "csv") {
        write_csv(test_data(), file)
      }
      if(input$filetype == "tsv") {
        write_tsv(test_data(), file)
      }
      
    }
  )
  
  # Create ROC plot for fun
  
  plot1 <- eventReactive(input$calc, {
    
    plot1 <- ggplot(test_data(), aes(x = 1 - Specificity,
                                     y = Sensitivity,
                                     color = Result,
                                     label = LR)) +
      geom_point() +
      xlim(0, 1) +
      ylim(0, 1) +
      labs(title = "ROC plot", color = "Test result\n") +
      scale_color_manual(labels = c("Positive", "Negative"),
                         values = c("blue", "red")) +
      geom_abline(slope = 1, intercept = 0, colour = "gray75")
    
  })
  
  observeEvent(input$dimension, {
    
    output$roc_plot <- renderPlotly({
      
      ggplotly(p = plot1(),
               tooltip = c("x", "y"),
               width = (0.8 * as.numeric(input$dimension[1])),
               height = as.numeric(input$dimension[2]))
      
      
      
    })
    
  })
  
  
  observeEvent(input$calc, {
    
    print(res_list())
    
  })
  

    
  
}

# run App ----------------------------------------------------

shinyApp(ui = ui, server = server)
