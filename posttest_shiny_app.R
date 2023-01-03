# Working directory -------------------------------------------
setwd("C:/Users/Maximilian/Documents/Repositories/Postttest-Probability-Shiny-App")

# Packages ----------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(shinydashboard)
library(reactlog)
library(DT)
library(plotly)
library(riskyr)
library(colourpicker)
library(shinyBS)


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
### one time commands  ----------------------

# Ui ---------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("yeti"),
  withMathJax(),
  navbarPage("PTP/PPV",
  ## PPC tab ----------------------------------------------------
    tabPanel("PTP",
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            id = "tabs",
            # Static Test 1 tab
            tabPanel(
              title = "Test 1",
              value = "Test1",

              # Content of Test 1 tab :
              numericInput("sens_1",
                        label = h4("Sensitvitiy"),
                        value = 0.5,
                        min = 0,
                        max = 1,
                        step = 0.1
                      ),
              bsTooltip(id = "sens_1", title = "Probability of correct detection of true condition <br>P(Dec pos|Cond true)",
                        placement = "right", options = list(container = "body")),
               numericInput("spec_1",
                          label = h4("Specificity"),
                          value = 0.5,
                          min = 0,
                          max = 1,
                          step = 0.1
                        ),
              bsTooltip(id = "spec_1", title = "Probability of correct detection of false condition <br>P(Dec neg|Cond false)",
                        placement = "right", options = list(container = "body")),
              numericInput("br",
                          label = h4("Base rate"),
                          value = 0.5,
                          min = 0,
                          max = 1,
                          step = 0.1
                        ),
              bsTooltip(id = "spec_1", title = "Probability of true condition (prevalence) <br>P(Cond true)",
                        placement = "right", options = list(container = "body")),
              selectInput("result_1",
                label = h4("Test result"),
                choices = list(
                  "Positive" = "positive",
                  "Negative" = "negative"
                ),
                selected = "Positive"
              ),
              bsTooltip(id = "result_1", title = "Currently only binary tests are supported",
                        placement = "right", options = list(container = "body")),
              selectInput("method",
                label = h4("Method"),
                choices = list(
                  "Fast" = "fast",
                  "Detail" = "detail"
                ),
                selected = "Fast"
              ),
              bsTooltip(id = "method", title = "Fast = Only the final PTP will be returned <br>Detail = PTP will be calculated after each test",
                        placement = "right", options = list(container = "body")),
              actionButton("add", "Add test",
                icon = icon("plus-circle")
              ),
              br(),
              br(),
              actionButton("calc", "Calculate posttest probability",
                icon = icon("calculator")
              ),
              # themeSelector2(),
            )
          )
        ),

        # Main Panel for Output
        mainPanel(
          tabsetPanel(
            id = "output_tabs",
            tabPanel(
              title = "Text", 
              br(),
              verbatimTextOutput("detail_text_out"),
              br(),
              uiOutput("post_prob_text"),
              br(),
              br(),
              p("All probabilities are calculated using", 
                a("likelihood ratios", href = "https://en.wikipedia.org/wiki/Likelihood_ratios_in_diagnostic_testing")),
            ),
            tabPanel(
              title = "Table",
              br(),
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
              br(),
              plotlyOutput(outputId = "roc_plot"),
              br(),
              br(),
              p("This interactive plot includes a download function in the upper right corner")
            )
          )
        )
      )
    ),
  ## 2x2 tab -------------------------------------------------------
    tabPanel("2x2",
      # sidebar 
      sidebarLayout(
        sidebarPanel(
              title = "2 x 2",
              value = "2x2_sidebar",
              # Content
              h4("True positives"),
              radioButtons("type_inp_tp", label = NULL,
                           choices = c("Numeric", "Slider"),
                           selected = "Numeric",
                           inline = TRUE),
              uiOutput("tp_UI"),
              h4("False positives"),
              radioButtons("type_inp_fp", label = NULL,
                           choices = c("Numeric", "Slider"),
                           selected = "Numeric",
                           inline = TRUE),
              uiOutput("fp_UI"),
              h4("True negatives"),
              radioButtons("type_inp_tn", label = NULL,
                           choices = c("Numeric", "Slider"),
                           selected = "Numeric",
                           inline = TRUE),
              uiOutput("tn_UI"),
              h4("False negatives"),
              radioButtons("type_inp_fn", label = NULL,
                           choices = c("Numeric", "Slider"),
                           selected = "Numeric",
                           inline = TRUE),
              uiOutput("fn_UI")
        ),
        # Main Panel for Output
        mainPanel(
          tabsetPanel(
            id = "2x2_output_tabs",
          #### Table Output Panel ----------------------------------------------------
            tabPanel(
              title = "2x2 Table",
              br(),
              tableOutput("table_2x2_out"),
              br(),
              br(),
              uiOutput("sens_2x2_out"), 
              br(),
              br(),
              uiOutput("spec_2x2_out"),
              br(),
              br(),
              uiOutput("br_2x2_out")
            ),
          #### Color Table Output Panel ----------------------------------------------------
            tabPanel(
              title = "Colorized Table",
              br(),
              plotOutput("col_table_out", height = 450, width = 600),
              br(),
              wellPanel(
              fluidRow(
                  column(
                    selectInput("tab_persp",
                                  label = "Perspective",
                                  choices = c("condition + decision" = "cddc",
                                              "condition + accuracy" = "cdac",
                                              "decision + condition" = "dccd",
                                              "decision + accuracy" = "dcac",
                                              "accuracy + condition" = "accd",
                                              "accuracy + decision" = "acdc"),
                                  selected = "cddc"
                                  ),
                    selectInput("tab_freq_lab",
                                  label = "Frequency label",
                                  choices = c("abbr. names + values" = "def",
                                              "abbr. names" = "abb",
                                              "names" = "nam",
                                              "values" = "num",
                                              "names + values" = "namnum",
                                              "none" = "no"),
                                  selected = "num"),
                    checkboxInput("tab_margins",
                                  label = "Display info at bottom"),
                      width = 6),
                  column(
                    selectInput("tab_pop_split",
                                  label = "Population split",
                                  choices = c("horizontal" = "h",
                                              "vertical" = "v"),
                                  selected = "v"),
                    selectInput("tab_prob_lab",
                                  label = "Probability label",
                                  choices = c("nothing" = NA,
                                              "abbr. names + values" = "def",
                                              "abbr. names" = "abb",
                                              "names" = "nam",
                                              "values" = "num",
                                              "names + values" = "namnum",
                                              "important names" = "min",
                                              "no labels" = "no")
                                ),
                    downloadButton("download_col_tab", "Download Colorized Table"),
                    width = 6)
                )
              ),
              p("Created using the", 
                a("riskyr", href = "https://cran.r-project.org/web/packages/riskyr/index.html"),  "package")
            ),
          #### Prism Output Panel ----------------------------------------------------
            tabPanel(
              title = "Prism Plot",
              plotOutput("prism_plot_out", height = 450, width = 600),
              br(),
              wellPanel(
              fluidRow(
                  column(
                    selectInput("prism_persp",
                                  label = "Perspective",
                                  choices = c("condition" = "cd",
                                              "decision" = "dc",
                                              "accuracy" = "ac",
                                              "condition + decision" = "cddc",
                                              "condition + accuracy" = "cdac",
                                              "decision + condition" = "dccd",
                                              "decision + accuracy" = "dcac",
                                              "accuracy + condition" = "accd",
                                              "accuracy + decision" = "acdc"),
                                  selected = "cddc"
                                  ),
                    selectInput("prism_freq_lab",
                                  label = "Frequency label",
                                  choices = c("abbr. names + values" = "def",
                                              "abbr. names" = "abb",
                                              "names" = "nam",
                                              "values" = "num",
                                              "names + values" = "namnum"),
                                  selected = "num"),
                    checkboxInput("prism_margins",
                                  label = "Display info at bottom"),
                      width = 6),
                  column(
                    selectInput("prism_area",
                                  label = "Box type",
                                  choices = c("default" = "no",
                                              "horizontal" = "hr",
                                              "square" = "sq"),
                                  selected = "no"),
                    selectInput("prism_prob_lab",
                                  label = "Probability label",
                                  choices = c("abbr. names + values" = "def",
                                              "abbr. names" = "abb",
                                              "names" = "nam",
                                              "values" = "num",
                                              "names + values" = "namnum",
                                              "important names" = "min",
                                              "important names + values" = "mix",
                                              "none" = "no"),
                                  selected = "num"),
                    downloadButton("download_prism", "Download Prism Plot"),
                    width = 6)
                )
              ),
              p("Created using the", 
                a("riskyr", href = "https://cran.r-project.org/web/packages/riskyr/index.html"),  "package")
            ),
          #### Area Output Panel ----------------------------------------------------
            tabPanel(
              title = "Area Plot",
              br(),
              plotOutput("area_plot_out", height = 450, width = 600),
              br(),
              wellPanel(
                fluidRow(
                    column(
                      selectInput("area_persp",
                                    label = "Perspective",
                                    choices = c("condition + decision" = "cddc",
                                                "condition + accuracy" = "cdac",
                                                "decision + condition" = "dccd",
                                                "decision + accuracy" = "dcac",
                                                "accuracy + condition" = "accd",
                                                "accuracy + decision" = "acdc"),
                                    selected = "cddc"
                                    ),
                      selectInput("area_freq_lab",
                                    label = "Frequency label",
                                    choices = c("abbr. names + values" = "def",
                                                "abbr. names" = "abb",
                                                "names" = "nam",
                                                "values" = "num",
                                                "names + values" = "namnum"),
                                    selected = "def"),
                      checkboxInput("area_margins",
                                    label = "Display info at bottom"),
                        width = 6),
                    column(
                      selectInput("area_split",
                                    label = "Split",
                                    choices = c("vertical" = "v",
                                                "horizontal" = "h"),
                                    selected = "no"),
                      selectInput("area_prob_lab",
                                    label = "Probability label",
                                    choices = c("none" = NA,
                                                "abbr. names + values" = "def",
                                                "abbr. names" = "abb",
                                                "names" = "nam",
                                                "values" = "num",
                                                "names + values" = "namnum",
                                                "important names" = "min",
                                                "important names + values" = "mix")
                                  ),
                      downloadButton("download_area", "Download Area Plot"),
                      width = 6)
                  )
              ),
              p("Created using the", 
                a("riskyr", href = "https://cran.r-project.org/web/packages/riskyr/index.html"),  "package")
            ),
          #### Curve Output panel ---------------------------------------------------------
            tabPanel(
              title = "Curve Plot",
              br(),
              plotOutput("curve_plot_out", height = 450, width = 600),
              br(),
              wellPanel(
                fluidRow(
                  column(
                    checkboxInput("curve_ppv",
                                  label = "Show PPV curve",
                                  value = TRUE),
                    checkboxInput("curve_values",
                                  label = "Show values",
                                  value = TRUE),
                    width = 3),
                  column(
                    checkboxInput("curve_npv",
                                  label = "Show NPV curve",
                                  value = TRUE),
                    checkboxInput("curve_margins",
                                  label = "Display info at bottom"),
                    width = 3),
                  column(
                    checkboxInput("curve_acc",
                                  label = "Show accuracy"),
                    checkboxInput("curve_log",
                                  label = "Use log scale"),
                    width = 3),
                  column(
                    checkboxInput("curve_ppod",
                                  label = "Show proportion of positive decisions"),
                    width = 3),
                ),
                fluidRow(
                  column(
                    sliderInput("curve_uncert",
                                label = "Uncertainity",
                                min = 0,
                                max = 50,
                                value = 0),
                    width = 9
                  ),
                  column(
                    div(downloadButton("download_curve", "Download Curve Plot"),
                        style = "padding:30px"),
                    width = 3
                  )
                )
              ),
              p("Created using the", 
                a("riskyr", href = "https://cran.r-project.org/web/packages/riskyr/index.html"),  "package")
            )
          )
        )
      )
    ),
  ## Customize names --------------------------------------------------------------
    tabPanel(title = "Customize naming sheme",
      sidebarLayout(
        sidebarPanel(
          h3("Customize the naming sheme"),
          fluidRow(
            column(
              textInput("main_title",
                        label = "Main Title",
                        value = "Main Title"),
              width = 7
            )
          ),
          fluidRow(
            column(
              textInput("pop_label",
                      label = "Population",
                      value = "Population"),
              width = 4
            ),
            column(
              textInput("n_label",
                      label = "N",
                      value = "N"),
              width = 3
            )
          ),
          fluidRow(
            column(
              textInput("cond_label",
                      label = "Condition",
                      value = "Condition"),
              textInput("dec_label",
                      label = "Decision",
                      value = "Decision"),
              textInput("acc_label",
                      label = "Accuracy",
                      value = "Accuracy"),
              textInput("cases_label",
                      label = "Cases",
                      value = "Cases"),
              width = 4
            ),
            column(
              textInput("cond_true_label",
                      label = "Condition True",
                      value = "True"),
              textInput("dec_pos_label",
                      label = "Decision Positive",
                      value = "Positive"),
              textInput("acc_cor_label",
                      label = "Decision Correct",
                      value = "Correct"),
              textInput("tp_label",
                      label = "Hit",
                      value = "TP"),
              textInput("fn_label",
                      label = "Miss",
                      value = "FN"),
              width = 3
            ),
            column(
              textInput("cond_false_label",
                      label = "Condition False",
                      value = "False"),
              textInput("dec_neg_label",
                      label = "Decision Negative",
                      value = "Negative"),
              textInput("acc_fal_label",
                      label = "Decision False",
                      value = "Incorrect"),
              textInput("fp_label",
                      label = "False Alarm",
                      value = "FP"),
              textInput("tn_label",
                      label = "Correct rejection",
                      value = "TN"),
              width = 3
            )
          ),
          fluidRow(
            column(
              actionButton("reset_labels",
                          label = "Reset to default",
                          icon = icon("arrows-rotate")),
              br(),
              p("Adapted from the", a("riskyr App", href = "https://riskyr.org"),  "."),
              width = 5)
          ),
          width = 5
        ),
        mainPanel(
          br(),
          h3("Preview"),
          plotOutput("prism_plot_ex_lbl", height = 340, width = 450),
          plotOutput("tab_plot_ex_lbl", height = 340, width = 450),
          width = 7
        )
      )
    ),
  ## Customize colors ----------------------------------------------------------------------  
    tabPanel(title = "Customize plot colors",
      sidebarLayout(
        sidebarPanel(
          h3("Customize the colors"),
          fluidRow(
            column(
              colourInput("col_tp",
                          showColour = "background",
                          label = "True positive",
                          value = pal[8]),
              colourInput("col_fn",
                          showColour = "background",
                          label = "False negative",
                          value = pal[9]),
              colourInput("col_cond_true",
                          showColour = "background",
                          label = "Condition True",
                          value = pal[2]),
              width = 4
            ),
            column(
              colourInput("col_fp",
                          showColour = "background",
                          label = "False positive",
                          value = pal[10]),
              colourInput("col_tn",
                          showColour = "background",
                          label = "True negative",
                          value = pal[11]),
              colourInput("col_cond_false",
                          showColour = "background",
                          label = "Condition False",
                          value = pal[3]),
              width = 4
            ),
            column(
              colourInput("col_dec_pos",
                          showColour = "background",
                          label = "Decision positive",
                          value = pal[4]),
              colourInput("col_dec_neg",
                          showColour = "background",
                          label = "Decision negative",
                          value = pal[5]),
              colourInput("col_pop",
                          showColour = "background",
                          label = "Population",
                          value = pal[1]),
              width = 4
            )
          ),
          br(),
          br(),
          fluidRow(
            column(
              colourInput("col_dec_true",
                          showColour = "background",
                          label = "Decision Correct",
                          value = pal[6]),
              width = 4
            ),
            column(
              colourInput("col_dec_fal",
                          showColour = "background",
                          label = "Decision False",
                          value = pal[7]),
              width = 4
            ),
            column(
              colourInput("col_bgr",
                          showColour = "background",
                          label = "Background",
                          value = pal[16]),
              width = 4
            )
          ),
          br(),
          br(),
          fluidRow(
            column(
              colourInput("col_ppv",
                          showColour = "background",
                          label = "PPV",
                          value = pal[12]),
              colourInput("col_text",
                          showColour = "background",
                          label = "Text",
                          value = pal[14]),
              width = 6
            ),
            column(
              colourInput("col_npv",
                          showColour = "background",
                          label = "NPV",
                          value = pal[13]),
              colourInput("col_lines",
                          showColour = "background",
                          label = "Lines",
                          value = pal[15]),
              width = 6
            )
          ),
          fluidRow(
            column(
              actionButton("reset_colors",
                              label = "Reset to default",
                              icon = icon("arrows-rotate")),
              br(),
              p("Adapted from the", a("riskyr App", href = "https://riskyr.org"),  "."),
              width = 5)
          ),
          width = 5
        ),
        mainPanel(
          br(),
          h3("Preview"),
          plotOutput("tab_plot_ex_col", height = 225, width = 300),
          plotOutput("prism_plot_ex_col", height = 225, width = 300),
          plotOutput("curve_plot_ex_col", height = 225, width = 300),
          width = 7
        )
      )
    ),
  ## About -----------------------------------------------------------------
    tabPanel(title = "Contact & References",
      titlePanel("Contact and References"),
        fluidRow(
          column(
            h3("Contact"),
            p("You can find me and the source code on", 
              a("Github", href = "https://github.com/ml-koch/Postttest-Probability-Shiny-App"), "."),
            p("Feel free to contribute if you would like to or suggest changes that you would like to see."),
            h3("References"),
            p("This app is based on R and RShiny."),
            p("The plots in the '2x2' section were created with the", tags$code("riskyr"), 
              "package and their customization was reverse-enginered from how they appeared in the",
              tags$code("riskyrApp"), "."),
          #### Reference list -------------------------------------------------------------------
            tags$ul(
              tags$li(p("Chang W. (2021).", 
                        tags$i("shinythemes: Themes for Shiny"),
                        ". R package version 1.2.0,", 
                        a("https://CRAN.R-project.org/package=shinythemes",
                          href = "https://CRAN.R-project.org/package=shinythemes"), ".")),
              tags$li(p("Chang W, Borges Ribeiro B (2021).", 
                        tags$i("shinydashboard: Create Dashboards with 'Shiny'"),
                        ". R package version 0.7.2, ", 
                        a("https://CRAN.R-project.org/package=shinydashboard",
                          href = "https://CRAN.R-project.org/package=shinydashboard"), ".")),
              tags$li(p("Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J,   
                        McPherson J, Dipert A, Borges B (2022).", 
                        tags$i("shiny: Web Application Framework for R"),
                        ". R package version 1.7.3,", 
                        a("https://CRAN.R-project.org/package=shiny",
                          href = "https://CRAN.R-project.org/package=shiny"), ".")),
              tags$li(p("Neth, H., Gaisbauer, F., Gradwohl, N., & Gaissmaier, W. (2022).", 
                        tags$i("riskyr: Rendering Risk Literacy more Transparent"),
                        ". Social Psychology and Decision Sciences, University of Konstanz, Germany. 
                        Computer software (R package version 0.4.0, Aug. 15, 2022). Retrieved from", 
                        a("https://CRAN.R-project.org/package=riskyr",
                          href = "https://CRAN.R-project.org/package=riskyr"), ".")),
              tags$li(p("R Core Team (2022).", 
                        tags$i("R: A language and environment for statistical computing"),
                        ". R Foundation for Statistical Computing, Vienna, Austria. URL", 
                        a("https://www.R-project.org/",
                          href = "https://www.R-project.org/"), ".")),
              tags$li(p("Sievert C.", 
                        tags$i("Interactive Web-Based Data Visualization with R"),
                        ", plotly, and shiny. Chapman and Hall/CRC Florida, 2020.", 
                        a("https://plotly-r.com",
                          href = "https://plotly-r.com"), ".")),
              tags$li(p("Wickham H.", 
                        tags$i("ggplot2: Elegant Graphics for Data Analysis"),
                        ". Springer-Verlag New York, 2016.", 
                        a("https://ggplot2.tidyverse.org",
                          href = "https://ggplot2.tidyverse.org"), ".")),
              tags$li(p("Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R,        
                        Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller   
                        E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V,
                        Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019).
                        'Welcome to the tidyverse.'", 
                        tags$i("Journal of Open Source Software"),
                        ", *4*(43), 1686. doi:10.21105/joss.01686,", 
                        a("https://doi.org/10.21105/joss.01686",
                          href = "https://doi.org/10.21105/joss.01686"), ".")),
              tags$li(p("Xie Y, Cheng J, Tan X (2022).", 
                        tags$i("DT: A Wrapper of the JavaScript Library 'DataTables'"),
                        ". R package version 0.26,", 
                        a("https://CRAN.R-project.org/package=DT",
                          href = "https://CRAN.R-project.org/package=DT"), "."))
            ),
          width = 8
          )
        )
      )
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
            "Positive" = "positive",
            "Negative" = "negative"
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

    if (input$method == "fast") {
      multiple_post_prob_fast(sens_list(), spec_list(), br_reactive(), res_list())
    }
    else if (input$method == "detail") {
      multiple_post_prob_detail(sens_list(), spec_list(), br_reactive(), res_list())
    }
  })

  ### Outputs --------------------------------------------------
  # Create text output 
  output$detail_text_out <- renderPrint({
    if (input$method == "detail" && nrow(test_data()) >= 2) {
        detail_text <- create_detail_text(test_data())
        cat(paste0(detail_text), sep = "")}
  }) %>%
  bindEvent(input$calc, ignoreInit = TRUE)
  output$post_prob_text <- renderUI({
    req(test_data())
      withMathJax(paste0("The posttest probability after all tests is: ",
                        "\\(\\frac{\\text { sensitivity } \\times \\text { base rate }}
                        {\\text { sensitivity } \\times \\text { base rate } +
                        (1-\\text { specificity }) \\times(1-\\text { base rate })} = ",
                        round(test_data()[nrow(test_data()), 7], 4), "\\)"))
  })

  # Create datatable for output
  output$test <- renderDataTable(
    {
      datatable(
        test_data() %>%
          mutate(across(where(is.numeric), round, 4)),
        options = list(rowCallback = JS(rowCallback))
      )
    },
    # Option for displaying NA; doesnt work when thematic is used 
    options = list(rowCallback = JS(rowCallback))
  )

  # Create download of dataframe
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("posttest_probability_data.", input$filetype)
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

  # Create ROC plot 
  plot1 <- eventReactive(input$calc, {
    plot1 <- ggplot(test_data(), 
                    aes(x = 1 - Specificity,
                    y = Sensitivity,
                    color = test_data()[, 1],
                    label = LR,
                    schmabel = Result)) +
             geom_point() +
             xlim(0, 1) +
             ylim(0, 1) +
             labs(title = "ROC plot", color = "Test \n") + 
             scale_color_brewer(palette = "Set1") +
             geom_abline(slope = 1, intercept = 0, 
                         colour = "gray75")
  })

    output$roc_plot <- renderPlotly({
      ggplotly(
        p = plot1(),
        tooltip = c("x", "y", "label", "schmabel")
      )
    })
  ### Debugging ------------------------------------------------
  # observer for debugging REMOVE BEFORE FINAL
  output$track <- renderText({
    paste("index", rv$counter, "Button", btn$counter)
  })

## 2x2 -------------------------------------------------------
 ### Render Input UI -----------------------------------------
  # render UI based on radio Buttons 
  output$tp_UI <- renderUI({ 
    # keep value when switching
    value <- isolate(input$tp)
    if (is.null(value)) {value <- 5}

    if (input$type_inp_tp == "Numeric") {
        numericInput("tp",
                      label = NULL,
                      value = value,
                      min = 1,
                      max = 25000,
                      step = 1
                    )
      }
    else if (input$type_inp_tp == "Slider") {
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
    if (is.null(value)) {value <- 5}

    if (input$type_inp_fp == "Numeric") {
        numericInput("fp",
            label = NULL,
            value = value,
            min = 1,
            max = 25000,
            step = 1
          )
    }
    else if (input$type_inp_fp == "Slider") {
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
    if (is.null(value)) {value <- 5}

    if (input$type_inp_tn == "Numeric") {
        numericInput("tn",
            label = NULL,
            value = value,
            min = 1,
            max = 25000,
            step = 1
          )
    }
    else if (input$type_inp_tn == "Slider") {
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
    if (is.null(value)) {value <- 5}

    if (input$type_inp_fn == "Numeric") {
        numericInput("fn",
            label = NULL,
            value = value,
            min = 1,
            max = 25000,
            step = 1
          )
    }
    else if (input$type_inp_fn == "Slider") {
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
    req(input$tp)
    all_pos <- c(input$tp, input$fn, input$tp + input$fn)
    all_neg <- c(input$fp, input$tn, input$fp + input$tn)
    row_names <- c(input$dec_pos_label, input$dec_neg_label, "Total")

    df <- data.frame(cbind(row_names, all_pos, all_neg)) %>%
          mutate(horizontal = as.integer(all_pos) + as.integer(all_neg)) %>%
          "colnames<-"(c("", input$cond_true_label, input$cond_false_label, "Total"))
  })
  ### Outputs --------------------------------------------------
  #### Text and basic table ------------------------------------
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
  #### riskyr plots ------------------------------------
  # Create plots
  prism_plot <- reactive({
    plot_prism(prev = br_2x2(),
               sens = sens_2x2(),
               spec = spec_2x2(),
               N = total(),
               by = input$prism_persp,
               f_lbl = input$prism_freq_lab,
               p_lbl = input$prism_prob_lab,
               area = input$prism_area,
               mar_notes = input$prism_margins,
               arr_c = -3,
               p_scale = TRUE,
               p_lwd = 2,
               lbl_txt = txt_mod(),
               main = input$main_title,
               col_pal = pal_mod()
              )
    recordPlot()
  })
  area_plot <- reactive({
    plot_area(prev = br_2x2(),
              sens = sens_2x2(),
              spec = spec_2x2(),
              N = total(),
              by = input$area_persp,
              f_lbl = input$area_freq_lab,
              p_lbl = input$area_prob_lab,
              p_split = input$area_split,
              mar_notes = input$area_margins,
              lbl_txt = txt_mod(),
              main = input$main_title,
              col_pal = pal_mod()
             )
  recordPlot()
  })
  tab_plot <- reactive({
    plot_tab(prev = br_2x2(),
            sens = sens_2x2(),
            spec = spec_2x2(),
            N = total(),
            by = input$tab_persp,
            p_lbl = input$tab_prob_lab,
            f_lbl = input$tab_freq_lab,
            p_split = input$tab_pop_split,
            mar_notes = input$tab_margins,
            lbl_txt = txt_mod(),
            main = input$main_title,
            col_pal = pal_mod()
            )
    recordPlot()
  })
  # indicate which curves for curve plot
  what_curves <- reactive({
    x <- c("prev")
    if (input$curve_ppv) {
      x <- append(x, "PPV")
    }
    if (input$curve_npv) {
      x <- append(x, "NPV")
    }
    if (input$curve_ppod) {
      x <- append(x, "ppod")
    }
    if (input$curve_acc) {
      x <- append(x, "acc")
    }
    x
  })
  curve_plot <- reactive({
    req(what_curves())
    plot_curve(prev = br_2x2(),
              sens = sens_2x2(),
              spec = spec_2x2(),
              N = total(),
              what = what_curves(),
              mar_notes = input$curve_margins,
              log_scale = input$curve_log,
              show_points = input$curve_values,
              lbl_txt = txt_mod(),
              main = input$main_title,
              col_pal = pal_mod(),
              uc = 0.01 * input$curve_uncert
              )
    recordPlot()
  })

  # output plots
  output$prism_plot_out <- renderPlot({
    prism_plot()
  })
  output$area_plot_out <- renderPlot({
    area_plot()
  })
  output$col_table_out <- renderPlot({
    tab_plot()
  })
  output$curve_plot_out <- renderPlot({
    curve_plot()
  })
  # Downloads
  # Prism download button
  output$download_prism <- downloadHandler(
    filename = function() {
      paste0("prism_plot_", Sys.Date(), ".png")
    },
    contentType = "image/png",
    content = function(file) {
      req(prism_plot())
      png(file, width = 600, height = 450)
      replayPlot(prism_plot())
      dev.off()
    }
  )
  # Area download button
  output$download_area <- downloadHandler(
    filename = function() {
      paste0("area_plot_", Sys.Date(), ".png")
    },
    contentType = "image/png",
    content = function(file) {
      req(area_plot())
      png(file, width = 600, height = 450)
      replayPlot(area_plot())
      dev.off()
    }
  )
  # Table download button
  output$download_col_tab <- downloadHandler(
    filename = function() {
      paste0("table_plot_", Sys.Date(), ".png")
    },
    contentType = "image/png",
    content = function(file) {
      req(tab_plot())
      png(file, width = 600, height = 450)
      replayPlot(tab_plot())
      dev.off()
    }
  )
  # Curve plot download button 
  output$download_curve <- downloadHandler(
    filename = function() {
      paste0("curve_plot_", Sys.Date(), ".png")
    },
    contentType = "image/png",
    content = function(file) {
      req(curve_plot())
      png(file, width = 600, height = 450)
      replayPlot(curve_plot())
      dev.off()
    }
  )
## Interactive text labels ------------------------------------------------
  observeEvent(input$reset_labels, {
    updateTextInput(session, inputId = "main_title", value = "Main Title")
    updateTextInput(session, inputId = "pop_label", value = "Population")
    updateTextInput(session, inputId = "n_label", value = "N")
    updateTextInput(session, inputId = "cond_label", value = "Condition")
    updateTextInput(session, inputId = "dec_label", value = "Decision")
    updateTextInput(session, inputId = "acc_label", value = "Accuracy")
    updateTextInput(session, inputId = "cases_label", value = "Cases")
    updateTextInput(session, inputId = "cond_true_label", value = "True")
    updateTextInput(session, inputId = "dec_pos_label", value = "Positive")
    updateTextInput(session, inputId = "acc_cor_label", value = "Correct")
    updateTextInput(session, inputId = "tp_label", value = "TP")
    updateTextInput(session, inputId = "fn_label", value = "FN")
    updateTextInput(session, inputId = "cond_false_label", value = "False")
    updateTextInput(session, inputId = "dec_neg_label", value = "Negative")
    updateTextInput(session, inputId = "acc_fal_label", value = "Incorrect")
    updateTextInput(session, inputId = "fp_label", value = "FP")
    updateTextInput(session, inputId = "tn_label", value = "TN")
  })
  txt_mod <- reactive({
    req(input$main_title)
    txt_modded <- list_modify(txt, 
                              scen_lbl = input$main_title,
                              popu_lbl = input$pop_label,
                              N_lbl = input$n_label,
                              cond_lbl = input$cond_label,
                              cond_true_lbl = input$cond_true_label,
                              cond_false_lbl = input$cond_false_label,
                              dec_lbl = input$dec_label,
                              dec_pos_lbl = input$dec_pos_label,
                              dec_neg_lbl = input$dec_neg_label,
                              acc_lbl = input$acc_label,
                              dec_cor_lbl = input$acc_cor_label,
                              dec_err_lbl = input$acc_fal_label,
                              sdt_lbl = input$cases_label,
                              hi_lbl = input$tp_label,
                              mi_lbl = input$fn_label,
                              fa_lbl = input$fp_label,
                              cr_lbl = input$tn_label
                              )
  })
  # Example plots
  output$prism_plot_ex_lbl <- renderPlot({
    req(input$main_title)
    plot_prism(prev = 0.5,
               sens = 0.5,
               spec = 0.5,
               N = 1000,
               by = "cddc",
               arr_c = -3,
               f_lbl = "nam",
               p_lbl = NA,
               lbl_txt = txt_mod(),
               col_pal = pal_mod(),
               main = input$main_title
               )
  })
  output$tab_plot_ex_lbl <- renderPlot({
    req(input$main_title)
    plot_tab(prev = 0.5,
            sens = 0.5,
            spec = 0.5,
            N = 1000,
            by = "cdac",
            p_lbl = NA,
            f_lbl = "nam",
            lbl_txt = txt_mod(),
            col_pal = pal_mod(),
            main = input$main_title)
  })
## Own colors for riskyr --------------------------------------------------------------------
  pal_mod <- reactive({
    x <- replace(pal, c(1:16), 
                 c(input$col_pop,
                   input$col_cond_true,
                   input$col_cond_false,
                   input$col_dec_pos,
                   input$col_dec_neg,
                   input$col_dec_true,
                   input$col_dec_fal,
                   input$col_tp,
                   input$col_fn,
                   input$col_fp,
                   input$col_tn,
                   input$col_ppv,
                   input$col_npv,
                   input$col_text,
                   input$col_lines,
                   input$col_bgr)
                  )
  })
  # reset button for colors
  observeEvent(input$reset_colors, {
    updateTextInput(session, inputId = "col_pop", value = "#E6E6E6FC")
    updateTextInput(session, inputId = "col_cond_true", value = "lightgoldenrod1")
    updateTextInput(session, inputId = "col_cond_false", value = "#BFBFBFFC")
    updateTextInput(session, inputId = "col_dec_pos", value = "#F7A97F")
    updateTextInput(session, inputId = "col_dec_neg", value = "#A6A6A6FC")
    updateTextInput(session, inputId = "col_dec_true", value = "olivedrab4")
    updateTextInput(session, inputId = "col_dec_fal", value = "steelblue3")
    updateTextInput(session, inputId = "col_tp", value = "#B8D989")
    updateTextInput(session, inputId = "col_fn", value = "#73C8EA")
    updateTextInput(session, inputId = "col_fp", value = "#1D95C6")
    updateTextInput(session, inputId = "col_tn", value = "#80B139")
    updateTextInput(session, inputId = "col_ppv", value = "#F26418")
    updateTextInput(session, inputId = "col_npv", value = "#1D95C6")
    updateTextInput(session, inputId = "col_text", value = "#000000FC")
    updateTextInput(session, inputId = "col_lines", value = "#3E3F3A")
    updateTextInput(session, inputId = "col_bgr", value = "white")
  })
  # Example plots
  output$prism_plot_ex_col <- renderPlot({
    req(input$main_title)
    plot_prism(prev = 0.5,
               sens = 0.5,
               spec = 0.5,
               N = 1000,
               by = "ac",
               arr_c = -3,
               f_lbl = "nam",
               p_lbl = NA,
               main = input$main_title,
               col_pal = pal_mod(),
               lbl_txt = txt_mod()
               )
  })
  output$tab_plot_ex_col <- renderPlot({
    req(input$main_title)
    plot_tab(prev = 0.5,
            sens = 0.5,
            spec = 0.5,
            N = 1000,
            by = "cddc",
            p_lbl = NA,
            f_lbl = "nam",
            main = input$main_title,
            col_pal = pal_mod(),
            lbl_txt = txt_mod()
            )
  })
  output$curve_plot_ex_col <- renderPlot({
    req(input$main_title)
    plot_curve(prev = 0.3,
              sens = 0.3,
              spec = 0.7,
              N = 1000,
              what = "all",
              show_points = TRUE,
              lbl_txt = txt_mod(),
              main = input$main_title,
              col_pal = pal_mod(),
              uc = 0.05
              )
  })

}
# run App ----------------------------------------------------

shinyApp(ui = ui, server = server)
