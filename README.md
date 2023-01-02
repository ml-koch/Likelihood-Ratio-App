# Postttest-Probability-Shiny-App

An RShiny App for calculating posttest probabilities from binary tests (positive predictive probability). 
Multiple tests can be chained together. 
Fast mode gives only the final resulting probability while detail mode saves probabilities after each new test.
Includes a text output, datatable output and an ROC plot. 

Includes:
- Posttest probability calculator for n tests using likelihood ratios
- downloadable datatable for all values used in calculation
- interactive ggplotly ROC plot displaying tests  
- 2x2 mode with Sensitivitiy calculator
- 2x2 table with riskyr plots
- downloadable plots
- references
- color and label customization for riskyr plots

To Do:
- General 
    - tests with multiple possible outcomes
    - add xslx download format
    - add hover/click explanation labels
    - add highcharter?
    - split project into single modules in different files?
        - create functions for repetitive tasks
    - improve design and layout (seek tutorials; font, colors, ...)
    - rename panels and app and also prevalance?
    - clean up code with indents and bracket placing
