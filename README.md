# Postttest-Probability-Shiny-App

An RShiny App for calculating posttest probabilities from binary tests (positive predictive probability). 
Multiple tests can be chained together. 
Fast mode gives only the final resulting probability while detail mode saves probabilities after each new test.
Includes a text output, datatable output and an ROC plot. 

Includes:
- 2x2 mode with Sensitivitiy calculator
- 2x2 table with various plots

To Do:
- General 
    - tests with multiple possible outcomes
    - implement fast and detail correctly in the text output
    - add xslx download format
    - interactive ROC plot design? (dimensions, colors, labels, ...)
    - add hover/click explanation labels
    - improve slider vs numeric selection?
    - add highcharter?
    - split project into single modules in different files?
    - make plots downloadable
- riskyr App emulation
    - add mosaic plot with riskyr or ggplot
    - add plot mit den balken da die so dick sind und so
    - add other riskyr plots
    - add colorsheme for plots?
    - add color customization
    - add population example slider for main part
    - rename posttest probability to ppv or npv?
    - add references list
    - add interactive plotly for riskyr-
