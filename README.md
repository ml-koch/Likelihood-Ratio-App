# Postttest-Probability-Shiny-App

An RShiny App for calculating posttest probabilities from binary tests (positive predictive probability). 
Multiple tests can be chained together. 
Fast mode gives only the final resulting probability while detail mode saves probabilities after each new test.
Includes a text output, datatable output, ROC plot and a tree plot detailing all resulting probabilities for all possible test paths. 

The "2x2" tab contains several visualizations of binary decisions from the 'riskyr' package. These can be customized in name and color. 

Includes:
- Posttest probability calculator for n tests using likelihood ratios
- downloadable datatable for all values used in calculation
- interactive customizable ggplotly ROC plot displaying tests  
- interactive customizable ggraph tree plot
- 2x2 mode with sensitivitiy calculator and 'riskyr' plots.
- color and label customization for riskyr plots

To Do:
- General 
    - tests with multiple possible outcomes
    - add xslx download format
    - add hover/click explanation labels to more fields
    - split project into single modules in different files?
        - create functions for repetitive tasks
    - improve design and layout (seek tutorials; font, colors, ...)
    - rename panels and app and also prevalance?
    - clean up code with indents and bracket placing
    - rewrite functions with pipes
    - add sensitivity, specificity and prevalence as possible inputs for "2x2" tab
    - add explanatory tab explaining the use of likelihood ratios and posttetst probability/positive predictive validity
    
    
    
  The "multiple_post_probs_nonshiny.R" file includes functions to calculate posttets probabiltiy/positive predictiva validity 
