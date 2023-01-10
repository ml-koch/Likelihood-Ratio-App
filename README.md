# Likelihood Ratio app

The app can be accessed online here: https://ml-koch.shinyapps.io/Posttest-Probability-Shiny-App/
(Currently there is a 25 User hours/month limit)
## Quick overview
An RShiny App for calculating likelihood ratios and posterior probabilities for binary tests according to [Bayesian statistics](https://en.wikipedia.org/wiki/Bayesian_statistics). 
It includes a page for clinical tests calculating diagnostic value according to test parameters and a risk visualization page based on a 2x2 input (true positive, false positives etc...). 

I plan on including a scientific/forensic page to calculate and visualize likelihood ratios and posterior probabilities of two competing hypotheses. 

## File overview
### lr_app.R
The main Shinyapp file including server and ui as well as necessary packages
### lr_functions.R
includes necessary functions for the app to work. Used to calculate posterio probabilities in fast and detail mode as well as creates the ROC plot and hte tree plot in clinical mode

## What can the app do currently?
### Clinical page
The app calculates [likelihood ratios](https://en.wikipedia.org/wiki/Likelihood_ratios_in_diagnostic_testing) for n tests based on their sensitivity and specificity. 
It then uses the input base rate and the likelihood ratios to sequentially calculate posterior probabilities following a positive or negative test result. 

If multiple tests are created then posterior probabilities will be calculated sequentially and in "Detail" mode all probabilities at every step are reported.

Fast mode gives only the final posterior probability (by combining likelihood ratios before multiplying them with the prior odd) while detail mode saves probabilities after each new test.

Features: 
- text output of posterior probabilites
- downloadable datatable for all values used in calculation (including tree plot data)
- interactive customizable ggplotly ROC plot displaying tests  
- interactive customizable ggraph tree plot displaying all possible posterior probabilites for all possible test results

Planned:
- text output of likelihood ratios and examples as for how to interpret them correctly 
- support for tests with 3+ potential results (or alternatively explanation how to convert them into binary tests as in the end often disease vs no disease is the question)

 ### 2x2 page
The app uses a 2x2 input of all values inside a [confusion matrix](https://en.wikipedia.org/wiki/Confusion_matrix).

It then calculates sensitivity and specificity of a test underlying the confusion matrix and displays various [riskyr plots](https://cran.r-project.org/web/packages/riskyr/index.html) which are customizable in name and color.

These display various values useful in analyzing decisions based on the provided values (e.g. accuracy, positive predictive value, ...)

Features:
- sensitivity and specificity calculation of confusion matrix
- interactive visualizations inspired by the [riskyr App](https://riskyr.org/) which inform decisions and test evaluation

Planned:
- Population mode for selecting a population of size N and specificity, sensitivity and base rate to visualize with riskyr plots

### Planned scientific page
TO BE BRAINSTORMED
## Bayesian theorem
(I will include references later and expand this to make any sense)
I will let wikipedia explain the concept to you: 
> 
> $P(A\mid B) = \frac{P(B \mid A) P(A)}{P(B)}$
>
> where $A$ and $B$ are events and $P(B) \neq 0$.
>
> * $P(A\mid B)$ is a conditional probability: the probability of event $A$ 
> occurring given that $B$ is true. It is also called the posterior probability 
> of $A$ given $B$.
>* $P(B\mid A)$ is also a conditional probability: the probability of event $B$ 
> occurring given that $A$ is true. It can also be interpreted as the 
> likelihood of $A$ given a fixed $B$ because $P(B\mid A)=L(A\mid B)$.
> * $P(A)$ and $P(B)$ are the probabilities of observing $A$ and $B$ 
> respectively without any given conditions; they are known as the prior 
> probability and marginal probability.

This allows us to calculate conditional probabilities using information that we know. This is important as presenting conditional information is crucial in research, clinical work (medicine/psychology) and forensics. 

For example in medicine test parameters like sensitivity (P(Test positive|Disease present)) and specificity (P(Test negative|Disease absent)) are often known but these values are the most relevant to people with a certain test result and can lead to misinformation and potentially unecessary treatment. It might be more informative to know P(Disease present|Test positive) if one has a positive test and often it is incorrectly inferred to be the same or the opposite of the sensitivity which, as we can see in the equation above, is wrong. 

TO BE REWRITTEN

### Likelihood ratio
This calculation can also be done using odds and likelihood ratios by converting the probabilities in Bayes theorem to odds and then multiplying a prior odd with a likelihood ratio to get a posterior odd. This is turn can be converted into a probability which states P(Event|Test result). 

TO BE EXPANDED



## To Do:
- Scientific mode
    - mode for calculating probabilities of competing hypothesis given a certain evidence (?)
- General ideas 
    - add text output for the clinical page explaining interpretation of results
    - tests with multiple possible outcomes
    - add xslx download format
    - add hover/click explanation labels to more fields
    - split project into single modules in different files?
        - create functions for repetitive tasks
    - improve design and layout (seek tutorials; font, colors, ...)
    - rename panels and app and also prevalance?
    - clean up code with indents and bracket placing
    - rewrite functions with pipes
    - add population mode: sensitivity, specificity and prevalence as possible inputs for "confusion matrix" tab
    - add explanatory tab explaining the use of likelihood ratios and posttetst probability/positive predictive validity
    - improve content and design of the readme
