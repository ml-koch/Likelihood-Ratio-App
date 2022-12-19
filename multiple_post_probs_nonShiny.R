library(ggplot2)
library(tidyverse)
library(plotly)

multiple_post_prob <- function(sens, spec, br, 
                               test_res, method = "fast"){
  
  #check if all arguments have the same length
  # needed to determine how many test there are
  
  if(length(sens) != length(spec)||
     length(sens) != length(test_res)||
     length(spec) != length(test_res)){
    
    stop("Each argument must contain the same number of elements")
    
  }
  
  else{
    # set a variable for the amount of tests
    n <- length(sens)
    # determine initial pre-test odds
    pre_odd <- br/(1 - br)
    
    if(method == "fast"){
      
      # initialize combined LR variable
      LR_comb <- 1
      
      # Iterate through each test (n is the amount of tests)
      for(i in 1:n){
        
        # Condition for positive test result
        if(test_res[i] == "pos"){
          
          LR_pos <- sens[i]/(1 - spec[i])
          
          # Multiply LR with the combined LR's
          LR_comb <- LR_comb * LR_pos
          
        }
        
        # Condition for negative test results
        else if(test_res[i] == "neg"){
          
          LR_neg <- (1 - sens[i])/spec[i]
          
          # Multiply LR with the combined LR's
          LR_comb <- LR_comb * LR_neg
          
        }
        
        # Error if test_res it neither "pos" or "neg"
        else{stop("Tests must be entered as dichotomous [pos/neg].")}
        
      }
      
      # Multiply combined LR with pre_odds
      post_odd <- LR_comb * pre_odd
      
      post_prob <- post_odd/(1 + post_odd)
      
      return(post_prob)
      
    }
    
    # Detail will print and return probabilites at every stage
    else if (method == "detail"){
      
      # Initalize an Output list
      list_of_probs <- list()
      
      for(i in 1:n){
        
        if(test_res[i] == "pos"){
          
          # The entire process from sens/spec to post_prob is done
          # for each test to be able to report each post_prob
          LR_pos <- sens[i]/(1 - spec[i])
          
          post_odd <- LR_pos * pre_odd
          
          post_prob <- post_odd/(1 + post_odd)
          
          pre_odd <- post_odd
          
          list_of_probs <- append(list_of_probs, post_prob)
          
          print(paste0("Test ", i, " resulted in a probability of ", 
                       round(post_prob, 4)))
          
        }
        
        else if(test_res[i] == "neg"){
          
          LR_neg <- (1 - sens[i])/spec[i]
          
          post_odd <- LR_neg * pre_odd
          
          post_prob <- post_odd/(1 + post_odd)
          
          pre_odd <- post_odd
          
          list_of_probs <- append(list_of_probs, post_prob)
          
          print(paste0("Test ", i, " resulted in a probability of ", 
                       round(post_prob, 4)))
          
        }
        
      }
      
      # A list with all post_prob after each test is returned
      return(list_of_probs)
      
    }
    
    # Error if a method other than fast or detail is selected
    else{stop("Unknown method. 
              Please enter fast or detail as characters")}
    
  }
  
}

multiple_post_prob_2x2 <- function(tp, tn, fp, fn, br, 
                                   test_res, plt = FALSE){
  
  sens <- vector()
  spec <- vector()
  
  for(i in 1:length(tp)){
    
    pre_sens <- tp[i]/(tp[i]+fn[i])
    pre_spec <- tn[i]/(tn[i]+fp[i])
    
    sens <- append(sens, pre_sens)
    spec <- append(spec, pre_spec)
    
  }
  
  # set a variable for the amount of tests
  n <- length(sens)
  # determine initial pre-test odds
  pre_odd <- br/(1 - br)
  
  # Initalize a list
  list_of_probs <- list()
  list_of_LR <- list()
  list_of_br <- list()
  
  for(i in 1:n){
    
    if(test_res[i] == "pos"){
      
      # The entire process from sens/spec to post_prob is done
      # for each test to be able to calculate each post_prob
      LR_pos <- sens[i]/(1 - spec[i])
      pre_prob <- pre_odd/(1 + pre_odd)
      list_of_br <- append(list_of_br, pre_prob)
      
      post_odd <- LR_pos * pre_odd
      
      post_prob <- post_odd/(1 + post_odd)
      
      pre_odd <- post_odd
      
      list_of_probs <- append(list_of_probs, post_prob)
      list_of_LR <- append(list_of_LR, LR_pos)
      
      print(paste0("Test ", i, " resulted in a probability of ", 
                   round(post_prob, 4)))
      
    }
    
    else if(test_res[i] == "neg"){
      
      LR_neg <- (1 - sens[i])/spec[i]
      pre_prob <- pre_odd/(1 + pre_odd)
      list_of_br <- append(list_of_br, pre_prob)
      
      post_odd <- LR_neg * pre_odd
      
      post_prob <- post_odd/(1 + post_odd)
      
      pre_odd <- post_odd
      
      list_of_probs <- append(list_of_probs, post_prob)
      list_of_LR <- append(list_of_LR, LR_neg)
      
      print(paste0("Test ", i, " resulted in a posttest probability of ", 
                   round(post_prob, 4)))
      
    }
    
  }
  
  df = data.frame(unlist(sens),unlist(spec), unlist(list_of_probs),
                  unlist(list_of_LR), unlist(list_of_br), test_res)
  colnames(df) <- c("Sensitivity", "Specificity",
                    "Posttest Probability", "LR",
                    "Pretest Probability", "Test_result")
  
  if(plt){
    
    plot1 <- ggplot(df, aes(x = 1-Specificity, y = Sensitivity, 
                            color = Test_result,
                            label = LR)) + 
      geom_point() + 
      xlim(0,1) +
      ylim(0,1) +
      labs(title = "ROC plot",color = "Test result\n") +
      scale_color_manual(labels = c("Positive", "Negative"), 
                         values = c("blue", "red")) +
      geom_abline(slope = 1, intercept = 0, colour = "gray75")
    
    plot2 <- ggplotly(plot1, tooltip = c("y","x","label"))
    
    output <- list(plot2, df)
    
    return(output)
    
  }
  
  else{
    
    return(df)
    
  }
  
}

multiple_post_prob_2x2(10,20,5,5,0.1,"pos", plt = TRUE)


# Testing the function with an example from
# https://darwin.unmc.edu/dxtests/dichotom.htm
multiple_post_prob(0.98, 0.93, 0.0288, "pos")

multiple_post_prob(0.98, 0.93, 0.0288, 
                   "pos", method = "detail")

# Testing for multiple tests from our Problem 3 pdf
multiple_post_prob(c(0.95,0.60,0.54), c(0.85,0.55,0.50), 0.01, 
                   c("pos","neg","neg"))

multiple_post_prob(c(0.95,0.60,0.54), c(0.85,0.55,0.50), 0.01, 
                   c("pos","neg","neg"), method = "detail")

multiple_post_prob(c(0.95,0.60,0.54), c(0.85,0.55,0.50), 0.80, 
                   c("pos","neg","neg"))

multiple_post_prob(c(0.95,0.60,0.54), c(0.85,0.55,0.50), 0.80, 
                   c("pos","neg","neg"), method = "detail")
