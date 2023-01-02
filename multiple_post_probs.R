multiple_post_prob <- function(sens, spec, br, 
                               test_res, method = "fast") {

  # set a variable for the amount of tests
  n <- length(sens)
  
  # determine initial pre-test odds
  pre_odd <- br / (1 - br)
  
  # Initialize output dataframe
  ## create NA vectors for pretest,LR and posttest
  ## I used NA to indicate to user that gaps are not computed in "fast"
  names_v <- c(paste0("Test ", 1:n))
  pretest_v <- as.numeric(rep(NA, n))
  LR_v <- as.numeric(rep(NA, n))
  posttest_v <- as.numeric(rep(NA, n))
  
  # enter the base rate as the first probability
  pretest_v[1] <- br
  
  ## Put together dataframe
  df <- data.frame(names_v, sens, spec, test_res,
                    LR_v, pretest_v, posttest_v)
  
  if (method == "fast") {
    
    # initialize combined LR variable
    LR_comb <- 1
    
    # Iterate through each test (n is the amount of tests)
    for (i in 1:n) {
      # Condition for positive test result
      if (test_res[i] == "positive") {
        
        LR_pos <- sens[i] / (1 - spec[i])
        
        df$LR_v[i] <- LR_pos
        
        # Multiply LR with the combined LR's
        LR_comb <- LR_comb * LR_pos
        
      }
      
      # Condition for negative test results
      else if (test_res[i] == "negative") {
        
        LR_neg <- (1 - sens[i]) / spec[i]
        
        df$LR_v[i] <- LR_neg
        
        # Multiply LR with the combined LR's
        LR_comb <- LR_comb * LR_neg
        
      }
    }
    
    # Multiply combined LR with pre_odds
    post_odd <- LR_comb * pre_odd
    # make post odd into a probability
    post_prob <- post_odd / (1 + post_odd)
    
    df$posttest_v[n] <- post_prob 
  }
  
  # Detail will print and return probabilities at every stage
  else if (method == "detail") {
    for (i in 1:n) {
      if (test_res[i] == "positive") {
        # The entire process from sens/spec to post_prob is done
        # for each test to be able to report each post_prob
        LR_pos <- sens[i] / (1 - spec[i])
        
        post_odd <- LR_pos * pre_odd
        
        post_prob <- post_odd / (1 + post_odd)
        
        pre_odd <- post_odd
        
        df$LR_v[i] <- LR_pos
        df$posttest_v[i] <- post_prob
        # set post prob as next pre prob in dataframe except for the last test
        if (i < n) {
          df$pretest_v[i + 1] <- post_prob}
        
      }
      
      else if (test_res[i] == "negative") {
        
        LR_neg <- (1 - sens[i]) / spec[i]
        
        post_odd <- LR_neg * pre_odd
        
        post_prob <- post_odd / (1 + post_odd)
        
        pre_odd <- post_odd
        
        df$LR_v[i] <- LR_neg
        df$posttest_v[i] <- post_prob
        # set post prob as next pre prob in dataframe except for the last test
        if (i < n) {
          df$pretest_v[i + 1] <- post_prob}
        
        }
      
    }
  }
  # Return the complete dataframe with correct names 
    colnames(df) <- c("Test Id", "Sensitivity", "Specificity",
                      "Result", "LR", "Pretest Probability",
                      "Posttest Probability")
    return(df)
}

# ROC plot function -------------------------------------------
roc_plot <- function(df) {
  
  plot1 <- ggplot(df, aes(x = 1 - Specificity, y = Sensitivity, 
                          color = Result,
                          label = LR)) + 
    geom_point() + 
    xlim(0, 1) +
    ylim(0, 1) +
    labs(title = "ROC plot", color = "Test result\n") +
    scale_color_manual(labels = c("Positive", "Negative"), 
                       values = c("blue", "red")) +
    geom_abline(slope = 1, intercept = 0, colour = "gray75")
  
  return(plot1)
  
}

# create dynamic text UI for detail output
create_detail_text <- function(data) {
  n <- nrow(data)
  out <- c()

  for (i in 1:n) {
    string <- paste("The probability of a true positive after a", data[i, 4], "result on", data[i, 1],
                    "is", round(data[i, 7], 4))
    out[i] <- paste0(string, "\n")
  }
  return(out)
}