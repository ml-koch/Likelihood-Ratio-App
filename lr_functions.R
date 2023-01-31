library(tidyverse)
multiple_post_prob_fast <- function(sens, spec, br, test_res) {

  # set a variable for the amount of tests
  n <- length(sens)
  
  # determine initial pre-test odds
  pre_odd <- br / (1 - br)
  
  # Initialize output dataframe
  ## create NA vectors for pretest, LR and posttest
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

# Return the complete dataframe with correct names 
  colnames(df) <- c("Test Id", "Sensitivity", "Specificity",
                    "Result", "LR", "Pretest Probability",
                    "Posttest Probability")
  return(df)
}

multiple_post_prob_detail <- function(sens, spec, br, test_res) {

  # set a variable for the amount of tests
  n <- length(sens)
  
  # determine initial pre-test odds
  pre_odd <- br / (1 - br)
  
  # Initialize output dataframe
  ## create NA vectors for pretest, LR and posttest
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

  # Detail will print and return probabilities at every stage
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
# Return the complete dataframe with correct names 
  colnames(df) <- c("Test Id", "Sensitivity", "Specificity",
                    "Result", "LR", "Pretest Probability",
                    "Posttest Probability")
  return(df)
}

multiple_post_prob_tree <- function(sens, spec, br, test_res) {

  # get amount of tests
  n <- length(sens)

  # create edges of tree
  edges <- data.frame(from = "Prevalence",
                      to = paste("T",
                                  1,
                                  c("P", "N"),
                                  sep = ""))
  if (n > 1) {
    for (i in 2:n) {
      # Which are the preceeding test names?
      preceeding <- edges[grep(paste("T", i - 1, sep = ""), edges$to), 2]

      exp_gr <- expand_grid(preceeding,  paste("T",
                                          i,
                                          c("P", "N"),
                                          sep = ""))

      # Put into partial dataframe
      temp <- data.frame(from = exp_gr[, 1],
                         to = paste(unlist(exp_gr[, 1]), 
                                    unlist(exp_gr[, 2]),
                                    sep = "_")
                        )
      colnames(temp) <- c("from", "to")
      edges <- rbind(edges, temp)
    }
  }

  # create a dataframe for vertices by using ids created for edges
  vertices <- data.frame(id = c(edges$from[1], edges$to),
                         test = NA,
                         sens = NA,
                         spec = NA,
                         LR = NA,
                         pretest_prob = NA,
                         prob = NA,
                         result = NA,
                         odd = NA,
                         parents = NA)
  # create test result from names
  vertices[2:nrow(vertices), ] <- vertices %>%
                                  filter(row_number() > 1) %>%
                                  mutate(result = substr(id, nchar(id), nchar(id))) 
  # create test from id
  vertices[2:nrow(vertices), ] <- vertices %>%
                                  filter(row_number() > 1) %>%
                                  mutate(test = substr(id, nchar(id) - 2, nchar(id) - 1)) 
  # create probabilities at every step
  # LR vector
  LR_pos <- c()
  LR_neg <- c()
  # calculate both LR for every test
  for (i in 1:n) {
    LR_pos_temp <- sens[i] / (1 - spec[i])
    LR_neg_temp <- (1 - sens[i]) / spec[i]

    LR_pos[i] <- LR_pos_temp
    LR_neg[i] <- LR_neg_temp

    # fill in LR
    vertices$LR[which(vertices$test == paste0("T", i) & vertices$result == "P")] <- LR_pos[i]
    vertices$LR[which(vertices$test == paste0("T", i) & vertices$result == "N")] <- LR_neg[i]

    # also add sens and spec
    vertices$sens[which(vertices$test == paste0("T", i))] <- sens[i]
    vertices$spec[which(vertices$test == paste0("T", i))] <- spec[i]
  }

  # setup prevalence
  vertices$prob[1] <- br
  pre_odd <- br / (1 - br)
  vertices$odd[1] <- pre_odd

  # calculate probabilities
  for (i in 1:n) {
    # special case for first test since prevalence does not fit into naming scheme
    if (i == 1) {
      #storing current path for convenience 
      current_path_pos <- which(vertices$id == "T1P")
      current_path_neg <- which(vertices$id == "T1N")

      post_odd_pos <- LR_pos[i] * pre_odd
      post_odd_neg <- LR_neg[i] * pre_odd

      post_prob_pos <- post_odd_pos / (1 + post_odd_pos)
      post_prob_neg <- post_odd_neg / (1 + post_odd_neg)
      
      vertices$prob[current_path_pos] <- post_prob_pos
      vertices$prob[current_path_neg] <- post_prob_neg
      vertices$odd[current_path_pos] <- post_odd_pos
      vertices$odd[current_path_neg] <- post_odd_neg

      # parent paths
      vertices$parents[current_path_pos] <- "Prevalence"
      vertices$parents[current_path_neg] <- "Prevalence"

      # pretest_prob
      vertices$pretest_prob[current_path_pos] <- br
      vertices$pretest_prob[current_path_neg] <- br
    }
    else {
      # now indexed by which test in extra column can be used
      parent_paths <- vertices$id[which(vertices$test == paste0("T", i - 1))]
      child_paths <- vertices$id[which(vertices$test == paste0("T", i))]
      # for each unique parent node 
      for (j in seq_along(parent_paths)) {
        # get the pre_odd and pre_prob from the respective parent path
        pre_odd <- vertices$odd[which(vertices$id == parent_paths[j])]
        pre_prob <- vertices$prob[which(vertices$id == parent_paths[j])]
        #storing current child paths for convenience 
        current_path_pos <- which(vertices$id == child_paths[j * 2 - 1])
        current_path_neg <- which(vertices$id == child_paths[j * 2])

        post_odd_pos <- LR_pos[i] * pre_odd
        post_odd_neg <- LR_neg[i] * pre_odd
        post_prob_pos <- post_odd_pos / (1 + post_odd_pos)
        post_prob_neg <- post_odd_neg / (1 + post_odd_neg)

        vertices$prob[current_path_pos] <- post_prob_pos
        vertices$prob[current_path_neg] <- post_prob_neg
        vertices$odd[current_path_pos] <- post_odd_pos
        vertices$odd[current_path_neg] <- post_odd_neg
        vertices$pretest_prob[current_path_pos] <- pre_prob
        vertices$pretest_prob[current_path_neg] <- pre_prob

        # parent paths
        vertices$parents[current_path_pos] <- parent_paths[j]
        vertices$parents[current_path_neg] <- parent_paths[j]
      }
    }
  }
  colnames(vertices) <- c("Test_path", "Test_name", "Sensitivity",
                          "Specificity", "LR", "Pretest_probability",
                          "Posttest_probability", "Test_result", 
                          "Posttest_odds", "Parent_path")
  return(list(edges, vertices))
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

# create dynamic text UI for lr text formulation
create_lr_text <- function(data, mode) {
  out <- c()

  if (mode == "normal") {
    n <- nrow(data)
    for (i in 1:n) {
    string <- paste("The likelihood of a positive condition after a", data[i, 4], "result on", data[i, 1],
                    "is", round(data[i, 5], 2), "times as much as the same result under a negative condition.")
    out <- append(out, paste0(string, sep = "\n"))
    }
  }
  else if (mode == "tree") {
    x <- data %>% distinct(Test_name, LR, Test_result)
    x$Test_name <- str_replace(string = x$Test_name, pattern = "T", replacement = "Test ")
    x$Test_result[x$Test_result == "P"] <- "positive"
    x$Test_result[x$Test_result == "N"] <- "negative"
    n <- nrow(x)
    for (i in 2:n) {
      string <- paste("The likelihood of a positive condition after a", x[i, 3], "result on", x[i, 1],
                      "is", round(x[i, 2], 2), "times as much as the same result under a negative condition.")
      out <- append(out, paste0(string, sep = "\n"))
    }
  }
  
  return(out)
}
