######################################################
#                                                    #
# PSET 1                                             #
#                                                    #
######################################################
# DISCLOSURE : For the code, I used Gemini 3.6 Flash provided by WashU to help understand the structure of the bootstrap loop, interpret error messages, and generate a histogram using vector values. For Questions 1–4, I used Gemini 3.6 Flash provided by WashU to understand the underlying concepts using the Korean prompt, 'Do not give the answer right away, but help me understand the process.' I then wrote all the answers myself.

# Q5
# (a) Write R code that uses Monte Carlo simulation to approximate the randomization distribution under the sharp null hypothesis. Use at least B = 100,000 random assignments. 

actual <- c(rep(1, 50), rep(0, 50))

bootstrap_loop <- function(data,
                           B = 100000L,
                           seed = 123L) {
  set.seed(seed)
  n <- length(data)
  bootstrap_estimates <- numeric(B)
  
  for (b in seq_len(B)) {
    indices <- sample.int(n, size = n, replace = TRUE)
    bootstrap_estimates[b] <- sum(data[indices] == data)
  }
  return(bootstrap_estimates)
}

# (b) For each simulated assignment, calculate the number of correctly classified cups. 
result <- bootstrap_loop(actual)
sum(result == 50)
# 7941 cases 

# (c) Plot the simulated randomization distribution and indicate the observed statistic Sobs = 67 with a vertical line. 
hist(result)
abline(v = 67, col = "red")

# (d) Estimate the one-sided permutation p-value. 
p <- mean(result >= 67)
p
# [1] 0.00035

# (e) Based on the estimated p-value, would you reject the null hypothesis at the 5% significance level? Explain what your conclusion means in the context of the experiment. 

# We can reject the null hypothesis. This means that it is extremely rare to consider that she is just randomly pick the order of milk or tea. 
