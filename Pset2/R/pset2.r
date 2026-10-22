######################################################
#                                                    #
# PSET 2.                                            #
#                                                    #
######################################################
# DISCLOSURE : For the code, I used GPT 6 to get help understanding the question and structure of the EM algorithms. I proofread my code using Gemini 2.5 Flash provided by WashU. For Questions 1, 2 and 4, I used GPT 6 to understand the underlying concepts using the Korean prompt, 'Do not give the answer right away, but help me understand the process.' I also had a conversation to better understand the underlying concepts and work through the problem. 

delta <- matrix(c(0, 0, 0, 
                  1, 0, 0, 
                  0, 1, 0, 
                  0, 0, 1, 
                  1, 1, 1), ncol = 3, byrow = TRUE)

counts <- c(172738, 2030, 114, 68, 50)

# Initial parameter 
50/172788 # using existing numbers 

p <- 0.0002 # the probability for being matched (prior)
a <- c(0.99, 0.99, 0.99) # when they are matched, the probability that each delta is matched 
b <- c(0.01, 0.01, 0.01) # when they are not matched, the probability that each delta is matched 

# E-Step
Ar = function(delta) {
  prod(a^delta * (1-a)^(1-delta))
}

Br = function(delta) {
  prod(b^delta * (1-b)^(1-delta))
}

gammar <- function(delta) {
  a_val <- Ar(delta)
  b_val <- Br(delta)
  
  (p * a_val) / (p * a_val + (1 - p) * b_val)
}

gammar_val <- gammar(delta)

# M-Step 
p_new <- sum(counts * gammar_val) / sum(counts)
a_new <- colSums(delta * counts * gammar_val) / sum(counts * gammar_val)
b_new <- colSums((1 - delta) * counts * gammar_val) / sum(counts * gammar_val)

# Log-likelihood function
loglike <- function(p, a, b, delta, counts) {
  # Evaluates likelihood per pattern/observation
  a_val <- Ar(delta)
  b_val <- Br(delta)
  
  sum(counts * log(p * a_val + (1 - p) * b_val))
}

# EM Algorithm 
loglike_history <- c(loglike(p, a, b, delta, counts))

for (iter in 1:10000) {
  # E-step 
  gammar_val <- gammar(delta)
  
  # M-step 
  p_new <- sum(counts * gammar_val) / sum(counts)
  a_new <- colSums(delta * counts * gammar_val) / sum(counts * gammar_val)
  b_new <- colSums((1 - delta) * counts * gammar_val) / sum(counts * gammar_val)
  
  # Update
  p <- p_new
  a <- a_new
  b <- b_new
  
  # Log-likelihood history 
  loglike_history <- c(loglike_history, 
                       loglike(p, a, b, delta, counts))
  
  # Convergence check
  if (abs(tail(loglike_history, 1) -
          tail(loglike_history, 2)[1]) < 1e-8) {
    break
  }
  
}

plot(loglike_history, type = "l",
     xlab = "Iteration",
     ylab = "Observed log-likelihood",
     main = "EM Algorithm Convergence")
