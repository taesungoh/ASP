######################################################
#                                                    #
# Lab 2 assignment: Tasks 3-5.                       #
#                                                    #
# R/assignment.r                                     #
#                                                    #
######################################################

library(Matrix)

source(file.path("R", "gendata.r"))
source(file.path("R", "assignment_helpers.r"))

set.seed(123)

threshold <- 0.5

# Task 3: Nested loops 
thresholded_distance_loop <- function(x, y, threshold = 0.5) {
  validate_pairwise_inputs(x, y)
  validate_threshold(threshold)
  distances <- matrix(
    0, nrow = length(x), ncol = length(y)
  )
  
  for (i in seq_along(x)) {
    for (j in seq_along(y)) {
      distance_first <- abs(x[i] - y[j])
      if (distance_first < threshold) {
        distances[i, j] <- 0
        }else{
            distances[i, j] <- distance_first
          }
        }
      }
  distances
}

thresholded_distance_loop(x, y)

# Task 4: Vectorized dense matrix 
thresholded_distance_vectorized <- function(x, y, threshold = 0.5) {
  validate_pairwise_inputs(x, y)
  validate_threshold(threshold)
  distances <- abs(outer(x, y, FUN = "-"))
  distances[distances < threshold] <- 0
  distances
}

thresholded_distance_loop(x, y, threshold = 0.5)

# Task 5: Vectorized sparse matrix
thresholded_distance_sparse <- function(x, y, threshold = 0.5){
  validate_pairwise_inputs(x, y)
  validate_threshold(threshold)
  distances <- abs(outer(x, y, FUN = "-"))
  distances[distances < threshold] <- 0
  sparse_matrix <- as(distances, "dgCMatrix")
  sparse_matrix
}

# How to validate threshold here? 
# thresholded_distance_sparse <- function(x, y, threshold = 0.5){
#   validate_pairwise_inputs(x, y)
#   validate_threshold(threshold)
#   distances <- thresholded_distance_loop(x, y, threshold)
#   sparse_matrix <- as(distances, "dgCMatrix")
#   sparse_matrix
# }

thresholded_distance_sparse(x, y, threshold = 0.5)

# Required correctness checks 
x_test <- c(0, 1)
y_test <- c(0, 0.5, 2)

expected <- matrix(
  c(0, 0.5, 2,
    1, 0.5, 1),
  nrow = 2,
  byrow = TRUE
)

# 1) the Task 3 result equals expected;
test1 <- thresholded_distance_loop(x_test, y_test)
test1
identical(expected, test1)

# 2) the Task 4 result equals expected;
test2 <- thresholded_distance_vectorized(x_test, y_test)
test2  
identical(expected, test2)

# 3) as.matrix() of the Task 5 result equals expected;
test3 <- as.matrix(thresholded_distance_sparse(x_test, y_test))
test3
identical(expected, test3)

# 4) the large results have dimensions 3000 x 5000;
test4_1 <- thresholded_distance_loop(lab2_vectors$x, lab2_vectors$y)
dim(test4_1)

test4_2 <- thresholded_distance_vectorized(lab2_vectors$x, lab2_vectors$y)
dim(test4_2)

test4_3 <- as.matrix(thresholded_distance_sparse(lab2_vectors$x, lab2_vectors$y))
dim(test4_3)

# 5) every dense output entry is either 0 or at least 0.5;
test5_1 <- all(test4_1 == 0 | test4_1 >= 0.5)
test5_1

test5_2 <- all(test4_2 == 0 | test4_2 >= 0.5)
test5_2

test5_3 <- all(test4_3 == 0 | test4_3 >= 0.5)
test5_3

# 6) distances exactly equal to 0.5 are preserved; and

# From Tests 1) – 3), each function preserved the value 0.5 (which appeared twice in the expected matrix).

# 7) Task 5 has the same number of nonzero entries as Task 4.
sum(test4_2 != 0)
sum(test4_3 != 0)

sum(test4_2 != 0) == sum(test4_3 != 0)

# Required performance comparison
time_loop <- elapsed_seconds(thresholded_distance_loop, 
                             lab2_vectors$x, lab2_vectors$y)

time_vectorized <- elapsed_seconds(thresholded_distance_vectorized, 
                             lab2_vectors$x, lab2_vectors$y)

time_sparce <- elapsed_seconds(thresholded_distance_sparse, 
                               lab2_vectors$x, lab2_vectors$y)

store_loop <- object.size(test4_1)
store_vectorized <- object.size(test4_2)
store_sparce <- object.size(thresholded_distance_sparse(lab2_vectors$x, lab2_vectors$y))

nonzero_loop <- Matrix::nnzero(test4_1)
nonzero_vectorized <- Matrix::nnzero(test4_2)
nonzero_sparce <- Matrix::nnzero(test4_3)

performance_table <- data.frame(
  method = c("Nested loop", "Vectorized", "Sparce"), 
  `elapsed seconds` = c(time_loop, time_vectorized, time_sparce), 
  `stored result bytes` = c(store_loop, store_vectorized, store_sparce), 
  `nonzero entries` = c(nonzero_loop, nonzero_vectorized, nonzero_sparce))

performance_table

## Calculate the Task 3-to-Task 4 time gain:
time_gain <- time_loop / time_vectorized
time_gain

## Calculate the percentage reduction in elapsed time:
time_reduction_percent <-
  100 * (1 - time_vectorized / time_loop)
time_reduction_percent

## Calculate the Task 4-to-Task 5 final-object memory reduction:
memory_reduction_percent <-
  100 * (1 - store_sparce/store_vectorized)
memory_reduction_percent

