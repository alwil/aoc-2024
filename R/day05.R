# Library
library(here)

# Load data
order_raw <-readLines(here("data", "day05_input_1.txt"))

report_raw <-readLines(here("data", "day05_input_2.txt"))


# Part 1
# The notation X|Y means that if both page number X and page number Y are to be produced as part of an update, page number X must be printed at some point before page number Y.


orders <- strsplit(order_raw, "|", fixed = T) |>
  unlist() |>
  as.numeric() |>
  matrix(nrow = length(order_raw), byrow = TRUE)

reports <- strsplit(report_raw, ",", fixed = T) |>
  lapply( function(x) as.numeric(x))
ord <- orders
repo <- reports

# start searching the first element of the row.
# does it exist in the element of the list vector?
# If yes, check if the second element of the row exists
# if so, check if it is after the first one

n_errors <- rep(0, length(repo))

for (r in 1: length(repo)){

  for (i in 1:nrow(ord)){

     if(ord[i,1] %in%  repo[[r]]) {

       if(ord[i,2] %in%  repo[[r]]){
         n_errors[r] = n_errors[r] +
           (
             which(repo[[r]] == ord[i,1]) >
               which(repo[[r]] == ord[i,2])
           )
       }
     }
  }
}


correct_repo <- Map(function(x, y) x * y, repo, (n_errors == 0))

# What do you get if you add up the middle page number from those correctly-ordered updates?

part1_result <- sum(
  sapply(
    correct_repo, function(x) x[ceiling(length(x)/2)]
    )
  )

# Part 2

