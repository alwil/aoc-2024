# Load libraries
library(here)
library(data.table)


# Part 1 ------------------------------------------------------------------

# Load the data -----------------------------------------------------------
dt <-fread(here("data", "day02_input.csv"))

# Save initial column names
initial_cols <- copy(names(dt))


# function to calculate differences between consecutive columns
calc_diff <- function(dt){
for (i in 1:(ncol(dt) - 1)) {
  dt[, paste0("diff_", i) := .SD[[i + 1]] - .SD[[i]], by = .I]
  }
}

# Function that
# Finds the maximum abs value for each row, and check if all are monotonic
# Safe column determines if the 'safe' condition is fulfilled

check_safe<- function (dt, sdcols) {
dt[, `:=`(  monotonic = apply(.SD, 1,
                              function(x)
                                all(x > 0, na.rm = T) | all(x < 0, na.rm = T)),
            max_diff  = apply(.SD, 1, function(x) max( abs( x ), na.rm = T))
), .SDcols = sdcols][,safe := monotonic == T & max_diff < 4]
}


# Calculate how many reports fulfill safe condition

calc_diff(dt) # differences


sdcols <- colnames(dt)[grep( "diff_", colnames(dt))] # difference colnames
check_safe(dt, sdcols) # check safe condition
part1_result <- dt[ ,sum(safe)] # calculate  number of safe reports

print(part1_result)

# Part 2
# Repeat procedure from part 1 for ncol-1 datasets, each time removing one column
# at a time

list_safe_scores <- list() # save results in a list

for (i in initial_cols) {
  dti <- dt[,..initial_cols][,-..i]
  calc_diff(dti)
  sdcols <- colnames(dti)[grep( "diff_", colnames(dti))]
  check_safe(dti, sdcols)
  list_safe_scores[[i]] <- dti[, safe]
}

safe_dt <- as.data.table(list_safe_scores)  # convert to data.table
names(safe_dt) <- paste0(initial_cols, "_safe")

dt <- cbind(dt, safe_dt ) # merge with initial dataset

safe_cols <- colnames(dt)[grep( "safe", colnames(dt))]


dt[, `:=`(  total_safe = apply(.SD, 1, sum)),
   .SDcols = safe_cols]         # calculate score of safe reports

part2_result <- dt[ ,sum(total_safe>0)] # calculate  number of safe reports

print(part2_result)
