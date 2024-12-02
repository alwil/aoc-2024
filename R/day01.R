
# Part 1 ------------------------------------------------------------------


# (...) To find out, pair up the numbers and measure how far apart they are. Pair  up the smallest number in the left list with the smallest number in the right list, then the second-smallest left number with the second-smallest right number, and so on.
# Within each pair, figure out how far apart the two numbers are; you'll need to add up all of those distances. For example, if you pair up a 3 from the left list with a 7 from the right list, the distance apart is 4; if you pair up a 9 with a 3, the distance apart is 6.


# Load libraries ----------------------------------------------------------
library(here)
library(data.table)


# Load the data -----------------------------------------------------------
input_data1 <-fread(here("data", "day01_input.csv"))


# Reorder each of the columns ---------------------------------------------

ordered_data <- data.table(
  loc_id_left =  input_data1[order(location_ID_1)]$location_ID_1,
  loc_id_right = input_data1[order(location_ID_2)]$location_ID_2
  )

# Calculate the differences between left and right columns -----------------

ordered_data[,delta_loc := abs(loc_id_left - loc_id_right)]

# Final difference :
result_part1 <- ordered_data[,sum(delta_loc)]


# Part 2 ------------------------------------------------------------------

# This time, you'll need to figure out exactly how often each number from the left list appears in the right list. Calculate a total similarity score by adding up each number in the left list after multiplying it by the number of times that number appears in the right list.

# calculate number of occurances:
ordered_data[, occurences_right := sapply(loc_id_left, function(x) sum(loc_id_right == x))]

# Multiple number by occurances

ordered_data[, multiplied_occurences := loc_id_left * occurences_right]



# Print the result
result_part2 <- ordered_data[,sum(multiplied_occurences)]

