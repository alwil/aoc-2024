# Load libraries
library(here)
library(stringr)



# Load the data -----------------------------------------------------------
text_d3 <-readLines(here("data", "day03_input.txt"))

# Part 1 ----------------------------------------------------------------

muls <- str_extract_all(text_d3, "mul\\([0-9]{1,3},[0-9]{1,3}\\)") |>
  unlist() |>
  str_replace_all(",", "*") |>
  str_remove_all("^(mul)") |>
  sapply(function(x) eval(parse(text=x))) |>
  sum()

# Part 2 ----------------------------------------------------------------

# include do's and don'ts
muls_do <- str_extract_all(text_d3,
                           "(mul\\([0-9]{1,3},[0-9]{1,3}\\))|(don't\\(\\))|(do\\(\\))") |>
  unlist() |>
  str_replace_all(",", "*") |>
  str_remove_all("^(mul)")


# loop with a multiplier (mul_status) to 'switch on' or 'switch off' calculation

counter <- 0
mul_status <- 1
for (i in 1: length(muls_do)) {
  if (!muls_do[i] %in%  c("don't()", "do()" ))
    counter <- counter + (eval(parse(text=muls_do[i])))* mul_status
  if (muls_do[i]==  "don't()")
    mul_status <- 0
  if (muls_do[i] ==  "do()")
    mul_status <- 1

}


