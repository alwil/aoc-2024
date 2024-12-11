# Load libraries
library(here)
#library(stringr)


# Load the data -----------------------------------------------------------
text_d4 <-readLines(here("data", "day04_input.txt"))


mat_d4 <- strsplit(text_d4, NULL) |>
  unlist() |>
  matrix(nrow = length(text_d4), byrow = TRUE)


s <- c("X", "M", "A", "S")

m <- x

for (i in 1:nrow(x)){
  for (j in 1:ncol(x)){
    if (i == 1 & j == 1){ check_set = c(x[i,j+1],
                                        x[i+1,j],
                                        x[i+1,j+1])
    } else

      if (i > 1  & i < nrow(x) & j == 1){
      } else

        if (i == 1 & j > 1  & j < ncol(x)){ check_set = c(x[i,j+1],
                                                          x[i+1,j],
                                                          x[i+1,j+1],
                                                          x[i,j-1],
                                                          x[i+1,j-1])
        } else

          if (i == nrow(x) & j == ncol(x) ){ check_set = c(x[i,j-1],
                                                           x[i-1,j],
                                                           x[i-1,j-1])
          } else

            if (i == nrow(x) & j > 1  & j < ncol(x)){ check_set = c(x[i,j+1],
                                                                    x[i-1,j],
                                                                    x[i-1,j+1],
                                                                    x[i,j-1],
                                                                    x[i-1,j-1])
            } else

              if (i > 1  & i < nrow(x) & j == ncol(x)){ check_set = c(x[i,j-1],
                                                                      x[i-1,j-1],
                                                                      x[i+1,j-1],
                                                                      x[i-1,j],
                                                                      x[i+1,j])
              } else

                check_set = c(x[i,j-1],
                              x[i,j+1],
                              x[i-1,j-1],
                              x[i-1,j+1],
                              x[i+1,j-1],
                              x[i+1,j+1],
                              x[i-1,j],
                              x[i+1,j])
  }
}

#     if (x[i,j] == "X" & !"M" %in%
#         ) { m[i,j] == "."}
#   }
#   }
#
#
# for (k in 1:(length(s))){
#   if (k < length(s)){
#     if (x[i,j] == s[k]){
#       if (  s[k+1]  %in% c(x[i,j-1],
#                            x[i,j+1],
#                            x[i-1,j-1],
#                            x[i-1,j+1],
#                            x[i+1,j-1],
#                            x[i+1,j+1],
#                            x[i-1,j],
#                            x[i+1,j]) )
#         next
#     } else
#       m[i,j] = "."
#   } else if (  s[k-1]  %in% c(x[i,j-1],
#                               x[i,j+1],
#                               x[i-1,j-1],
#                               x[i-1,j+1],
#                               x[i+1,j-1],
#                               x[i+1,j+1],
#                               x[i-1,j],
#                               x[i+1,j]) )
#     next
# } else
#   m[i,j] = "." next
