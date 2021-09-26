library(stringr)

update_data <- function (elem) {
  str_w_spaces <- str_replace_all(elem, " ", "")
  if (is.na(as.numeric(str_w_spaces))) {
    return(elem)
  } else {
    return(as.numeric(str_w_spaces))
  }
}

# Read file
myfile = read.csv('test_data_01.csv')
print(data.frame((apply(myfile, MARGIN=c(1,2), FUN = update_data))))
