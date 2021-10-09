library(stringr)

update_data <- function (elem) {
  str_w_spaces <- str_replace_all(elem, " ", "")
  if (is.na(as.numeric(str_w_spaces))) {
    return(elem)
  } else {
    return(as.numeric(str_w_spaces))
  }
}

fix_data <- function (data) {
  return(data.frame((apply(data, MARGIN=c(1,2), FUN = update_data))))
}

myfile = read.csv('test_data_01.csv')
print(fix_data(myfile))
