get_id <- function(data) {
  new_data = merge(data[1], data[2], by='id', all=TRUE, suffixes = c(1, 2))
  
  for (i in 3:length(data)) {
    new_data = merge(new_data, data[i], by='id', all=TRUE, suffixes = c(i-1, i))
  }
  
  new_data = new_data[complete.cases(new_data), ]
  n = apply(new_data[,-1], 1, function(x) mean(x))
  return = (data.frame(id = new_data$id, mean_temp = n))
}


load("data.RData")
new_data = get_id(data)
print(new_data)
