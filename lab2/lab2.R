library(stringr)

create_data_frame <- function(data) {
  data_district = data[grep('округ$', data$Регион),]
  print(data_district)
  import = apply(data_district[grep('Импорт$', names(data_district))], 1, function(x) sum(as.numeric(str_replace_all(x, "-", "0"))))
  export = apply(data_district[grep('Экспорт$', names(data_district))], 1, function(x) sum(as.numeric(str_replace_all(x, "-", "0"))))
  data_district['СумИмпорт'] = import
  data_district['СумЭкспорт'] = export
  return (data_district)
}

compare_export_import <- function(data) {
  return (subset(data, СумЭкспорт > СумИмпорт)$Регион)
}

load("~/lab2/ExpImp.RData")
data_district = create_data_frame(ExpImp)
print(data_district)
districts = compare_export_import(data_district)
print(districts)
