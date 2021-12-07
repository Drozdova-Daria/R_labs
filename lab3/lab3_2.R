library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

load("ExpImp.RData")


create_data <- function(data) {
  data_district = data[grep('область$', data$Регион),]
  print(data_district)
  import = apply(data_district[grep('Импорт$', names(data_district))], 1, function(x) sum(as.numeric(str_replace_all(x, "-", "0"))))
  export = apply(data_district[grep('Экспорт$', names(data_district))], 1, function(x) sum(as.numeric(str_replace_all(x, "-", "0"))))
  data_district['СумИмпорт'] = import
  data_district['СумЭкспорт'] = export
  regions_import_export = data_district %>%
    select(1,14,15)
  
  return (regions_import_export)
}

build_graph <- function(data, x_data, y_data, fill_data, title, x_title, y_title, fill_title)  {
  return (ggplot(data, aes(x = x_data, y = y_data, fill = fill_data)) + geom_col(position = "dodge") +
            xlab(x_title) + ylab(y_title) + theme(axis.text.x = element_text(angle = 90)) +
            ggtitle(title) + scale_fill_discrete(name = fill_title))
}

build_cord_graph <- function(data, x_data, y_data, fill_data, label_data, title, x_title, y_title, fill_title)  {
  ggplot(data, aes(x = x_data, y = y_data, group = fill_data)) + 
    geom_col(aes(fill = fill_data)) +
    xlab(x_title) + ylab(y_title) +
    ggtitle(title) + scale_fill_discrete(name = fill_title) + coord_flip() +
    geom_text(aes(label = label_data), position = position_stack(vjust = 0.5))
}

regions = create_data(ExpImp)
regions_ti <- regions %>%
  pivot_longer(names_to = 'export_import',
               values_to = 'values',
               cols = -Регион)

export_import_regions = build_graph(regions_ti, 
                             regions_ti$Регион, 
                             regions_ti$values,
                             regions_ti$export_import,
                             'Суммарный экспорт и импорт по регионам России',
                             'Регионы',
                             'Объем экспорта',
                             'Экспорт/импорт')
export_import_regions
export_import_regions + facet_grid(. ~ regions_ti$export_import)

main_regions = subset(regions_ti, regions_ti$values > 2000)
export_import_regions_main = build_cord_graph(main_regions, 
                                              main_regions$Регион, 
                                              main_regions$values,
                                              main_regions$export_import,
                                              round(main_regions$values/1000, 2),
                                              'Ведущие регионы России по объему экспорта и импорта',
                                              'Регионы',
                                              'Объем экспорта',
                                              'Экспорт/импорт')

export_import_regions_main

                                       