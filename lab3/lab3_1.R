library(ggplot2)
library(dplyr)

load("trades.RData")

merge_trades <- function(trades) {
  c = trades[[1]]
  for (i in 2:length(trades)) {
    c = mapply(c, c, trades[[i]], SIMPLIFY=FALSE)
  }
  
  return (subset(as.data.frame(c), select = -geo ))
}

get_export_or_import <- function(all_trades, name_exp_imp) {
  return (all_trades[all_trades$indic_et == name_exp_imp, ])
}

build_graph <- function(data, x_data, y_data, fill_data, title, x_title, y_title, fill_title)  {
  return (ggplot(data, aes(x = x_data, y = y_data, fill = fill_data)) + geom_col(position = "dodge") +
    xlab(x_title) + ylab(y_title) +
    ggtitle(title) + scale_fill_discrete(name = fill_title))
}

build_cord_graph <- function(data, x_data, y_data, fill_data, label_data, title, x_title, y_title, fill_title)  {
  ggplot(data, aes(x = x_data, y = y_data, group = fill_data)) + 
    geom_col(aes(fill = fill_data)) +
    xlab(x_title) + ylab(y_title) +
    ggtitle(title) + scale_fill_discrete(name = fill_title) + coord_flip() +
    geom_text(aes(label = label_data), position = position_stack(vjust = 0.5))
}

all_trades = merge_trades(trades)

export_name = 'Exports in million of ECU/EURO'
import_name = 'Imports in million of ECU/EURO'
export = get_export_or_import(all_trades, export_name)
import = get_export_or_import(all_trades, import_name)
export_import = bind_rows(export, import)

all_export_import = build_graph(export_import, 
                  export_import$indic_et, 
                  export_import$values,  
                  export_import$indic_et, 
                  'Общий объем импорта и экспорта в Европе за 2008-2019 гг.',
                  'Ипорт/Экспорт',
                  'Объем импорта/экспорта',
                  'Ипорт/Экспорт')
all_export_import

export_products = build_graph(export, 
                   export$partner, 
                   export$values,  
                   export$sitc06, 
                   'Общий объем экспорта в Европе за 2008-2019 гг. (с товарами)',
                   'Страны-партнеры',
                   'Объем экспорта',
                   'Экспортируемые товары') + coord_flip()
export_products 

export_lead_partners = subset(export, export$value > 10000) 
lead_export_products =  build_graph(export_lead_partners, 
                    export_lead_partners$partner, 
                    export_lead_partners$values,  
                    export_lead_partners$sitc06, 
                    'Ведущие экспортеры в Европе за 2008-2019 гг.',
                    'Страны-партнеры',
                    'Объем экспорта',
                    'Экспортируемые товары') + coord_flip()
lead_export_products

share_export_name = 'Share of exports by partner (%)'
share_export = all_trades[all_trades$indic_et == share_export_name, ]
share_export_data = cbind(export, share = share_export$values)
dates = unique(share_export_data$time)
lapply(dates, function(date) {
  export_data = subset(share_export_data, share_export_data$time == date & share_export_data$value >= 10000)
  build_cord_graph(export_data,
              export_data$partner,
              export_data$values,  
              export_data$sitc06, 
              export_data$share,
              paste0('Ведущие экспортеры в Европе за ', substring(date, 1, 4), ' г.'),
              'Страны-партнеры',
              'Объем экспорта',
              'Экспортируемые товары') 

})
