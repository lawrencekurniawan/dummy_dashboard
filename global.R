#Portfolio Dashboard
#author: Lawrence Kurniawan Wong

sysenv <- Sys.getenv()

if('XPC_SERVICE_NAME' %in% names(unlist(sysenv))) {
  #if (sysenv[['UDEPLOY_DEPLOYMENT_NAME']] %in% c('production', 'staging')) {
  savepath <- "~/Projects/dummy_dashboard/dummy_dashboard/data/"
  filepath <- "~/Projects/dummy_dashboard/dummy_dashboard/"
  #}
} else {
  #filepath <- "/home/rstudio/ShinyApps/dummy_dashboard/"
  #savepath <- "/home/rstudio/ShinyApps/dummy_dashboard/data/"
  filepath <- "/home/rstudio/ShinyApps/dummy_dashboard/"
  savepath <- "/home/rstudio/ShinyApps/dummy_dashboard/data/"
  #.libPaths("/usr/local/lib/R/site-library")
  #preloaded.pkgs <- c("shiny", "htmltools", "httpuv", "digest", "xtable", "mime", "R6", "Rcpp")
  #sapply(preloaded.pkgs, unloadNamespace)
}

#savepath <- "~/Projects/dummy_dashboard/dummy_dashboard/data/"
#filepath <- "~/Projects/dummy_dashboard/dummy_dashboard/"

packages <- c("shiny", "shinydashboard", #shiny
              "dplyr", "lazyeval", "tidyr", "readr", "purrr", "stringr", "lubridate", "data.table", "sitools", "reshape2", "rlang", "glue", #data wrangling
              "scales", "htmlwidgets", "highcharter", "DT", "sparkline", "htmltools", "RColorBrewer", "ggplot2") #JS bindings / data viz

#pkgs_not_installed <- packages[!(packages %in% installed.packages())]

#if(length(pkgs_not_installed > 0)) {
#  install.packages(pkgs = pkgs_not_installed,
#                   dependencies = TRUE)
#}

lapply(packages, library, character.only = TRUE)

#load formulas for calculation of ratio metrics
load(paste0(savepath, 'formula_rds.rds'))
formula = formula_rds

#Dehypenate and hyphenate
dehyphenate <- function(string) {
  return(
    string %>%
      str_split(pattern = "_") %>%
      unlist() %>%
      str_c(collapse = " ") %>%
      str_to_title()
  )
}

hyphenate <- function(string) {
  return(
    string %>%
      str_split(pattern = " ") %>%
      unlist() %>%
      str_to_lower() %>%
      str_c(collapse = "_")
  )
}

group_hyphenate <- function(list) {
  return(
    lapply(
      list, hyphenate
    ) %>%
      unlist()
  )
}

# 1st metric classification (binary: if ratio metrics then include here, otherwise no need)
ratio_metrics <- formula_rds$metric

# 2nd metric classification (mutually exclusive: each metric only belongs to 1 group)
percentage_metrics <- c("percentage_daily_growth")

dollar_metrics <- c("open", "high", "low", "close", "adjusted", "daily_growth")

full_number_metrics <- c("volume")

two_digits_metrics <- c()

# to modify the tooltip and axis units of highcharter graphs
hc_unit <- function(metric, percentage_unit, dollar_unit, full_number_unit, two_digits_unit) {
  case_when(
    metric %in% percentage_metrics ~ percentage_unit,
    metric %in% dollar_metrics ~ dollar_unit,
    metric %in% full_number_metrics ~ full_number_unit,
    metric %in% two_digits_metrics ~ two_digits_unit
  )
}

#snapshot data generation
paste2 <- function(vector) {
  paste(vector, collapse = ",")
}

dod_change <- function(vector) {
  (last(vector) / last(lag(vector))) - 1
}

DT2 <- function(data, first_col, caption, subheader_columns, reverse_color_columns, two_digits_columns, full_number_columns, dollar_columns, percentage_columns, spark = F, spark_index, selected_metrics) {
  #reorder the data to match subheader_columns's order
  #if(length(subheader_columns) > 3) {
  ordered <- lapply(selected_metrics, function(varname) {paste0(varname, "_", subheader_columns)}) %>% unlist
  ordered <- ordered[ordered %in% names(data)]
  
  data <- dplyr::select(data, names(data)[1], ordered)
  #}
  
  #to be used inside 'sketch' object. creates a vector of subheader columns to be used for each metric selected by the user. eg: c("Last", "Dod", "Trend", "Last", "Dod", "Trend", "Per_Trip")
  th_list_for_subheader_columns <- 
    rep(subheader_columns, length(selected_metrics))
  
  #creating HTML table for the complex header
  sketch <-  
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, first_col),
          lapply(
            selected_metrics, 
            function(varname) {
              th(colspan = length(subheader_columns),
                 #varname
                 dehyphenate(varname)
              )
            }
          )
        ),
        tr(lapply(th_list_for_subheader_columns, function(varname) {
          th(varname)
        })
        )
      )
    ))
  
  select_subheader_columns <- function(metric_list, col_num = NULL) { #returns a list of columns with format like this: rider_first_trip_Last (if col_num = 1 and columns = c("Last", "Dod", "Trend"))
    lapply(metric_list, function(metric) {
      paste0(metric, "_", subheader_columns[[col_num]])
    }) %>%
      unlist()
  }
  
  vars_column_1 <- select_subheader_columns(selected_metrics, 1) #"Last" columns
  
  vars_column_2 <- select_subheader_columns(selected_metrics, 2) #"Dod" columns
  
  select_columns_by_property <- function(property_columns_for_patterns, from_column_set, has_suffix, suffix) {
    if(has_suffix) {
      lapply(property_columns_for_patterns, function(pattern) {
        str_subset(from_column_set, paste0("^", pattern, "_", suffix))
      }) %>%
        unlist()
    } else {
      lapply(property_columns_for_patterns, function(pattern) {
        str_subset(from_column_set, pattern)
      }) %>%
        unlist()
    }
  }
  
  reverse_color <- select_columns_by_property(property_columns_for_patterns = reverse_color_columns
                                              , from_column_set = vars_column_2
                                              , has_suffix = F)
  
  normal_color <- vars_column_2[!(vars_column_2 %in% reverse_color)]
  
  
  percentage <- select_columns_by_property(property_columns_for_patterns = percentage_columns
                                           , from_column_set = vars_column_1
                                           , has_suffix = T
                                           , suffix = subheader_columns[[1]])
  
  dollar <- select_columns_by_property(property_columns_for_patterns = dollar_columns
                                       , from_column_set = vars_column_1
                                       , has_suffix = T
                                       , suffix = subheader_columns[[1]]) 
  
  two_digits <- select_columns_by_property(property_columns_for_patterns = two_digits_columns 
                                           , from_column_set = vars_column_1
                                           , has_suffix = T
                                           , suffix = subheader_columns[[1]]) 
  
  full_numbers <- select_columns_by_property(property_columns_for_patterns = full_number_columns
                                             , from_column_set = vars_column_1
                                             , has_suffix = T
                                             , suffix = subheader_columns[[1]]) 
  
  col_indices <- function(col_num, max_cols = subheader_columns) {
    #seq(col_num, ncol(data), length(max_cols))
    str_which(names(data), subheader_columns[col_num])-1 #-1 because the indices will be used in javascript, and js is zero-indexed
  }
  
  if (spark) {
    
    line_string <- "type: 'line', 
    lineColor: 'black', 
    fillColor: '#ccc', 
    highlightLineColor: 'orange', 
    highlightSpotColor: 'orange', 
    width : 50"
    
    #because this is javascript, it uses zero-indexing. therefore, 4 = column 5 in R.
    cd <- list(list(targets = col_indices(col_num = spark_index), #indices of sparkline column 
                    render = JS("function(data, type, full){ 
                                return '<span class = sparkSeries>' + data + '</span>' }"),
                    orderable = F),
               list(targets = "_all",
                    className = "dt-right")
    )
    
    cb <- JS(paste0("function (oSettings, json) {
                    \n $('.sparkSeries:not(:has(canvas))')
                    .sparkline('html', { ", line_string, " });
                    \n}"), collapse = "")
    
    dt <- datatable(data.table(data), 
                    rownames = FALSE, 
                    container = sketch, 
                    caption = caption,
                    options = list(
                      columnDefs = cd, 
                      fnDrawCallback = cb, 
                      scrollX = F, #no scroll. scroll setting is made in the ui. if made T, will mess up the table after you sort the last columns
                      autoWidth = F, 
                      dom = 't', 
                      pageLength = nrow(data))) 
} else {
  dt <- datatable(data.table(data), 
                  rownames = FALSE, 
                  container = sketch, 
                  options = list(
                    scrollX = T,
                    autoWidth = T,
                    columnDefs = list(
                      list(width = "70px"
                           , targets = c(0))
                    ),
                    dom = 't', 
                    pageLength = nrow(data)))
}
  
  dt <- dt %>%
    formatCurrency(full_numbers, #for columns that have round numbers (eg trips)
                   digits = 0, 
                   currency = "", 
                   mark = ",") %>%
    formatCurrency(two_digits, #for columns that need more digits (eg C/R)
                   digits = 2, 
                   currency = "", 
                   mark = ",") %>%
    formatCurrency(dollar, #for columns that are in USD
                   digits = 2,
                   currency = "$",
                   mark = ",") %>%
    formatStyle(normal_color, #-100% is dark red, 100% is dark green
                backgroundColor = styleInterval(
                  seq(from = -1, to = 1, length.out = 9),  
                  brewer.pal(10, "RdYlGn")), 
                color = styleInterval(
                  seq(from = -1, to = 1, length.out = 9), 
                  c(rep("white", 2), rep("black", 6), rep("white", 2)))) %>%
    formatStyle(reverse_color, #-100% is dark green, 100% is dark red
                backgroundColor = styleInterval(
                  seq(from = -1, to = 1, length.out = 9),  
                  rev(brewer.pal(10, "RdYlGn"))), 
                color = styleInterval(
                  seq(from = -1, to = 1, length.out = 9), 
                  c(rep("white", 2), rep("black", 6), rep("white", 2)))) %>%
    formatPercentage(append(vars_column_2, percentage), #all Dod columns have 1 digits
                     digits = 1)
  
  if (spark) {
    dt$dependencies <- append(dt$dependencies, htmlwidgets::getDependency("sparkline"))
    
    dt
  }
  else {
    dt
  }
  }
