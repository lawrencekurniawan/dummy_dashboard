#data loading and general settings
# DATA LOADING

load(paste0(savepath, 'portfolio.rds'))

##### create lists for settings
instrument_list <- unique(portfolio$instrument)

snap_metrics <- names(portfolio)[!(names(portfolio) %in% c("date", "instrument"))]

dehyphenated_snap_metrics <- unlist(lapply(snap_metrics, dehyphenate))

##highcharter theme
newtheme <- hc_theme_merge(
  getOption("highcharter.theme")#,
  #hc_theme(colors = c("#008000","#0088cc","#ff9900"))
)

options(highcharter.theme = newtheme) #setting theme as default

# server code starts
shinyServer(function(input, output) {
  ####################################################################################
  #selector filters
  ####################################################################################
  ####################################################################################
  #snapshot
  ####################################################################################
  output$snap_select_metric <- renderUI({
    selectizeInput("snap_select_metric", 
                   label = h4("Metrics"), 
                   choices = dehyphenated_snap_metrics,
                   multiple = TRUE, 
                   selected = dehyphenated_snap_metrics[1:5],
                   width = "100%") 
  })
  
  output$snap_select_instrument <- renderUI({
    selectizeInput("snap_select_instrument",
                   label = h4("Instruments"),
                   choices = instrument_list,
                   multiple = TRUE,
                   selected = instrument_list,
                   width = '100%')
  })

  output$snap_select_date <- renderUI({
    dateRangeInput("snap_select_date",
                   label = h4("Period"),
                   start = today() - 7 * 4, #8 weeks ago
                   end = today(),
                   width = '100%')
  })
  
  ####################################################################################
  #metric explorer
  ####################################################################################
  
  output$me_select_metric <- renderUI({
    selectInput("me_select_metric", label = h4("Select Metric:"),
                choices = dehyphenated_snap_metrics,
                multiple = F,
                width = '100%'
    )
  })
  
  output$me_select_instrument <- renderUI({
    selectInput("me_select_instrument", label = h4("Select instrument:"),
                choices = instrument_list,
                multiple = T,
                selected = instrument_list,
                width = '100%'
    )
  })
  
  output$me_select_date <- renderUI({
    dateRangeInput("me_select_date",
                   label = h4("Select Period:"),
                   start = today() - 7*5,
                   end = today(),
                   width = '100%')
  })
  
  ####################################################################################
  #data, charts and tables
  ####################################################################################
  ########################################################################################################
  ## snapshot
  ########################################################################################################
  
  hyphenated_selected_metrics <- reactive({
    group_hyphenate(input$snap_select_metric)
  }
  )
  
  ss_period.dt <- reactive({
      portfolio %>%
        filter(date >= ymd(input$snap_select_date[[1]]),
               date <= ymd(input$snap_select_date[[2]]))
    }
  )
  
  ss_instrument.dt <- reactive({
      if(length(input$snap_select_instrument) == length(instrument_list)) {
        ss_period.dt()
      } else {
        ss_period.dt() %>%
          filter(instrument %in% input$snap_select_instrument)
      }
    }
  )
  
  output$marketplace_snapshot_download <- downloadHandler(
    filename = function() {
      paste0("snapshot_", today(), ".csv")
    },
    content = function(file) {
      write.csv((ss_instrument.dt() %>% dplyr::select(date, instrument, hyphenated_selected_metrics()))
                , file, row.names = F)
    }
  )
  
  ss_spark.dt <- reactive({
      req(input$snap_select_metric)
      validate(
        need(length(input$snap_select_metric) > 1, "Need 2 or more metrics selected")
      )
      
      ss_instrument.dt() %>%
        dplyr::select(date, instrument, hyphenated_selected_metrics()) %>% 
        group_by(instrument) %>%
        summarise_at(.vars = hyphenated_selected_metrics(),
                     .funs = c(Last = last, DoD = dod_change, Trend = paste2)) %>% #the name of funs here (eg Last, DoD) have to be the same as subheader_columns because each metric will have the colname changed to metric_Last/metric_DoD/metric_Trend.
        dplyr::select(instrument, unlist(lapply(hyphenated_selected_metrics(), function(pattern) {str_subset(names(.), paste0("^", pattern))} ))) #lapply function sorts the columns by order of appearance in the input$snap_select_metric
    })
  
  output$snap_table <- 
    DT::renderDataTable({
      validate(
        need(!is.null(hyphenated_selected_metrics()), "Loading..")
      )
      
      DT2(data = ss_spark.dt(),
          first_col = "Instrument",
          caption = paste("'Last' subcolumn: date beginning", max(portfolio$date)),
          subheader_columns = c("Last", "DoD", "Trend"),
          reverse_color_columns = c(),
          two_digits_columns = two_digits_metrics,
          full_number_columns = full_number_metrics,
          dollar_columns = dollar_metrics,
          percentage_columns = percentage_metrics,
          spark = T,
          spark_index = 3,
          selected_metrics = hyphenated_selected_metrics())
    }, server = FALSE
    )
  
  ################################################################################################
  ####### Metric Explorer
  ################################################################################################
  
  hyphenated_selected_me_metric <- reactive({
      group_hyphenate(input$me_select_metric)
    }
  )
  
  #-----------------
  #chart
  #-----------------
  
  formula_filtered <- reactive({
      formula_rds %>% 
        filter(metric == hyphenated_selected_me_metric())
    }
  ) 
  
  m_expl_metric_instrument_for_chart.dt <- reactive({
      if(hyphenated_selected_me_metric() %in% ratio_metrics) {
        portfolio %>%
          filter(instrument %in% input$me_select_instrument) 
      } else {
        portfolio %>%
          filter(instrument %in% input$me_select_instrument) %>%
          select(date, instrument, hyphenated_selected_me_metric()) %>%
          rename_(metric = names(.)[[3]])
      }
    })
  
  me_tooltip_unit <- reactive({ #<b>{point.y:', me_tooltip_unit, "</b> <br>"
    hc_unit(hyphenated_selected_me_metric()
            , percentage_unit = ",.1f}%"
            , dollar_unit = ",.2f} USD"
            , full_number_unit = ",.0f}"
            , two_digits_unit = ",.2f}")
  })
  
  me_yaxis_unit <- reactive({ #<b>{point.y:', me_tooltip_unit, "</b> <br>"
    hc_unit(hyphenated_selected_me_metric()
            , percentage_unit = ",.0f}%"
            , dollar_unit = ",.2f} USD"
            , full_number_unit = ",.0f}"
            , two_digits_unit = ",.2f}")
  })
  
  output$chart_maker <- 
    renderHighchart({
      validate(
        need(!is.null(hyphenated_selected_me_metric()), "Loading..")
      )
      #Non-ratio metrics:
      #", str_c(lapply(names(portfolio)[!(names(portfolio) %in% append(ratio_metrics, c("date", "instrument")))], function(metric) {dehyphenate(metric)}), collapse = ", ")))
      #        )
      if(hyphenated_selected_me_metric() %in% ratio_metrics) {
        #formula_filtered <- formula %>% filter(metric == hyphenated_selected_me_metric())
        
        data <- m_expl_metric_instrument_for_chart.dt() %>%
          group_by(date, instrument) %>%
          summarise(!!!(
            formula_filtered()$constituents %>%
              unlist() %>% # output example: c("completed_trips", "request")
              lapply(function(constituent) {parse_quosure(glue("sum({constituent}, na.rm = T)"))}) %>% #output example: sum(completed_trips, na.rm =T)
              setNames(formula_filtered()$constituents %>% unlist()) #creates names for the summarised metrics, eg completed_trips, request
          ))
        
        if(hyphenated_selected_me_metric() %in% percentage_metrics) {
          data <- data %>%
            mutate(metric = !!!(parse_quosure(paste(formula_filtered()$formula, "* 100"))))
        } else {
          data <- data %>%
            mutate(metric = !!!(parse_quosure(formula_filtered()$formula)))
        }
        
        
      } else {
        data <- m_expl_metric_instrument_for_chart.dt() %>% 
          group_by(date, instrument) %>%
          summarise(metric = sum(metric, na.rm = T))
      }
      
      highchart() %>%
        hc_chart(zoomType = "x") %>%
        hc_title(text = dehyphenate(hyphenated_selected_me_metric())) %>%
        hc_add_series(data, "line", hcaes(x = date, y = metric, group = instrument)) %>%
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
        hc_tooltip(crosshairs = T
                   , shared = T
                   , pointFormat = paste0('<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y:', me_tooltip_unit(), "</b> <br>")
        ) %>%
        hc_subtitle(text = list('Highlight along the x-axis to zoom; Click Legends to enable/disable')) %>%
        hc_yAxis(title = list(text = dehyphenate(hyphenated_selected_me_metric())),
                 labels = list(format = paste0("{value:", me_yaxis_unit())))
      
    })
  
  #-----------------
  #data table
  #-----------------
  
  m_expl_metric_period.dt <- reactive({
      if(hyphenated_selected_me_metric() %in% ratio_metrics) {
        portfolio %>%
          filter(instrument %in% input$me_select_instrument,
                 date >= ymd(input$me_select_date[[1]]),
                 date <= ymd(input$me_select_date[[2]])) 
      } else {
        portfolio %>%
          filter(instrument %in% input$me_select_instrument,
                 date >= ymd(input$me_select_date[[1]]),
                 date <= ymd(input$me_select_date[[2]])) %>%
          select(date, instrument, hyphenated_selected_me_metric()) #may not need this line.
      }
    })
  
  m_expl_mp_wide.dt <- reactive({
      m_expl_metric_period.dt() %>%
        dcast(instrument ~ date, value.var = hyphenated_selected_me_metric())#, fun.aggregate = mean) #fun.aggregate aggregates the values if there are multiple values in 
    }) #%>%
  #coalesce(., list(0L)) #remove all NAs
  
  # summarise all the ratio metrics using 'mean' instead of sum
    m_expl_instrument_total.dt <- reactive({
        if(hyphenated_selected_me_metric() %in% ratio_metrics) {
          #m_expl_metric_period.dt() %>%
          #  group_by(instrument) %>%
          #  summarise(Average = mean(!!sym(hyphenated_selected_me_metric()), na.rm = T)) #mean is a base function and doesn't understand character string as a variable name. input$me_select_metric will give a character string eg "rider_first_trip". So we have to turn the string to symbol (using sym function from rlang package) and then unquote the symbol
          
          m_expl_metric_period.dt() %>%
            group_by(instrument) %>%
            summarise(!!!(
              formula_filtered()$constituents %>%
                unlist() %>% # output example: c("completed_trips", "request")
                lapply(function(constituent) {parse_quosure(glue("sum({constituent}, na.rm = T)"))}) %>% #output example: sum(completed_trips, na.rm =T)
                setNames(formula_filtered()$constituents %>% unlist()) #creates names for the summarised metrics, eg completed_trips, request
            )) %>%
            mutate(Average = !!!(parse_quosure(formula_filtered()$formula))) %>%
            select(instrument, Average)
          
        } else {
          m_expl_metric_period.dt() %>%
            group_by(instrument) %>%
            summarise(Total = sum(!!sym(hyphenated_selected_me_metric()), na.rm = T))
        }
      })
    
    m_expl_wide_total.dt <- reactive({ #creates df with columns: instrument, Average, date1, date2, ...., date n.
      req(m_expl_mp_wide.dt())  
      
      if(hyphenated_selected_me_metric() %in% ratio_metrics) {
          m_expl_mp_wide.dt() %>%
            inner_join(m_expl_instrument_total.dt(), by = "instrument") %>%
            select(instrument, Average, everything())  
        } else {
          m_expl_mp_wide.dt() %>%
            inner_join(m_expl_instrument_total.dt(), by = "instrument") %>%
            select(instrument, Total, everything()) 
        }
      }) 
    
    m_expl_ratio_metrics_average_by_date <- reactive({ #creates df with columns: date, {constituent1}, {constituent2}, ...., Average, instrument (filled with "Average" values)
        if(hyphenated_selected_me_metric() %in% ratio_metrics) {
          m_expl_metric_period.dt() %>%
            filter(instrument %in% input$me_select_instrument) %>% #only calculate the dately average for selected cities
            group_by(date) %>%
            summarise(!!!(
              formula_filtered()$constituents %>%
                unlist() %>% # output example: c("completed_trips", "request")
                lapply(function(constituent) {parse_quosure(glue("sum({constituent}, na.rm = T)"))}) %>% #output example: sum(completed_trips, na.rm =T)
                setNames(formula_filtered()$constituents %>% unlist()) #creates names for the summarised metrics, eg completed_trips, request
            )) %>%
            mutate(Average = (!!!(parse_quosure(formula_filtered()$formula))),
                   instrument = "Average") 
        } else {
          NULL
        }
    })
    
    m_expl_ratio_metrics_average_final_value <- reactive({ #creates 1x1 df with columns: instrument, Average
        if(hyphenated_selected_me_metric() %in% ratio_metrics) {
          m_expl_ratio_metrics_average_by_date() %>%
            group_by(instrument) %>%
            summarise(!!!(
              formula_filtered()$constituents %>%
                unlist() %>% # output example: c("completed_trips", "request")
                lapply(function(constituent) {parse_quosure(glue("sum({constituent}, na.rm = T)"))}) %>% #output example: sum(completed_trips, na.rm =T)
                setNames(formula_filtered()$constituents %>% unlist()) #creates names for the summarised metrics, eg completed_trips, request
            )) %>%
            mutate(Average = !!!(parse_quosure(formula_filtered()$formula))) %>%
            dplyr::select(instrument, Average)
          
        } else {
          NULL
        }
      })
    
    m_expl_nat_total.dt <- reactive({
        if(hyphenated_selected_me_metric() %in% ratio_metrics) {
          m_expl_ratio_metrics_average_by_date() %>%
            dcast(instrument ~ date, value.var = "Average") %>%
            inner_join(m_expl_ratio_metrics_average_final_value(), by = "instrument")
          
          
        } else {
          m_expl_wide_total.dt() %>%
            melt(id.vars = c("instrument")) %>%
            group_by(variable) %>% #variable = date
            summarise(Total = sum(value, na.rm = T)) %>% #value = 
            spread(variable, Total) %>%
            mutate(instrument = "Total") %>%
            select(instrument, everything()) 
        }
      })
  
  m_expl_final.dt <- reactive({
      m_expl_wide_total.dt() %>%
        dplyr::union_all(m_expl_nat_total.dt()) %>%
        rename(Instrument = instrument)
    })
  
  output$metric_explorer_download <- downloadHandler(
    filename = #function() {
      paste0("metric_explorer_", lubridate::today(), ".csv")
    ,
    content = function(file) {
      write.csv(m_expl_final.dt(), file, row.names = F)
    }
  )
    
  output$me_table <- 
    DT::renderDataTable({
      validate(
        need(!is.null(hyphenated_selected_me_metric()), "Loading..")
      )
      
      dt <- datatable(data.table(m_expl_final.dt())
                , rownames = F
                , caption = paste0("Showing: ", dehyphenate(hyphenated_selected_me_metric()))
                , options = list(
                  pageLength = nrow(m_expl_final.dt())
                  , dom = "t"
                  , autoWidth = T
                  , scrollX = T
                ))
      
      if(hyphenated_selected_me_metric() %in% ratio_metrics) {
        summary_type <- "Average"
      } else {
        summary_type <- "Total"
      }  
      
      dt <- dt %>%
            formatStyle(
              "Instrument"
              , target = 'row'
              , backgroundColor = styleEqual(summary_type, 'orange')
            ) %>%
            formatStyle(
              summary_type
              , backgroundColor = 'orange'
            )
      
      if(hyphenated_selected_me_metric() %in% percentage_metrics) {
        dt <- dt %>%
          formatPercentage(
            2:length(m_expl_final.dt())
            , digits = 1
          )
      } else if(hyphenated_selected_me_metric() %in% dollar_metrics) {
        dt <- dt %>%
          formatCurrency(
            2:length(m_expl_final.dt())
            , mark = ","
            , currency = "$"
            , digits = 2
          )
      } else if(hyphenated_selected_me_metric() %in% full_number_metrics) {
        dt <- dt %>%
          formatCurrency(
            2:length(m_expl_final.dt())
            , mark = ","
            , currency = ""
            , digits = 0
          )
      } else {
        dt <- dt %>%
          formatCurrency(
            2:length(m_expl_final.dt())
            , mark = ","
            , currency = ""
            , digits = 2
          )
      }
      
      return(dt)
    }, server = FALSE)
  
})