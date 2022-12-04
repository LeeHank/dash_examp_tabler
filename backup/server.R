server = function(input, output) {
  
  # 認證
  res_auth <- secure_server(
    check_credentials = my_check
  )
  
  # 動態頁面呈現
  
  observe({
    
    empno = reactiveValuesToList(res_auth)$empno
    if(!is.null(empno)){
      
      if(empno %in% paste0(c("NA","NC","ND","NE","NV"), "_HEAD")){
        
        user_bg = str_sub(empno, 1, 2)
        remove_tabs = setdiff(c("Overview","NA","NC","ND","NE","NV"), user_bg)
        purrr::map(remove_tabs, ~removeTab(inputId = "tabs", target = .x))
        
      }else if(empno %in% c(empno1, # Hank
                            empno2
      )){
        # removeTab(inputId = "tabs", target = "NA")
      }else{
        purrr::map(c("Overview","NA","NC","ND","NE","NV"),
                   ~removeTab(inputId = "tabs", target = .x))
      }
      
    }
  })
  
  
  
  
  # 認證
  # login_res = eventReactive(input$login_button, {
  #         
  #         r <- POST("api_url_here",
  #                   body = list(EmpNo = input$user_name,
  #                               EmpPwd = input$password),
  #                   encode = "json")
  #         
  #         if(r$status == 200){
  #                 validation = content(r)
  #                 print(validation)
  #                 if (validation) removeUI(selector = "#login_page")
  #         }else{
  #                 validation = FALSE
  #                 
  #         }
  #         
  #         list(validation = validation, user_name = input$user_name, api_status = r$status)
  #         
  # })
  # 
  # 
  # output$help = renderUI({
  #         
  #         if(!is.null(login_res())){
  #                 if(login_res()$api_status == 200){
  #                         if(!login_res()$validation){
  #                                 p("工號 或 密碼 輸入錯誤", style = "color:red; text-align:center")
  #                         }
  #                 }else{
  #                         
  #                         p("API 認證功能出錯，status code = ", login_res()$api_status)
  #                         
  #                 }
  #                 
  #         }
  #         
  # })
  
  
  output$report_date = renderUI({
    div(
      tags$em(tags$b("2021-08"))
    )
  })
  
  data_with_crit = reactive({
    temp = crit_func(data_current_proc$data_clean, av_criteria)
    temp$crit_temp %>%
      mutate(CEO = ifelse(CEO, "warning", "pass"))
  })
  
  # 0. 製作所有 info card 的 UI ----
  info_card_results = reactive({
    temp = purrr::pmap(
      card_param,
      tabler_card
    )
    names(temp) = card_param$plot_id
    temp
  })
  # 0. 製作 info card 的圖的 function ----
  cards_hist_plot = function(data_hist_summary, BG, BU){
    
    p = bgbu_hist(data_hist_summary, BG, BU) %>%
      mutate(base_date = ym(base_date),
             text = paste(
               base_date, "\n",
               paste0(round(100*AV_PREC,2), "%")
             )) %>%
      ggplot(aes(x = base_date, y = AV_PREC, text = text, group = 1)) + 
      geom_line(color = "blue") +
      geom_point(color = "blue", size = 0.5)+
      geom_area(fill = "blue", alpha = 0.3) +
      scale_x_date(expand = c(0, 0)) +
      scale_y_continuous(limits = c(0,0.8), expand = c(0, 0)) +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            plot.margin = margin(0, 0, 0, 0, "cm"),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid=element_blank(),
            plot.background=element_blank())
    
    pp = ggplotly(p, tooltip="text") %>% 
      layout(margin = list(l = 0, r=0, b=0, t=0),
             xaxis = list(automargin = FALSE),
             yaxis = list(automargin = FALSE)) %>%
      config(displayModeBar = F)
    
    renderPlotly(pp)
  }
  # 0. 製作各 BG 下的 BU compare boxplot ----
  bu_compare_boxes = reactive({
    bgs = c("NA","NC","ND","NE","NV")
    tt = purrr::map(
      bgs,
      ~bu_compare_boxplot(data_current_proc$data_clean, .x)
    )
    names(tt) = bgs
    tt
  })
  # 0. 製作各 BG 下的 BU top 3 plot ----
  bg_top3_plot = reactive({
    bgs = c("NA","NC","ND","NE","NV")
    tt = purrr::map(
      bgs,
      ~bg_highest_lowest_plot(data_current_proc$data_clean, .x)
    )
    names(tt) = bgs
    tt
  })
  # 0. column_def
  column_def = list(
    BUSINESS_MODEL = colDef(minWidth = 150),
    PRODUCT_TYPE = colDef(minWidth = 150),
    F_PRODUCT = colDef(minWidth = 150),
    F_MODEL = colDef(minWidth = 150),
    PDD_DESCRIPTION = colDef(minWidth = 200),
    AV_PERCENT = colDef(format = colFormat(percent = TRUE, digits = 1),
                        minWidth = 120),
    STATUS = colDef(cell = function(value) {
      class <- paste0("tag status-", tolower(value))
      div(class = class, value)
    }),
    CRITERIA = colDef(html = TRUE, minWidth = 120),
    BOOK_SALES = colDef(format = colFormat(prefix = "NT$", separators = TRUE, digits = 2),
                        minWidth = 180),
    BOOK_RM = colDef(format = colFormat(prefix = "NT$", separators = TRUE, digits = 2),
                     minWidth = 180),
    BOOK_OH = colDef(format = colFormat(prefix = "NT$", separators = TRUE, digits = 2),
                     minWidth = 180),
    BOOK_OSP = colDef(format = colFormat(prefix = "NT$", separators = TRUE, digits = 2),
                      minWidth = 180),
    BOOK_DL = colDef(format = colFormat(prefix = "NT$", separators = TRUE, digits = 2),
                     minWidth = 180),
    REBATE_NTD = colDef(format = colFormat(prefix = "NT$", separators = TRUE, digits = 2),
                        minWidth = 150),
    ROYALTY_NTD = colDef(format = colFormat(prefix = "NT$", separators = TRUE, digits = 2),
                         minWidth = 180),
    KICK_OFF_DATE = colDef(minWidth = 200)
  )
  
  # 1. CEO 頁面 ----
  ## 1.1 Info card ----
  output$ceo_card_panel = renderUI({
    fluidRow(
      column(
        width = 2,
        info_card_results()[["CEO_ALL_ALL"]]
      ),
      column(
        width = 2,
        info_card_results()[["CEO_NA_ALL"]]
      ),
      column(
        width = 2,
        info_card_results()[["CEO_NC_ALL"]]
      ),
      column(
        width = 2,
        info_card_results()[["CEO_ND_ALL"]]
      ),
      column(
        width = 2,
        info_card_results()[["CEO_NE_ALL"]]
      ),
      column(
        width = 2,
        info_card_results()[["CEO_NV_ALL"]]
      )
    )
  })
  ## 1.2 info card 的圖 ----
  output$CEO_ALL_ALL = cards_hist_plot(data_hist_summary, BG = "ALL", BU = "ALL")
  output$CEO_NA_ALL = cards_hist_plot(data_hist_summary, BG = "NA", BU = "ALL")
  output$CEO_NC_ALL = cards_hist_plot(data_hist_summary, BG = "NC", BU = "ALL")
  output$CEO_ND_ALL = cards_hist_plot(data_hist_summary, BG = "ND", BU = "ALL")
  output$CEO_NE_ALL = cards_hist_plot(data_hist_summary, BG = "NE", BU = "ALL")
  output$CEO_NV_ALL = cards_hist_plot(data_hist_summary, BG = "NV", BU = "ALL")
  
  # output$ceo_card_panel = renderUI({
  #         
  #         bg_func = function(BG){
  #                 if(BG == "ALL"){
  #                         data = data_hist_summary$total
  #                         num_models = data_current_proc$data_clean %>%
  #                                 nrow()
  #                 }else{
  #                         data = data_hist_summary$bg %>%
  #                                 filter(F_BG == BG)
  #                         num_models = data_current_proc$data_clean %>%
  #                                 filter(F_BG == BG) %>%
  #                                 nrow()
  #                 }
  #                 
  #                 data %>%
  #                         arrange(base_date) %>%
  #                         mutate(diff = round(100*(AV_PREC-lag(AV_PREC)))) %>%
  #                         summarise(card_title = paste0(round(100*last(AV_PREC)), "%"),
  #                                   card_subtitle = BG,
  #                                   trend = ifelse(last(diff)>=0, "increase", "decrease"),
  #                                   diff_value = paste0(last(diff), "%")) %>%
  #                         ungroup() %>%
  #                         mutate(num_models = num_models,
  #                                plot_id = paste0("ceo_card_",BG))
  #         }
  #         ceo_cats = c("ALL","NA","NC","ND","NE","NV")
  #         bg_res = purrr::map(ceo_cats, bg_func) %>%
  #                 bind_rows()
  #         
  #         cards_res = purrr::pmap(bg_res, tabler_card)
  #         names(cards_res) = ceo_cats
  #         
  #         fluidRow(
  #                 column(
  #                         width = 2,
  #                         cards_res[["ALL"]]
  #                 ),
  #                 column(
  #                         width = 2,
  #                         cards_res[["NA"]]
  #                         # tabler_card(
  #                         #         card_title = "15%", 
  #                         #         card_subtitle = "NA", 
  #                         #         trend = "decrease", 
  #                         #         diff_value = "33%",
  #                         #         plot_id = "ceo_card_NA"
  #                         # )
  #                 ),
  #                 column(
  #                         width = 2,
  #                         cards_res[["NC"]]
  #                 ),
  #                 column(
  #                         width = 2,
  #                         cards_res[["ND"]]
  #                 ),
  #                 column(
  #                         width = 2,
  #                         cards_res[["NE"]]
  #                 ),
  #                 column(
  #                         width = 2,
  #                         cards_res[["NV"]]
  #                 )
  #         )
  # })
  # 
  # 
  # ceo_cards = function(BG){
  #         if(BG == "ALL"){
  #                 data = data_hist_summary$total
  #         }else{
  #                 data = data_hist_summary$bg %>%
  #                         filter(F_BG == BG)
  #         }
  #         p = data %>%
  #                 mutate(base_date = ym(base_date),
  #                        text = paste(
  #                                base_date, "\n",
  #                                paste0(round(100*AV_PREC,2), "%")
  #                        )) %>%
  #                 ggplot(aes(x = base_date, y = AV_PREC, text = text, group = 1)) + 
  #                 geom_line(color = "blue") +
  #                 geom_point(color = "blue", size = 0.5)+
  #                 geom_area(fill = "blue", alpha = 0.3) +
  #                 scale_x_date(expand = c(0, 0)) +
  #                 scale_y_continuous(limits = c(0,0.8), expand = c(0, 0)) +
  #                 theme(axis.line=element_blank(),
  #                       axis.text.x=element_blank(),
  #                       axis.text.y=element_blank(),
  #                       axis.ticks=element_blank(),
  #                       axis.title.x=element_blank(),
  #                       axis.title.y=element_blank(),
  #                       legend.position="none",
  #                       plot.margin = margin(0, 0, 0, 0, "cm"),
  #                       panel.background=element_blank(),
  #                       panel.border=element_blank(),
  #                       panel.grid=element_blank(),
  #                       plot.background=element_blank())
  #         pp = ggplotly(p, tooltip="text") %>% 
  #                 layout(margin = list(l = 0, r=0, b=0, t=0),
  #                        xaxis = list(automargin = FALSE),
  #                        yaxis = list(automargin = FALSE)) %>%
  #                 config(displayModeBar = F)
  #         
  #         renderPlotly(pp)
  # }
  # 
  # 
  # output$ceo_card_ALL = ceo_cards("ALL")
  # output$ceo_card_NA = ceo_cards("NA")
  # output$ceo_card_NC = ceo_cards("NC")
  # output$ceo_card_ND = ceo_cards("ND")
  # output$ceo_card_NE = ceo_cards("NE")
  # output$ceo_card_NV = ceo_cards("NV")
  
  # 1.3 CEO_compare_boxplot ----
  output$bg_compare_boxplot = renderPlotly({
    p = data_current_proc$data_clean %>%
      group_by(F_BG) %>%
      mutate(n = n(), cat = paste0(F_BG, "\n(n=", n, ")")) %>%
      ungroup() %>%
      ggplot(aes(x = cat, y = AV_PREC)) +
      geom_boxplot() +
      scale_y_continuous(labels = scales::percent)+ #, limits=c(0,1)
      theme_minimal() +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            plot.margin = margin(0, 0, 0, 0, "cm"),
            panel.background=element_blank(),
            panel.border=element_blank(),
            plot.background=element_blank())
    ggplotly(p) %>%
      config(displayModeBar = F) %>%
      layout(yaxis = list(hoverformat = ",.0%"))
    
  })
  # 1.4 CEO top 3 plot ----
  output$ALL_highest_lowest = renderPlotly({
    p = bind_rows(
      data_current_proc$data_clean %>%
        group_by(F_BG) %>%
        slice_max(n = 3, order_by = AV_PREC) %>%
        ungroup() %>%
        mutate(role = "max3"),
      data_current_proc$data_clean %>%
        group_by(F_BG) %>%
        slice_min(n = 3, order_by = AV_PREC) %>%
        ungroup() %>%
        mutate(role = "min3")
    ) %>%
      arrange(F_BG, AV_PREC) %>%
      mutate(F_MODEL = factor(F_MODEL, levels = F_MODEL),
             text = paste0(
               "MODEL: ", F_MODEL, "\n",
               "AV(%): ", round(100*AV_PREC,2), "%", "\n",
               "F_PRODUCT: ", F_PRODUCT, "\n",
               "BUSINESS_MODEL: ", BUSINESS_MODEL, "\n",
               "DESCRIPTION: ", ifelse(is.na(PDD_DESCRIPTION), "None", PDD_DESCRIPTION)
             )) %>%
      ggplot(aes(x = F_MODEL, y = AV_PREC, fill = role, text = text)) +
      geom_bar(stat = "identity") +
      facet_wrap(~F_BG, nrow = 1, scale = "free_x") +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.margin = margin(0, 0, 0, 0, "cm"),
            legend.position = "none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            plot.background=element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      # layout(
      #         legend = list(orientation = "h", x = -0.2, y = -0.1)
      # ) %>%
      config(displayModeBar = F)
  })
  
  
  # CEO tab boxplot ----
  output$CEO_biz_module = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "Module")
  )
  output$CEO_biz_component = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "Component")
  )
  output$CEO_biz_system = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "System")
  )
  
  # ceo_data ----
  all_data_display = reactive({
    data_with_crit() %>%
      select(F_BG, F_BU,
             BUSINESS_MODEL, PRODUCT_TYPE, F_PRODUCT,
             F_MODEL, 
             AV_PREC, CEO, CEO_AV_CRIT,
             BOOK_SALES:ROYALTY_NTD, KICK_OFF_DATE,PDD_DESCRIPTION) %>%
      rename(AV_PERCENT = AV_PREC) %>%
      mutate_if(is.numeric, ~round(.x, 2)) %>%
      mutate(KICK_OFF_DATE = ymd_hms(KICK_OFF_DATE, tz = "Asia/Taipei") %>% date()) %>%
      mutate(KICK_OFF_DATE = as.character(KICK_OFF_DATE)) %>%
      mutate(CEO_AV_CRIT = paste0("&leq;", 100*CEO_AV_CRIT,"%")) %>%
      arrange(desc(CEO), 
              F_BG, F_BU, BUSINESS_MODEL, PRODUCT_TYPE, F_PRODUCT,
              AV_PERCENT) %>%
      rename(STATUS = CEO, 
             CRITERIA = CEO_AV_CRIT)
  })
  num_vars = reactive(
    all_data_display() %>%
      summarise_all(is.numeric) %>%
      pivot_longer(everything()) %>%
      filter(value) %>%
      pull(name)
  )
  
  output$ceo_data = renderReactable({
    
    # decision = function(x){
    #         temp = (all_data_display()[[x]] >= input[[x]][1]) &
    #                 (all_data_display()[[x]] <= input[[x]][2])
    #         temp[which(is.na(temp))] = TRUE
    #         temp
    # }
    # aa = purrr::map(num_vars(), decision)
    # fi = reduce(aa, ~ .x & .y)
    # filtered_data = all_data_display()[fi,]
    filtered_data = all_data_display()
    
    reactable(filtered_data,
              filterable = TRUE,
              # pagination = FALSE,
              highlight = TRUE,
              height = "550px",
              columns = column_def
    )
  })
  output$NA_data = renderReactable({
    
    filtered_data = all_data_display() %>%
      filter(F_BG == "NA")
    
    reactable(filtered_data,
              filterable = TRUE,
              # pagination = FALSE,
              highlight = TRUE,
              height = "550px",
              columns = column_def
    )
  })
  output$NC_data = renderReactable({
    
    filtered_data = all_data_display() %>%
      filter(F_BG == "NC")
    
    reactable(filtered_data,
              filterable = TRUE,
              # pagination = FALSE,
              highlight = TRUE,
              height = "550px",
              columns = column_def
    )
  })
  output$ND_data = renderReactable({
    
    filtered_data = all_data_display() %>%
      filter(F_BG == "ND")
    
    reactable(filtered_data,
              filterable = TRUE,
              # pagination = FALSE,
              highlight = TRUE,
              height = "550px",
              columns = column_def
    )
  })
  output$NE_data = renderReactable({
    
    filtered_data = all_data_display() %>%
      filter(F_BG == "NE")
    
    reactable(filtered_data,
              filterable = TRUE,
              # pagination = FALSE,
              highlight = TRUE,
              height = "550px",
              columns = column_def
    )
  })
  output$NV_data = renderReactable({
    
    filtered_data = all_data_display() %>%
      filter(F_BG == "NV")
    
    reactable(filtered_data,
              filterable = TRUE,
              # pagination = FALSE,
              highlight = TRUE,
              height = "550px",
              columns = column_def
    )
  })
  
  
  # output$ceo_data_filter = renderUI({
  #         
  #         meta = all_data_display() %>%
  #                 select(num_vars()) %>%
  #                 pivot_longer(everything()) %>%
  #                 group_by(name) %>%
  #                 summarise(min = min(value, na.rm = TRUE),
  #                           max = max(value, na.rm = TRUE)) %>%
  #                 ungroup() %>%
  #                 group_by(name) %>%
  #                 mutate(value = list(c(min, max))) %>%
  #                 ungroup() %>%
  #                 rename(inputId = name) %>%
  #                 mutate(label = inputId, width = "100%") %>%
  #                 select(inputId, label, min, max, value, width)
  #         elements = purrr::pmap(meta, sliderInput)
  #         names(elements) = meta$inputId
  #         div(
  #                 style = "padding: 0rem 2rem;",
  #                 fluidRow(
  #                         column(
  #                                 width = 3,
  #                                 elements[[1]]
  #                         ),
  #                         column(
  #                                 width = 3,
  #                                 elements[[2]]
  #                         ),
  #                         column(
  #                                 width = 3,
  #                                 elements[[3]]
  #                         ),
  #                         column(
  #                                 width = 3,
  #                                 elements[[4]]
  #                         )
  #                 ),
  #                 fluidRow(
  #                         column(
  #                                 width = 3,
  #                                 elements[[5]]
  #                         ),
  #                         column(
  #                                 width = 3,
  #                                 elements[[6]]
  #                         ),
  #                         column(
  #                                 width = 3,
  #                                 elements[[7]]
  #                         ),
  #                         column(
  #                                 width = 3,
  #                                 elements[[8]]
  #                         )
  #                 )
  #         )
  #         
  # })
  
  # 2. NA 頁面 ----
  # 2.1 NA: info card ----
  output$NA_card_panel = renderUI({
    fluidRow(
      column(
        width = 2,
        info_card_results()[["NA_NA_ALL"]]
      ),
      column(
        width = 2,
        info_card_results()[["NA_NA_AIC"]]
      ),
      column(
        width = 2,
        info_card_results()[["NA_NA_ICS"]]
      ),
      column(
        width = 2,
        info_card_results()[["NA_NA_SMA"]]
      )
    )
  })
  ## 2.2 NA: info card 的圖
  output$NA_NA_ALL = cards_hist_plot(data_hist_summary, BG = "NA", BU = "ALL")
  output$NA_NA_AIC = cards_hist_plot(data_hist_summary, BG = "NA", BU = "AIC")
  output$NA_NA_ICS = cards_hist_plot(data_hist_summary, BG = "NA", BU = "ICS")
  output$NA_NA_SMA = cards_hist_plot(data_hist_summary, BG = "NA", BU = "SMA")
  
  # 2.3 NA_compare_boxplot ----
  output$NA_compare_boxplot = renderPlotly(bu_compare_boxes()[["NA"]])
  
  # 2.4 NA top 3 plot ----
  output$NA_highest_lowest = renderPlotly({bg_top3_plot()[["NA"]]})
  
  # 2.5 NA tab boxplot ----
  output$NA_biz_module = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "Module", "NA")
  )
  output$NA_biz_system = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "System", "NA")
  )
  
  # 3.1 NC: info card ----
  output$NC_card_panel = renderUI({
    fluidRow(
      column(
        width = 2,
        info_card_results()[["NC_NC_ALL"]]
      ),
      column(
        width = 2,
        info_card_results()[["NC_NC_CH1"]]
      ),
      column(
        width = 2,
        info_card_results()[["NC_NC_CH2"]]
      ),
      column(
        width = 2,
        info_card_results()[["NC_NC_CH3"]]
      )
    )
  })
  ## 2.2 NC: info card 的圖
  output$NC_NC_ALL = cards_hist_plot(data_hist_summary, BG = "NC", BU = "ALL")
  output$NC_NC_CH1 = cards_hist_plot(data_hist_summary, BG = "NC", BU = "CH1")
  output$NC_NC_CH2 = cards_hist_plot(data_hist_summary, BG = "NC", BU = "CH2")
  output$NC_NC_CH3 = cards_hist_plot(data_hist_summary, BG = "NC", BU = "CH3")
  
  # 3.3 NC_compare_boxplot ----
  output$NC_compare_boxplot = renderPlotly(bu_compare_boxes()[["NC"]])
  
  # 3.4 NC top 3 plot ----
  output$NC_highest_lowest = renderPlotly({bg_top3_plot()[["NC"]]})
  
  # 3.5 NC tab boxplot ----
  output$NC_biz_module = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "Module", "NC")
  )
  output$NC_biz_system = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "System", "NC")
  )
  
  # 4. ND 頁面 ----
  # 4.1 ND: info card ----
  output$ND_card_panel = renderUI({
    fluidRow(
      column(
        width = 2,
        info_card_results()[["ND_ND_ALL"]]
      ),
      column(
        width = 2,
        info_card_results()[["ND_ND_NW1"]]
      ),
      column(
        width = 2,
        info_card_results()[["ND_ND_NW2"]]
      ),
      column(
        width = 2,
        info_card_results()[["ND_ND_NW3"]]
      ),
      column(
        width = 2,
        info_card_results()[["ND_ND_WIF"]]
      )
    )
  })
  ## 4.2 ND: info card 的圖
  output$ND_ND_ALL = cards_hist_plot(data_hist_summary, BG = "ND", BU = "ALL")
  output$ND_ND_NW1 = cards_hist_plot(data_hist_summary, BG = "ND", BU = "NW1")
  output$ND_ND_NW2 = cards_hist_plot(data_hist_summary, BG = "ND", BU = "NW2")
  output$ND_ND_NW3 = cards_hist_plot(data_hist_summary, BG = "ND", BU = "NW3")
  output$ND_ND_WIF = cards_hist_plot(data_hist_summary, BG = "ND", BU = "WIF")
  
  # 4.3 ND_compare_boxplot ----
  output$ND_compare_boxplot = renderPlotly(bu_compare_boxes()[["ND"]])
  
  # 4.4 ND top 3 plot ----
  output$ND_highest_lowest = renderPlotly({bg_top3_plot()[["ND"]]})
  
  # 4.5 NC tab boxplot ----
  output$ND_biz_module = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "Module", "ND")
  )
  output$ND_biz_system = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "System", "ND")
  )
  
  
  # 5.1 NE: info card ----
  output$NE_card_panel = renderUI({
    fluidRow(
      column(
        width = 2,
        info_card_results()[["NE_NE_ALL"]]
      )
    )
  })
  ## 5.2 NE: info card 的圖
  output$NE_NE_ALL = cards_hist_plot(data_hist_summary, BG = "ND", BU = "ALL")
  
  # 5.3 NE_compare_boxplot ----
  output$NE_compare_boxplot = renderPlotly({
    bu_compare_boxes()[["NE"]]
  })
  
  # 5.4 NE top 3 plot ----
  output$NE_highest_lowest = renderPlotly({bg_top3_plot()[["NE"]]})
  
  # 5.5 NE tab boxplot ----
  output$NE_biz_module = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "Module", "NE")
  )
  output$NE_biz_component = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "Component", "NE")
  )
  
  # 6.1 NV: info card ----
  output$NV_card_panel = renderUI({
    fluidRow(
      column(
        width = 2,
        info_card_results()[["NV_NV_ALL"]]
      )
    )
  })
  ## 6.2 NV: info card 的圖
  output$NV_NV_ALL = cards_hist_plot(data_hist_summary, BG = "NV", BU = "ALL")
  
  # 6.3 NV_compare_boxplot ----
  output$NV_compare_boxplot = renderPlotly({
    bu_compare_boxes()[["NV"]]
  })
  
  # 6.4 NV top 3 plot ----
  output$NV_highest_lowest = renderPlotly({bg_top3_plot()[["NV"]]})
  
  # 6.5 NV tab boxplot ----
  output$NV_biz_module = renderPlotly(
    biz_prod_boxplot(data_current_proc$data_clean, 
                     av_criteria,
                     "Module", "NV")
  )
  
  # olds
  # All boxplot ----
  # gen_boxplot = function(BG){
  #         renderPlot({
  #                 plot_bg(data_current_proc$data_clean,
  #                         av_criteria_for_plot,BG)
  #         })
  # }
  # 
  # output$ALL_NA_boxplot <- gen_boxplot("NA")
  # output$ALL_NC_boxplot <- gen_boxplot("NC")
  # output$ALL_ND_boxplot <- gen_boxplot("ND")
  # output$ALL_NE_boxplot <- gen_boxplot("NE")
  # output$ALL_NV_boxplot <- gen_boxplot("NV")
  # 
  # 
  # # All trend ----
  # output$ALL_trend <- renderPlotly({
  #         all_d = data_hist_summary$total %>%
  #                 select(base_date, AV_PREC) %>%
  #                 mutate(base_date = ym(base_date))
  #         
  #         bg_d = data_hist_summary$bg %>%
  #                 select(base_date, F_BG, AV_PREC) %>%
  #                 mutate(base_date = ym(base_date),
  #                        label = if_else(base_date == max(base_date),
  #                                        as.character(F_BG),
  #                                        NA_character_))
  #         
  #         p = ggplot() +
  #                 geom_line(aes(x = base_date, y = AV_PREC),
  #                           data = all_d, col = "red", lwd = 2) +
  #                 geom_line(aes(x = base_date, y = AV_PREC, col = F_BG),
  #                           data = bg_d) +
  #                 geom_label_repel(aes(x = base_date, y = AV_PREC, col = F_BG, label = label),
  #                                  data = bg_d,
  #                                  nudge_x = 1,
  #                                  na.rm = TRUE) +
  #                 theme_bw() +
  #                 theme(legend.position = "none") +
  #                 scale_y_continuous(labels = scales::percent, limits=c(0,0.8)) +
  #                 xlab("Date") +
  #                 ylab("AV(%)")# +
  #         #ggtitle("整體(紅線) vs 各 BG 之 AV(%) 趨勢圖")
  #         
  #         ggplotly(p)
  # })
  
  
}