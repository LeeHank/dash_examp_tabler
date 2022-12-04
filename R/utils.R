# sql & 整理資料相關 ----
query_string = function(date = "202105"){
  "sql string here"
}

get_data = function(date){
  dbGetQuery(db_oracle_finbidb, query_string(date)) %>%
    mutate(base_date = date) %>%
    filter(DEVELOPMENT_TYPE !="RD") # 濾掉 RD
}

sum_func = function(group_data){
  group_data %>%
    summarise(across(c(BOOK_SALES:ROYALTY_NTD), ~sum(.x, na.rm = TRUE)), n = n()) %>%
    ungroup() %>%
    mutate(AV_PREC = (BOOK_SALES-BOOK_RM-ROYALTY_NTD+REBATE_NTD)/BOOK_SALES)
}

get_data_summary = function(data_hist){
  
  total = data_hist %>%
    group_by(base_date) %>%
    sum_func()
  
  bg = data_hist %>%
    group_by(base_date, F_BG) %>%
    sum_func()
  
  bu = data_hist %>%
    group_by(base_date, F_BG, F_BU) %>%
    sum_func()
  
  list(total = total,
       bg = bg,
       bu = bu)
}

preproc_func = function(raw_data){
  
  data_temp = raw_data %>%
    filter(DEVELOPMENT_TYPE !="RD") %>%
    # 移除 duplicate
    # distinct(F_BG, F_BU, F_PRODUCT, F_MODEL, DEVELOPMENT_TYPE, PRODUCT_TYPE, AV_PREC) %>% 
    # 改 DEVELOPMENT_TYPE
    mutate(BUSINESS_MODEL = case_when(
      DEVELOPMENT_TYPE == "CMI" ~ "CM_All",
      DEVELOPMENT_TYPE == "ODMI" ~ "ODM",
      DEVELOPMENT_TYPE == "RD" ~ "RD",
      DEVELOPMENT_TYPE == "ODMII" ~ "JDM",
      DEVELOPMENT_TYPE == "CMII" ~ "CM_All"
    )) %>%
    # 把 AV_PREC 改單位
    mutate(no_na = !is.na(F_BG) & !is.na(F_BU) & !is.na(F_PRODUCT) &
             !is.na(BUSINESS_MODEL) & !is.na(PRODUCT_TYPE) & !is.na(F_MODEL) & 
             !is.na(AV_PREC))
  
  
  data_clean = data_temp %>% 
    filter(no_na) %>%
    select(-no_na)
  
  # data_clean = data_temp
  
  total_sum = data_clean %>% sum_func()
  
  bg_sum = data_clean %>%
    group_by(F_BG) %>%
    sum_func()
  
  bg_bu_sum = data_clean %>%
    group_by(F_BG, F_BU) %>%
    sum_func()
  
  list(data_temp = data_temp, 
       data_clean = data_clean,
       total_sum = total_sum,
       bg_sum = bg_sum,
       bg_bu_sum = bg_bu_sum)
  
}

crit_func = function(data_clean, av_criteria){
  crit_temp = data_clean %>%
    left_join(av_criteria) %>%
    group_by(F_BG, F_BU, BUSINESS_MODEL, PRODUCT_TYPE) %>%
    mutate(CEO = AV_PREC <= CEO_AV_CRIT,
           BG_HEAD = AV_PREC <= BG_HEAD_AV_CRIT,
           BU_HEAD = AV_PREC <= BU_HEAD_AV_CRIT) %>%
    ungroup()
  
  crit_detail = crit_temp %>%
    group_by(F_BG, F_BU, BUSINESS_MODEL, PRODUCT_TYPE) %>%
    summarise(筆數 = n(),
                CEO = sum(CEO),
                BG_HEAD = sum(BG_HEAD),
                BU_HEAD = sum(BU_HEAD)) %>%
    ungroup()
  
  crit_bg = crit_detail %>%
    group_by(F_BG) %>%
    summarise(筆數 = sum(筆數),
                CEO = sum(CEO),
                BG_HEAD = sum(BG_HEAD),
                BU_HEAD = sum(BU_HEAD)) %>%
    ungroup() %>%
    mutate(F_BG = paste0(F_BG, "_ALL"))
  
  crit_bu = crit_detail %>%
    group_by(F_BG, F_BU) %>%
    summarise(筆數 = sum(筆數),
                CEO = sum(CEO),
                BG_HEAD = sum(BG_HEAD),
                BU_HEAD = sum(BU_HEAD)) %>%
    ungroup() %>%
    mutate(F_BU = paste0(F_BU, "_ALL"))
  
  crit_ceo = crit_detail %>%
    summarise(筆數 = sum(筆數),
                CEO = sum(CEO),
                BG_HEAD = sum(BG_HEAD),
                BU_HEAD = sum(BU_HEAD)) %>%
    mutate(F_BG = "ALL")
  
  crit_final = bind_rows(
    crit_detail,
    crit_bg,
    crit_bu,
    crit_ceo
  ) %>%
    arrange(F_BG, F_BU, PRODUCT_TYPE, BUSINESS_MODEL)
  
  list(crit_final = crit_final, crit_temp = crit_temp)
}

# 網頁 UI 用 functions ----
navbarPageWithRightUI <- function(..., right_ui) {
  navbar <- navbarPage(
    ...
  )
  form <- tags$form(
    class = "form-inline my-2 my-lg-0", #
    right_ui
  )
  navbar[[4]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[4]][[1]]$children[[1]], form
  )
  navbar
}

# 算網頁要用的資料的 funcs ----

bgbu_hist = function(data_hist_summary, BG = "ALL", BU = "ALL"){
  if(BG=="ALL" & BU == "ALL"){
    data_hist_summary$total %>%
      arrange(base_date)
  }else if(BG != "ALL" & BU == "ALL"){
    data_hist_summary$bg %>%
      filter(F_BG == BG) %>%
      arrange(base_date)
  }else if(BG != "ALL" & BU != "ALL"){
    data_hist_summary$bu %>%
      filter(F_BG == BG, F_BU == BU) %>%
      arrange(base_date)
  }else{
    NULL
  }
}
info_card_gen_data = function(BG = "ALL", BU = "ALL", plot_id){
  if(BG == "ALL" & BU == "ALL"){
    card_subtitle = "ALL"
  }
  if(BG != "ALL" & BU == "ALL"){
    card_subtitle = BG
  }
  if(BG != "ALL" & BU != "ALL"){
    card_subtitle = BU
  }
  
  bgbu_hist(data_hist_summary, BG, BU) %>%
    arrange(base_date) %>%
    mutate(diff = round(100*(AV_PREC-lag(AV_PREC)))) %>%
    summarise(card_title = paste0(round(100*last(AV_PREC)), "%"),
              card_subtitle = card_subtitle,
              trend = ifelse(last(diff)>=0, "increase", "decrease"),
              diff_value = paste0(last(diff), "%"),
              num_models = last(n)) %>%
    ungroup() %>%
    mutate(plot_id = plot_id)
  
  # if(BG=="ALL" & BU == "ALL"){
  #         
  #         data_hist_summary$total %>%
  #                 arrange(base_date) %>%
  #                 mutate(diff = round(100*(AV_PREC-lag(AV_PREC)))) %>%
  #                 summarise(card_title = paste0(round(100*last(AV_PREC)), "%"),
  #                           card_subtitle = "ALL",
  #                           trend = ifelse(last(diff)>=0, "increase", "decrease"),
  #                           diff_value = paste0(last(diff), "%"),
  #                           num_models = last(n)) %>%
  #                 ungroup() %>%
  #                 mutate(plot_id = plot_id)
  # }else if(BG != "ALL" & BU == "ALL"){
  #         num_models = data_current %>%
  #                 filter(F_BG == BG) %>%
  #                 nrow()
  #         data_hist_summary$bg %>%
  #                 filter(F_BG == BG) %>%
  #                 arrange(base_date) %>%
  #                 mutate(diff = round(100*(AV_PREC-lag(AV_PREC)))) %>%
  #                 summarise(card_title = paste0(round(100*last(AV_PREC)), "%"),
  #                           card_subtitle = BG,
  #                           trend = ifelse(last(diff)>=0, "increase", "decrease"),
  #                           diff_value = paste0(last(diff), "%")) %>%
  #                 ungroup() %>%
  #                 mutate(num_models = num_models,
  #                        plot_id = plot_id)
  # }else if(BG != "ALL" & BU != "ALL"){
  #         num_models = data_current %>%
  #                 filter(F_BG == BG, F_BU == BU) %>%
  #                 nrow()
  #         data_hist_summary$bu %>%
  #                 filter(F_BG == BG, F_BU == BU) %>%
  #                 arrange(base_date) %>%
  #                 mutate(diff = round(100*(AV_PREC-lag(AV_PREC)))) %>%
  #                 summarise(card_title = paste0(round(100*last(AV_PREC)), "%"),
  #                           card_subtitle = BU,
  #                           trend = ifelse(last(diff)>=0, "increase", "decrease"),
  #                           diff_value = paste0(last(diff), "%")) %>%
  #                 ungroup() %>%
  #                 mutate(num_models = num_models,
  #                        plot_id = plot_id)
  # }else{
  #         
  # }
}

prc_ggplotly = function(pl = ggplot2::last_plot(), i = 1)
{
  ply = plotly::ggplotly(pl)
  
  if(any(grepl('%', ply$x$layout$yaxis$ticktext)))
  {
    tip = ply$x$data[[i]]$text
    tip = do.call('c', lapply(strsplit(tip, 'y:'), function(x) {paste0(x[1], 'y:',paste0(x[2], '%'))}  ))
    ply$x$data[[i]]$text = tip
  }
  
  if(any(grepl('%', ply$x$layout$xaxis$ticktext)))
  {
    tip = ply$x$data[[i]]$text
    tip = do.call('c', lapply(strsplit(tip, '<'), function(x) { paste0(paste0(x[1], '%'), '<', x[2])}  ))
    ply$x$data[[i]]$text = tip
  }
  
  return(ply)
}

bu_compare_boxplot = function(data, BG){
  p = data %>%
    filter(F_BG == BG) %>%
    group_by(F_BU) %>%
    mutate(n = n(), 
           cat = paste0(F_BU, "\n(n=", n, ")")) %>%
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
    config(displayModeBar = F)  %>%
    layout(yaxis = list(hoverformat = ",.0%"))
  
  
  
}

bg_highest_lowest_plot = function(data, BG){
  p = bind_rows(
    data %>%
      filter(F_BG == BG) %>%
      group_by(F_BU) %>%
      slice_max(n = 3, order_by = AV_PREC) %>%
      ungroup() %>%
      mutate(role = "max3"),
    data %>%
      filter(F_BG == BG) %>%
      group_by(F_BU) %>%
      slice_min(n = 3, order_by = AV_PREC) %>%
      ungroup() %>%
      mutate(role = "min3")
  ) %>%
    arrange(F_BU, AV_PREC) %>%
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
    facet_wrap(~F_BU, nrow = 1, scale = "free_x") +
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
}



biz_prod_boxplot = function(data, av_criteria, prod_type, BG = "ALL"){
  
  temp = data %>%
    filter(PRODUCT_TYPE == prod_type) %>%
    {if(BG == "ALL"){
      .
    }else{
      filter(., F_BG == BG)
    }}
  
  bu_levels = temp %>%
    count(F_BG, F_BU, BUSINESS_MODEL) %>%
    mutate(cat = paste0(F_BU, "\n(n=", n,")")) %>%
    select(-n) %>%
    arrange(F_BG, F_BU) %>%
    mutate(cat = factor(cat, levels = unique(cat)))
  
  av_crit = av_criteria %>%
    filter(BUSINESS_MODEL %in% unique(bu_levels$BUSINESS_MODEL),
           PRODUCT_TYPE == prod_type)
  
  p = temp %>%
    left_join(bu_levels) %>%
    ggplot(aes(x = cat, y = AV_PREC, fill = F_BG)) +
    geom_boxplot() +
    {
      if(BG == "ALL"){
        geom_hline(aes(yintercept = CEO_AV_CRIT), data = av_crit, linetype = 3, color = "red")
      }else{
        geom_hline(aes(yintercept = BG_HEAD_AV_CRIT), data = av_crit, linetype = 3, color = "red")
      }
    } +
    facet_wrap(~BUSINESS_MODEL, nrow = 1, scales = "free_x") +
    theme_bw() +
    xlab("") + ylab("") + 
    #theme(legend.position = "none") +
    scale_y_continuous(labels = scales::percent)
  
  ggplotly(p) %>%
    layout(yaxis = list(hoverformat = ",.0%")) #%>%
  # layout(
  #         legend = list(orientation = "h", x = 0, y = -0.1)
  # )
  
}

plot_bgbu_func = function(data_clean, av_criteria_for_plot, BG, BU){
  
  data_by_bgbu = data_clean %>%
    filter(F_BG == BG, F_BU == BU) %>%
    group_by(BUSINESS_MODEL, PRODUCT_TYPE) %>%
    mutate(n = n(),
           cat = paste0(BUSINESS_MODEL, " (n=", n, ")")) %>%
    ungroup() %>%
    mutate(av2 = case_when(
      AV_PREC >= 0.8 ~ 0.8,
      AV_PREC <= 0 ~ 0
    ))
  
  av_criteria_for_plot2 = data_by_bgbu %>%
    distinct(BUSINESS_MODEL, PRODUCT_TYPE, cat) %>%
    left_join(av_criteria_for_plot)
  
  data_by_bgbu %>%
    ggplot(aes(x = cat, y = AV_PREC)) +
    geom_boxplot() +
    geom_point(aes(x = cat, y = AV_PREC, col = ROLE), 
               data = av_criteria_for_plot2, shape = 17) +
    geom_text(aes(x = cat, y = AV_PREC, 
                  label = paste0(AV_PREC*100, "%"), 
                  col = ROLE), 
              data = av_criteria_for_plot2, hjust = 0, nudge_x = 0.1, size=4) + #, 
    geom_point(aes(x = cat, y = av2), col = "blue", shape = 13) +
    facet_wrap(~PRODUCT_TYPE, scales = "free_x", nrow = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none") +
    xlab("") +
    scale_y_continuous(labels = scales::percent, limits=c(0,0.8)) +
    ggtitle(BU)
  
}
plot_bg = function(data_clean, av_criteria_for_plot, BG){
  bu_vec = data_clean %>%
    filter(F_BG == BG) %>%
    pull(F_BU) %>%
    unique() %>%
    sort()
  
  pp = purrr::map(bu_vec, ~plot_bgbu_func(data_clean, av_criteria_for_plot, BG, .x))
  grid.arrange(grobs = pp, nrow = 1,
               top = paste0("BG: ", BG, "\n(三角形點為 CEO, BG_HEAD, BU_HEAD 需簽核的 AV(%) 標準)\n"),
               bottom = "BUSINESS_MODEL")
}

# 認證用 ----
my_check = function(user, password){
  
  if(user %in% paste0(c("NA","NC","ND","NE","NV"), "_HEAD")){
    result = TRUE
  }else{
    r <- POST("api_url_here",
              body = list(EmpNo = user,
                          EmpPwd = password),
              encode = "json")
    result = content(r)
  }
  
  list(result = result,
       user_info = list(empno = user))
}