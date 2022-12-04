ui = navbarPageWithRightUI(
  theme = bs_theme(
    version = 4,
    bg = "#f4f6fa",
    fg = "#000",
    "navbar-bg" = "white"#,
    # "navbar-padding-x" = "8rem"
  ),
  id = "tabs",
  # right_ui = div(
  #         tags$em(tags$b("2021-08"))
  # ),
  right_ui = uiOutput("report_date"),
  title = "AV(%) Dashboard",
  #認證頁面
  # div(
  #         id = "box1",
  #         class = "login",
  #         div(
  #                 style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
  #                 div(
  #                         class = "well",
  #                         h2(class = "text-center", "Please Login"),
  # 
  #                         textInput(inputId     = "user_name",
  #                                   label       = tagList(icon("user"), "工號"),
  #                                   placeholder = "Enter Employee Number"),
  # 
  #                         passwordInput(inputId     = "password",
  #                                       label       = tagList(icon("unlock-alt"), "密碼(筆電登入密碼)"),
  #                                       placeholder = "Enter password"),
  #                         uiOutput(outputId = "help"),
  #                         div(
  #                                 class = "text-center",
  #                                 actionButton(inputId = "login_button",
  #                                              "Log in",
  #                                              class = "btn-primary",
  #                                              style = "color:white;")
  #                         )
  #                 )
  #         )
  # ),
  
  # CEO ----
  tabPanel(
    tags$head(includeCSS("www/custom.css")),
    style = "padding: 0rem 4rem",
    icon = icon("home"),
    title = "Overview",
    # CEO: info cards ----
    uiOutput("ceo_card_panel") %>% withSpinner(),
    br(),
    # CEO: side by side boxplot & top 3 plot ----
    fluidRow(
      column(
        width = 4,
        tabler_card2(
          title = "AV(%) model distribution by BG",
          plotlyOutput("bg_compare_boxplot", height = "300px") %>% 
            withSpinner()
        )
      ),
      column(width = 8,
             tabler_card2(
               title = "Top 3 lowest & highest AV(%) models by BG",
               plotlyOutput("ALL_highest_lowest", height = "300px") %>% 
                 withSpinner()
             ))
    ),
    br(),
    # CEO: biz & prod boxplot ----
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          title = "AV(%) model distribution by PRODUCT_TYPE + BUSINESS_MODEL + BG",
          navlistPanel(
            well = FALSE,
            widths = c(2,10),
            tabPanel(
              title = "Module",
              plotlyOutput("CEO_biz_module", height = "300px") %>% 
                withSpinner()
            ),
            tabPanel(
              title = "System",
              plotlyOutput("CEO_biz_system", height = "300px") %>% 
                withSpinner()
            ),
            tabPanel(
              title = "Component",
              plotlyOutput("CEO_biz_component", height = "300px") %>% 
                withSpinner()
            )
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          title = "Raw Data (Sorting by STATUS, F_BG, F_BU)", 
          reactableOutput("ceo_data")  %>% 
            withSpinner()
          #br(),
          #uiOutput("ceo_data_filter")
        )
      )
    )
    
  ),
  # NA page ----
  tabPanel(
    icon = icon("car"),
    title = "NA",
    style = "padding: 0rem 4rem",
    # NA: info cards ----
    uiOutput("NA_card_panel") %>% 
      withSpinner(),
    br(),
    # NA: side by side boxplot & top 3 plot ----
    fluidRow(
      column(
        width = 4,
        tabler_card2(
          title = "AV(%) model distribution by BU",
          plotlyOutput("NA_compare_boxplot", height = "300px") %>% 
            withSpinner()
        )
      ),
      column(width = 8,
             tabler_card2(
               title = "Top 3 lowest & highest AV(%) models in NA",
               plotlyOutput("NA_highest_lowest", height = "300px") %>% 
                 withSpinner()
             ))
    ),
    br(),
    # NA biz vs prod_type boxplot ----
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          "AV(%) model distribution by PRODUCT_TYPE + BUSINESS_MODEL + BU",
          navlistPanel(
            well = FALSE,
            widths = c(2,10),
            tabPanel(
              title = "Module",
              plotlyOutput("NA_biz_module", height = "300px") %>% 
                withSpinner()
            ),
            tabPanel(
              title = "System",
              plotlyOutput("NA_biz_system", height = "300px") %>% 
                withSpinner()
            )
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          title = "Raw Data (Sorting by STATUS, F_BG, F_BU)",
          reactableOutput("NA_data")  %>%
            withSpinner()
          #br(),
          #uiOutput("ceo_data_filter")
        )
      )
    )
  ),
  # NC page ----
  tabPanel(
    style = "padding: 0rem 4rem",
    icon = icon("laptop-house"),
    title = "NC",
    uiOutput("NC_card_panel") %>% 
      withSpinner(),
    br(),
    # NC: side by side boxplot & top 3 plot ----
    fluidRow(
      column(
        width = 4,
        tabler_card2(
          title = "AV(%) model distribution by BU",
          plotlyOutput("NC_compare_boxplot", height = "300px") %>% 
            withSpinner()
        )
      ),
      column(width = 8,
             tabler_card2(
               title = "Top 3 lowest & highest AV(%) models in NC",
               plotlyOutput("NC_highest_lowest", height = "300px") %>% 
                 withSpinner()
             ))
    ),
    br(),
    # NC biz vs prod_type boxplot ----
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          title = "AV(%) model distribution by PRODUCT_TYPE + BUSINESS_MODEL + BU",
          navlistPanel(
            well = FALSE,
            widths = c(2,10),
            tabPanel(
              title = "Module",
              plotlyOutput("NC_biz_module", height = "300px") %>% 
                withSpinner()
            ),
            tabPanel(
              title = "System",
              plotlyOutput("NC_biz_system", height = "300px") %>% 
                withSpinner()
            )
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          title = "Raw Data (Sorting by STATUS, F_BG, F_BU)",
          reactableOutput("NC_data")  %>%
            withSpinner()
          #br(),
          #uiOutput("ceo_data_filter")
        )
      )
    )
  ),
  # ND page ----
  tabPanel(
    style = "padding: 0rem 4rem",
    icon = icon("network-wired"),
    title = "ND",
    uiOutput("ND_card_panel") %>% 
      withSpinner(),
    br(),
    # ND: side by side boxplot & top 3 plot ----
    fluidRow(
      column(
        width = 4,
        tabler_card2(
          title = "AV(%) model distribution by BU",
          plotlyOutput("ND_compare_boxplot", height = "300px") %>% 
            withSpinner()
        )
      ),
      column(width = 8,
             tabler_card2(
               title = "Top 3 lowest & highest AV(%) models in ND",
               plotlyOutput("ND_highest_lowest", height = "300px") %>% 
                 withSpinner()
             ))
    ),
    br(),
    # ND biz vs prod_type boxplot ----
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          title = "AV(%) model distribution by PRODUCT_TYPE + BUSINESS_MODEL + BU",
          navlistPanel(
            well = FALSE,
            widths = c(2,10),
            tabPanel(
              title = "Module",
              plotlyOutput("ND_biz_module", height = "300px") %>% 
                withSpinner()
            ),
            tabPanel(
              title = "System",
              plotlyOutput("ND_biz_system", height = "300px") %>% 
                withSpinner()
            )
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          title = "Raw Data (Sorting by STATUS, F_BG, F_BU)", 
          reactableOutput("ND_data")  %>% 
            withSpinner()
          #br(),
          #uiOutput("ceo_data_filter")
        )
      )
    )
  ),
  # NE page ----
  tabPanel(
    style = "padding: 0rem 4rem",
    icon = icon("broadcast-tower"),
    title = "NE",
    uiOutput("NE_card_panel") %>% 
      withSpinner(),
    br(),
    # NE: side by side boxplot & top 3 plot ----
    fluidRow(
      column(
        width = 4,
        tabler_card2(
          title = "AV(%) model distribution in NE",
          plotlyOutput("NE_compare_boxplot", height = "300px") %>% 
            withSpinner()
        )
      ),
      column(width = 8,
             tabler_card2(
               title = "Top 3 lowest & highest AV(%) models in NE",
               plotlyOutput("NE_highest_lowest", height = "300px") %>% 
                 withSpinner()
             ))
    ),
    br(),
    # NE biz vs prod_type boxplot ----
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          title = "AV(%) model distribution by PRODUCT_TYPE + BUSINESS_MODEL",
          navlistPanel(
            well = FALSE,
            widths = c(2,10),
            tabPanel(
              title = "Module",
              plotlyOutput("NE_biz_module", height = "300px") %>% 
                withSpinner()
            ),
            tabPanel(
              title = "Component",
              plotlyOutput("NE_biz_component", height = "300px") %>% 
                withSpinner()
            )
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          title = "Raw Data (Sorting by STATUS, F_BG, F_BU)", 
          reactableOutput("NE_data")  %>% 
            withSpinner()
          #br(),
          #uiOutput("ceo_data_filter")
        )
      )
    )
  ),
  # NV page ----
  tabPanel(
    style = "padding: 0rem 4rem",
    icon = icon("microchip"), #calendar
    title = "NV",
    uiOutput("NV_card_panel") %>% 
      withSpinner(),
    br(),
    # NV: side by side boxplot & top 3 plot ----
    fluidRow(
      column(
        width = 4,
        tabler_card2(
          title = "AV(%) model distribution in NV",
          plotlyOutput("NV_compare_boxplot", height = "300px") %>% 
            withSpinner()
        )
      ),
      column(width = 8,
             tabler_card2(
               title = "Top 3 lowest & highest AV(%) models in NV",
               plotlyOutput("NV_highest_lowest", height = "300px") %>% 
                 withSpinner()
             ))
    ),
    br(),
    # NV biz vs prod_type boxplot ----
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          title = "AV(%) model distribution by PRODUCT_TYPE + BUSINESS_MODEL",
          navlistPanel(
            well = FALSE,
            widths = c(2,10),
            tabPanel(
              title = "Module",
              plotlyOutput("NV_biz_module", height = "300px") %>% 
                withSpinner()
            )
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        tabler_card2(
          title = "Raw Data (Sorting by STATUS, F_BG, F_BU)", 
          reactableOutput("NV_data")  %>% 
            withSpinner()
          #br(),
          #uiOutput("ceo_data_filter")
        )
      )
    )
  )
  
  # 導入 css
  
  # nav_mine(),
  # tabler_navbar(
  #         # brand_url = "https://preview-dev.tabler.io",
  #         brand_image = "Navbar",
  #         nav_menu = tabler_navbar_menu(
  #                 tabler_navbar_menu_item(
  #                         text = "Tab 1",
  #                         tabName = "tab1",
  #                         selected = TRUE
  #                 ),
  #                 tabler_navbar_menu_item(
  #                         text = "Tab 2",
  #                         tabName = "tab2"
  #                 ),
  #                 p("19002329")
  #         )
  # ),
  
  
)

ui = secure_app(ui,
                enable_admin = TRUE,
                fab_position = "none",
                # customize UI
                # id = "auth",
                # add image on top ?
                tags_top = tags$img(src = "wnclogo.jpg"),
                # add information on bottom ?
                tags_bottom = tags$div(
                  tags$p("For any question, please contact"),
                  tags$p("myname(myname) #mynumber"),
                  style = "text-align:center"))