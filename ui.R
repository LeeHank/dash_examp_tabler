ui = page_navbar(
  theme = bs_theme(version = 4,
                   bg = "#f4f6fa", # 淡藍色
                   fg = "#000",
                   "navbar-bg" = "#0062cc", # "white",
                   # "navbar-padding-x" = "8rem"
                   base_font = font_google("Prompt"),
                   heading_font = font_google("Prompt"),
                   code_font = font_google("JetBrains Mono")
                   ),
  # theme = bs_theme(),
  title = "AV_report",
  #bg = "#0062cc",
  nav(
    tags$head(includeCSS("www/custom.css")),
    style = "padding: 0rem 4rem",
    title = "App Usage",
    fluidRow(
      column(
        width = 2,
        tabler_card2("hahahha", "fdfdfdfdf")
      ),
      column(
        width = 2,
        tabler_card(card_title = "12646", 
                    card_subtitle = "TEM5", 
                    trend = "increase", # 變顏色用的
                    diff_value = "32%", # 右上角的趨勢
                    num_models = '53', # sub-title
                    plot_id = 'abcde' # plotly的id
        )
      ),
      column(
        width = 4,
        HTML(
          '<div class="card text-white bg-secondary mb-3" style="max-width: 18rem;">
  <div class="card-header">Header</div>
  <div class="card-body">
    <h5 class="card-title">Secondary card title</h5>
    <p class="card-text">Some quick example text to build on the card title and make up the bulk of the cards content.</p>
            </div>
            </div>'
        )
      ),
      column(
        width = 3,
        card(
          card_header("this is title"),
          card_body("hahahaha")
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        card(
          pickerInput("aaa",NULL, 
                      c("drift result", "BEOL Blur"), 
                      width = "100%", options = list(
                        style = "btn-primary"
                      )),
          card_body("hahahaha")
        )
      ),
      column(
        width = 3,
        tabler_card3(
          title = pickerInput("bbb",NULL, 
                              c("drift result", "BEOL Blur"), 
                              width = "100%", options = list(
                                style = "btn-primary"
                              )),
          div("hahahaha", style="padding: 1rem;")
        )
      )
    )
  ),
  nav(
    title = "Index Analysis",
    pickerInput("start_week", "start_week", choices = paste0("W",201:220), inline = TRUE, width = "fit"),
    HTML("&nbsp;&nbsp;"),
    pickerInput("end_week", "end_week", choices = paste0("W",201:220), inline = TRUE, width = "fit"),
    HTML("&nbsp;&nbsp;"),
    pickerInput("iq_metric", "image quality metric", choices = c("drift", "contrast", "fft", "snowflake", "blur"), inline = TRUE, width = "fit"),
    HTML("&nbsp;&nbsp;"),
    actionButton("query","start", class = "btn-success")
  ),
  nav(
    title = "utility classes",
    # add margin
    actionButton("primary", "Primary", icon("product-hunt"), class = "btn-primary m-2"),
    actionButton("secondary", "Secondary (default)", class = "m-2"),
    actionButton("success", "Success", icon("check"), class = "btn-success m-2"),
    br(),
    # add padding
    tabsetPanel(
      tabPanel("One", "No padding"),
      tabPanel("Two", "Very sad!")
    ),
    tabsetPanel(
      tabPanel("One", "With padding", class = "p-3"),
      tabPanel("Two", "Nice!", class = "p-3 border border-top-0 rounded-bottom")
    ),
    # add border & rounded corner
    tabsetPanel(
      type = "pills",
      tabPanel("One", "With padding", class = "p-3 border rounded"),
      tabPanel("Two", "Nice!")
    ),
    # add background color
    uiOutput("dat")
  ),
  nav_item(
    tags$a(icon("github"), "Shiny", href = "https://github.com/rstudio/shiny", target = "_blank")
  ),
  nav_spacer(),
  nav_menu(
    "Other links", align = "right",
    nav("c", paste(": tab c content")),
    nav_item(
      tags$a(icon("r-project"), "RStudio", href = "https://rstudio.com", target = "_blank")
    )
  )
)
