ui = page_navbar(
  theme = bs_theme(version = 4,
                   bg = "#f4f6fa", # 淡藍色
                   fg = "#000",
                   "navbar-bg" = "#0062cc", # "white",
                   # "navbar-padding-x" = "8rem"
                   ),
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
      )
    )
  ),
  nav(
    title = "Index Analysis",
    "tab_bbb's content_here"
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
