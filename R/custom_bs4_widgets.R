# R 的寫法，我先寫個 function 我比較好彈性使用
tabler_card = function(card_title, 
                       card_subtitle, 
                       trend, # 變顏色用的
                       diff_value, # 右上角的趨勢
                       num_models, # sub-title
                       plot_id # plotly的id
){
  if(trend == "increase"){
    last_icon = icon("arrow-up")
    last_color = "text-danger"
  }else{
    last_icon = icon("arrow-down")
    last_color = "text-success"
  }
  div(
    class = "tabler-card",
    div(
      class = "tabler-card-body",
      style = "padding: 0",
      div(
        style = "display: flex; justify-content: space-between; align-items: center; padding: 0rem 1rem",
        div(
          class = "tabler-subheader",
          card_subtitle
        ),
        span(class = last_color,
             last_icon, 
             tags$small(diff_value)
        )
      ),
      h1(card_title, style = "margin: 0rem;padding: 0rem 1rem;"),
      span("(From ", span(tags$b(num_models)), " models)",
           style = "margin: 0rem 0rem 1rem; padding: 0rem 1rem;")#,
      #plotlyOutput(plot_id, height = "60px")
    )
  )
}

tabler_card2 = function(title, ...){
  div(
    class = "tabler-card",
    div(
      class = "tabler-card-body",
      span(class = "tabler-card-title", title),
      ...
    )
  )
}


# backup
# library(shiny)
# library(htmltools)
# library(tidyverse)
# # library(echarts4r)
# 
# bs4_deps <- htmlDependency(
#   name = "Bootstrap",
#   version = "4.6.0",
#   src = c(href = "https://cdn.jsdelivr.net/npm/bootstrap@4.6.0/dist/"),
#   script = "js/bootstrap.bundle.min.js",
#   stylesheet = "css/bootstrap.min.css"
# )
# 
# # add all dependencies to a tag. Don't forget to set append to TRUE to preserve any existing dependency
# add_tabler_deps <- function(tag) {
#   # below, the order is of critical importance!
#   deps <- list(bs4_deps)
#   attachDependencies(tag, deps, append = TRUE)
# }