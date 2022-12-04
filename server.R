server <- function(input, output, session) {
  # bs_themer()
  output$dat <- renderUI({
    table <- DT::datatable(mtcars, fillContainer = TRUE, style = "bootstrap4", rownames = FALSE)
    bs4_card(table, "The mtcars dataset")
  })
}