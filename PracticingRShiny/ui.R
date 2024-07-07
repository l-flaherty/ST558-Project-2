# ui.R

fluidPage(
  titlePanel("Gold Holdings Analysis"),
  sidebarLayout(
    sidebarPanel(
      dateInput("default_date", "Select Date:", value = "2024-05-31", format = "yyyy-mm-dd")
    ),
    mainPanel(
      plotOutput("gold_plot")
    )
  )
)
