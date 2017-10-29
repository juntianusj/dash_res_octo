library(shiny)
library(dygraphs)

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectizeInput("specimen", "Specimen:",
                    choices = seq(config[["specimens"]])),
        selectizeInput("depth", "Depth:", choices = seq(config[["depths"]]),
                       multiple = TRUE,
                       selected = seq(config[["specimens"]])[1]),
        selectizeInput("thermistor", "Thermistor:",
                    choices = seq(config[["thermistors"]])),
        textInput("ea", "Ea:", value = 21),
        textInput("tref", "Tref:", value = 20),
        downloadButton("download_data", label = "Download data",
                       style = "width:100%")
      )
    ),
    mainPanel(
      dygraphOutput("resistancePlot"),
      dygraphOutput("temperaturePlot")
    )
  )
))
