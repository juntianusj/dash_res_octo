library(shiny)
library(dygraphs)

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectInput("specimen", "Specimen:",
                    choices = seq(config[["specimens"]])),
        selectInput("depth", "Depth:", choices = seq(config[["depths"]])),
        selectInput("thermistor", "Thermistor:",
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
