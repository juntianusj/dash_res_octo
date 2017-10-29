options(shiny.trace = TRUE)

library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(dygraphs)
library(xts)

shinyServer(function(input, output) {
  logs <- list.files(path = "~/Downloads/Data 2", pattern = "^[0-9]{8}[.](CSV|csv)",
                     full.names = TRUE)
  res_octo_data <- Reduce(function(csv1, csv2) {
    merge(csv1, csv2, all = TRUE)
  }, lapply(logs, read_csv, col_names = columns))

  res_octo_data %<>% mutate(Date = paste(Date, as.character(Time)) %>%
                              as.POSIXct(format = "%d/%m/%Y %H:%M:%S",
                                         tz = "GMT")) %>%
    select(-Time)

  res_octo_data %<>%
    gather(SensorType, Value, -Temp, -var1, -var2, -Voltage, -Date) %>%
    mutate(Specimen = factor(gsub("_\\w+_\\d+$", "", SensorType)),
           SensorNumber = factor(gsub("^\\d+_\\w+_", "", SensorType)),
           SensorType = factor(gsub("(\\d_|_\\d)", "", SensorType)))

    values <- reactive({
      res_octo_data %>% filter(Specimen == input$specimen &
                                       ((SensorNumber == input$depth &
                                           SensorType == "depth") |
                                          (SensorNumber == input$thermistor &
                                             SensorType == "thermistor"))) %>%
      distinct() %>%
      select(-SensorNumber) %>%
      spread(SensorType, Value) %>%
      mutate(Corrected = correctValue(depth, as.numeric(input$ea), rg,
                                       as.numeric(input$tref),
                                       temp(thermistor))) %>%
      drop_na()
    })
  output$resistancePlot <- renderDygraph({
    vals <- values()
    cvalsts <- xts(vals$Corrected, vals$Date)
    rvalsts <- xts(vals %>% select(Raw = depth) %>% pull(Raw), vals$Date)
    valsts <- cbind(cvalsts, rvalsts)
    dimnames(valsts)[[2]] <- c("Corrected", "Raw")
    dygraph(valsts, "Resistance by Time") %>%
      dyRangeSelector(height = 20, strokeColor = "")
  })

  output$temperaturePlot <- renderDygraph({
    vals <- values()
    # Something weird going on here with %<>%, so using self assignment.
    vals <- vals %>% mutate(Temp = temp(thermistor))
    valsts <- xts(vals$Temp, vals$Date)
    dygraph(valsts, "Temperature (degC) by Time") %>%
      dyRangeSelector(height = 20, strokeColor = "")
  })





})
