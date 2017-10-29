options(shiny.trace = TRUE)

library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(dygraphs)
library(xts)

shinyServer(function(input, output) {
  withProgress(message = "Loading data", value = 1, expr = {
    logs <- list.files(path = "Data", pattern = "^[0-9]{8}[.](CSV|csv)",
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
  })

  getCorrectedValues <- function(specimen, depth, thermistor, ea, tref, rg) {
    res_octo_data %>% filter(Specimen == specimen &
                                     ((SensorNumber == depth &
                                         SensorType == "depth") |
                                        (SensorNumber == thermistor &
                                           SensorType == "thermistor"))) %>%
    distinct() %>%
    select(-SensorNumber) %>%
    spread(SensorType, Value) %>%
    mutate(Corrected = correctValue(depth, as.numeric(ea), rg,
                                     as.numeric(tref),
                                     temp(thermistor)),
           Depth = depth) %>%
    drop_na()
  }

  output$resistancePlot <- renderDygraph({
    vals <- lapply(input$depth, function(depth) {
      getCorrectedValues(input$specimen, depth, input$thermistor,
                         input$ea, input$tref, rg)
    })
    cvalsts <- lapply(vals, function(val) {
      xts(val$Corrected, val$Date)
    })
    cvalsts <- do.call(cbind, cvalsts)
    rvalsts <- lapply(vals, function(val) {
      xts(val %>% select(Raw = depth) %>% pull(Raw), val$Date)
    })
    rvalsts <- do.call(cbind, rvalsts)
    valsts <- cbind(cvalsts, rvalsts)
    dimnames(valsts)[[2]] <- c(paste0("Depth ", input$depth, " Corrected"),
                               paste0("Depth ", input$depth, " Raw"))
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

  output$download_data <- downloadHandler(
    filename = "dash_res_octo_data.csv",
    content = function(con) {
      write.csv(res_octo_data, con, row.names = FALSE)
    })



})
