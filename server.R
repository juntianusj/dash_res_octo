options(shiny.trace = TRUE)

library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(dygraphs)
library(xts)

config <- list(
  "specimens" = 3,
  "depths" = 7,
  "thermistors" = 3
)

rg <- 0.0083141

headers <- c(paste0("depth_", seq(config[["depths"]])),
             paste0("thermistor_", seq(config[["thermistors"]])))

specimen_columns <- unlist(lapply(seq(3), function(X) paste0(X, "_", headers)))

columns <- c("Date", "Time", specimen_columns, "Temp", "var1", "var2",
             "Voltage")

correctValue <- function(depth, ea, rg, tref, temp) {
  depth*exp(ea/rg*((1/(tref + 273.15) - (1/(temp + 273.15)))))
}

temp <- function(thermistor) {
  (1.287600011/10^3 + 2.357183092/10^4*log(thermistor) +
     9.509464377/10^8*(log(thermistor))^3)^(-1) - 273.15
}

shinyServer(function(input, output) {
  logs <- paste0("Data/",
                 list.files(path = "Data", pattern = "^[0-9]{8}[.](CSV|csv)"))
  res_octo_data <- Reduce(function(csv1, csv2) {
    merge(csv1, csv2, all = TRUE)
  }, lapply(logs, read_csv, col_names = columns))
  message("CSVs processed")
  res_octo_data %<>% mutate(Date = paste(Date, as.character(Time)) %>%
                              as.POSIXct(format = "%d/%m/%Y %H:%M:%S",
                                         tz = "GMT")) %>%
    select(-Time)
  message("cleaning up date and time")
  res_octo_data %<>%
    gather(SensorType, Value, -Temp, -var1, -var2, -Voltage, -Date) %>%
    mutate(Specimen = factor(gsub("_\\w+_\\d+$", "", SensorType)),
           SensorNumber = factor(gsub("^\\d+_\\w+_", "", SensorType)),
           SensorType = factor(gsub("(\\d_|_\\d)", "", SensorType)))
  message("restructuring data")
  print(head(res_octo_data))
  
  output$plots <- renderDygraph({
    message("vals <- res_octo_data ")
    vals <- res_octo_data %>% filter(Specimen == input$specimen &
                                       ((SensorNumber == input$depth &
                                           SensorType == "depth") |
                                          (SensorNumber == input$thermistor &
                                             SensorType == "thermistor"))) %>%
      spread(SensorType, Value) %>%
      mutate(Corrected = correctValue(depth, as.numeric(input$ea), rg,
                                       as.numeric(input$tref),
                                       temp(thermistor)))
    cvalsts <- xts(vals$Corrected, vals$Date)
    rvalsts <- xts(vals %>% select(Raw = depth) %>% pull(Raw), vals$Date)
    valsts <- cbind(cvalsts, rvalsts)
    dimnames(valsts)[[2]] <- c("Corrected", "Raw")
    dygraph(valsts, "Resistance by Time") %>% 
      dyRangeSelector(height = 20, strokeColor = "")
  })
  
  temperaturePlot <- renderDygraph({
    
  })
  

   

  
})
