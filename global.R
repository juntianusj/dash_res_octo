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