#' Download data from Energidataservice.dk
#' 
#' Downloads time-bound production, consumption and price data from
#' Danish TSO Energinet via its free API, found on api.energidataservice.dk 
#' The API does not require user and/or token. 
#' See energidataservice.dk for documentaion.
#' 
#' This function downloads settlement, i.e. validated data
#' 
#' Last updated 2022-12-17 by Magnus Gottlieb
#' 
#' @param most.recent integer: No. of observations, starting with most recent
#' @param all bool: Used to get entire series. Defaults to FALSE.
#' @param start time of first point in data series. Format="YYYY-mm-dd" or "YYYY-mm-dd HH:MM". Defaults to Jan. 1st current year.
#' @param end time of last point in data series. Format="YYYY-mm-dd" or "YYYY-mm-dd HH:MM". Defaults to now.
#' @return A data frame with production and consumption data
#' @export
energidataservice <- function(dataset="ProductionConsumptionSettlement",
                              most.recent=NA, all=FALSE,
                              start=lubridate::floor_date(Sys.time(), "year"),
                              end=Sys.time()){
  url <- paste0("https://api.energidataservice.dk/dataset/", dataset, "?")
  if(all){
    limit <- jsonlite::fromJSON(RCurl::getURL(url))$total # no. of available observations
    dat <- jsonlite::fromJSON(RCurl::getURL(paste0(url, "limit=", limit))) 
  } else if(!is.na(most.recent)){
    dat <- jsonlite::fromJSON(RCurl::getURL(paste0(url, "limit=", most.recent)))
  } else {
    start <- format(as.POSIXct(start), "%Y-%m-%dT%H:%M")
    end <- format(as.POSIXct(end), "%Y-%m-%dT%H:%M")
    request <- paste0(url, "start=", start, "&end=", end)
    dat <- jsonlite::fromJSON(RCurl::getURL(request))
  }
  dat <- dat$records
  try({dat$HourUTC <- as.POSIXct(dat$HourUTC, format="%Y-%m-%dT%H:%M:%S")}, silent=TRUE)
  try({dat$Minutes5UTC <- as.POSIXct(dat$Minutes5UTC, format="%Y-%m-%dT%H:%M:%S")}, silent=TRUE)
  try({dat$Minutes1UTC <- as.POSIXct(dat$Minutes1UTC, format="%Y-%m-%dT%H:%M:%S")}, silent=TRUE)
  try({dat$HourDK <- as.POSIXct(dat$HourDK, format="%Y-%m-%dT%H:%M:%S")}, silent=TRUE)
  try({dat$Minutes5DK <- as.POSIXct(dat$Minutes5DK, format="%Y-%m-%dT%H:%M:%S")}, silent=TRUE)
  try({dat$Minutes1DK <- as.POSIXct(dat$Minutes1DK, format="%Y-%m-%dT%H:%M:%S")}, silent=TRUE)
  if(is.null(nrow(dat))){
    cat("Something went wrong. Check input.")
    return()
  }
  cat(paste("Downloaded", nrow(dat), "datapoints from energidataservice.dk"))
  return(dat)
}
