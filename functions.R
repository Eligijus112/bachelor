download.terror <- function(years){

  for(year in years){
    cat('downloading year...', year, "\n")
    url <- paste0('http://www.start.umd.edu/gtd/search/ResultsCSV.aspx?csv=1&casualties_type=&casualties_max=&start_year=', 
                  year, '&start_month=1&start_day=1&end_year=', year, '&end_month=12&end_day=31')
    download.file(url, paste0("data/raw data", year, ".csv"), "internal", quiet = FALSE, mode = "w",
                              cacheOK = TRUE,
                              extra = getOption("download.file.extra"))
  }

}

read.terror <- function(path){
  raw.data <- data.frame()
  files    <- list.files(path) 
  
  for(file in files){
    duom <- read.csv(paste0(path, file), stringsAsFactors = F)
    raw.data <- rbind.fill(raw.data, duom)    
  }
  
  ## Formating in a nice way
  raw.data[raw.data==""] <- NA
  na.index <- which(apply(raw.data, 1, function(x) all(is.na(x))))
  if(length(na.index)!=0) raw.data <- raw.data[-na.index, ]
  raw.data <- raw.data[, c("DATE", "COUNTRY", "CITY", "PERPETRATOR.1", "FATALITIES", "INJURED", "TARGET.TYPE.1", "ATTACK.TYPE.1",
                           "WEAPON.TYPE.1")]
  raw.data <- rename(raw.data, replace = c("DATE" = "Date", "CITY"="City", 
                                           "COUNTRY" = "Country", "PERPETRATOR.1"="Perpetrator",
                                           "FATALITIES"="Fatalities", "INJURED"="Injured", 
                                           "TARGET.TYPE.1"="Target type", "ATTACK.TYPE.1" = "Attack type",
                                           "WEAPON.TYPE.1"="Weapon type"))
  raw.data$Fatalities <- raw.data$Fatalities %>% as.numeric()
  raw.data$Injured    <- raw.data$Injured %>% as.numeric()
  return(raw.data)
}



