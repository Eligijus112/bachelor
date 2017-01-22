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