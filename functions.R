download.terror <- function(years, path){
  
  files <- list.files(path)
  if(length(grep(unique(files), pattern = "raw data"))!=length(years)){
  
    for(year in years){
      cat('downloading year...', year, "\n")
      url <- paste0('http://www.start.umd.edu/gtd/search/ResultsCSV.aspx?csv=1&casualties_type=&casualties_max=&start_year=', 
                    year, '&start_month=1&start_day=1&end_year=', year, '&end_month=12&end_day=31')
      download.file(url, paste0(path, "raw data", year, ".csv"), "internal", quiet = FALSE, mode = "w",
                    cacheOK = TRUE,
                    extra = getOption("download.file.extra"))
    }
  } else {
    cat("The files appear to be downloaded \n")
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
  raw.data[raw.data=="<NA>"] <- NA
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
  
  # finding unique rows
  
  raw.data$key <- paste0(raw.data$Date, "+", raw.data$Country, "+", raw.data$City, "+", raw.data$Fatalities, "+", raw.data$Injured)
  
  unique.frame <- ddply(raw.data, ~key, function(xframe){
    xframe <<- xframe
    # print(xframe$key)
    return(xframe[nrow(xframe), ])
  })
  
  na.index <- which(apply(unique.frame, 1, function(x){ all(is.na(x)) }))
  if(length(na.index)!=0) unique.frame <- unique.frame[-na.index, ]
  # Returning results
  
  unique.frame$key <- NULL
  
  return(unique.frame)
}

legendary2 <- function (legend, col, line = 0, side = 3, adj = 1, cex = 0.8, 
                        presymbol = " - ") 
{
  legend <- paste(presymbol, legend)
  olegend <- legend
  for (i in 1:length(olegend)) {
    xlegend <- legend
    xlegend <- paste0("'", legend, "'")
    xlegend[-i] <- paste0("phantom(`", xlegend[-i], "`)")
    xlegend <- paste0(xlegend, collapse = " * ")
    xlegend <- paste0("expression(", xlegend, ")")
    xlegend <- paste0("mtext(", xlegend, ",col = '", col[i], 
                      "',line = ", line, ",side = ", side, ",adj = ", adj, 
                      ",cex = ", cex, ")")
    eval(parse(text = xlegend))
  }
}

pie.chart <- function(data, slices, cols=NULL, title){
  data <- data[!is.na(data)]
  for.pie <- numeric()
  
  for(j in 2:length(slices)){
    for.pie <- c(for.pie, length(data[which(data>=slices[j - 1] & data<slices[j])])/length(data))
  }  
  
  for.pie <- c(for.pie, length(data[which(data>=slices[j])])/length(data))
  
  if(is.null(cols)) cols <- 1:length(slices)
  
  label <- character()
  
  for(j in 1:length(slices)){
    if(j == length(slices)){
      label <- c(label, paste0("More than ", slices[j])) 
    } else {
      if(j == 1) {
        label <- c(label, paste0("Equal to ", slices[j]))
      } else {
        label <- c(label, paste0("Between ", slices[j], " and ", slices[j + 1]))
      }
    }
  }
  
  pie3D(x = for.pie, col=cols, labels= label, main = title, labelcex = 1.2, explode = 0.1, radius=.9, labelrad = 1.6)
  legendary2(round(for.pie*100, 1), cols, cex=1)
}


createdir <- function (dir) 
{
  if (file.exists(dir)) {
    if (!file.info(dir)$isdir) {
      dir.create(dir, recursive = TRUE)
      cat("\n Created  directory ", dir, "\n")
    }
  }
  else {
    dir.create(dir, recursive = TRUE)
    cat("\n Created  directory ", dir, "\n")
  }
}


aggregate.terror <- function(dat, freq="daily"){
  
  # This function is very specific to this project
  
  # freq can obtain three values: daily, monthly and yearly
  
  # dt - data frame which we will be aggregating
  
  dates <- unique(dat$Date)
  
  if(freq=="monthly"){
    
    dates <- substr(dates, 1, 7) %>% unique()
    
  }
  
  if(freq=="yearly"){
    
    dates <- substr(dates, 1, 4) %>% unique()
    
  }
  
    result.table <- matrix(ncol=3, nrow=length(dates)) %>% as.data.frame()
    colnames(result.table) <- c("Date", "Injured", "Killed")
    result.table[, 1] <- dates
    
    for(d in dates){
      cat("calculating...", d, "\n")
      result.table[result.table$Date==d, "Injured"] <- dat[grep(d, dat$Date), "Injured"] %>% sum(na.rm=T)
      result.table[result.table$Date==d, "Killed"]  <- dat[grep(d, dat$Date), "Fatalities"] %>% sum(na.rm=T)
    }
  
  return(result.table)
}

grid.frame <- function (x = NULL, y, grid.lty = "dotted", grid.col = "lightgray", 
          xlab = "", ylab = "", ...) 
{
  if (is.null(x)) {
    if (is.null(dim(y))) {
      x <- 1:length(y)
    }
    else {
      x <- 1:nrow(y)
    }
  }
  matplot(x, y, type = "n", xlab = xlab, ylab = ylab, ...)
  grid(col = grid.col, lty = grid.lty)
  box()
}