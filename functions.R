
# Functions for data manipulation -----------------------------------------

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


MASplineVector <-  function (x, n = 4, k = 0.7, step = 4, epsilon = NA, method = "monoH.FC", 
                             n0 = n, n1 = n, k0 = k, k1 = k, plot = F) 
{
  forplot1 <- x
  require(TTR)
  x.length <- length(x)
  x.notna <- which(!is.na(x))
  x.first <- min(x.notna)
  x.last <- max(x.notna)
  x <- x[c(x.first:x.last)]
  time <- c(x.first:x.last)
  s <- splinefun(time, x, method = method)
  x <- s(time)
  x <- c(rep(NA, x.first - 1), x, rep(NA, x.length - x.last))
  forplot2 <- x
  sk.pok <- max(which(!is.na(x))) - min(which(!is.na(x)))
  n.def <- sk.pok
  k.def <- 1
  n1 <- ifelse(is.na(n1), n.def, n1)
  n0 <- ifelse(is.na(n0), n.def, n0)
  k1 <- ifelse(is.na(k1), k.def, k1)
  k0 <- ifelse(is.na(k0), k.def, k0)
  n1 <- ifelse(n1 > sk.pok, sk.pok, n1)
  n0 <- ifelse(n0 > sk.pok, sk.pok, n0)
  x.notna.spl <- which(!is.na(x))
  if (length(x.notna.spl) > n1) {
    j <- x.last
    while (j < x.length) {
      x_aug <- (c(x, NA) - c(NA, x))[c(1:length(x))]
      y <- x_aug[!(is.na(x_aug))]
      if (is.na(step) | step == 0 | step == 1) {
        k11 <- k1
      }
      else {
        step.no <- j - x.last + 1
        if (step.no >= step) {
          k11 <- k1
        }
        else {
          k11 <- k1 + (1 - k1) * (step - step.no)/step
        }
      }
      x[j + 1] <- ifelse(n1 == 1, x[j] + y[length(y)] * 
                           k11, x[j] + SMA(y, n1)[length(y)] * k11)
      j <- j + 1
    }
  }
  if (length(x.notna.spl) > n0) {
    j <- x.first
    while (j > 1) {
      x_aug <- (c(x, NA) - c(NA, x))[c(1:length(x))]
      x_maz <- c(-x_aug[2:length(x_aug)], NA)
      y <- x_maz[!(is.na(x_maz))]
      if (is.na(step) | step == 0 | step == 1) {
        k00 <- k0
      }
      else {
        step.no <- x.first - j + 1
        if (step.no >= step) {
          k00 <- k0
        }
        else {
          k00 <- k0 + (1 - k0) * (step - step.no)/step
        }
      }
      x[j - 1] <- ifelse(n0 == 1, x[j] + y[1] * k00, x[j] + 
                           SMA(y, n0)[n0] * k00)
      j <- j - 1
    }
  }
  if (!is.na(epsilon)) {
    origlo.bound <- min(x[x.notna])
    origup.bound <- max(x[x.notna])
    origgap <- origup.bound - origlo.bound
    lo.bound <- origlo.bound - epsilon * origgap
    up.bound <- origup.bound + epsilon * origgap
    xna <- setdiff(c(1:length(x)), x.notna)
    xna.forward <- xna[xna > x.last]
    xna.backward <- xna[xna < x.first]
    if (any(x[xna.forward] > up.bound)) {
      x[xna.forward] <- x[x.last] + (x[xna.forward] - x[x.last])/(max(x[xna.forward]) - 
                                                                    x[x.last]) * (up.bound - x[x.last])
    }
    else {
      if (any(x[xna.forward] < lo.bound)) {
        x[xna.forward] <- x[x.last] + (x[xna.forward] - 
                                         x[x.last])/(min(x[xna.forward]) - x[x.last]) * 
          (lo.bound - x[x.last])
      }
    }
    if (any(x[xna.backward] > up.bound)) {
      x[xna.backward] <- x[x.first] + (x[xna.backward] - 
                                         x[x.first])/(max(x[xna.backward]) - x[x.first]) * 
        (up.bound - x[x.first])
    }
    else {
      if (any(x[xna.backward] < lo.bound)) {
        x[xna.backward] <- x[x.first] + (x[xna.backward] - 
                                           x[x.first])/(min(x[xna.backward]) - x[x.first]) * 
          (lo.bound - x[x.first])
      }
    }
    if (any(is.na(x))) {
      time <- c(1:length(x))
      s <- splinefun(time, x, method = "monoH.FC")
      x <- s(time)
    }
  }
  if (plot == T) {
    plot(x, type = "o", pch = 20, col = "black", main = paste0("n=", 
                                                               n, ", k=", k, ", step=", step, ", epsilon=", epsilon, 
                                                               ", method=", method), cex.main = 0.8)
    points(x, col = "firebrick2", pch = 20)
    points(forplot2, col = "dodgerblue1", pch = 20)
    points(forplot1, col = "black", pch = 20)
  }
  return(x)
}

fill.inside <- function (x, ...) 
{
  if (sum(!is.na(x)) > 1) {
    fill.indexes <- min(which(!is.na(x))):max(which(!is.na(x)))
    x[fill.indexes] <- MASplineVector(x[fill.indexes], ...)
  }
  return(x)
}


# Functions for data visualization ----------------------------------------

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



# Functions for downloading and reading data ------------------------------------------

download.terror <- function(years, path){
  
  createdir(paste0(path, "terror"))
  
  files <- list.files(paste0(path, "terror/"))
  if(length(grep(unique(files), pattern = "terror"))!=length(years)){
    
    for(year in years){
      cat('downloading year...', year, "\n")
      url <- paste0('http://www.start.umd.edu/gtd/search/ResultsCSV.aspx?csv=1&casualties_type=&casualties_max=&start_year=', 
                    year, '&start_month=1&start_day=1&end_year=', year, '&end_month=12&end_day=31')
      download.file(url, paste0(path, "terror/terror ", year, ".csv"), "internal", quiet = FALSE, mode = "w",
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

aggregate.terror.by.country <- function(dat){
  
  # This function is very specific to this project
  
  # dt - data frame which we will be aggregating
  
  dates <- unique(dat$Date)
  
  dates <- substr(dates, 1, 4) %>% unique()
  
  ## calculating number of incidents  
  
  c.by.c <- ddply(dat, ~Country, function(xframe){
    
    xframe <<- xframe
    xframe$numb <- NA
    
    for(d in dates){
      
      if(length(grep(d, xframe$Date))!=0){
        xframe[grep(d, xframe$Date), "numb"] <- nrow(xframe[grep(d, xframe$Date), ])
      }
      
      xframe[grep(d, xframe$Date), "Date"] <- d
    }
    
    return(xframe)
  })
  
  # dropping not wanted rows
  
  c.by.c <- ddply(c.by.c, ~Country + Date, function(xframe){
    
    xframe <<- xframe
    
    xframe <- xframe[nrow(xframe), ]
    return(xframe)
  })
  
  # if a year is missing that means there were no recorded terrorist incidents
  
  c.by.c <- c.by.c[, c("Date", "Country", "numb")]
  
  c.by.c <- ddply(c.by.c, ~Country, function(xframe){
    
    xframe <<- xframe
    
    missing.years <- setdiff(years.to.survey, xframe$Date)
    if(length(missing.years)!=0){
      
      add.rows <- matrix(nrow=length(missing.years), ncol=dim(xframe)[2]) %>% as.data.frame()
      colnames(add.rows) <- colnames(xframe)  
      add.rows$Date    <- missing.years
      add.rows$Country <- xframe$Country[1]
      add.rows$numb    <- 0
      xframe <- rbind.fill(xframe, add.rows)
      
    }
    
    return(xframe)
    
  })
  
  c.by.c <- plyr::arrange(c.by.c, Country,  Date)
  colnames(c.by.c) <- c("Date", "Country", "Terror.attacks")
  return(c.by.c)
}


## Function to download tourism data
# Note that a more comprehensive source is the UNWTO database but the data there is not free

download.tourism <- function(path, write=T){
  
  url <- "http://api.worldbank.org/v2/en/indicator/ST.INT.ARVL?downloadformat=csv"
  createdir(paste0(path, "tourism"))
  download.file(url, destfile = paste0(path, "tourism/tour.zip"), mode='wb')
  unzip(paste0(path, "tourism/tour.zip"), exdir = paste0(path, "tourism"))
  
  # we find the file with the biggest size 
  
  files <- list.files(paste0(path, "tourism/"))
  name  <- files[which(max(file.info(paste0(path, "tourism/", files))$size) == file.info(paste0(path, "tourism/", files))$size)] 
  
  raw.data <- read.csv(paste0(path, "tourism/", name), header=F, stringsAsFactors = F)
  
  # Data clean up
  
  raw.data <- raw.data[-c(1:2), ]
  
  colnames(raw.data) <- as.character(raw.data[1, ])
  raw.data <- raw.data[-1, ]
  raw.data <- plyr::rename(raw.data, c("Indicator Name" = "Indicator"))
  raw.data$Indicator <- "Total arrivals"
  colnames(raw.data) <- gsub(" ", "", colnames(raw.data))
  raw.data <- raw.data[, -grep("NA", colnames(raw.data))]
  
  # Saving the country codes for lates use
  
  write.csv(raw.data[, c("CountryName", "CountryCode")], "input/decoder.csv",row.names=F, na="")
  
  if(write){
    
    write.csv(raw.data, "data/tourism/raw tourist data.csv", row.names = F, na="")  
    
  }
  
  # Interpolating
  
  raw.data[, paste(years.to.survey)] <- apply(raw.data[, paste(years.to.survey)], 1, function(x){
    x <- exp(fill.inside(log(as.numeric(x)*1000)))/1000
    x <- round(x, digits = 0)
    return(x)
  }) %>% t()
  
  # melting the data frame
  
  melted.data <- melt(raw.data)
  melted.data <- melted.data[, c("CountryName", "variable", "value")]
  melted.data <- rename(melted.data, c("variable" = "Year", "value" = "Total.Arrivals"))
  
  return(melted.data)
}

## Function to download ALL economic data from world bank

download.world.bank <- function(path) {
  
  createdir(paste0(path, "economy"))
  
  if(length(grep("raw economic data.csv", list.files(paste0(path, "economy"))))==1){
    
    cat("The data seems to be downloaded")
    return(read.csv(paste0(path,"economy/raw economic data.csv"), stringsAsFactor=F))
    
  } else{
    
    decode <- read.csv("input/decoder.csv", stringsAsFactors = F)
    
    if(dim(decode)[1]<1){
      
      print("Decoder is missing. Please download tourism data first")
      
    } else {
      
      big.data <- data.frame()
      
      options(warn=2) ## turns warnings into errors
      
      for(cd in decode$CountryCode){
        cd <- as.character(cd)
        cat("Downloading...", decode[decode$CountryCode==cd, "CountryName"], "\n")
        url <- paste0("http://api.worldbank.org/v2/en/country/",cd,"?downloadformat=csv")
        
        # we will give the downloader and unziper 5 chances to work
        
        j <- 1
        
        while( j <= 5 ){
          err <- tryCatch({
            download.file(url, destfile = paste0(path, "economy/tmp.zip"), mode='wb')
            unzip(paste0(path, "economy/tmp.zip"), exdir = paste0(path, "economy"))
          },
          error = function(e){
            j <- j + 1
            return("ERROR")
          })
          
          if(err[1]!="ERROR"){
            break
          } else { j <- j + 1}
        }
        
        # we find the file with the appropriate name
        
        name  <- paste0("API_" ,cd ,"_DS2_en_csv_v2.csv")
        
        raw.data <- read.csv(paste0(path, "economy/", name), header=F, stringsAsFactors = F)
        
        # we tidy the data up
        
        raw.data <- raw.data[-c(1:2), ]
        
        colnames(raw.data) <- as.character(raw.data[1, ])
        raw.data <- raw.data[-1, ]
        raw.data <- plyr::rename(raw.data, c("Indicator Name" = "Indicator"))
        colnames(raw.data) <- gsub(" ", "", colnames(raw.data))
        raw.data <- raw.data[, -grep("NA", colnames(raw.data))]
        
        big.data <- rbind(big.data, raw.data)
        
        # removing the unnecessary files
        
        files <- setdiff(list.files(paste0(path, "economy/")), "tmp.zip")
        file.remove(paste0(path, "economy/", files[1:length(files)]))
        
      }
    }
    
    write.csv(big.data, paste0(path,"economy/raw economic data.csv"), na="", row.names=F)
    
    return(big.data)
  }
}

## we will only download GDP per capita 

download.economy <- function(path) {
  
  createdir(paste0(path, "economy"))
  
  decode <- read.csv("input/decoder.csv", stringsAsFactors = F)
  
  cat("Downloading... GDP per capita \n")
  url <- paste0('http://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.CD?downloadformat=csv')
  
  # we will give the downloader and unziper 5 chances to work
  
  j <- 1
  
  while( j <= 5 ){
    err <- tryCatch({
      download.file(url, destfile = paste0(path, "economy/tmp.zip"), mode='wb')
      unzip(paste0(path, "economy/tmp.zip"), exdir = paste0(path, "economy"))
    },
    error = function(e){
      j <- j + 1
      return("ERROR")
    })
    
    if(err[1]!="ERROR"){
      break
    } else { j <- j + 1}
  }
  
  # we find the file with the appropriate name
  
  name  <- paste0("API_NY.GDP.PCAP.CD_DS2_en_csv_v2.csv")
  
  raw.data <- read.csv(paste0(path, "economy/", name), header=F, stringsAsFactors = F)
  
  # we tidy the data up
  
  raw.data <- raw.data[-c(1:2), ]
  
  colnames(raw.data) <- as.character(raw.data[1, ])
  raw.data <- raw.data[-1, ]
  raw.data <- plyr::rename(raw.data, c("Indicator Name" = "Indicator"))
  colnames(raw.data) <- gsub(" ", "", colnames(raw.data))
  raw.data <- raw.data[, -grep("NA", colnames(raw.data))]
  
  # big.data <- rbind(big.data, raw.data)
  
  # removing the unnecessary files
  
  files <- setdiff(list.files(paste0(path, "economy/")), "tmp.zip")
  file.remove(paste0(path, "economy/", files[1:length(files)]))
  
  write.csv(raw.data, paste0(path,"economy/gdp per capita.csv"), na="", row.names=F)
  
  return(raw.data)
}



# Leaving desired products

make.economy.great.again <- function(dt){
  
  # We will leave the following products (for now):
  
  # GDP per capita USD - to measure the state of the economy
  # 
  # 
  
  colnames(dt) <- gsub("X", "", colnames(dt))
  
  dt <- dt[dt$Indicator %in% c("GDP per capita (current US$)"), ]
  dt[dt<0] <- NA
  
  dt[, paste(years.to.survey)] <- apply(dt[, paste(years.to.survey)], 1, function(x){
    x <<- x
    x <- exp(fill.inside(log(as.numeric(x)*1000)))/1000
    x <- round(x, digits = 0)
    return(x)
  }) %>% t()
  
  # melting the data frame
  
  melted.data <- melt(dt)
  melted.data <- melted.data[, c("CountryName", "Indicator" , "variable", "value")]
  melted.data <- rename(melted.data, c("variable" = "Date"))
  
  return(melted.data)
  
}

## functions to download OECD data for annual hours worked

get_decoder <- function(path, ...){
  createdir(paste0(path, "OECD"))
  url <- 'http://www.oecd.org/migration/mig/34107835.xls'
  download.file(url, paste0(path, "OECD/decoder.xls"), "internal", quiet = FALSE, mode = "wb",
                cacheOK = TRUE,
                extra = getOption("download.file.extra"))
  dec <- read_excel(paste0(path, "OECD/decoder.xls"), sheet=1)
  dec$CODE <- gsub("CSFR-", "", dec$CODE)
  dec$CODE <- gsub("FYUG-", "", dec$CODE)
  dec$CODE <- gsub("KOREA-", "", dec$CODE)
  dec$CODE <- gsub("USSR-", "", dec$CODE)
  dec <- dec %>% as.data.frame(dec)
  
  return(dec)
}

get_datasets <- function(...) {
  
  url <- "http://stats.oecd.org/RestSDMX/sdmx.ashx/GetKeyFamily/all"
  
  page <- xml2::read_xml(url)
  
  id <- xml2::xml_attr(xml2::xml_find_all(page, "//*[@agencyID='OECD']"), "id")
  
  title <- xml2::xml_text(
    xml2::xml_find_all(page, "//*[@agencyID='OECD']/*[@xml:lang='en']"))
  
  df <- data.frame(id, title, ...)
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}


get_dataset <- function(dataset, filter = NULL, start_time = NULL, end_time = NULL, 
                        pre_formatted = FALSE, ...) {
  
  # Case error
  if (is.null(filter) && pre_formatted) {
    stop("If pre_formatted is TRUE, you must provide a value to the filter argument.")
  }
  
  # Case all data
  if (is.null(filter) && !pre_formatted) {
    filter <- "all"
  } 
  
  # Case user-provided filter
  if (!is.null(filter) && !pre_formatted) {
    filter <- lapply(filter, function(x) paste(x, collapse = "+")) 
    filter <- paste(filter, collapse = ".")
  }
  
  # Case pre-formatted filter
  if (!is.null(filter) && pre_formatted) {
    filter <- filter
  }
  
  path <- sprintf("restsdmx/sdmx.ashx/GetData/%s/%s/all", dataset, filter)
  
  url_list <- list("scheme"   = "http", 
                   "hostname" = "stats.oecd.org",
                   "path"     = path,
                   "query"    = list("startTime" = start_time,
                                     "endTime" = end_time))
  class(url_list) <- "url"
  
  url <- httr::build_url(url_list)
  df <- as.data.frame(rsdmx::readSDMX(url), ...)
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}

find.neighbours <- function(cn, path){
  
  createdir(paste0(path, "GeoCountries"))
  
  # Calculating minimal distances between countries
  # This might take a while
  
  require(cshapes)
  if(length(grep("distances.csv", list.files(paste0(path, "GeoCountries"))))==1){
    
    cat("The data seems to be downloaded")
    dmat <- read.csv(paste0(path, "GeoCountries/distances.csv"), header = F)
    colnames(dmat) <- dmat[1, ]
    rownames(dmat) <- dmat[1, ]
    dmat <- dmat[-1, -1] 
    
    
  } else{
    
    dmat <- distmatrix(as.Date("2002-1-1"), type="mindist", useGW=F)
    write.csv(dmat, paste0(path, "GeoCountries/distances.csv"))
  }
  
  dmat <- as.data.frame(dmat)
  
  names.on.file <- colnames(dmat)
  decode <- merge(data.frame(cown=as.numeric(names.on.file)), countrycode_data[, c("country.name.en", "cown")], sort=F)
  
  decode$country.name <- gsub("Republic of Korea", "South Korea", decode$country.name)
  decode$country.name <- gsub("Slovakia", "Slovak Republic", decode$country.name)
  decode$country.name <- gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", decode$country.name)  
  decode$country.name <- gsub("United States of America", "United States", decode$country.name)  
  
  colnames(dmat) <- decode$country.name
  rownames(dmat) <- decode$country.name
  
  results <- data.frame()
  
  for(country in cn){
    
    numb <- length(which(dmat[country, ]<100))
    results <- rbind(results, t(as.data.frame(c(country, numb)))) 
    
  }
  
  colnames(results) <- c("Country", "Numb.neighbour")
  return(results)
  
}

## This function downloads the number of neighbours of the given countries

# download.neighbours <- function(cn, path){
#  
#   createdir(paste0(path, "GeoCountries"))
#   
#   if(length(grep("tmp.zip", list.files(paste0(path, "GeoCountries"))))==1){ 
#     
#     cat("The data seems to be downloaded")
#     
#   } else {
#     
#     url <- 'http://download.geonames.org/export/dump/allCountries.zip'
#     download.file(url, destfile = paste0(path, "GeoCountries/tmp.zip"))
#     
#   }
#   
#   
#   ## Downloading the decoder
#   url <- "http://download.geonames.org/export/dump/countryInfo.txt"
#   download.file(url, destfile = paste0(path, "GeoCountries/decoder.txt"))
#   decoder <- read.table(paste0(path, "GeoCountries/decoder.txt"), sep=";", header=T)
#   
#   # Adjustments
#   
#   decoder <- 
#   
#   grep(decoder, "Australia")
#   
#   unzip(paste0(path, "GeoCountries/tmp.zip"), exdir = paste0(path, "GeoCountries"))
#   
#   return(read.table(paste0(path, "GeoCountries/allCountries.txt"), sep=";"))
# }

# Other -----------------------------------------------------------------


## we will finish the function bellow later

statistics.middle.east <- function(start.year){
  url <- 'https://en.wikipedia.org/wiki/List_of_modern_conflicts_in_the_Middle_East'
  terror.table <- getURL(url)
  write.table(terror.table, file="output/wiki page.txt") # it is easier to navigate in txt form
  
  
  as.text <- read.table("output/wiki page.txt", sep="\t") %>% apply(1, as.character)
  as.text[as.text==""] <- NA
  as.text <- as.text[which(!is.na(as.text))] %>% paste0(as.text, collapse="")
  index.start <- gregexpr("wikitable sortable", as.text)[[1]] %>% as.numeric()
  index.end   <- gregexpr("2016_Turkish_coup", as.text)[[1]] %>% as.numeric() %>% max() # the last known conflict
  index.end   <- index.end + 100
  
  as.text <- substr(as.text, index.start, index.end)
  ## Greping the positions of the links
  full.links <- as.character()
  link.indexes <- gregexpr("/wiki/", as.text)[[1]] %>% as.numeric()
  for(link in link.indexes){
    char <- (substr(as.text, link, link + 6) %>% strsplit(split=""))[[1]]
    char <- char[length(char)]
    i <-  1
    while(char!=' ' && i < 100){
      # cat(substr(as.text, link + i + 6, link + 6 + i), "\n")
      char <- substr(as.text, link + i + 6, link + 6 + i)
      i <- i + 1  
    }
    
    end.index <- i - 1
    full.link <- substr(as.text, link, link + end.index + 4)
    full.links <- rbind(full.links, full.link)
  }
  
  ## deleting unwanted links
  
  bad.words <- c("File", "Wikipedia", "Special:", "Portal:", "Privacy_policy", "Category:", "Template:",
                 "Google_Books", "Lists_of_wars")
  
  for(word in bad.words){
    if(length(grep(word, full.links))!=0){
      full.links <- full.links[-grep(word, full.links)]
    }
  }
  
  # Searching through the links
  
  for(link in full.links){
    url <- paste0("https://en.wikipedia.org", link)
    html <- getURL(url)
    write.table(html, file=paste0("html files",
                                  substr(link, start = 6, stop=length(strsplit(link, split="")[[1]])),
                                  ".txt"))
    as.text <- read.table(paste0("html files",
                                 substr(link, start = 6, stop=length(strsplit(link, split="")[[1]])),
                                 ".txt"), sep="\t") %>% apply(1, as.character)
    
    # The list of countries is between between the titles "Belligerents" and "Commanders and leaders"
    as.text[as.text==""] <- NA
    as.text <- as.text[which(!is.na(as.text))] %>% paste0(as.text, collapse="")
    
    index.start <- gregexpr(">Belligerents</th>", as.text)[[1]] %>% as.numeric() %>% min()
    index.end   <- gregexpr(">Commanders and leaders</th>", as.text)[[1]] %>% as.numeric() %>% min() 
    as.text <- substr(as.text, index.start, index.end)
  }
  
}

joinCountryData2Map <- function (dF, joinCode = "ISO3", nameJoinColumn = "ISO3V10", 
                                 nameCountryColumn = "Country", suggestForFailedCodes = FALSE, 
                                 mapResolution = "coarse", projection = NA, verbose = FALSE) 
{
  mapWithData <- getMap(resolution = mapResolution)
  if (!is.na(projection)) 
    warning("the projection argument has been deprecated, returning Lat Lon, use spTransform from package rgdal as shown in help details or the FAQ")
  listJoinCodesNew <- c("ISO_A2", "ISO_A3", "FIPS_10_", "ADMIN", 
                        "ISO_N3")
  listJoinCodesOld <- c("ISO2", "ISO3", "FIPS", "NAME", "UN")
  listJoinCodes <- c(listJoinCodesOld, listJoinCodesNew)
  if (joinCode %in% listJoinCodes == FALSE) {
    stop("your joinCode (", joinCode, ") in joinCountryData2Map() is not one of those supported. Options are :", 
         paste(listJoinCodes, ""), "\n")
    return(FALSE)
  }
  joinCodeOld <- joinCode
  if (joinCode %in% listJoinCodesOld) {
    joinCode <- listJoinCodesNew[match(joinCode, listJoinCodesOld)]
  }
  if (is.na(match(nameJoinColumn, names(dF)))) {
    stop("your chosen nameJoinColumn :'", nameJoinColumn, 
         "' seems not to exist in your data, columns = ", 
         paste(names(dF), ""))
    return(FALSE)
  }
  dF[[joinCode]] <- as.character(dF[[nameJoinColumn]])
  dF[[joinCode]] <- gsub("[[:space:]]*$", "", dF[[joinCode]])
  if (joinCode == "ADMIN") {
    dF$ISO3 <- NA
    for (i in 1:nrow(dF)) dF$ISO3[i] = rwmGetISO3(dF[[joinCode]][i])
    joinCode = "ISO3"
    nameCountryColumn = nameJoinColumn
  }
  matchPosnsInLookup <- match(as.character(dF[[joinCode]]), 
                              as.character(mapWithData@data[[joinCode]]))
  failedCodes <- dF[[joinCode]][is.na(matchPosnsInLookup)]
  numFailedCodes <- length(failedCodes)
  numMatchedCountries <- nrow(dF) - numFailedCodes
  
  failedCountries <- dF[[nameCountryColumn]][is.na(matchPosnsInLookup)]
  failedCountries <- cbind(failedCodes, failedCountries = as.character(failedCountries))
  
  if (verbose) 
    print(failedCountries)
  matchPosnsInUserData <- match(as.character(mapWithData@data[[joinCode]]), 
                                as.character(dF[[joinCode]]))
  codesMissingFromUserData <- as.character(mapWithData@data[[joinCode]][is.na(matchPosnsInUserData)])
  countriesMissingFromUserData <- as.character(mapWithData@data[["NAME"]][is.na(matchPosnsInUserData)])
  numMissingCodes <- length(codesMissingFromUserData)
  
  mapWithData@data <- cbind(mapWithData@data, dF[matchPosnsInUserData, 
                                                 ])
  
  if(verbose){
    cat(numMatchedCountries, "codes from your data successfully matched countries in the map\n")
    cat(numFailedCodes, "codes from your data failed to match with a country code in the map\n")
    cat(numMissingCodes, "codes from the map weren't represented in your data\n")
  }
  
  invisible(mapWithData)
}

MSE <- function(y,yhat)
{
  mean((y-yhat)**2)
}

## mean absolute (prediction) error
MAE <- function(y,yhat)
{
  mean(abs(y-yhat))
}

## mean absolute percentage (prediction) error
MAPE <- function(y,yhat,percent=TRUE)
{
  
  na.index <- union(which(is.na(y)), which(is.na(yhat)))
  
  if(length(na.index)!=0){
    
    y <- y[-na.index]
    yhat <- yhat[-na.index]
    
  } 
  
  if(percent){
    100*mean(abs( (y-yhat)/y ))
  } else {
    mean(abs( (y-yhat)/y ))
  }
}

#Predictin pgmm models

predict.pgmm.custom <- function(model, X.frame, j, i, h){
  
  coefs <- model$coefficients
  var.names <- names(coefs)
  
  # Creating lags if needed
  
  if(length(grep("lag", var.names))!=0){
    
    lagged <- var.names[grep("lag", var.names)]
    
    for(laag in lagged){
      
      # extracting variable name
      
      x <- substr(laag, 5, length(strsplit(laag, "")[[1]]))
      x <- gsub("[(]", "", x)
      x <- gsub("[)]", "", x)
      
      xx <- strsplit(x, ",")[[1]][1]
      
      # extracting number of lags
      
      ll <- strsplit(x, ",")[[1]][2] %>% trimws()
      
      if(length(grep("j", ll))!=0){
        
        ll <- 1:j
        
      }
      
      if(length(grep("i", ll))!=0){
        
        ll <- 1:i
        
      }
      
      if(length(grep("h", ll))!=0){
        
        ll <- 1:h
        
      }
      
      # adding lags to X.frame
      
      for(l in ll){
        
        data.to.write <- lag(X.frame[, xx], as.numeric(l))
        eval(parse(text=paste0("X.frame$`", laag,  "`<-", 'data.to.write')))
        
      }
    }
  }
  
  ## Calculating forecasts
  
  fit <- rep.int(0, dim(X.frame)[1])
  for(cc in names(coefs)){
    
    fit   <- fit + coefs[which(cc==names(coefs))] * X.frame[, cc]
    
  }
  
  eval(parse(text=paste0("X.frame$fc.pgmm <-", 'fit')))
  
  return(X.frame)    
}


# summary of pgmm ---------------------------------------------------------

# source("pgmm.methods.R")
