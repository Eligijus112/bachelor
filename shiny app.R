library(shiny)
library(DT)
library(shinythemes)

source('functions.R')

runApp(shinyApp(
  
  ui = fluidPage(
    
    theme = shinytheme("united"),
    
    navbarPage("OECD tourism",
               
               
               tabPanel("OECD countries info", 
                        
                        actionButton('download', "Initiate data download"),
                        uiOutput("select.cn"),
                        plotOutput('country.map')
               ),
               
               
               
               tabPanel("Data visualization")
    )
    
    
    
  ),
  
  server = function(input, output, session) {
    
    observeEvent(input$download, {
      
      download.terror(years.to.survey, "data/") 
      dt <- read.terror("data/terror/")
      dt <- arrange(dt, Date)
      na.index <- which(apply(dt, 1, function(x){ all(is.na(x)) }))
      if(length(na.index)!=0) dt <- dt[-na.index, ]
      write.csv(dt, file="output/terror raw data.csv", na="", row.names=F)
      
      ## Tourism data
      
      dt.tour <- download.tourism("data/")
      
      ## Economic data
      
      dt.eco <- download.economy("data/")
      dt.eco <- make.economy.great.again(dt.eco)
      
      ## OECD data on hours worked
      
      dt.oecd <- get_dataset("ANHRS")
      decode  <- get_decoder("data/")  
      
      # Decoding country names
      
      dt.oecd <- ddply(dt.oecd, ~COUNTRY + obsTime + EMPSTAT, function(xframe){
        
        xframe <<- xframe
        real.name <-  decode[decode$CODE==xframe$COUNTRY[1], "Country"]
        if(length(real.name)==0) real.name <- xframe$COUNTRY[1]
        xframe$COUNTRY <- real.name
        return(xframe)
        
      })
      
      
      master.data <- aggregate.terror.by.country(dt)
      
      dt.tour <- rename(dt.tour, c("Year" = "Date", "CountryName" = "Country"))
      dt.eco <- rename(dt.eco, c("Year" = "Date", "CountryName" = "Country"))
      
      dt.tour$Country <- gsub("Korea, Rep.", "South Korea", dt.tour$Country)
      dt.eco$Country <- gsub("Korea, Rep.", "South Korea", dt.eco$Country)
      
      master.data <- merge(master.data, dt.tour)
      
      for(prod in unique(dt.eco$Indicator)){
        
        tmp <- dt.eco[dt.eco$Indicator==prod, c("Country", "Date", "value")]
        tmp <- rename(tmp, c("value" = prod))
        master.data <- merge(master.data, tmp)
        
      }
      
      # Merging with OECD data
      
      dt.oecd <- plyr::rename(dt.oecd, c("COUNTRY" = "Country", "obsTime" = "Date", "obsValue" = "Working hours"))
      dt.oecd <- dt.oecd[, c("Country", "Date", "Working hours")]
      
      master.data <- merge(master.data, dt.oecd)
      
      # Dropping incomplete rows
      
      master.data <- master.data[complete.cases(master.data), ]
      
      master.data <- arrange(master.data, Country)
      
      # Adding number of neighbours
      
      neighbours <- find.neighbours(unique(master.data$Country), path=path)
      
      master.data <- merge(master.data, neighbours, sort=F)
      master.data$Numb.neighbour <- as.numeric(as.character(master.data$Numb.neighbour))
      
      
      master.data <- plyr::rename(master.data, c('GDP per capita (current US$)' = "GDP.per.capita"))
      master.data <- plyr::rename(master.data, c('Working hours' = "Hours.worked"))
      
      myData <- reactive({ master.data })
      
      output$select.cn <- renderUI({
        selectInput("cn_input", "Select a OECD country",
                    choices = unique(myData()[, "Country"]))
        
        
        
      })
      
      output$country.map <- renderPlot({
        
        all.regions <- c('eurasia', 'africa',  'latin america', 'north america' , 'uk' , 'oceania', 'asia')
        total.info <- getMap()
        
        OECD.cn <- data.frame(country = input$cn_input,
                              OECD = rep.int("OECD countries",
                                             length(unique(input$cn_input))))
        
        iso.2 <- countrycode_data[countrycode_data$country.name==input$cn_input, "iso2c"]
        
        cn.info <- total.info[total.info$ISO_A2==iso.2, ]
        reg <- cn.info$GEO3major[1] %>% as.character()
        
        if(reg=="Latin America and the Caribbean") reg <- "lating america"
        if(reg=="Asia and the Pacific") reg <- "oceania"
        
        malMap <- joinCountryData2Map(OECD.cn, joinCode = "NAME",
                                      nameJoinColumn = "country", verbose = F, suggestForFailedCodes = F)
        
        mapCountryData(malMap, nameColumnToPlot="OECD", catMethod = "categorical",
                       missingCountryCol = gray(.8), oceanCol = 'cyan', mapTitle = paste0("Boundries of ", input$cn_input), mapRegion = reg)
        
        
      })
      
    })
  }
))
