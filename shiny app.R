library(shiny)
library(DT)
library(shinythemes)
library(plm)
library(RCurl)
library(dplyr)
library(plyr)
# library(emibase)
library(plotrix)
# library(mgcv)
library(reshape2)
library(readxl)
library(knitr)
library(rworldmap)
library(cshapes)
library(countrycode)
library(texreg)
library(ggplot2)
library(directlabels)
library(splines)
library(MASS)
library(xtable)
source('functions.R')

runApp(shinyApp(
  
# ui ---------------------------------------------------------------------- 
  
  ui = fluidPage(
    
    theme = shinytheme("flatly"),
    
    navbarPage("OECD tourism",

               tabPanel("Models",
                        
                        sidebarLayout(
                          
                          sidebarPanel(
                            actionButton('download', "Initiate data download"),
                            uiOutput("select.cn2"),
                            uiOutput("model"),
                            uiOutput("range"),
                            plotOutput('country.map', height = 600)
                            # plotOutput('country.map')
                            # tableOutput("render.test")
                            # plotOutput("out.of.sample")
                            
                          ),
                          
                          mainPanel(
                            column(8,  plotOutput('plot.model', width=800)),
                            # column(8,  textOutput('slider.text')),
                            column(8, htmlOutput("formula")),
                            column(8,  plotOutput('out.of.sample', width=800)),
                            column(8, htmlOutput("formula2"))
                            
                            # column(8,  tableOutput('render.test'))
                          )
                          
                        )
                        
                      )
    )
  ),
# server ----------------------------------------------------------------------   
  
   server = function(input, output, session) {
    

    
    observeEvent(input$download, {
      
      withProgress(message = "downloading and tidying up data", value=0, { 
        
        
        download.terror(years.to.survey, "data/") 
        dt <- read.terror("data/terror/")
        dt <- arrange(dt, Date)
        na.index <- which(apply(dt, 1, function(x){ all(is.na(x)) }))
        if(length(na.index)!=0) dt <- dt[-na.index, ]
        write.csv(dt, file="output/terror raw data.csv", na="", row.names=F)
        
        incProgress(0.2, message = "Data from terrorist database has been downloaded")
        ## Tourism data
        
        dt.tour <- download.tourism("data/")
        
        incProgress(0.2, message  = "Data regarding tourism has been downloaded")
        
        ## Economic data
        
        dt.eco <- download.economy("data/")
        dt.eco <- make.economy.great.again(dt.eco)
        
        incProgress(0.2, message  = "Data regarding economy has been downloaded")
        
        ## OECD data on hours worked
        
        dt.oecd <- get_dataset("ANHRS")
        decode  <- get_decoder("data/")  
        
        incProgress(0.2, message  = "OECD data has been downloaded")
        
        # Decoding country names
        
        dt.oecd <- ddply(dt.oecd, ~COUNTRY + obsTime + EMPSTAT, function(xframe){
          
          xframe <<- xframe
          real.name <-  decode[decode$CODE==xframe$COUNTRY[1], "Country"]
          if(length(real.name)==0) real.name <- xframe$COUNTRY[1]
          xframe$COUNTRY <- real.name
          return(xframe)
          
        })
        
        dt.oecd <- dt.oecd[dt.oecd$EMPSTAT=="TE", ]
        dt.oecd[dt.oecd$COUNTRY=="KOR", "COUNTRY"] <- "South Korea"
        
        
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
        
        incProgress(0.2, message  = "Data has been baked")
        
      
      
      myData <- reactive({ master.data })
      
      output$select.cn <- renderUI({
        selectInput("cn_input", "Select a OECD country",
                    choices = unique(myData()[, "Country"]))
      })
      
      
    })
    
    # 2 tab -------------------------------------------------------------------
    
    myData2 <- reactive({ master.data })
    
    output$select.cn2 <- renderUI({
      selectInput("cn_input2", "Select a OECD country",
                  choices = unique(myData2()[, "Country"]))
    })
    
    output$model <- renderUI({
      selectInput("model2", "Select a model",
                  choices = c("OLS", "Fixed effects", "Random effects"))
    })
    
    output$range <- renderUI({
      sliderInput("range", "Select range of years for test data",
                    min=min(as.numeric(unique(myData2()[, "Date"]))), 
                    max=max(as.numeric(unique(myData2()[, "Date"]))) - 1, 
                    value=c(1995, 2011))
    })
    
    output$country.map <- renderPlot({
      
      all.regions <- c('eurasia', 'africa',  'latin america', 'north america' , 'uk' , 'oceania', 'asia')
      total.info <- getMap()
      
      OECD.cn <- data.frame(country = input$cn_input2,
                            OECD = rep.int("OECD countries",
                                           length(unique(input$cn_input2))))
      
      iso.2 <- countrycode_data[countrycode_data$country.name.en==input$cn_input2, "iso2c"]
      
      cn.info <- total.info[total.info$ISO_A2==iso.2, ]
      reg <- cn.info$GEO3major[1] %>% as.character()
      
      if(reg=="Latin America and the Caribbean") reg <- "latin america"
      if(reg=="Asia and the Pacific") reg <- "oceania"
      
      malMap <- joinCountryData2Map(OECD.cn, joinCode = "NAME",
                                    nameJoinColumn = "country", verbose = F, suggestForFailedCodes = F)
      
      mapCountryData(malMap, nameColumnToPlot="OECD", catMethod = "categorical",
                     missingCountryCol = gray(.8), oceanCol = 'cyan', mapTitle = paste0("Boundries of ", input$cn_input2), mapRegion = reg)
      
      
    })
    
    create.model <- reactive({
      
      modelz <- list()
      
      ddt <- myData2()
      ddt$Total.Arrivals <- log(ddt$Total.Arrivals)
      ddt$GDP.per.capita <- log(ddt$GDP.per.capita)
      ddt$Hours.worked   <- log(ddt$Hours.worked)
      
      pdata <- plm.data(ddt, index = c("Country", "Date")) 
        
      modelz[['OLS']] <-  lm(formula = Total.Arrivals ~ Terror.attacks + GDP.per.capita + Hours.worked, data=ddt)
        
      modelz[['Fixed effects']] <- plm(formula = Total.Arrivals ~ Terror.attacks + GDP.per.capita + Hours.worked, data=pdata, model="within")
        
      modelz[['Random effects']]  <- plm(formula = Total.Arrivals ~ Terror.attacks + GDP.per.capita + Hours.worked, data=pdata, model="random")
      
      modelz
      
    })
    
    output$formula <- renderPrint({
      
      tab <- xtable(summary(create.model()[[input$model2]])$coef, digits=c(3, 3, 3, 3, 3))
      print(tab, type="html", only.contents = F, comment = F, width = 600, 
            html.table.attributes = "border=0",  size = "\\setlength{\\tabcolsep}{32pt}")
      
    })
    
    fitted.values <- reactive({
      
      ddt <- myData2()
      ddt$Total.Arrivals <- log(ddt$Total.Arrivals)
      ddt$GDP.per.capita <- log(ddt$GDP.per.capita)
      ddt$Hours.worked   <- log(ddt$Hours.worked)
      
      X <- ddt[ddt$Country==input$cn_input2, c("Date", 'Terror.attacks', "GDP.per.capita", "Hours.worked")]
      
      if(input$model2=="OLS"){
        
        X <- ddply(X, ~Date, function(xframe){
          
          coefs <- coefficients(create.model()[["OLS"]])
          fc <- coefs[2] * xframe[2] + coefs[3] * xframe[3] + coefs[4] * xframe[4] + coefs[1]
          xframe$fit <- exp(fc) %>% as.numeric()
          return(xframe)
          
        })
        
      }
      
      if(input$model2=="Fixed effects"){
        
        X <- ddply(X, ~Date, function(xframe){
          
          coefs <- coefficients(create.model()[["Fixed effects"]])
          fc <- coefs[1] * xframe[2] + coefs[2] * xframe[3] + coefs[3] * xframe[4]
          fc <- fc + fixef(create.model()[["Fixed effects"]])[input$cn_input2] 
          xframe$fit <- exp(fc) %>% as.numeric()
          return(xframe)
        })
      }
        
        if(input$model2=="Random effects"){
          
          X <- ddply(X, ~Date, function(xframe){
            
            coefs <- coefficients(create.model()[["Random effects"]])
            fc    <- coefs[1] + coefs[2] * xframe[2] + coefs[3]* xframe[3] + coefs[4] * xframe[4] 
            xframe$fit <- exp(fc) %>% as.numeric()
            return(xframe)
          })
        }
      
        X$fit
    })
    
    output$plot.model <- renderPlot({
      
      data.to.plot <- myData2()[myData2()[, "Country"]==input$cn_input2, 'Total.Arrivals']
      data.to.plot <- cbind(data.to.plot, fitted.values())
      # grid.frame(x = as.numeric(myData2()[myData2()[, "Country"]==input$cn_input2, 'Date']),
      #            y = data.to.plot, xlab="Time")
      # 
      matplot(x = as.numeric(myData2()[myData2()[, "Country"]==input$cn_input2, 'Date']),
              y = data.to.plot, 
              lwd=3, lty=1, cex=2, pch=20, xlab="Time", type="o",
              col=c('dodgerblue4', "firebrick1"), ylab="Total arrivals")
      
      mtext(input$cn_input2, col="blueviolet", line=2, cex=1.5, adj = 0)
      
      mtext("Total Arrivals", col="firebrick3", line=1, cex=1.25, adj = 0)
      legendary2(c("Original", "Fitted"), col=c('dodgerblue4', "firebrick1"))
      
    })
    
    test.set <- reactive({
      
      myData2()[myData2()[, "Date"] %in% paste(input$range[1] : input$range[2]),]
      
    })
    
    training.set <- reactive({
      
      myData2()[myData2()[, "Date"] %in% paste((max(input$range) + 1 ):max(as.numeric(unique(myData2()[, "Date"])))),]
      
    })
    
    output$slider.text <- renderText({ input$range })
    
    output$render.test <- renderTable({

      test.set()

    })

    output$render.training <- renderTable({

      training.set()

    })

    create.model.test <- reactive({

      modelz <- list()

      ddt <- test.set()
      ddt$Total.Arrivals <- log(ddt$Total.Arrivals)
      ddt$GDP.per.capita <- log(ddt$GDP.per.capita)
      ddt$Hours.worked   <- log(ddt$Hours.worked)

      pdata <- plm.data(ddt, index = c("Country", "Date"))

      modelz[['OLS']] <-  lm(formula = Total.Arrivals ~ Terror.attacks + GDP.per.capita + Hours.worked, data=ddt)

      modelz[['Fixed effects']] <- plm(formula = Total.Arrivals ~ Terror.attacks + GDP.per.capita + Hours.worked, data=pdata, model="within")

      modelz[['Random effects']]  <- plm(formula = Total.Arrivals ~ Terror.attacks + GDP.per.capita + Hours.worked, data=pdata, model="random")

      modelz

    })
    
    output$formula2 <- renderPrint({
      
      tab <- xtable(summary(create.model.test()[[input$model2]])$coef, digits=c(3, 3, 3, 3, 3))
      print(tab, type="html", only.contents = F, comment = F, width = 600, 
            html.table.attributes = "border=0",  size = "\\setlength{\\tabcolsep}{32pt}")
      
    })
    
    fitted.values.test <- reactive({
      
      ddt <- training.set()
      ddt$Total.Arrivals <- log(ddt$Total.Arrivals)
      ddt$GDP.per.capita <- log(ddt$GDP.per.capita)
      ddt$Hours.worked   <- log(ddt$Hours.worked)
      
      X <- ddt[ddt$Country==input$cn_input2, c("Date", 'Terror.attacks', "GDP.per.capita", "Hours.worked")]
      
      if(input$model2=="OLS"){
        
        X <- ddply(X, ~Date, function(xframe){
          
          coefs <- coefficients(create.model.test()[["OLS"]])
          fc <- coefs[2] * xframe[2] + coefs[3] * xframe[3] + coefs[4] * xframe[4] + coefs[1]
          xframe$fit <- exp(fc) %>% as.numeric()
          return(xframe)
          
        })
        
      }
      
      if(input$model2=="Fixed effects"){
        
        X <- ddply(X, ~Date, function(xframe){
          
          coefs <- coefficients(create.model.test()[["Fixed effects"]])
          fc <- coefs[1] * xframe[2] + coefs[2] * xframe[3] + coefs[3] * xframe[4]
          fc <- fc + fixef(create.model.test()[["Fixed effects"]])[input$cn_input2] 
          xframe$fit <- exp(fc) %>% as.numeric()
          return(xframe)
        })
      }
      
      if(input$model2=="Random effects"){
        
        X <- ddply(X, ~Date, function(xframe){
          
          coefs <- coefficients(create.model.test()[["Random effects"]])
          fc    <- coefs[1] + coefs[2] * xframe[2] + coefs[3]* xframe[3] + coefs[4] * xframe[4] 
          xframe$fit <- exp(fc) %>% as.numeric()
          return(xframe)
        })
      }
      
      X$fit
    })
    
    output$out.of.sample <- renderPlot({

      data.to.plot <- training.set()[training.set()[, "Country"]==input$cn_input2, 'Total.Arrivals']
      data.to.plot <- cbind(data.to.plot, fitted.values.test())
      # grid.frame(x = as.numeric(training.set()[training.set()[, "Country"]==input$cn_input2, 'Date']),
      #            y = data.to.plot, xlab="Time")

      matplot(x = as.numeric( training.set()[ training.set()[, "Country"]==input$cn_input2, 'Date']),
              y = data.to.plot,
              lwd=3, lty=1, cex=2, pch=20, xlab="Time", type="o",
              col=c('dodgerblue4', "firebrick1"), ylab="Total arrivals")
      #
      mtext(input$cn_input2, col="blueviolet", line=2, cex=1.5, adj = 0)

      mtext("Total Arrivals", col="firebrick3", line=1, cex=1.25, adj = 0)
      legendary2(c("Original", "Forecasted"), col=c('dodgerblue4', "firebrick1"))

    })
    })
  }
))









