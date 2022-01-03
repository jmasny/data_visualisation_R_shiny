#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(reshape)
library(DT)
library(dplyr)
library(googleVis)
library(lmtest)
library(knitr)
library(markdown)
library(rmarkdown)

shinyServer(function(input, output) {
    
    b <- reactiveValues(dataLoadDownload = FALSE)
    
    observeEvent(input$getDataFromServer,{
        b$dataLoadDownload <- !b$dataLoadDownload
    })
    
    dataIn <- reactive({
        
        if(b$dataLoadDownload==TRUE) {
            
            var = input$setRange
            from = var[[1]]
            to = var[[2]]
            
            getNBPData <- function(year=2019){
                
                ret <- data.frame()
                
                if(year>=2013){
                    
                    fileName <- paste0(year,"_NBP_data.csv")
                    
                    cat(paste("Downloading data", year, "\n"))
                    
                    res <- try({
                        
                        d <- readLines(paste0("https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_",year,".csv"))
                        d <- d[-2]
                        d <- d[-c((length(d)-3):length(d))]
                        tmpColnames <- strsplit(d[1],";",useBytes=T)[[1]]
                        tmpColnames <- tmpColnames[-c((length(tmpColnames)-1):length(tmpColnames))]
                        d <- do.call("rbind",
                                     lapply(strsplit(d[-1],";"),
                                            function(x){
                                                matrix(as.numeric(gsub(",",".",x[-c((length(x)-1):length(x))])),nrow=1)
                                            })
                        )
                        colnames(d) <- tmpColnames
                        d <- as.data.frame(d)
                        
                        d$data <- as.Date(as.character(d$data),format="%Y%m%d")
                        ret <- d
                        #write.table(ret,file=fileName,sep=";",dec=",",row.names=F)
                        
                    },silent=T)
                    
                    if(inherits(res,"try-error")){
                        cat(paste("An error occurred while downloading data!!!\n")) 
                    }
                    
                    
                }
                
                # usuwanie roznicy w dodatkowych walutach w latach 2013 i 2014
                if(year==2013) {
                    ret <- ret[, !(colnames(ret) %in% c("1LTL","1LVL"))]

                }
                if(year==2014) {
                    ret <- ret[, !(colnames(ret) %in% c("1LTL"))]
                    
                }
                
                return(ret)
                
            }
            
            retData <- data.frame()
            
            for(i in from:to){
                retData <- rbind(retData, getNBPData(i))
            }
            
            retData$data <- as.Date(retData$data)
            
            return(retData)
            
        }
        return(data.frame())
        
    })
    
    # tabela interaktywna
    output$dataInterTable <- DT::renderDataTable({
        DT::datatable(  
            dataIn(), 
            rownames = FALSE,
            options = list(
                scrollX = TRUE,
                pageLength = 50,
                lengthMenu = seq(from=10,by=10,to=100)
            )
        )
    })
    
    # mapa
    output$countryMap <- renderGvis({
        
        # iso_3166-1_alpha-2
        countries <- data.frame(
            country = c("Andorra", "Australia", "Austria", "Belgium", "Brazil",
                        "Bulgaria", "Canada", "Chile", "China", "Croatia",
                        "Cyprus", "Czechia", "Denmark", "United Kingdom", "Estonia",
                        "Finland", "France", "Germany", "Grecce", "Hong Kong",
                        "Hungary", "Iceland", "India", "ID", "Ireland",
                        "Israel", "Italy", "Japan", "Latvia",
                        "Lithuania", "Luxemburg", "Malaysia", "Malta", "Mexico",
                        "Monaco", "Montenegro", "Netherlands", "New Zealand",
                        "Norway", "Philipines", "Portugal", "Romania",
                        "South Africa","RU", "Singapore", "Slovakia",
                        "Slovenia", "KR", "Spain", "Sweden", "Switzerland",
                        "Thailand", "Turkey", "Ukraine", "United States",
                        "WO"),
            
            currency = c("1EUR", "1AUD", "1EUR", "1EUR", "1BRL", "1BGN", "1CAD",
                         "100CLP", "1CNY", "1HRK", "1EUR", "1CZK", "1DKK",
                         "1GBP", "1EUR", "1EUR", "1EUR", "1EUR", "1EUR", "1HKD",
                         "100HUF", "100ISK", "100INR", "1000IDR", "1EUR",
                         "1ILS", "1EUR", "100JPY", "1EUR", "1EUR",
                         "1EUR", "1MYR", "1EUR", "1MXN", "1EUR", "1EUR", "1EUR",
                         "1NZD", "1NOK", "1PHP", "1EUR", "1RON", "1ZAR", "1RUB",
                         "1SGD", "1EUR", "1EUR", "100KRW", "1EUR", "1SEK",
                         "1CHF", "1THB", "1TRY", "1UAH", "1USD", "1XDR")
        )
        
        abc <- data.frame(countries)
        if (!input$currencyOne == "1XDR" & !input$currencyTwo == "1XDR"){
            abc <- filter(countries, currency %in% c(input$currencyOne,input$currencyTwo))
        }
        
        gvisGeoChart(abc, locationvar="country", colorvar="currency",
                     options=list(width=650, height=650))
    })
    
    #szereg czasowy
    output$seriesGraph <- renderPlotly({
        
        tmp <- data.frame(dataIn())
        
        currencyOne <- input$currencyOne
        currencyTwo <- input$currencyTwo
        one <- paste('X',currencyOne,sep="")
        two <- paste('X',currencyTwo,sep="")
        
        abc <- select(tmp, 'data', one, two)

        ggplot(abc, aes(x=data)) + geom_line(aes(y=get(one), colour=one)) + geom_line(aes(y=get(two), colour=two)) + scale_color_manual(name="Waluta", breaks=c(one, two), values=c('red', 'blue')) + xlab("Data") + ylab("Kurs waluty obcej wzgledem PLN") + ggtitle("Kursy walut obcych wzgledem PLN w czasie")

    })
    
    # histogram roznic
    output$diffHistogram <- renderPlotly({
        
        tmp <- data.frame(dataIn())
        
        currencyOne <- input$currencyOne
        currencyTwo <- input$currencyTwo
        one <- paste('X',currencyOne,sep="")
        two <- paste('X',currencyTwo,sep="")
        
        abc <- select(tmp, 'data', one, two)
        
        # jesli wybrana zostala ta sama waluta to uzyj do roznicy tej samej
        if (one == two) {
            colnames(abc)[2] <- "ONE"
            abc$diff <- abc$ONE - abc$ONE
        } else {
            colnames(abc)[2] <- "ONE"
            colnames(abc)[3] <- "TWO"
            abc$diff <- abc$ONE - abc$TWO
        }
        
        ggplot(abc, aes(x=diff)) + geom_histogram(bins=50) + xlab("Różnica") + ylab("Liczność") + ggtitle("Histogram różnicy kursów obu walut")
    })
    
    # regresja liniowa
    output$regression <- renderPrint({
        tmp <- data.frame(dataIn())
        
        currencyOne <- input$currencyOne
        currencyTwo <- input$currencyTwo
        one <- paste('X',currencyOne,sep="")
        two <- paste('X',currencyTwo,sep="")
        
        abc <- select(tmp, 'data', one, two)
        
        model <- lm(as.numeric(abc[[one]])~as.numeric(abc[[two]]), data=abc)
        summary(model)
    })
    
    output$testRainbow <- renderPrint({
        tmp <- data.frame(dataIn())
        
        currencyOne <- input$currencyOne
        currencyTwo <- input$currencyTwo
        one <- paste('X',currencyOne,sep="")
        two <- paste('X',currencyTwo,sep="")
        
        abc <- select(tmp, 'data', one, two)
        
        model <- lm(as.numeric(abc[[one]])~as.numeric(abc[[two]]), data=abc)
        raintest(model)
    })
    
    output$regressionGraph <- renderPlotly({
        
        tmp <- data.frame(dataIn())
        
        currencyOne <- input$currencyOne
        currencyTwo <- input$currencyTwo
        one <- paste('X',currencyOne,sep="")
        two <- paste('X',currencyTwo,sep="")
        
        abc <- select(tmp, 'data', one, two)
        
        ggplot(abc, aes(x = get(one), y = get(two))) + geom_point(colour="black") + stat_smooth(method = "lm", colour="red") + ylab(one) + xlab(two)
    })

    # generowanie raportu
   output$createReport <- downloadHandler(
       
       filename = "Raport.html",
       content = function(file) {
           tempReport <- file.path(tempdir(), "report.Rmd")
           file.copy("report.Rmd", tempReport, overwrite = TRUE)
           
           rmarkdown::render(tempReport, output_file = file,
                             params = list(mamona_one = input$currencyOne, 
                                           mamona_two = input$currencyTwo,
                                           dejta = dataIn()),
                             envir = new.env(parent = globalenv()))
       }
   )

})
