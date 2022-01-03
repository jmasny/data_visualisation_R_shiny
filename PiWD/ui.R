#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(googleVis)
library(DT)
library(reshape)
library(knitr)
library(markdown)
library(rmarkdown)

shinyUI(fluidPage(

    titlePanel("Projekt PiWD - Notowania kursów walut"),

    sidebarLayout(
        
        sidebarPanel(
            
            sliderInput("setRange",
                        "Wybierz zakres lat dla analizowanych danych",
                        min = 2013,
                        max = 2021,
                        step = 1,
                        value = c(2015,2021),
                        sep = ""
            ),
            
            actionButton("getDataFromServer",
                         "Pobierz dane"
            ),
            
            selectInput("currencyOne", "Wybierz pierwszą walutę:",
                        list(`Afryka` = list("1ZAR"),
                             `Ameryka Płd.` = list("1BRL", "100CLP"),
                             `Ameryka Płn.` = list("1CAD", "1MXN", "1USD"),
                             `Australia i Ocenia` = list("1AUD", "1NZD"),
                             `Azja` = list("1CNY", "1HKD", "10000IDR", "1ILS",
                                           "100INR", "100KRW", "1MYR", "1PHP",
                                           "1RUB","1SGD", "1THB"),
                             `Europa` = list("1BGN", "1CHF", "1DKK", "1EUR", 
                                             "1GBP", "1HRK", "100HUF", "100ISK",
                                             "100JPY", "1NOK", "1RON", "1SEK",
                                             "1TRY", "1UAH"),
                             `Świat` = list("1XDR"))
            ),
            
            selectInput("currencyTwo", "Wybierz drugą walutę:",
                        list(`Afryka` = list("1ZAR"),
                             `Ameryka Płd.` = list("1BRL", "100CLP"),
                             `Ameryka Płn.` = list("1CAD", "1MXN", "1USD"),
                             `Australia i Ocenia` = list("1AUD", "1NZD"),
                             `Azja` = list("1CNY", "1HKD", "10000IDR", "1ILS",
                                           "100INR", "100KRW", "1MYR", "1PHP",
                                           "1RUB","1SGD", "1THB"),
                             `Europa` = list("1BGN", "1CHF", "1DKK", "1EUR", 
                                             "1GBP", "1HRK", "100HUF", "100ISK",
                                             "100JPY", "1NOK", "1RON", "1SEK",
                                             "1TRY", "1UAH"),
                             `Świat` = list("1XDR"))
            ),
            downloadButton("createReport", "Wygeneruj raport HTML")
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Tabela", DT::dataTableOutput("dataInterTable")),
                        
                        tabPanel("Mapa", htmlOutput("countryMap")),
                        
                        tabPanel("Szeregi", plotlyOutput("seriesGraph")),
                        
                        tabPanel("Histogram", plotlyOutput("diffHistogram")),
                        
                        tabPanel("Regresja liniowa", verbatimTextOutput("regression"),
                                 verbatimTextOutput("testRainbow"), 
                                 plotlyOutput("regressionGraph"))
            )
        )
    )
))
