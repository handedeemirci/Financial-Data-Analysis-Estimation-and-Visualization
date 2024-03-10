setwd("C:/Users/Hande/Desktop/R project")

source("Last code.R")


# list of tickers
tickers <- c("AAPL", "MSFT", "AMZN","PG", "NKE","JNJ","GE","IBM","KO","BA","XOM")
# tickers <- c("EEM", "AAPL", "AMZN", "GOOG", "TSLA", "JPM", "WMT", "VZ", "PG",
#              "MCD", "KO", "XOM", "GE", "IBM", "DIS", "BA", "CAT", "MSFT", "CSCO", "INTC")
# start and end date
start_date <- as.Date("2000-01-01")
end_date <- Sys.Date()



importData <- function(tickers, start_date, end_date) {
  stopifnot(all(is.character(tickers)))
  stopifnot(all(nchar(tickers) >= 2))
  stopifnot(all(grepl("^[A-Z]+$", tickers)))
  stopifnot(grepl("\\d{4}-\\d{2}-\\d{2}", start_date))
  stopifnot(grepl("\\d{4}-\\d{2}-\\d{2}", end_date))
  
  invisible(map(tickers, function(i) {
    getSymbols(i, src = "yahoo", from = start_date, to = end_date)
    stock <- stock$new(
      ticker = i,
      n_obs = length(as.Date(index(get(i)))),
      date = as.Date(index(get(i))),
      adjPrice = as.numeric(Cl(get(i))),
      logRet = toLogReturn(as.numeric(Cl(get(i))))
    )
    assign(i, stock, envir = globalenv())
  }))
}

importData(tickers,start_date = start_date,end_date = end_date)


# UI ----------------------------------------------------------------------

ui <- navbarPage("Stock Analysis with Shiny",
                 tabPanel("Choice of tickers and data",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("ticker", "Choose one or more ticker(s):",
                                            choices = tickers, multiple = TRUE),
                                radioButtons("priceType", "Choose what analyse between 'adjusted price' and 'logarithmic return':",
                                             choices = list("Adjusted Price" = "adjPrice",
                                                            "Logarithmic Return" = "logRet")),
                                dateInput("startDate", "Start Date:", value = "2000-01-01",max=Sys.Date()-150,min="2000-01-01"),
                                dateInput("endDate", "End Date:", value = "2023-05-05",max=Sys.Date())
                              ),
                              mainPanel(
                                verbatimTextOutput("summaryText"),
                                
                              )
                            )
                          )),
                 tabPanel("Descriptive Statistics",
                          fluidPage(
                            
                            splitLayout(
                              cellWidths = c("50%", "50%"),
                              
                              verbatimTextOutput("descrStatText"),
                              uiOutput("plotHist"),
                            )
                          )
                 ),
                 tabPanel("Stationarity",
                          fluidPage(
                            a("To learn more about stationarity and ADF test", 
                              href="https://en.wikipedia.org/wiki/Dickey%E2%80%93Fuller_test", 
                              target="_blank"), 
                            splitLayout(
                              cellWidths = c("50%", "50%"),
                              verbatimTextOutput("stationarityText"),
                              
                              uiOutput("plotAcfPacf"))
                            
                          )
                          
                 ),
                 tabPanel("ARIMA model",
                          fluidPage(
                            a("To learn more about ARIMA models", 
                              href="https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average", 
                              target="_blank"), 
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("pARIMA", "Order P:", value = 1, min = 0, max = 10, step = 1),
                                numericInput("dARIMA", "Order D:", value = 1, min = 0, max = 10, step = 1),
                                numericInput("qARIMA", "Order Q:", value = 1, min = 0, max = 10, step = 1)
                              ),
                              mainPanel(
                                verbatimTextOutput("arimaText"),
                                uiOutput("arimaPlot")
                              )
                            )
                          )),
                 tabPanel("GARCH model",
                          fluidPage(
                            a("To learn more about GARCH models", 
                              href="https://en.wikipedia.org/wiki/Autoregressive_conditional_heteroskedasticity", 
                              target="_blank"),
                            sidebarLayout(
                              sidebarPanel(
                                numericInput("pGARCH", "Order P:", value = 1, min = 1, max = 10, step = 1),
                                numericInput("qGARCH", "Order Q:", value = 1, min = 1, max = 10, step = 1)
                              ),
                              mainPanel(
                                verbatimTextOutput("garchText"),
                                uiOutput("garchPlot")
                              )
                            )
                          )),
                 tabPanel("VAR model",
                          fluidPage(
                            a("To learn more about VAR models", 
                              href="https://faculty.washington.edu/ezivot/econ584/notes/varModels.pdf", 
                              target="_blank"),
                            splitLayout(
                              cellWidths = c("40%", "60%"),
                              sidebarPanel(
                                numericInput("pVAR", "Order of VAR model:", value = 1, min = 1, max = 20, step = 1),
                                numericInput("naheadVAR", "Number of step ahead:", value = 10, min = 5, max = 30, step = 1)),
                              verbatimTextOutput("VARText")),
                            plotOutput("plotIRF"),
                            
                          )
                 ),
                 tabPanel("Portfolio analysis",
                          fluidPage(
                            sidebarPanel(
                              numericInput("retMark", "Choose an expected return:", value = 0.05, min = 0.01, max = 1, step = 0.01)),
                            mainPanel(
                              verbatimTextOutput("markText")),
                            plotOutput("plotMark"))
                          
                          
                 ),
                 tabPanel("Excel file",
                          fluidPage(
                            sidebarPanel(
                              fluidRow(
                                column(12,
                                       downloadButton("downloadData", label = "Download CSV")
                                )
                              )
                            ),
                            mainPanel(DT::dataTableOutput("tab"))
                          )
                 )
)





# SERVER ------------------------------------------------------------------

server <- function(input, output) {
  
  tick<- reactive({
    tickers <- input$ticker
  })
  pos1_reactive <- reactive({
    tickers=tick()
    if(length(tickers) != 0){
      which(get(tickers[1])$date == find_nearest_date(input$startDate, get(tickers[1])$date))
    }
  })
  
  pos2_reactive <- reactive({
    # tickers <- input$ticker
    tickers=tick()
    if(length(tickers) != 0){
      which(get(tickers[1])$date == find_nearest_date(input$endDate, get(tickers[1])$date))
    }
  })
  
  observe({
    pos1 <- pos1_reactive()
    pos2 <- pos2_reactive()
    
    
    
    
    tickers <- tick()
    price_type <- input$priceType
    orderARIMA <- c(input$pARIMA,input$dARIMA,input$qARIMA)
    orderGARCH <- c(input$pGARCH,input$qGARCH)
    pVAR <- input$pVAR
    
    dataRet=c()
    dataPrice=c()
    
    # dataDown=c()
    for(i in tickers){
      dataRet=cbind(dataRet,get(i)$logRet[pos1:pos2])
      dataPrice=cbind(dataPrice,get(i)$adjPrice[pos1:pos2])
      
    }
    
    colnames(dataRet)=c(tickers)
    colnames(dataPrice)=c(tickers)
    
    
    output$summaryText <- renderPrint({
      validate(
        need(input$ticker != "", "Please select at least one ticker."),
        need(pos1 < pos2, "Start date must be before end date.")
      )
      
      
    })
    ###inizio descriptive
    output$descrStatText<- renderPrint({
      cat("\n")
      cat("The following descriptive statistics are referred to the period",
          paste(get(tickers[1])$date[pos1]),"and",paste(get(tickers[1])$date[pos2]))
      cat("\n")
      
      invisible(map(tickers, function(i) {
        get(i)$descriptive_stat(get(i)[[price_type]][pos1:pos2], pos1, pos2)
        cat("\n")
      })
      )
    })
    
    output$plotHist<-renderUI({
      plot_output_list_hist <- lapply(input$ticker, function(ticke) {
        plotOutput(paste0("plotHist_", ticke))
      })
      do.call(tagList, plot_output_list_hist)
    })
    observe({
      lapply(input$ticker, function(ticke) {
        output[[paste0("plotHist_", ticke)]] <- renderPlot({
          get(ticke)$descriptive_stat(get(ticke)[[input$priceType]][pos1:pos2],pos1,pos2)
        })
      })
      
    })
    ###fine descriptive
    
    
    #inizio stationarity
    output$stationarityText<- renderPrint({
      cat("The stationarity control of",tickers[1],"is done using data from",
          paste(get(tickers[1])$date[pos1]),"to",paste(get(tickers[1])$date[pos2]))
      cat("\n\n")
      invisible(map(tickers, function(i) {
        get(i)$stationarity(get(i)[[price_type]][pos1:pos2])
        cat("\n\n")
      })
      )
      
    })
    output$plotAcfPacf <- renderUI({
      plot_output_list <- lapply(input$ticker, function(ticke) {
        plotOutput(paste0("plotAcfPacf_", ticke))
      })
      do.call(tagList, plot_output_list)
    })
    
    
    
    
    observe({
      lapply(input$ticker, function(ticke) {
        output[[paste0("plotAcfPacf_", ticke)]] <- renderPlot({
          print(get(ticke)$stationarity(get(ticke)[[input$priceType]][pos1:pos2]))
        })
      })
      
    })
    #### fine stationarity
    
    
    #### inizio Arima model
    output$arimaText <- renderPrint({
      
      cat("The data used for estimating the ARIMA model for the choosen tickers prices are referenced to the period:",
          paste(get(tickers[1])$date[pos1]),"and",paste(get(tickers[1])$date[pos2]))
      cat("\n\n")
      
      walk(tickers, function(i) {
        get(i)$arimaModel(pos1,pos2,orderARIMA)
        cat("\n\n")
      })
      
    })
    output$arimaPlot <- renderUI({
      # validate(
      #   need(input$priceType == "adjPrice", "Please change your previous choice if you want to fit the model.")
      # )
      plot_output_list <- lapply(input$ticker, function(ticke) {
        plotOutput(paste0("arimaPlot_", ticke))
      })
      do.call(tagList, plot_output_list)
    })
    
    
    observe({
      
      lapply(input$ticker, function(ticke) {
        output[[paste0("arimaPlot_", ticke)]] <- renderPlot({
          print(get(ticke)$arimaModel(pos1,pos2,orderARIMA))
        })
      })
      
      
    })
    #fine arima model
    
    #inizio garch model
    
    output$garchText<- renderPrint({
      # validate(
      #   need(input$priceType == "logRet", "Keep in mind that GARCH models are mainly built for returns.")
      # )
      cat("The data used for estimating the GARCH model for the chosen tickers logaritmic returns are referenced to the period:",
          paste(get(tickers[1])$date[pos1]),"and",paste(get(tickers[1])$date[pos2]))
      cat("\n\n")
      
      
      walk(tickers, function(i) {
        get(i)$garchModel(pos1,pos2,orderGARCH)
        cat("\n\n")
      })
      
    })
    output$garchPlot <- renderUI({
      # validate(
      #   need(input$priceType == "logRet", "Please change your previous choice if you want to fit the model.")
      # )
      plot_output_list <- lapply(input$ticker, function(ticke) {
        plotOutput(paste0("garchPlot_", ticke))
      })
      do.call(tagList, plot_output_list)
    })
    
    
    observe({
      lapply(input$ticker, function(ticke) {
        output[[paste0("garchPlot_", ticke)]] <- renderPlot({
          print(get(ticke)$garchModel(pos1,pos2,orderGARCH))
        })
      })
      
    })#fine garch
    
    
    #inizio VAR
    output$VARText<- renderPrint({
      validate(need(length(input$ticker) > 1, "Please select MORE than one ticker in order to fit a VAR model."))
      get(tickers[1])$VARModel(dataRet,p=input$pVAR,input$naheadVAR)
      cat("\n")
    })
    output$plotIRF<- renderPlot({
      print(get(tickers[1])$VARModel(dataRet,p=input$pVAR,input$naheadVAR)[[1]])
    })#fine VAR
    
    
    
    #inizio markovitz
    output$markText<- renderPrint({
      validate(need(length(input$ticker) > 1, "Please select MORE than one ticker in order to fit a VAR model."))
      get(tickers[1])$portfolioAnalysis(dataRet,input$retMark)
      
    })
    output$plotMark<- renderPlot({
      print(get(tickers[1])$portfolioAnalysis(dataRet,input$retMark))
    })#fine portfolio
    
    
    #download
    output$tab <- DT::renderDataTable({
      if(price_type=="adjPrice"){
        dataDown=cbind(as.character.Date(get(tickers[1])$date[pos1:pos2]),dataPrice)
      }else{
        dataDown=cbind(as.character.Date(get(tickers[1])$date[pos1:pos2]),dataRet)
      }
      colnames(dataDown)=c("Date",tickers)
      print(dataDown)
      
    }) 
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        if(price_type=="adjPrice"){
          dataDown=cbind(as.character.Date(get(tickers[1])$date[pos1:pos2]),dataPrice)
        }else{
          dataDown=cbind(as.character.Date(get(tickers[1])$date[pos1:pos2]),dataRet)
        }
        colnames(dataDown)=c("Date",tickers)
        write.csv(dataDown, file,row.names = FALSE)
      }
    )
    
    
  })#end of 1st observe
  
}



shinyApp(ui, server)



