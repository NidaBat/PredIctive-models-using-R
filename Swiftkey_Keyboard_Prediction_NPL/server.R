
library(shiny)
#setwd("~/Desktop/coursera/Capstone Project")
#readRDS("output/unigrams.RData",.GlobalEnv)
#readRDS("output/bigrams.RData",.GlobalEnv)
#readRDS("output/trigrams.RData",.GlobalEnv)
#readRDS("output/quardgrams.RData",.GlobalEnv)
source("predict_model.R")
#rsconnect::showLogs() 

server <- function(input, output, session) {
    
    output$text <- renderText({
        paste("Your input text is:", input$text)
    })
    
    observe({
        iniTime <- Sys.time()
        
        textCleansed <- clean(input$text)
        if(textCleansed != " ") 
        {
            output$cleaned <- renderText({
                paste0("Text after cleaning is: [",textCleansed,"]")
            })
            
            textCleansed <- gsub(" \\* "," ",textCleansed)    # not taking account of profanity terms
            predictWords <- predict_model(textCleansed)
            updateSelectInput(session = session, inputId = "predicts", choices = predictWords)
            #updateTextInput(session = session, inputId = "predicts", choices = predictWords)
            
            endTime <- Sys.time()
            output$msg <- renderText({
                paste(msg, "\n", sprintf("- Total time processing = %6.3f msecs",1000*(endTime-iniTime)))
            })
            gc()
        }  
    })
}

