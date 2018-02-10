library(shiny)
library(shinythemes)

# ui.R

shinyUI(fluidPage(theme = shinytheme("lumen"),
        # Application title
        titlePanel("SwiftKey Capstone Project - Word Prediction App"),
         sidebarLayout(
         sidebarPanel(
        h4("Enter word to predict:"),
             textInput(inputId="text", label = ""),
             submitButton("Predict"),
        hr(),
        helpText(h5("Help Instruction:")),
        helpText(h6("1. Enter word in English and hit `Enter` or click on `PREDICT` button.", style="color:#7ba0af")),
        helpText(h6("2. The model will predict up to 5 next words based on what you entered." , style="color:#7ba0af")),
        helpText(h6("3. Click on the down arrow in the select box to see the predictions." , style="color:#7ba0af")),
        helpText(h6("4. Input a word from predictions to get the next predicted word." , style="color:#7ba0af")),
        helpText(h6("5. For details about the model, click 'Documentation' tab on the menu bar." , style="color:#7ba0af")),
        hr(),
       h6( a("Data Science Capstone (SwiftKey)", href="https://www.coursera.org/course/dsscapstone")),
        h6(p("Developed by: Nida Batt",  style = "color:#949696")), 
        h6(p("Last update: 11/17/2017",  style = "color:#949696"))
             ),
       
    
        mainPanel(
            tabsetPanel(
            tabPanel("Result", h3("Next word prediction"),hr(),
                         conditionalPanel(condition = "input.text != ''",
                         verbatimTextOutput("text"),
                         verbatimTextOutput("cleaned"), verbatimTextOutput("msg"),
                       h4( selectInput("predicts","Next word prediction choices :",choices=c("")))
                      # h4(textInput("predicts","Next word prediction choices :",choices=c("")))
                         )
                ),  
            
   tabPanel("Documentation", h3("Predicting the Next Word"),
p("Whenever the user of our Shiny web app types in something, the application suggests up to 5 words that it thinks would most likely follow what the user has typed in so far. It does so by searching its n-gram language model for words that most often follow the user's input words."),
    hr(),
h3("Background"),
    p("This report provides a preliminary research regarding SwiftKey's NLP project."),
    p("The SwiftKey's NLP project is aimed to implement NLP (Natural language processing) techniques to build an algorithm in R environment. The algorithm will predict the next word that the user will type most likely."),
    p("Tasks to be performed: Sampling, Data cleansing, Exploratory analysis, Predictive modelling."),
hr(),    
    h4("Product Development"),
p("The initial model was creatd with 30% datset using a systematic sampling process. This sample was cleaned by conversion to lowercase, removing punctuation, stemming, links, extra white spaces, numbers and all kinds of special characters. As a result, we got the clean corpus using `tm()` package."),
p("Next step was assembling tables of the most frequent,",a("n-grams (1-gram, 2-gram, 3-gram, and 4-gram).", href = "https://en.wikipedia.org/wiki/N-gram")),
p("Each data frame was sorted according to the frequency of n-grams (decreasing order)."),
p("Creating ngrams was the most challenging part of the project. With `RWeka()` package, the speed reduced drastically as the sample size increased."),
p(a("Finally,`ngram()` package was used to improve the efficiency of the model.", href = "https://cran.r-project.org/web/packages/ngram/ngram.pdf")),
 hr(),
h4("Model Algorithm"),
p(a("Katz's back-off model", href = "https://en.wikipedia.org/wiki/Katz's_back-off_model"),'was used for predicting the next word."To rank next-word candidates, we use a mechanism known as Stupid Backoff ← it always takes the prediction from the highest order n-gram, which its description follows:'),
p("1. It uses a 4-grams data frame; the first three words of which are the last three words of the sentence input by the user and that we are trying to predict the next words."),
p("The Fourgram data frame is already sorted from highest to lowest frequency."),
p("2. The entered words are matched with the all the 4-grams and the ones matching all the first 3 words, or 2 words are shown on the basis of frequency.The entered words are matched with the all the 4-grams and the ones matching all the first 3 words, or 2 words are shown on the basis of frequency."), 
p("3. If no 4-gram is found, we back off to 3-grams df (first two words of Trigram last two words of the sentence)."),
p("4. Recursivelly, If no 3-gram is found, we back off to 2-gram (first word of Bigram last word of the sentence)."),
p("Lastly, if no 2-gram is found, we back off to Onegram (the most common words with the highest frequency)."),
  hr(),
  h4("Limitations"),
  p("If the user digit more than three words, only the last three are considered as an initial parameter of predicting model function (because the largest data frame loaded by the app is a four-gram)"),
                     br(),
                     br()
                     )
                    )
                    )
                 )
                )
          )