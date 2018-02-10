library(tm)
library(ngram)
MAX_N_GRAMS = 3
TOP_FREQ = 5
getwd()
dirname <-  '~/Desktop/coursera/capstone project/Swiftkey'
if (!dir.exists(dirname))dir.create(dirname,recursive=TRUE)
#setwd("~/Desktop/coursera/capstone project/Swiftkey")
# read data files to configure the app
df2 <- readRDS("bigrams.RDATA")
df3 <- readRDS("trigrams.RDATA")
df4 <- readRDS("qardgrams.RDATA")

msg <- ""

# these functions makes cleansing in input parameter

removeWhiteSpaces <- function(str) {
    return( gsub("\\s+"," ",str) )
}

# dataframe 'profanity' was previosly saved
#prof <- read.csv("profanity.txt", header=FALSE, stringsAsFactors=FALSE)
#head(prof)
#saveRDS(prof,"profanity.RDATA")

removeProfanity <- function(str) {
    dfProf <- readRDS("profanity.RDATA")
    nWords <- nrow(dfProf)
    for(i in 1:nWords) 
        str <- gsub(dfProf[i,1],"*",str, perl=TRUE) 
    return(str)
}

clean <- function(input) {
    input <- removePunctuation(input)
    input <- removeNumbers(input)
    input <- removeWhiteSpaces(input)
    #input <- rev(unlist(strsplit(input," ")))
    input <- tolower(input) 
    input <- removeProfanity(input)
}

# this function acts as this example:
# get_last_words( "If I want to only the last three words it returns this way", 3 ) ==> "returns this way"

get_last_words <- function(lst, n) {
    x <- as.data.frame(strsplit(lst, " "))
    colnames(x) <- c("words")
    len <- nrow(x)
    if(len<n) n<-len 
    lst <- paste(x[ (len-(n-1)) : len ,], collapse=" ")
    return(lst)
}

# some heuristcs applied before returning to the server interface

contractions <- function(token) {
    token <- gsub("^s\\s","\'s ",token)
    token <- gsub("^n\\s","\'n ",token)
    token <- gsub("^d\\s","\'d ",token)
    token <- gsub("^t\\s","\'t ",token)
    token <- gsub("^ve\\s","\'ve ",token)
    token <- gsub("^ll\\s","\'ll ",token)
    token <- gsub("^re\\s","\'re ",token)
    return(token)
}

# collect the top frequent words from unigram table and return in a vector

get_top <- function(n=TOP_FREQ) {
    df1 <- readRDS("unigrams.RDATA")
    nrows <- nrow(df1)
    vTop <- vector("list", length=25)
    for(i in seq_len(25))
        vTop[i] <- as.character(df1[i,1])
    return(head(vTop,n))
}
# the objective of this function is find the most probable word in a n-gram dataframe , given a group of n last words

find_in_grams <- function(lastW, n) {
    lastW <- paste0('^',lastW,' ')
    
    # points the search to correspondent 'n-gram' dataframe 
    dfSearch <- data.frame()
    
    # subset 'n-gram' dataframe to find the most probable occurrences 
    if(n==3)
        dfsub <- subset(df4, grepl(lastW, df4$ngrams))
    else
        if(n==2)
            dfsub <- subset(df3, grepl(lastW, df3$ngrams))
    else
        if(n==1)
            dfsub <- subset(df2, grepl(lastW, df2$ngrams))
    
    #print(head(dfsub)); print(nrow(dfsub))
    
    if(nrow(dfsub) > 0) {
        # if matches, return the 5 (or TOP_FREQ) top Words (without any spaces)
        top5words <- head(contractions(gsub(lastW,"",dfsub$ngrams)),TOP_FREQ)
        #top5words <- heuristics(top5words)
        msg <<- sprintf("Next word was predicted with %1d-gram dataframe.",(n+1))
        return( gsub("[[:space:]]"," ",top5words) )
    }
    else{
        n <- n-1;
        if(n > 0) { 
            lastW <- substr(lastW,2,nchar(lastW))
            find_in_grams( get_last_words(lastW,2), n )
        }
        else {
            lastW <- substr(lastW,2,nchar(lastW))
            msg <<- paste("Next word not found in 2, 3 or 4-grams dataframes.\nReturning the",TOP_FREQ,"most frequent words of uni-gram.")
            return(get_top(TOP_FREQ))
        }
    }
}

predict_model <- function(user_input) {
    return( find_in_grams( get_last_words(user_input, MAX_N_GRAMS), n=MAX_N_GRAMS) )
}