suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))

# Load Quadgram,Trigram & Bigram Data frame files

cuatroGram <- readRDS("cuatroGram.RData")
tresGram <- readRDS("tresGram.RData")
dosGram <- readRDS("dosGram.RData")
mensaje <<- ""

# Cleaning of user input before predicting the next word

Predict <- function(x) {
    xclean <- removeNumbers(removePunctuation(tolower(x)))
    xs <- strsplit(xclean, " ")[[1]]
    

    
    if (length(xs)>= 3) {
        xs <- tail(xs,3)
        if (identical(character(0),head(cuatroGram[cuatroGram$unigram == xs[1] & cuatroGram$bigram == xs[2] & cuatroGram$trigram == xs[3], 4],1))){
            Predict(paste(xs[2],xs[3],sep=" "))
        }
        else {mensaje <<- "Using a 4-gram to predict the next word. "; head(cuatroGram[cuatroGram$unigram == xs[1] & cuatroGram$bigram == xs[2] & cuatroGram$trigram == xs[3], 4],1)}
    }
    else if (length(xs) == 2){
        xs <- tail(xs,2)
        if (identical(character(0),head(tresGram[tresGram$unigram == xs[1] & tresGram$bigram == xs[2], 3],1))) {
            Predict(xs[2])
        }
        else {mensaje<<- "Using a 3-gram to predict the next word. "; head(tresGram[tresGram$unigram == xs[1] & tresGram$bigram == xs[2], 3],1)}
    }
    else if (length(xs) == 1){
        xs <- tail(xs,1)
        if (identical(character(0),head(dosGram[dosGram$unigram == xs[1], 2],1))) {mensaje<<-"No matches. The most used word 'the' will be used. "; head("the",1)}
        else {mensaje <<- "Using a 2-gram to predict the next word. "; head(dosGram[dosGram$unigram == xs[1],2],1)}
    }
}


shinyServer(function(input, output) {
    output$prediction <- renderPrint({
        result <- Predict(input$inputString)
        output$text2 <- renderText({mensaje})
        result
    });
    
    output$text1 <- renderText({
        input$inputString});
}
)