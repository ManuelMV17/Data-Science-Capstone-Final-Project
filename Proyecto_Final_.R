library(rJava)
library(RWeka)
library(tm)

con <- file("C:/Users/Manue/Desktop/en_US/en_US.blogs.txt", open = "r")
con2 <- file("C:/Users/Manue/Desktop/en_US/en_US.news.txt", open = "r")
con3 <- file("C:/Users/Manue/Desktop/en_US/en_US.twitter.txt", open = "r")

blogs <- readLines(con)
close(con)

noticias <- readLines(con2)
close(con2)

twitter <- readLines(con3)
close(con3)

set.seed(976431)

blogs1 <- iconv(blogs, "latin1", "ASCII", sub="")
noticias1 <- iconv(noticias, "latin1", "ASCII", sub="")
twitter1 <- iconv(twitter, "latin1", "ASCII",sub="")

muestrear_datos <- c(sample(blogs, length(blogs)*0.001), 
                     sample(noticias, length(noticias)*0.001), 
                     sample(twitter, length(twitter)*0.001))

corpus_general <- VCorpus(VectorSource(muestrear_datos))
corpus_general1 <- tm_map(corpus_general, removePunctuation)
corpus_general2 <- tm_map(corpus_general1, stripWhitespace)
corpus_general3 <- tm_map(corpus_general2, tolower)
corpus_general4 <- tm_map(corpus_general3, removeNumbers)
corpus_general5 <- tm_map(corpus_general4, PlainTextDocument)
corpus_general6 <- tm_map(corpus_general5, removeWords, stopwords("english"))

resultado_corpus <- data.frame(text = unlist(sapply(corpus_general6, '[', "content")), stringsAsFactors = F)

uno <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
dos <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tres <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
cuatro <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4 ))

tdm_Ngram <- function (textcp, n) {
    NgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))}
    tdm_ngram <- TermDocumentMatrix(textcp, control = list(tokenizer = NgramTokenizer))
    tdm_ngram
}

uno_gram <- tdm_Ngram(corpus_general6, 1)
dos_gram <- tdm_Ngram(corpus_general6, 2)
tres_gram <- tdm_Ngram(corpus_general6, 3)
cuatro_gram <- tdm_Ngram(corpus_general6, 4)




ngram_sorted_df <- function (tdm_ngram) {
    tdm_ngram_m <- as.matrix(tdm_ngram)
    tdm_ngram_df <- as.data.frame(tdm_ngram_m)
    colnames(tdm_ngram_df) <- "Count"
    tdm_ngram_df <- tdm_ngram_df[order(-tdm_ngram_df$Count), , drop = TRUE]
    tdm_ngram_df
}

cuatro_acomodado <- ngram_sorted_df(cuatro_gram)
tres_acomodado <- ngram_sorted_df(tres_gram)
dos_acomodado <- ngram_sorted_df(dos_gram)
uno_acomodado <- ngram_sorted_df(uno_gram)
head(cuatro_acomodado)

cuatroGram <- data.frame(rows = rownames(cuatro_gram), count = cuatro_acomodado$Count)
cuatroGram$rows <- as.character(cuatroGram$rows)
cuatroGram_dividir <- strsplit(as.character(cuatroGram$rows), split = " ")
cuatroGram <- transform(cuatroGram, first = sapply(cuatroGram_dividir, "[[", 1),
                        second = sapply(cuatroGram_dividir, "[[", 2),
                        third = sapply(cuatroGram_dividir, "[[", 3),
                        fourth = sapply(cuatroGram_dividir, "[[", 4))
cuatroGram <- data.frame(unigram = cuatroGram$first, bigram = cuatroGram$second,
                         trigram = cuatroGram$third, quadgram = cuatroGram$fourth,
                         freq = cuatroGram$count, stringsAsFactors = F)
write.csv(cuatroGram[cuatroGram$freq > 1, ], "C:/Users/Manue/Desktop/en_US/Proyecto_Final/cuatroGram.csv",
          row.names = FALSE)
cuatroGram <- read.csv("C:/Users/Manue/Desktop/en_US/Proyecto_Final/cuatroGram.csv", stringsAsFactors = F)
saveRDS(cuatroGram, "C:/Users/Manue/Desktop/en_US/Proyecto_Final/cuatroGram.RData")

tresGram <- data.frame(rows = rownames(tres_gram), count = tres_acomodado$Count)
tresGram$rows <- as.character(tresGram$rows)
tresGram_dividir <- strsplit(as.character(tresGram$rows), split = " ")
tresGram <- transform(tresGram, first = sapply(tresGram_dividir, "[[", 1),
                      second = sapply(tresGram_dividir, "[[", 2),
                      third = sapply(tresGram_dividir, "[[", 3))
tresGram <- data.frame(unogram = tresGram$first, dosgram = tresGram$second,
                       trigram = tresGram$third, 
                       freq = tresGram$count, stringsAsFactors = F)
write.csv(tresGram[tresGram$freq > 1, ], "C:/Users/Manue/Desktop/en_US/Proyecto_Final/tresGram.csv", row.names = F)
tresGram <- read.csv("C:/Users/Manue/Desktop/en_US/Proyecto_Final/tresGram.csv", stringsAsFactors = F)
saveRDS(tresGram, "C:/Users/Manue/Desktop/en_US/Proyecto_Final/tresGram.RData")

dosGram <- data.frame(rows = rownames(dos_gram), count = dos_acomodado$Count)
dosGram$rows <- as.character(dosGram$rows)
dosGram_dividir <- strsplit(as.character(dosGram$rows), split = " ")
dosGram <- transform(dosGram, first = sapply(dosGram_dividir, "[[", 1),
                     second = sapply(dosGram_dividir, "[[", 2))
dosGram <- data.frame(unogram = dosGram$first, bigram = dosGram$second,
                      freq = dosGram$count, stringsAsFactors = F)
write.csv(dosGram[dosGram$freq > 1, ], "C:/Users/Manue/Desktop/en_US/Proyecto_Final/dosGram.csv", row.names = F)
dosGram <- read.csv("C:/Users/Manue/Desktop/en_US/Proyecto_Final/dosGram.csv", stringsAsFactors = F)
saveRDS(dosGram, "C:/Users/Manue/Desktop/en_US/Proyecto_Final/dosGram.RData")