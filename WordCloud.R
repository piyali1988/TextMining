install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("readstata13")
install.packages("NLP")
install.packages("openNLP")
library("NLP")
library("openNLP")
library(tm)
library(SnowballC)
library(wordcloud)
library(readstata13)

jeopQ <- read.dta13('W:/proj/g8/data_2016/Survey and linked files/open-ended only.dta', convert.dates = TRUE, convert.factors = TRUE)
wb = loadWorkbook('W:/proj/g8/data_2018/preliminary data/G8_OpenEnds1.xlsx')
df = readWorksheet(wb, sheet = "O_E8M1", header = TRUE)

jeopCorpus <- Corpus(VectorSource(df$O_E8M1))
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus <- tm_map(jeopCorpus, content_transformer(tolower))
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, stemDocument)

wordcloud(jeopCorpus, max.words = 100, random.order=FALSE)

dtm <- TermDocumentMatrix(jeopCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 50)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


#extractChunks(text_col)