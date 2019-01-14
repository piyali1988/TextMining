# options(java.parameters = "- Xmx1024m")
# .libpaths("U:/das22/3.4/")
library(readstata13)
library(NLP)
library(openNLP)
library(XLConnect)

wb = loadWorkbook('W:/proj/g8/data_2018/preliminary data/G8_OpenEnds1.xlsx')
df = readWorksheet(wb, sheet = "O_E8M1", header = TRUE)

extractChunks <- function(x) {
  x <- as.String(x)
  x <- tolower(x)
  
  ## For phrases less than 3 words, keep the phrase
  if(sapply(gregexpr("\\W+", x), length) < 4){
    x <- gsub('[[:punct:] ]+',' ',x)
    return (x)
    gc()
  }
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  tokenizedAndTagged <- data.frame(Tokens = x[POSwords], Tags = tags)
  
  tokenizedAndTagged$Tags_mod = grepl("NN|JJ|VB|CD", tokenizedAndTagged$Tags)
  chunk = vector()
  chunk[1] = as.numeric(tokenizedAndTagged$Tags_mod[1])
  
  for (i in 2:nrow(tokenizedAndTagged)) {
    if(!tokenizedAndTagged$Tags_mod[i]){  #False i.e.Not tagged
      chunk[i] = 0
    } else if (tokenizedAndTagged$Tags_mod[i] == tokenizedAndTagged$Tags_mod[i-1]) {
      chunk[i] = chunk[i-1]
    } else {
      chunk[i] = max(chunk) + 1
    }
  }
  text_chunk <- split(as.character(tokenizedAndTagged$Tokens), chunk)
  tag_pattern <- split(as.character(tokenizedAndTagged$Tags), chunk)
  names(text_chunk) <- sapply(tag_pattern, function(x) paste(x, collapse = "-"))
  
  # Extract chunks matching pattern
  res = text_chunk[grepl("NN.-VB.|JJ-NN|NN.-NN|NN|VB.-JJ|JJ.|VB|VBG", names(text_chunk))]
  res = sapply(res, function(x) paste(x, collapse =  " "))
  return(res)
  gc()
}

text <- df$O_E8M1
vec <- list()
# vec <- apply(text,1,function(x)do.call(extractChunks, as.list(x)))
for(i in 1:length(text)){
    vec[[i]] <- extractChunks(text[i])
}

