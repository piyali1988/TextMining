library(readstata13)
library(NLP)
library(openNLP)
library(XLConnect)
library(tm)
library(SnowballC)

wb <- loadWorkbook("W:/proj/g8/staff/das22/E8_Misc_Stratify_Piyali.xlsx")
df <- readWorksheet(wb, sheet = "E8MiscResponses", header = TRUE)

extractChunks <- function(x) {
  x <- as.String(x)
  x <- tolower(x)
  x <- gsub("[[:punct:]]+", "",x)
  
  ## For phrases less than 3 words, keep the phrase
  if(sapply(gregexpr("\\W+", x), length) < 4){
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

text <- df$Responses
vec <- list()
# vec <- apply(text,1,function(x)do.call(extractChunks, as.list(x)))
for(i in 1:length(text)){
  vec[[i]] <- extractChunks(text[i])
}

trans <- c('transport','vehicl', 'car','movement','mobil','drive','remot','locat','distanc')
criminal <- c('crimin','crime','feloni','prison','polic','record','background','convict','violat','ticket','arrest')
nojob <- c('found','home','look','search','hire','cannot','find','seek')
terminate <- c('laid','lay','off','let', 'go','termin','down')


for(i in 1:nrow(df)){
  x <- unlist(vec[[i]])
  x <- unname(x)
  m <- strsplit(x," ")
  m <- unlist(m)

  m <- wordStem(m,language = "english")
  
  j <- m %in% trans
  k <- m %in% criminal 
  l <- m %in% nojob
  o <- m %in% terminate
  
  if(any(j)){
    df$trans[i] <- 1
  }else {
    df$trans[i] <- 0
  }
  if(any(k)){
    df$criminal[i] <- 1
  }else {
    df$criminal[i] <- 0
  }
  if(any(l)){
    df$nojob[i] <- 1
  }else{
    df$nojob[i] <- 0
  }
  if(any(o)){
    df$terminate[i] <- 1
    for(iterator in 1:length(m)){
      if((m[iterator] == "let" && m[iterator+1] != "go")){
        df$terminate[i] <- 0
      }
    }
    
  }else {
    df$terminate[i] <- 0
  }


  
}
## Create a new column that stores a boolean 0/1 if no match found in existig categories, miscl is 1, else 0
df$miscl <- ifelse((df$trans | df$criminal | df$nojob | df$terminate), 0,1)

## Plot bargraphs to display bins
par(mar=c(11,4,4,2))
df1 <- c('Transport','Criminal', 'No job','Terminate','Miscellaneous')
df2 <- c(as.numeric(nrow(df[df$trans==1,])),nrow(df[df$criminal==1,]),nrow(df[df$nojob==1,]),nrow(df[df$terminate==1,]),nrow(df[df$miscl,]))
barplot(df2,main = "Histogram of Non exempted reasons for not working (E8)", 
        # xlab = "Reasons for not working",
        ylab = "# of respondents",
        names.arg = df1, 
        cex.names = .75, las =2)
