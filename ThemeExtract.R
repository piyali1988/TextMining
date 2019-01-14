## vec is already here
# 
# install.packages("tm")
# install.packages("SnowballC")
library(tm)
library(SnowballC)

nw_vec <- vec
med <- c('doctor', 'dr','pulmonari','medic','depress','anxieti','hear','diseas','healthcar','childcare', 'cancer', 
         'recoveri','chemo','therapi','chemotherapi','eye', 'teeth','hip','knee','leg','surgeri','coverag','injuri',
         'sick','sleep','spasm','pain','ache','rehab','ssi','handicap','disabl','social security','meds','joint','arthriti')
preg_care <- c('pregnanc','pregnant','care','help','child','birth','childcar','kid','mother','baby', 'caretaker',
               'matern','homemak','stay at home','mom','caregiv','babysitt','help','family','son','daughter','grand',
               'grandson','grandchildren','grandparents','granddaughter','father','wife','husband','dad','care')
school <- c('student','full time student','starting school','school', 'educ','graduat','recent graduate','studi','degre')
unemp <- c('unemploy')
age <- c('old','age')
abuse <- c('addict','drug','alcohol','abuse')
age_cond <- c('month','year','under','below')
flag <- 0

for(i in 1:nrow(df)){
  x <- unlist(nw_vec[[i]])
  x <- unname(x)
  m <- strsplit(x," ")
  m <- unlist(m)
  ## For extracting digits out of the phrase
  num <- unlist(regmatches(m, gregexpr("[[:digit:]]+", m)))
  if(length(num) >= 1){
    for(j in 1:length(num)){
      if(as.numeric(num[j]) >= 50){
        flag <- 1
      }else
        flag <- 99
    }
  }
  m <- wordStem(m,language = "english")
  
  j <- m %in% med
  k <- m %in% preg_care 
  l <- m %in% school
  x <- m %in% unemp
  y <- m %in% age
  z <- m %in% abuse
  cond <- m %in% age_cond

  if(any(j)){
    df$med[i] <- 1
  }else {
    df$med[i] <- 0
  }
  if(any(k)){
    df$pregCare[i] <- 1
  }else {
    df$pregCare[i] <- 0
  }
  if(any(l)){
    df$school[i] <- 1
  }else{
    df$school[i] <- 0
  }
  if(any(x)){
    df$unemp[i] <- 1
  }else {
    df$unemp[i] <- 0
  }
  if(any(z)){
    df$abuse[i] <- 1
  }else {
    df$abuse[i] <- 0
  }
  ## Age category calculation
  if(flag == 1){ # if 50 years and older
    df$age[i] <- 1
    flag <- 0
  }else if(any(y)){   # if keyword 'old' and 'age' matches
    if(any(cond) && flag == 99){   # if digit less than 50 and has keyword months or years
      df$pregCare[i] <- 1      # assign to caregiver
      flag <- 0
    }else{
      df$age[i] <- 1
    }
  }else {
    df$age[i] <- 0
  }
  
  ## If both medical care and caretaking is checked true, consider only care taking
  if(df$med[i] == 1 && df$pregCare[i] == 1){
    df$med[i] <- 0
  }
  
}
## Create a new column that stores a boolean 0/1 if no match found in existig categories, miscl is 1, else 0
df$miscl <- ifelse((df$med | df$pregCare | df$school | df$unemp | df$age | df$abuse), 0,1)

## Plot bargraphs to display bins
par(mar=c(11,4,4,2))
df1 <- c('medical','pregnancy / caregiving', 'school','unemployment','abuse', 'age', 'miscellaneous')
df2 <- c(as.numeric(nrow(df[df$med==1,])),nrow(df[df$pregCare==1,]),nrow(df[df$school==1,]),nrow(df[df$unemp==1,]),nrow(df[df$abuse,]),nrow(df[df$age,]),nrow(df[df$miscl,]))
barplot(df2,names.arg = df1, cex.names = .75, las =2)

