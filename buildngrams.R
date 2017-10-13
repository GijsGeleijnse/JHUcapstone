library(quanteda)
library(dplyr)
library(data.table)
library(RWeka)
library(tm)
library(stringr)

# http://rstudio-pubs-static.s3.amazonaws.com/136810_7a06cc571b01432b855f1b13b0f82195.html

sample <- NULL
test_file <-NULL

createSampleAndTest <- function(){
  
  b <- getFile("C:/Users/GGe1706.17794/Documents/GitHub/Capstone/final/en_US/en_US.blogs.txt")
  w <- which(rbinom(length(b),1,0.001)%in% 1)
  test_file <- b[w]
  b <- b[-w]
  n <- getFile("C:/Users/GGe1706.17794/Documents/GitHub/Capstone/final/en_US/en_US.news.txt")
  w <- which(rbinom(length(n),1,0.001)%in% 1)
  test_file <- c(test_file,b[w])
  n <- n[-w]
  t <- getFile("C:/Users/GGe1706.17794/Documents/GitHub/Capstone/final/en_US/en_US.twitter.txt")
  w <- which(rbinom(length(t),1,0.001)%in% 1)
  test_file <<- c(test_file,b[w])
  t <- t[-w]
  sample <<- c(b,n,t)
  write(sample,"sample.txt")
  write(test_file,"test_file.txt")
}


batchNgrams <- function(){
  str <- ""
  ptmTidy <- proc.time()
  buildNgrams(sample,0.02,filter_1=FALSE,max_n=2,fileout="gram2_p002_nofilter.csv")
  ptmTidy <- proc.time() - ptmTidy
  str <- c(str,paste("gram2_p002_nofilter",ptmTidy,"\n")) 
  print("done!")
    ptmTidy <- proc.time()
  buildNgrams(sample,0.2,filter_1=FALSE,max_n=2,fileout="gram2_p02_nofilter.csv")
  ptmTidy <- proc.time() - ptmTidy
  str <- c(str,paste("gram2_p02_nofilter",ptmTidy,"\n"))
  print("done!")
   ptmTidy <- proc.time()
  buildNgrams(sample,0.2,filter_1=FALSE,max_n=3,fileout="gram3_p02_nofilter.csv")
  ptmTidy <- proc.time() - ptmTidy
  str <- c(str,paste("gram3_p02_nofilter",ptmTidy,"\n"))
  print("done!")
  ptmTidy <- proc.time()
  buildNgrams(sample,0.2,filter_1=TRUE,max_n=3,fileout="gram3_p02_withfilter.csv")
  ptmTidy <- proc.time() - ptmTidy
  
  str <- c(str,paste("gram3_p02_withfilter.csv",ptmTidy,"\n"))
  print("done!")
  ptmTidy <- proc.time()
  buildNgrams(sample,0.2,filter_1=TRUE,max_n=5,fileout="gram5_p02_withfilter.csv")
  ptmTidy <- proc.time() - ptmTidy
  
  str <- c(str,paste("gram5_p02_withfilter.csv",ptmTidy,"\n"))
  print("done!")
  ptmTidy <- proc.time()
  buildNgrams(sample,0.2,filter_1=FALSE,max_n=5,fileout="gram5_p02_nofilter.csv")
  ptmTidy <- proc.time() - ptmTidy
  
  str <- c(str,paste("gram5_p02_nofilter.csv",ptmTidy,"\n"))
  print("done!")
  
   write(str,"building_times.txt")
  
}




buildTrumpModel <- function(){
  sample <- getFile("trumptweets.txt") # taken from http://www.trumptwitterarchive.com/archive
  buildNgrams(sample,1,FALSE,5,"trumpgrams.csv")
}
  


buildNgrams <- function(sample, p, filter_1=FALSE, max_n=5, fileout="C:/Users/GGe1706.17794/Documents/GitHub/Capstone/sample25_blogs.csv"){
  w <- which(rbinom(length(sample),1,p)%in% 1 )
  sample <- sample[w]
  sample <- str_replace_all(sample,"[^[:graph:]]", " ") # a patch to deal with nasty characters https://stackoverflow.com/questions/9637278/r-tm-package-invalid-input-in-utf8towcs
  sample <- CleanCorpusWeka(sample)
  sample <- lapply(sample,as.character)
  sample <- unlist(sample)
  sample <- tolower(sample)
  bs <- tokenize(sample, remove_numbers = TRUE, remove_symbols = TRUE, remove_punct = TRUE, remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, what="word")
  
  
  dt<-NULL
  for(i in 2:max_n){
    df3 <- as.data.frame(table(unlist(tokens_ngrams(bs, n=i))))
    if(filter_1){df3 <- filter(df3,df3$Freq >1)}
    df3 <- mutate(df3, Var1 = as.character(Var1))
    given <- sapply(df3$Var1,grabFirst)
    predict <- sapply(df3$Var1,grabLast)
    df3$given <- given
    df3$predict <- predict
    df3 <- df3[,c(3,4,2)]
    
    # only store top 3 
    df3 <- df3 %>%
           group_by(given) %>%
           top_n(n=3,wt=Freq)
    
    df3 <- arrange(df3, given, desc(Freq))
    
    df3 <- data.table(df3)
    dt <- rbind(dt,df3)
  }
  
  write.csv(dt,fileout)
  }

buildTestFile <- function(tf, fileout){
  tf <- CleanCorpusWeka(tf)
  tf <- lapply(tf,as.character)
  tf <- unlist(tf)
  tf <- tolower(tf)
  bs <- tokenize(tf, remove_numbers = TRUE, remove_symbols = TRUE, remove_punct = TRUE, remove_separators = TRUE, remove_twitter = TRUE, remove_hyphens = TRUE, what="word")
  df3 <- as.data.frame(table(unlist(tokens_ngrams(bs, n=5))))
  #df3 <- filter(df3,df3$Freq >1)
  df3 <- mutate(df3, Var1 = as.character(Var1))
  given <- sapply(df3$Var1,grabFirst)
  predict <- sapply(df3$Var1,grabLast)
  df3$given <- given
  df3$predict <- predict
  df3 <- df3[,c(3,4,2)]
  write.csv(df3,fileout)
  
}


getFile <- function(filename){
  con <- file(filename, "r") 
  t <- readLines(con) #,  encoding = "UTF-8")
  close(con)
  #t <-iconv(t, "ASCII", "UTF-8", sub="")
  return(t)
}

getFileP <- function(n = NULL,p = 1, filename) {
  
  con <- file(filename, "r") 
  if(!is.null(n))
  {t <- readLines(con, n,  encoding = "UTF-8")} ## Read in the next 5 lines of text 
  else
  {t <- readLines(con,  encoding = "UTF-8")}
  close(con) ## 
  w <- which(rbinom(length(t),1,p)%in% 1 )
  t[w]}

SplitTrainTestP <- function(p, l) {
  
  w <- which(rbinom(l,1,p)%in% 1 )
  return(w)
}



CleanCorpusWeka <- function(corpus){
  corpus <- VCorpus(VectorSource(corpus))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  return(corpus)
}


grabLast <- function(str){
  s <- strsplit(str,split="_")
  return(s[[1]][length(s[[1]])])
  
}

grabFirst <- function(str){
  s <- strsplit(str,split="_")
  l <- nchar(s[[1]][length(s[[1]])])
  return(substr(str,1,nchar(str)-l-1))
}
