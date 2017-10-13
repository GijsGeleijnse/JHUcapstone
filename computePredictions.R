library(sqldf)
library(data.table)
library(dplyr)



#my_table <- fread("sample25_freq2clean.csv", encoding = "UTF-8")
#my_table <- as.data.frame(my_table)


batchEval <- function()
  {
  
  str <- ""
  ptmTidy <- proc.time()
  tst <-  evalPredict("test_case.csv","gram2_p002_nofilter.csv")
  ptmTidy <- proc.time() - ptmTidy
  str <- c(str,paste("gram2_p002_nofilter",ptmTidy,"\n")) 
  str <- c(str,paste("percentage correct:",tst))
  print("done!")
  
  ptmTidy <- proc.time()
  tst <-  evalPredict("test_case.csv","gram2_p02_nofilter.csv")
  ptmTidy <- proc.time() - ptmTidy
  str <- c(str,paste("gram2_p02_nofilter",ptmTidy,"\n"))
  str <- c(str,paste("percentage correct:",tst))
  print("done!")
  ptmTidy <- proc.time()
  tst <-  evalPredict("test_case.csv","gram3_p02_nofilter.csv")
  ptmTidy <- proc.time() - ptmTidy
  str <- c(str,paste("gram3_p02_nofilter",ptmTidy,"\n"))
  str <- c(str,paste("percentage correct:",tst))
  print("done!")
  ptmTidy <- proc.time()
  tst <-  evalPredict("test_case.csv","gram3_p02_withfilter.csv")
  
  ptmTidy <- proc.time() - ptmTidy
  
  str <- c(str,paste("gram3_p02_withfilter.csv",ptmTidy,"\n"))
  str <- c(str,paste("percentage correct:",tst))
  print("done!")
  ptmTidy <- proc.time()
  tst <-  evalPredict("test_case.csv","gram5_p02_withfilter.csv")
  ptmTidy <- proc.time() - ptmTidy
  
  str <- c(str,paste("gram5_p02_withfilter.csv",ptmTidy,"\n"))
  str <- c(str,paste("percentage correct:",tst))
  print("done!")
  
  ptmTidy <- proc.time()
  tst <-  evalPredict("test_case.csv","gram5_p02_nofilter.csv")
  ptmTidy <- proc.time() - ptmTidy
  
  str <- c(str,paste("gram5_p02_nofilter.csv",ptmTidy,"\n"))
  str <- c(str,paste("percentage correct:",tst))
  print("done!")
  
  write(str,"evaluation_times_perc.txt")}


evalPredict <- function(modelFile, testFile)
  {
  my_table <- fread(modelFile)
  my_table <- as.data.frame(my_table)
  test <- fread(testFile,encoding = "UTF-8")
  test <- as.data.frame(test)
  test <- mutate(test, assess = evalLine(given,predict,my_table))
  
  return(sum(test$assess / nrow(test)))
  }


evalLine <- function(fourgr, one_gram, my_table){
  the_three <- predictNext3(fourgr,my_table)
  return(one_gram %in% the_three)
}

getPref <- function(some_text,my_t){
  # some text in right format
  f <- dplyr::filter(my_t, given == some_text)
  f <- arrange(f,desc(Freq))
  return(f$predict)
}

predictNext3 <- function(some_text,my_table){
  str <- some_text
  s <- unlist(tstrsplit(str,split="_"))
  l <- length(s)
  v <- max(0,l - 4)
  enough <- FALSE
  m <- NULL
  while(!enough & v <=l){
    m <- c(m,getPref(some_text=paste(s[v:l],collapse="_"),my_t=my_table) )
    
    enough <- length(m) >=3
    v <- v+1
  }
  if(length(m)<3){m <- c(m,"i","we","you")}
  return(unique(m))
  
}





