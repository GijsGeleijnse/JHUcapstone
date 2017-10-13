
library(shiny)
library(sqldf)
library(data.table)
library(dplyr)

my_table <- fread("gram5_p02_withfilter.csv")
my_table <- as.data.frame(my_table)
my_table_trump <- fread("trumpgrams.csv")
my_table_trump <- as.data.frame(my_table_trump)



getPrefTrump <- function(some_text, the_table=my_table){
  # some text in right format
  f <- dplyr::filter(my_table_trump, given == some_text)
  f <- arrange(f,desc(Freq))
  return(f$predict)
}

getPrefNormal <- function(some_text){
  # some text in right format
  f <- dplyr::filter(my_table, given == some_text)
  f <- arrange(f,desc(Freq))
  return(f$predict)
}


predictNext3Trump <- function(some_text){
  str <- some_text
  str <- tolower(str)
  s <- unlist(tstrsplit(str,split=" "))
  l <- length(s)
  v <- max(0,l - 4)
  enough <- FALSE
  m <- NULL
  while(!enough & v <=l){
    m <- c(m,getPrefTrump(paste(s[v:l],collapse="_")))
    
    enough <- length(m) >=3
    v <- v+1
  }
  m <- unique(m)
  if(length(m)<3){m <- c(m,"terrible","crooked","I")}
  return(m)
  
}

predictNext3 <- function(some_text){
  str <- some_text
  str <- tolower(str)
  s <- unlist(tstrsplit(str,split=" "))
  l <- length(s)
  v <- max(0,l - 4)
  enough <- FALSE
  m <- NULL
  while(!enough & v <=l){
    m <- c(m,getPrefNormal(paste(s[v:l],collapse="_")) )
    
    enough <- length(m) >=3
    v <- v+1
  }
  m <- unique(m)
  if(length(m)<3){m <- c(m,"I","love","you")}
  return(m)
  
}


my_preds_coursera <- predictNext3("")
my_preds <- predictNext3("")
my_preds_trump <- predictNext3Trump("")


ui <- fluidPage(
   
   # Application title
   titlePanel("My Three Automatic Crystal Balls"),
   
   hr(),
   
   h4("Text prediction according to the review criteria:"),
   textInput("my_text_coursera", label="Type and be surprised:"),  
   actionButton("button_coursera", label="Submit"),
   fluidRow(column(3, verbatimTextOutput("answer_coursera"))),
   hr(),
   
   h4("Text prediction based on the course corpora - a little more dynamic and exciting:"),
   textInput("my_text", label="Type and be surprised:"),  
   
   
   # 
    actionButton("button1", label=uiOutput("button1")),
    actionButton("button2", label=uiOutput("button2")),
    actionButton("button3", label=uiOutput("button3")),
   p("Simply type or click a button to select the next word in the text field."),
   
   
   
   hr(),
   
   
   h4("Presidential Twitter Support (based on Trump Tweet Archive):"),
   textInput("my_text_trump", label="Type and be surprised:")
   
   ,  
   
   
    # 
    actionButton("button1_trump", label=uiOutput("button1_trump")),
    actionButton("button2_trump", label=uiOutput("button2_trump")),
    actionButton("button3_trump", label=uiOutput("button3_trump")),
    p("Start typing your next tweet, Mr. President.")

  
   )   
      
   

server <- function(input, output,session) {
 
  ## observe coursera
  
  
  observeEvent(input$button_coursera, {
    my_preds_coursera <<- predictNext3(input$my_text_coursera)
    output$answer_coursera <- renderText({ my_preds_coursera[1] })
  })
  
  
  
  observeEvent(input$button1, {
  
    updateTextInput(session, "my_text", value = paste(input$my_text, my_preds[1]))
  })
  observeEvent(input$button2, {
    updateTextInput(session, "my_text", value = paste(input$my_text, my_preds[2]))
  })
  observeEvent(input$button3, {
    updateTextInput(session, "my_text", value = paste(input$my_text, my_preds[3]))
  })
 
  
  ### observeEvent regular
   observeEvent(input$my_text, {
    my_preds <<- predictNext3(input$my_text)
    v <- unlist(tstrsplit(input$my_text,split=" "))
    #if(length(v)>=6){
    #  updateTextInput(session, "my_text", value = paste(s[length(v)-6:length(v)],collapse=" "))
    #}
              

  output$button1 <- renderUI({
    #my_preds <<- predictNext3(input$my_text)
    
    actionButton("action", label = my_preds[1])
  })
  output$button2 <- renderUI({
    #my_preds <<- predictNext3(input$my_text)
    actionButton("action", label = my_preds[2])
  })
  output$button3 <- renderUI({
    #my_preds <<- predictNext3(input$my_text)
    actionButton("action", label = my_preds[3])
  })
  
  })
   
   ### Trump
   observeEvent(input$button1_trump, {

     updateTextInput(session, "my_text_trump", value = paste(input$my_text_trump, my_preds_trump[1]))
   })
   observeEvent(input$button2_trump, {
     updateTextInput(session, "my_text_trump", value = paste(input$my_text_trump, my_preds_trump[2]))
   })
   observeEvent(input$button3_trump, {
     updateTextInput(session, "my_text_trump", value = paste(input$my_text_trump, my_preds_trump[3]))
   })


   ### observeEvent trump
   observeEvent(input$my_text_trump, {
     my_preds_trump <<- predictNext3Trump(input$my_text_trump)
     v <- unlist(tstrsplit(input$my_text_trump,split=" "))
     #if(length(v)>=6){
     #  updateTextInput(session, "my_text_trump", value = paste(s[length(v)-6:length(v)],collapse=" "))
     #}


     output$button1_trump <- renderUI({
       #my_preds <<- predictNext3(input$my_text)

       actionButton("action", label = my_preds_trump[1])
     })
     output$button2_trump <- renderUI({
       #my_preds <<- predictNext3(input$my_text)
       actionButton("action", label = my_preds_trump[2])
     })
     output$button3_trump <- renderUI({
       #my_preds <<- predictNext3(input$my_text)
       actionButton("action", label = my_preds_trump[3])
     })

   })
   
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

