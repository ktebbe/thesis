library(shiny)
library(igraph)
library(RMySQL)
library(pool)
source("course.R",local=TRUE)

pool <- dbPool(
  drv = RMySQL::MySQL(),
  user='ktebbe', 
  password='[hidden]', 
  dbname='words', 
  host='words.cl2itmibl6gz.us-west-2.rds.amazonaws.com')

shinyServer(function(input, output, session) {
  sessionID <- sample(1000000, 1)
  
  writeToDB <- function(session_id, Source, choice, Target, back, suggestion){
    query <- paste("INSERT INTO network3 (session_id, source, choice, target, back, suggestion) VALUES (", 
                   session_id, ", '",
                   Source, "', '", 
                   choice, "', '", 
                   Target, "', ", 
                   back, ", ", 
                   suggestion, ");", sep="")
    conn <- poolCheckout(pool) #added 
    dbSendQuery(conn, query) #change from mydb
    conn <- poolReturn(conn)
  }
  
  ## To stop game
  GAMEOVER <- reactive({
    if(length(history[['clicked']]) == 0){
      return(FALSE)
    }
    else if(history[['clicked']][length(history[['clicked']])] == SourceTarget[2])
      return(TRUE)
    else{
      return(FALSE)
    }
  })
  
  # Randomly determining source & target classes.
  SourceTarget <- selectClasses()
  # reading in the network
  net <- read_graph("networkDist.txt", format = "ncol", directed=FALSE)
  
  output$title_panel <- renderUI({
    if(length(history[['clicked']]) == 0){
      titlePanel("Welcome to my Game!")
    }
    else{
      titlePanel(paste("Current Class - ", getName(history[['clicked']][length(history[['clicked']])]), sep=""))
    }
  })
  output$sourceClass <- renderText({ 
    paste("Start: ", getName(SourceTarget[1]), sep="")
  })
  output$targetClass <- renderText({
    paste("End: ", getName(SourceTarget[2]), sep="")
  })
  
  ## This is the path the user has clicked so far to get to the target. 
  output$clicks <- renderText({
    toPrint <- c()
    if(length(history[['clicked']]) > 0){
      for(i in 1:length(history[['clicked']])){
        toPrint <- c(toPrint, history[['clicked']][i], " -> ")
      }
    }
    #history[['clicked']]
    toPrint
  })
  
  ###-- Reactive --###
  # Click history stored as reactive values.
  history <- reactiveValues(clicked = c())
  
  ## Update history when any button is clicked - add most recent button label.
  observeEvent(input$course1,{
    writeToDB(sessionID, SourceTarget[1], inside()[1], SourceTarget[2], 0, 0)
    history[['clicked']] <- c(history[['clicked']],inside()[1])
  })
  observeEvent(input$course2,{
    writeToDB(sessionID, SourceTarget[1], inside()[2], SourceTarget[2], 0, 0)
    history[['clicked']] <- c(history[['clicked']],inside()[2])
  })
  observeEvent(input$course3,{
    writeToDB(sessionID, SourceTarget[1], inside()[3], SourceTarget[2], 0, 0)
    history[['clicked']] <- c(history[['clicked']],inside()[3])
  })
  observeEvent(input$course4,{
    writeToDB(sessionID, SourceTarget[1], inside()[4], SourceTarget[2], 0, 0)
    history[['clicked']] <- c(history[['clicked']],inside()[4])
  })
  observeEvent(input$course5,{
    writeToDB(sessionID, SourceTarget[1], inside()[5], SourceTarget[2], 0, 0)
    history[['clicked']] <- c(history[['clicked']],inside()[5])
  })
  observeEvent(input$course6,{
    writeToDB(sessionID, SourceTarget[1], inside()[6], SourceTarget[2], 0, 0)
    history[['clicked']] <- c(history[['clicked']],inside()[6])
  })
  observeEvent(input$course7,{
    writeToDB(sessionID, SourceTarget[1], inside()[7], SourceTarget[2], 0, 0)
    history[['clicked']] <- c(history[['clicked']],inside()[7])
  })
  observeEvent(input$course8,{
    writeToDB(sessionID, SourceTarget[1], inside()[8], SourceTarget[2], 0, 0)
    history[['clicked']] <- c(history[['clicked']],inside()[8])
  })
  observeEvent(input$course9,{
    writeToDB(sessionID, SourceTarget[1], inside()[9], SourceTarget[2], 0, 0)
    history[['clicked']] <- c(history[['clicked']],inside()[9])
  })
  observeEvent(input$course10,{
    writeToDB(sessionID, SourceTarget[1], inside()[10], SourceTarget[2], 0, 0)
    history[['clicked']] <- c(history[['clicked']],inside()[10])
  })
  
  ## if suggest button is pressed --> just write to DB
  observeEvent(input$suggest,{
    writeToDB(sessionID, SourceTarget[1], history[['clicked']][length(history[['clicked']])], SourceTarget[2], 0, 1)
  })
  
  ## if back button is pressed --> update click list & shown buttons & write DB
  observeEvent(input$back,{
      ## write to DB before going back
      writeToDB(sessionID, SourceTarget[1], history[['clicked']][length(history[['clicked']])], SourceTarget[2], 1, 0)
      length(history[['clicked']]) <- length(history[['clicked']]) - 1
  })
  
  ## Reset session if clicked
  observeEvent(input$reset,{
    js$reset()
  })
  
  ## Update inside when history updates
  inside <- reactive({
    history[['clicked']]
    
    if(length(history[['clicked']]) == 0){
      ## if no clicked classes, use source
      names(neighbors(net, v=SourceTarget[1]))
    }
    else{
      ## otherwise, use most recent class
      names(neighbors(net, v=history[['clicked']][length(history[['clicked']])] ))
    }
  })
  
  ###-- Buttons --###
  output$button1 <- renderUI({
    if(!GAMEOVER() & length(inside()) >= 1){
      if(inside()[1] == SourceTarget[2]){ textColor <- "dodgerblue" }
      else{ textColor <- "black" }
      actionButton("course1", label = getName(inside()[1]),
                   style=paste("color:", textColor, sep=""))
    }
  })
  output$button2 <- renderUI({
    if(!GAMEOVER() & length(inside()) >= 2){
      if(inside()[2] == SourceTarget[2]){ textColor <- "dodgerblue" }
      else{ textColor <- "black" }
      actionButton("course2", label = getName(inside()[2]),
                   style=paste("color:", textColor, sep=""))
    }
  })
  output$button3 <- renderUI({
    if(!GAMEOVER() & length(inside()) >= 3){
      if(inside()[3] == SourceTarget[2]){ textColor <- "dodgerblue" }
      else{ textColor <- "black" }
      actionButton("course3", label = getName(inside()[3]),
                   style=paste("color:", textColor, sep=""))
    }
  })
  output$button4 <- renderUI({
    if(!GAMEOVER() & length(inside()) >= 4){
      if(inside()[4] == SourceTarget[2]){ textColor <- "dodgerblue" }
      else{ textColor <- "black" }
      actionButton("course4", label = getName(inside()[4]),
                   style=paste("color:", textColor, sep=""))
    }
  })
  output$button5 <- renderUI({
    if(!GAMEOVER() & length(inside()) >= 5){
      if(inside()[5] == SourceTarget[2]){ textColor <- "dodgerblue" }
      else{ textColor <- "black" }
      actionButton("course5", label = getName(inside()[5]),
                   style=paste("color:", textColor, sep=""))
    }
  })
  output$button6 <- renderUI({
    if(!GAMEOVER() & length(inside()) >= 6){
      if(inside()[6] == SourceTarget[2]){ textColor <- "dodgerblue" }
      else{ textColor <- "black" }
      actionButton("course6", label = getName(inside()[6]),
                   style=paste("color:", textColor, sep=""))
    }
  })
  output$button7 <- renderUI({
    if(!GAMEOVER() & length(inside()) >= 7){
      if(inside()[7] == SourceTarget[2]){ textColor <- "dodgerblue" }
      else{ textColor <- "black" }
      actionButton("course7", label = getName(inside()[7]),
                   style=paste("color:", textColor, sep=""))
    }
  })
  output$button8 <- renderUI({
    if(!GAMEOVER() & length(inside()) >= 8){
      if(inside()[8] == SourceTarget[2]){ textColor <- "dodgerblue" }
      else{ textColor <- "black" }
      actionButton("course8", label = getName(inside()[8]),
                   style=paste("color:", textColor, sep=""))
    }
  })
  output$button9 <- renderUI({
    if(!GAMEOVER() & length(inside()) >= 9){
      if(inside()[9] == SourceTarget[2]){ textColor <- "dodgerblue" }
      else{ textColor <- "black" }
      actionButton("course9", label = getName(inside()[9]),
                   style=paste("color:", textColor, sep=""))
    }
  })
  output$button10 <- renderUI({
    if(!GAMEOVER() & length(inside()) >= 10){
      if(inside()[10] == SourceTarget[2]){ textColor <- "dodgerblue" }
      else{ textColor <- "black" }
      actionButton("course10", label = getName(inside()[10]),
                   style=paste("color:", textColor, sep=""))
    }
  })
  
  output$resetLink <- renderUI({
    actionLink("reset", label = "Start a new game!")
  })
  output$gameover <- renderUI(
    if(GAMEOVER()){
      HTML("<h1>Congratulations! You won! Play again?</h1>")
    }
  )
  
  ## Only show "back" & "suggest" button if there is progress - prevent crashes. 
  output$backButton <- renderUI({
    if(length(history[['clicked']]) >= 1){
      actionLink("back", label = "Oops! Go back a step.")
    }
  })
  output$suggestLink <- renderUI({
    if(length(history[['clicked']]) >= 1){
      actionLink("suggest", label = "Suggest a connection.")
    }
  })
  
})