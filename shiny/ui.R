library(shiny)
library(shinyjs)
library(V8)
jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

shinyUI(fluidPage(
  uiOutput("title_panel"),
  
  ## For session resetting
  useShinyjs(),                                       
  extendShinyjs(text = jsResetCode),  
  
  sidebarLayout(
    sidebarPanel( 
      textOutput("sourceClass"),
      br(),
      textOutput("clicks"),
      br(),
      textOutput("targetClass"),
      uiOutput("backButton"),
      uiOutput("resetLink"),
      uiOutput("suggestLink"),
      actionLink("about", "Help!")
    ),
  
    mainPanel(
      htmlOutput("gameover"),
      div(style="display:display-block",uiOutput("button1")), 
      div(style="display:display-block",uiOutput("button2")),
      div(style="display:display-block",uiOutput("button3")),
      div(style="display:display-block",uiOutput("button4")),
      div(style="display:display-block",uiOutput("button5")),
      div(style="display:display-block",uiOutput("button6")),
      div(style="display:display-block",uiOutput("button7")),
      div(style="display:display-block",uiOutput("button8")),
      div(style="display:display-block",uiOutput("button9")),
      div(style="display:display-block",uiOutput("button10")),
      tags$head(tags$script(HTML(
                    paste0(
                    '$(document).ready(function(){
                    $("#about")
                    .popover({html: true, 
                    title: "About this game", 
                    content: "', readChar("about.txt", file.info("about.txt")$size), 
                    '", trigger: "click", placement: "bottom"
                    });
                    });'))))
    )
  
)))