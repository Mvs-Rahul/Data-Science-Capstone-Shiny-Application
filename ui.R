#
# ui.R
#
# Capstone App created using Shiny and R Studio 
#
# by Mvs Rahul
# 
# November 15, 2022
#

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Data Science Capstone Shiny Application"),
  h5("by Mvs Rahul on November 15, 2022"),
  h5("Requesting to wait a few seconds for the data to load"),
  
  mainPanel(
    textInput("textIn", 
              label = h3("Please Input the text here: "), 
              value = ""),
    h3("Text output: "),
    textOutput("textOut"),
   
  )
)
)
