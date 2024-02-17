library(shiny)
library(bslib)
library(bsicons)
library(htmltools)
library(httr2)
library(purrr)
library(glue)

source("helper.R")

ui <- page_fillable(
  title = "Chatbot wtih R & OpenAI",
  card()
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
