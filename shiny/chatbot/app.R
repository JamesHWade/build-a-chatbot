library(shiny)
library(bslib)
library(bsicons)
library(htmltools)
library(httr2)
library(purrr)
library(glue)

source("helper.R")

ui <- page_fillable(
  title = "Chatbot with R & OpenAI",
  theme = bs_theme(bootswatch = "pulse"),
  press_enter_to_chat(),
  card(
    card_header("Chatbot",
      popover(
        bs_icon("gear", class = "ms-auto"),
        passwordInput("api_key", "API Key",
          placeholder = "Enter your OpenAI API key"
        ),
        selectInput("model", "Model",
          choices = c(
            "gpt-3.5-turbo",
            "gpt-4-turbo-preview"
          )
        ),
        selectInput("task", "Task",
          choices = c("general", "code")
        ),
        title = "Chat Settings"
      ),
      class = "d-flex align-items-center gap-1"
    ),
    uiOutput("instructions"),
    uiOutput("chat_history"),
    div(
      class = "mt-auto",
      style = css(
        "margin-left" = "15%",
        "margin-right" = "15%"
      ),
      fluidRow(
        column(
          10,
          textAreaInput("prompt", NULL,
            width = "100%", rows = 1,
            placeholder = "Ask a question ..."
          ),
        ),
        column(
          2,
          actionButton(
            inputId = "chat",
            label = bs_icon("send"),
            class = "btn-primary m-1",
            width = "100%"
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues()
  rv$chat_history <- NULL
  observe({
    if (input$api_key == "") {
      output$instructions <- renderUI({
          glue(
            "Please enter your OpenAI API key to start chatting.
          You can get your API key from the [OpenAI website](https://platform.openai.com).
          Add your API key in {bs_icon(\"gear\")} settings. Select a
          model and task to start chatting."
          ) |>
            markdown()
      })
    } else {
      output$instructions <- NULL
    }
  })
  observe({
    req(input$prompt != "")
    response <- chat(input$prompt,
      model = input$model,
      history = rv$chat_history,
      system_prompt = input$task,
      api_key = input$api_key
    )
    rv$chat_history <- update_history(rv$chat_history, input$prompt, response)
    output$chat_history <- renderUI({
      req(!is.null(rv$chat_history))
      card(
        map(
          .x = rv$chat_history,
          .f = \(x) markdown(glue("**{x$role}**: {x$content}"))
        ),
        fill = TRUE
      )
    })
    updateTextAreaInput(session, "prompt",
      value = "",
      placeholder = "Ready for more input..."
    )
  }) |> bindEvent(input$chat)
}

shinyApp(ui, server)
