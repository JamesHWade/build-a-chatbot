# Load necessary libraries for the Shiny app and additional functionalities
library(shiny)      # Core Shiny package for building interactive web apps
library(bslib)      # Bootstrap libraries for theming and styling
library(bsicons)    # Bootstrap icons for graphical elements
library(htmltools)  # Tools for HTML manipulation and rendering
library(httr2)      # HTTP client package for API interactions
library(purrr)      # Functional programming tools
library(glue)       # Interpolates strings and variables

# Source external R script(s) that likely contain helper functions
source("helper.R")

# Define the user interface of the Shiny application
ui <- page_fillable(
  title = "Chatbot with R & OpenAI", # Title of the web page
  theme = bs_theme(bootswatch = "lumen"), # Use 'lumen' theme from Bootswatch
  sidebar = sidebar(
    open = "closed" # Initialize the sidebar as closed
  ),
  card(
    card_header("Chatbot",
                popover(
                  bs_icon("gear", class = "ms-auto"), # Gear icon for settings
                  # Dropdown for model selection
                  selectInput("model", "Model",
                              choices = c("gpt-3.5-turbo", "gpt-4-turbo-preview")),
                  # Dropdown for task selection
                  selectInput("task", "Task",
                              choices = c("general", "code")),
                  title = "Chat Settings"             # Title of the popover
                ),
                class = "d-flex align-items-center gap-1" # Styling classes to align elements
    ),
    uiOutput("chat_history"), # Placeholder for chat history, to be rendered server-side
    div(
      class = "mt-auto",      # Styling: margin-top auto
      style = css(
        "margin-left" = "20%", # Custom CSS for margins
        "margin-right" = "20%"),
      fluidRow(
        column(
          10,
          textAreaInput("prompt", NULL,              # Input area for user prompt
                        width = "100%", rows = 1,
                        placeholder = "Ask a question ..."
          ),
        ),
        column(
          2,
          actionButton(
            inputId = "chat",
            label = icon("fas fa-paper-plane"),      # Button icon (paper plane)
            class = "btn-primary m-1",               # Button styling
            width = "100%"
          )
        )
      )
    )
  )
)

# Server-side logic for the Shiny application
server <- function(input, output, session) {
  rv <- reactiveValues() # Initialize reactive values
  rv$chat_history <- NULL # Start with empty chat history

  observe({
    req(input$prompt != "") # Ensure prompt is not empty before proceeding
    response <- chat(input$prompt,              # Call `chat` function (defined in helper.R?)
                     model = input$model,       # Pass selected model
                     history = rv$chat_history, # Pass current chat history
                     system_prompt = input$task # Pass selected task
    )
    rv$chat_history <- update_history(rv$chat_history, input$prompt, response) # Update chat history
    output$chat_history <- renderUI({
      req(!is.null(rv$chat_history)) # Ensure chat history is not null before rendering
      card(
        map(
          .x = rv$chat_history,
          .f = \(x) markdown(glue("**{x$role}**: {x$content}")) # Format each chat entry
        ),
        max_height = "500px" # Set maximum height for chat history card
      )
    })
    updateTextAreaInput(session, "prompt",
                        value = "", # Clear the prompt input field after submission
                        placeholder = "Ready for more input..." # Update placeholder text
    )
  }) |> bindEvent(input$chat) # Trigger observe block on `chat` button click
}

# Run the Shiny application
shinyApp(ui, server)
