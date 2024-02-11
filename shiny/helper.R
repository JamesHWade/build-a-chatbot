# Note: All libraries are loaded in the app.R script, which is why we don't
# have to use explicit namespaces here.

# Function to interact with OpenAI's Chat API, sending user messages and
# receiving responses.
# Parameters include the user's message, the chosen model, chat history, the
# type of system prompt, and the API key.
chat <- function(user_message,
                 model,
                 history = NULL,
                 system_prompt = c("general", "code"),
                 api_key = Sys.getenv("OPENAI_API_KEY")) {
  # Generate the system prompt based on the desired task.
  system <- get_system_prompt(system_prompt)

  # Prepare the full prompt to send to OpenAI, combining system instructions,
  # history, and the user's message
  prompt <- prepare_prompt(user_message, system, history)

  # Define the request body with the model and prepared messages.
  body <- list(
    model = model,
    messages = prompt
  )

  # Make a request to OpenAI's chat API, authenticate, send the request body,
  # and parse the response
  response <-
    request("https://api.openai.com/v1") |>
    req_url_path_append("chat/completions") |>
    req_auth_bearer_token(token = api_key) |>
    req_body_json(body) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()

  if (httr2::resp_is_error(response)) {
    status <- httr2::resp_status(response)
    description <- httr2::resp_status_desc(response)

    cli::cli_warn(message = c(
      "x" = glue::glue("OpenAI API request failed. Error {status} - {description}"),
      "i" = "Visit the OpenAI API documentation for more details"
    ))

    error_msg <-
      glue("_Uh Oh._ There was an error. Please check your API key.\n
           Use the {bs_icon(\"gear\")} at the top right to update settings.")

    return(invisible(error_msg))
  }

  response |>
    resp_body_json(simplifyVector = TRUE) |>
    # Extract the content of the response from the nested list structure
    pluck("choices", "message", "content")
}

# Function to determine the system prompt based on the task type.
# This sets the tone and context for the chatbot's responses.
get_system_prompt <- function(system = c("general", "code")) {
  instructions <-
    switch(system,
      "general" = "You are a helpful assistant.",
      "code"    = "You are a helpful chat bot that answers questions for an R programmer working in the RStudio IDE."
    )
  # Return as a list of lists to match API format.
  list(list(role = "system", content = instructions))
}

# Function to prepare the prompt by combining system instructions, any existing
# chat history, and the user's latest message.
prepare_prompt <- function(user_message, system_prompt, history) {
  # Wrap the user message in a list to match API format.
  user_prompt <- list(list(role = "user", content = user_message))
  # Combine system prompt, history, and user message.
  c(system_prompt, history, user_prompt) |>
    compact() # Remove any NULL elements, keeping the prompt clean.
}

# Function to update the conversation history with the latest exchange between
# the user and the assistant.
update_history <- function(history, user_message, response) {
  c(
    history, # Existing conversation history.
    list(
      list(role = "user", content = user_message), # Add the latest user message
      list(role = "assistant", content = response) # Add the latest response
    )
  ) |> compact() # Clean the history from any NULL values.
}

# Define a function to enable message submission by pressing the Enter key.
press_enter_to_chat <- function() {
  # Insert custom HTML and JavaScript into the Shiny app's HTML head section.
  # This script is designed to listen for keydown events on an input field with the ID 'prompt'.
  tags$head(
    tags$script(
      # Use the HTML function to ensure that the JavaScript code is correctly interpreted as HTML.
      HTML("
        // jQuery document ready function to ensure the script runs after the document is fully loaded.
        $(document).on('keydown', '#prompt', function(e) {
          // Check if the key pressed is Enter (key code 13) or Line Feed (key code 10), and the Shift key is NOT pressed.
          if ((e.keyCode == 10 || e.keyCode == 13) && (!e.shiftKey)) {
            // Prevent the default action of the Enter key press, which is typically form submission.
            e.preventDefault();
            // Set a timeout before simulating a click event on the 'chat' button.
            // This delay ensures that any UI updates or validations can occur before the click action.
            setTimeout(function() {
              // Programmatically click the button with the ID 'chat' to submit the message.
              $('#chat').click();
            }, 500); // Delay set to 500 milliseconds.
          }
        });
      ")
    )
  )
}
