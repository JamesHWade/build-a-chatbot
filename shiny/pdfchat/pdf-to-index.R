ingest_pdf <- function(file_path, source = basename(file_path), link = NULL, api_key) {
  text <- pdftools::pdf_text(file_path) |>
    readr::read_lines() |>
    stringr::str_c(collapse = " ") |>
    stringr::str_remove_all("\\.\\.\\.|\\ \\ \\ ")

  tibble::tibble(
    text = text,
    source = source,
    link = link
  ) |>
    dplyr::mutate(
      chunks = purrr::map(text, \(x) {
        gpttools:::chunk_with_overlap(
          x,
          chunk_size = 500,
          overlap_size = 50,
          doc_id = source |> janitor::make_clean_names(),
          lowercase = FALSE,
          strip_punct = FALSE,
          strip_numeric = FALSE,
          stopwords = NULL
        )
      })
    ) |>
    tidyr::unnest(chunks) |>
    tidyr::unnest(chunks) |>
    dplyr::select(-text) |>
    dplyr::mutate(
      n_tokens = tokenizers::count_characters(chunks) %/% 4
    ) |>
    add_embeddings(api_key) |>
    dplyr::rename(embedding = embeddings)
}

ingest_pdf_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Choose PDF file(s)",
              multiple = TRUE, accept = ".pdf"),
    actionButton(ns("ingest"), "Ingest PDF")
  )
}

ingest_pdf_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv$pdf_index <- NULL

    observeEvent(input$ingest, {
      req(input$file)
      req(rv$api_key())
      files <- input$file$datapath
      new_pdfs <-
        purrr::map(files, \(x) {
          ingest_pdf(x, api_key = rv$api_key())
        }) |> dplyr::bind_rows()

      cli::cli_alert_success("Made an index")

      rv$pdf_index <- dplyr::bind_rows(rv$pdf_index, new_pdfs)
    })
  })
}

add_embeddings <- function(index, api_key) {
  index |>
    dplyr::mutate(
      embeddings = purrr::map(
        .x = chunks,
        .f = \(x) gpttools:::create_openai_embedding(x, openai_api_key = api_key),
        .progress = "Create Embeddings"
      ),
      embedding_method = "OpenAI"
    )
}
