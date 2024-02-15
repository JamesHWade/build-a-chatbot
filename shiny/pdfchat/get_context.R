get_context <- function(prompt, index, k = 4) {
  query_embedding <- gpttools:::get_query_embedding(prompt,
                                                    local = FALSE)
  index <- index |> tidyr::unnest(embedding)

  context <-
    gpttools:::get_query_context(query_embedding, index, k) |>
    dplyr::select(source, chunks) |>
    purrr::pmap(\(source, chunks) {
      glue::glue("Source: {source}
                  Text: {chunks}")
    }) |>
    unlist() |>
    paste(collapse = "\n\n")

  glue::glue("{context}
             Prompt from user:
             {prompt}") |> cli::cat_print()
}

