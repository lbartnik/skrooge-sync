#' @importFrom magrittr %<>%
#' @importFrom dplyr filter
#'
process_any <- function (candidates, skrooge, output_path)
{
  skrooge <- read_skrooge(skrooge)

  classified <- classify_candidates(candidates, skrooge)
  if (nrow(classified) < 1) {
    message("No new transactions found.")
    return()
  }

  readr::write_csv(classified, output_path)

  message("Found ", nrow(classified), " new transaction(s).")
  message("Classified transactions written to: ", normalizePath(output_path))

  invisible(classified)
}
