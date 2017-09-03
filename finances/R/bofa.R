
#' @export
#' @importFrom magrittr %<>%
bofa_credit <- function (new, skrooge, output_path = "BankOfAmerica.csv", after_date = today())
{
  skrooge <- read_skrooge(skrooge)
  candidates <-
    bofa_credit_read(new) %>%
    filter(date >= after_date)

  classified <- classify_bofa(candidates, skrooge)
  if (nrow(classified) < 1) {
    message("No new transactions found.")
    return()
  }

  readr::write_csv(classified, output_path)

  message("Found ", nrow(classified), " new transaction(s).")
  message("Classified transactions written to: ", normalizePath(output_path))

  invisible(classified)
}


#' @export
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr rename mutate select
#' @importFrom lubridate mdy
#'
bofa_credit_read <- function (path)
{
  path %>%
    readr::read_csv(col_types = cols(`Reference Number` = col_character())) %>%
    rename(date = `Posted Date`,
           reference = `Reference Number`,
           payee = `Payee`,
           address = `Address`,
           amount = `Amount`) %>%
    mutate(date = mdy(date),
           comment = payee) %>%
    select(-reference, -address)
}
