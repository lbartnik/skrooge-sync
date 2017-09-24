
#' Classify new transaction for Bank oF America accounts.
#'
#' @param new Path to the CSV transaction file.
#' @param skrooge Path to the Skrooge database file.
#' @param output_path Path to the output CSV file.
#' @param after_date Classify only transactions younger than this date.
#'
#' @export
#' @importFrom magrittr %<>%
#' @importFrom dplyr filter
#'
bofa_credit <- function (new, skrooge, output_path = "BankOfAmerica.csv", after_date = today())
{
  candidates <-
    bofa_credit_read(new) %>%
    filter(date >= after_date)

  process_any(candidates, skrooge, output_path)
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
