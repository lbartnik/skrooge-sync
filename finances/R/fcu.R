
#' @importFrom readr read_csv
#' @importFrom magrittr %>%
#' @importFrom lubridate mdy
#' @importFrom dplyr rename mutate
#'
fcu_read <- function (path)
{
  readr::read_csv(path) %>%
    rename(date = `Posting Date`,
           amount = `Amount`,
           comment = `Description`) %>%
    mutate(date = mdy(date)) %>%
    select(date, comment, amount)
}


#' Classify new transaction for FCU accounts.
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
fcu_any <- function (new, skrooge, output_path = "FCU.csv", after_date = today())
{
  candidates <-
    fcu_read(new) %>%
    filter(date >= after_date)

  process_any(candidates, skrooge, output_path)
}
