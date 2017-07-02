
#' @export
#' @importFrom magrittr %<>%
bofa_credit <- function (new, skrooge, output_path = "BankOfAmerica.csv", after_date = today())
{
  skrooge <- read_skrooge(skrooge)
  candidates <-
    bofa_credit_read(new) %>%
    filter(date >= after_date)

  classified <- classify_bofa(candidates, skrooge)
  readr::write_csv(classified, output_path)

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


#' @importFrom dplyr anti_join setdiff select left_join mutate filter group_by summarize arrange
#' @importFrom magrittr extract2 %>%
#' @importFrom plyr adply
#'
classify_bofa <- function (candidates, skrooge)
{
  new_transactions <-
    anti_join(candidates, skrooge, by = c("date", "amount"))

  # existing matches - bank comment and its known (from existing date)
  # category and payee
  known <-
    dplyr::setdiff(candidates, new_transactions) %>%
    select(date, amount, comment) %>%
    rename(new_comment = comment)

  known <-
    skrooge %>%
    select(date, amount, payee, category, comment) %>%
    rename(skrooge_comment = comment) %>%
    left_join(known, by = c("date", "amount")) %>%
    mutate(comment = ifelse(is.na(new_comment), skrooge_comment, new_comment)) %>%
    select(-new_comment, -skrooge_comment) %>%
    mutate(comment = tolower(comment))

  # find which known transactions are most similar to those not yet
  # classified (new)
  classify_transaction <- function (new) {
    # compute distance based on comment
    with_distance <-
      known %>%
      mutate(dist = as.vector(adist(tolower(new$comment), comment)),
             dist = 1/(1 + dist)) %>%
      filter(!is.na(dist))

    # there needs to be at least one existing transaction we can
    # relate to
    if (!nrow(with_distance)) {
      new$category <- NA_character_
      new$payee    <- NA_character_
      return(new)
    }

    new$category <-
      with_distance %>%
      group_by(category) %>%
      summarize(dist = sqrt(mean(sum(dist ** 2))), n = n()) %>%
      arrange(desc(dist), desc(n)) %>%
      head(1) %>%
      extract2("category")

    new$payee <-
      with_distance %>%
      filter(nchar(payee) > 0) %>%
      group_by(payee) %>%
      summarize(dist = sqrt(mean(sum(dist ** 2))), n = n()) %>%
      arrange(desc(dist), desc(n)) %>%
      head(1) %>%
      extract2("payee")

    new
  }

  classified <- adply(new_transactions, 1, classify_transaction, .expand = FALSE)

  classified$X1 <- NULL
  classified
}
