library(readr)
library(plyr)
library(dplyr)
library(lubridate)
library(magrittr)


if (FALSE) {
  skrooge_transactions <-
    readr::read_delim("skrooge.csv", ";", col_types = cols(date = col_character())) %>%
    filter(date != '0000-00-00') %>%
    mutate(date = as_date(date))


  new_transactions <-
    readr::read_csv("currentTransaction_1760.csv",
                    col_types = cols(`Reference Number` = col_character())) %>%
    rename(date = `Posted Date`,
           reference = `Reference Number`,
           payee = `Payee`,
           address = `Address`,
           quantity = `Amount`) %>%
    mutate(date = mdy(date),
           comment = payee) %>%
    select(-reference, -address)
}


classify <- function (skrooge, candidates)
{
  new_transactions <- anti_join(candidates, skrooge, by = c("date", "quantity"))

  # existing matches - bank comment and its known (from existing date)
  # category and payee
  known <-
    dplyr::setdiff(candidates, new_transactions) %>%
    select(date, quantity, comment)

  known <-
    skrooge %>%
    select(date, quantity, payee, category) %>%
    left_join(known, by = c("date", "quantity"))

  # find which known transactions are most similar to those not yet
  # classified (new)
  classified <-
    new_transactions %>%
    adply(1, function (new) {
      # compute distance based on comment
      with_distance <-
        known %>%
        mutate(dist = as.vector(adist(new$comment, comment)),
               dist = 1/(1 + dist))

      # there needs to be at least one existing transaction we can
      # relate to
      if (all(is.na(with_distance$dist))) {
        new$category <- NA_character_
        new$payee    <- NA_character_
        return(new)
      }

      new$category <-
        with_distance %>%
        group_by(category) %>%
        summarize(dist = sum(dist ** 2), n = n()) %>%
        arrange(desc(dist), desc(n)) %>%
        head(1) %>%
        extract2("category")

      new$payee <-
        with_distance %>%
        filter(nchar(payee) > 0) %>%
        group_by(payee) %>%
        summarize(dist = sum(dist ** 2), n = n()) %>%
        arrange(desc(dist), desc(n)) %>%
        head(1) %>%
        extract2("payee")

      new
    }, .expand = FALSE)

  classified$X1 <- NULL
  classified
}


hand_editing <- function (candidates)
{

}


