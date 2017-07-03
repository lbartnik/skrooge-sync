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
    mutate(comment = ifelse(is.na(comment), payee, comment)) %>%
    select(-new_comment, -skrooge_comment) %>%
    mutate(comment = strip_comment(comment))

  classified <- adply(new_transactions, 1, function (new) {
    classify_transaction(new, known)
  }, .expand = FALSE)

  classified$X1 <- NULL
  classified
}


strip_comment <- function (comment)
{
  comment %>%
    tolower %>%
    str_replace_all("[^a-z]", "")
}


#' find which known transactions are most similar to those not yet
#' classified (new)
#' @importFrom dplyr %>% group_by summarize arrange filter
#' @importFrom magrittr extract2
#' @importFrom utils adist
#' @importFrom stringr str_replace
#' @importFrom stringdist stringdist
#'
classify_transaction <- function (new, known)
{
  stripped_comment <-
    new$comment %>%
    strip_comment

  # compute distance based on comment
  with_distance <-
    known %>%
    mutate(dist = as.vector(stringdist(stripped_comment, comment, method = "soundex")),
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
    summarize(dist = mean(dist ** 2), n = n()) %>%
    arrange(desc(dist), desc(n)) %>%
    head(1) %>%
    extract2("category")

  new$payee <-
    with_distance %>%
    filter(nchar(payee) > 0) %>%
    group_by(payee) %>%
    summarize(dist = mean(dist ** 2), n = n()) %>%
    arrange(desc(dist), desc(n)) %>%
    head(1) %>%
    extract2("payee")

  new
}
