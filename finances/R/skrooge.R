
#' @importFrom tools file_ext
convert_skrooge <- function (path)
{
  stopifnot(file.exists(path))
  stopifnot(identical(file_ext(path), "skg"))

  out_path <- tempfile("skrooge_data", fileext = ".csv")
  rc <- system2("skroogeconvert", paste("--in", path, "--out",  out_path),
                stdout = TRUE, stderr = TRUE)

  if (!is.null(attr(rc, 'status')) && !identical(attr(rc, 'status'), 0)) {
    stop("an error occurred while converting Skrooge budget file: ",
         paste(rc, collapse = "\n"), call. = FALSE)
  }

  out_path
}


#' @export
#' @importFrom readr read_delim cols col_character
#' @importFrom lubridate as_date
#' @importFrom tools file_ext
#'
read_skrooge <- function (path = "skrooge.csv")
{
  if (identical(file_ext(path), "skg")) {
    path <- convert_skrooge(path)
  }

  transactions <-
    readr::read_delim(path, ";", col_types = cols(date = col_character()))

  # make sure this is the right data set
  expected <- c('date', 'account', 'payee', 'amount', 'category', 'comment')
  i <- expected %in% names(transactions)
  if (!all(i)) {
    stop("columns missing in the Skrooge data (", path, "): ",
         paste(expected[!i], collapse = ", "), call. = FALSE)
  }

  # 0000-00-00 - these are initial account states
  transactions %>%
    filter(date != '0000-00-00') %>%
    mutate(date = as_date(date)) %>%
    select(date, amount, payee, category, comment)
}
