library(plyr)
library(dplyr)
library(lubridate)

if (FALSE) {
  setwd('work')

  existingDataFile <- 'skrooge.csv'
  newerThan <- ymd('2017-06-11')

  accountName      <- 'Lukasz Checking'
  classifiedData   <- processData(accountName, existingDataFile, paste0(accountName, '.csv'), newerThan)

  accountName      <- 'Lukasz Credit Card'
  newDataFile      <- paste0(accountName, '.csv')
  classifiedData   <- processData(accountName, existingDataFile, newDataFile, newerThan)
}

# outstanding questions:
#
# 1. how to make sure we don't miss any new operation?
# 2. how to make sure we don't lose any "old" operation?
# 3. how do duplicates influence this process? what if there has
#    been an operation that is exactly the same as another and it
#    needs to be added? how to identify such corner cases?



classify <- function (overlap, notSeen)
{
  # now try and assign "category" and "payee" to these new operations;
  # in order to do so, look at the `overlap` set and find the set of
  # most similar "old" transactions and out of them choose the most
  # frequest category and payee

  classified <-
    adply(notSeen, 1, function (toClassify) {
      withDistance <-
        overlap %>%
        mutate(
          adist = as.vector(adist(toClassify$comment, comment)),
          ddist = abs(amount - toClassify$amount)) %>%
        mutate(adist = 1/(1 + adist))

      categories <-
        withDistance %>%
        group_by(category) %>%
        summarize(adist = sum(adist ** 2), n = n()) %>%
        arrange(desc(adist), desc(n))

      payees <-
        withDistance %>%
        filter(nchar(payee) > 0) %>%
        group_by(payee) %>%
        summarize(adist = sum(adist ** 2), n = n()) %>%
        arrange(desc(adist), desc(n))

      toClassify$category <- categories$category[1]
      toClassify$payee    <- payees$payee[1]
      toClassify
    }, .expand = FALSE)

  classified$X1 <- NULL
  classified
}


splitNewData <- function (existingData, newData)
{
  # here we will store those that do not overlap; these are candidates
  # for new operations

  notSeen <- newData


  # first, identify the overlap; it will be used to later generate
  # the "category" and "payee" tags;
  # combine sets row-wise, that is, if there are multiple rows with
  # given (date, amount) pair only those that have a match in the
  # other set - unlike the JOIN operation, which can inflate the
  # size of data

  X <- alply(existingData, 1, function (row) {
    i   <- which(with(notSeen, date == row$date & amount == row$amount))
    if (!length(i)) return(NULL)

    i   <- i[[1]]
    ret <- c(row, select(notSeen[i, ], -date, -amount))
    notSeen <<- notSeen[-i, ]
    ret
  })

  overlap <-
    X[!vapply(X, is.null, logical(1))] %>%
    ldply(function(x) as.data.frame(x, stringsAsFactors = FALSE)) %>%
    select(-.id) %>%
    tbl_df

  list(overlap = overlap, notSeen = notSeen)
}


processData <- function (accountName, existingDataFile, newDataFile, newerThan)
{
  # read existing database & the batch of potentially new operations

  existingData <-
    read.csv(existingDataFile, sep = ';', stringsAsFactors = FALSE) %>%
    tbl_df %>%
    filter(account == accountName) %>%
    select(date, account, amount, payee, category) %>%
    mutate(date = ymd(date)) %>%
    arrange(date, amount)

  newData <-
    read.csv(newDataFile, stringsAsFactors = FALSE) %>%
    tbl_df %>%
    select(date = Posting.Date, amount = Amount, comment = Description) %>%
    mutate(date = mdy(date)) %>%
    arrange(date, amount)

  split <- splitNewData(existingData, newData)

  # limit notSeen given the threshold date; this is required because
  # split transactions cannot be matched and thus will produce a lot
  # of false positives
  notSeen   <- filter(split$notSeen, date > newerThan)

  classifiedData <- classify(split$overlap, notSeen)

  write.csv2(classifiedData, paste0('new-', accountName, '.csv'), row.names = FALSE)

  invisible(classifiedData)
}
