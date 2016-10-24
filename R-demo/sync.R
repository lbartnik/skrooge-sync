library(plyr)
library(dplyr)
library(lubridate)

existingDataFile <- 'skrooge.csv'
newDataFile      <- 'all-bank.csv'


# outstanding questions:
#
# 1. how to make sure we don't miss any new operation?
# 2. how to make sure we don't lose any "old" operation?
# 3. how do duplicates influence this process? what if there has
#    been an operation that is exactly the same as another and it
#    needs to be added? how to identify such corner cases?



classify <- function (accountName, existingData, newData)
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
    tbl_df
  
  # limit notSeen given the threshold date; this is required because
  # split transactions cannot be matched and thus will produce a lot
  # of false positives
  
  onlyNewer <- ymd('2016-10-01')
  notSeen <- filter(notSeen, date > onlyNewer)
  
  # now try and assign "category" and "payee" to these new operations;
  # in order to do so, look at the `overlap` set and find the set of
  # most similar "old" transactions and out of them choose the most
  # frequest category and payee
  
  classified <- adply(notSeen, 1, function (toClassify) {
    categories <-
      overlap %>%
      mutate(adist = as.vector(adist(toClassify$comment, comment))) %>%
      group_by(category) %>%
      summarize(adist = mean(adist), n = n()) %>%
      arrange(adist, desc(n))
    
    toClassify$guessedCat <- categories$category[1]
    toClassify
  }, .expand = FALSE)
  
  classified
}


processData <- function (accountName, existingDataFile, newDataFile)
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
    select(date = Posting.Date, amount = Amount, comment = Memo) %>%
    mutate(date = mdy(date)) %>%
    arrange(date, amount)
  
  classifiedData <- classify(accountName, existingData, newData)
  
  write.csv2(classifiedData, paste0('new-', accountName, '.csv'))
}
