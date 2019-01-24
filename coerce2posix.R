dates <- c('2017-01-01', '2009-10-09 13:25')

dates <- data.frame(dates = c('2017-01-01', '2009-10-09 13:25'))

dim(dates$dates)

coerce2posix(dates)

coerce2posix <- function(dates) {

  if (!is.null(dim(dates))) {

    print('gimme a vector, fool!')
    
  } else {
    
    if()
    
  }

}
