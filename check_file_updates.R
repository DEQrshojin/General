check_file_updates <- function(files) {

  library(DescTools)
  
  options(warn=-1)
  
  # remove rows with <DIR>, blank rows, 'Volume', or 'bytes'
  files <- files[-which(grepl('<DIR>|^\\s*$|Volume|bytes', files))]
  
  # Coerce to data frame
  files <- data.frame(files = files, stringsAsFactors = FALSE)
  
  # Record directory in a second column
  files$dir <- NA
  
  for (i in 1 : nrow(files)) {
  
    if(grepl('Directory of', files[i, 1])) {
      
      files[i, 2] <- files[i, 1] 
      
    } else {
      
      files[i, 2] <- files[i - 1, 2] 
      
    }
  }
  
  # Remove instances in first column of 'Directory of...'
  dirLines <- which(grepl('Directory of', files$files))
  
  files <- files[-dirLines, ]
  
  # Remove 'Directory of ' in column 2
  files$dir <- gsub('Directory of ', '', files$dir)
  
  # Replace commas in the first column with nothing
  files$files <- gsub(',', '', files$files)
  
  tmp <- files$files
  
  tmp <- gsub('AM', 'AM,', tmp)
  
  tmp <- gsub('PM', 'PM,', tmp)
  
  tmp <- data.frame(do.call('rbind', strsplit(tmp, ',')),
                    stringsAsFactors = FALSE)
  
  if (length(tmp) > 2) {tmp <- tmp[, 1 : 2]}
  
  names(tmp) <- c('Date', 'Name')
  
  tmp$Name <- trimws(tmp$Name, which = 'both')
   
  firstSpace <- StrPos(tmp$Name, pattern = ' ', pos = 1)
  
  spltLoc <- function(string, location) {
    
    fileSets <- c(substr(string, 1, location - 1),
                  substr(string, location + 1, nchar(string)))
                     
    return(fileSets)
    
  }
  
  tmp2 <- apply(X = data.frame(tmp$Name, stringsAsFactors = FALSE),
                MARGIN = 2, FUN = spltLoc, location = firstSpace)
  
  tmp2 <- data.frame(flSz = tmp2[1 : (nrow(tmp2) / 2), ],
                     flNm = tmp2[(1 + nrow(tmp2) / 2) : nrow(tmp2), ],
                     stringsAsFactors = FALSE)
  
  tmp3 <- data.frame(cbind(tmp$Date, as.numeric(tmp2$flSz), tmp2$flNm),
                     stringsAsFactors = FALSE)
  
  names(tmp3) <- c('Date', 'flSz', 'flNm')
  
  # Coerce dates to posixct
  tmp3$Date <- as.POSIXct(tmp3$Date, 
                          '%m/%d/%Y %H:%M %p', 
                          tz = 'America/Los_Angeles')
  
  tmp3$flSz <- as.numeric(tmp3$flSz) / 1024
  
  return(tmp3)

}
