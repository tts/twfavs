library(googlesheets)
library(tidyverse)
library(twitteR)

# Read sheet data from GDrive
my_sheets <- gs_ls()

# With the sheet_key of all those rows 
# where the sheet_title starts with 'New favorite tweet',
# download all items
fav_sheets <- my_sheets %>% 
  filter(grepl('New favorite tweet', sheet_title)) 

tweets_in_sheet <- function(key = NULL){
  tw <- gs_key(as.character(key))
  gs_ws_ls(tw) 
  tweets <- tw %>% 
    gs_read(col_names = c("date", "name", "ifttt", "url"))
}

# Bind all rows
tw_l <- lapply(fav_sheets$sheet_key, function(x) tweets_in_sheet(x))
favs <- do.call(rbind, tw_l)

# We don't want to rerun rows we've already processed,
# so first subset to those that come after the (last) one saying
# "Done up to here"
#
# So first first, sort by date.
#
# Due to my initial mistake of using the default Win locale,
# replacing that value here
favs[favs$date == "kesäkuu 18, 2017 at 15:02",]$date <- "June 18, 2017 at 3:02PM"

Sys.setlocale("LC_TIME", "English")

favs <- favs %>% 
  rowwise() %>% 
  mutate(date = as.Date(date, "%B %d, %Y")) %>% 
  arrange(date) 

doneuptohere_rows <- which(grepl("^Done up to here$", favs$ifttt))
last_doneuptohere_row <- doneuptohere_rows[length(doneuptohere_rows)]
firstnewrow <- last_doneuptohere_row + 1

# Take the newest rows only
newfavs <- favs %>% 
  slice(firstnewrow:n())

# Extract Twitter status ID
newfavs <- newfavs %>% 
  mutate(statusid = sapply(url, function(x) { strsplit(x,"/")[[1]][6] }))

# https://www.r-bloggers.com/unshorten-urls-in-r/
getLongURL.curl <- function(shorturl){
  # uses curl statement to get expanded url from t.co links (or any link)
  # loop through until there's no location attribute... that's the long link.
  message("Getting new url to ", shorturl, "\r")
  flush.console()
  
  newurl <- shorturl
  url <- ""
  while(url != newurl){
    data <- system(paste0("curl -I ", newurl), intern=T)
    if(sum(grepl("location: ", tolower(data))) == 0){
      url <- newurl
    }else{
      data <- subset(data, tolower(substring(data, 1, 9))=="location:")
      stringurl <- substring(data[1], 11, nchar(data[1])-1)
      # sometimes the location data isn't a url.
      if(substring(stringurl, 1, 4)=="http"){ 
        newurl <- stringurl
      }else{
        url <- newurl
      }
    }
  }
  return(newurl)
}

newfavs <- newfavs %>% 
  rowwise() %>% 
  mutate(longurl = if( is.na(statusid) )  getLongURL.curl(url) else url ) %>% 
  mutate(id = sapply(longurl, function(x) { strsplit(x,"/")[[1]][6] }))

write.csv(favs, paste0("favs", Sys.Date(), ".csv"), row.names = F)

##########################
#
# Get the tweet text
#
##########################

my_key <- ""
my_secret <- ""
my_access_token <- ""
my_access_secret <- ""
setup_twitter_oauth(my_key, my_secret, my_access_token, my_access_secret)

newfavs$id <- as.character(newfavs$id)

# Sys.sleep to deal with rate limits
# https://dev.twitter.com/rest/public/rate-limits
for (i in 1:nrow(newfavs)){
  message("Getting text to ", newfavs$id[i], "\r")
  flush.console()

  tryCatch({
    newfavs$statustext[i] <- showStatus(newfavs$id[i])$text
  }, error = function(e){
    message(':(')
  })
  Sys.sleep(1)
}

write.csv(newfavs, paste0("favs_with_status", Sys.Date(), ".csv"), row.names = FALSE)

###########################
#
# To the app
#
# "tweet","date","text"
#
###########################

to_app <- newfavs %>% 
  rename(tweet = longurl,
         text = statustext) %>%
  select(tweet, date, text)

# Merge with previous sets
# TO DO: read all files based on name, here still just one
olderfavs <- read.csv("to_app.csv", stringsAsFactors = F)
olderfavs$date = as.Date(olderfavs$date, "%Y-%m-%d")

to_app_all <- rbind(olderfavs, to_app)
write.csv(to_app_all, paste0("to_app_all", Sys.Date(), ".csv"), row.names = FALSE)  

#################################################################
#
# Add a row in the latest spreadsheet to denote 
# from where to start the next run.
#
# Select the sheet with the newest datetime value in column 'updated'
#
# input: c("date", "name", "ifttt", "url")
################################################################

latest_sheet_updated <- max(fav_sheets$updated)
latest_sheet_key <- as.character(fav_sheets[fav_sheets$updated == latest_sheet_updated, 
                                            c("sheet_key")])

# Register sheet
l <- gs_key(latest_sheet_key)

# Build row data
now <- iconv(format(Sys.time(), "%B %d, %Y at %R"), 
             from = "ISO-8859-1", to = "UTF-8")
row_text <- c(now, "@ttso", "Done up to here", "")

# Add row to sheet
l <- l %>% 
  gs_add_row(input = row_text)

# Check
#
#l_all <- l %>% 
# gs_read()
