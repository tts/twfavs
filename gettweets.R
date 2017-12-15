library(googlesheets)
library(dplyr)
library(twitteR)

# TO DO 15.12.2017
# Edit to reflect what's been done after the first round
# - process only rows after the last 'Done up to here' line
# - rbind with the older data set
# - add a new 'Done up to here' line

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

# Extract Twitter status ID
favs <- favs %>% 
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

# If the url is a short one, expand it, and extract ID
favs <- favs %>% 
  rowwise() %>% 
  mutate(longurl = if( is.na(statusid) )  getLongURL.curl(url) else url ) %>% 
  mutate(id = sapply(longurl, function(x) { strsplit(x,"/")[[1]][6] }))

write.csv(favs, "favs.csv", row.names = F)

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

favs$id <- as.character(favs$id)

# Sys.sleep to deal with rate limits
# https://dev.twitter.com/rest/public/rate-limits
for (i in 1:nrow(favs)){
  message("Getting text to ", favs$id[i], "\r")
  flush.console()

  tryCatch({
    favs$statustext[i] <- showStatus(favs$id[i])$text
  }, error = function(e){
    message(':(')
  })
  Sys.sleep(1)
}

write.csv(favs, "favs_with_status.csv", row.names = FALSE)

#############################################
#
# To the app
#
# "tweet","date","text"
#
#############################################

Sys.setlocale("LC_TIME", "C")

to_app <- favs %>% 
  rename(tweet = longurl,
         text = statustext) %>%
  mutate(date_ext = gsub("(.*) at.*", "\\1", date), 
         date = as.Date(date_ext, "%B %d, %Y")) %>% 
  select(tweet, date, text)

write.csv(to_app, "to_app.csv", row.names = FALSE)  

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
