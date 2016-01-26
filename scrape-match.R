#install.packages("rvest")
#install.packages("GGally")

library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(GGally)
library(testthat)

setwd("~/accellerator/football-scraping/")

scrape_match <- function(url) {

  football <- read_html(url)
  content <- football %>% html_nodes("#page_match_1_block_match_info_4")
  
  # Warning: The order of elements might change 
  # content %>% html_nodes("dt") %>% html_text
  
  match_details <- content %>% html_nodes("dd") %>% html_text
  match_details_headers <- content %>% html_nodes("dt") %>% html_text
  
  
  details <- football %>% html_nodes("dl")
  referees <- tryCatch({details[4] %>% html_nodes("dd a")},error=function(err){return(NA)})
  referees_header <- tryCatch({details[4] %>% html_nodes("dt")},error=function(err){return(NA)})
  
  test <- list()
  for(i in 1:length(match_details)){
    test[[match_details_headers[i]]] <- match_details[i]
  }
  
  if(!is.na(referees[1])){
    referee_name <- referees[1] %>% html_text
    referee_header <- referees_header[1] %>% html_text
    test[[referee_header]] <- referee_name
  }
  
  test %>% lapply(str_trim)
}

scrape_match_by_number <- function(number) {
  scrape_match(paste0("http://int.soccerway.com/matches/9999/99/99/some-league/serie-a/team1/team2/", number))
}

df <- data.frame()
for (i in 2120375:2120000) {
  print(paste("get match", i))
  match <- scrape_match_by_number(i)
  dftest <- data.frame(match)
  df <- bind_rows(df,dftest)
}

# test_that("team home is a character array", {
#   expect_equal(is.character(match$team_home), TRUE)
#   expect_equal(is.character(match$team_away), TRUE)
#   expect_equal(is.character(match$result), TRUE)
# })
