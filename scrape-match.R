#install.packages("rvest")
#install.packages("GGally")

library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(GGally)

setwd("~/accellerator/football-scraping/")

scrape_match <- function(url) {

  football <- read_html(url)
  details <- football %>% html_nodes("dl")
  referees <- details[4] %>% html_nodes("dd a")
  content <- football %>% html_nodes("#page_match_1_block_match_info_4")
  
  # Warning: The order of elements might change 
  # content %>% html_nodes("dt") %>% html_text
  
  match_details <- content %>% html_nodes("dd") %>% html_text
  
  data.frame (
    team_home = content %>% html_nodes(".container.left h3") %>% html_text,
    team_away = content %>% html_nodes(".container.right h3") %>% html_text,
    result = content %>% html_nodes(".thick.scoretime") %>% html_text,
    competition = match_details[1],
    date = match_details[2],
    game_week = match_details[3],
    kick_off = match_details[4],
    half_time = match_details[5],
    full_time = match_details[6],
    venue = match_details[7],
    referee_name = referees[1] %>% html_text,
    referee_href = referees[1] %>% html_attr("href")
  )  %>% lapply(str_trim)

}

scrape_match_by_number <- function(number) {
  scrape_match(paste0("http://int.soccerway.com/matches/9999/99/99/some-league/serie-a/team1/team2/", number))
}

for (i in 2120390:2120000) {
  print(paste("get match", i))
  df = rbind(df, scrape_match_by_number(i))
  Sys.sleep(0.2)
}
