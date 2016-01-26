#install.packages("rvest")
#install.packages("GGally")

library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(GGally)

setwd("~/accellerator/flight-ticket-scraping/")

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

match <- scrape_match("http://int.soccerway.com/matches/2016/01/16/italy/serie-a/atalanta-bergamo/fc-internazionale-milano/2120570/?ICID=PL_MS_01")
