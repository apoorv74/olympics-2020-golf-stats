
########################################################################################
########################################################################################
###                                                                                  ###
###                                                                                  ###
###                                                                                  ###
###  SCRIPT TO FETCH STATISTICS FROM THE OLYMPICS WEBSITE - HTTPS://OLYMPICS.COM/.   ###
###                                                                                  ###
###                                  SPORT - GOLF                                    ###
###                      EVENT - WOMENS INDIVIDUAL STROKE PLAY                       ###
###                   EVENT TIMELINE - AUG 04, 2021 - AUG 07, 2021                   ###
###                                                                                  ###
###                                                                                  ###
########################################################################################
########################################################################################



# Libraries ---------------------------------------------------------------

library(rvest)
library(tidyverse)
library(glue)
library(pbapply)


# Paths -------------------------------------------------------------------

scorecard_url <- "https://olympics.com/tokyo-2020/olympic-games/en/results/golf/glfr073a-women-s-individual-stroke-play-fnl-.htm"
scorecard_path <- ".d-none .competitor-tab-container .glf-rounds-table"
id_path <- ".d-none .competitor-tab-container ul"
stats_path <- ".competitor-tab-container .tab-content"


# Functions ---------------------------------------------------------------

get_all_scorecard <- function(){
  id_data <-
  scorecard_url %>% read_html() %>% html_nodes(css = id_path) %>% html_attr("competitor")
  scorecard_data <- scorecard_url %>% read_html() %>% html_nodes(css = scorecard_path) %>% html_table() %>% bind_rows()
  id_to_attach <- c()
  for(i in 1:length(id_data)){
    id_to_attach <- c(id_to_attach, rep(id_data[i],6))
  }
  scorecard_data$player_id <- id_to_attach
  par_data <- scorecard_data[scorecard_data$Hole == "Par",]
  par_data$player_id <- NULL
  par_data <- unique(par_data)
  scorecard_data <- scorecard_data[!scorecard_data$Hole %in% c("Par","Meters\rYards"),]
  player_scores <- list("par_data"=par_data, "scorecard_data"=data.frame(scorecard_data))
  return(player_scores)
}

# player_id <- "1312000"

fetch_player_statistics <- function(player_id){
  css_id <- glue::glue("{stats_path} #statistics-{player_id}")
  player_url <- scorecard_url %>% read_html() %>% html_nodes(css = css_id) %>% html_attr("data-url")
  player_url <- stringr::str_replace_all(string = player_url, pattern = "../../..",replacement = "https://olympics.com/tokyo-2020/olympic-games")  
  
  player_stats <- player_url %>% read_html() %>% html_nodes(css = "table") %>% html_table() %>% data.frame(check.names = FALSE, stringsAsFactors = FALSE)
  names(player_stats)[] <- c("Stats",paste0("Round",c(1:4)),"Total")
  player_stats <- player_stats[player_stats$Stats!='',]
  player_stats$player_id <- player_id
  return(player_stats)
}


# Calls -------------------------------------------------------------------

scores <- get_all_scorecard()

player_scorecard <- scores$scorecard_data
par_score <- scores$par_data
all_player_ids <- player_scorecard$player_id %>% unique()

all_player_stats <- pbapply::pblapply(X = all_player_ids, fetch_player_statistics)
all_player_stats <- bind_rows(all_player_stats)

# Export Files ------------------------------------------------------------

readr::write_csv(all_player_stats,"data/all_player_stats.csv")
readr::write_csv(par_score,"data/par_score.csv")
readr::write_csv(player_scorecard,"data/player_scorecard.csv")

