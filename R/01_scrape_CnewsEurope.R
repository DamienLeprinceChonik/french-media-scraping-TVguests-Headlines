# =============================================================================
# Title: Scraping Sonia Mabrouk emission Archives
# Author: Damien Leprince-Chonik
# Date: 2025-05-11
# Description:
#   This script scrapes emissions metadata (title, subtitle, guests, date, link)
#   from Europe 1/Cnews archives of the emission "linterview politique de Sonia Mabrouk"
#
# Technical point:
#   - Two functions have been constructed due to differences in the structure of the archive.
#   - Name of the guest is taken from the description of the emission and its description 
#     is taken (following a pattern described below) as a proxy of the television banner presentation which is of interest.
#
# Data Source:
#   https://www.europe1.fr/emissions/linterview-politique-de-8h20?page=
#
# Notes:
#   - Intended for academic/research purposes only.
#
# WARNING: 
#   Cnews/Europe 1 website might change its structure (code done in May 2025), small adjustment might be needed. 
# =============================================================================

# =============================================================================
# 1. Load Required Packages
# =============================================================================

library(rvest)
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(writexl)

# ========================
# 2. --- Function to scrape pages 1-10 (descriptions included) ---
# ========================

get_episodes_with_desc_in_list <- function(page_num) {
  # Build the URL for the requested page
  url <- paste0("https://www.europe1.fr/emissions/linterview-politique-de-8h20?page=", page_num)
  page <- read_html(url)
  
  # Select all episode cards on the page
  episodes <- page %>% html_nodes(".episode-card")
  
  # Extract information for each episode
  map_df(episodes, function(ep) {
    titre <- ep %>% html_node(".episode-name a") %>% html_text(trim = TRUE)
    lien <- ep %>% html_node(".episode-name a") %>% html_attr("href")
    date_text <- ep %>% html_nodes(".episode-details span") %>% tail(1) %>% html_text(trim = TRUE)
    
    desc <- ep %>% html_node(".episode-description p") %>% html_text(trim = TRUE)
    
    # Complete relative links to full URLs
    lien <- paste0("https://www.europe1.fr", lien)
    
    # Return a tibble
    tibble(titre = titre, lien = lien, date_text = date_text, description = desc)
  })
}

# ========================
# 3. --- Function to scrape pages 11-37 (descriptions extracted separately) ---
# ========================

get_episodes_without_desc <- function(page_num) {
  url <- paste0("https://www.europe1.fr/emissions/linterview-politique-de-8h20?page=", page_num)
  page <- read_html(url)
  
  episodes <- page %>% html_nodes(".episode-card")
  
  map_df(episodes, function(ep) {
    titre <- ep %>% html_node(".episode-name a") %>% html_text(trim = TRUE)
    lien <- ep %>% html_node(".episode-name a") %>% html_attr("href")
    date_text <- ep %>% html_nodes(".episode-details span") %>% tail(1) %>% html_text(trim = TRUE)
    
    lien <- paste0("https://www.europe1.fr", lien)
    
    tibble(titre = titre, lien = lien, date_text = date_text)
  })
}

# ========================
# 3.1 --- Function to retrieve episode description from its link ---
# ========================

get_description_from_episode <- function(episode_url) {
  page <- read_html(episode_url)
  
  # CSS selector for the description paragraph
  desc <- page %>% 
    html_node("section#about .about__wrapper.rte p:nth-child(3)") %>%
    html_text(trim = TRUE)
  
  if (is.null(desc) || desc == "") return(NA_character_)
  desc
}

# ========================
# 4. --- Collect data for all pages
# ========================

# 4.1 To collect data with description (1-10)
pages_1_10 <- 1:10
episodes_1_10 <- map_df(pages_1_10, get_episodes_with_desc_in_list)

# 4.2 To collect data with description on emissions (11-37)
pages_11_37 <- 11:37
episodes_11_37_basic <- map_df(pages_11_37, get_episodes_without_desc)

# 4.3 To add description
episodes_11_37 <- episodes_11_37_basic %>%
  mutate(description = map_chr(lien, possibly(get_description_from_episode, otherwise = NA_character_)))

# 4.4 To merge data set
all_episodes <- bind_rows(episodes_1_10, episodes_11_37) %>%
  mutate(date = dmy(date_text)) %>%
  select(-date_text)

# ========================
# 5. --- Automatic cleaning for properly identifying guests and description of the guest ---
# ========================

emissions_SoniaMabroukInterview_clean_automatically = all_episodes %>%
  mutate(
    invite = str_extract(description, "^[^,]+"),
    bandeau = str_extract(description, "(?<=, ).*?(?=\\s+r[eé]pond)")) %>%
  distinct(date, invite, bandeau, .keep_all = TRUE) %>%
  mutate(bandeau = str_remove(bandeau, ",\\s*$"))%>%
  slice_head(n = 356) # To keep only emission that were both on Europe 1 and Cnews
