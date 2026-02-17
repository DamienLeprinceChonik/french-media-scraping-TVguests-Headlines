# =============================================================================
# Title: Scraping le débat du 7-10h FranceInter emission Archives
# Author: Damien Leprince-Chonik
# Date: 2025-05-05
# Description:
#   This script scrapes emissions metadata (title, subtitle, guests, guests radio presentation, date, link)
#   from FranceInter archives of the emission "Le débat du 7-10h"
#
# Technical point:
#   - Name of the guest is taken from the description of the emission (following a pattern described below) 
#
# Data Source:
#   "https://www.radiofrance.fr/franceinter/podcasts/le-debat-du-7-10?p="
#
# Notes:
#   - Intended for academic/research purposes only.
#
# =============================================================================

# =============================================================================
# 1. Load Required Packages
# =============================================================================

library(rvest) 
library(stringr)
library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(writexl)
# =============================================================================
# 2. --- Archive URLs Definition ---
# =============================================================================

archives_url <- c(
  "https://www.radiofrance.fr/franceinter/podcasts/le-debat-du-7-10",
  paste0("https://www.radiofrance.fr/franceinter/podcasts/le-debat-du-7-10?p=", 2:22) # Adjusting range
)

# =============================================================================
# 3. --- Function: Extract Episode Links from Archive Pages ---
# =============================================================================

get_episode_links <- function(page_url) {
  page <- read_html(page_url) #To read HTML page
  liens <- page %>% html_elements("a") %>% html_attr("href") #To extract all the links of the web_page. We collect all the balises <a> (balises of links) and the attribute href (qui contient l'URL)
  #We have all the links now we want to keep the ones leading to emissions
  liens_episodes <- liens %>%
    str_subset("^/franceinter/podcasts/le-debat-du-7-10/le-debat-du-7-10-du-") %>%
    paste0("https://www.radiofrance.fr", .)
  return(liens_episodes)
}

# =============================================================================
# 4. --- Collect All Episode URLs ---
# =============================================================================

all_episodes_links <- unlist(lapply(archives_url, get_episode_links))

# =============================================================================
# 5. --- Function: Extract Information from an Episode Page ---
# =============================================================================

extract_info_episode <- function(url) {
  tryCatch({
    page <- read_html(url)
    titre <- page %>% html_element("h1.CoverEpisode-title") %>% html_text(trim = TRUE)
    date <- page %>% html_element("p.CoverEpisode-publicationInfo") %>% html_text(trim = TRUE)
    description <- page %>% html_element("p.ExpressionSummary-standFirst strong") %>% html_text(trim = TRUE)
    invites <- page %>% html_elements("div.Expression-guests li span.qg-st4") %>% html_text(trim = TRUE)
    invite_descriptions <- page %>% html_elements("div.Expression-guests li span.Expression-guests-role") %>% html_text(trim = TRUE)
    tibble(
      url = url,
      titre = titre,
      date = date,
      description = description, 
      invite = paste(invites, collapse = " / "),
      invite_description = paste(invite_descriptions, collapse = " / ")
    )
  }, error = function(e) {
    message("Erreur avec l'URL : ", url)
    return(tibble(
      url = url,
      titre = NA,
      date = NA,
      description = NA,
      invite = NA,
      invite_description = NA
    ))
  })
}

# =============================================================================
# 6. --- Scrape All Episodes ---
# =============================================================================

donnees_emissions <- lapply(seq_along(all_episodes_links), function(i) {
  message("Scraping ", i, " / ", length(all_episodes_links))
  extract_info_episode(all_episodes_links[[i]])
})
Ledébatdu07_10FranceInter_raw <- bind_rows(donnees_emissions)

# =============================================================================
# 7. --- Date Cleaning and Formatting ---
# =============================================================================

Ledébatdu07_10FranceInter_raw <- Ledébatdu07_10FranceInter_raw %>%
  mutate (date = str_extract(date, "\\d{1,2} \\w+ \\d{4}"),
          date_formate = dmy(date))%>%
  select(url, titre, date, date_formate, invite, invite_description, description)

Ledébatdu07_10FranceInter_long <- Ledébatdu07_10FranceInter_raw %>%
  mutate(
    invite_list = str_split(invite, "/"),
    invite_description_list = str_split(invite_description, "/")
  ) %>%
  unnest(c(invite_list, invite_description_list)) %>%
  select(-invite, -invite_description)%>%
  rename(invite = invite_list,
         invite_description = invite_description_list)%>%
  mutate(
    invite = str_trim(invite),
    invite_description = str_trim(invite_description)
  )
