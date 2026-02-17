# =============================================================================
# Title: Scraping Les Informés emission Archives
# Author: Damien Leprince-Chonik
# Date: 2025-05-05
# Description:
#   This script scrapes emissions metadata (title, subtitle, guests, guests tv banner, date, link)
#   from FranceInfo archives of the emission "Les Informés"
#
# Technical point:
#   - Name of the guest is taken from the description of the emission and its description 
#     is taken (following a pattern described below) as a proxy of the television banner presentation which is of interest.
#
# Data Source:
#   "https://www.franceinfo.fr/replay-radio/les-informes-de-france-info/"
#
# Notes:
#   - Intended for academic/research purposes only.
#
# =============================================================================

# =============================================================================
# 1. Load Required Packages
# =============================================================================

library(rvest)
library(httr)
library(purrr)
library(dplyr)
library(stringr)
library(writexl)

# =============================================================================
# 2. --- Define Archive Structure ---
# =============================================================================

base_url <- "https://www.franceinfo.fr/replay-radio/les-informes-de-france-info/"
pages <- paste0(base_url, 1:14, ".html")

# =============================================================================
# 3. --- Function: Extract Episode Links from Archive Page ---
# =============================================================================

get_links_from_page <- function(url) {
  
  tryCatch({
    page <- read_html(url)
    links <- page %>%
      html_nodes("a") %>% #links
      html_attr("href") %>% #url of links
      na.omit() %>%
      unique()%>%
      str_subset("^/replay-radio/les-informes-de-france-info/.+_[0-9]+\\.html$") %>%
      paste0("https://www.franceinfo.fr", .)
    return(links)
  }, error = function(e) {
    message(sprintf("Erreur avec l'URL %s : %s", url, e$message))
    return(character(0))
  })
}

# =============================================================================
# 4. --- Collect All Episode Links ---
# =============================================================================

all_links_toward_emission <- c()  # Initialisation

for (i in 1:14) {
  url <- paste0(base_url, i, ".html")
  message(sprintf("Scraping page %d / 149 : %s", i, url))
  links <- get_links_from_page(url)
  all_links_toward_emission <- c(all_links_toward_emission, links)
}

# =============================================================================
# 5. --- Function: Scrape Episode Information ---
# =============================================================================

scrap_emission_info <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Extract title
    titre <- page %>% html_node(".rf-player-wrapper__title-text") %>% html_text(trim = TRUE)
    
    # Extract datetime attribute
    date <- page %>% html_node("time") %>% html_attr("datetime")
    
    # Identify sections related to guests (H2 with "invité" or "intervenant")
    h2_nodes <- page %>% html_nodes("h2.bullet")
    h2_match <- which(str_detect(html_text(h2_nodes), regex("invité|intervenant", ignore_case = TRUE)))
    
    # If no guest section found, return empty guest info
    if (length(h2_match) == 0) {
      return(tibble(url = url, titre = titre, date = date, invite = NA, bandeau = NA))
    }
    
    # Extract paragraphs following the first matching H2
    invited_paragraphs <- h2_nodes[h2_match[1]] %>%
      html_nodes(xpath = "following-sibling::p[position() <= 7]")
    
    # Extract guest names and descriptions
    invite <- invited_paragraphs %>%
      map_dfr(function(p) {
        nom <- p %>% html_node("strong") %>% html_text(trim = TRUE)
        description <- p %>% html_text(trim = TRUE) %>%
          str_remove(paste0("^", nom)) %>% str_trim()
        tibble(invite = nom, bandeau = description)
      })
    
    invite <- invite %>% mutate(url = url, titre = titre, date = date) %>%
      select(url, titre, date, invite, bandeau)
    
    return(invite)
    
  }, error = function(e) {
    message(sprintf("Erreur pour %s : %s", url, e$message))
    return(tibble(url = url, titre = NA, date = NA, invite = NA, bandeau = NA))
  })
}

# =============================================================================
# 6. --- Scrape All Episodes ---
# =============================================================================

test_result <- map_dfr(all_links_toward_emission, scrap_emission_info)

# =============================================================================
# 7. --- Data Cleaning & Post-Processing ---
# =============================================================================

emissions_Informes_date_clean <- test_result %>%
  mutate (
    date = ymd_hms(date),
    date = as_date(date)) %>%
  filter (date > as.Date("2018-03-02")) %>% #Beyond this date invites were not properly scrapped
  mutate ( 
    invite = if_else(
      is.na(invite) & !is.na(bandeau),
      str_extract(bandeau, "^[^,]+"), invite)) %>%# To extract what there is before the first ","
  filter (
    !str_detect(invite, "-{3,}"),
    !str_detect(bandeau, "-{3,}"),
    !(str_trim(invite) == "" & str_trim(bandeau) == "")) %>% #To delete lines with "---------"
  filter(
    !str_detect(invite, regex("intégralité", ignore_case = TRUE)),
    !str_detect(bandeau, regex("intégralité", ignore_case = TRUE)) # To delete badly scraped lines
  ) %>%
  mutate ( bandeau = str_remove(bandeau, "^,\\s+"),
           bandeau = str_remove(bandeau, "^-\\s+"),
           bandeau = str_remove(bandeau, "^:\\s+"),
           invite = str_remove(invite, "^-\\s+"),
           invite = str_remove_all(invite, ","),
           invite = str_remove_all(invite, ":"))%>%
  filter(
    (str_length(invite) >= 4 & str_length(invite) <= 27) | is.na(invite) # After checking longer and shorter name manually, removing data that are not a name
  )
