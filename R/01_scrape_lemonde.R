# =============================================================================
# Title: Scraping Le Monde Archives
# Author: Damien Leprince-Chonik
# Date: 2025-11-16
# Description:
#   This script scrapes article metadata (title, subtitle, date, author, link)
#   from Le Monde's daily archives between selected dates.
#
# Data Source:
#   https://www.lemonde.fr/archives-du-monde/
#
# Notes:
#   - Scraping respects polite delays (Sys.sleep).
#   - The script handles pagination and possible read errors.
#   - Intended for academic/research purposes only.
# =============================================================================

# =============================================================================
# 1. Load Required Packages
# =============================================================================

library(rvest)
library(dplyr)
library(purrr)
library(lubridate)

# =============================================================================
# 2. Define Date Range
# =============================================================================
# WARNING:
# Scraping large date ranges can be time-consuming and server-intensive, please adjust the date range responsibly

dates <- seq(
  from = as.Date("2026-01-30"), #Please define the range here
  to   = as.Date("2026-01-31"),
  by   = "day"
)

# Convert dates to French archive URL format (dd-mm-yyyy)
dates_fr <- format(dates, "%d-%m-%Y") # Transforming dates to the website URLs format

# Construct archive URLs
daily_URLs <- paste0(
  "https://www.lemonde.fr/archives-du-monde/", 
  dates_fr, 
  "/"
)

# =============================================================================
# 3. Function: Detect Pagination for a Given Archive Day and return subpage URLs
# =============================================================================
# Description:
# Checks whether an archive page contains pagination and
# returns all associated subpage URLs.
#
# Parameter:
#   url (character) – archive URL for a specific day
#
# Returns:
#   Character vector of URLs (including pagination pages if present)

get_daily_subpages <- function(url) {
  message("Checking pagination for ", url)
  
  page <- tryCatch(
    read_html(url),
    error = function(e) {
      message("Error on ", url)
      return(NULL)
    }
  )
  
  if (is.null(page)) return(NULL)

  # Extract pagination links
  links <- page %>%
    html_elements("a.river__pagination.river__pagination--page") %>%
    html_attr("href")
  
  if (length(links) == 0) {
    return(url)
  }
  
  # If no pagination exists, return original URL
  full_links <- ifelse(
    startsWith(links, "http"),
    links,
    paste0("https://www.lemonde.fr", links)
  )
  
  # Convert relative URLs to absolute
  full_links <- unique(full_links)
  Sys.sleep(runif(1, 1, 2)) # polite delay
  
  return(full_links)
}

# Collect all subpages across date range
sub_daily_URLs <- map(daily_URLs, get_daily_subpages) %>%
  flatten_chr() %>%
  unique()

# =============================================================================
# 4. Function: Extract Article Metadata from Teaser Block
# =============================================================================
# Description:
# Extracts metadata from a single article teaser section.
#
# Parameter:
#   article (xml_node) – teaser section node
#
# Returns:
#   Tibble with article metadata

extracting_info <- function(article) {
  titre <- article %>%
    html_element("h3.teaser__title") %>%
    html_text(trim = TRUE)
  
  sous_titre <- article %>%
    html_element("p.teaser__desc") %>%
    html_text(trim = TRUE)
  
  date <- article %>%
    html_element("span.meta__date") %>%
    html_text(trim = TRUE)
  
  auteur <- article %>%
    html_element("span.meta__author") %>%
    html_text(trim = TRUE)
  
  # Control for relative or absolute link
  lien <- article %>% html_element("a.teaser__link") %>% html_attr("href")
  lien <- ifelse(startsWith(lien, "http"), lien, paste0("https://www.lemonde.fr", lien))
  
  tibble(
    titre = titre,
    sous_titre = sous_titre,
    date = date,
    auteur = auteur,
    lien = lien
  )
}

# =============================================================================
# 5. Function: Scrape a Single Archive Page
# =============================================================================
# Description:
# Scrapes all article teaser blocks from a given archive URL.
#
# Parameter:
#   url (character) – archive or pagination URL
#
# Returns:
#   Tibble containing article metadata for that page

scrape_day <- function(url) {
  message("Scraping ", url)
  
  # Trying to read pages
  page <- tryCatch(read_html(url), error = function(e) {
    message("Erreur lecture sur ", url)
    return(NULL)
  })
  
  if (is.null(page)) return(NULL)
  
  # Each articles are in the CSS tag ".teaser": <section class="teaser teaser--inline-picture"
  articles <- page %>% html_elements("section.teaser")
  
  if (length(articles) == 0) {
    message("No article founds ", url)
    return(NULL)
  }
  
  # Applying the function
  data_day <- map_dfr(articles, extracting_info)
  
  # Adding the date of the archive to keep the source 
  data_day$date_archive <- url
  
  Sys.sleep(runif(1, 1, 2)) # polite delay
  
  return(data_day)
}

# =============================================================================
# 6. Run Scraping Pipeline
# =============================================================================

LeMonde_archive_data <- map_dfr(sub_daily_URLs, scrape_day)
