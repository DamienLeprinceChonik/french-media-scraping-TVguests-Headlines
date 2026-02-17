# =============================================================================
# Title: Scraping Face à Duhameml emission (BFM TV) Archives
# Author: Damien Leprince-Chonik
# Date: 2025-06-06
# Description:
#   This script scrapes emissions metadata (title, subtitle, guests, guests tv banner, date, link)
#   from BFM TV archives of the emission "Face à Duhamel"
#
# Technical point:
#   - Name of the guest is taken from the description of the emission and its description 
#     is taken (following a pattern described below) as a proxy of the television banner presentation which is of interest.
#
# Data Source:
#   "https://www.bfmtv.com/archives/replay-emissions/face-a-duhamel/"
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
library(writexl)
library(readxl)
library(lubridate)

# =============================================================================
# 2. --- Retrieve Monthly Archive Links ---
# =============================================================================
# Archive page listing monthly sub-archives

url_archives <- "https://www.bfmtv.com/archives/replay-emissions/face-a-duhamel/"
page_archives <- read_html(url_archives)

# Extract relative URLs for each monthly archive (YYYY/MM format)
Monthly_URL <- page_archives %>% html_elements("a") %>% 
  html_attr("href") %>%
  str_subset("/archives/replay-emissions/face-a-duhamel/\\d{4}/\\d{2}/") %>%
  unique()

# =============================================================================
# 3. --- Retrieve Episode Links from Each Monthly Page ---
# =============================================================================

base_url <- "https://www.bfmtv.com"

# Extraire les liens complets vers les pages de mois
monthly_full_urls <- paste0(base_url, Monthly_URL)

# Fonction pour récupérer les liens d'émissions d'un mois
get_emissions <- function(mois_url) {
  page_mois <- read_html(mois_url)
  
  # Extraire les liens d'émissions (relatifs), sans les rendre uniques
  emissions <- page_mois %>% html_elements("a") %>%
    html_attr("href") %>%
    str_subset("^/replay-emissions/face-a-duhamel/")  # Pas de unique ici
  
  # Convertir en liens complets
  emissions_full <- paste0(base_url, emissions)
  
  return(emissions_full)
}

# Retrieve all episode URLs
all_emission_urls <- unlist(lapply(monthly_full_urls, get_emissions))

# =============================================================================
# 4. --- Remove Duplicate Language Versions (_VN / _EN) ---
# =============================================================================
# Keep only French video versions (_VN-...)

# Filtrer pour ne garder que les liens qui se terminent par "_VN-..."
all_emission_urls_VN <- all_emission_urls[grepl("_VN-", all_emission_urls)]

# Remove suffix to identify duplicates
urls_sans_suffixe <- str_remove(all_emission_urls_VN, "_[A-Z]{2}-\\d+\\.html")

# Keep first occurrence of each episode
unique_indices <- !duplicated(urls_sans_suffixe)
emissions_uniques_VN <- all_emission_urls_VN[unique_indices]

# =============================================================================
# 5. --- Function: Extract Episode Metadata ---
# =============================================================================

extraire_infos_emission <- function(url) {
  tryCatch({
    page <- read_html(url)
    
    # Titre
    titre <- page %>% html_element("h1") %>% html_text(trim = TRUE)
    
    # Essayons d'abord dans le span connu
    description <- page %>%
      html_element("span.content_description_text") %>%
      html_text(trim = TRUE)
    
    # Si vide, essayons dans la div.chapo
    if (is.na(description) || description == "") {
      description <- page %>%
        html_element("div.chapo") %>%
        html_text(trim = TRUE)
    }
    
    # Si toujours rien, fallback : chercher dans les spans contenant le repère Alain Duhamel
    if (is.na(description) || description == "") {
      spans <- page %>% html_elements("span")
      spans_text <- spans %>% html_text(trim = TRUE)
      description <- spans_text[grepl("Alain Duhamel, éditorialiste", spans_text)][1]
    }
    
    # Extraction de l'invité
    invite <- str_match(description, "a débattu avec ([^,]+)")[,2]
    
    # Extraction souple de la date
    date_complète <- str_match(description, "[Cc]e \\w+ \\d{1,2} \\w+ \\d{4}")[,1]
    
    # Extraction du bandeau
    bandeau <- NA
    if (!is.na(description) && !is.na(invite)) {
      pattern_bandeau <- paste0("a débattu avec ", invite, ", ([^,]+)")
      bandeau <- str_match(description, pattern_bandeau)[,2]
    }
    
    tibble(
      url = url,
      titre = titre,
      date = date_complète,
      invite = invite,
      description = description,
      bandeau = bandeau
    )
    
  }, error = function(e) {
    tibble(
      url = url,
      titre = NA,
      date = NA,
      invite = NA,
      description = NA,
      bandeau = NA
    )
  })
}

# =============================================================================
# 5. --- Scrape All Episodes ---
# =============================================================================

resultats <- vector("list", length(emissions_uniques_VN))  

for (i in seq_along(emissions_uniques_VN)) { 
  message(sprintf("Traitement %d / %d : %s", i, length(emissions_uniques_VN), emissions_uniques_VN[i]))
  resultats[[i]] <- extraire_infos_emission(emissions_uniques_VN[i])
}

# Combiner tous les tibbles en un seul data frame
FaceToFaceDuhamel_guest <- bind_rows(resultats)

# =============================================================================
# 6. --- Final Date Cleaning & Formatting ---
# =============================================================================

FaceToFaceDuhamel_guest <- FaceToFaceDuhamel_guest %>%
  mutate (date = str_extract(date, "\\d{1,2} \\w+ \\d{4}"),
          date_formate = dmy(date))%>%
  select(url, titre, date, date_formate, invite, bandeau, description)
