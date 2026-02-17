# =============================================================================
# Title: Scraping C ce Soir (France 5) Archives through Youtube 
# Author: Damien Leprince-Chonik
# Date: 2025-15-05
# Description:
#   This script scrapes emission metadata (title, subtitle, guests, date, link)
#   from C ce Soir emission archives from Youtube.
#
# Data Source:
#   Youtube
#
# Notes:
#   - Intended for academic/research purposes only.
# =============================================================================

# =============================================================================
# 1. Load Required Packages
# =============================================================================

library(httr)
library(jsonlite)
library(tuber)
library(writexl)
library(stringr)
library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)

# ========================
# 2. --- Authenticate with YouTube API ---
# ========================
# WARNING:
#   Please use your own identifiant 

app_id = "YOUR_ID"
key = "YOUR_KEY"
yt_oauth(app_id = app_id, 
         app_secret = key)

# ========================
# 3. --- Function to scrape all videos from a YouTube playlist ---
# ========================

#Function to extract video from C ce Soir channel
get_all_videos_from_playlist <- function(playlist_id) { #Take upload_playlist_id as an argument (the playlist with all the public videos)
  all_videos <- list() # Create a list which will have all the videos
  next_page_token <- NULL 
  
  repeat {
    # Endpoint api
    url <- "https://www.googleapis.com/youtube/v3/playlistItems"
    
    # Request parameters
    query_params <- list(
      part = "snippet,contentDetails", #To have metadata and ID of videos
      maxResults = 50,
      playlistId = playlist_id,
      pageToken = next_page_token
    )
    
    # For the first request, delete NULL for page token 
    if (is.null(next_page_token)) {
      query_params$pageToken <- NULL
    }
    
    # Get the token OAuth running in tuber
    token <- yt_token()
    
    # GET request with authentification OAuth
    res <- GET(url, query = query_params, config(token = token))
    
    # Checking success
    if (res$status_code != 200) {
      stop("Erreur API : ", content(res, "text"))
    }
    
    # To parse the JSON
    res_content <- content(res, as = "text", encoding = "UTF-8")
    res_json <- fromJSON(res_content, flatten = TRUE)
    
    # To extract from videos
    videos <- data.frame(
      videoId = res_json$items$contentDetails.videoId,
      publishedAt = res_json$items$snippet.publishedAt,
      title = res_json$items$snippet.title,
      description = res_json$items$snippet.description,
      stringsAsFactors = FALSE
    )
    
    all_videos[[length(all_videos) + 1]] <- videos
    
    next_page_token <- res_json$nextPageToken
    cat("Next page token:", next_page_token, "\n")
    
    if (is.null(next_page_token)) {
      break
    }
  }
  
  #To merge all the pages
  all_videos_df <- do.call(rbind, all_videos)
  return(all_videos_df)
}

# ========================
# 4. --- Get playlist ID for "C ce Soir" channel ---
# ========================

channel_id <- "UC1UUvhau_2V8ETvHfwkUQwg" #identifiant of Youtube channel
uploads_playlist_id <- paste0("UU", substring(channel_id, 3)) #To delete UC and replace by UU to have the playlist with all videos (substring begin at 1 and stop at 3)

CceSoir_scrapper <- get_all_videos_from_playlist(uploads_playlist_id)
cat("Total number of emissions: ", nrow(all_videos), "\n")

# ========================
# 5. --- Automatic cleaning: extract show dates and type from titles ---
# ========================

Ccesoir_clean_automatically_date <- CceSoir_scrapper %>%
  mutate (
    date = str_extract(title, "(?i)(?<=C ce Soir du |#CCesoir du |C Politique du |C Politique |C ce Soir |#CCesoir |C Ce soir  |CCeSoir |En Société du )((1er|\\d{1,2}) \\w+ \\d{4})"),
    date = str_replace(date, "1er", "1"),
    date = if_else(date == "28 mois 2023", "28 novembre 2023", date),
    date = if_else(date == "26 mois 2023", "28 novembre 2023", date),
    date = if_else(is.na(date) & str_detect(title, "2 novembre 2023"), "2 novembre 2023", date),
    date_formate = dmy(date),
    emission = case_when(
      str_detect(title, regex("C Politique", ignore_case = TRUE)) ~ "CPolitique",
      str_detect(title, regex("C ce soir|#CCesoir|CCesoir", ignore_case = TRUE)) ~ "C ce soir",
      str_detect(title, regex("En Société", ignore_case = TRUE)) ~ "C ce soir",
      TRUE ~ NA_character_
    )
  )

# ========================
# 6. --- Manual cleaning of guest names in descriptions ---
# ========================

Ccesoir_clean_manually <- Ccesoir_clean_automatically_date %>%
  mutate(description = str_replace_all(description, c(
    "Léa Balage El Mariky" = "Léa BALAGE EL MARIKY",
    "Usul" = "Vidéaste USUL",
    "Anne de GUIGNÉ " = "Anne DE GUIGNÉ ",
    "Isabelle THIS SAINT JEAN" = "Isabelle THIS SAINT JEAN économiste, professeure d'Université à l'Université Sorbonne Paris Nord, chercheuse au CEPN",
    "Pauline de Saint-Rémy" = "Pauline DE SAINT-REMY",
    "Tore Keller" = "Tore KELLER",
    "Anne-Sophie ALSIF" = "Anne-Sophie ALSIF Cheffe économiste de BDO France, professeur d'économie à la Sorbonne",
    "Jean-Louis BOURLANGES" = "Jean-Louis BOURLANGES Ancien député MoDem et ex-président de la Commission des Affaires étrangères de l’Assemblée nationale",
    "Isabelle Lasserre" = "Isabelle LASSERRE",
    "Anne-Charlène BEZZINA" = "Anne-Charlène BEZZINA Constitutionnaliste, maîtresse de conférence à l'université de Rouen, membre associé de l'Institut des sciences juridique et philosophique de la Sorbonne",
    "Hélène Rossinot" = "Hélène ROSSINOT",
    "Cécile Cée" = "Cécile CEE",
    "Rachel Flore Prado" = "Rachel FLORE PRADO",
    "Cécile Ollivier" = "Cécile OLLIVIER",
    "Dominique Voynet" = "Dominique VOYNET",
    "Gaëlle Nerbard" = "Gaëlle NERBARD",
    "Rémi Carayol" = "Rémi CARAYOL",
    "Clémentine Lehuger" = "Clémentine LEHUGER",
    "Philippe Vigier" = "Philippe VIGIER",
    "Seline Soula" = "Seline SOULA",
    "Charlotte Lemay Mannequin" = "Charlotte LEMAY MANNEQUIN",
    "Oxana Melnychuk" = "Oxana MELNYCHUK",
    "Clara Marchaud" = "Clara MARCHAUD",
    "Pierre Lellouche" = "Pierre LELLOUCHE",
    "Stéphane Audoin-Rouzeau" = "Stéphane AUDOIN-ROUZEAU",
    "Gaspard Kœnig" = "Gaspard KOENING",
    "Maya Khadra" = "Maya KHADRA",
    "Georges Malbrunot" = "Georges MALBRUNOT",
    "Ziad Majed" = "Ziad MAJED",
    "Jean-François Colosimo" = "Jean-François COLOSIMO",
    "Mariam Pirzadeh" = "Mariam PIRZADEH",
    "Hicham, Étudiant à Sciences Po Paris" = "Hicham ETUDIANT, Étudiant à Sciences Po Paris",
    "Stéphane Fournier" = "Stéphane FOURNIER",
    "Marie Toussaint" = "Marie TOUSSAINT",
    "Vincent Lemire" = "Vincent LEMIRE",
    "Valérie Zenatti" = "Valérie ZENATTI",
    "Sara Daniel" = "Sara DANIEL",
    "Omar Ouahmane" = "Omar OUAHMANE",
    "Jean-Marie Guéhenno" = "Jean-Marie GUEHENNO",
    "Anne Berest" = "Anne BEREST",
    "Olivier Lalieu" = "Olivier LALIEU",
    "Sophie Nahum" = "Sophie NAHUM",
    "Rachel Jedinak" = "Rachel JEDINAK",
    "Laurent Joly" = "Laurent JOLY",
    "Alice Diop" = "Alice DIOP",
    "Dominique Schnapper" = "Dominique SCHNAPPER",
    "Cole Stangler" = "Cole STRANGLER",
    "Isabelle de Gaulmyn" = "Isabelle DE GAULMYN",
    "Emmanuelle Wargon" = "Emmanuelle WARGON",
    "Virgile Mouquet" = "Virgile MOUQUET",
    "Édouard Bergeon" = "Edouard BERGEON",
    "Lucile Schmid" = "Lucile SCHMID",
    "Gérard Noiriel" = "Gérard NOIRIEL",
    "Laure Mandeville" = "Laure MANDEVILLE",
    "Christian Salmon" = "Christian SALMON",
    "Cole Stangler" = "Cole STANGLER",
    "François Bégaudeau" = "François BEGAUDEAU",
    "Adélaïde Zulfikarpasic" = "Adélaïde ZULFIKARPASIC",
    "Nelly Garnier" = "Nelly GARNIER",
    "Martial Foucault" = "Martial FOUCAULT",
    "David Djaïz" = "David DJAÏZ",
    "Hervé Le Bras" = "Hervé LE BRAS",
    "Didier Leschi" = "Didier LESCHI",
    "Mercedes Erra" = "Mercedes ERRA",
    "ANDRÉ COMTE-SPONVILLE" = "André COMTE-SPONVILLE",
    "Ludivine Bantigny" = "Ludivine BANTIGNY",
    "Monique Dagnaud" = "Monique DAGNAUD",
    "Gaspard Koenig" = "Gaspard KEONING",
    "Jérôme Fourquet" = "Jérôme FOURQUET",
    "Jean-Laurent Cassely" = "Jean-Laurent CASSELY",
    "Thibault Lhonneur" = "Thibault LHONNEUR",
    "Maud Bregeon" = "Maud BREGEON",
    "Nicolas Offenstadt" = "Nicolas OFFENSTADT",
    "Cécile Alduy" = "Cécile ALDUY",
    "Judith Waintraub" = "Judith WAINTRAUB",
    "Paul Melun" = "Paul MELUN",
    "Jean-Michel Aphatie" = "Jean-Michel APHATIE",
    "Charles Consigny" = "Charles CONSIGNY",
    "Ovidie" = "Ovidie REALISATRICE",
    "Clément Beaune" = "Clément BEAUNE",
    "Thierry Pech" = "Thierry PECH",
    "Astrid de Villaines" = "Astrid DE VILLAINES",
    "Mathieu Souquière" = "Mathieu SOUQUIERE",
    "Léa Chamboncel" = "Léa CHAMBONCEL",
    "Charlotte Blandiot-Faride" = "Charlotte BLANDIOT-FARIDE",
    "Bruno Tertrais" = "Bruno TERTRAIS",
    "Frédéric Encel" = "Frédéric ENCEL",
    "Laure Mandeville" = "Laure MANDEVILLE",
    "Général Dominique TRINQUAND" = "Dominique TRINQUAND",
    "Veronika Dorman" = "Veronika DORMAN",
    "François Ruffin" = "François RUFFIN",
    "Bruno Bonnell" = "Bruno BONNELL",
    "Martine Duchemin" = "Martine DUCHEMIN",
    "Pierre Blavier" = "Pierre BLAVIER",
    "Emmanuelle Anizon" = "Emmanuelle ANIZON",
    "Ariane Chemin" = "Ariane CHEMIN",
    "Gallagher Fenwick" = "Gallagher FENWICK",
    "Jean-Dominique Merchet" = "Jean-Dominique MERCHET",
    "Irena Karpa" = "Irena KARPA",
    "Roman Sigov" = "Roman SIGOV",
    "Nicolas Tenzer" = "Nicolas TENZER",
    "Frédéric Worms" = "Frédéric WORMS",
    "Jérémie Peltier" = "Jérémie PELTIER",
    "Lamia El Aaraje" = "Lamia EL AARAJE",
    "Yonathan Freund" = "Yonathan FREUND",
    "Mathieu Slama" = "Mathieu SLAMA",
    "Nathalie Iannetta" = "Nathalie IANNETTA",
    "Pierre Rondeau" = "Pierre RONDEAU",
    "Kévin Veyssière" = "Kévin VEYSSIERE",
    "Lola Schulmann" = "Lola SCHULMANN",
    "Christian Chesnot" = "Christian CHESNOT",
    "Daniel Riolo" = "Daniel RIOLO",
    "Eva Sadoun" = "Eva SADOUN",
    "Rémi Lefebvre" = "Rémi LEFEBVRE",
    "Corinne Lepage" = "Corinne LEPAGE",
    "Jacques Attali" ="Jacques ATTALI",
    "Léa Falco" = "Léa FALCO",
    "Elsa Vidal" = "Elsa VIDAL",
    "Giuliano Da Empoli" = "Giuliano DA EMPOLI",
    "Pascal Boniface" = "Pascal BONIFACE",
    "Céline Bardet" = "Céline BARDET",
    "Élise Vincent" = "Elise VINCENT",
    "Gérard Miller" = "Gérard MILLER",
    "Marine Braud" = "Marine BRAUD",
    "Vincent Martigny" = "Vincent MARTIGNY",
    "Caroline Vigoureux" = "Caroline VIGOUREUX",
    "Juliette Méadel" = "Juliette MEADEL",
    "Marc Lazar" = "Marc LAZAR",
    "Renaud Dély" = "Renaud DELY",
    "François Gemenne" = "François GEMENNE",
    "Aurélie Trouvé" = "Aurélie TROUVE",
    "Céline Braconnier" = "Céline BRACONNIER",
    "Grégoire Cazcarra" = "Grégoire CAZCARRA",
    "Antoine Buéno" = "Antoine BUENO",
    "Jérôme Sainte-Marie" = "Jérôme SAINTE-MARIE",
    "Diastème" = "Diastème REALISATEUR",
    "Marc Crépon" = "Marc CREPON",
    "Cédric Mas" = "Cédric MAS",
    "Raphaël Pitti" = "Raphaël PITTI",
    "Elena Volochine" = "Elena VOLODOCHINE",
    "Clémence Bectarte" = "Clémence BECTARTE",
    "Patrick Chauvel" = "Patrick CHAUVEL",
    "Nicolas Delesalle" = "Nicolas DELESALLE",
    "Bénédicte Chéron" = "Bénédicte CHERON",
    "Arnaud Mercier" = "Arnaud MERCIER",
    "Pascal Boniface" = "Pascal BONIFACE",
    "Natalia Turine" = "Natalia TURINE",
    "Alla Lazareva" = "Alla LAZAREVA",
    "Jacques Rupnik" = "Jacques RUPNIK",
    "Bertrand Badie" = "Bertrand BADIE",
    "Andreï Vaitovich" = "Andreï VAITOVICH",
    "Pierre Gentillet" = "Pierre GENTILLET",
    "Isabelle Mandraud" = "Isabelle MANDRAUD",
    "Caroline Janvier" = "Caroline JANVIER",
    "Dov Alfon" = "Dov ALFON",
    "Erwan Lecoeur" = "Erwan LECOEUR",
    "Marine Turchi" = "Marine TURCHI",
    "Alexandre Lacroix" = "Alexandre LACROIX",
    "Géraldine Muhlmann" = "Géraldine MUHLMANN",
    "Stéphane Brizé" = "Stéphane BRIZE",
    "Benoît Hamon" = "Benoît HAMON",
    "Dominique Carlac’h" = "Dominique CARLACH",
    "Éric Collenne" = "Eric COLLENNE",
    "Sonia Devillers" = "Sonia DEVILLERS",
    "Laurent Cantet" = "Laurent CANTET",
    "Franck Louvrier" = "Franck LOUVRIER",
    "Antoine Bristielle" = "Antoine BRISTIELLE",
    "Isabelle Saporta" = "Isabelle SAPORTA",
    "Marlène Schiappa" = "Marlène SCHIAPPA",
    "Florence Torrollion" = "Florence TORROLLION",
    "Me Negar Haeri" = "Negard HAERI",
    "Anne-Cécile Mailfert" = "Anne-Cécile MAILFERT",
    "Maddy Scheurer" = "Maddy SCHEURER",
    "Reda Kateb" = "Reda KATEB",
    "Thomas Kruithof" = "Thomas KRUITHOF",
    "Philippe Rio" = "Phillippe RIO",
    "Anne Nivat" = "Anne NIVAT",
    "Benoît Coquard" = "Benoît COQUARD",
    "Raphaël Glucksmann" = "Raphaël GLUCKSMANN",
    "Sylvain Fort" = "Sylvain FORT",
    "Anna Bonalume" = "Anna BONALUME",
    "Max-Erwann Gastineau" = "Max-Erwann GASTINEAU",
    "Stéphanie Hennette-Vauchez" = "Stéphanie HENETTE-VAUCHEZ",
    "Karine Lacombe" = "Karine LACOMBE",
    "Bruno Lina" = "Bruno LINA",
    "Najat Vallaud-Belkacem" = "Natjat VALLAUD-BELKACEM",
    "Olivier Babeau" = "Olivier BABEAU",
    "Félix Marquardt" = "Félix MARQUARDT",
    "Nicolas Frémaux" = "Nicolas FREMAUX",
    "Ferghane Azihari" = "Ferghane AZIHARI",
    "Anthony Bourbon" = "Anthony BOURBON",
    "Pr André Grimaldi" = "André GRIMALDI",
    "Gilles Finchelstein" = "Gilles FINCHELSTEIN",
    "Ludovic Mendes" = "Ludovic MENDES",
    "Vincent Brengarth" = "Vincent BRENGARTH",
    "ANNE BEREST" = "Anne BEREST",
    "OLIVIER LALIEU" = "Olivier LALIEU",
    "SOPHIE NAHUM" = "Sophie NAHUM",
    "RACHEL JEDINAK" = "Rachel JEDINAK",
    "LAURENT JOLY" = "Laurent JOLY",
    "Philippe Descola" = "Philippe DESCOLA",
    "Ferghane Azihari" = "Ferghane AZIHARI",
    "Eva Sadoun" = "Eva SADOUN",
    "Paul Ariès" = "Paul ARIES",
    "Clothilde Mraffko" = "Clothilde MRAFFKO",
    "Selim Nassib" = "Selim NASSIB",
    "Denis Charbit" = "Denis CHARBIT",
    "Jadd Hilal" = "Jadd HILAL",
    "Rina Bassist" = "Rina BASSIST",
    "Laetitia Strauch-Bonart" = "Laetitia STRAUCH-BONART",
    "Sylvain Bourmeau" = "Sylvain BOURMEAU",
    "Alexandre Devecchio" = "Alexandre DEVECCHIO",
    "Thomas Piketty" = "Thomas PIKETTY",
    "Claire Hédon" = "Claire HEDEON",
    "Raphaël Enthoven" = "Raphaël ENTHOVEN",
    "Sandrine Rousseau" = "Sandrine ROUSSEAU",
    "Marc Weitzmann" = "Marc WEITZMANN",
    "Jean-Yves Camus" = "Jean-Yves CAMUS",
    "Roger-Pol Droit" = "Roger-Pol DROIT",
    "Serge Guérin" = "Serge GUERIN",
    "Alexandra Saphores" = "Alexandra SAPHORES",
    "Nathalie Levy" = "Nathalie LEVY",
    "Jean-Philippe Gautrais" = "Jean-Philippe GAUTRAIS",
    "Régine Florin" = "Régine FLORIN",
    "Sylvie Brunel" = "Sylvie BRUNEL",
    "Sébastien Jean" = "Sébastien JEAN",
    "Élie Cohen" = "Elie COHEN",
    "Béatrice Giblin" = "Béatrice GIBLIN",
    "Arnaud Montebourg" = "Arnaud MONTEBOURG",
    "Louis Schweitzer" = "Louis SCHWEITZER",
    "Sorj Chalandon" = "Sorj CHALANDON",
    "Mathieu Delahousse" = "Mathieu DELAHOUSSE",
    "Jean-Pierre Filiu" = "Jean-Pierre FILIU",
    "Nassim Majidi" = "Nassim MAJIDI",
    "Olivier Weber" = "Olivier WEBER",
    "Emmanuel Todd" = "Emmanuel TODD",
    "Laetitia Strauch-Bonart" = "Laetitia STRAUCH-BONART",
    "Titiou Lecoq" = "Titiou LECOQ",
    "Georges Vigarello" = "Georges VIGARELLO",
    "Benjamin Rossi" = "Benjamin ROSSI",
    "Nora Sahara" = "Nora SAHARA",
    "Olivier Christin" = "Olivier CHRISTIN",
    "Stéphanie Rist" = "Stéphane RIST",
    "Erwan Le Noan" = "Erwan LE NOAN",
    "Bernard Guetta" = "Bernard GUETTA",
    "Sandra Regol" = "Sandra REGOL"
  )
  ))

# ========================
# 7. --- Function to extract guest names from description ---
# ========================

extract_invites_from_description <- function(desc) {
  # Cases with ▶︎ or 📌 or ➤
  if (str_detect(desc, "▶︎|📌|➤")) {
    invite_blocks <- str_split(desc, "▶︎|📌|➤")[[1]]
    invite_blocks <- invite_blocks[-1]  # To delete the part before the first emoji
    
    invites_clean <- map_chr(invite_blocks, function(block) {
      lines <- str_split(block, "\n")[[1]]
      lines <- str_trim(lines)
      
      # Regex pour détecter un nom seul (Prénom + NOM majuscules) sans description
      pattern_nom_seul <- "^([A-ZÉÈÙÂÊÎÔÛÀÇ][a-zéèàêîôûçëïüöäÿ'-]+\\s+)?[A-ZÉÈÙÂÊÎÔÛÀÇ’'\\-]{2,}(\\s+[A-ZÉÈÙÂÊÎÔÛÀÇ’'\\-]{2,})?$"
      
      # Si la première ligne semble être seulement un nom et qu'il existe une ligne suivante
      if (length(lines) >= 2 && str_detect(lines[1], pattern_nom_seul)) {
        # On concatène la 1ère et 2ème ligne pour inclure la description
        paste(lines[1], lines[2])
      } else {
        # Sinon on garde juste la première ligne
        lines[1]
      }
    })
    
    return(invites_clean)
  }
  
  # Cas "avec :" suivi d’une liste d’invités sans symboles
  else if (str_detect(desc, regex("avec :", ignore_case = TRUE))) {
    after_avec <- str_split(desc, regex("avec :", ignore_case = TRUE))[[1]][2]
    if (is.na(after_avec) || nchar(after_avec) == 0) return(character(0))
    
    lines <- str_split(after_avec, "\n")[[1]]
    lines <- lines[str_length(lines) > 5]
    
    pattern_invite <- "^([A-ZÉÈÙÂÊÎÔÛÀÇÏÄÖÜ][a-zéèàêîôûç'-]+\\s+)?[A-ZÉÈÙÂÊÎÔÛÀÇÏÄÖÜ]{2,}(\\s+[A-ZÉÈÙÂÊÎÔÛÀÇÏÄÖÜ]{2,})?"
    filtered <- lines[str_detect(lines, pattern_invite)]
    filtered <- str_trim(filtered)
    return(filtered)
  }
  
  else {
    return(character(0))
  }
}

# ========================
# 8. --- Clean and extract guests from all descriptions ---
# ========================

emissions_Ccesoir_propre <- Ccesoir_clean_manually %>%
  rowwise() %>%
  mutate(
    invite_list = list(extract_invites_from_description(description))
  ) %>%
  unnest(invite_list) %>%
  mutate(
    invite = str_extract(invite_list, "([A-ZÉÈÙÂÊÎÔÛÀÇÏÄÖÜÁ][a-zéèàêîôûçëïüöäÿ'-]+(?:-[A-ZÉÈÙÂÊÎÔÛÀÇÏÄÖÜÁ][a-zéèàêîôûçëïüöäÿ'-]+)?(?:\\s+[A-A-ZÉÈÙÂÊÎÔÛÀÇÏÄÖÜÁ][a-zéèàêîôûçëïüöäÿ'-]+)*)\\s+([A-ZÉÈÙÂÊÎÔÛÀÇÏÄÖÜÁ’'\\-]{2,}(?:\\s+[A-ZÉÈÙÂÊÎÔÛÀÇÏÄÖÜÁ’'\\-]{2,})*)"),
    description_invite = str_trim(str_remove(invite_list, fixed(invite)))
  ) %>%
  ungroup() %>%
  distinct(invite, date_formate, description_invite, .keep_all = TRUE)%>% # Removing the duplicates
  mutate(description_invite = str_remove(description_invite, "^,\\s*"))%>%# Deleting ", " at the begining of some cases
  select(date,date_formate,invite,description_invite, description)
View(emissions_Ccesoir_propre)
