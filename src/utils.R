require(tidyverse)
require(httr)
require(xml2)
require(rvest)
require(furrr)
require(tictoc)
require(tm)
require(tm.plugin.webmining)
require(cld3)
require(syuzhet)
require(RColorBrewer)
require(wordcloud)
require(tm)
require(caret)
require(text2vec)
require(glmnet)
require(text)
require(stringi)
require(mlr3)
require(mlr3tuning)
require(mlr3mbo)
require(mlr3learners)
require(ranger)
require(DiceKriging)
require(rgenoud)
require(pROC)
require(reshape2)
require(mlrMBO)
require(ParamHelpers)
require(mlr)
require(tidymodels)
require(finetune)
require(tune)
require(themis)
require(doParallel)


genre_options <- c("bossa-nova", "forro", "funk-carioca", "gospel", "infantil",
                   "jovem-guarda", "mpb", "musicas-gauchas", "pagode", "regional",
                   "samba", "samba-enredo", "sertanejo", "velha-guarda")

labels_pt <- c(
  "anger" = "raiva",
  "anticipation" = "antecipação",
  "disgust" = "desgosto",
  "fear" = "medo",
  "joy" = "alegria",
  "negative" = "negativo",
  "positive" = "positivo",
  "sadness" = "tristeza",
  "surprise" = "surpresa",
  "trust" = "confiança"
)

labels_en <- c(
  "anger",
  "anticipation",
  "disgust",
  "fear",
  "joy",
  "negative",
  "positive",
  "sadness",
  "surprise",
  "trust"
)

get_artist_link <- function(genre, number){

  page <- read_html(paste0("https://www.vagalume.com.br/browse/style/", genre, ".html"))
  nameList <- page %>% html_nodes(".moreNamesContainer") %>% as_list()

  artist_links <- nameList %>% 
    unlist(recursive = F) %>%
    unlist(recursive = F) %>%
    unlist(recursive = F) %>%
    map(., ~attr(., "href")) %>%
    unlist() %>% as.character() %>%
    head(number)
  
  return(unique(artist_links))
}


get_artist <- function(artist_link){
  
  page <- read_html(paste0("https://www.vagalume.com.br", artist_link))
  pop_page <- read_html(paste0("https://www.vagalume.com.br", artist_link, "popularidade/"))
  
  A <- page %>% html_nodes(".darkBG") %>% html_text()
  G <- page %>% html_nodes(".subHeaderTags") %>% 
    as_list() %>% unlist() %>% paste(collapse = "; ")
  S <- page %>% html_nodes(".nameMusic") %>% html_text() %>% 
    unique() %>% length()
  P <- pop_page %>% html_nodes(".text") %>% html_text() %>% 
    tail(1) %>% 
    str_extract(., "(?<=está em )(.*)(?= pontos)") %>% 
    str_replace(",", ".") %>% as.numeric() 
  
  res <- tibble(Aname = A, Agenres = G, Asongs = S, Apopularity = P, Alink = artist_link)
  return(res)
}


scrap_artist <- possibly(get_artist, otherwise = tibble(Aname = NA,  Agenres = NA,  Asongs = NA,  Apopularity = NA,  Alink = NA))


get_lyrics_link <- function(artist_link, number) {
  
  page <- read_html(paste0("https://www.vagalume.com.br", artist_link))
  music_name_node <- page %>% html_nodes(".nameMusic")
  S <- music_name_node %>% html_text() %>% head(number)
  L <- music_name_node %>% html_attr("href") %>% head(number)
  
  res <- tibble(
    Srank = 1:number,
    Alink = rep(artist_link, number),
    Sname = S,
    Slink = L
  )
  return(res)
}


scrap_lyrics_link <- possibly(get_lyrics_link, otherwise = tibble(Srank = NA, Alink = NA, Sname = NA, Slink = NA))


get_lyric <- function(song_link) {
  
  lyric <- read_html(paste0("https://www.vagalume.com.br", song_link)) %>% html_nodes("#lyrics")
  dummy <- xml_node(read_xml("<doc><span>. </span></doc>"), "span")
  xml_add_sibling(xml_nodes(lyric, "br"), dummy)
  
  res <- lyric %>% html_text()
  return(res)
}


scrap_lyric <- possibly(get_lyric, otherwise = NA)


remove_stopwords <- function(text_column) {
  stopwords_br <- c(stopwords("pt"), "nao", "quê", "prá", "umas", "uns", "é", "és", "tá", "tô", "pra", "pro", "ô", "aê", "aí", "ai", "ali", "lá", "cá", "ilá", "ilê", "iê", "obá", "cê", "ah", "né", "x", "etc", "ltda", "tal", "refrao", "refrão", "estrofe", "yeah", "what", "you", "your", "the")
  
  remove_stopwords_from_text <- function(text) {
    words <- unlist(strsplit(text, "\\s+"))
    words <- words[!tolower(words) %in% stopwords_br]
    return(paste(words, collapse = " "))
  }
  
  sapply(text_column, remove_stopwords_from_text)
}


syuzhet_classification <- function(lyrics, minimum) {
  
  tokens <- get_tokens(lyrics)
  feelings <- get_nrc_sentiment(tokens, lang="portuguese")
  
  res <- data.frame(
    anger = ifelse(mean(feelings$anger) >= minimum, 1, 0),
    anticipation = ifelse(mean(feelings$anticipation) >= minimum, 1, 0),
    disgust = ifelse(mean(feelings$disgust) >= minimum, 1, 0),
    fear = ifelse(mean(feelings$fear) >= minimum, 1, 0),
    joy = ifelse(mean(feelings$joy) >= minimum, 1, 0),
    sadness = ifelse(mean(feelings$sadness) >= minimum, 1, 0),
    surprise = ifelse(mean(feelings$surprise) >= minimum, 1, 0),
    trust = ifelse(mean(feelings$trust) >= minimum, 1, 0),
    negative = ifelse(mean(feelings$negative) >= minimum, 1, 0),
    positive = ifelse(mean(feelings$positive) >= minimum, 1, 0)
  )
  
  return(res)
}


clean_names <- function(names) {
  clean_names <- stri_trans_general(names, "Latin-ASCII")
  clean_names <- gsub("[^[:alnum:]_]", "", clean_names)

  clean_names <- make.names(clean_names)
  unique_names <- make.unique(clean_names, sep = "_")
  
  return(unique_names)
}


