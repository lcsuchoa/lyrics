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


genre_options <- c("bossa-nova", "forro", "funk-carioca", "gospel", "infantil",
                   "jovem-guarda", "mpb", "musicas-gauchas", "pagode", "regional",
                   "samba", "samba-enredo", "sertanejo", "velha-guarda")

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


remove_stopwords <- function(lyrics) {
  
  stopwords_pt <- tm::stopwords("portuguese")
  
  tokens <- unlist(strsplit(tolower(lyrics), "\\W+"))
  tokens_without_stopwords <- tokens[!tokens %in% tm::stopwords("portuguese")]
  lyrics_without_stopwords <- paste(tokens_without_stopwords, collapse = " ")
  
  return(lyrics_without_stopwords)
}

