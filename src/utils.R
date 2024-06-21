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


balance_data_for_emotions <- function(data, sentiment_cols, seed = 7) {
  set.seed(seed)
  
  # Converter colunas de sentimentos para fatores
  data <- convert_to_factors(data, sentiment_cols)
  
  balanced_data_list <- list()
  
  for (sentiment in sentiment_cols) {
    # Separar os dados em treino e teste
    data_split <- initial_split(data, prop = 0.8)
    train_data <- training(data_split)
    test_data <- testing(data_split)
    
    # Selecionar apenas colunas numéricas e a coluna de sentimento
    train_subset <- train_data %>%
      select(-all_of(sentiment_cols), all_of(sentiment))
    
    # Criar receita para aplicar SMOTE apenas na coluna de sentimento
    recipe <- recipe(as.formula(paste(sentiment, "~ .")), data = train_subset) %>%
      step_smote(all_outcomes())
    
    balanced_train <- prep(recipe, training = train_subset) %>%
      bake(new_data = NULL)
    
    # Combinar com os dados de teste
    balanced_data <- list(
      train_data = balanced_train,
      test_data = test_data
    )
    
    balanced_data_list[[sentiment]] <- balanced_data
  }
  
  return(balanced_data_list)
}


classify_sentiment <- function(data, sentiment_col, cutoff_seq = seq(0.1, 0.9, by = 0.05), seed = 7) {
  set.seed(seed)
  
  # Selecionar apenas as colunas de TF-IDF e a coluna de resposta
  tfidf_cols <- data %>% select(1:(ncol(data) - 10))
  response_col <- data %>% select(all_of(sentiment_col))
  model_data <- bind_cols(tfidf_cols, response_col) %>%
    rename(response = all_of(sentiment_col))
  
  # Separar os dados em treino e teste
  data_split <- initial_split(model_data, prop = 0.8)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Especificar o modelo de random forest
  rf_model <- rand_forest(mode = "classification", trees = 1000) %>%
    set_engine("ranger")
  
  # Especificar a receita de pré-processamento
  recipe <- recipe(response ~ ., data = train_data) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())
  
  # Criar o fluxo de trabalho
  wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(rf_model)
  
  # Treinar o modelo
  rf_fit <- wf %>%
    fit(data = train_data)
  
  # Fazer previsões no conjunto de teste
  predictions <- rf_fit %>%
    predict(new_data = test_data, type = "prob") %>%
    bind_cols(test_data)
  
  # Função para calcular a acurácia para um dado ponto de corte
  calculate_accuracy <- function(cutoff) {
    preds <- predictions %>%
      mutate(.pred_class = factor(ifelse(.pred_Yes > cutoff, "Yes", "No"), levels = c("No", "Yes")))
    accuracy_metric <- accuracy(preds, truth = response, estimate = .pred_class)
    return(accuracy_metric$.estimate)
  }
  
  # Testar uma gama de valores de ponto de corte
  accuracy_values <- sapply(cutoff_seq, calculate_accuracy)
  
  # Plotar a acurácia para diferentes pontos de corte
  cutoff_data <- data.frame(cutoff = cutoff_seq, accuracy = accuracy_values)
  
  cutoff_plot <- ggplot(cutoff_data, aes(x = cutoff, y = accuracy)) +
    geom_line() +
    geom_point() +
    labs(title = "Acurácia para diferentes pontos de corte",
         x = "Ponto de corte",
         y = "Acurácia") +
    theme_minimal()
  
  print(cutoff_plot)
  
  # Imprimir o ponto de corte com a maior acurácia
  best_cutoff <- cutoff_seq[which.max(accuracy_values)]
  best_accuracy <- max(accuracy_values)
  print(paste("Melhor ponto de corte:", best_cutoff))
  print(paste("Melhor acurácia:", best_accuracy %>% round(4)))
  
  # Classificar as previsões com o melhor ponto de corte
  predictions_best <- predictions %>%
    mutate(.pred_class_best = factor(ifelse(.pred_Yes > best_cutoff, "Yes", "No"), levels = c("No", "Yes")))
  
  # Avaliar o modelo com as métricas personalizadas
  custom_metrics <- metric_set(roc_auc, accuracy, precision, recall, f_meas)
  final_metrics_best <- custom_metrics(predictions_best, truth = response, estimate = .pred_class_best, .pred_Yes)
  print(final_metrics_best)
  
  # Calcular a matriz de confusão para o melhor ponto de corte
  conf_matrix_best <- conf_mat(predictions_best, truth = response, estimate = .pred_class_best)
  print(conf_matrix_best)
  
  # Calcular a curva ROC
  roc_curve_data <- roc_curve(predictions, truth = response, .pred_Yes)
  
  # Encontrar o ponto de corte otimizado (índice de Youden)
  optimal_cutoff <- roc_curve_data %>%
    mutate(youden = sensitivity + specificity - 1) %>%
    filter(youden == max(youden)) %>%
    slice(1) %>%
    pull(.threshold)
  
  print(paste("Ponto de corte otimizado pela curva ROC:", optimal_cutoff))
  
  # Classificar as previsões com o ponto de corte otimizado
  predictions_optimal <- predictions %>%
    mutate(.pred_class_optimal = factor(ifelse(.pred_Yes > optimal_cutoff, "Yes", "No"), levels = c("No", "Yes")))
  
  # Avaliar o modelo com as métricas personalizadas para o ponto de corte otimizado
  final_metrics_optimal <- custom_metrics(predictions_optimal, truth = response, estimate = .pred_class_optimal, .pred_Yes)
  print(final_metrics_optimal)
  
  # Calcular a matriz de confusão para o ponto de corte otimizado
  conf_matrix_optimal <- conf_mat(predictions_optimal, truth = response, estimate = .pred_class_optimal)
  print(conf_matrix_optimal)
  
  # Plotar a curva ROC
  roc_plot <- ggplot(roc_curve_data, aes(x = 1 - specificity, y = sensitivity)) +
    geom_path() +
    geom_abline(linetype = "dashed", color = "red") +
    labs(title = "Curva ROC",
         x = "1 - Especificidade",
         y = "Sensibilidade") +
    theme_minimal()
  
  print(roc_plot)
}


