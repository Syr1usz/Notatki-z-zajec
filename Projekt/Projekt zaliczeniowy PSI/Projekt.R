#' ---
#' title: "Projekt na PSI (Chmura slow i TF-IDF)"
#' author: "Piotr Regenczuk, Weronika Ploszaj, Zofia Maj"
#' date:   "25.05.2026"
#' output:
#'   html_document:
#'     df_print: paged
#'     theme: readable
#'     highlight: kate
#'     toc: true
#'     toc_depth: 3
#'     toc_float:
#'       collapsed: false
#'       smooth_scroll: true
#'     code_folding: show
#'     number_sections: false # Numeruje nagłówki (lepsza nawigacja)
#' ---

# Ukrywa nieistotne komunikaty
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)
# install.packages(c("tm","tidyverse","tidytext","wordcloud","ggplot2","ggthemes","RColorBrewer"))
#' # Wymagane pakiety
# Wymagane pakiety----
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(SnowballC)

#' # Dane tekstowe
# Dane tekstowe----
# Wczytuje dany plik csv
data <- read.csv("Opinie.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

# Tworzy z opinii korpus dokumentów, żeby pakiet 'tm' mógł na nich pracować
corpus <- VCorpus(VectorSource(data$Review.text..Original.))

#' # Przetwarzanie i oczyszczanie tekstu
# Przetwarzanie i oczyszczanie tekstu----
# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# Czyszcenie tekstu nasza funkcja
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "@\\w+")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")
corpus <- tm_map(corpus, toSpace, "http\\w*")
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")

# Zmienia wszystkie litery na male
corpus <- tm_map(corpus, content_transformer(tolower))

# Usuwa cyfry
corpus <- tm_map(corpus, removeNumbers)

# Usuwa nic nie znaczace slowa
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Usuwa znaki interpunkcyjne
corpus <- tm_map(corpus, removePunctuation)

# Usuwanie zbędnych słów
corpus <- tm_map(corpus, removeWords, c("james", "say", "bond", "game", "can", "just", "take", "get", "see", "first", "light", "will","even","give"))

# Stemming
corpus <- tm_map(corpus, stemDocument)

# Usuwanie zbędnych słów po raz drugi, żeby te co się zamieniły w niepotrzebne zostały ponownie usunięte
corpus <- tm_map(corpus, removeWords, c("james", "say", "bond", "game", "can", "just", "take", "get", "see", "first", "light", "will","even","give"))

# Zmienia wielokrotne spacje na pojedyncze
corpus <- tm_map(corpus, stripWhitespace)

#' # Macierz częstości TDM z TF-IDF
# Macierz częstości TDM z TF-IDF----
# Robi macierz TF-IDF
tdm_tfidf <- TermDocumentMatrix(corpus,
                                control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

# Zamienia obiekt na standardową macierz w R
tdm_tfidf_m <- as.matrix(tdm_tfidf)

#' # Zliczanie częstości słów
# Zliczanie częstości słów----
# Zlicza wagi slow w opiniach i sortuje te slowa
v_tfidf <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)

# Tworzy tabelę ze słowami i ich wagami na podstawie wcześniejszych obliczeń
tdm_tfidf_df <- data.frame(word = names(v_tfidf), freq = v_tfidf)

#' # Eksploracyjna analiza danych
# Eksploracyjna analiza danych----
# Generuje chmurę słów na podstawie naszej tabeli
wordcloud(words = tdm_tfidf_df$word,freq = tdm_tfidf_df$freq, min.freq = 7, max.words = 100,scale = c(4, 0.5),  colors = brewer.pal(8, "Dark2"))

# Wyświetla 10 pierwszych wierszy tabeli
head(tdm_tfidf_df, 10)

#' # Interpretacja wyników
# Interpretacja wyników----
# Zastosowaliśmy chmurę słów i algorytm TF-IDF do analizy opinii o grze „007 First Light”,
# dzięki temu udało nam się wyodrębnić słowa najczęściej występujące i najlepiej charakteryzujące
# ogólny wydźwięk opinii. Wyniki wskazały na jednoznacznie pozytywny odbiór.
# Pośród pierwszych 10 słów z TF-IDF możemy zauważyć: „like”, „good”,
# „realli” (od really, co wskazuje na silne emocje, a w połączeniu z innymi słowami silnie pozytywne),
# „great” oraz „fun”. W chmurze słów tych pozytywnych określeń możemy znaleźć jeszcze więcej.
# Warto również pochylić się nad słowem feel, które sugeruje, że gra faktycznie oddała
# klimat filmów o agencie 007, co potwierdzają „cinemat” i „movi” z chmury słów.
# Innym ciekawym i nieoczywistym słowem jest „hitman”, czyli tytuł najpopularniejszej gry tego studia,
# możemy zauważyć że były one silnie porównywane i że „007 First Light” sprostała tym porównaniom.

#' # Podział pracy
# Podział pracy----
# Piotr Regeńczuk - kod
# Weronika Płoszaj - opinie i druga połowa dokumentacji
# Zofia Maj - pierwsza połowa dokumentacji