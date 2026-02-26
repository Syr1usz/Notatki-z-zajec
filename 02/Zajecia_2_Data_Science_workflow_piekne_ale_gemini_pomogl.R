# 1. KATALOG ROBOCZY I PAKIETY ----
# R musi wiedzieć, gdzie szukać plików
# Sprawdź bieżący katalog roboczy:
# getwd()

# Ustaw swój katalog roboczy (wskaż folder z plikami) z poziomu menu w RStudio:
# Session -> Set Working Directory -> Choose Directory

# Można też to zrobić w konsoli, 
# ale wtedy trzeba znać pełną ścieżkę do folderu z plikami, np.:
# setwd("C:/folder")        # domyślnym katalogiem będzie C:/folder
# setwd("C:/Data/project")  # domyślnym katalogiem będzie C:/Data/project

# W R dla ścieżki do pliku używamy / lub \\
# setwd("C:\\Data\\project") # domyślnym katalogiem będzie C:/Data/project

# Środowisko programistyczne R opiera swoje potężne możliwości na dołączaniu 
# różnych dodatkowych funkcji czyli pakietów (packages) rozwijanych przez 
# programistów na całym świecie.

# Funkcja install.packages() służy do jednorazowej instalacji wybranego pakietu:
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("writexl")

# Funkcja library() wczytuje wybrany pakiet, aby móc z niego korzystać:
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)


# 2. IMPORT DANYCH ----
# Separatory oddzielają kolumny lub wartości dziesiętne.
# Parametr | Wartość            | Opis
# header   | TRUE / FALSE       | Czy pierwszy wiersz to nagłówki?
# sep      | ";" "," "\t" " "   | Separator kolumn (średnik, przecinek, tab, spacja)
# dec      | "." ","            | Separator dziesiętny (kropka lub przecinek)

# Import pliku CSV. Funkcja read.table() domyślnie zakłada:
# header = FALSE, sep = "" (spacje), dec = "."

# Prawidłowy import plików CSV z folderu - wykonaj:
kraje_1 = read.table("C:\\Users\\piotr\\Desktop\\R jakies rzeczy\\kraje_makro_1.csv", header = TRUE, sep = ",", dec = ".")
kraje_2 = read.table("C:\\Users\\piotr\\Desktop\\R jakies rzeczy\\kraje_makro_2.csv", header = TRUE, sep = ",", dec = ".")


# 3. PRZYGOTOWANIE DANYCH ----
# Po zaimportowaniu danych przygotuj je do analizy, wykonując:
# - podgląd danych
# - porządkowanie nazw kolumn (jeśli potrzeba)
# - porządkowanie typów danych (jeśli potrzeba np. z tekstu na liczby)
# - porządkowanie braków danych (usuwanie lub uzupełnianie brakujących wartości)
# - czyszczenie danych (jeśli potrzeba)

# 3.1. Podgląd danych ----
head(kraje_1)      # pierwsze 6 wierszy (obserwacji)
head(kraje_2)      

head(kraje_1, 10)  # pierwsze 10 wierszy (obserwacji)
head(kraje_2, 10)

tail(kraje_1, 5)   # ostatnie 5 wierszy (obserwacji)
tail(kraje_2, 5)

# Podstawowe statystyki wszystkich kolumn (zmiennych)
summary(kraje_1)   # min, max, średnia, mediana, kwantyle
summary(kraje_2)

# Statystyki pojedynczej kolumny (zmiennej)
mean(kraje_1$Przyrost_populacji)   # średnia
median(kraje_1$Przyrost_populacji) # mediana
min(kraje_1$Przyrost_populacji)    # minimum
max(kraje_1$Przyrost_populacji)    # maksimum


# 3.2. Porządkowanie nazw kolumn (zmiennych) ----
# Usuwanie zbędnej kolumny
kraje_1$X = NULL
kraje_2$X = NULL

# Zmiana nazw kolumn z angielskich na polskie
colnames(kraje_2) = c("Kod_kraju", "Nazwa", "Region", "Urbanizacja_proc.", "Internet_proc.")


# 3.3. Porządkowanie typów danych ----
# W ramce danych kraje_2 sprawdź typ zmiennej Region 
is.numeric(kraje_2$Region)   # czy zmienna jest liczbowa? Odp. Nie.
is.character(kraje_2$Region) # czy zmienna jest tekstowa? Odp. Tak.

# Region to zmienna kategorialna, więc nadajemy jej typ factor:
kraje_2$Region = as.factor(kraje_2$Region)

# Sprawdzenie kategorii:
summary(kraje_2)
levels(kraje_2$Region)
# Teraz widać, że jest 7 kategorii regionów, na których operuje zmienna Region.


# 3.4. Porządkowanie braków danych ----
# Szybka kontrola braków danych we wszystkich kolumnach:
colSums(is.na(kraje_1)) # nie ma braków danych
colSums(is.na(kraje_2)) # są 4 braki danych w kolumnie (zmiennej) Internet_proc.

# Liczba braków w konkretnej kolumnie:
sum(is.na(kraje_2$Internet_proc.)) # 4 braki

# Zobaczmy te 4 wiersze, w których brakuje wartości:
kraje_2[is.na(kraje_2$Internet_proc.), ]

# Braki danych są częścią rzeczywistości ekonomisty, dlatego trzeba umieć je obsłużyć:
# OPCJA 1 - Pozostawić (teraz tak postąpimy)
# OPCJA 2 - Usunąć obserwacje z brakami
# OPCJA 3 - Uzupełnić braki (np. imputacja medianą)


# 3.5. Czyszczenie danych ----
# W ramce danych kraje_2, w kolumnie Region są kategorie, w których nazwie jest znak &:
levels(kraje_2$Region)

# Znak & bywa problematyczny przy dalszym przetwarzaniu, dlatego zastąp go "and".
# Funkcja gsub() działa jak "Znajdź i zamień".
kraje_2$Region <- gsub("&", "and", kraje_2$Region)

# Sprawdzenie (po zamianie ponownie ustawiamy typ factor):
kraje_2$Region = as.factor(kraje_2$Region)
levels(kraje_2$Region)


# 4. ŁĄCZENIE (SCALANIE) RAMEK DANYCH W JEDNĄ ----
# Funkcja merge() łączy dwie ramki danych/tabele po wspólnej kolumnie (kluczu) 
# działa analogicznie jak WYSZUKAJ.PIONOWO w Excelu.

# Łączenie (scalanie) ramek danych kraje_1 i kraje_2
kraje = merge(kraje_1, kraje_2, by.x="Kod", by.y="Kod_kraju")

# Usuwanie zbędnej kolumny po połączeniu
kraje$Nazwa = NULL

# Zobacz ramkę danych po scaleniu
summary(kraje)
str(kraje)


# 5. PODSTAWOWA ANALIZA DANYCH ----
# Na tym etapie nie budujemy jeszcze modeli, tylko skupiamy się na poznaniu zbioru danych.
# dplyr to pakiet R umożliwiający manipulowanie ramkami danych w intuicyjny sposób.

# Najczęściej używane funkcje pakietu dplyr:
# mutate()    - tworzenie nowych zmiennych na bazie istniejących
# filter()    - wybieranie wierszy spełniających określone warunki
# select()    - wybieranie kolumn
# arrange()   - sortowanie
# group_by()  - grupowanie
# summarise() - obliczanie wartości zagregowanych (np. średnich, sum)


# 5.1. mutate() – tworzenie nowych zmiennych na bazie istniejących ----
# Tworzenie nowej zmiennej Populacja_w_mln w dplyr:
kraje = kraje %>%
  mutate(Populacja_mln = Populacja / 1e6)

# Równoważny kod w base R:
# kraje$Populacja_mln = kraje$Populacja / 1e6

# 1e6 to zapis miliona w R (1 razy 10 do potęgi 6), 1e9 (miliard), 1e12 (bilion)

# Tworzenie nowej zmiennej PKB_per_capita w dplyr:
kraje = kraje %>%
  mutate(PKB_per_capita = PKB / Populacja)

# Równoważny kod w base R:
# kraje$PKB_per_capita = kraje$PKB / kraje$Populacja


# 5.2. filter() – wybieranie wierszy i select() – wybieranie kolumn ----
# Wyświetl kraje, w których % poziom urbanizacji jest większy niż 50
kraje %>%
  filter(Urbanizacja_proc. > 50)

# Wyświetl tylko dane pokazujące zmienne Panstwo, Region, PKB, Populacja_mln
kraje %>%
  select(Panstwo, Region, PKB, Populacja_mln)


# 5.3. arrange() – sortowanie ----
# Posortuj kraje według przyrostu populacji rosnąco i malejąco:
kraje %>% arrange(Przyrost_populacji)
kraje %>% arrange(desc(Przyrost_populacji))

# Wybierz kraje z PKB większym niż 1 bilion, posortuj je rosnąco względem PKB 
# i wyświetl nazwę państwa, PKB i PKB per capita:
kraje %>%
  filter(PKB > 1e12) %>%
  arrange(PKB) %>%
  select(Panstwo, PKB, PKB_per_capita)

# Wybierz kraje z regionu Afryki Subsaharyjskiej, wybierz wybrane zmienne,
# a następnie posortuj malejąco po PKB per capita:
kraje %>%
  filter(Region == "Sub-Saharan Africa") %>%
  select(Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja_proc.) %>%
  arrange(desc(PKB_per_capita))


# 5.4. group_by() – grupowanie i summarise() - agregacja ----
# Wyświetl tylko te kraje, które są bogatsze niż średnia regionu
bogate = kraje %>%
  group_by(Region) %>%
  filter(PKB_per_capita > mean(PKB_per_capita, na.rm = TRUE))

# Znajdź największą wartość PKB per capita w całym zbiorze krajów
kraje %>%
  summarise(max_PKB_per_capita = max(PKB_per_capita, na.rm = TRUE))

# Znajdź największą i najmniejszą wartość Populacji w mln w całym zbiorze krajów
kraje %>%
  summarise(
    min_populacja = min(Populacja_mln, na.rm = TRUE),
    max_populacja = max(Populacja_mln, na.rm = TRUE)
  )

# Oblicz średnią populację i ile krajów jest w całym zbiorze danych
kraje %>%
  summarise(
    srednia_populacja = mean(Populacja_mln, na.rm = TRUE),
    liczba_krajow = n()
  )

# Policz, ile krajów jest w każdym regionie
kraje %>%
  group_by(Region) %>%
  summarise(liczba_krajow = n())

# Dla każdego regionu świata: oblicz liczbę krajów, średni % dostęp do internetu 
# i średni % poziom urbanizacji, a następnie posortuj regiony malejąco wg internetu:
kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_internet))


# 6. WIZUALIZACJA DANYCH [* ZAAWANSOWANE *] ----
# Wizualizacja danych także pozwala zidentyfikować wzorce i zależności w zbiorze danych.

# 1. Prosty wykres punktowy: urbanizacja a PKB per capita ----
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita)) +
  geom_point() +
  labs(
    title = "Urbanizacja a PKB per capita",
    x = "Urbanizacja (%)",
    y = "PKB per capita")

# 2. Zaawansowany wykres punktowy: urbanizacja a PKB per capita ----
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Urbanizacja a PKB per capita",
    subtitle = "Czy bardziej zurbanizowane kraje są bogatsze?",
    x = "Urbanizacja (% ludności miejskiej)",
    y = "PKB per capita (USD, skala log)",
    color = "Region świata"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom")

# 3. Zaawansowany wykres punktowy: rozmiar gospodarki a populacja ----
ggplot(kraje, aes(x = Populacja_mln, y = PKB, size = PKB_per_capita, color = Region)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Skala gospodarki i demografia",
    x = "Populacja (mln, log10)",
    y = "PKB (USD, log10)",
    size = "PKB per capita"
  ) +
  theme_minimal()

# 4. Prosty wykres słupkowy: liczba krajów w regionach ----
ggplot(kraje, aes(x = Region)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(
    title = "Liczba krajów w regionach świata",
    x = "Region",
    y = "Liczba krajów"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5))

# 5. Zaawansowany wykres słupkowy poziomy: TOP 15 najbogatszych krajów ----
kraje %>%
  arrange(desc(PKB_per_capita)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Panstwo, PKB_per_capita), y = PKB_per_capita, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "TOP 15 najbogatszych krajów świata (2016)",
    subtitle = "PKB per capita w USD",
    x = NULL,
    y = "PKB per capita (USD)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10))

# 6. Wykres pudełkowy (boxplot): dostęp do internetu według regionów ----
ggplot(kraje, aes(x = reorder(Region, Internet_proc., FUN = median), 
                  y = Internet_proc., fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(
    title = "Dostęp do internetu według regionów świata",
    subtitle = "(punkty to poszczególne kraje)",
    x = NULL,
    y = "Dostęp do internetu (% populacji)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none")

# 7. Wykres pudełkowy (boxplot): przyrost populacji według regionów ----
ggplot(kraje, aes(x = Region, y = Przyrost_populacji)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Tempo przyrostu populacji w regionach świata",
    subtitle = "(punkty to poszczególne kraje, linia przerywana = 0%)",
    x = "Region",
    y = "Przyrost populacji (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14))


# 7. EKSPORT ----

# Zapisanie ramki danych do pliku CSV
write.csv(kraje, "kraje_analiza.csv") 

# Zapisanie ramki danych do pliku Excel 
write_xlsx(kraje, "kraje_wynik.xlsx")

# Zapisz wszystkie wykresy – prawe dolne okno, zakładka Plots:
# Export -> Save as image
# Niestety każdy wykres trzeba zapisać ręcznie, nie ma prostej funkcji do masowego eksportu.