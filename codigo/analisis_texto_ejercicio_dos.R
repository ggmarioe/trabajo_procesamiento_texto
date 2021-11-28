
library(tidytext)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggplot2)
library(tokenizers)
library(tidyverse)
# library(textreadr)
library(rvest)
library(beepr)
library(robotstxt)

paths_allowed(paths = c("https://www.elmostrador.cl/claves/cambio-climatico"))

base_url = "https://www.elmostrador.cl/claves/cambio-climatico"

titulares <- c()

extraer_titulares <- function(numero_pagina){
  Sys.sleep(3)
  
  enlace <- paste0("https://www.elmostrador.cl/claves/cambio-climatico/page/", numero_pagina)
  
  html <- read_html(enlace)
  
  titulares <- html %>% 
    html_elements("h4 a") %>% 
    html_text(trim = TRUE)
  
  enlaces <- html %>% 
    html_elements("h4 a") %>% 
    html_attr("href")
  
  tibble(titular = titulares, enlace_noticia = enlaces)
}

# map(1:30, extraer_titulares)
df_titulares <- map_df(1:30, extraer_titulares_cc)
beepr::beep(sound = 5)
  
  
# Cargar las stopwords desde 7 Partidas Digital
unas_stopwords <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")
# Cargar stopwords 
otras_stopwords <- tibble(palabra = c("mil", "miles", "ciento", "cientos", "millón", "millones", "pesos", "dólares"))

#df_titulares <- tibble(titulares)
#df_titulares

bigramas <- tibble(discurso = titulares) %>% 
  unnest_ngrams(input = discurso,
                output = bigrama,
                n = 2) %>% 
  count(bigrama, sort = TRUE) %>% 
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>% 
  filter(!palabra2 %in% unas_stopwords$palabra,
         !palabra2 %in% otras_stopwords$palabra,
         !palabra1 %in% unas_stopwords$palabra,
         !palabra1 %in% otras_stopwords$palabra) %>% 
  unite(col = "bigrama", c(palabra1, palabra2), sep = " ") 

bigramas

bigramas %>% 
  slice_max(n, n = 10) %>% 
  ggplot(aes(y = reorder(bigrama, n), x= n)) +
  geom_col(fill = "#E9967A") + # por nombre o código hexadecimal
  labs(x = "frecuencia",
       y = NULL,
       title = "Bigramas más utilizados en titulares acerca del cambio climático en el mostrador",
       subtitle = "Fuente: elmostrador.cl") +
  theme_minimal()

#guardar gráfico



