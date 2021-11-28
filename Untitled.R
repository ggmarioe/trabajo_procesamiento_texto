library(tidytext)
library(syuzhet)
library(stringr)
library(dplyr)
library(readr)
library(readtext)
library(tidyr)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(udpipe)

programa <- scan("./datos/programas_candidatos_2021/2021_parisi_partido-de-la-gente_primera-vuelta.txt", what = "char", encoding = "UTF-8") %>% 
  paste(collapse = " ")


sentimientos_programa <- get_nrc_sentiment(programa, language = "spanish")
sentimientos_programa

polaridad_programa <- programa %>% 
  get_sentences() %>% 
  get_sentiment(method = "nrc", language = "spanish") %>% 
  get_dct_transform()
polaridad_programa


sentimientos <- read_tsv('https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt') %>% 
  filter(lexicon == "nrc") %>% 
  select(palabra, sentimiento)

tibble(texto = programa) %>% 
  unnest_tokens(input = texto, output = palabra) %>% 
  inner_join(sentimientos) %>% 
    count(palabra,sentimiento, sort = T)

modelo_ancora <- udpipe_load_model(file = "datos/spanish-ancora-ud-2.5-191206.udpipe")

programa_anotado <- udpipe_annotate(modelo_ancora, programa) %>% as_tibble()

frecuencia_sentimientos_lemas <- programa_parisi_anotado %>% 
  inner_join(sentimientos, by = c("lemma" = "palabra")) %>% 
  count(lemma,sentimiento, sort = T)


frecuencia_sentimientos_lemas %>% 
  filter(!sentimiento %in% c("positivo","negativo")) %>% 
  group_by(sentimiento) %>% 
  slice_max(n, n = 10) %>% 
  ggplot(aes(y = reorder(lemma, n),
            x = n,
            fill = sentimiento )) + 
    geom_col(show.legend = F)
    facet_wrap(~sentimiento, scales = "free") + 
    labs(y = NULL)