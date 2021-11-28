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


programa <- scan("./datos/programas_candidatos_2021/2021_parisi_partido-de-la-gente_primera-vuelta.txt", what = "char", encoding = "UTF-8") %>% 
              paste(collapse = " ")

sentimientos_programa <- get_nrc_sentiment(programa, language = "spanish")
polaridad_programa <- programa %>% 
                        get_sentences() %>% 
                        get_sentiment(method = "nrc", language="spanish")


head(polaridad_programa,40)
unas_stopwords <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")
sentimientos <- read_tsv('https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt') %>% 
  filter(lexicon == "nrc") %>% 
  select(palabra, sentimiento)


#Ejercicio 4.1
tibble(indice = 1:length(polaridad_programa), polaridad = polaridad_programa) %>% 
  ggplot(aes(indice,polaridad )) + 
  geom_line()

frecuencia_sentimientos <- tibble(texto = programa) %>% 
                              unnest_tokens(input = texto,                                             
                                            output = palabra) %>% 
                              inner_join(sentimientos) %>% 
                              count(palabra, sentimiento, sort = T)



frecuencia_sentimientos %>% filter(sentimiento %in% c("positivo", "negativo")) %>% 
  group_by(sentimiento) %>% 
  slice_max(n , n = 10) %>% 
  ggplot(aes(y = reorder(palabra,n), x = n, fill = sentimiento)) + 
  geom_col() +
  facet_wrap(~sentimiento, scales = "free") + 
  labs(y = NULL)


library(udpipe)
# 4.3 
## descargar los datos según lo que necesito
udpipe_download_model(language = "spanish-ancora", model_dir = "datos")
modelo_ancora <- udpipe_load_model(file = "datos/spanish-ancora-ud-2.5-191206.udpipe")
programa_ancora <- udpipe_annotate(object = modelo_ancora, x = programa) %>% as_tibble()

View(programa_ancora)

programa_anotado <- udpipe_annotate(modelo_ancora, programa) %>% as_tibble()

frecuencia_sentimientos_lemas <- programa_parisi_anotado %>% 
  inner_join(sentimientos, by = c("lemma" = "palabra")) %>% 
  count(lemma,sentimiento, sort = T)

frecuencia_sentimientos_lemas %>% filter(sentimiento %in% c("positivo", "negativo")) %>% 
  group_by(sentimiento) %>% 
  slice_max(n , n = 10) %>% 
  ggplot(aes(y = reorder(lemma,n), x = n, fill = sentimiento)) + 
  geom_col() +
  facet_wrap(~sentimiento, scales = "free") + 
  labs(y = NULL)



# 4.4
frecuencia_sentimientos_lemas %>% 
  filter(!sentimiento %in% c("positivo","negativo")) %>% 
  group_by(sentimiento) %>% 
  slice_max(n, n = 10) %>% 
  ggplot(aes(y = reorder(lemma, n),
             x = n,
             fill = sentimiento )) + 
  geom_col(show.legend = F) +
  facet_wrap(~sentimiento, scales = "free") + 
  labs(y = NULL)






