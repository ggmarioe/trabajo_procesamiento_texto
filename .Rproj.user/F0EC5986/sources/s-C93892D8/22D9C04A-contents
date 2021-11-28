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

polaridad_programa <- sentimientos_programa %>% 
                        get_sentences() %>% 
                        get_sentiment(method = "nrc", language="spanish")


head(polaridad_programa,40)

#Ejercicio 4.1
tibble(indice = 1:length(polaridad_programa), polaridad = polaridad_programa) %>% 
  ggplot(aes(indice,polaridad )) + 
  geom_line()



diccionario <- read_tsv('https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt') %>% 
                              filter(lexicon == "nrc") %>% 
                              select(palabra, sentimiento)

diccionario

frecuencia_sentimientos <- tibble(texto = programa) %>% 
                              unnest_tokens(input = texto,                                             
                                            output = palabra) %>% 
                              inner_join(diccionario) %>% 
                              count(palabra, sentimiento, sort = T)


frecuencia_sentimientos %>% filter(sentimiento %in% c("positivo", "negativo")) %>% 
                              group_by(sentimiento) %>% 
                              slice_max(n , n = 10) %>% 
                              ggplot(aes(y = reorder(palabra,n), x = n, fill = sentimiento)) + 
                              geom_col()

frecuencia_sentimientos %>% filter(sentimiento %in% c("positivo", "negativo")) %>% 
  group_by(sentimiento) %>% 
  slice_max(n , n = 10) %>% 
  ggplot(aes(y = reorder(palabra,n), x = n, fill = sentimiento)) + 
  geom_col()

frecuencia_sentimientos %>% filter(sentimiento %in% c("positivo", "negativo")) %>% 
  group_by(sentimiento) %>% 
  slice_max(n , n = 10) %>% 
  ggplot(aes(y = reorder(palabra,n), x = n, fill = sentimiento)) + 
  geom_col() +
  facet_wrap(~sentimiento, scales = "free") + 
  labs(y = NULL)




library(udpipe)










sentimientos_programa <- sentimientos %>% 
  pivot_longer(anger:positive, names_to = "sentimiento", values_to = "frecuencia") %>% 
  mutate(sentimiento = c("enojo", "anticipación", "disgusto", "miedo", "alegría", "tristeza", "sorpresa", "confianza", "negativo", "positivo"))



frecuencia_sentimientos %>% 
  filter(sentimiento %in% c("negativo", "positivo")) %>% 
  ggplot(aes(reorder(sentimiento, -frecuencia), frecuencia, fill = sentimiento)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL,
       title = 'Frecuencia de palabras según sentimiento',
       subtitle = "Parisi 2021") +
  theme_minimal()


