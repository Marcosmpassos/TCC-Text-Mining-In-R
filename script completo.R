# Universidade Federal Rural da Amazônia
# Bacharelado em Sistemas de Informação
# Código do TCC intitulado: "Análise de Conteúdo dos Tweets Publicados Pelos Parlamentares Brasileiros 
#                            Utilizando Técnicas de Mineração de Texto em Linguagem R"
# Orientadora: Aleksandra do Socorro da Silva
# Autor: @Marcos Martins dos Passos

install.packages ("rtweet")
install.packages ("xlsx")
install.packages ("dplyr")
install.packages ("tm")
install.packages ("wordcloud")
install.packages ("ggplot2")
 ***** testar aquelas outras bibliotecas tipo library(RColorBrewer) library(cluster)  library(fpc) 




library(rtweet)

consumer_key <- "consumer_key"
consumer_secret <- "consumer_secret"
access_token <- "access_token"
access_secret <- "access_secret"

create_token(
  app             = "TCC Marcos",
  consumer_key    = consumer_key,
  consumer_secret = consumer_secret,
  access_token    = access_token,
  access_secret   = access_secret)




#Coleta de 1 tweet de 150 perfis
*** verificar a quantidade /// tl_politicos <- get_timelines(c("alvarodias_", "majorolimpio", 
                                                               "davialcolumbre",  "RomarioOnze", 
                                                               "capitaoderrite", "senadorhumberto", 
                                                               "capalbertoneto", "PauloMartins10", 
                                                               "lpbragancabr", "IvanValente", 
                                                               "filipebarrost", "deppimentel", 
                                                               "mariadorosario", "lasiermartins", 
                                                               "tabataamaralsp", "randolfeap", 
                                                               "fernandapsol", "alessandromolon", 
                                                               "JoseMedeirosMT", "KatiaAbreu", 
                                                               "DanielPMERJ", "Flordelismk", 
                                                               "JFMargarida", "carlosjordy", 
                                                               "EduGiraoOficial", "CelioStudart", 
                                                               "SenadorKajuru", "marcelvanhattem", 
                                                               "celsorussomanno", "AbilioSantana_", 
                                                               "RogerioCorreia_", "Glauber_Braga", 
                                                               "SorayaThronicke", "guimaraes13PT", 
                                                               "professorLFG", "BohnGass", 
                                                               "padilhando", "luizaerundina", 
                                                               "HenriqueFontana", "deputadopeninha", 
                                                               "delegado_waldir", "aureacarolinax", 
                                                               "carlosaviana", "ContaratoSenado", 
                                                               "MajorVitorHugo", "marcosdoval", 
                                                               "OtoniDepFederal", "EderMauroPA", 
                                                               "joseserra_", "ToniettoChris", 
                                                               "Sen_Alessandro", "majorfabianadep", 
                                                               "caleromarcelo", "senadorjpprates ", 
                                                               "AlencarBraga13", "enioverri", 
                                                               "NiltoTatto", "MariliaArraes", 
                                                               "CarlosZarattini",  "dep_joserocha",
                                                               "Alice_Portugal", "rodrigoagost", 
                                                               "AlineSleutjes", "Oficialluizlima", 
                                                               "CoronelTadeu", "BetoFaroPT", 
                                                               "marciolabre", "heldersalomao", 
                                                               "maragabrilli", "SimoneTebetms", 
                                                               "JulioCesarRib", "LucasBarreto_AP", 
                                                               "ArnsFlavio", "depzeneto", 
                                                               "capitao_wagner", "FlavioBolsonaro",
                                                               "erosbiondini", "carlosveraspt", 
                                                               "pauloteixeira13", "leiladovolei", 
                                                               "MiltonVieiraOfc", "EdmilsonPSOL",
                                                               "ViniciusPoit", "josiasgomesba", 
                                                               "depjosenelto", "ZeRicardoAM ", 
                                                               "depmarcon", "ValdevanNoventa", 
                                                               "RenataAbreu1919", "Vinicius_Farah", 
                                                               "AlexisFonteyne", "jaqueswagner", 
                                                               "CelioMouraTO", "uczai", 
                                                               "BiradoPindare", "paulopaim", 
                                                               "GeneralGirao", "SigaRoberto_", 
                                                               "depjorgesolla", "depjorgesolla", 
                                                               "DelegadoFurtado", "marciombittar", 
                                                               "DepSostenes", "LucasRedecker", 
                                                               "dasilvabenedita", "mararocha4545",
                                                               "RPdeputado", "sorayasantos", 
                                                               "depnicoletti", "DepRosanaValle", 
                                                               "zeca_dirceu", "Vanderlan_VC", 
                                                               "DepSanderson", "eduardobismarck",
                                                               "marcospereira04", "juniorbozzella", 
                                                               "aroldomartins", "SenStyvenson", 
                                                               "LaercioFederal", "angelocoronel_", 
                                                               "RodrigoMaia", "Anastasia", 
                                                               "ciro_nogueira", "EliasVazGyn", 
                                                               "jader_barbalho", "JulianLemosopb1", 
                                                               "SenadorHeinze", "rpsenador", 
                                                               "senadorarose", "ZequinhaMarinho", 
                                                               "GurgelSoares"), n = 1) 143
library(dplyr)
tl_politicos <- select(tl_politicos, screen_name, verified, statuses_count, friends_count, followers_count)
library(xlsx)
write.xlsx(tl_politicos, "/tl_politicos.xlsx")
###########################

#Coleta dos perfis selecionados com mais de 2000 tweets publicados
tl_esquerda24 <- get_timelines(c("Alice_Portugal","jandira_feghali","eduardobismarck",
                                  "KatiaAbreu","caleromarcelo","alessandromolon",
                                  "BiradoPindare","professorLFG","Glauber_Braga",
                                  "taliriapetrone","aureacarolinax","luizaerundina",
                                  "fernandapsol","samiabomfim","IvanValente",
                                  "EdmilsonPSOL","MarceloFreixo","prof_rosaneide",
                                  "depjorgesolla","BetoFaroPT","uczai",
                                  "AlencarBraga13","jaqueswagner","depmarcon"), 
                                  n = 2000, retryonratelimit = TRUE)
                                  
tl_esquerda25 <- get_timelines(c("HenriqueFontana","enioverri", "gleisi",
                                  "MariliaArraes","heldersalomao","paulopaim",
                                  "dasilvabenedita","CarlosZarattini","ZeRicardoAM",
                                  "josiasgomesba","depzeneto","BohnGass",
                                  "senadorjpprates","erikakokay","mariadorosario",
                                  "pauloteixeira13","JFMargarida","zeca_dirceu",
                                  "guimaraes13PT","padilhando","senadorhumberto",
                                  "DeputadoFederal", "RogerioCorreia_", "NiltoTatto",
                                  "randolfeap"), n = 2000, retryonratelimit = TRUE)


tl_centro8 <- get_timelines(c("AecioNeves", "SigaRoberto_", "Anastasia",
                              "LucasRedecker", "joseserra_", "maragabrilli", 
                              "deputadopeninha", "erosbiondini"), 
                              n = 2000, retryonratelimit = TRUE)

tl_direita23 <- get_timelines(c("angelocoronel_","lpbragancabr","MiltonVieiraOfc",
                              "AlexisFonteyne","ViniciusPoit","RenataAbreu1919",
                              "CoronelTadeu", "SargentoFAHUR","aroldomartins",
                              "RodrigoMaia","MajorVitorHugo","DepSanderson",
                              "marcosdoval","juniorbozzella","marciolabre",
                              "EderMauroPA","carlosjordy","kimpkat",
                              "GurgelSoares","DanielPMERJ","GeneralGirao",
                              "davialcolumbre","OtoniDepFederal"),
                              n = 2000, retryonratelimit = TRUE)

tl_direita24 <- get_timelines(c("celsorussomanno","lasiermartins","JulioCesarRib",
                              "depjosenelto","carlosaviana","CarlaZambelli17",
                              "marcospereira04","SenadorHeinze","FlavioBolsonaro",
                              "LaercioFederal","Vanderlan_VC","BolsonaroSP",
                              "ciro_nogueira","PauloMartins10","marcelvanhattem",
                              "RomarioOnze","Biakicis","Flordelismk",
                              "DepSostenes","JoseMedeirosMT","marcofeliciano",
                              "joicehasselmann","SenadorKajuru","alvarodias_"),
                              n = 2000, retryonratelimit = TRUE)

tl_esquerda49 <- rbind(tl_esquerda24, tl_esquerda25)
tl_direita47 <- rbind(tl_direita24, tl_direita23 

tl_direita47 <- select(tl_direita47, screen_name, text, description)
tl_centro8 <- select(tl_centro8, screen_name, text, description)
tl_esquerda49 <- select(tl_esquerda49, screen_name, text, description)
##############################

# Mineração de Texto
library(tm)
library(wordcloud)
library(ggplot2)

# DESCRIÇÃO 
# Descrição Direita

descr_direita_text <- unique(tl_direita47$description)
#Criando e limpando o corpus
descr_direita_corpus <- VCorpus(VectorSource(descr_direita_text))
descr_direita_corpus <- tm_map(descr_direita_corpus, content_transformer(function(x)
                        iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
descr_direita_corpus <- tm_map(descr_direita_corpus, content_transformer(tolower))
descr_direita_corpus <- tm_map(descr_direita_corpus, removePunctuation) 
descr_direita_corpus <- tm_map(descr_direita_corpus,removeWords, stopwords("portuguese"))
descr_direita_corpus <- tm_map(descr_direita_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

formatacao <- brewer.pal(4, "Dark2")
wordcloud(descr_direita_corpus,min.freq=2,max.words=100, random.order=FALSE, rot.per=0.35, colors=formatacao)
#Limpeza do texto com a Document Term Matrix
descr_direita_dtm <- DocumentTermMatrix(descr_direita_corpus)   
#Removendo termos esparsos
descr_direita_frequencia <- colSums(as.matrix(descr_direita_dtm))   
descr_direita_frequencia <- sort(colSums(as.matrix(descr_direita_dtm)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para o plot
descr_direita_plot <- data.frame(word=names(descr_direita_frequencia), freq=descr_direita_frequencia)  
#Criando o grafico
grafico <- ggplot(subset(descr_direita_plot, descr_direita_frequencia >= 3), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") + 
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.y=element_text(angle=0, hjust=1)) +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais 
                            parlamentares de direita na descrição do Twitter") +
                  labs(y="Frequência", x = "Termos")
grafico
 
# Descrição Centro
descr_centro_text <- unique(tl_centro8$description)
#Criando e limpando o corpus
descr_centro_corpus <- VCorpus(VectorSource(descr_centro_text))
descr_centro_corpus <- tm_map(descr_centro_corpus, content_transformer(function(x) 
                       iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
descr_centro_corpus <- tm_map(descr_centro_corpus, content_transformer(tolower))
descr_centro_corpus <- tm_map(descr_centro_corpus, removePunctuation) 
descr_centro_corpus <- tm_map(descr_centro_corpus,removeWords, stopwords("portuguese"))
descr_centro_corpus <- tm_map(descr_centro_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

#Primeira visualizaçao
formatacao <- brewer.pal(4, "Dark2")
wordcloud(descr_centro_corpus,min.freq=1,max.words=100, random.order=FALSE, rot.per=0.35, colors=formatacao)
#Limpeza do texto com a Document Term Matrix
descr_centro_dtm <- DocumentTermMatrix(descr_centro_corpus)   
#Removendo termos esparsos
descr_centro_frequencia <- colSums(as.matrix(descr_centro_dtm))   
descr_centro_frequencia <- sort(colSums(as.matrix(descr_centro_dtm)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para o plot
descr_centro_plot <- data.frame(word=names(descr_centro_frequencia), freq=descr_centro_frequencia)  
#Criando o grafico
grafico <- ggplot(subset(descr_centro_plot, descr_centro_frequencia >= 2), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") + 
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.y=element_text(angle=0, hjust=1)) +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais parlamentares de centro na descrição do Twitter") +
                  labs(y="Frequência", x = "Termos")
grafico 

# Descrição Esquerda

descr_esquerda_text <- unique(tl_esquerda49$description)
#Criando e limpando o corpus
descr_esquerda_corpus <- VCorpus(VectorSource(descr_esquerda_text))
descr_esquerda_corpus <- tm_map(descr_esquerda_corpus, content_transformer(function(x) 
                         iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
descr_esquerda_corpus <- tm_map(descr_esquerda_corpus, content_transformer(tolower))
descr_esquerda_corpus <- tm_map(descr_esquerda_corpus, removePunctuation) 
descr_esquerda_corpus <- tm_map(descr_esquerda_corpus,removeWords, stopwords("portuguese"))
descr_esquerda_corpus <- tm_map(descr_esquerda_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

#Primeira visualizaçao
formatacao <- brewer.pal(4, "Dark2")
wordcloud(descr_esquerda_corpus,min.freq=2,max.words=100, random.order=FALSE, rot.per=0.35, colors=formatacao)
#Limpeza do texto com a Document Term Matrix
descr_esquerda_dtm <- DocumentTermMatrix(descr_esquerda_corpus)   
#Removendo termos esparsos
descr_esquerda_frequencia <- colSums(as.matrix(descr_esquerda_dtm))   
descr_esquerda_frequencia <- sort(colSums(as.matrix(descr_esquerda_dtm)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para o plot
descr_esquerda_plot <- data.frame(word=names(descr_esquerda_frequencia), freq=descr_esquerda_frequencia)  
#Criando o grafico
grafico <- ggplot(subset(descr_esquerda_plot, descr_esquerda_frequencia >= 3), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") + 
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.y=element_text(angle=0, hjust=1)) +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais parlamentares de esquerda na descrição do Twitter") +
                  labs(y="Frequência", x = "Termos")
grafico

 ########################
 # Atividade ao longo dos meses 

 # Direita
 tl_direita47 %>%
  ts_plot("1 months") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Frequência de Tweets postados por influenciadores políticos de direita",
                subtitle = "Contagem de Tweets até o dia 15 de setembro de 2019",
                caption = "\nFonte: Dados coletados da API do Twitter pela biblioteca 'rtweet'")

# Centro
 tl_centro8 %>%
  ts_plot("1 months") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(x = NULL, y = NULL,
                title = "Frequência de Tweets postados por influenciadores políticos de centro",
                subtitle = "Contagem de Tweets até o dia 15 de setembro de 2019",
                caption = "\nFonte: Dados coletados da API do Twitter pela biblioteca 'rtweet'")

#Frequencia individual de centro
cbPalette <- c("#FF0000", "#B18904", "#5FB404", 
               "#04B4AE", "#08088A", "#8A0886", 
               "#6E6E6E", "#000000")

## plota o grafico referente as postagens das contas
tl_centro8 %>%
  dplyr::filter(created_at > "2010-01-01") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("month", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank(),
                 legend.position = "bottom",
                 plot.title = ggplot2::element_text(face = "bold")
  ) +  
  ggplot2::scale_color_manual(values = cbPalette) +
  ggplot2::labs( x = NULL, y = NULL,
                title = "Frequência de Tweets postados por influenciadores políticos de centro",
                subtitle = "Contagem de Tweets até o dia 15 de setembro de 2019",
                caption = "\nFonte: Dados coletados da API do Twitter pela biblioteca 'rtweet'"
  )

# Esquerda
 tl_esquerda49 %>%
  ts_plot("1 months") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(x = NULL, y = NULL,  
                title = "Frequência de Tweets postados por influenciadores políticos de esquerda",
                subtitle = "Contagem de Tweets até o dia 15 de setembro de 2019",
                caption = "\nFonte: Dados coletados da API do Twitter pela biblioteca 'rtweet'"
)

###############################
#Analise por período

#organizando direita 2018-2019
direita_pre <- subset(tl_direita47, 
                        created_at >= "2018-01-01 00:00:00" & 
                        created_at <= "2018-07-20 00:00:00")

direita_camp <- subset(tl_direita47, 
                         created_at >= "2018-08-16 00:00:00" & 
                         created_at <= "2018-10-07 00:00:00")

direita2019 <- subset(tl_direita47, 
                         created_at >= "2019-01-01 00:00:00")

#organizando centro 2018-2019

centro_pre <- subset(tl_esquerda49, 
                       created_at >= "2018-01-01 00:00:00" & 
                       created_at <= "2018-07-20 00:00:00")

centro_camp <- subset(tl_esquerda49, 
                       created_at >= "2018-08-16 00:00:00" & 
                       created_at <= "2018-10-07 00:00:00")

centro2019 <- subset(tl_esquerda49, 
                       created_at >= "2019-01-01 00:00:00")

#organizando esquerda 2018-2019

esquerda_pre <- subset(tl_esquerda49, 
                         created_at >= "2018-01-01 00:00:00" & 
                         created_at <= "2018-07-20 00:00:00")

esquerda_camp <- subset(tl_esquerda49, 
                         created_at >= "2018-08-16 00:00:00" & 
                         created_at <= "2018-10-07 00:00:00")

esquerda2019 <- subset(tl_esquerda49, 
                         created_at >= "2019-01-01 00:00:00")


######################################
#O trabalho de Mineraçao de Textos (Text Mining) - Tweets da direita antes da campanha eleitoral

#Seleciona a coluna text do dataframe direita_pre
direita_pre_text <- direita_pre$text
#Criando e limpando o corpus
direita_pre_corpus <- VCorpus(VectorSource(direita_pre_text))
direita_pre_corpus <- tm_map(direita_pre_corpus, content_transformer(function(x) 
                      iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
#Transforma todos os caracteres para minúsculos
direita_pre_corpus <- tm_map(direita_pre_corpus, content_transformer(tolower))]
#Remove pontuações
direita_pre_corpus <- tm_map(direita_pre_corpus, removePunctuation) 
#Remove as stopwords
direita_pre_corpus <- tm_map(direita_pre_corpus,removeWords, stopwords("portuguese"))
#Remove links e palavras específicas
direita_pre_corpus <- tm_map(direita_pre_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

#Primeira visualizaçao
formatacao <- brewer.pal(8, "Dark2")
wordcloud(direita_pre_corpus,min.freq=2,max.words=75, random.order=FALSE, rot.per=0.35, colors=formatacao)


#Limpeza do texto com a Document Term Matrix
direita_pre_dtm <- DocumentTermMatrix(direita_pre_corpus)   
#Removendo termos esparsos
direita_pre_dtms <- removeSparseTerms(direita_pre_dtm, 0.98) 
direita_pre_frequencia <- colSums(as.matrix(direita_pre_dtms))   
direita_pre_frequencia <- sort(colSums(as.matrix(direita_pre_dtms)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para plotagem de gráficos
direita_pre_plot <- data.frame(word=names(direita_pre_frequencia), freq=direita_pre_frequencia)  




#Criando o grafico
grafico <- ggplot(subset(direita_pre_plot, direita_pre_frequencia >= 83), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") + 
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.y=element_text(angle=0, hjust=1)) +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais 
                          parlamentares de direita antes da Campanha Eleitoral de 2018") +
                  labs(y="Frequência", x = "Termos")
grafico   

#Clustering - Dendograma
distancia <- dist(t(direita_pre_dtms), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-50, main = "Dendograma Tweets dos Atuais Parlamentares da Direita Antes da Campanha Eleitoral de 2018",
     xlab = "Distância",
     ylab = "Altura")

# DIREITA CAMPANHA
direita_camp_text <- direita_camp$text
#Criando e limpando o corpus
direita_camp_corpus <- VCorpus(VectorSource(direita_camp_text))
direita_camp_corpus <- tm_map(direita_camp_corpus, content_transformer(function(x) ]
                       iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
direita_camp_corpus <- tm_map(direita_camp_corpus, content_transformer(tolower))
direita_camp_corpus <- tm_map(direita_camp_corpus, removePunctuation) 
direita_camp_corpus <- tm_map(direita_camp_corpus,removeWords, stopwords("portuguese"))
direita_camp_corpus <- tm_map(direita_camp_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

formatacao <- brewer.pal(4, "Dark2")
wordcloud(direita_camp_corpus,min.freq=2,max.words=75, random.order=FALSE, rot.per=0.35, colors=formatacao)
#Limpeza do texto com a Document Term Matrix
direita_camp_dtm <- DocumentTermMatrix(direita_camp_corpus)   
#Removendo termos esparsos
direita_camp_dtms <- removeSparseTerms(direita_camp_dtm, 0.966) 
direita_camp_frequencia <- colSums(as.matrix(direita_camp_dtms))   
direita_camp_frequencia <- sort(colSums(as.matrix(direita_camp_dtms)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para o plot
direita_camp_plot <- data.frame(word=names(direita_camp_frequencia), freq=direita_camp_frequencia)  
#Criando o grafico
grafico <- ggplot(subset(direita_camp_plot, direita_camp_frequencia >= 218), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") + 
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.y=element_text(angle=0, hjust=1)) +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais parlamentares de direita 
                            durante campanha eleitoral de 2018") +
                  labs(y="Frequência", x = "Termos")
grafico   

#Clustering - Dendograma
distancia <- dist(t(direita_camp_dtms), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-1,main = "Dendograma Tweets dos Atuais Parlamentares da Direita Durante Campanha Eleitoral de 2018",
     xlab = "Distância",
     ylab = "Altura")  


# DIREITA 2019
direita_2019_text <- direita2019$text
#Criando e limpando o corpus
direita_2019_corpus <- VCorpus(VectorSource(direita_2019_text))
direita_2019_corpus <- tm_map(direita_2019_corpus, content_transformer(function(x) 
                       iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
direita_2019_corpus <- tm_map(direita_2019_corpus, content_transformer(tolower))
direita_2019_corpus <- tm_map(direita_2019_corpus, removePunctuation) 
direita_2019_corpus <- tm_map(direita_2019_corpus,removeWords, stopwords("portuguese"))
direita_2019_corpus <- tm_map(direita_2019_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

#Primeira visualizaçao
formatacao <- brewer.pal(4, "Dark2")
wordcloud(direita_2019_corpus,min.freq=2,max.words=100, random.order=FALSE, rot.per=0.35, colors=formatacao)
#Limpeza do texto com a Document Term Matrix
direita_2019_dtm <- DocumentTermMatrix(direita_2019_corpus)   
#Removendo termos esparsos
direita_2019_dtms <- removeSparseTerms(direita_2019_dtm, 0.98) 
direita_2019_frequencia <- colSums(as.matrix(direita_2019_dtms))   
direita_2019_frequencia <- sort(colSums(as.matrix(direita_2019_dtms)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para o plot
direita_2019_plot <- data.frame(word=names(direita_2019_frequencia), freq=direita_2019_frequencia)  
#Criando o grafico
grafico <- ggplot(subset(direita_2019_plot, direita_2019_frequencia >= 1137), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") + 
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.y=element_text(angle=0, hjust=1)) +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais parlamentares de direita em 2019") +
                  labs(y="Frequência", x = "Termos")
grafico   

#Clustering - Dendograma
distancia <- dist(t(direita_2019_dtms), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-1,main = "Dendograma Tweets dos Atuais Parlamentares da Direita em 2019",
     xlab = "Distância",
     ylab = "Altura")  
#############

# CENTRO PRÉ CAMPANHA
centro_pre_text <- centro_pre$text

#Criando e limpando o corpus
centro_pre_corpus <- VCorpus(VectorSource(centro_pre_text))
centro_pre_corpus <- tm_map(centro_pre_corpus, content_transformer(function(x) 
                     iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
centro_pre_corpus <- tm_map(centro_pre_corpus, content_transformer(tolower))
centro_pre_corpus <- tm_map(centro_pre_corpus, removePunctuation) 
centro_pre_corpus <- tm_map(centro_pre_corpus,removeWords, stopwords("portuguese"))
centro_pre_corpus <- tm_map(centro_pre_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

#Primeira visualizaçao
formatacao <- brewer.pal(4, "Dark2")
wordcloud(centro_pre_corpus,min.freq=2,max.words=75, random.order=FALSE, rot.per=0.35, colors=formatacao)
#Limpeza do texto com a Document Term Matrix
centro_pre_dtm <- DocumentTermMatrix(centro_pre_corpus)   
#Removendo termos esparsos
centro_pre_dtms <- removeSparseTerms(centro_pre_dtm, 0.974) 
centro_pre_frequencia <- colSums(as.matrix(centro_pre_dtms))   
centro_pre_frequencia <- sort(colSums(as.matrix(centro_pre_dtms)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para o plot
centro_pre_plot <- data.frame(word=names(centro_pre_frequencia), freq=centro_pre_frequencia)  

#Criando o grafico
grafico <- ggplot(subset(centro_pre_plot, centro_pre_frequencia >= 51), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") +
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.x=element_text(angle=45, hjust=1)) +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais parlamentares de centro 
                            antes da campanha eleitoral de 2018") +
                  labs(y="Frequência", x = "Termos")
grafico   


#Clustering - Dendograma
distancia <- dist(t(centro_pre_dtms), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-50,main = "Dendograma - Tweets dos Atuais Parlamentares de centro Antes da Campanha Eleitoral de 2018",
     xlab = "Distância",
     ylab = "Altura") 

# CENTRO CAMPANHA
centro_camp_text <- centro_camp$text

#Criando e limpando o corpus
centro_camp_corpus <- VCorpus(VectorSource(centro_camp_text))
centro_camp_corpus <- tm_map(centro_camp_corpus, content_transformer(function(x) 
                      iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
centro_camp_corpus <- tm_map(centro_camp_corpus, content_transformer(tolower))
centro_camp_corpus <- tm_map(centro_camp_corpus, removePunctuation) 
centro_camp_corpus <- tm_map(centro_camp_corpus,removeWords, stopwords("portuguese"))
centro_camp_corpus <- tm_map(centro_camp_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

formatacao <- brewer.pal(4, "Dark2")
wordcloud(centro_camp_corpus,min.freq=2,max.words=75, random.order=FALSE, rot.per=0.35, colors=formatacao)




#Limpeza do texto com a Document Term Matrix
centro_camp_dtm <- DocumentTermMatrix(centro_camp_corpus)   
#Removendo termos esparsos
centro_camp_dtms <- removeSparseTerms(centro_camp_dtm, 0.950) 
centro_camp_frequencia <- colSums(as.matrix(centro_camp_dtms))   
centro_camp_frequencia <- sort(colSums(as.matrix(centro_camp_dtms)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para o plot
centro_camp_plot <- data.frame(word=names(centro_camp_frequencia), freq=centro_camp_frequencia)  





#Criando o grafico
grafico <- ggplot(subset(centro_camp_plot, centro_camp_frequencia >= 73), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") + 
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.y=element_text(angle=0, hjust=1)) +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais parlamentares de centro 
                           durante a campanha eleitoral de 2018") +
                  labs(y="Frequência", x = "Termos")
grafico   

#Clustering - Dendograma
distancia <- dist(t(centro_camp_dtms), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-50,main = "Dendograma - Tweets dos Atuais Parlamentares de Centro Durante a Campanha Eleitoral de 2018",
     xlab = "Distância",
     ylab = "Altura")  

# CENTRO 2019
centro2019_text <- centro2019$text
#Criando e limpando o corpus
centro2019_corpus <- VCorpus(VectorSource(centro2019_text))
centro2019_corpus <- tm_map(centro2019_corpus, content_transformer(function(x) 
                     iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
centro2019_corpus <- tm_map(centro2019_corpus, content_transformer(tolower))
centro2019_corpus <- tm_map(centro2019_corpus, removePunctuation) 
centro2019_corpus <- tm_map(centro2019_corpus,removeWords, stopwords("portuguese"))
centro2019_corpus <- tm_map(centro2019_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

formatacao <- brewer.pal(4, "Dark2")
wordcloud(centro2019_corpus,min.freq=2,max.words=100, random.order=FALSE, rot.per=0.35, colors=formatacao)
#Limpeza do texto com a Document Term Matrix
centro2019_dtm <- DocumentTermMatrix(centro2019_corpus)   
#Removendo termos esparsos
centro2019_dtms <- removeSparseTerms(centro2019_dtm, 0.963)
centro2019_frequencia <- colSums(as.matrix(centro2019_dtms))   
centro2019_frequencia <- sort(colSums(as.matrix(centro2019_dtms)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para o plot
centro2019_plot <- data.frame(word=names(centro2019_frequencia), freq=centro2019_frequencia)  
#Criando o grafico
grafico <- ggplot(subset(centro2019_plot, centro2019_frequencia >= 143), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") + 
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.y=element_text(angle=0, hjust=1))  +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais parlamentares de centro em 2019") +
                  labs(y="Frequência", x = "Termos")
grafico   

#Clustering - Dendograma
distancia <- dist(t(centro2019_dtms), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-50,main = "Dendograma - Tweets dos Atuais Parlamentares de Centro em 2019",
     xlab = "Distância",
     ylab = "Altura")  

##########

#ESQUERDA PRÉ CAMPANHA
esquerda_pre_text <- esquerda_pre$text
#Criando e limpando o corpus
esquerda_pre_corpus <- VCorpus(VectorSource(esquerda_pre_text))
esquerda_pre_corpus <- tm_map(esquerda_pre_corpus, content_transformer(function(x) 
                       iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
esquerda_pre_corpus <- tm_map(esquerda_pre_corpus, content_transformer(tolower))
esquerda_pre_corpus <- tm_map(esquerda_pre_corpus, removePunctuation) 
esquerda_pre_corpus <- tm_map(esquerda_pre_corpus,removeWords, stopwords("portuguese"))
esquerda_pre_corpus <- tm_map(esquerda_pre_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

formatacao <- brewer.pal(4, "Dark2")
wordcloud(esquerda_pre_corpus,min.freq=2,max.words=75, random.order=FALSE, rot.per=0.35, colors=formatacao)
#Limpeza do texto com a Document Term Matrix
esquerda_pre_dtm <- DocumentTermMatrix(esquerda_pre_corpus)   
#Removendo termos esparsos
esquerda_pre_dtms <- removeSparseTerms(esquerda_pre_dtm, 0.977) 
esquerda_pre_frequencia <- colSums(as.matrix(esquerda_pre_dtms))   
esquerda_pre_frequencia <- sort(colSums(as.matrix(esquerda_pre_dtms)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para o plot
esquerda_pre_plot <- data.frame(word=names(esquerda_pre_frequencia), freq=esquerda_pre_frequencia)  
#Criando o grafico
grafico <- ggplot(subset(esquerda_pre_plot, esquerda_pre_frequencia >= 143), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") + 
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.y=element_text(angle=0, hjust=1)) +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais parlamentares de esquerda 
                           antes da campanha eleitoral de 2018") +
                  labs(y="Frequência", x = "Termos")
grafico   

#Clustering - Dendograma
distancia <- dist(t(esquerda_pre_dtms), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-50,main = "Dendograma - Tweets dos Atuais Parlamentares de Esquerda Antes da Campanha Eleitoral de 2018",
     xlab = "Distância",
     ylab = "Altura")  

# ESQUERDA CAMPANHA
esquerda_camp_text <- esquerda_camp$text

#Criando e limpando o corpus
esquerda_camp_corpus <- VCorpus(VectorSource(esquerda_camp_text))
esquerda_camp_corpus <- tm_map(esquerda_camp_corpus, content_transformer(function(x) 
                        iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
esquerda_camp_corpus <- tm_map(esquerda_camp_corpus, content_transformer(tolower))
esquerda_camp_corpus <- tm_map(esquerda_camp_corpus, removePunctuation) 
esquerda_camp_corpus <- tm_map(esquerda_camp_corpus,removeWords, stopwords("portuguese"))
esquerda_camp_corpus <- tm_map(esquerda_camp_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

formatacao <- brewer.pal(4, "Dark2")
wordcloud(esquerda_camp_corpus,min.freq=2,max.words=75, random.order=FALSE, rot.per=0.35, colors=formatacao)

#Limpeza do texto com a Document Term Matrix
esquerda_camp_dtm <- DocumentTermMatrix(esquerda_camp_corpus)   
#Removendo termos esparsos
esquerda_camp_dtms <- removeSparseTerms(esquerda_camp_dtm, 0.966) 
esquerda_camp_frequencia <- colSums(as.matrix(esquerda_camp_dtms))   
esquerda_camp_frequencia <- sort(colSums(as.matrix(esquerda_camp_dtms)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para o plot
esquerda_camp_plot <- data.frame(word=names(esquerda_camp_frequencia), freq=esquerda_camp_frequencia)  

#Criando o grafico
grafico <- ggplot(subset(esquerda_camp_plot, esquerda_camp_frequencia >= 122), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") + 
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.y=element_text(angle=0, hjust=1)) +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais parlamentares de esquerda 
                            durante a campanha eleitoral de 2018") +
                  labs(y="Frequência", x = "Termos")
grafico   

#Clustering - Dendograma
distancia <- dist(t(esquerda_camp_dtms), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-50,main = "Dendograma - Tweets dos Atuais Parlamentares de Esquerda Durante a Campanha Eleitoral de 2018",
     xlab = "Distância",
     ylab = "Altura")  

# ESQUERDA 2019
esquerda2019_text <- esquerda2019$text

#Criando e limpando o corpus
esquerda2019_corpus <- VCorpus(VectorSource(esquerda2019_text))
esquerda2019_corpus <- tm_map(esquerda2019_corpus, content_transformer(function(x) 
                       iconv(x, from='UTF-8', to='latin2//TRANSLIT', sub='byte')))
esquerda2019_corpus <- tm_map(esquerda2019_corpus, content_transformer(tolower))
esquerda2019_corpus <- tm_map(esquerda2019_corpus, removePunctuation) 
esquerda2019_corpus <- tm_map(esquerda2019_corpus,removeWords, stopwords("portuguese"))
esquerda2019_corpus <- tm_map(esquerda2019_corpus, removeWords, c("http[[:alnum:][:punct:]]*", 
                                                                  "não", "nao","faz", "tudo", "quer",
                                                                  "aqui", "sim", "dias", "ainda", 
                                                                  "pode", "pra", "para", "gente", 
                                                                  "sao", "assim", "ver", "deve", 
                                                                  "porque", "todos", "todas", 
                                                                  "fazer", "onde", "estao", "hoje", 
                                                                  "pouco", "f09f9898","ser", "sao", 
                                                                  "dia", "voce", "vai", "sobre", 
                                                                  "pode", "diz", "nada", "apenas", 
                                                                  "cada", "bom", "mundo", "todo", 
                                                                  "nunca", "mim","voces", "via", 
                                                                  "campcisa", "bem", "vez", "maior", 
                                                                  "semana", "grande","menos", "agora", 
                                                                  "dar", "momento", "todo", "ter", 
                                                                  "outros", "anos", "dia", "noite", 
                                                                  "boa", "nesta", "vou", "neste", "tarde", 
                                                                  "manha", "nesse", "sempre", "melhor", 
                                                                  "nessa", "olá"))

formatacao <- brewer.pal(4, "Dark2")
wordcloud(esquerda2019_corpus,min.freq=2,max.words=100, random.order=FALSE, rot.per=0.35, colors=formatacao)
#Limpeza do texto com a Document Term Matrix
esquerda2019_dtm <- DocumentTermMatrix(esquerda2019_corpus)   
#Removendo termos esparsos
esquerda2019_dtms <- removeSparseTerms(esquerda2019_dtm, 0.972) 
esquerda2019_frequencia <- colSums(as.matrix(esquerda2019_dtms))   
esquerda2019_frequencia <- sort(colSums(as.matrix(esquerda2019_dtms)), decreasing=TRUE) 
#Convertendo a matriz de frequencia em dataframe para o plot
esquerda2019_plot <- data.frame(word=names(esquerda2019_frequencia), freq=esquerda2019_frequencia)  

#Criando o grafico
grafico <- ggplot(subset(esquerda2019_plot, esquerda2019_frequencia >= 2091), 
                  aes(x = reorder(word, -freq), y = freq)) +
                  geom_bar(stat = "identity") + 
                  coord_flip()+
                  geom_col() +
                  theme(axis.text.y=element_text(angle=0, hjust=1)) +
                  ggtitle("Gráfico de barras com os termos mais usados pelos atuais parlamentares de esquerda em 2019") +
                  labs(y="Frequência", x = "Termos")
grafico   

#Clustering - Dendograma
distancia <- dist(t(esquerda2019_dtms), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-50,main = "Dendograma - Tweets dos Atuais Parlamentares de Esquerda no ano de 2019",
     xlab = "Distância",
     ylab = "Altura") 
#################################