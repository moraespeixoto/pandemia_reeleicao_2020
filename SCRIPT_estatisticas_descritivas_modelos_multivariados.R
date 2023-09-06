##########################################################

# Estatisticas descritivas e analises multivariadas      #
                                                         
##########################################################

# 1.0 Abrir pacotes e bases ----

## 1.1 Pacotes utilizados

pacman::p_load(tidyverse, ggthemes,
               stats, patchwork, 
               kableExtra, sjPlot, 
               caret, performance, 
               rstatix, MetBrewer)

library(ggstatsplot) # pacote pode não abrir com pacman e reinstalar involutariamente

## 1.2 Bases


load ("banco_pandemia_reeleicao.Rda") 
load("grafico_reeleicao.Rda")

### Renomear variável de gastos de campaha

banco_pandemia_reeleicao <- banco_pandemia_reeleicao %>% 
  rename(`% despesa do candidato` = `% despesa por candidato`)

### Criar a variavel situacao em 2020 do prefeito eleito em 2016

banco_pandemia_reeleicao <- banco_pandemia_reeleicao %>% 
  mutate(situacao_2020 = case_when(sit_20_sem_suplementar ==  1 ~ "Reeleito",
                                   sit_20_sem_suplementar ==  2 ~ "Não reeleito",
                                   sit_20_sem_suplementar ==  3 ~ "Não tentou",
                                   sit_20_sem_suplementar ==  4 ~ "Impedido"))




## 1.3 Set theme

tema <- theme_fivethirtyeight() +
  theme(legend.title = element_text(family = "Times New Roman"), 
        legend.position = "bottom",
        title = element_text(size = 10),
        axis.title.y = element_text(size = 19, family = "Times New Roman"),
        axis.text.y = element_text(size = 19, family = "Times New Roman"),
        axis.text.x = element_text(size = 19, family = "Times New Roman"),
        axis.title.x = element_text(size = 19,family = "Times New Roman"),
        legend.text = element_text(size = 19, family = "Times New Roman"),
        plot.caption = element_text(hjust = 0, size = 11, family = "Times New Roman" ),
        panel.background = element_rect(fill = "white", colour = "white", color = "white"),
        plot.background = element_rect(fill = "white", colour = "white", color = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "#eceff2", size = 0.8),
        panel.grid.major.y = element_line(colour = "#eceff2", size = 0.8),
        legend.key = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", colour = "white"), 
        strip.text = element_text(size = 15, colour = "black", face = "bold", family = "Times New Roman"))

ggplot2::theme_set(tema)

# 2.0 Resultados ----

## 2.1 Graficos Reeleicao longitudinal 2000-2020

graf_1 <- grafico_reeleicao %>%
  ggplot(aes(x=as_factor(ano), y=Contagem,
             col = `Situação do Candidato`, 
             group = `Situação do Candidato`))+
  geom_point(size = 6.7, alpha = 0.9)+
  ggalt::geom_xspline(size = 2.8, alpha = 0.7)+
  labs(y = "Número de casos", 
       x= "",
       col = "")+
  scale_y_continuous(limits = c(900,2400), breaks = c(1200, 1500, 1800, 2100, 2400))+
  MetBrewer::scale_color_met_d(name = "Degas")

# Salvar o grafico 1

ggsave(graf_1, 
       filename = "graf_1.png", 
       width = 16, 
       height = 10)


## 2.2 Grafico Relacoes bivariadas entre o enfrentamento e a situacao politica dos prefeitos em 2020


(a <- banco_pandemia_reeleicao %>% 
  filter(`Δ% de médicos (2016-2020)`<= 750) %>% 
  ggbetweenstats(x = situacao_2020,
                 y = `Δ% de médicos (2016-2020)`,
                 ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
                 centrality.label.args = list(size = 3.5))+
  scale_x_discrete(limits = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
 MetBrewer::scale_color_met_d(name = "Degas")+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))
)

(b <- banco_pandemia_reeleicao %>%
  filter( `Despesas totais com saúde per capita (2020)` <= 4000) %>% 
  ggbetweenstats(
    x = situacao_2020,
    y =   `Despesas totais com saúde per capita (2020)`,
    ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
    centrality.label.args = list(size = 3.5)) + 
    scale_x_discrete(limits = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
  MetBrewer::scale_color_met_d(name = "Degas")+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))
)

(c <- ggbetweenstats(
  data = banco_pandemia_reeleicao,
  x = situacao_2020,
  y =  `Nº óbitos até outubro/10 mil hab.`,
  ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
  centrality.label.args = list(size = 3.5)) + 
    scale_x_discrete(limits = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
  MetBrewer::scale_color_met_d(name = "Degas")+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))
)

(d <- banco_pandemia_reeleicao %>%
  ggbetweenstats(
    x = situacao_2020,
    y =   `Valor recebido de auxílio per capita`,
    ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
    centrality.label.args = list(size = 3.5)) + 
    scale_x_discrete(limits = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
  MetBrewer::scale_color_met_d(name = "Degas")+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))
)

(g <-  banco_pandemia_reeleicao %>%
  ggbetweenstats(
    x = situacao_2020,
    y =    `Média restrição em locais de trabalho`,
    ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
    centrality.label.args = list(size = 3.5)) + 
    scale_x_discrete(limits = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
  MetBrewer::scale_color_met_d(name = "Degas")+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))
)

(h <-  banco_pandemia_reeleicao %>%
  filter( `Δ% auxiliar de enfermagem (2016-2020)` <= 500) %>% 
  ggbetweenstats(
    x = situacao_2020,
    y =    `Δ% auxiliar de enfermagem (2016-2020)`,
    ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
    centrality.label.args = list(size = 3.5)) + 
    scale_x_discrete(limits = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
  MetBrewer::scale_color_met_d(name = "Degas")+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))
)
##### Plotar todos juntos - a ordem das letras importa

figura_artigo <- (a + h)/(b + d)/(c + g)

ggsave(figura_artigo, width = 15, height = 22, filename = "figura_artigo_dados.png")

# 3.0 Analises multivariadas ----

## 3.1 Organizacao dos modelos

### Modelos logisticos ----


###### Modelo 1 ----

modelo_1 <- glm(`Reeleito 2020` ~ 
              
                  # Sociodemograficas
                
                  `Log da população (2019)`+
                  Nordeste +
                  Sudeste +  
                  Sul +
                  Norte + 
                  `Gini (2010)` + 
                  `PIB per capita (2017)` + 
                  Metropole + 
                  `Município polo em saúde` +
                  `Densidade demográfica` +
                  `Dependência financeira (2019)` +
                  
                  ## Políticas 
                  
                  NEP +
                  `Diferença entre Haddad e Bolsonaro` + 
                  #   `Diferença entre o primeiro e segundo colocado (2016)` +
                  `% candidato mais votado (2016)` +
                  
                  ## Individuais 
                  
                  PT +  
                  PSDB + 
                  PP +
                  PSD +
                  PSB +
                  PMDB +
                  PR +
                  DEM +
                  `Prefeita` +
                  `Médico(a)` +
                  `Casado(a)` +
                  `Ensino Superior` + 
                  `% despesa do candidato` +
                  
                  # Governo 
                  
                  `% de funcionários discricionários` +
                  `Funcionários per capita` +
                  `Despesas totais com saúde per capita (2020)` +
                  `Δ% em saúde per capita (2019-2020)` +
                  `Δ% de médicos (2016-2020)` +
                  `Δ% auxiliar de enfermagem (2016-2020)` +
                  `IDEB (2019)` +
                  `Δ% IDEB (2017-2019)` +
                  `Δ% educação fundamental infantil per capita (2016-2020)` +
                  `Despesas com educação infantil per capita (2020)` +
                  
                  # Conjunturais da Pandemia
                  
                  `Proporção de beneficiários` +
                  `Valor recebido de auxílio per capita` +
                  `Média restrição em locais de trabalho` +
                  `Nº óbitos até outubro/10 mil hab.` +
                  `Nº casos até outubro/10 mil hab.`, 
                
                family = binomial(link = "logit"), data = banco_pandemia_reeleicao)


banco_pandemia_reeleicao$pred_modelo_1 <- as.factor(
  ifelse(
    predict(modelo_1, 
            newdata = banco_pandemia_reeleicao, 
            type = "response")
    >0.5,"1","0"))


matriz_modelo_1 <- caret:: confusionMatrix(banco_pandemia_reeleicao$pred_modelo_1, banco_pandemia_reeleicao$`Reeleito 2020`, positive = "1")

check_model(modelo_1) 
check_collinearity(modelo_1)

# Obs: modelo 1 possui alto grau de colinearidade


######### Modelo 2 ----

modelo_2 <- glm(`Reeleito 2020` ~ 
                  
                  # Sociodemograficas
                  
                  `Log da população (2019)`+
                  Nordeste +
                  Sudeste +  
                  Sul +
                  Norte + 
                  `Gini (2010)` + 
                  `PIB per capita (2017)` + 
                  `Dependência financeira (2019)` +
                  
                  ## Políticas 
                  
                  NEP +
                  `Diferença entre Haddad e Bolsonaro` + 
                  # `Diferença entre o primeiro e segundo colocado (2016)` +
                  `% candidato mais votado (2016)` +
                  
                  ## Individuais 
                  
                  PT +  
                  PSDB + 
                  PP +
                  PSD +
                  PSB +
                  PMDB +
                  PR +
                  DEM +
                  `Prefeita` +
                  `% despesa do candidato` +
                  
                  # Governo 
                  
                  `Funcionários per capita` +
                  `Despesas totais com saúde per capita (2020)` +
                  `Δ% de médicos (2016-2020)` +
                  `Δ% auxiliar de enfermagem (2016-2020)` +
                  `IDEB (2019)` +
                  `Δ% IDEB (2017-2019)` +
                  
                  # Conjunturais da Pandemia
                  
                  `Proporção de beneficiários` +
                  `Valor recebido de auxílio per capita` +
                  `Nº óbitos até outubro/10 mil hab.` +
                  `Nº casos até outubro/10 mil hab.`, 
                
                family = binomial(link = "logit"), data = banco_pandemia_reeleicao)



#tab_model(modelo_2)

banco_pandemia_reeleicao$pred_modelo_2 <- as.factor(
  ifelse(
    predict(modelo_2, 
            newdata = banco_pandemia_reeleicao, 
            type = "response")
    >0.5,"1","0"))


matriz_modelo_2 <- caret:: confusionMatrix(banco_pandemia_reeleicao$pred_modelo_2,
                                           banco_pandemia_reeleicao$`Reeleito 2020`,
                                           positive = "1")

check_model(modelo_2)
check_collinearity(modelo_2)
# Obs: Tambem alto grau de colinearidade 


######### Modelo 3 ----

modelo_3 <- glm(`Reeleito 2020` ~ 
                  
                  # Sociodemograficas
                  
                  `Log da população (2019)`+
                  Nordeste +
                  Sudeste +
                  Sul +
                  Norte + 
                  `Gini (2010)` + 
                  `PIB per capita (2017)` + 
                  `Dependência financeira (2019)` +
                  
                  ## Políticas 
                  
                  NEP +
                  `Diferença entre Haddad e Bolsonaro` + 
                  #  `Diferença entre o primeiro e segundo colocado (2016)` +
                  `% candidato mais votado (2016)` +
                  
                  ## Individuais 
                  
                  PT +  
                  PSDB + 
                  PP +
                  PSD +
                  PSB +
                  PMDB +
                  PR +
                  DEM +
                  `Prefeita` +
                  `% despesa do candidato` +
                  
                  # Governo 
                  
                  `Funcionários per capita` +
                  `Despesas totais com saúde per capita (2020)` +
                  `Δ% de médicos (2016-2020)` +
                  
                  # Conjunturais da Pandemia
                  
                  `Proporção de beneficiários` +
                  `Valor recebido de auxílio per capita` +
                  `Nº óbitos até outubro/10 mil hab.` +
                  `Nº casos até outubro/10 mil hab.`, 
                
                family = binomial(link = "logit"), data = banco_pandemia_reeleicao)

tab_model(modelo_3)


banco_pandemia_reeleicao$pred_modelo_3 <- as.factor(
  ifelse(
    predict(modelo_3, 
            newdata = banco_pandemia_reeleicao, 
            type = "response")
    >0.5,"1","0"))


matriz_modelo_3 <- caret:: confusionMatrix(banco_pandemia_reeleicao$pred_modelo_3,
                                           banco_pandemia_reeleicao$`Reeleito 2020`,
                                           positive = "1")

check_model(modelo_3)
check_collinearity(modelo_3)



#### Modelo 4 ( Modelo refeito para acatar parecer da Revista Dados) ####


modelo_4.1 <- glm(`Reeleito 2020` ~ 
                    
                    # Sociodemograficas
                    
                    `Log da população (2019)`+
                    Nordeste +
                    Sudeste +
                    Sul +
                    Norte + 
                    `PIB per capita (2017)` + 
                    
                    ## Políticas 
                    
                    NEP +
                    `Diferença entre Haddad e Bolsonaro` + 
                    #  `Diferença entre o primeiro e segundo colocado (2016)` +
                    `% candidato mais votado (2016)` +
                    
                    ## Individuais 
                    
                    `Prefeita` +
                    `% despesa do candidato` +
                    # Governo 
                    #   `Despesas com educação infantil per capita (2020)`+
                    `Despesas totais com saúde per capita (2020)` +
                    #   `Despesas com educação infantil per capita (2020)` +
                    
                    # Conjunturais da Pandemia
                    
                    `Nº óbitos até outubro/10 mil hab.` +
                    # `Δ% em saúde per capita (2019-2020)` +
                    `Δ% de médicos (2016-2020)` ,
                  
                  family = binomial(link = "logit"), data = banco_pandemia_reeleicao)


banco_pandemia_reeleicao$pred_modelo_4.1 <- as.factor(
  ifelse(
    predict(modelo_4.1, 
            newdata = banco_pandemia_reeleicao, 
            type = "response")
    >0.5,"1","0"))


matriz_modelo_4 <- caret:: confusionMatrix(banco_pandemia_reeleicao$pred_modelo_4.1, 
                                           banco_pandemia_reeleicao$`Reeleito 2020`,
                                           positive = "1")

check_model(modelo_4.1)
check_collinearity(modelo_4.1)

tab_model(modelo_4.1)



## teste de pressuposto do modelo 4 anexo da dados 

checkmodel_modelo4_dados <- check_model(modelo_4.1)

## Para plotar os graficos acima separadamente 
performance::plot(check_normality(modelo_4.1))
performance::plot(posterior_predictive_check(modelo_4.1))
performance::plot(binned_residuals(modelo_4.1))
performance::plot(check_collinearity(modelo_4.1))
performance::plot(binned_residuals(modelo_4.1))



banco_pandemia_reeleicao$pred_modelo_4 <- as.factor(
  ifelse(
    predict(modelo_4.1, 
            newdata = banco_pandemia_reeleicao, 
            type = "response")
    >0.5,"1","0"))


matriz_modelo_4 <- caret:: confusionMatrix(banco_pandemia_reeleicao$pred_modelo_4.1, 
                                           banco_pandemia_reeleicao$`Reeleito 2020`,
                                           positive = "1")

check_model(modelo_4.1)
check_collinearity(modelo_4.1)

##### Graficos de probabilidades preditas pelo modelo 4.1 ----

plot_1 <-  sjPlot::plot_model(modelo_4.1, type = "pred", terms = c("NEP", "Prefeita"), 
                              colors =c("#999999", "#0088cc"))+
  labs(title = "NEP e Prefeita", 
       y = "", 
       col = "", x = "")+
  scale_color_manual(labels = c("Homem", "Mulher"), values = c("grey60", "#994614"))+
  scale_fill_manual(values = c("gray60", "#994614"))+
  scale_y_continuous(limits = c(0, 1))#
             

plot_2 <- sjPlot::plot_model(modelo_4.1, type = "pred", terms = c("Log da população (2019)", "Prefeita"), colors =c("#0088cc", "#999999")) +
  labs(title = "Log da População e Prefeita",
       y = "",
       col = "", x = "")+
  scale_color_manual(labels = c("Homem", "Mulher"), values = c("grey60", "#994614"))+
  scale_fill_manual(values = c("gray60", "#994614"))+
  scale_y_continuous(limits = c(0, 1))#


plot_3 <- sjPlot::plot_model(modelo_4.1, type = "pred", terms = c("% despesa do candidato", "Prefeita"), colors =c("#0088cc", "#999999")) +
  labs(title = "% Despesa de campanha e Prefeita",
       y = "",
       col = "", x = "")+
  scale_color_manual(labels = c("Homem", "Mulher"), values = c("grey60", "#994614"))+
  scale_fill_manual(values = c("gray60", "#994614"))+
  scale_y_continuous(limits = c(0, 1))#


plot_4 <- sjPlot::plot_model(modelo_4.1, type = "pred", terms = c("Despesas totais com saúde per capita (2020)", "Prefeita"), colors =c("#0088cc", "#999999")) +
  labs(title = "Gastos em Saúde e Prefeita",
       y = "",
       col = "", x = "")+
  scale_color_manual(labels = c("Homem", "Mulher"), values = c("grey60", "#994614"))+
  scale_fill_manual(values = c("gray60", "#994614"))+
  scale_y_continuous(limits = c(0, 1))#


plot_5 <- sjPlot::plot_model(modelo_4.1, type = "pred", terms = c("Δ% de médicos (2016-2020)", "Prefeita"), colors =c("#0088cc", "#999999")) +
  labs(title = "Contratação de médicos e Prefeita",
       y = "",
       col = "", x = "")+
  scale_color_manual(labels = c("Homem", "Mulher"), values = c("grey60", "#994614"))+
  scale_fill_manual(values = c("gray60", "#994614"))+
  scale_y_continuous(limits = c(0, 1))#


plot_6 <- sjPlot::plot_model(modelo_4.1, type = "pred", terms = c("Diferença entre Haddad e Bolsonaro", "Prefeita"), colors =c("#0088cc", "#999999")) +
  labs(title = "Diferença entre Haddad e Bolsonaro e Prefeita",
       y = "",
       col = "", x = "")+
  scale_color_manual(labels = c("Homem", "Mulher"), values = c("grey60", "#994614"))+
  scale_fill_manual(values = c("gray60", "#994614"))+
  scale_y_continuous(limits = c(0, 1))#

library(patchwork)

prob_preditas_artio_dados <- (plot_1 + plot_2)/(plot_3 + plot_6 )/(plot_5 + plot_4)

ggsave(prob_preditas_artio_dados, width = 17, height = 18, 
       filename = "prob_preditas_artigo_dados.png")


########## Modelo  5 ----


modelo_5 <- glm(`Reeleito 2020` ~ 
                  
                  # Sociodemograficas
                  
                  `Log da população (2019)`+
                  Nordeste +
                  Sudeste +
                  Sul +
                  Norte + 
                  `PIB per capita (2017)` + 
                  
                  ## Políticas 
                  
                  NEP +
                  `Diferença entre Haddad e Bolsonaro` + 
                  #  `Diferença entre o primeiro e segundo colocado (2016)` +
                  `% candidato mais votado (2016)` +
                  
                  ## Individuais 
                  
                  `Prefeita` +
                  
                  # Governo 
                  
                  `Despesas totais com saúde per capita (2020)` +
                  `Δ% de médicos (2016-2020)` +
                  
                  
                  # Conjunturais da Pandemia
                  
                  `Nº óbitos até outubro/10 mil hab.` ,
                
                family = binomial(link = "logit"), data = banco_pandemia_reeleicao)


tab_model(modelo_5)
check_collinearity(modelo_5)



## Tabela com razoes de chance dos cinco modelos 

tab_model(modelo_1, modelo_2,
          modelo_3, modelo_4.1,
          modelo_5, show.ci = FALSE,
          dv.labels = c("Modelo 1","Modelo 2","Modelo 3","Modelo 4","Modelo 5"))


## Salvar para anexo da revista dados

ggsave(checkmodel_modelo5_dados, height = 14, width = 14, 
       filename = "~/petro_rendas/trabalhos/covid_reeleicao_2020/resultados/checkmodel_modelo5_dados.png")

banco_pandemia_reeleicao$pred_modelo_5 <- as.factor(
  ifelse(
    predict(modelo_5, 
            newdata = banco_pandemia_reeleicao, 
            type = "response")
    >0.5,"1","0"))


matriz_modelo_5 <- caret:: confusionMatrix(banco_pandemia_reeleicao$pred_modelo_5,
                                           banco_pandemia_reeleicao$`Reeleito 2020`, positive = "1")



###### Cria variável desistencia ----


banco_pandemia_reeleicao <- banco_pandemia_reeleicao %>% 
mutate(tentativa =  case_when(sit_desc_sem_suplementar %in% c("N<e3>o Reeleito", "Reeleito")  ~ "1",
                              sit_desc_sem_suplementar == "N<e3>o tentou" ~ "0",
                              sit_desc_sem_suplementar %in% c("Anulada", "Impedido") ~ "NA"),
       tentativa_reeleicao = as_factor(ifelse(tentativa == 1, 1,
                                              ifelse(tentativa == 0, 0, NA))))




#  filter(!is.na(tentativa_reeleicao)) ##### ATENCAO PARA ESTE FILTRO RETIRA CASOS 

########## Modelo X: teste de vies de selecao para desistencias de reeleicao ----

modelo_x <- glm(tentativa_reeleicao ~
                  
                  # Sociodemograficas
                  
                  `Log da população (2019)` +
                  Nordeste +
                  Sudeste +
                  Sul +
                  Norte +
                  #   `Gini (2010)` +
                  `PIB per capita (2017)` +
                  Metropole +
                  `Município polo em saúde` +
                  `Densidade demográfica` +
                  `Dependência financeira (2019)` +
                  
                  ## Políticas
                  
                  NEP +
                  `Diferença entre Haddad e Bolsonaro` +
                  `Diferença entre o primeiro e segundo colocado (2016)` +
                  `% candidato mais votado (2016)` +
                  
                  ## Individuais
                  
                  PT +
                  PSDB +
                  PP +
                  PSD +
                  PSB +
                  PMDB +
                  PR +
                  DEM +
                  `Prefeita` +
                  `Médico(a)` +
                  #   `Casado(a)` +
                  #   `Ensino Superior` +
                  #    `% despesa do candidato` +
                  
                  # Governo
                  
                  `% de funcionários discricionários` +
                  `Funcionários per capita` +
                  `Despesas totais com saúde per capita (2020)` +
                  `Δ% em saúde per capita (2019-2020)` +
                  # `Δ% de médicos (2016-2020)` +
                  #    `Δ% auxiliar de enfermagem (2016-2019)` +
                  #    `IDEB (2019)` +
                  #  `Δ% IDEB (2017-2019)` +
                  #    `Δ% educação fundamental infantil per capita (2016-2020)` +
                  #   `Despesas com educação infantil per capita (2020)` +
                  
                  # Conjunturais da Pandemia
                  
                  #     `Proporção de beneficiários` +
                  #    `Valor recebido de auxílio per capita` +
                #    `Média restrição em locais de trabalho` +
                `Nº óbitos até outubro/10 mil hab.`,
                #   `Nº casos até outubro/10 mil hab.`,
                family = binomial(link = "logit"), data = banco_pandemia_reeleicao
)

sjPlot::tab_model(modelo_x)

sjPlot::plot_model(modelo_x)


banco_pandemia_reeleicao$pred_modelo_x <- as.factor(
  ifelse(
    predict(modelo_x,
            newdata = banco_pandemia_reeleicao,
            type = "response"
    )
    > 0.5, "1", "0"
  )
)



matriz_modelo_x <- caret:: confusionMatrix(banco_pandemia_reeleicao$pred_modelo_x,
                                           banco_pandemia_reeleicao$tentativa_reeleicao, positive = "1")


####### Modelos logisticos hierarquicos ----


######## Modelo 6 ----
library(lme4)

modelo_6 <- glmer(`Reeleito 2020` ~ 
                    
                    # Sociodemograficas
                    
                    `Log da população (2019)`+
                    #Nordeste +
                    #Sudeste +
                    #Sul +
                    #Norte + 
                    `PIB per capita (2017)` + 
                    
                    ## Políticas 
                    
                    NEP +
                    `Diferença entre Haddad e Bolsonaro` + 
                    #  `Diferença entre o primeiro e segundo colocado (2016)` +
                    `% candidato mais votado (2016)` +
                    
                    ## Individuais 
                    
                    `Prefeita` +
                    
                    # Governo 
                    
                    `Despesas totais com saúde per capita (2020)` +
                    `Δ% de médicos (2016-2020)`+
                    `Nº óbitos até outubro/10 mil hab.` +
                    (1|sigla_uf), 
                  
                  
                  # Conjunturais da Pandemia
                  
                  
                  
                  family = binomial(link = "logit"), data = banco_pandemia_reeleicao)


tab_model(modelo_6)

check_model(modelo_6)
#heck_collinearity(modelo_6)

plot_model(modelo_6, show.values = TRUE)






####### Modelo 7 ----
modelo_7 <- glmer(`Reeleito 2020` ~ 
                    
                    # Sociodemograficas
                    
                    `Log da população (2019)`+
                    #Nordeste +
                    #Sudeste +
                    #Sul +
                    #Norte + 
                    `PIB per capita (2017)` + 
                    
                    ## Políticas 
                    
                    NEP +
                    `Diferença entre Haddad e Bolsonaro` + 
                    #  `Diferença entre o primeiro e segundo colocado (2016)` +
                    `% candidato mais votado (2016)` +
                    
                    ## Individuais 
                    
                    `Prefeita` +
                    `% despesa do candidato`+
                    # Governo 
                    
                    `Despesas totais com saúde per capita (2020)` +
                    `Δ% de médicos (2016-2020)`+
                    
                    
                    
                    # Conjunturais da Pandemia
                    
                    `Nº óbitos até outubro/10 mil hab.` +
                  

                    (1|sigla_uf),
                  
                  family = binomial(link = "logit"), data = banco_pandemia_reeleicao)


## Os dois modelos hierarquicos que foram para o anexo do artigo por não trazerem ganhos substantivos

tab_model(modelo_6, modelo_7,
          dv.labels = c("Modelo 6", "Modelo 7"))

plot_models(modelo_6, modelo_7, show.values = TRUE)
  



########## Testes individuais dos modelos -----

banco_pandemia_reeleicao %>% 
  get_summary_stats(despesa_campanha) %>% 
  kbl()

check_model(modelo_1)

check_collinearity(modelo_1)

check_model(modelo_2)
check_collinearity(modelo_2)

check_model(modelo_3)
check_collinearity(modelo_3)

check_model(modelo_4)
check_collinearity(modelo_4)

check_model(modelo_2)
check_collinearity(modelo_4)

compare_performance(modelo_1, modelo_2, modelo_3, modelo_4, modelo_5)



#### Modelo para óbitos em resposta ao parecer n 3 ####




modelo_obitos <- lmer( `Nº óbitos até outubro/10 mil hab.` ~ 
                    
                    # Sociodemograficas
                    
                    `Log da população (2019)`+
                  #  Nordeste +
                   # Sudeste +
                    #Sul +
                  #  Norte + 
                    `PIB per capita (2017)` + 
                    
                    ## Políticas 
                    
               #     NEP +
                    `Diferença entre Haddad e Bolsonaro` + 
                   #  `Diferença entre o primeiro e segundo colocado (2016)` +
               #     `% candidato mais votado (2016)` +
                    
                    ## Individuais 
                    
                    `Prefeita` +
                 #   `% despesa do candidato` +
                    # Governo 
                  #  `Despesas com educação infantil per capita (2020)`+
                    `Despesas totais com saúde per capita (2020)` +
                    
                    # Conjunturais da Pandemia
                    
                   
                #    `Δ% em saúde per capita (2019-2020)` +
                    `Δ% de médicos (2016-2020)` +
                 `Δ% auxiliar de enfermagem (2016-2020)` +
                 `Nº casos até outubro/10 mil hab.`+
                 (1|sigla_uf),
          
                  
                   data = banco_pandemia_reeleicao)


tab_model(modelo_obitos)

plot_model(modelo_obitos, type = "pred", 
           terms = c("Despesas totais com saúde per capita (2020)", "Prefeita"))

