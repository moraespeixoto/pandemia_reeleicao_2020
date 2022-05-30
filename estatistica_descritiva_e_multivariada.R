##########################################################

# Estatisticas descritivas e analises multivariadas      #
                                                         
##########################################################

# 1.0 Abrir pacotes e bases ----

## 1.1 Pacotes utilizados

pacman::p_load(tidyverse, ggthemes, stats, patchwork, kableExtra, ggstatsplot, sjPlot)

## 1.2 Bases

#OBS: MUDAR OS CAMINHOS !!!!

load ("~/pandemia_reeleicao_2020/banco_pandemia_reeleicao.Rda")
load("~/petro_rendas/trabalhos/covid_reeleicao_2020/banco_de_dados/bancos_base/maquina_publica/bcovid_despesas_funcionalismo.Rda")
load( "~/petro_rendas/trabalhos/covid_reeleicao_2020/resultados/banco_prara_rodar_rmd/banco_bcovid_var_novas.Rda")
load("grafico_reeleicao.Rda")

banco_pandemia_reeleicao <- banco_bcovid_var_novas %>% 
  select ( sigla_uf,
           sit_20_sem_suplementar,
    `Reeleito 2020`,
`Log da população (2019)`,
  Nordeste ,
  Sudeste ,  
  Sul ,
  Norte , 
  `Gini (2010)` , 
  `PIB per capita (2017)` , 
  Metropole , 
  `Município polo em saúde` ,
  `Densidade demográfica` ,
  `Dependência financeira (2019)` ,
    NEP ,
  `Diferença entre Haddad e Bolsonaro` , 
  `Diferença entre o primeiro e segundo colocado (2016)` ,
  `% candidato mais votado (2016)` ,
  PT ,  
  PSDB , 
  PP ,
  PSD ,
  PSB ,
  PMDB ,
  PR ,
  DEM ,
  `Prefeita` ,
  `Médico(a)` ,
  `Casado(a)` ,
  `Ensino Superior` , 
  `% despesa por candidato` ,
  `% de funcionários discricionários` ,
  `Funcionários per capita` ,
  `Despesas totais com saúde per capita (2020)` ,
  `Δ% em saúde per capita (2019-2020)` ,
  `Δ% de médicos (2016-2020)` ,
  `Δ% auxiliar de enfermagem (2016-2020)` ,
  `IDEB (2019)` ,
  `Δ% IDEB (2017-2019)` ,
  `Δ% educação fundamental infantil per capita (2016-2020)` ,
  `Despesas com educação infantil per capita (2020)` ,
  `Proporção de beneficiários` ,
  `Valor recebido de auxílio per capita` ,
  `Média restrição em locais de trabalho` ,
  `Nº óbitos até outubro/10 mil hab.` ,
  `Nº casos até outubro/10 mil hab.`)

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
       height = 10,
       path = "~/petro_rendas/trabalhos/covid_reeleicao_2020/resultados/")


## 2.2 Grafico Relacoes bivariadas entre o enfrentamento e a situacao politica dos prefeitos em 2020


a <-  banco_bcovid_var_novas %>% 
  filter(`Δ% de médicos (2016-2020)`<= 750) %>% 
  ggbetweenstats(x = sit_desc_sem_suplementar,
                 y = `Δ% de médicos (2016-2020)`,
                 ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
                 centrality.label.args = list(size = 3.5))+
  scale_x_discrete(labels = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
  MetBrewer::scale_color_met_d(name = "Degas")+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))


b <- banco_bcovid_var_novas %>%
  filter( `Despesas totais com saúde per capita (2020)` <= 4000) %>% 
  ggbetweenstats(
    x = sit_desc_sem_suplementar,
    y =   `Despesas totais com saúde per capita (2020)`,
    ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
    centrality.label.args = list(size = 3.5)) + 
  scale_x_discrete(labels = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
  MetBrewer::scale_color_met_d(name = "Degas")+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))

c <- ggbetweenstats(
  data = banco_bcovid_var_novas,
  x = sit_desc_sem_suplementar,
  y =  `Nº óbitos até outubro/10 mil hab.`,
  ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
  centrality.label.args = list(size = 3.5)) + 
  scale_x_discrete(labels = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
  MetBrewer::scale_color_met_d(name = "Degas")+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))

d <- banco_bcovid_var_novas %>%
  ggbetweenstats(
    x = sit_desc_sem_suplementar,
    y =   `Valor recebido de auxílio per capita`,
    ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
    centrality.label.args = list(size = 3.5)) + 
  scale_x_discrete(labels = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
  MetBrewer::scale_color_met_d(name = "Degas")+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))

g <-  banco_bcovid_var_novas %>%
  ggbetweenstats(
    x = sit_desc_sem_suplementar,
    y =    `Média restrição em locais de trabalho`,
    ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
    centrality.label.args = list(size = 3.5)) + 
  scale_x_discrete(labels = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
  MetBrewer::scale_color_met_d(name = "Degas")+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))

h <-  banco_bcovid_var_novas %>%
  filter( `Δ% auxiliar de enfermagem (2016-2020)` <= 500) %>% 
  ggbetweenstats(
    x = sit_desc_sem_suplementar,
    y =    `Δ% auxiliar de enfermagem (2016-2020)`,
    ggsignif.args = list(textsize = 3.5, tip_length = 0.01),
    centrality.label.args = list(size = 3.5)) + 
  MetBrewer::scale_color_met_d(name = "Degas")+
  scale_x_discrete(labels = c("Impedido", "Não tentou", "Não reeleito", "Reeleito"))+
  labs(x = "", caption = "")+
  theme(axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size =14),
        plot.subtitle = element_text(size = 14))

##### Plotar todos juntos - a ordem das letras importa

figura_artigo <- (a + h)/(b + d)/(c + g)

ggsave(figura_artigo, width = 15, height = 22, filename = "figura_aritgo_dados.png")

# 3.0 Analises multivariadas ----

## 3.1 Organizacao dos modelos

######### Renomear as variáveis que vão entrar no modelo sem sobreescrever o banco ####

banco_bcovid_var_novas <- bcovid_despesas_funcionalismo %>% 
  rename(`Reeleito 2020` = reeleito_20_dummy_sem_suplementar,
         `Log da população (2019)` = log_pop_2019, 
         Nordeste = nordeste, 
         Sudeste = sudeste, 
         Sul = sul,
         Norte = norte, 
         `Gini (2010)` = gini_2010, 
         `PIB per capita (2017)` = pib_per_2017, 
         Metropole = metropole, 
         `Município polo em saúde` = mun_polo,
         `Densidade demográfica` = densidade,
         `Dependência financeira (2019)` = pp_trans_2019,
         NEP = nep, 
         `Diferença entre Haddad e Bolsonaro`  = diferenca_bozo_haddad, 
         `Diferença entre o primeiro e segundo colocado (2016)` = dif_1_2_colocado_2016,
         `% candidato mais votado (2016)` = perc_cand_mais_votado_2016,
         PT = pt,  
         PSDB = psdb, 
         PP = pp,
         PSD = psd,
         PSB = psb,
         PMDB = pmdb,
         PR = pr,
         DEM = dem,
         `Prefeita` = mulher,
         `Médico(a)` = medico,
         `Casado(a)` = casado,
         #  Branco(a) = branco, # não tem no banco
         `Ensino Superior` = ens_superior, 
         `% despesa por candidato` =  perc_despesa_candidato,
         `% de funcionários discricionários` = perc_func_discricionarios,
         `Funcionários per capita` = func_percapita,
         `Despesas totais com saúde per capita (2020)` =  desp_total_saude_per_capita_defla_2020,
         `Δ% em saúde per capita (2019-2020)` = variacao_perc_saude_per_capita_2019_2020,
         `Δ% de médicos (2016-2020)` = vari_medico_2016_2020,
         `Δ% auxiliar de enfermagem (2016-2020)` = vari_tec_aux_enfermagem_2016_2020,
         `IDEB (2019)` = ideb_2019,
         `Δ% IDEB (2017-2019)` = variacao_perc_ideb_2017_2019,
         `Δ% educação fundamental infantil per capita (2016-2020)` = variacao_perc_edu_fundamental_infantil_per_capita_2016_2020,
         `Despesas com educação infantil per capita (2020)` = despesas_edu_fundamental_infantil_per_capita_defla_2020,
         `Proporção de beneficiários` = beneficiados_per_capita,
         `Valor recebido de auxílio per capita` = aux_per_capita,
         `Média restrição em locais de trabalho` = md_local_trabalho,
         `Nº óbitos até outubro/10 mil hab.` =  obitos_acum_out_10hab,
         `Nº casos até outubro/10 mil hab.` = casos_acum_out_10hab) %>% 
  mutate(`Reeleito 2020` = as_factor(as.numeric(`Reeleito 2020`)))

## Modelos logisticos 

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
                  `% despesa por candidato` +
                  
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
                
                family = binomial(link = "logit"), data = banco_bcovid_var_novas)


banco_bcovid_var_novas$pred_modelo_1 <- as.factor(
  ifelse(
    predict(modelo_1, 
            newdata = banco_bcovid_var_novas, 
            type = "response")
    >0.5,"1","0"))


matriz_modelo_1 <- caret:: confusionMatrix(banco_bcovid_var_novas$pred_modelo_1, banco_bcovid_var_novas$`Modelo 1`, positive = "1")


#check_model(modelo_1)


#########

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
                  `% despesa por candidato` +
                  
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
                
                family = binomial(link = "logit"), data = banco_bcovid_var_novas)



#tab_model(modelo_2)

banco_bcovid_var_novas$pred_modelo_2 <- as.factor(
  ifelse(
    predict(modelo_2, 
            newdata = banco_bcovid_var_novas, 
            type = "response")
    >0.5,"1","0"))


matriz_modelo_2 <- caret:: confusionMatrix(banco_bcovid_var_novas$pred_modelo_2, banco_bcovid_var_novas$`Modelo 2`, positive = "1")

#check_model(modelo_2)
#check_collinearity(modelo_2)

############################

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
                  `% despesa por candidato` +
                  
                  # Governo 
                  
                  `Funcionários per capita` +
                  `Despesas totais com saúde per capita (2020)` +
                  `Δ% de médicos (2016-2020)` +
                  
                  # Conjunturais da Pandemia
                  
                  `Proporção de beneficiários` +
                  `Valor recebido de auxílio per capita` +
                  `Nº óbitos até outubro/10 mil hab.` +
                  `Nº casos até outubro/10 mil hab.`, 
                
                family = binomial(link = "logit"), data = banco_bcovid_var_novas)

#tab_model(modelo_3)


banco_bcovid_var_novas$pred_modelo_3 <- as.factor(
  ifelse(
    predict(modelo_3, 
            newdata = banco_bcovid_var_novas, 
            type = "response")
    >0.5,"1","0"))


matriz_modelo_3 <- caret:: confusionMatrix(banco_bcovid_var_novas$pred_modelo_3, banco_bcovid_var_novas$`Modelo 3`, positive = "1")

#check_model(modelo_3)
#check_collinearity(modelo_3)

##########################

modelo_4 <- glm(`Reeleito 2020` ~ 
                  
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
                  #    `Diferença entre o primeiro e segundo colocado (2016)` +
                  `% candidato mais votado (2016)` +
                  
                  ## Individuais 
                  
                  PT +  
                  PP +
                  PSD +
                  PR +
                  DEM +
                  `Prefeita` +
                  `% despesa por candidato` +
                  
                  # Governo 
                  
                  `Despesas totais com saúde per capita (2020)` +
                  
                  # Conjunturais da Pandemia
                  
                  `Valor recebido de auxílio per capita` +
                  `Nº óbitos até outubro/10 mil hab.` ,
                
                family = binomial(link = "logit"), data = banco_bcovid_var_novas)

tab_model(modelo_4)


## teste de pressuposto do modelo 4 anexo da dados 

checkmodel_modelo4_dados <- check_model(modelo_4)

banco_bcovid_var_novas$pred_modelo_4 <- as.factor(
  ifelse(
    predict(modelo_4, 
            newdata = banco_bcovid_var_novas, 
            type = "response")
    >0.5,"1","0"))


matriz_modelo_4 <- caret:: confusionMatrix(banco_bcovid_var_novas$pred_modelo_4, banco_bcovid_var_novas$`Modelo 4`, positive = "1")

check_model(modelo_4)
check_collinearity(modelo_4)


plot_1 <-  sjPlot::plot_model(modelo_4, type = "pred", terms = c("NEP", "Prefeita"), colors =c("#999999", "#0088cc"))+
  labs(title = "NEP e Prefeita", 
       y = "", 
       col = "", x = "")+
  scale_color_manual(labels = c("Homem", "Mulher"), values = c("grey60", "#994614"))+
  scale_fill_manual(values = c("gray60", "#994614"))


plot_2 <- sjPlot::plot_model(modelo_4, type = "pred", terms = c("Log da população (2019)", "Prefeita"), colors =c("#0088cc", "#999999")) +
  labs(title = "Log da População e Prefeita",
       y = "",
       col = "", x = "")+
  scale_color_manual(labels = c("Homem", "Mulher"), values = c("grey60", "#994614"))+
  scale_fill_manual(values = c("gray60", "#994614"))


plot_3 <- sjPlot::plot_model(modelo_4, type = "pred", terms = c("% despesa por candidato", "Prefeita"), colors =c("#0088cc", "#999999")) +
  labs(title = "% Despesa de campanha e Prefeita",
       y = "",
       col = "", x = "")+
  scale_color_manual(labels = c("Homem", "Mulher"), values = c("grey60", "#994614"))+
  scale_fill_manual(values = c("gray60", "#994614"))


plot_4 <- sjPlot::plot_model(modelo_4, type = "pred", terms = c("Despesas totais com saúde per capita (2020)", "Prefeita"), colors =c("#0088cc", "#999999")) +
  labs(title = "Gastos em Saúde e Prefeita",
       y = "",
       col = "", x = "")+
  scale_color_manual(labels = c("Homem", "Mulher"), values = c("grey60", "#994614"))+
  scale_fill_manual(values = c("gray60", "#994614"))

library(patchwork)

prob_preditas_artio_dados <- (plot_1 + plot_2)/(plot_3 + plot_4 )

ggsave(prob_preditas_artio_dados, width = 17, height = 18, 
       filename = "~/petro_rendas/trabalhos/covid_reeleicao_2020/resultados/prob_preditas_artigo_dados.png")

########

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
                  
                  # Conjunturais da Pandemia
                  
                  `Nº óbitos até outubro/10 mil hab.` ,
                
                family = binomial(link = "logit"), data = banco_bcovid_var_novas)


tab_model(modelo_5)
check_collinearity(modelo_5)



## Salvar para anexo da revista dados

ggsave(checkmodel_modelo5_dados, height = 14, width = 14, 
       filename = "~/petro_rendas/trabalhos/covid_reeleicao_2020/resultados/checkmodel_modelo5_dados.png")

banco_bcovid_var_novas$pred_modelo_5 <- as.factor(
  ifelse(
    predict(modelo_5, 
            newdata = banco_bcovid_var_novas, 
            type = "response")
    >0.5,"1","0"))


matriz_modelo_5 <- caret:: confusionMatrix(banco_bcovid_var_novas$pred_modelo_5, banco_bcovid_var_novas$`Modelo 5`, positive = "1")

##########

banco_bcovid_var_novas$sit_desc_anulado
glimpse(banco_bcovid_var_novas)
glimpse(banco_pandemia_reeleicao

        table(banco_bcovid_var_novas$sit_20_sem_suplementar,banco_bcovid_var_novas$sit_desc_anulado)
        table(
        
banco_bcovid_var_novas <- banco_bcovid_var_novas %>% 
  mutate(tentativa =  case_when(sit_desc_anulado %in% c("N<e3>o Reeleito", "Reeleito")  ~ "1",
                                sit_desc_anulado == "N<e3>o tentou" ~ "0",
                                sit_desc_anulado %in% c("Anulada", "Impedido") ~ "NA"),
         tentativa_reeleicao = as_factor(ifelse(tentativa == 1, 1,
                                                ifelse(tentativa == 0, 0, NA)))) %>% 
  
  filter(!is.na(tentativa_reeleicao)) ##### ATENCAO PARA ESTE FILTRO RETIRA CASOS 



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
                  #    `% despesa por candidato` +
                  
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
                family = binomial(link = "logit"), data = banco_bcovid_var_novas
)

sjPlot::tab_model(modelo_x)

sjPlot::plot_model(modelo_x)


banco_bcovid_var_novas$pred_modelo_x <- as.factor(
  ifelse(
    predict(modelo_x,
            newdata = banco_bcovid_var_novas,
            type = "response"
    )
    > 0.5, "1", "0"
  )
)



matriz_modelo_x <- caret:: confusionMatrix(banco_bcovid_var_novas$pred_modelo_x, banco_bcovid_var_novas$tentativa_reeleicao, positive = "1")


#############################

## Testes individuais dos modelos

rstatix:: get_summary_stats(banco_bcovid_var_novas$`% despesa por candidato`)

check_model(modelo_1)

check_collinearity(modelo_1)

check_model(modelo_5)

check_model(modelo_2)
check_collinearity(modelo_2)

check_model(modelo_3)
check_collinearity(modelo_3)

check_model(modelo_4)
check_collinearity(modelo_4)

check_model(modelo_2)
check_collinearity(modelo_4)

compare_performance(modelo_1, modelo_2, modelo_3, modelo_4, modelo_5)
