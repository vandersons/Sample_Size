library(ribge)
library(samplesize4surveys)
library(clipr)
#Baixa população estimada para 2021 de cada município
pop2021 <- populacao_municipios(2021)
#Captura os dados da tabela do Anderson a partir do clipboard
dados_pos <- read_clip_tbl()
#Seleciona apenas a prevalência da última semana e elimina as UF que não tem dado
dados_pos <- dados_pos %>% 
  rename(uf = DS_UF_SIGLA) %>% 
  select(uf, X7.30.2022) %>% 
  filter(!is.na(X7.30.2022)) %>% 
  rename(perc = X7.30.2022)
#Agrega população por UF
pop_uf <- pop2021 %>% 
  group_by(uf) %>% 
  summarise(pop = sum(populacao)) %>% 
  mutate(pop_sem = pop/52)
pop_uf$pop_sem <- round(pop_uf$pop_sem, digits = 0)
#Junta as informações de população e positividade e ajusta os valores para decimais
data_pop_prev <-inner_join(pop_uf, dados_pos, by="uf")
data_pop_prev$perc <- gsub("\\%", "", data_pop_prev$perc)
data_pop_prev$perc <- gsub("\\,", "\\.", data_pop_prev$perc)
data_pop_prev$perc <- as.numeric(data_pop_prev$perc)
data_pop_prev$perc <- data_pop_prev$perc / 100
#Cálculo do tamanho amostral para cada UF considerando um teste de hipótese de uma proporção
data_pop_prev <- data_pop_prev %>% 
  mutate(ss = ss4p(pop_sem, perc, conf=0.85, delta = 0.2, plot=FALSE))
data_pop_prev
#Salva o dataset para a posteridade
writexl::write_xlsx(data_pop_prev, "/Users/987352344/Documents/sample_size_uf.xlsx")
