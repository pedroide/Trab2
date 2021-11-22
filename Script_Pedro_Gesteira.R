if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, stargazer, reshape, fixest, modelsummary)

###Questão 1----------------------------

##Pessoas Cadastradas

#Puxando a base

pcadastradas <- read.csv("C:/Users/Pedro/Google Drive/Mestrado/Microeconometria/Trabalho 2/Trabalho_2/PessoasCadastradas.csv",
                         sep = "\t")

#Vetor com as futuras colunas do dataframe

colunas <- unlist(str_split(pcadastradas[3,], pattern = ";",))

#Gerando as colunas do dataframe

pcadastradas <- as_tibble(pcadastradas[4:5493,]) %>% separate(col = 1,into = colunas, sep = ";")

pcadastradas <- separate(pcadastradas, col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")

#Criando os anos inexistentes 

anos_faltantes <- c(as.character(1993:1997))

pcadastradas[anos_faltantes] <- "0"

pcadastradas[pcadastradas == "-"] <- "0"

pcadastradas <- pcadastradas[,as.character(c("M_cod","M_name",1993:2012))]

#Adaptando para o formato de dados em painel

pcadastradas <- pivot_longer(pcadastradas, cols = as.character(c(1993:2012)), names_to = "Ano", values_to = "n_cadastrados")


##Pop Residente - Total e por faixa et?ria

#Lista para armazenamento das bases

lpops <- list()

#Vetor contendo o nome dos arquivos

arquivos.pop <- list.files(paste(getwd(),"/FE",sep = ""), pattern = "PopFE", full.names = TRUE, include.dirs = TRUE)

#Loop para a leitura dos arquivos e armazenamento dos mesmos na lista

for (i in seq_along(arquivos.pop)) {
  
  #Leitura da base
  
  Pop <- read.csv(arquivos.pop[i], sep = "\t")
  
  #Vetor com colunas da base
  
  cols.pop <- unlist(str_split(Pop[which(str_detect(Pop[,1], "^Município")),], pattern = ";"))
  
  cols.pop[-1] <- paste("Pop", cols.pop[-1], sep = "_")
  
  #vetor com o ano da base
  
  ano <- substr(arquivos.pop[i],85,88)
  
  #Corte para pegar apenas as informa??es necess?rias
  
  Pop <- as_tibble(select(Pop,1)) %>% slice((which(str_detect(Pop[,1], "^Município"))+1):(which(str_detect(Pop[,1], "^Total"))-1)) 
  
  #Gerando as colunas na base
  
  Pop <- separate(Pop, col = 1,into = cols.pop, sep = ";") %>% 
      separate(col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")
  
  #Adaptando para o formato de dados em painel
  
  Pop <- Pop %>% pivot_longer(3:ncol(Pop), names_to = "FE",values_to = ano, values_drop_na = FALSE)
  
  #Armazenando na lista
  
  lpops[[i]] <- Pop
  
}

##Gerando o dataframe com todos os anos

#Definindo o dataframe como a base de 1993

Pop <- lpops[[1]]

for (i in seq_along(lpops)) {
  
  #Condi??o necess?ria para parar o loop quando ele percorrer toda a lista
  
  if (i+1 > last(seq_along(lpops))) { 
    
    break }
  
  #Definindo o dataframe como a jun??o a esquerda da base de 93 com a base do ano seguinte
  
  Pop <- left_join(Pop,lpops[[i+1]])
  
}

#Adaptando para o formato de dados em painel

Pop <- Pop %>% pivot_longer(4:ncol(Pop), names_to = "Ano", values_to = "Valor", values_drop_na = FALSE) %>% 
  pivot_wider(names_from = "FE", values_from = "Valor") 

#Escolha de substituir dados inexistentes por 0 para manter o balan?o da base

Pop[Pop == "-"] <- "0"


##Mortalidade - Total e por faixa et?ria

#Lista para armazenamento das bases

lmorts <- list()

#Vetor contendo o nome dos arquivos

arquivos.mort <- list.files(paste(getwd(),"/Mortalidade",sep = ""), pattern = "Mort", full.names = TRUE, include.dirs = TRUE)

#Loop para a leitura dos arquivos e armazenamento dos mesmos na lista

for (i in seq_along(arquivos.mort)) {
  
  #Leitura da base
  
  Mort <- read.csv(arquivos.mort[1], sep = "\t")
  
  #Vetor com colunas da base
  
  cols.mort <- unlist(str_split(Mort[which(str_detect(Mort[,1], "^Município")),], pattern = ";"))
  
  cols.mort[-1] <- paste("Obitos", cols.mort[-1], sep = "_")
  
  #vetor com o ano da base
  
  ano <- substr(arquivos.mort[i],93,96)
  
  #Corte para pegar apenas as informa??es necess?rias
  
  Mort <- as.tibble(select(Mort,1)) %>% slice((which(str_detect(Mort[,1], "^Município"))+1):(which(str_detect(Mort[,1], "^Total"))-1)) 
  
  #Gerando as colunas na base
  
  Mort <- separate(Mort, col = 1,into = cols.mort, sep = ";") %>% 
    separate(col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")
  
  #Adaptando para o formato de dados em painel
  
  Mort <- Mort %>% pivot_longer(3:ncol(Mort), names_to = "FE",values_to = ano, values_drop_na = FALSE)
  
  #Armazenando na lista
  
  lmorts[[i]] <- Mort
  
}

##Gerando o dataframe com todos os anos

#Definindo o dataframe como a base de 1993

Mort <- lmorts[[1]]

for (i in seq_along(lmorts)) {
  
  #Condi??o necess?ria para parar o loop quando ele percorrer toda a lista
  
  if (i+1 > last(seq_along(lmorts))) { 
    
    break }
  
  #Definindo o dataframe como a jun??o a esquerda da base de 93 com a base do ano seguinte
  
  Mort <- left_join(Mort,lmorts[[i+1]])
  
}

#Adaptando para o formato de dados em painel

Mort <- Mort %>% pivot_longer(4:ncol(Mort), names_to = "Ano", values_to = "Valor", values_drop_na = FALSE) %>% 
  pivot_wider(names_from = "FE", values_from = "Valor") 

#Escolha de substituir dados inexistentes por 0 para manter o balan?o da base

Mort[Mort == "-"] <- "0"

##Unindo as tr?s bases

#Definindo o dataframe como a jun??o a esquerda da base de pop com a base de mort e pcadastradas

base <- left_join(left_join(Pop, Mort), pcadastradas)

#Transformando NAs em 0

base[is.na(base) == TRUE] <- "0"

#Convertendo os tipos das colunas

base <- type.convert(base)

#Agregando as faixas etárias para obter apenas as faixas pedidas
  
base <- base[,-c(6:13,17:24)] %>% 
  
  mutate(Pop_15a59 = rowSums(base[,6:10]),
         
         Obitos_15a59 = rowSums(base[,17:21]),
    
         Pop_maior60 = rowSums(base[,11:13]),
         
         Obitos_maior60 = rowSums(base[,22:24])
  ) %>% 
  
  #Realocando as faixas para facilitar a visualiza??o da base
  
  relocate(`Pop_maior60`, .before = `Pop_Total`) %>% 
  relocate(`Obitos_maior60`, .before = `Obitos_Total`) %>% 
  relocate(`Pop_15a59`, .before = `Pop_maior60`) %>% 
  relocate(`Obitos_15a59`, .before = `Obitos_maior60`)

#Transformando NAs em 0 (?, j? ta virando tique nervoso)

base[is.na(base) == TRUE] <- 0


##Alterando as colunas de total porque percebi que ela ? apenas a soma das faixas etárias que eu peguei, -------------------
##n?o a soma de todas dispon?veis

poptotal <- read.csv("C:/Users/Pedro/Google Drive/Mestrado/Microeconometria/Trabalho 2/Trabalho_2/PopTotal.csv",
                         sep = "\t")

#Vetor com as futuras colunas do dataframe

cols.poptotal <- unlist(str_split(poptotal[3,], pattern = ";",))

#Gerando as colunas do dataframe

poptotal <- as_tibble(poptotal[4:5569,]) %>% separate(col = 1,into = cols.poptotal, sep = ";")

poptotal <- separate(poptotal, col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")

#Adaptando para o formato de dados em painel

poptotal <- pivot_longer(poptotal, cols = as.character(c(1993:2012)), names_to = "Ano", values_to = "Total")

poptotal[poptotal == "-"] <- "0"

poptotal <- type.convert(poptotal)

poptotal[,4] <- as.integer(poptotal$Total)

#Substituindo na base

poptotal <- left_join(base[,1:3],poptotal)

base$Pop_Total <- poptotal$Total

#Mortalidade agora...

mort1total <- read.csv("C:/Users/Pedro/Google Drive/Mestrado/Microeconometria/Trabalho 2/Trabalho_2/MortTotal_93_95.csv",
                     sep = "\t")

mort2total <- read.csv("C:/Users/Pedro/Google Drive/Mestrado/Microeconometria/Trabalho 2/Trabalho_2/MortTotal_96_12.csv",
                       sep = "\t")

#Vetor com as futuras colunas do dataframe

cols.mort1total <- unlist(str_split(mort1total[3,], pattern = ";",))

cols.mort2total <- unlist(str_split(mort2total[3,], pattern = ";",))


#Gerando as colunas do dataframe

mort1total <- as_tibble(mort1total[4:4957,]) %>% separate(col = 1,into = cols.mort1total, sep = ";")

mort1total <- separate(mort1total, col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")

mort1total[mort1total == "-"] <- "0"

mort2total <- as_tibble(mort2total[4:5593,]) %>% separate(col = 1,into = cols.mort2total, sep = ";")

mort2total <- separate(mort2total, col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")

mort2total[mort2total == "-"] <- "0"

morttotal <- full_join(mort1total[,-ncol(mort1total)],mort2total[,-ncol(mort2total)])

#Adaptando para o formato de dados em painel

morttotal <- pivot_longer(morttotal, cols = as.character(c(1993:2012)), names_to = "Ano", values_to = "Total")

morttotal$Ano <- as.integer(morttotal$Ano)

morttotal$Total <- as.integer(morttotal$Total)

morttotal <- type.convert(morttotal)

#Substituindo na base

morttotal <- left_join(base[,1:3],morttotal)

base$Obitos_Total <- as.integer(morttotal$Total)

base[is.na(base) == TRUE] <- 0 

###Questão 2----------------------------

#Criando as vari?veis pedidas

tabmun <- readxl::read_xls(list.files(pattern = "RELATORIO")) %>% 
  select(c("UF","Nome_UF")) %>% 
  type.convert() %>% 
  distinct()

base <- base %>% mutate(txobitos_total = ifelse(`Pop_Total` == 0, 0, (`Obitos_Total` / `Pop_Total`)*100000),
                      txobitos_menor1 = (ifelse( `Pop_Menor 1 ano` == 0, 0, ( `Obitos_Menor 1 ano` / `Pop_Menor 1 ano` ) * 1000)),
                      txobitos_1a4 = (ifelse( `Pop_1 a 4 anos` == 0, 0, ( `Obitos_1 a 4 anos` / `Pop_1 a 4 anos` ) * 1000)),
                      txobitos_15a59 = (ifelse( `Pop_15a59` == 0, 0, ( `Obitos_15a59` / `Pop_15a59` ) * 100000)),
                      txobitos_maior60 = (ifelse( `Pop_maior60` == 0, 0, ( `Obitos_maior60` / `Pop_maior60` ) * 100000)),
  
                      cob = ( (`n_cadastrados` / `Pop_Total`)*100 ),
                      
                      psf = ifelse(base$n_cadastrados != 0, 1, 0),
                      
                      UF = as.numeric(substr(base$M_cod,1,2))
                      )

base <- left_join(base, tabmun, by = "UF") %>% 
  relocate(c("UF","Nome_UF"), .before = "Ano")

base <- base %>% mutate_at(c("M_cod","UF","Ano","psf"), as.factor)

###Questão 3-------------------------------------

faixas <- c("Total", "< 1 ano", "1 a 4 anos", "15 a 59 anos", "> 60 anos")

vind <- colnames(base[,17:21])

q3regs <- list()

for (i in seq_along(faixas)) {

  q3reg <- feols(as.formula(paste(vind[i],"~ cob | M_cod + Ano")),
               data = base,
               cluster = ~ M_cod)
  
  q3regs[[i]] <- q3reg
  
  names(q3regs)[i] <- faixas[i]
  
}

modelsummary(q3regs,
             title = "Faixas etárias por cobertura, utilizando efeitos fixos por município e ano",
             coef_rename = c("cob" = "Cobertura"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             estimate  = c("{estimate}{stars}"),
             gof_omit = "^(R2 Within|R2 Pseudo|Num|Std|FE|AIC|BIC|Log.Lik)",
             output = "latex_tabular",
             notes = c('teste', 'testezada'))



###Questão 4-------------------------------------

q4regs <- list()

for (i in seq_along(faixas)) {
  
  q4reg <- feols(as.formula(paste(vind[i],"~ cob + UF:Ano | M_cod + Ano")), 
              data = base,
               cluster = ~ M_cod)
  
  q4regs[[i]] <- q4reg
  
  names(q4regs)[i] <- faixas[i]
  
}

modelsummary(q4regs, 
             coef_omit = "^UF",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             estimate  = c("{estimate}{stars}"),
             gof_omit = "^(R2 Within|R2 Pseudo|Num|Std|FE|AIC|BIC|Log.Lik)",
             output = "latex_tabular")

###Questão 5-------------------------------------

gc()

q5_1regs <- list()

for (i in seq_along(faixas)) {
  
  q5_1reg <- feols(as.formula(paste(vind[i],"~ psf | M_cod + Ano")), 
                   data = base,
                   cluster = ~ M_cod)
  
  q5_1regs[[i]] <- q5_1reg
  
  names(q5_1regs)[i] <- faixas[i]
  
}

modelsummary(q5_1regs, 
             coef_omit = "^UF",
             estimate  = c("{estimate}{stars}"),
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_omit = "^(R2 Within|R2 Pseudo|Num|Std|FE|AIC|BIC|Log.Lik)",
             output = "latex_tabular",
             title = "teste",
             note = "teste")

gc()

q5_2regs <- list()

for (i in seq_along(faixas)) {
  
  q5_2reg <- feols(as.formula(paste(vind[i],"~ psf + UF:Ano | M_cod + Ano")), 
                 data = base,
                 cluster = ~ M_cod)
  
  q5_2regs[[i]] <- q5_2reg
  
  names(q5_2regs)[i] <- faixas[i]
  
}

modelsummary(q5_2regs, 
             coef_omit = "^UF",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             estimate  = c("{estimate}{stars}"),
             gof_omit = "^(R2 Within|R2 Pseudo|Num|Std|FE|AIC|BIC|Log.Lik)",
             output = "latex_tabular")

###Questão 6-------------------------------------

trat <- base[,c(1,5,23)] %>% 
  group_by(M_cod) %>% type.convert() %>% 
  summarise_at(vars(psf), funs(max(., na.rm = TRUE)))
  
names(trat) = c("M_cod", "tratamento")

trat$M_cod <- as.factor(trat$M_cod)

base <- left_join(base, trat, "M_cod")

q6_1reg1 <- feols(txobitos_total ~ i(Ano, tratamento, 1997) | M_cod + Ano, cluster = "M_cod", base)
iplot(q6_1reg1,
      value.lab = "Estimativa e Intervalo de Confiança (95%)",
      main = "Efeito na taxa de óbitos total")

q6_1reg2 <- feols(txobitos_menor1 ~ i(Ano, tratamento, 1997) | M_cod + Ano, cluster = "M_cod", base)
iplot(q6_1reg2,
      value.lab = "Estimativa e Intervalo de Confiança (95%)",
      main = "Efeito na taxa de óbitos < 1")

q6_1reg3 <- feols(txobitos_1a4 ~ i(Ano, tratamento, 1997) | M_cod + Ano, cluster = "M_cod", base)
iplot(q6_1reg3,
      value.lab = "Estimativa e Intervalo de Confiança (95%)",
      main = "Efeito na taxa de óbitos 1 a 4 anos")

q6_1reg4 <- feols(txobitos_15a59 ~ i(Ano, tratamento, 1997) | M_cod + Ano, cluster = "M_cod", base)
iplot(q6_1reg4,
      value.lab = "Estimativa e Intervalo de Confiança (95%)",
      main = "Efeito na taxa de óbitos 15 a 59 anos")  

q6_1reg5 <- feols(txobitos_maior60 ~ i(Ano, tratamento, 1997) | M_cod + Ano, cluster = "M_cod", base)
iplot(q6_1reg5,
      value.lab = "Estimativa e Intervalo de Confiança (95%)",
      main = "Efeito na taxa de óbitos > 60 anos")  

#Regress?es com o fator de intera??o

q6_2reg1 <- feols(txobitos_total ~ i(Ano, tratamento, 1997) + Ano:UF | M_cod + Ano, cluster = "M_cod", base)
iplot(q6_2reg1,
      value.lab = "Estimativa e Intervalo de Confiança (95%)",
      main = "Efeito na taxa de óbitos total (com termos de interação UF x Ano)")

q6_2reg2 <- feols(txobitos_menor1 ~ i(Ano, tratamento, 1997) + Ano:UF | M_cod + Ano, cluster = "M_cod", base)
iplot(q6_2reg2,
      value.lab = "Estimativa e Intervalo de Confiança (95%)",
      main = "Efeito na taxa de óbitos < 1 ano (com termos de interação UF x Ano)")

q6_2reg3 <- feols(txobitos_1a4 ~ i(Ano, tratamento, 1997) + Ano:UF | M_cod + Ano, cluster = "M_cod", base)
iplot(q6_2reg3,
      value.lab = "Estimativa e Intervalo de Confiança (95%)",
      main = "Efeito na taxa de óbitos 1 a 4 anos (com termos de interação UF x Ano)")

q6_2reg4 <- feols(txobitos_15a59 ~ i(Ano, tratamento, 1997) + Ano:UF | M_cod + Ano, cluster = "M_cod", base)
iplot(q6_2reg4,
      value.lab = "Estimativa e Intervalo de Confiança (95%)",
      main = "Efeito na taxa de óbitos 15 a 59 anos (com termos de interação UF x Ano)")

q6_2reg5 <- feols(txobitos_maior60 ~ i(Ano, tratamento, 1997) + Ano:UF | M_cod + Ano, cluster = "M_cod", base)
iplot(q6_2reg5,
      value.lab = "Estimativa e Intervalo de Confiança (95%)",
      main = "Efeito na taxa de óbitos > 60 anos (com termos de interação UF x Ano)")

