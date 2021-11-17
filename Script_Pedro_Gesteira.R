library(tidyverse)
library(haven)
library(stargazer)
library(reshape)
library(plm)

###Questão 1----------------------------

##Pessoas Cadastradas

pcadastradas <- read.csv("C:/Users/Pedro/Google Drive/Mestrado/Microeconometria/Trabalho 2/Trabalho_2/PessoasCadastradas.csv",
                         sep = "\t")

colunas <- unlist(str_split(pcadastradas[3,], pattern = ";",))

pcadastradas <- as_tibble(pcadastradas[4:5493,]) %>% separate(col = 1,into = colunas, sep = ";")

pcadastradas <- separate(pcadastradas, col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")

anos_faltantes <- c(as.character(1993:1997))

pcadastradas[anos_faltantes] <- "0"

pcadastradas[pcadastradas == "-"] <- "0"

pcadastradas <- pcadastradas[,as.character(c("M_cod","M_name",1993:2012))]

pcadastradas <- pivot_longer(pcadastradas, cols = as.character(c(1993:2012)), names_to = "Ano", values_to = "n_cadastrados")

##Pop Residente - Total e por faixa etária

lpops <- list()

arquivos.pop <- list.files(paste(getwd(),"/FE",sep = ""), pattern = "PopFE", full.names = TRUE, include.dirs = TRUE)

for (i in seq_along(arquivos.pop)) {
  
  Pop <- read.csv(arquivos.pop[i], sep = "\t")
  
  cols.pop <- unlist(str_split(Pop[which(str_detect(Pop[,1], "^Município")),], pattern = ";"))
  
  cols.pop[-1] <- paste("Pop", cols.pop[-1], sep = "_")
  
  ano <- substr(arquivos.pop[i],85,88)
  
  Pop <- as.tibble(select(Pop,1)) %>% slice((which(str_detect(Pop[,1], "^Município"))+1):(which(str_detect(Pop[,1], "^Total"))-1)) 
  
  Pop <- separate(Pop, col = 1,into = cols.pop, sep = ";") %>% 
    separate(col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")
  
  Pop <- Pop %>% pivot_longer(3:ncol(Pop), names_to = "FE",values_to = ano, values_drop_na = FALSE)
  
  lpops[[i]] <- Pop
  


}

Pop <- lpops[[1]]

for (i in seq_along(lpops)) {
  
  if (i+1 > last(seq_along(lpops))) { 
    
    break }
  
  Pop <- left_join(Pop,lpops[[i+1]])
  
}

Pop <- Pop %>% pivot_longer(4:ncol(Pop), names_to = "Ano", values_to = "Valor", values_drop_na = FALSE) %>% 
  pivot_wider(names_from = "FE", values_from = "Valor") 

Pop[Pop == "-"] <- "0"

#colnames(Pop)[4:14] <- paste("Pop",colnames(Pop[4:14]), sep ="_") #Não vou precisar graças a alteração na linha 30

##Mortalidade - Total e por faixa etária

lmorts <- list()

arquivos.mort <- list.files(paste(getwd(),"/Mortalidade",sep = ""), pattern = "Mort", full.names = TRUE, include.dirs = TRUE)

for (i in seq_along(arquivos.mort)) {
  
  Mort <- read.csv(arquivos.mort[1], sep = "\t")
  
  cols.mort <- unlist(str_split(Mort[which(str_detect(Mort[,1], "^Município")),], pattern = ";"))
  
  cols.mort[-1] <- paste("Obitos", cols.mort[-1], sep = "_")
  
  ano <- substr(arquivos.mort[i],93,96)
  
  Mort <- as.tibble(select(Mort,1)) %>% slice((which(str_detect(Mort[,1], "^Município"))+1):(which(str_detect(Mort[,1], "^Total"))-1)) 
  
  Mort <- separate(Mort, col = 1,into = cols.mort, sep = ";") %>% 
    separate(col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")
  
  Mort <- Mort %>% pivot_longer(3:ncol(Mort), names_to = "FE",values_to = ano, values_drop_na = FALSE)
  
  lmorts[[i]] <- Mort
  
}

Mort <- lmorts[[1]]

for (i in seq_along(lmorts)) {
  
  if (i+1 > last(seq_along(lmorts))) { 
    
    break }
  
  Mort <- left_join(Mort,lmorts[[i+1]])
  
}

Mort <- Mort %>% pivot_longer(4:ncol(Mort), names_to = "Ano", values_to = "Valor", values_drop_na = FALSE) %>% 
  pivot_wider(names_from = "FE", values_from = "Valor") 

Mort[Mort == "-"] <- "0"

##Unindo as três bases

base <- left_join(left_join(Pop, Mort), pcadastradas)

base[is.na(base) == TRUE] <- "0"

base <- type.convert(base)
  
base <- base[,-c(11:13,22:24)] %>% 
  
  mutate(Pop_maior60 = rowSums(base[,11:13]),
         
         Obitos_maior60 = rowSums(base[,22:24])
  ) %>% 
  
  relocate(`Pop_maior60`, .before = `Pop_Total`) %>% 
  relocate(`Obitos_maior60`, .before = `Obitos_Total`)

base[is.na(base) == TRUE] <- 0

###Questão 2----------------------------

#Criando as variáveis pedidas

tabmun <- readxl::read_xls(list.files(pattern = "RELATORIO")) %>% 
  select(c("UF","Nome_UF")) %>% 
  type.convert() %>% 
  distinct()

base <- base %>% mutate(txobitos_total = ifelse(`Pop_Total` == 0, 0, (`Obitos_Total` / `Pop_Total`)*100000),
                    txobitos_menor1 = (ifelse( `Pop_Menor 1 ano` == 0, 0, ( `Obitos_Menor 1 ano` / `Pop_Menor 1 ano` ) * 1000)),
                    txobitos_1a4 = (ifelse( `Pop_1 a 4 anos` == 0, 0, ( `Obitos_1 a 4 anos` / `Pop_1 a 4 anos` ) * 1000)),
                    txobitos_15a19 = (ifelse( `Pop_15 a 19 anos` == 0, 0, ( `Obitos_15 a 19 anos` / `Pop_15 a 19 anos` ) * 100000)),
                    txobitos_20a29 = (ifelse( `Pop_20 a 29 anos` == 0, 0, ( `Obitos_20 a 29 anos` / `Pop_20 a 29 anos` ) * 100000)),
                    txobitos_30a39 = (ifelse( `Pop_30 a 39 anos` == 0, 0, ( `Obitos_30 a 39 anos` / `Pop_30 a 39 anos` ) * 100000)),
                    txobitos_40a49 = (ifelse( `Pop_40 a 49 anos` == 0, 0, ( `Obitos_40 a 49 anos` / `Pop_40 a 49 anos` ) * 100000)),
                    txobitos_50a59 = (ifelse( `Pop_50 a 59 anos` == 0, 0, ( `Obitos_50 a 59 anos` / `Pop_50 a 59 anos` ) * 100000)),
                    txobitos_maior60 = (ifelse( `Pop_maior60` == 0, 0, ( `Obitos_maior60` / `Pop_maior60` ) * 100000)),

                    cob = ( `n_cadastrados` / `Pop_Total` ),
                    
                    psf = ifelse(base$n_cadastrados != 0, 1, 0),
                    
                    UF = as.numeric(substr(base$M_cod,1,2))
                    )

base <- left_join(base, tabmun, by = "UF") %>% 
  relocate(c("UF","Nome_UF"), .before = "Ano")



###Questão 3-------------------------------------

faixas <- substring(names(base[,6:14]),5)
faixas <- c(faixas[9],faixas[-9])

vind <- colnames(base[,25:33])

q3regs <- list()

for (i in seq_along(faixas)) {

  q3reg <- plm(paste(vind[i],"~ cob"), data = base,
               index = c("M_cod","Ano"),
               model = "within",
               effect = "twoways"
               )
  
  q3regs[[i]] <- q3reg
  
  names(q3regs)[i] <- faixas[i]
    
}

for (i in seq_along(faixas)) {
  
  q3reg <- plm(paste(vind[i],"~ cob"), data = base,
               index = c("M_cod","Ano"),
               model = "within",
               effect = "twoways",
               na.rm = TRUE)
  
  q3regs[[i]] <- q3reg
  
  names(q3regs)[i] <- faixas[i]
  
}

#ENTENDER ESSE NEGOCIO DE TWO WAYS
 

###Questão 4-------------------------------------



###Questão 5-------------------------------------



###Questão 6-------------------------------------

