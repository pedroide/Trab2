library(tidyverse)
library(haven)
library(stargazer)
library(reshape)

###Questão 1

##Pessoas Cadastradas

pCadastradas <- read.csv("C:/Users/Pedro/Google Drive/Mestrado/Microeconometria/Trabalho 2/Trabalho_2/PessoasCadastradas.csv",
                         sep = "\t")

colunas <- unlist(str_split(pCadastradas[3,], pattern = ";",))

pCadastradas <- as_tibble(pCadastradas[4:5493,]) %>% separate(col = 1,into = colunas, sep = ";") %>% 
  separate(pCadastradas, col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")

##Pop Residente - Total e por faixa etária

lpops <- list()

arquivos.pop <- list.files(paste(getwd(),"/FE",sep = ""), pattern = "PopFE", full.names = TRUE, include.dirs = TRUE)

for (i in seq_along(list.files(paste(getwd(),"/FE",sep = ""), pattern = "PopFE", full.names = TRUE, include.dirs = TRUE))) {

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
  Pop <- left_join(Pop,lpops[[i+1]], by = c("M_cod", "M_name"))
}

Pop <- Pop %>% pivot_longer(4:ncol(Pop), names_to = "Ano", values_to = "Valor", values_drop_na = FALSE) %>% 
  pivot_wider(names_from = "FE", values_from = "Valor") 

#colnames(Pop)[4:14] <- paste("Pop",colnames(Pop[4:14]), sep ="_") #Não vou precisar graças a alteração na linha 30

##Mortalidade - Total e por faixa etária

lmorts <- list()

arquivos.mort <- list.files(paste(getwd(),"/Mortalidade",sep = ""), pattern = "Mort", full.names = TRUE, include.dirs = TRUE)

for (i in seq_along(list.files(paste(getwd(),"/Mortalidade",sep = ""), pattern = "Mort", full.names = TRUE, include.dirs = TRUE))) {
  
  Mort <- read.csv(arquivos.mort[i], sep = "\t")
  
  cols <- unlist(str_split(Mort[which(str_detect(Mort[,1], "^Município")),], pattern = ";"))
  
  ano <- substr(arquivos.mort[i],93,96)
  
  Mort <- as.tibble(select(Mort,1)) %>% slice((which(str_detect(Mort[,1], "^Município"))+1):(which(str_detect(Mort[,1], "^Total"))-1)) 
  
  Mort <- separate(Mort, col = 1,into = cols, sep = ";") %>% 
    separate(col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")
  
  lmorts[[i]] <- Mort
  
}

