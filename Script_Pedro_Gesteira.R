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

arquivos <- list.files(paste(getwd(),"/FE",sep = ""), pattern = "PopFE", full.names = TRUE, include.dirs = TRUE)

for (i in seq_along(list.files(paste(getwd(),"/FE",sep = ""), pattern = "PopFE", full.names = TRUE, include.dirs = TRUE))) {

  Pop <- read.csv(arquivos[i], sep = "\t")
  
  cols <- unlist(str_split(Pop[which(str_detect(Pop[,1], "^Município")),], pattern = ";"))
  
  ano <- substr(arquivos[i],85,88)
  
  Pop <- as.tibble(select(Pop,1)) %>% slice((which(str_detect(Pop[,1], "^Município"))+1):(which(str_detect(Pop[,1], "^Total"))-1)) 
  
  Pop <- separate(Pop, col = 1,into = cols, sep = ";") %>% 
    separate(col = 1, into = c("M_cod","M_name"), sep = "\\s", extra = "merge")
  
  Pop <- Pop %>% pivot_longer(3:ncol(Pop), names_to = "FE",values_to = ano, values_drop_na = FALSE)
  
  lpops[[i]] <- Pop

}

