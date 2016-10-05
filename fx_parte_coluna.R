# primeira função#
# para usar em colunas com mais do que uma informação por célula
# separadas com simbolo pre-definido: ex: - , +
# devolve uma tabela com 2 colunas: identificador info

### packages necessárias ###
library(stringr)
library(dplyr)

### pre-requisitos ###

#1 criar um objecto com os separadores
# ex com 2 separadores, o + e a ,:
# > separadores <- c("+",",")
# se não for criado, a função usa o + como separador padrão

fx_parte_coluna <- function(coluna_id, coluna_complexa,separadores = "+"){
  #print(coluna_id)
  #print(coluna_complexa)
  
  # transforma separadores em regex
  separadores <- paste(separadores,sep = "",collapse = "|")
  separadores <- str_replace(string = separadores,pattern = "\\+","\\\\+")
  
  # cria a lista com a coluna complexa separada pelos separadores
  l_separada <- str_split(coluna_complexa,separadores)
  #print(l_separada)
  
  # cria indice
  indice <- 1:length(coluna_id)
  
  # cria tabela de retorno
  for(i in indice){
      #print(i)
    
      for(j in l_separada[[i]]){
          
          #print(j)
          
          if(exists("tabela")){
            tabela <- bind_rows(tabela,data.frame(coluna_id[i],j))
          } else{
            tabela <- data.frame(coluna_id[i],j)
          }
          
      }
    
    tabela$j <- str_trim(tabela$j)
    
  }
  
  #print(tabela)
  names(tabela) <- c("identificador","info")
  return(tabela)
  
  
}

### como converter em tabela espalhada
# criar coluna chamada valores com o valor 1
# library(tidyr)
# tabela_espalhada <- spread(tabela,coluna_a_espalhar, valores)