# funçao que cria uma coluna com os conceitos finais usando regex - hits

fx_coluna_hits <- function(tab_expreg,expressao){
  tab_expreg$conceito -> conceito
  tab_expreg$exp_reg -> expreg
  
  hits <- c()
  
  for(i in expressao){
    #print(i)
    
    # no caso de i ser um NA
    if(is.na(i)) {
      bb <- NA
      hits <- append(hits,bb)
      next
    }
    
    
    sapply(expreg,function(x) str_detect(i, x)) -> bb
    bb <- unname(bb)
    bb <- conceito[bb]
    
    bb <- paste(bb, collapse = " : ")
    
    hits <- append(hits,bb)
    
  }
  
  return(hits)
  
  
}