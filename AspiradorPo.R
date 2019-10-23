source("Estado.R")

## Classe e métodos para o problema do aspirador de pó
AspiradorPo <- function(desc = NULL, pai = NULL){

  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("AspiradorPo", "Estado")

  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.AspiradorPo = function(obj1,obj2){
  if(.Generic == "=="){
    ##objetivo <- AspiradorPo()
    ##objetivo$desc <- c(NO = 0, NE = 0, SO = 0, SE = 0, AP = 0, C = 0)
    ##if(obj1$desc[1] == objetivo$desc[1] && obj1$desc[2] == objetivo$desc[2] 
    ##  && obj1$desc[3] == objetivo$desc[3] && obj1$desc[4] == objetivo$desc[4]){
    ##  print("primeiro if")
    ##  return (TRUE);
    
    ##}else if(obj2$desc[1] == objetivo$desc[1] && obj2$desc[2] == objetivo$desc[2] 
    ##  && obj2$desc[3] == objetivo$desc[3] && obj2$desc[4] == objetivo$desc[4]){
    ##  print("segundo if")
    ##  return (TRUE);
    
    ##}else{
    ##  print("ultimo if")
      return(obj1$desc[1] == obj2$desc[1] && obj1$desc[2] == obj2$desc[2] 
      && obj1$desc[3] == obj2$desc[3] && obj1$desc[4] == obj2$desc[4])
    ##}
  }
}

## Sobrecarga da função genérica "print" do R
print.AspiradorPo <- function(obj) {
  cat("(NO NE SO SE AP C): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.AspiradorPo <- function(atual){
  
  if(is.null(atual$desc))
    return(Inf)

  desc = atual$desc

  sujos = desc[1] + desc[2] + desc[3] + desc[4]
  
  if(desc[desc[5]] == 0){ ## quadrado do aspirador está limpo
    return ((2 * sujos) + 1)
  }else{ ## quadrado do aspirador está sujo
    return (2 * sujos)
  }
}

geraFilhos.AspiradorPo <- function(obj) {
  
  desc <- obj$desc

  ## filhos no formato: (?,?,?,?,?,?), em que:
  ## as 4 primeiras posições indicam se seu respectivo quadrado está limpo ou sujo
  ## a 5ª posição possui o índice do quadrado em que está presente o aspirador
  ## a 6ª posição possui o custo da operação

  if(desc[5] == 1){ ## aspirador está no canto superior esquerdo
    if(desc[1] == 0){ ## quadrado do aspirador está limpo
      filhos <- list(c(desc[1],desc[2],desc[3],desc[4],2,1), c(desc[1],desc[2],desc[3],desc[4],3,3))
    
    }else{ # quadrado do aspirador está sujo
      filhos <- list(c(0,desc[2],desc[3],desc[4],desc[5],2), c(desc[1],desc[2],desc[3],desc[4],2,1), c(desc[1],desc[2],desc[3],desc[4],3,3))
    }
  }else if(desc[5] == 2){ ## aspirador está no canto superior direito
    if(desc[2] == 0){ ## quadrado do aspirador está limpo
      filhos <- list(c(desc[1],desc[2],desc[3],desc[4],4,3), c(desc[1],desc[2],desc[3],desc[4],1,1))
    
    }else{ # quadrado do aspirador está sujo
      filhos <- list(c(desc[1],0,desc[3],desc[4],desc[5],2), c(desc[1],desc[2],desc[3],desc[4],4,3), c(desc[1],desc[2],desc[3],desc[4],1,1))
    }
  }else if(desc[5] ==  3){ ## aspirador está no canto inferior esquerdo
    if(desc[3] == 0){ ## quadrado do aspirador está limpo
      filhos <- list(c(desc[1],desc[2],desc[3],desc[4],4,1), c(desc[1],desc[2],desc[3],desc[4],1,3))

    }else{ # quadrado do aspirador está sujo
      filhos <- list(c(desc[1],desc[2],0,desc[4],desc[5],2), c(desc[1],desc[2],desc[3],desc[4],4,1), c(desc[1],desc[2],desc[3],desc[4],1,3))
    }
  }else{ ## aspirador está no canto inferior direito
    if(desc[4] == 0){ ## quadrado do aspirador está limpo
      filhos <- list(c(desc[1],desc[2],desc[3],desc[4],3,1), c(desc[1],desc[2],desc[3],desc[4],2,3))

    }else{ # quadrado do aspirador está sujo
      filhos <- list(c(desc[1],desc[2],desc[3],0,desc[5],2), c(desc[1],desc[2],desc[3],desc[4],3,1), c(desc[1],desc[2],desc[3],desc[4],2,3))
    }
  }
  
  ## gera os objetos AspiradorPo para os filhos
  count = 1
  for(f in filhos){
    print(f)
    x <- AspiradorPo(desc = f, pai = obj)
    x$h <- heuristica(x)
    x$g <- obj$g + f[[6]]
    if(count == 1){
      filhosGerados<- x
    }
    filhosGerados <- c(filhosGerados, x)
  }
  
  return(filhosGerados)
}
