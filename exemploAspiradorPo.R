source("AspiradorPo.R")
source("buscaDesinformada.R")
source("buscaInformada.R")

inicial <- AspiradorPo(desc = c(NO = 0, NE = 1, SO = 0, SE = 1, AP = 1, C = 0))

objetivo <- AspiradorPo()
objetivo$desc <- c(NO = 0, NE = 0, SO = 0, SE = 0, AP = 0, C = 0) # valores de AP e C são indiferentes

##cat("====\tBusca em Largura\t====\n")
##print(unlist(buscaEmLargura(inicial, objetivo)))

##cat("====\tBusca em Profundidade\t=====\n")
##print(buscaEmProfundidade(inicial, objetivo))

##cat("====\tBusca de Custo Uniforme\t=====\n")
##print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))