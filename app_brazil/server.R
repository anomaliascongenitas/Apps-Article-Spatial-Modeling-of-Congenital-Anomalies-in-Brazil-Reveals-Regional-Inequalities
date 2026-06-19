options(OutDec= ",") #Muda de ponto para virgula nos decimais! 

server <- function(input, output,session) {
    

    
    source("arquivos_server/banco_reativos.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
    source("arquivos_server/aba_mapa_uf_server.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
    source("arquivos_server/aba_mapa_municipio_server.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
    source("arquivos_server/aba_serie_uf_server.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
    source("arquivos_server/aba_serie_munic_server.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
}



