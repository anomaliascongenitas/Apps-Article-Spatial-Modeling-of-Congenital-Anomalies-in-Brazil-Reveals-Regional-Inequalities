library(readxl)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(DT)
library(leaflet)
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(sf)
library(ps)
library(spdep) 
library(kableExtra)
library(viridis)
library(ggbeeswarm)
library(rmapshaper)
library(shinyjs)

options(scipen=99)
options(OutDec= ",") 

banco_nascimentos = utils::read.csv("dados/banco_nascimentos.csv", encoding="UTF-8") %>%
  mutate(NOMEMUN = replace_na(NOMEMUN,"Não identficado"))

banco_anomalias =  utils::read.csv("dados/banco_anomalias.csv", encoding="UTF-8")

mapa_regioes <- st_read("shapefile/grandes_regioes_shp/grandes_regioes_shp_simplify.shp", quiet = TRUE)
mapa_uf <- st_read("shapefile/Brasil/UFEBRASIL_simplify.shp", quiet = TRUE) %>%
  mutate(CODUF = as.numeric(as.character(CD_GEOCODU)))
mapa_municipios <- st_read("shapefile/br_municipios_leve/BRMUE250GC_SIR_simplify.shp", quiet = TRUE)

mapa_municipios$codigo_ibge = as.numeric(substr(mapa_municipios$CD_GEOCMU,1,6))

## Tirando lagoa Mirim e Lagoa dos Patos
mapa_municipios = mapa_municipios %>%
  filter(mapa_municipios$codigo_ibge != 430000)

## Codigo para tabelas
rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"  
)



siglas_ufs =  utils::read.csv("dados/siglas_ufs.csv", encoding="UTF-8")


op_ufs = str_c("(",siglas_ufs$SIGLAUF,") ", siglas_ufs$UF)
op_ufs_munic = siglas_ufs$SIGLAUF
op_ufs_munic2 = siglas_ufs$CODUF



banco_nascimentos = banco_nascimentos %>%
  left_join(siglas_ufs, by = c("UF" = "CODUF"))


anos <- unique(banco_nascimentos$ANONASC)
limites_contagem <- round(100*1.2,0)
limites_prevalencia <- 100*1.2
cids_values <- c("Cardiopatias congênitas",                                              
                 "Defeitos de parede abdominal",                                         
                 "Defeitos de redução de membros/ pé torto/ artrogripose / polidactilia",
                 "Defeitos de Tubo Neural",                                              
                 "Fendas orais",                                                         
                 "hipospadia",                                                           
                 "Microcefalia",                                                         
                 "Sexo indefinido",                                                      
                 "Síndrome de Down","Outras")   

cids_values2 <- c("Cardiopatias congênitas – CID Q20, Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28",
                  "Defeitos de parede abdominal – CID Q79.2 Q79.3",
                  "Defeitos de redução de membros/ pé torto/ artrogripose / polidactilia – CID Q66, Q69, Q71, Q72, Q73 e Q74.3",
                  "Defeitos de Tubo Neural – CID Q00.0, Q00.1, Q00.2, Q01 e Q05",
                  "Fendas orais – CID Q35, Q36 e Q37",
                  "hipospadia - CID  Q54",
                  "Microcefalia – CID Q02",
                  "Sexo indefinido CID Q56",
                  "Síndrome de Down – CID Q90",
                  "Outras")




variavel_opcoes = c("Número de nascimentos","Número de nascimentos com anomalia",
                    "Prevalência ao nascimento por 10000 nascimentos")
variavel_opcoes2 = c("n_nascimentos","n_anomalias",
                    "prevalencia")




mapa_municipios2 = mapa_municipios %>%
  mutate(CODUF = as.numeric(substr(CD_GEOCMU,1,2))) %>%
  left_join(siglas_ufs,by = c("CODUF"))


old = 0
contador = 0


nomes_munic = with(mapa_municipios2,str_c(codigo_ibge," ",SIGLAUF," - ",NM_MUNICIP))
cod_munic  = mapa_municipios2$codigo_ibge