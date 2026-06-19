library(profvis)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(DT)
library(leaflet)
library(tidyverse)
library(tidyr)
library(plotly)
library(sf)
library(ps)
library(spdep) 
library(kableExtra)
library(viridis)
library(ggbeeswarm)
library(rmapshaper)
library(timetk)
library(kableExtra)
library(highcharter)
library(xts)
library(quantmod)
library(forecast)
#install.packages("fpp")
#library(fpp)
library(shinyjs)
library(ggplot2)
library(ggfortify)
library(leafpop)

options(OutDec= ".") #Muda de ponto para virgula nos decimais! 

eval_parse <- function(x){
  eval(parse(text = str_c(x,collapse = "")))
}


variavel <- c("nº de nascimentos com anomalia" ,"prevalência por 1000 nascimentos" ,"nº nascimentos")
variavel2 <- c("nº de nascimentos com anomalia" ,"prevalência por 1000 nascimentos" ,"nº nascimentos")
variavel_aux <- c("n_anomalias" ,"prevalencia" ,"n_nascimentos")

anom_hosp <- utils::read.csv("anom_hosp_selec.csv", encoding="UTF-8")

anom_hosp <- anom_hosp %>%
  mutate(nome = ifelse(codestab == 2246988, 
                       "HOSPITAL SAO VICENTE DE PAULO PASSO FUNDO", 
                       nome))

anom_hosp <- anom_hosp %>%
  mutate(nome = ifelse(codestab == 2257815, 
                       "HOSPITAL SAO VICENTE DE PAULO OSÓRIO", 
                       nome))

dados_responsaveis_hosp_sel <- utils::read.csv("dados_responsaveis_hosp_sel.csv", encoding="UTF-8")
dados_caracteristicas_hosp_sel <- utils::read.csv("dados_caracteristicas_hosp_sel.csv", , encoding="UTF-8")

hosp_selec<- utils::read.csv("hosp_selec.csv", encoding="UTF-8")

hosp_selec <- hosp_selec %>%
  mutate(nome = ifelse(codigo == 2246988, 
                       "HOSPITAL SAO VICENTE DE PAULO PASSO FUNDO", 
                       nome))

hosp_selec <- hosp_selec %>%
  mutate(nome = ifelse(codigo == 2257815, 
                       "HOSPITAL SAO VICENTE DE PAULO OSÓRIO", 
                       nome))

nasc_hosp <- utils::read.csv("banco_estab_rs.csv", encoding="UTF-8") %>%
  filter(CODESTAB %in% hosp_selec$codigo)

base_cnes <- utils::read.csv("base_cnes.csv", encoding="UTF-8") 

lista_hosp_analise <- unique(anom_hosp$nome)

x = base_cnes[1,14]
gambiarra = function(x){
  if(x< -1000){
    y = as.numeric(str_c(substr(x,1,3),".",substr(x,4,1000000L)))
    return(y)
  } else {
    x
  }
}

for (j in 14:15) {
  for (i in 1:nrow(base_cnes)) {
    base_cnes[i,j] = gambiarra(base_cnes[i,j])
  }
}





 hosp_mapa <- nasc_hosp %>%
  left_join(base_cnes,by=c("CODESTAB" = "CNES")) %>%
  filter(ANONASC == 2019)

 
 hosp_mapa <- hosp_mapa %>%
   mutate(
     LATITUDE = case_when(
       CODESTAB == "2237253" ~  -30.0307,
       CODESTAB == "2237822" ~  -30.0293,
       CODESTAB == "2232057" ~  -29.6780,
       CODESTAB == "2232146" ~  -29.6748,
       CODESTAB == "2223538" ~  -29.1610,
       CODESTAB == "2223546" ~   -29.1672,
       CODESTAB == "2223570" ~   -29.1533,
       CODESTAB == "3356868" ~  -29.1647,
       TRUE ~ LATITUDE
     ),
     LONGITUDE = case_when(
       CODESTAB == "2237253" ~ -51.2215,
       CODESTAB == "2237822" ~ -51.2147,
       CODESTAB == "2232057" ~ -51.1155,
       CODESTAB == "2232146" ~ -51.1319,
       CODESTAB == "2223538" ~ -51.1559,
       CODESTAB == "2223546" ~ -51.1836,
       CODESTAB == "2223570" ~ -51.1734,
       CODESTAB == "3356868" ~ -51.2004,
       TRUE ~ LONGITUDE
     )
   ) 


cids_values4 <- c("Card_Cong",                                              
                  "Def_par_abdom",                                         
                  "Def_red_membros",
                  "Def_Tubo_Neural",                                              
                  "Fendas_orais",                                                         
                  "Hipospadia",                                                           
                  "Microcefalia",                                                         
                  "Sexo_indef",                                                      
                  "Sindrome_Down",
                  "Outras_anomalias")  

cids_nomes <- c("Cardiopatias Congênitas",                                              
                "Parede Abdominal",                                         
                "Redução Membros",
                "Tubo Neural",                                              
                "Fendas Orais",                                                         
                "Hipospadia",                                                           
                "Microcefalia",                                                         
                "Sexo Indefinido",                                                      
                "Sindrome Down",
                "Outras Anomalias")  


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



