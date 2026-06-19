library(profvis)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(DT)
library(leaflet)
library(tidyverse)
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

dados_responsaveis_hosp_sel <- utils::read.csv("dados_responsaveis_hosp_sel.csv", encoding="UTF-8")
dados_caracteristicas_hosp_sel <- utils::read.csv("dados_caracteristicas_hosp_sel.csv", , encoding="UTF-8")

hosp_selec<- utils::read.csv("hosp_selec.csv", encoding="UTF-8")

nasc_hosp <- utils::read.csv("banco_estab_norte.csv", encoding="UTF-8") %>%
  filter(CODESTAB %in% hosp_selec$codigo)

base_cnes <- utils::read.csv("base_cnes.csv", encoding="UTF-8") 

lista_hosp_analise <- unique(anom_hosp$nome)

corrige_coord <- function(x) {
  ifelse(
    x < -1000,
    as.numeric(str_c(substr(x,1,3), ".", substr(x,4,nchar(x)))),
    x
  )
}

base_cnes <- base_cnes %>%
  mutate(
    LATITUDE = corrige_coord(LATITUDE),
    LONGITUDE = corrige_coord(LONGITUDE)
  )


 hosp_mapa <- nasc_hosp %>%
  left_join(base_cnes,by=c("CODESTAB" = "CNES")) %>%
  filter(ANONASC == 2019)



cids_values4 <- c("Card_Cong",                                              
                  "Def_par_abdom",                                         
                  "Def_red_membros",
                  "Def_Tubo_Neural",                                              
                  "Fendas_orais",                                                         
                  "hipospadia",                                                           
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

uf_map <- data.frame(
  codigo = c(11,12,13,14,15,16,17),
  estado = c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins"),
  stringsAsFactors = FALSE
)


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



