 ## Pacote para monitorar o desempenho do aplicativo

################################################

#tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")

anos <- 2025:2010
cids_values <- c("Cardiopatias congênitas",                                              
                 "Defeitos de parede abdominal",                                         
                 "Defeitos de redução de membros/ pé torto/ artrogripose / polidactilia",
                 "Defeitos de Tubo Neural",                                              
                 "Fendas orais",                                                         
                 "Hipospadia",                                                           
                 "Microcefalia",                                                         
                 "Sexo indefinido",                                                      
                 "Síndrome de Down",
                 "Outras Anomalias")   

cids_values2 <- c("Cardiopatias congênitas – CID Q20, Q21, Q22, Q23, Q24, Q25, Q26, Q27, Q28",
                  "Defeitos de parede abdominal – CID Q79.2 Q79.3",
                  "Defeitos de redução de membros/ pé torto/ artrogripose / polidactilia – CID Q66, Q69, Q71, Q72, Q73 e Q74.3",
                  "Defeitos de Tubo Neural – CID Q00.0, Q00.1, Q00.2, Q01 e Q05",
                  "Fendas orais – CID Q35, Q36 e Q37",
                  "Hipospadia - CID  Q54",
                  "Microcefalia – CID Q02",
                  "Sexo indefinido CID Q56",
                  "Síndrome de Down – CID Q90",
                  "Outras Anomalias")


header <- shinydashboardPlus::dashboardHeader(
  #enable_rightsidebar = T,
  controlbarIcon = shiny::icon("gears"),
  title = tagList(
    span(class = "logo-lg", str_c("Análise dos Hospitais da região Norte")), 
    icon = icon("tachometer-alt")),
  titleWidth = 650
)

rightsidebar <- dashboardControlbar(disable = TRUE)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Mapa e análise por hospital ",tabName = "aba_mapa",icon = icon("globe-americas",lib = "font-awesome")),
    menuItem("Comparação dos hospitais e estados",tabName = "aba_hospitais",icon = icon("file-medical-alt")),
    menuItem("Comparação por grupos de anomalias",tabName = "aba_previsao",icon = icon("chart-line"))
  ),
  width = 300
)


source("arquivos_ui/hospitais_vigilancia.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/previsao.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/mapa.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)

body <- dashboardBody(
  tags$head(tags$style(HTML(
    ".small-box {height: 115px}"
  ))),
  tabItems(
    aba_hospitais,
    aba_previsao,
    aba_mapa
  )
) 
#ys.setlocale(locale="")

shinyUI(dashboardPage(controlbar = rightsidebar, header = header, sidebar = sidebar, 
                          body = body))

