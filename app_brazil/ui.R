#library(profvis) ## Pacote para monitorar o desempenho do aplicativo

################################################
options(OutDec= ",") #Muda de ponto para virgula nos decimais! 








header <- shinydashboardPlus::dashboardHeader(
  #enable_rightsidebar = T,
  #controlbarIcon = shiny::icon("gears"),
  title = tagList(
    span(class = "logo-lg", "Análise de nasc. vivos com anomalias congênitas "), 
    icon = icon("tachometer-alt")),
  titleWidth = 650
)

rightsidebar <- dashboardControlbar(icon = "",
  width = 400#,
  #h3("Digite as Macrorregiões de saúde de interesse"),
  # selectizeInput("filtro_geral",
  #                label = NULL,
  #                choices = macro_saude_shape$macroregiao,
  #                selected = macro_saude_shape$macroregiao,
  #                multiple = T,
  #                width = "100%")
)






sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Mapa por UFs",tabName = "mapa_uf",icon = icon("globe-americas")),
    menuItem("Mapa por Municípios",tabName = "mapa_municipios",icon = icon("globe-americas")),
    menuItem("Série Temporal por UFs",tabName = "serie_uf",icon = icon("globe-americas")),
    menuItem("Série Temporal por Municípios",tabName = "serie_munic",icon = icon("globe-americas")),
    menuItem("Sobre", tabName = "sobre",icon = icon("book"))
  ),
  width = 300
)

source("arquivos_ui/aba_mapa_uf.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/aba_mapa_municipios.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/aba_serie_uf.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/aba_serie_munic.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)
source("arquivos_ui/aba_sobre.R",encoding = "UTF-8",local = TRUE,keep.source = TRUE)


body <- dashboardBody(
  useShinyjs(),
  tags$head(tags$style(HTML(
    ".small-box {height: 115px}"
  ))),
  tabItems(
    aba_mapa_uf,
    aba_mapa_municipios,
    aba_serie_uf,
    aba_serie_munic,
    aba_sobre
  )
) 
#ys.setlocale(locale="")

shinyUI(dashboardPage(header = header, sidebar = sidebar, 
                      #controlbar = rightsidebar,    
                      body = body))




