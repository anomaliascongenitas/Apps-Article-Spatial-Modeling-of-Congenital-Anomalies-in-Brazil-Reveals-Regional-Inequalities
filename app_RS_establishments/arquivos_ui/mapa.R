library(shiny)

aba_mapa <- tabItem(
  "aba_mapa",
  fluidPage(
    fluidRow(
      tags$img(src = "logo_projeto_anomalias_git.png", height = 107 * 0.75),
      tags$img(
        src = "logo_parceiros_projeto.png",
        height = 107 * 0.75,
        width = 1275 * .75
      )
    ),
    fluidRow(
      tags$img(src = "ufrgs_logo.png", height = 107 * 0.75),
      tags$img(src = "logos_hcpa_ibc.png", height = 107 *
                 0.75),
      tags$img(src = "logo_ime.png", height = 107 *
                 0.75),
      tags$img(src = "pos_estatistica_logo.png", height = 107 *
                 0.75),
      tags$img(src = "ppg_genetica.png", height = 107 *
                 0.75)
    ),
    br(),
    fluidRow(
      tags$div("Aplicativo atualizado em 30 de abril de 2026", 
               style =  "color: #b30000; font-weight: bold; font-size: 18px; margin-left: 15px;")
    ),
    titlePanel(
      h1("Mapa das localizações dos hospitais que participam 
      da vigilância ativa")
    ),
    br(),
    fluidRow(
      box(
        title = "Filtros",
        checkboxGroupInput(
          "checkbox_cid_mapa",
          "Escolha o(s) grupo(s) de CID(s):",
          selected = 1:9,
          choiceNames = cids_values2,
          choiceValues = 1:10
        ),
        selectInput(
          "ano_grafico_mapa",
          label = "Escolha o ano ou os anos que terão seus dados apresentados na análise abaixo",
          choices = anos,
          selected = c("2010","2011","2012","2013","2014","2015","2016","2017",
                       "2018","2019", "2020", "2021", "2022", "2023", "2024", "2025"), multiple = TRUE
        ),
        #shiny::textOutput("teste"),
        background = "blue",
        width = 12
      )
    ),
    fluidRow(

      box(
        title = "Mapa",
        #background = "blue",
        width = 12,
        leafletOutput(outputId = "map",height = 700)
      )
    ),
    
    fluidRow(
      box(
        title = "Informações Hospital Selecionado",
        width = 12,
        uiOutput("mapa_texto"), 
        collapsible = TRUE,
        
        conditionalPanel(
          condition = "input.map_marker_click != null",
          
          
          fluidRow(
            valueBoxOutput("box_nascimentos", width = 4),
            valueBoxOutput("box_numero_casos_hosp", width = 4),
            valueBoxOutput("box_prevalencia_hosp", width = 4)
          ),
          highchartOutput("grafico_barras", height = "350px"),
          h3(
            "Características recém-nascidos:",
            style = "font-size: 18px; font-weight: bold; text-align: center;"
          ),
          fluidRow(
            column(4, highchartOutput("sexo")),
            column(4, highchartOutput("raca_bebe")),
            column(4, highchartOutput("peso"))
          ),
          h3(
            "Características materna:",
            style = "font-size: 18px; font-weight: bold; text-align: center;"
          ),
          fluidRow(
            column(4, highchartOutput("idade_mae")),
            column(4, highchartOutput("raca_mae")),
            column(4, highchartOutput("esco_mae"))
          ),
          h3(
              "Características gestação e registro:",
              style = "font-size: 18px; font-weight: bold; text-align: center;"
            ),
          fluidRow(
            column(4, highchartOutput("semana_gesta")),
            column(4, highchartOutput("tipo_parto")),
            column(4, highchartOutput("preencheu"))
          ),
          highchartOutput(outputId = "mapa_serie", height = "350px"),
          highchartOutput(outputId = "grafico_previsao",height = 700),
          h5("Previsão estimada através do método de alisamento exponencial."),
        )
      )
    ),
    fluidRow(
      box(
        title = h2(p("Tabela com todos hospitais considerando as anomalias e os anos e selecionados.")),
        h4(div("OBS: O primeiro valor indica a prevalência por 1.000 nascidos vivos com AC,
        o valor entre parênteses é o número de nascidos vivos com AC."#, 
              #  style = "color:red"
               )),
        #background = "blue",
        width = 12,
        dataTableOutput("tabela1"),
        downloadButton("download_tabela1"),
        collapsible = TRUE
      )
    )
    

  )
)
