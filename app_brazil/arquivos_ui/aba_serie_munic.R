aba_serie_munic <- tabItem(
  "serie_munic",
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
      tags$img(src = "ppg_genetica.png",  height = 107 *
                 0.75)
    ),
    
    titlePanel(
      "Série Temporal das prevalências ao nascimento de anomalias congenitas por 10.000
                       por município"
    ),
    fluidRow(
      box(
        title = "Filtros",
        h3("Selecione o(s) grupo(s) de CID(s):", style = "line-height:0"),
        br(),
        checkboxGroupInput(label = NULL,
                           "serie_munic_cid",
                           selected = 1,
                           choiceNames = cids_values2,
                           choiceValues = 1:10
        ),
        
        
        h3("Selecione a variável de interesse:"),
        br(),
        radioButtons(label = NULL,
                     "serie_munic_variavel",
                     selected = variavel_opcoes2[3],
                     choiceNames = variavel_opcoes,
                     choiceValues = variavel_opcoes2
        ),
        br(),
        h3("Digite os municípios de interesse:"),
        
        #checkboxInput(inputId = "serie_munic_cod_ibge",label = "Com Código IBGE",value = FALSE),
        #uiOutput(outputId = "gerar_munic_serie"),
        selectizeInput("serie_munic",
                       label = "Escolha a(s) Município(s)",
                       choices = nomes_munic,
                       multiple = T,
                       options = list(maxItems = 100, placeholder = 'Escolha a(s) Município(s)'),
                       selected = nomes_munic[2616]),
        
        background = "blue",
        width = 12
      )
    ),
    br(),
    fluidRow(
      sidebarPanel(
        #htmlOutput("input_quadradinhos_html_cidade1"),
        sliderInput(
          "serie_limite_munic",
          "Limites do eixo vertical",
          min = (0),
          max = limites_prevalencia + 2,
          value = c(0, limites_prevalencia + 2),
          step = 1,
          round = TRUE
        ),
        width = 12
      ),
      mainPanel(
        uiOutput("teste2"),
        plotlyOutput("aba_serie_munic", height = "600px"),
        width = 12
      )
    ),
    br(),
    br()

  )
)
