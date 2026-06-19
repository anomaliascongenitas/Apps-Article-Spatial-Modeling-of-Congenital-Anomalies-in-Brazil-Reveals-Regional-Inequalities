aba_serie_uf <- tabItem(
  "serie_uf",
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
                       por Unidade Federativa"
    ),
    fluidRow(
      box(
        title = "Filtros",
        h3("Selecione o(s) grupo(s) de CID(s):", style = "line-height:0"),
        br(),
        checkboxGroupInput(label = NULL,
                           "serie_uf_cid",
                           selected = 1,
                           choiceNames = cids_values2,
                           choiceValues = 1:10
        ),
        
        
        h3("Selecione a variável de interesse:"),
        br(),
        radioButtons(label = NULL,
                     "serie_uf_variavel",
                     selected = variavel_opcoes2[3],
                     choiceNames = variavel_opcoes,
                     choiceValues = variavel_opcoes2
        ),
        
        selectizeInput("serie_uf",
                       label = "Escolha a(s) UF(s)",
                       choices = op_ufs,
                       multiple = T,
                       options = list(maxItems = 300, placeholder = 'Escolha a(s) UF(s)'),
                       selected = op_ufs[23]),
        
        background = "blue",
        width = 12
      )
    ),
    br(),
    fluidRow(
      sidebarPanel(
        #htmlOutput("input_quadradinhos_html_cidade1"),
        sliderInput(
          "serie_limite",
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
        plotlyOutput("aba_serie_ufs", height = "600px"),
        width = 12
      )
    ),
    br(),
    br()
  )
)
