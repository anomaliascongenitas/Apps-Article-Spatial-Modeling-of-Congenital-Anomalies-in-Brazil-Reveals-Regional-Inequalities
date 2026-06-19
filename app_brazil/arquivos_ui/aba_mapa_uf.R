############# Aba UFs #############

aba_mapa_uf = tabItem(
  "mapa_uf",
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
    br(),
    fluidRow(
      tags$div("Aplicativo atualizado em 30 de abril de 2026", 
               style =  "color: #b30000; font-weight: bold; font-size: 18px; margin-left: 15px;")
    ),
    titlePanel(
      "Mapa e gráficos das Anomalias Congêntitas (ACs) das unidades federativas do Brasil"
    ),

    
    fluidRow(
      box(
        title = "Filtros",
        h3("Selecione o(s) grupo(s) de CID(s):", style = "line-height:0"),
        br(),
        checkboxGroupInput(label = NULL,
                           "mapa_uf_cid",
                           selected = 1,
                           choiceNames = cids_values2,
                           choiceValues = 1:10
        ),
        
        h3("Escolha o(s) ano(s) a ser(em) considerado(s):"),
        br(),
        selectizeInput(
          "mapa_uf_ano",
          label = NULL,
          choices = anos[order(anos,decreasing = T)],
          selected = max(anos),
          multiple = TRUE
        ),
        br(),
        h3("Selecione a variável de interesse:"),
        br(),
        radioButtons(label = NULL,
                     "mapa_uf_variavel",
                     selected = 3,
                     choiceNames = variavel_opcoes,
                     choiceValues = 1:3
        ),
        
     
        
        background = "blue",
        width = 12
      )
    ),
    
    
    
    
    
    # fluidRow(
    #   box(
    #     title = "Selecione a variável de interesse:",
    #     radioButtons(label = NULL,
    #                  "mapa_uf_variavel",
    #                  selected = 3,
    #                  choiceNames = variavel_opcoes,
    #                  choiceValues = 1:3
    #     ),
    #     background = "blue",
    #     width = 12
    #   )
    # ),
    # fluidRow(
    #   box(
    #     title = "Selecione o(s) grupo(s) de CID(s):",
    #     checkboxGroupInput(label = NULL,
    #       "mapa_uf_cid",
    #       selected = 1,
    #       choiceNames = cids_values2,
    #       choiceValues = 1:9
    #     ),
    #     background = "blue",
    #     width = 12
    #   )
    # ),
    # fluidRow(box(
    #   title = "Escolha o(s) ano(s) a ser(em) considerado(s):",
    #   selectizeInput(
    #     "mapa_uf_ano",
    #     label = NULL,
    #     choices = anos[order(anos,decreasing = T)],
    #     selected = max(anos),
    #     multiple = TRUE
    #   ),
    #   background = "blue",
    #   width = 12
    # )),
    
    
    
    fluidRow(
      valueBoxOutput("box_populacao_uf", width = 4),
      valueBoxOutput("box_numero_casos_uf", width = 4),
      valueBoxOutput("box_prevalencia_uf", width = 4)
    ),
    #br(),
    fluidRow(
      box(
        title = "Mapa pela variável de interesse considerando os grupos de CIDs e anos selecionados",
        background = "blue",
        width = 12,
        leafletOutput("grafico_mapa_uf", height = "700px"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Gráfico de barras da variável de interesse considerando os grupos de CIDs e anos selecionados",
        width = 6,
        background = "blue",
        plotlyOutput("grafico_barras_uf"),
        collapsible = TRUE
        
      ),
      box(
        title = "Serie temporal da variável de interesse considerando os grupos de CIDs e anos selecionados",
        width = 6,
        background =  "blue",
        plotlyOutput("grafico_serie_uf"),
        collapsible = TRUE
      )
    ), 
    fluidRow(
      box(
        title = "Gráfico de densidade da variável de interesse ao longo dos anos considerando os grupos de CIDs selecionados",
        background = "blue",
        #uiOutput("gerar_limite_dots_cid_uf"),
        plotlyOutput("plot_dots_uf"),
        width = 12,
        collapsible = TRUE
      )
    ), 
    fluidRow(
      box(
        title = "Gráfico da evolução da variável de interesse considerando os grupos de CIDs selecionados",
        background = "blue",
        selectizeInput("input_quadradinhos_uf",
                       label = "Escolha a(s) UF(s)",
                       choices = op_ufs,
                       multiple = T,
                       options = list(maxItems = 300, placeholder = 'Escolha a(s) UF(s)'),
                       selected = op_ufs),
        #uiOutput("teste"),
        plotlyOutput("plot_quadradinhos_uf"),
        width = 12,
        collapsible = TRUE
      )
    ), 
    # fluidRow(
    #   box(
    #     title = "Gráfico de Área do Número de nascidos vivos com anomalia congênita por grupo de CID considerando os grupos de CIDs e macrorregiões de saúde selecionados",
    #     background = "blue",
    #     plotlyOutput("plot_area_chart_cid"),
    #     width = 12,
    #     collapsible = TRUE
    #   )
    # ), 
    fluidRow(
      column(
        width = 12,
        h2("Tabela de prevalência ao nascimento agrupada por cada grupo de CID de saúde selecionadas"),
        dataTableOutput("tabela_uf_1"),
        br(),
        downloadButton("downloadData_uf_1", "Download Tabela de dados")
      )
    ),
    
    hr(),
    
    fluidRow(
      column(
        width = 12,
        h2("Tabela de prevalência ao nascimento com apenas os grupos de CID's e anos selecionados"),
        dataTableOutput("tabela_uf_2"),
        br(),
        downloadButton("downloadData_uf_2", "Download Tabela de dados"),
        br(), br(), br()
      )
    )
    
  )
)
