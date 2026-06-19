############# Aba Municípios #############

aba_mapa_municipios = tabItem(
  "mapa_municipios",
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
      "Mapa e gráficos das Anomalias Congêntitas (ACs) das unidades federativas do Brasil"
    ),

    
    fluidRow(
      box(
        title = "Filtros",
        h3("Selecione o(s) grupo(s) de CID(s):", style = "line-height:0"),
        br(),
        checkboxGroupInput(label = NULL,
                           "mapa_munic_cid",
                           selected = 1,
                           choiceNames = cids_values2,
                           choiceValues = 1:10
        ),
        
        h3("Escolha o(s) ano(s) a ser(em) considerado(s):"),
        br(),
        selectizeInput(
          "mapa_munic_ano",
          label = NULL,
          choices = anos[order(anos,decreasing = T)],
          selected = max(anos),
          multiple = TRUE
        ),
        
        h3("Selecione a variável de interesse:"),
        br(),
        radioButtons(label = NULL,
                     "mapa_munic_variavel",
                     selected = 3,
                     choiceNames = variavel_opcoes,
                     choiceValues = 1:3
        ),
        
        h3("Selecione as UFs:"),
        checkboxGroupInput(
          inputId = "mapa_munic_uf",
          label = "",
          choiceNames = op_ufs_munic ,choiceValues = op_ufs_munic2,
          selected = c("43"),inline = TRUE
        ),
        actionButton("desmarcar_uf","Desmarcar todas UF(s)"),
        actionButton("marcar_uf","Marcar todas UF(s)"),
        
        background = "blue",
        width = 12
      )
    ),
    
    fluidRow(
      valueBoxOutput("box_populacao_munic", width = 4),
      valueBoxOutput("box_numero_casos_munic", width = 4),
      valueBoxOutput("box_prevalencia_munic", width = 4)
    ),
    #br(),
    fluidRow(
      box(
        title = "Mapa pela variável de interesse considerando os grupos de CIDs e anos selecionados",
        background = "blue",
        width = 12,
        leafletOutput("grafico_mapa_munic", height = "700px"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Gráfico de barras da variável de interesse considerando os grupos de CIDs e anos selecionados",
        width = 6,
        background = "blue",
        plotlyOutput("grafico_barras_munic"),
        collapsible = TRUE
        
      ),
      box(
        title = "Serie temporal da variável de interesse considerando os grupos de CIDs e anos selecionados",
        width = 6,
        background =  "blue",
        plotlyOutput("grafico_serie_munic"),
        collapsible = TRUE
      )
    ), 
    fluidRow(
      box(
        title = "Gráfico de densidade da variável de interesse ao longo dos anos considerando os grupos de CIDs selecionados",
        background = "blue",
        sliderInput(
          "limite_dots_cid_munic",
          "Limites para aparecer apenas municípios esse limite ou mais de nascimentos em 2021",
          min = 0,
          max = 5000,
          value = c(1000),
          step = 5000/100
        ),
        plotlyOutput("plot_dots_munic"),
        width = 12,
        collapsible = TRUE
      )
    ), 
    fluidRow(
      box(
        title = "Gráfico da evolução da variável de interesse considerando os grupos de CIDs selecionados",
        background = "blue",
        uiOutput("gerar_input_quadradinhos_munic"),

        #uiOutput("teste"),
        plotlyOutput("plot_quadradinhos_munic"),
        width = 12,
        collapsible = TRUE
      )
    ), 
    

    fluidRow(
      column(
        width = 12,
        h2("Tabela de prevalência ao nascimento agrupada por cada grupo de CID de saúde selecionadas"),
        dataTableOutput("tabela_munic_1"),
        br(),
        downloadButton("downloadData_munic_1", "Download Tabela de dados")
      )
    ),
    
    br(),  
    br(),
    
  
    fluidRow(
      column(
        width = 12,
        h2("Tabela de prevalência ao nascimento com apenas os grupos de CID's e anos selecionados"),
        dataTableOutput("tabela_munic_2"),
        br(),
        downloadButton("downloadData_munic_2", "Download Tabela de dados")
      )
    ),
    
    br(),
    br()
    
 
  )
)

