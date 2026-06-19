aba_hospitais <- tabItem(
  "aba_hospitais",
  fluidPage(
    shinyjs::useShinyjs(),
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
    titlePanel(
      "Estatísticas descritivas dos hospitais com mais de 10 mil nascimentos."
    ),
    br(),
    fluidRow(
      box(
        title = "Escolha os grupos de ACs que terão seus dados apresentados no
        boxplot abaixo.",
        checkboxGroupInput(
          "checkbox_cid",
          "Escolha o(s) grupo(s) de CID(s):",
          selected = 1:9,
          choiceNames = cids_values2,
          choiceValues = 1:10
        ),

        shiny::selectInput(inputId = "variavel",label = "Selecione a variável que terá seus dados ilustrados no gráfico abaixo:",
                           choices = c("nº de nascimentos com anomalia" = 1,"prevalência por 1000 nascimentos" =2,"nº nascimentos" =3), selected = 2),
        
        selectInput("uf_filtro", "Selecione o(s) Estado(s):",
          choices =  c(
            "Rondônia" = "11",
            "Acre" = "12",
            "Amazonas" = "13",
            "Roraima" = "14",
            "Pará" = "15",
            "Amapá" = "16",
            "Tocantins" = "17"
          ),
          selected =  c("11","12","13","14","15","16","17"),
          multiple = TRUE
        ),
        
        selectInput(
          "ano_hospitais",
          label = "Escolha o(s) Ano(s):",
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
        title = "Boxplot com dados de TODOS os hospitais e ACs selecionadas.",
        width = 12, status = "primary", solidHeader = TRUE,
        plotlyOutput("box_plot"),
        collapsible = TRUE
      )
    ),
    
    fluidRow(
      box(
        title = "Heatmap Temporal", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("heatmap_top10"),
        collapsible = TRUE
      )
    ),
    
    fluidRow(
      box(
        title = "Comparação dos Hospitais", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("barras_top10_ano"),
        collapsible = TRUE
      )
    ),
    
    titlePanel(
      h1("Série Temporal dos dados de ACs selecionadas acima, 
         mas considerando SOMENTE o(s) hospital(is) selecionado(s) no filtro abaixo.")
    ),
    
    fluidRow(
      box(
        title = "Serie temporal dos dados de ACs e para os hospitais selecionados",
        selectInput('hosp_selec', 'Selecione o(s) Hospital(is)', unique(anom_hosp$nome), multiple=TRUE, selectize=TRUE,selected = unique(anom_hosp$nome)[1]),
        #background = "blue",
        width = 12,
        checkboxInput(inputId = "eixoyauto",label= "ajustar eixo y automaticamente",value = TRUE),
        sliderInput(inputId = "eixoy",label="Eixo y",value = c(0,50),min=0,max=5000,step = 0.01),

        highchartOutput(outputId = "serie_hosp_selec",height = 700),
        collapsible = TRUE
      )
    ),
    

    titlePanel("Os dados abaixo apresentam as estatísticas
               dos grupos de anomalias, anos e hospitais selecionados acima."),
    fluidRow(
      valueBoxOutput("box_populacao", width = 3),
      valueBoxOutput("box_numero_casos", width = 3),
      valueBoxOutput("box_prevalencia", width = 3)
    ),
    
    fluidRow(
      box(
        title = "Tabela por hospitais com os grupos de cids e anos selecionados",
       # background = "blue",
        width = 12,
        dataTableOutput("tabela2"),
        downloadButton("download_tabela2"),
        collapsible = TRUE
      )
    )
  )
)
