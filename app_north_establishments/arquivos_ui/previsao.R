aba_previsao <- tabItem(
  "aba_previsao",
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
    titlePanel(
      "Comparação dos hospitais pelos grupos de anomalias."
    ),
    br(),
    fluidRow(
      box(
        title = "Filtros",
        selectInput(
          "ano_anomalias",
          label = "Escolha o ano ou os anos que terão seus dados apresentados na análise abaixo",
          choices = anos,
          selected = c("2010","2011","2012","2013","2014","2015","2016","2017",
                       "2018","2019", "2020", "2021", "2022", "2023", "2024", "2025"), multiple = TRUE
        ),
        shiny::selectInput(inputId = "variavel_3",label = "Selecione a variável que terá seus dados ilustrados no gráfico abaixo:",
                           choices = c("nº de nascimentos com anomalia" = 1,"prevalência por 1000 nascimentos" =2),
                           selected = 2),
        #shiny::textOutput("teste"),
        selectInput("uf_filtro2", "Selecione o(s) Estado(s):",
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
        background = "blue",
        width = 12
      )
    ),
    fluidRow(
      box(
        title = "Cardiopatias congênitas", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("barras_cc_top10"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Defeitos de parede abdominal", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("barras_dpa_top10"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Defeitos de redução de membros/ pé torto/ artrogripose/ polidactilia", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("barras_membros_top10"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Defeitos de tubo neural", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("barras_tubo_top10"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Fendas orais", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("barras_fendas_top10"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Hipospadia", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("barras_hipo_top10"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Microcefalia", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("barras_micro_top10"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Sexo indefinido", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("barras_sexo_top10"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Síndrome de Down", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("barras_down_top10"),
        collapsible = TRUE
      )
    ),
    fluidRow(
      box(
        title = "Outras anomalias", 
        width = 12, status = "primary", solidHeader = TRUE,
        highchartOutput("barras_outras_top10"),
        collapsible = TRUE
      )
    ),
    fluidRow(column(
      width = 6,
      tags$img(
        src = "pos_estatistica_logo.png",
        height = 100,
        width = 220
      ),
      tags$img(
        src = "ppg_genetica.png",
        height = 100,
        width = 124
      )
    )
    )
  )
)
