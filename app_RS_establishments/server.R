server <- function(input, output, session) {
  
  output$serie_hosp_selec <- renderHighchart({
    aux <- anom_hosp %>%
      filter(nome %in% input$hosp_selec) %>%
      select(1:5, 16, as.numeric(input$checkbox_cid) + 5)
    
    if(ncol(aux) > 7){
      aux$n_anomalias <- apply(aux[,7:ncol(aux)], 1, any) 
    } else {
      aux$n_anomalias <- as.numeric(aux[,7])
    }
    
    aux2 <- aux %>%
      group_by(codestab, nome, ANONASC) %>%
      summarise(n_anomalias = sum(n_anomalias)) %>%
      ungroup()
    
    aux3 <- aux2 %>%
      left_join(nasc_hosp, by = c("codestab" = "CODESTAB", "ANONASC")) %>%
      select(codestab, nome, ANONASC, n_anomalias = n_anomalias.x, n_nascimentos) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    names(aux3)[which(names(aux3) == variavel_aux[as.numeric(input$variavel)])] <- "variavel"
    
    hc <- aux3 %>%
      hchart(
        "line", 
        hcaes(x = ANONASC, y = variavel, group = nome)
      )
    
    if(input$eixoyauto){
      hc <- hc %>% hc_yAxis(title = list(text = variavel[as.numeric(input$variavel)])) %>%
        hc_xAxis(title = list(text = "Ano"))
    } else {
      hc <- hc %>% hc_yAxis(title = list(text = variavel[as.numeric(input$variavel)]),
                            min = input$eixoy[1], max = input$eixoy[2]) %>%
        hc_xAxis(title = list(text = "Ano"))
    }
    
    hc
  })
  
  dados_reativos <- reactive({
    req(input$variavel, input$ano_hospitais, input$checkbox_cid, input$hosp_selec)
    
    aux <- anom_hosp %>%
      filter(nome %in% input$hosp_selec,
             ANONASC %in% input$ano_hospitais) %>%
      select(1:5,16,as.numeric(input$checkbox_cid)+5)
    
    if(ncol(aux) >7){
      aux$n_anomalias <- apply(aux[,7:ncol(aux)],1,any) 
    }else {
      aux$n_anomalias <-  as.numeric(aux[,7])
    }
    
    aux2 <- aux %>%
      group_by(codestab,nome) %>%
      summarise(n_anomalias = sum(n_anomalias), na.rm = TRUE) %>%
      ungroup()
    
    
    aux_nasc_sum <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_hospitais) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        cod_mun = first(CODMUNNASC) 
      ) %>%
      ungroup()
    
    aux3 <- aux2 %>%
      left_join(aux_nasc_sum ,by = c("codestab" = "CODESTAB")) %>%
      select(codestab,nome,n_anomalias,n_nascimentos) %>%
      mutate(prevalencia = n_anomalias/n_nascimentos*1000)
    return(aux3)
  })
  
  
  output$box_populacao <- renderValueBox({
    
    valueBox(
      sum(dados_reativos()$n_nascimentos),
      "Total nascidos vivos",
      icon = icon("baby",lib = "font-awesome"),
      color = "blue"
    )
  })
  
  
  output$box_numero_casos <- renderValueBox({
    valueBox(
      sum(dados_reativos()$n_anomalias),
      "Total nascidos vivos com anomalias congenitas",
      icon = icon("notes-medical",lib = "font-awesome"),
      color = "red"
    )
  })
  
  output$box_prevalencia <- renderValueBox({
    valueBox(
      round(sum(dados_reativos()$n_anomalias)/sum(dados_reativos()$n_nascimentos)*10^3,3),
      "Prevalência ao nascimento por 1000",
      icon = icon("notes-medical"),
      color = "purple"
    )
  })
  
  
  ######################## ABA MAPA ########################
  
  aux5_pop <- reactiveVal(NULL)
  
  output$map <- renderLeaflet({
    aux <- anom_hosp %>%
      select(1:5, 16, as.numeric(input$checkbox_cid_mapa) + 5) %>%
      filter(ANONASC %in% input$ano_grafico_mapa)
    
    if(ncol(aux) > 7){
      aux$n_anomalias <- apply(aux[,7:ncol(aux)], 1, any) 
    } else {
      aux$n_anomalias <- as.numeric(aux[,7])
    }
    
    aux2 <- aux %>%
      group_by(codestab, nome, ANONASC) %>%
      summarise(n_anomalias = sum(n_anomalias)) %>%
      ungroup()
    
    aux3 <- aux2 %>%
      left_join(nasc_hosp, by = c("codestab" = "CODESTAB", "ANONASC")) %>%
      select(codestab, nome, ANONASC, n_anomalias = n_anomalias.x, n_nascimentos) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux4 <- aux3 %>%
      group_by(codestab, nome) %>%
      summarise(n_anomalias = sum(n_anomalias), n_nascimentos = sum(n_nascimentos)) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    hosp_mapa_aux <- aux4 %>% 
      left_join(hosp_mapa, by = c("codestab" = "CODESTAB")) %>%
      select(-n_anomalias.y, -n_nascimentos.y) %>%
      rename(n_anomalias = n_anomalias.x, n_nascimentos = n_nascimentos.x)
    
    icons <- awesomeIcons(
      icon = 'stethoscope',
      iconColor = 'black',
      library = 'fa'
    )
    
    mapa <- leaflet(hosp_mapa_aux) %>% addTiles() %>%
      addAwesomeMarkers(~LONGITUDE, ~LATITUDE, icon = icons, layerId = ~codestab,
                        label = ~sprintf("<strong>%s</strong>", nome) %>% lapply(htmltools::HTML))
    
    return(mapa)
  })
  
  observeEvent(c(input$map_marker_click, input$ano_grafico_mapa), { 
    
    req(input$map_marker_click)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    
    dados_hospital <- anom_hosp %>%
      filter(codestab == codestab_id, 
             ANONASC %in% input$ano_grafico_mapa)
    
    if(nrow(dados_hospital) > 0) {
      
      colunas_cids <- paste0("cid_", 1:10)
      
      totais <- sapply(colunas_cids, function(col) {
        if(col %in% names(dados_hospital)) {
          sum(dados_hospital[[col]], na.rm = TRUE)
        } else {
          0
        }
      })
      
      df_fixo <- data.frame(
        Anomalia = cids_values4[1:10], 
        Total = as.numeric(totais),
        stringsAsFactors = FALSE
      )
      
      nasc_total <- nasc_hosp %>%
        filter(CODESTAB == sprintf("%07d", codestab_id), 
               ANONASC %in% input$ano_grafico_mapa) %>%
        summarise(total = sum(n_nascimentos, na.rm = TRUE)) %>%
        pull(total)
      
      df_fixo$n_nascimentos <- if(length(nasc_total) > 0) nasc_total else 0
      df_fixo$nome <- dados_hospital$nome[1]
      
      aux5_pop(df_fixo)
    } else {
      aux5_pop(NULL)
    }
  })
  
  output$mapa_texto <- renderUI({
    if(is.null(input$map_marker_click)){
      h3("Clique em um dos ícones do mapa para obter estatísticas sobre ACs do estabelecimento de saúde.")
    } else {
      req(aux5_pop())
      h2(unique(aux5_pop()$nome))
    }
  })
  
  output$box_nascimentos <- renderValueBox({
    req(input$map_marker_click, aux5_pop())
    valueBox(
      value = unique(aux5_pop()$n_nascimentos),
      subtitle = "Número de nascidos vivos",
      icon = icon("baby"),
      color = "blue"
    )
  })
  
  output$box_numero_casos_hosp <- renderValueBox({
    req(input$map_marker_click, aux5_pop())
    
    selecionados <- as.numeric(input$checkbox_cid_mapa)
    
    dados_filtrados <- aux5_pop()[selecionados, ]
    
    valueBox(
      value = sum(dados_filtrados$Total, na.rm = TRUE),
      subtitle = "Número de anomalias (das selecionadas):",
      icon = icon("notes-medical", lib = "font-awesome"),
      color = "red"
    )
  })
  
  output$box_prevalencia_hosp <- renderValueBox({
    req(input$map_marker_click, aux5_pop())
    
    selecionados <- as.numeric(input$checkbox_cid_mapa)
    dados_filtrados <- aux5_pop()[selecionados, ]
    
    nasc_vivos <- unique(aux5_pop()$n_nascimentos)
    prev <- if(nasc_vivos > 0) sum(dados_filtrados$Total, na.rm = TRUE) / nasc_vivos * 1000 else 0
    
    valueBox(
      value = round(prev, 2),
      subtitle = "Prevalência por 1000 nascidos vivos",
      icon = icon("notes-medical"),
      color = "purple"
    )
  })
  
  output$grafico_barras <- renderHighchart({
    
    req(input$map_marker_click, input$ano_grafico_mapa, input$checkbox_cid_mapa)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    req(!is.na(codestab_id))
    
    dados <- anom_hosp %>%
      filter(
        codestab == codestab_id,
        ANONASC %in% input$ano_grafico_mapa
      )
    
    if(nrow(dados) == 0){
      return(NULL)
    }
    
    colunas_cids <- paste0("cid_", 1:10)
    
    totais <- sapply(colunas_cids, function(col) {
      if(col %in% names(dados)) {
        sum(dados[[col]], na.rm = TRUE)
      } else {
        0
      }
    })
    
    df_plot <- data.frame(
      Anomalia = cids_nomes[1:10],
      Total = as.numeric(totais)
    )
    
    df_plot$Total[is.na(df_plot$Total)] <- 0
    
    df_plot <- df_plot[input$checkbox_cid_mapa, ]
    
    highchart() %>%
      hc_chart(type = "bar", height = 350) %>%   
      hc_title(text = NULL) %>%                  
      hc_xAxis(
        categories = df_plot$Anomalia,
        title = list(text = NULL),
        labels = list(style = list(fontSize = "11px")),
        tickmarkPlacement = "on"
      ) %>%
      hc_yAxis(
        title = list(text = "Número de casos"),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_add_series(
        name = "Casos",
        data = as.list(df_plot$Total),
        color = "#2E86C1"
      ) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 5,
          dataLabels = list(enabled = TRUE)
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = "<b>{point.category}</b><br>Casos: {point.y}"
      ) %>%
      hc_legend(enabled = FALSE)
  })
    
  
  output$mapa_serie <- renderHighchart({
    
    req(input$map_marker_click, input$ano_grafico_mapa)
    codestab_id <- as.numeric(input$map_marker_click$id)
    
    cols_cid <- as.numeric(input$checkbox_cid_mapa) + 5
    
    anos_sel <- as.integer(input$ano_grafico_mapa)
    
    aux <- anom_hosp %>%
      mutate(ANONASC = as.integer(ANONASC)) %>% 
      filter(codestab == codestab_id,
             ANONASC %in% anos_sel) %>%
      select(codestab, nome, ANONASC, all_of(cols_cid))
    
    aux2 <- aux %>%
      group_by(codestab, nome, ANONASC) %>%
      summarise(across(starts_with("cid"), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    
    aux2 <- aux2 %>%
      tidyr::complete(ANONASC = anos_sel) %>%
      mutate(across(starts_with("cid"), ~ tidyr::replace_na(.x, 0)))
    
    aux2 <- aux2 %>%
      mutate(
        codestab = codestab_id,
        nome = unique(aux$nome)[1],
        ANONASC = as.integer(ANONASC)  
      )
    
    nasc_hosp2 <- nasc_hosp %>%
      mutate(ANONASC = as.integer(ANONASC))
    
    aux3 <- aux2 %>%
      left_join(nasc_hosp2, by = c("codestab" = "CODESTAB", "ANONASC"))
    
    aux3 <- aux3 %>%
      filter(ANONASC %in% anos_sel)
    
    aux3 <- aux3 %>%
      mutate(across(starts_with("cid"), 
                    ~ .x / n_nascimentos * 1000))
    
    cids_nomes_codigo <- c(
      "cid_1"  = "Cardiopatias Congênitas",
      "cid_2"  = "Parede Abdominal",
      "cid_3"  = "Redução Membros",
      "cid_4"  = "Tubo Neural",
      "cid_5"  = "Fendas Orais",
      "cid_6"  = "Hipospadia",
      "cid_7"  = "Microcefalia",
      "cid_8"  = "Sexo Indefinido",
      "cid_9"  = "Sindrome Down",
      "cid_10" = "Outras Anomalias"
    )
    
    aux_long <- aux3 %>%
      pivot_longer(
        cols = starts_with("cid"),
        names_to = "anomalia",
        values_to = "valor"
      ) %>%
      mutate(anomalia = cids_nomes_codigo[anomalia])
    
    aux_long <- aux_long %>%
      dplyr::mutate(ANONASC = factor(ANONASC))
    
    hc <- hchart(
      aux_long,
      "line",
      hcaes(x = ANONASC, y = valor, group = anomalia)
    ) %>%
      hc_yAxis(title = list(text = "Prevalência por 1000 nascimentos")) %>%
      hc_xAxis(title = list(text = "Ano")) %>%
      hc_title(text = "Série histórica prevalência por anomalia:", style = list(fontSize = "18px", fontWeight = "bold")) %>%
      hc_tooltip(
        shared = TRUE,
        pointFormat = "<b>{series.name}</b>: {point.y:.2f}<br>"
      )
    
  })
  
  observeEvent(input$eixoyauto, ({
    if(input$eixoyauto){
      shinyjs::disable("eixoy")
    } else {
      shinyjs::enable("eixoy")
    }
  }))
  
  observeEvent(input$variavel, ({
    if(input$variavel == 1){
      updateSliderInput(inputId = "eixoy", min = 0, max = 200, value = c(0, 200))
    } else if(input$variavel == 2){
      updateSliderInput(inputId = "eixoy", min = 0, max = 60, value = c(0, 60))
    } else if(input$variavel == 3){
      updateSliderInput(inputId = "eixoy", min = 0, max = 5000, value = c(0, 5000))
    } 
  }))
  
  
  
  output$grafico_previsao <- renderHighchart({
    
    req(input$map_marker_click)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    colunas_cids <- paste0("cid_", input$checkbox_cid_mapa)
    
    aux <- anom_hosp %>%
      filter(codestab == codestab_id) %>%
      select(codestab, nome, ANONASC, all_of(colunas_cids))
    
    if(length(colunas_cids) > 1){
      aux$n_anomalias <- apply(aux[, colunas_cids], 1, any)
    } else {
      aux$n_anomalias <- as.numeric(aux[[colunas_cids]])
    }
    
    aux2 <- aux %>%
      group_by(codestab, nome, ANONASC) %>%
      summarise(n_anomalias = sum(n_anomalias)) %>%
      ungroup()
    
    aux3 <- aux2 %>%
      left_join(nasc_hosp, by = c("codestab" = "CODESTAB", "ANONASC")) %>%
      select(codestab, nome, ANONASC, n_anomalias = n_anomalias.x, n_nascimentos) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux3 <- aux3 %>% arrange(ANONASC)
    
    aux3 <- aux3 %>%
      complete(ANONASC = full_seq(ANONASC, 1),
               fill = list(n_anomalias = 0))
    
    nascimentos <- ts(aux3$n_anomalias,
                      start = min(aux3$ANONASC))
    
    modelo <- ets(nascimentos, "ANN")
    
    x <- forecast(modelo, level = c(95, 80))
    df <- fortify(x) %>%
      filter(Index %in% 2010:2029)
    
    highchart(type = "stock") %>% 
      hc_title(text = "Previsão para os anos de 2026 até 2029 do número de nascimentos com as ACs selecionadas:",
              style = list(fontSize = "18px", fontWeight = "bold"))  %>%
      hc_add_series(df, "line", hcaes(x = Index, Data), name = "Observado", color = "blue") %>% 
      hc_add_series(df, "scatter", hcaes(x = Index, Data), name = "Observado", color = "blue") %>% 
      hc_add_series(df, "arearange", hcaes(Index, low = round(`Lo 95`, 3), high = round(`Hi 95`, 3)), name = "Intervalo de 95% da predição", color = "cyan", alpha = 0.25) %>%
      hc_add_series(df, "arearange", hcaes(Index, low = round(`Lo 80`, 3), high = round(`Hi 80`, 3)), name = "Intervalo de 80% da predição", color = "blue", alpha = 0.25) %>%
      hc_add_series(df, "line", hcaes(Index, round(`Point Forecast`, 3)), name = "Previsão", color = "black") %>% 
      hc_xAxis(labels = list(format = '{value:%f}'))
  })
  
  
  
  output$sexo <- renderHighchart({
    
    req(input$map_marker_click, input$ano_grafico_mapa)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    req(!is.na(codestab_id))
    
    dados <- dados_caracteristicas_hosp_sel %>%
      filter(
        CODESTAB == codestab_id,
        ANONASC %in% input$ano_grafico_mapa
      )
    
    if(nrow(dados) == 0){
      return(NULL)
    }
    
    df_plot <- dados %>%
      mutate(
        sexo_cat = case_when(
          SEXO %in% c(1, "1") ~ "Masculino",
          SEXO %in% c(2, "2") ~ "Feminino",
          SEXO %in% c(0, "0") ~ "Ignorado",
          is.na(SEXO) ~ "Sem informação",
          TRUE ~ "Sem informação"  
        )
      ) %>%
      group_by(sexo_cat) %>%
      summarise(Total = n(), .groups = "drop") %>%
      mutate(
        Perc = round(100 * Total / sum(Total), 1),
        sexo_cat = factor(sexo_cat, 
                          levels = c("Masculino", "Feminino", "Ignorado", "Sem informação"))
      ) %>%
      arrange(sexo_cat) 
    
    highchart() %>%
      hc_chart(type = "bar", height = 350) %>%   
      hc_title(text = "Sexo:",
               style = list(fontSize = "14px", fontWeight = "bold"))  %>%                  
      hc_xAxis(
        categories = df_plot$sexo_cat,  
        title = list(text = NULL),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Número de nascimentos"),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_add_series(
        name = "Casos",
        data = purrr::map2(df_plot$Total, df_plot$Perc, function(total, perc) {
          list(y = total, perc = perc)
        }), 
        color = "#e87026"
      ) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 5,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("
        function() {
          return this.y + ' (' + this.point.perc + '%)';
        }
      "))
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = "<b>{point.category}</b><br>N°: {point.y} ({point.perc}%)"
      ) %>%
      hc_legend(enabled = FALSE)
  })
  
  
  
  output$raca_bebe <- renderHighchart({
    
    req(input$map_marker_click, input$ano_grafico_mapa)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    req(!is.na(codestab_id))
    
    dados <- dados_caracteristicas_hosp_sel %>%
      filter(
        CODESTAB == codestab_id,
        ANONASC %in% input$ano_grafico_mapa
      )
    
    if(nrow(dados) == 0){
      return(NULL)
    }
    
    df_plot <- dados %>%
      mutate(
        raca_bebe_cat = case_when(
          RACACOR %in% c(1, "1") ~ "Branca",
          RACACOR %in% c(2, "2") ~ "Preta",
          RACACOR %in% c(3, "3") ~ "Amarela",
          RACACOR %in% c(4, "4") ~ "Parda",
          RACACOR %in% c(5, "5") ~ "Indígena",
          is.na(RACACOR) ~ "Sem informação",
          TRUE ~ "Sem informação"  
        )
      ) %>%
      group_by(raca_bebe_cat) %>%
      summarise(Total = n(), .groups = "drop") %>%
      mutate(
        Perc = round(100 * Total / sum(Total), 1),
        raca_bebe_cat = factor(raca_bebe_cat, 
                          levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena",  "Sem informação"))
      ) %>%
      arrange(raca_bebe_cat) 
    
    highchart() %>%
      hc_chart(type = "bar", height = 350) %>%   
      hc_title(text = "Raça/Cor:",
               style = list(fontSize = "14px", fontWeight = "bold"))  %>%                  
      hc_xAxis(
        categories = df_plot$raca_bebe_cat,  
        title = list(text = NULL),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Número de nascimentos"),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_add_series(
        name = "Casos",
        data = purrr::map2(df_plot$Total, df_plot$Perc, function(total, perc) {
          list(y = total, perc = perc)
        }), 
        color = "#e87026"
      ) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 5,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("
        function() {
          return this.y + ' (' + this.point.perc + '%)';
        }
      "))
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = "<b>{point.category}</b><br>N°: {point.y} ({point.perc}%)"
      ) %>%
      hc_legend(enabled = FALSE)
  })
  
  output$peso <- renderHighchart({
    
    req(input$map_marker_click, input$ano_grafico_mapa)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    req(!is.na(codestab_id))
    
    dados <- dados_caracteristicas_hosp_sel %>%
      filter(
        CODESTAB == codestab_id,
        ANONASC %in% input$ano_grafico_mapa
      ) 
    
    if(nrow(dados) == 0){
      return(NULL)
    }
    
    media_peso <- round(mean(dados$PESO), 1)
    
    hist_data <- hist(dados$PESO, plot = FALSE, breaks = 20)
    
    df_hist <- data.frame(
      xmin = hist_data$breaks[-length(hist_data$breaks)],
      xmax = hist_data$breaks[-1],
      count = hist_data$counts
    )
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(
        text = "Peso recém-nascido:",
        style = list(fontSize = "14px", fontWeight = "bold")
      ) %>%
      hc_subtitle(
        text = paste0("Peso médio: ", media_peso, " gramas")
      ) %>%
      hc_xAxis(
        title = list(text = "Peso"),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Número de nascimentos")
      ) %>%
      hc_add_series(
        name = "Frequência",
        data = purrr::pmap(
          list(df_hist$xmin, df_hist$xmax, df_hist$count),
          function(xmin, xmax, count){
            list(x = (xmin + xmax)/2, y = count)
          }
        ),
        color = "#e87026"
      ) %>%
      hc_plotOptions(
        column = list(
          pointPadding = 0,
          groupPadding = 0,
          borderWidth = 0
        )
      ) %>%
      hc_tooltip(
        pointFormat = "N°: {point.y}"
      ) %>%
      
      hc_legend(enabled = FALSE)
  })
  
  
  
  output$idade_mae <- renderHighchart({
    
    req(input$map_marker_click, input$ano_grafico_mapa)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    req(!is.na(codestab_id))
    
    dados <- dados_caracteristicas_hosp_sel %>%
      filter(
        CODESTAB == codestab_id,
        ANONASC %in% input$ano_grafico_mapa
      ) 
    
    if(nrow(dados) == 0){
      return(NULL)
    }
    
    media_idade <- round(mean(dados$IDADEMAE), 1)
    
    hist_data <- hist(dados$IDADEMAE, plot = FALSE, breaks = 20)
    
    df_hist <- data.frame(
      xmin = hist_data$breaks[-length(hist_data$breaks)],
      xmax = hist_data$breaks[-1],
      count = hist_data$counts
    )
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(
        text = "Idade materna:",
        style = list(fontSize = "14px", fontWeight = "bold")
      ) %>%
      hc_subtitle(
        text = paste0("Idade média: ", media_idade, " anos")
      ) %>%
      hc_xAxis(
        title = list(text = "Idade"),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Número de nascimentos")
      ) %>%
      hc_add_series(
        name = "Frequência",
        data = purrr::pmap(
          list(df_hist$xmin, df_hist$xmax, df_hist$count),
          function(xmin, xmax, count){
            list(x = (xmin + xmax)/2, y = count)
          }
        ),
        color = "#4b3080"
      ) %>%
      hc_plotOptions(
        column = list(
          pointPadding = 0,
          groupPadding = 0,
          borderWidth = 0
        )
      ) %>%
      hc_tooltip(
        pointFormat = "N°: {point.y}"
      ) %>%
      
      hc_legend(enabled = FALSE)
  })
  
  output$raca_mae <- renderHighchart({
    
    req(input$map_marker_click, input$ano_grafico_mapa)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    req(!is.na(codestab_id))
    
    dados <- dados_caracteristicas_hosp_sel %>%
      filter(
        CODESTAB == codestab_id,
        ANONASC %in% input$ano_grafico_mapa
      )
    
    if(nrow(dados) == 0){
      return(NULL)
    }
    
    df_plot <- dados %>%
      mutate(
        raca_mae_cat = case_when(
          RACACORMAE %in% c(1, "1") ~ "Branca",
          RACACORMAE %in% c(2, "2") ~ "Preta",
          RACACORMAE %in% c(3, "3") ~ "Amarela",
          RACACORMAE %in% c(4, "4") ~ "Parda",
          RACACORMAE %in% c(5, "5") ~ "Indígena",
          is.na(RACACORMAE) ~ "Sem informação",
          TRUE ~ "Sem informação"  
        )
      ) %>%
      group_by(raca_mae_cat) %>%
      summarise(Total = n(), .groups = "drop") %>%
      mutate(
        Perc = round(100 * Total / sum(Total), 1),
        raca_mae_cat = factor(raca_mae_cat, 
                               levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena",  "Sem informação"))
      ) %>%
      arrange(raca_mae_cat) 
    
    highchart() %>%
      hc_chart(type = "bar", height = 350) %>%   
      hc_title(text = "Raça/Cor:",
               style = list(fontSize = "14px", fontWeight = "bold"))  %>%                  
      hc_xAxis(
        categories = df_plot$raca_mae_cat,  
        title = list(text = NULL),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Número de nascimentos"),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_add_series(
        name = "Casos",
        data = purrr::map2(df_plot$Total, df_plot$Perc, function(total, perc) {
          list(y = total, perc = perc)
        }), 
        color = "#4b3080"
      ) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 5,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("
        function() {
          return this.y + ' (' + this.point.perc + '%)';
        }
      "))
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = "<b>{point.category}</b><br>N°: {point.y} ({point.perc}%)"
      ) %>%
      hc_legend(enabled = FALSE)
  })
  
  
  output$esco_mae <- renderHighchart({
    
    req(input$map_marker_click, input$ano_grafico_mapa)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    req(!is.na(codestab_id))
    
    dados <- dados_caracteristicas_hosp_sel %>%
      filter(
        CODESTAB == codestab_id,
        ANONASC %in% input$ano_grafico_mapa
      )
    
    if(nrow(dados) == 0){
      return(NULL)
    }
    
    df_plot <- dados %>%
      mutate(
        esco_cat = case_when(
          ESCMAE %in% c(1, "1") ~ "Nenhuma",
          ESCMAE %in% c(2, "2") ~ "1 a 3 anos",
          ESCMAE %in% c(3, "3") ~ "4 a 7 anos",
          ESCMAE %in% c(4, "4") ~ "8 a 11 anos",
          ESCMAE %in% c(5, "5") ~ "12 e mais",
          ESCMAE %in% c(9, "9") ~ "Ignorado",
          is.na(ESCMAE) ~ "Sem informação",
          TRUE ~ "Sem informação"  
        )
      ) %>%
      group_by(esco_cat) %>%
      summarise(Total = n(), .groups = "drop") %>%
      mutate(
        Perc = round(100 * Total / sum(Total), 1),
        esco_cat  = factor(esco_cat , 
                               levels = c("Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 e mais",  "Ignorado", "Sem informação"))
      ) %>%
      arrange(esco_cat) 
    
    highchart() %>%
      hc_chart(type = "bar", height = 350) %>%   
      hc_title(text = "Escolaridade:",
               style = list(fontSize = "14px", fontWeight = "bold"))  %>%                  
      hc_xAxis(
        categories = df_plot$esco_cat,  
        title = list(text = NULL),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Número de nascimentos"),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_add_series(
        name = "Casos",
        data = purrr::map2(df_plot$Total, df_plot$Perc, function(total, perc) {
          list(y = total, perc = perc)
        }), 
        color = "#4b3080"
      ) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 5,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("
        function() {
          return this.y + ' (' + this.point.perc + '%)';
        }
      "))
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = "<b>{point.category}</b><br>N°: {point.y} ({point.perc}%)"
      ) %>%
      hc_legend(enabled = FALSE)
  })
  
  
  output$semana_gesta <- renderHighchart({
    
    req(input$map_marker_click, input$ano_grafico_mapa)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    req(!is.na(codestab_id))
    
    dados <- dados_caracteristicas_hosp_sel %>%
      filter(
        CODESTAB == codestab_id,
        ANONASC %in% input$ano_grafico_mapa
      )
    
    if(nrow(dados) == 0){
      return(NULL)
    }
    
    df_plot <- dados %>%
      mutate(
        gestacao_cat = case_when(
          GESTACAO %in% c(1, "1") ~ "Menos 22 semanas",
          GESTACAO %in% c(2, "2") ~ "22 a 27 semanas",
          GESTACAO %in% c(3, "3") ~ "28 a 31 semanas",
          GESTACAO %in% c(4, "4") ~ "32 a 36 semanas",
          GESTACAO %in% c(5, "5") ~ "37 a 41 semanas",
          GESTACAO %in% c(6, "6") ~ "42 semanas e mais",
          GESTACAO %in% c(9, "9") ~ "Ignorado",
          is.na(GESTACAO) ~ "Sem informação",
          TRUE ~ "Sem informação"  
        )
      ) %>%
      group_by(gestacao_cat) %>%
      summarise(Total = n(), .groups = "drop") %>%
      mutate(
        Perc = round(100 * Total / sum(Total), 1),
        gestacao_cat = factor(gestacao_cat, 
                              levels = c("Menos 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais", "Ignorado", "Sem informação"))
      ) %>%
      arrange(gestacao_cat) 
    
    highchart() %>%
      hc_chart(type = "bar", height = 350) %>%   
      hc_title(text = "Semanas de gestação:",
               style = list(fontSize = "14px", fontWeight = "bold"))  %>%                  
      hc_xAxis(
        categories = df_plot$gestacao_cat,  
        title = list(text = NULL),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Número de nascimentos"),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_add_series(
        name = "Casos",
        data = purrr::map2(df_plot$Total, df_plot$Perc, function(total, perc) {
          list(y = total, perc = perc)
        }), 
        color = "darkblue"
      ) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 5,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("
        function() {
          return this.y + ' (' + this.point.perc + '%)';
        }
      "))
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = "<b>{point.category}</b><br>N°: {point.y} ({point.perc}%)"
      ) %>%
      hc_legend(enabled = FALSE)
  })
  
  
  output$tipo_parto <- renderHighchart({
    
    req(input$map_marker_click, input$ano_grafico_mapa)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    req(!is.na(codestab_id))
    
    dados <- dados_caracteristicas_hosp_sel %>%
      filter(
        CODESTAB == codestab_id,
        ANONASC %in% input$ano_grafico_mapa
      )
    
    if(nrow(dados) == 0){
      return(NULL)
    }
    
    df_plot <- dados %>%
      mutate(
        tipo_parto_cat = case_when(
          PARTO %in% c(1, "1") ~ "Vaginal",
          PARTO %in% c(2, "2") ~ "Cesário",
          PARTO %in% c(9, "9") ~ "Ignorado",
          is.na(PARTO) ~ "Sem informação",
          TRUE ~ "Sem informação"  
        )
      ) %>%
      group_by(tipo_parto_cat) %>%
      summarise(Total = n(), .groups = "drop") %>%
      mutate(
        Perc = round(100 * Total / sum(Total), 1),
        tipo_parto_cat = factor(tipo_parto_cat, 
                              levels = c("Vaginal", "Cesário", "Ignorado", "Sem informação"))
      ) %>%
      arrange(tipo_parto_cat) 
    
    highchart() %>%
      hc_chart(type = "bar", height = 350) %>%   
      hc_title(text = "Tipo parto:",
               style = list(fontSize = "14px", fontWeight = "bold"))  %>%                  
      hc_xAxis(
        categories = df_plot$tipo_parto_cat,  
        title = list(text = NULL),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Número de nascimentos"),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_add_series(
        name = "Casos",
        data = purrr::map2(df_plot$Total, df_plot$Perc, function(total, perc) {
          list(y = total, perc = perc)
        }), 
        color = "darkblue"
      ) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 5,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("
        function() {
          return this.y + ' (' + this.point.perc + '%)';
        }
      "))
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = "<b>{point.category}</b><br>N°: {point.y} ({point.perc}%)"
      ) %>%
      hc_legend(enabled = FALSE)
  })
  
  output$preencheu <- renderHighchart({
    
    req(input$map_marker_click, input$ano_grafico_mapa)
    
    codestab_id <- as.numeric(input$map_marker_click$id)
    req(!is.na(codestab_id))
    
    dados <- dados_responsaveis_hosp_sel %>%
      filter(
        CODESTAB == codestab_id,
        ANONASC %in% input$ano_grafico_mapa
      )
    
    if(nrow(dados) == 0){
      return(NULL)
    }
    
    df_plot <- dados %>%
      mutate(
        preencheu_cat = case_when(
          TPFUNCRESP %in% c(1, "1") ~ "Médico",
          TPFUNCRESP %in% c(2, "2") ~ "Enfermeiro",
          TPFUNCRESP %in% c(3, "3") ~ "Parteira",
          TPFUNCRESP %in% c(4, "4") ~ "Funcionário do cartório",
          TPFUNCRESP %in% c(5, "5") ~ "Outros",
          is.na(TPFUNCRESP) ~ "Sem informação",
          TRUE ~ "Sem informação"  
        )
      ) %>%
      group_by(preencheu_cat) %>%
      summarise(Total = n(), .groups = "drop") %>%
      mutate(
        Perc = round(100 * Total / sum(Total), 1),
        preencheu_cat= factor(preencheu_cat, 
                              levels = c("Médico", "Enfermeiro", "Parteira","Funcionário do cartório", "Outros", "Sem informação"))
      ) %>%
      arrange(preencheu_cat) 
    
    highchart() %>%
      hc_chart(type = "bar", height = 350) %>%   
      hc_title(text = "Responsável pelo preenchimento da DN:",
               style = list(fontSize = "14px", fontWeight = "bold"))  %>%  
      hc_subtitle(
        text = paste0("Informação disponível apenas para os dados de 2014 a 2025")
      ) %>%
      hc_xAxis(
        categories = df_plot$preencheu_cat,  
        title = list(text = NULL),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_yAxis(
        title = list(text = "Número de nascimentos"),
        labels = list(style = list(fontSize = "11px"))
      ) %>%
      hc_add_series(
        name = "Casos",
        data = purrr::map2(df_plot$Total, df_plot$Perc, function(total, perc) {
          list(y = total, perc = perc)
        }), 
        color = "darkblue"
      ) %>%
      hc_plotOptions(
        bar = list(
          borderRadius = 5,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("
        function() {
          return this.y + ' (' + this.point.perc + '%)';
        }
      "))
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = "<b>{point.category}</b><br>N°: {point.y} ({point.perc}%)"
      ) %>%
      hc_legend(enabled = FALSE)
  })
  ######################## ABA ESTATÍSTICAS DESCRITIVAS ########################
  
  output$box_plot <- renderPlotly({
    req(input$ano_hospitais)
    
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_hospitais) %>%
      select(1:5, 16, as.numeric(input$checkbox_cid) + 5)
    
    if(ncol(aux) > 7){
      aux$n_anomalias <- rowSums(aux[,7:ncol(aux)], na.rm = TRUE)
    } else {
      aux$n_anomalias <- as.numeric(aux[,7])
    }
    
    aux2 <- aux %>%
      group_by(codestab, nome, ANONASC) %>%
      summarise(n_anomalias = sum(n_anomalias)) %>%
      ungroup()
    
    aux3 <- aux2 %>%
      left_join(nasc_hosp, by = c("codestab" = "CODESTAB", "ANONASC")) %>%
      select(codestab, nome, ANONASC, n_anomalias = n_anomalias.x, n_nascimentos) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000,
             ANONASC = as.factor(ANONASC))
    
    names(aux3)[which(names(aux3) == variavel_aux[as.numeric(input$variavel)])] <- "variavel"
    
    aux3 %>%
      plot_ly(
        x = ~ANONASC,
        y = ~variavel,
        type = "box",
        marker = list(color = 'rgba(139,0,139,0.75)'),
        color = I("rgba(139,0,139,1)"),
        text = ~sprintf("%s <br>Ano: %s<br>%s: %s ", nome, ANONASC, variavel2[as.numeric(input$variavel)], round(variavel, 2)),
        boxpoints = "all"
      ) %>%
      layout(
        xaxis = list(
          title = "Ano", 
          tickmode = "linear",
          dtick = 1
        ),
        yaxis = list(
          title = variavel2[as.numeric(input$variavel)] 
        )
      )
  })
  

  dados_top10 <- reactive({
    req(input$variavel, input$botao_POA_heat, input$ano_hospitais)
    
    anom_hosp2 = anom_hosp %>%
      filter(codestab != "2262568")
    
    df <- anom_hosp2
    if(input$botao_POA_heat == "Sem considerar POA") {
      df <- df %>% filter(CODMUNNASC != 431490)
    }
    
    aux <- df %>%
      filter(ANONASC %in% input$ano_hospitais) %>%
      select(1:5, 16, as.numeric(input$checkbox_cid) + 5)
    
    if(ncol(aux) > 7){
      aux$n_anomalias <- apply(aux[,7:ncol(aux)], 1, any) 
    } else {
      aux$n_anomalias <- as.numeric(aux[,7])
    }
    
    aux2 <- aux %>%
      group_by(codestab, nome, ANONASC) %>%
      summarise(n_anomalias = sum(n_anomalias, na.rm = TRUE)) %>%
      ungroup()
    
    aux3 <- aux2 %>%
      left_join(nasc_hosp, by = c("codestab" = "CODESTAB", "ANONASC")) %>%
      select(codestab, nome, ANONASC, n_anomalias = n_anomalias.x, n_nascimentos) %>%
      mutate(prevalencia = (n_anomalias / n_nascimentos) * 1000)
    
    names(aux3)[which(names(aux3) == variavel_aux[as.numeric(input$variavel)])] <- "variavel_selecionada"
    
    top_10_hosp <- aux3 %>%
      group_by(nome) %>%
      summarise(total_historico = sum(variavel_selecionada, na.rm = TRUE)) %>%
      arrange(desc(total_historico)) %>%
      slice(1:16) %>%
      pull(nome)
    
    aux3 %>% filter(nome %in% top_10_hosp)
  })
  
  output$heatmap_top10 <- renderHighchart({
    req(dados_top10())
    df <- dados_top10()
    
    hchart(df, "heatmap", hcaes(x = as.factor(ANONASC), y = nome, value = round(variavel_selecionada, 2))) %>%
      hc_colorAxis(stops = color_stops(10, viridis::viridis(10))) %>%
      hc_title(text = paste("Heatmap de", variavel2[as.numeric(input$variavel)])) %>%
      hc_xAxis(title = list(text = "Ano")) %>%
      hc_yAxis(title = list(text = "Hospital")) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return '<b>Hospital:</b> ' + this.series.yAxis.categories[this.point.y] + '<br>' +",
          "'<b>Ano:</b> ' + this.series.xAxis.categories[this.point.x] + '<br>' +",
          "'<b>", variavel2[as.numeric(input$variavel)], ":</b> ' + this.point.value;",
          "}"
        ))
      )
  })
  

  dados_barra <- reactive({
    req(input$variavel, input$botao_POA_barra, input$ano_hospitais)
    
    df <- anom_hosp
    if(input$botao_POA_barra == "Sem considerar POA") {
      df <- df %>% filter(CODMUNNASC != 431490)
    }
    
    selecao_cids <- paste0("cid_", input$checkbox_cid)
    
    aux <- df %>% 
      filter(ANONASC %in% input$ano_hospitais)
    
    if(length(selecao_cids) > 1){
      aux$houve_anomalia <- apply(aux[, selecao_cids, drop = FALSE], 1, any)
    } else {
      aux$houve_anomalia <- as.numeric(aux[[selecao_cids]])
    }
    
    aux_anom_sum <- aux %>%
      group_by(codestab, nome) %>%
      summarise(n_anomalias = sum(houve_anomalia, na.rm = TRUE), .groups = "drop")
    
    aux_nasc_sum <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_hospitais) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        cod_mun = first(CODMUNNASC) 
      ) %>%
      ungroup()
    
    df_plot <- aux_anom_sum %>%
      inner_join(aux_nasc_sum, by = c("codestab" = "CODESTAB")) %>%
      mutate(
        prevalencia = (n_anomalias / n_nascimentos) * 1000
      )
    
    var_nome <- variavel_aux[as.numeric(input$variavel)]
    df_plot$valor <- df_plot[[var_nome]]
    
    df_plot %>%
      arrange(desc(valor)) %>%
      slice_head(n = 15)
  })
  
  
  output$barras_top10_ano <- renderHighchart({
    df <- dados_barra()
    req(nrow(df) > 0)
    
    hchart(df, "bar", hcaes(x = nome, y = round(valor, 2))) %>%
      hc_colors("steelblue") %>%
      hc_title(text = paste("15 hospitais com maior ",variavel2[as.numeric(input$variavel)])) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = variavel2[as.numeric(input$variavel)])) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return '<b>", variavel2[as.numeric(input$variavel)], ":</b> ' + this.point.y;",
          "}"
        ))
      )
  })
  
  ######################## TABELAS E DOWNLOADS ########################
  dados_tabela_processados <- reactive({
    selecao <- input$checkbox_cid_mapa
    req(length(selecao) > 0)
    cids_selecionadas <- paste0("cid_", selecao)
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_grafico_mapa) %>% 
      group_by(codestab, nome) %>%
      summarise(across(all_of(cids_selecionadas), sum, na.rm = TRUE), .groups = "drop")
    
    aux2 <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_grafico_mapa) %>%
      group_by(CODESTAB) %>%
      summarise(n_nascimentos = sum(n_nascimentos, na.rm = TRUE), .groups = "drop")
    
    resultado <- aux %>%
      left_join(aux2, by = c("codestab" = "CODESTAB")) %>%
      select(codestab, nome, n_nascimentos, all_of(cids_selecionadas)) %>%
      mutate(across(all_of(cids_selecionadas), 
                    ~ paste0(round(. / n_nascimentos * 1000, 2), " (", ., ")")))
    
    indices <- as.numeric(selecao)
    nomes_amigaveis <- cids_nomes[indices]
    
    colnames(resultado) <- c("CNE", "Nome do Hospital", "Total Nascimentos", 
                             paste0(nomes_amigaveis, " Prevalência (nº)"))
    
    return(resultado)
  })
  
  
  output$tabela1 <- renderDataTable({
    datatable(dados_tabela_processados(), 
              rownames = FALSE, 
              options = list(scrollX = TRUE, rowCallback = JS(rowCallback)))
  })
  
  
  output$download_tabela1 <- downloadHandler(
    filename = function() {
      paste("banco_hospital_filtrado_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dados_tabela_processados(), file, row.names = FALSE, fileEncoding = "latin1")
    }
  )
  
  
  dados_tabela2_processados <- reactive({
    req(input$hosp_selec, input$ano_hospitais, input$checkbox_cid)
    
    selecao_cids <- paste0("cid_", input$checkbox_cid)
    
    aux <- anom_hosp %>%
      filter(nome %in% input$hosp_selec,
             ANONASC %in% input$ano_hospitais) %>%
      select(codestab, nome, all_of(selecao_cids))
    
    if(length(selecao_cids) > 1) {
      aux$houve_anomalia <- apply(aux[, selecao_cids, drop = FALSE], 1, any)
    } else {
      aux$houve_anomalia <- as.numeric(aux[[selecao_cids]])
    }
    
    aux_anom_agregado <- aux %>%
      group_by(codestab, nome) %>%
      summarise(total_anomalias = sum(houve_anomalia, na.rm = TRUE), .groups = "drop")
    
    aux_nasc_agregado <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_hospitais) %>%
      group_by(CODESTAB) %>%
      summarise(total_nascimentos = sum(n_nascimentos, na.rm = TRUE), .groups = "drop")
    
    
    resultado <- aux_anom_agregado %>%
      left_join(aux_nasc_agregado, by = c("codestab" = "CODESTAB")) %>%
      transmute(
        CNE = codestab,
        `Nome Hospital` = nome,
        `Total nascimentos` = total_nascimentos,
        `N° nascimentos com anomalias` = total_anomalias,
        `Prevalência 1000 nascimentos` = round((total_anomalias / total_nascimentos) * 1000, 2)
      )
    
    return(resultado)
  })
  
  output$tabela2 <- renderDataTable({
    datatable(dados_tabela2_processados(), 
              rownames = FALSE,
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                rowCallback = JS(rowCallback)
              ))
  })
  
  output$download_tabela2 <- downloadHandler(
    filename = function() {
      paste("banco_hosp_ac_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dados_tabela2_processados(), file, row.names = FALSE, fileEncoding = "latin1")
    }
  )
  
  
  ################ ABA COMPARAÇÃO ANOMALIAS #################
  
  # cardiopatias
  output$barras_cc_top10 <- renderHighchart({
    
    req(input$ano_anomalias, input$variavel_3)
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      select(codestab, nome, ANONASC, cid_1)
    
    aux2 <- aux %>%
      group_by(codestab, nome) %>%
      summarise(
        n_anomalias = sum(cid_1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codestab = sprintf("%07d", as.numeric(codestab)))
    
    nasc_total <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(CODESTAB = sprintf("%07d", as.numeric(CODESTAB)))
    
    aux_final <- aux2 %>%
      left_join(nasc_total, by = c("codestab" = "CODESTAB")) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux_final <- aux_final %>%
      left_join(
        hosp_mapa %>%
          mutate(CODESTAB_c = sprintf("%07d", as.numeric(CODESTAB))) %>%
          select(CODESTAB_c, MUNICIPIO),
        by = c("codestab" = "CODESTAB_c")
      )
    

    if(input$botao_POA_cc == "Sem considerar POA"){
      aux_final <- aux_final %>%
        filter(MUNICIPIO != "PORTO ALEGRE")
    }
    
   
    if(input$variavel_3 == 1){
      aux_final <- aux_final %>%
        mutate(valor = n_anomalias)
      titulo_y <- "Número de casos"
    } else {
      aux_final <- aux_final %>%
        mutate(valor = prevalencia)
      titulo_y <- "Prevalência por 1000 nascimentos"
    }
    

    top15 <- aux_final %>%
      arrange(desc(valor)) %>%
      slice_head(n = 20)
    
    hchart(top15, "bar", hcaes(x = nome, y = round(valor, 2))) %>%
      hc_colors("steelblue") %>%
      hc_title(text = paste("20 hospitais com maior ",variavel2[as.numeric(input$variavel_3)], "Cardiopatias congênitas")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = variavel2[as.numeric(input$variavel_3)])) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return  this.point.nome + '<br>' +",
          "'<b>", variavel2[as.numeric(input$variavel_3)], ":</b> ' + this.point.y;",
          "}"
        ))
      )
  })

  
  # defeito de parede abdminal
  output$barras_dpa_top10 <- renderHighchart({
    
    req(input$ano_anomalias, input$variavel_3)
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      select(codestab, nome, ANONASC, cid_2)
    
    aux2 <- aux %>%
      group_by(codestab, nome) %>%
      summarise(
        n_anomalias = sum(cid_2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codestab = sprintf("%07d", as.numeric(codestab)))
    
    nasc_total <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(CODESTAB = sprintf("%07d", as.numeric(CODESTAB)))
    
    aux_final <- aux2 %>%
      left_join(nasc_total, by = c("codestab" = "CODESTAB")) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux_final <- aux_final %>%
      left_join(
        hosp_mapa %>%
          mutate(CODESTAB_c = sprintf("%07d", as.numeric(CODESTAB))) %>%
          select(CODESTAB_c, MUNICIPIO),
        by = c("codestab" = "CODESTAB_c")
      )
    
    
    if(input$botao_POA_dpa == "Sem considerar POA"){
      aux_final <- aux_final %>%
        filter(MUNICIPIO != "PORTO ALEGRE")
    }
    
    
    if(input$variavel_3 == 1){
      aux_final <- aux_final %>%
        mutate(valor = n_anomalias)
      titulo_y <- "Número de casos"
    } else {
      aux_final <- aux_final %>%
        mutate(valor = prevalencia)
      titulo_y <- "Prevalência por 1000 nascimentos"
    }
    
    
    top15 <- aux_final %>%
      arrange(desc(valor)) %>%
      slice_head(n = 20)
    
    hchart(top15, "bar", hcaes(x = nome, y = round(valor, 2))) %>%
      hc_colors("darkblue") %>%
      hc_title(text = paste("20 hospitais com maior ",variavel2[as.numeric(input$variavel_3)], "Defeitos de parede abdominal")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = variavel2[as.numeric(input$variavel_3)])) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return  this.point.nome + '<br>' +",
          "'<b>", variavel2[as.numeric(input$variavel_3)], ":</b> ' + this.point.y;",
          "}"
        ))
      )
  })
  
  
  # redução membros
  output$barras_membros_top10 <- renderHighchart({
    
    req(input$ano_anomalias, input$variavel_3)
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      select(codestab, nome, ANONASC, cid_3)
    
    aux2 <- aux %>%
      group_by(codestab, nome) %>%
      summarise(
        n_anomalias = sum(cid_3, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codestab = sprintf("%07d", as.numeric(codestab)))
    
    nasc_total <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(CODESTAB = sprintf("%07d", as.numeric(CODESTAB)))
    
    aux_final <- aux2 %>%
      left_join(nasc_total, by = c("codestab" = "CODESTAB")) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux_final <- aux_final %>%
      left_join(
        hosp_mapa %>%
          mutate(CODESTAB_c = sprintf("%07d", as.numeric(CODESTAB))) %>%
          select(CODESTAB_c, MUNICIPIO),
        by = c("codestab" = "CODESTAB_c")
      )
    
    
    if(input$botao_POA_membros == "Sem considerar POA"){
      aux_final <- aux_final %>%
        filter(MUNICIPIO != "PORTO ALEGRE")
    }
    
    
    if(input$variavel_3 == 1){
      aux_final <- aux_final %>%
        mutate(valor = n_anomalias)
      titulo_y <- "Número de casos"
    } else {
      aux_final <- aux_final %>%
        mutate(valor = prevalencia)
      titulo_y <- "Prevalência por 1000 nascimentos"
    }
    
    
    top15 <- aux_final %>%
      arrange(desc(valor)) %>%
      slice_head(n = 20)
    
    hchart(top15, "bar", hcaes(x = nome, y = round(valor, 2))) %>%
      hc_colors("steelblue") %>%
      hc_title(text = paste("20 hospitais com maior ",variavel2[as.numeric(input$variavel_3)], "Defeitos de redução de membros")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = variavel2[as.numeric(input$variavel_3)])) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return  this.point.nome + '<br>' +",
          "'<b>", variavel2[as.numeric(input$variavel_3)], ":</b> ' + this.point.y;",
          "}"
        ))
      )
  })
  
  
  
  # tubo neural
  output$barras_tubo_top10 <- renderHighchart({
    
    req(input$ano_anomalias, input$variavel_3)
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      select(codestab, nome, ANONASC, cid_4)
    
    aux2 <- aux %>%
      group_by(codestab, nome) %>%
      summarise(
        n_anomalias = sum(cid_4, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codestab = sprintf("%07d", as.numeric(codestab)))
    
    nasc_total <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(CODESTAB = sprintf("%07d", as.numeric(CODESTAB)))
    
    aux_final <- aux2 %>%
      left_join(nasc_total, by = c("codestab" = "CODESTAB")) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux_final <- aux_final %>%
      left_join(
        hosp_mapa %>%
          mutate(CODESTAB_c = sprintf("%07d", as.numeric(CODESTAB))) %>%
          select(CODESTAB_c, MUNICIPIO),
        by = c("codestab" = "CODESTAB_c")
      )
    
    
    if(input$botao_POA_tubo == "Sem considerar POA"){
      aux_final <- aux_final %>%
        filter(MUNICIPIO != "PORTO ALEGRE")
    }
    
    
    if(input$variavel_3 == 1){
      aux_final <- aux_final %>%
        mutate(valor = n_anomalias)
      titulo_y <- "Número de casos"
    } else {
      aux_final <- aux_final %>%
        mutate(valor = prevalencia)
      titulo_y <- "Prevalência por 1000 nascimentos"
    }
    
    
    top15 <- aux_final %>%
      arrange(desc(valor)) %>%
      slice_head(n = 20)
    
    hchart(top15, "bar", hcaes(x = nome, y = round(valor, 2))) %>%
      hc_colors("darkblue") %>%
      hc_title(text = paste("20 hospitais com maior ",variavel2[as.numeric(input$variavel_3)], "Defeitos de tubo neural")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = variavel2[as.numeric(input$variavel_3)])) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return  this.point.nome + '<br>' +",
          "'<b>", variavel2[as.numeric(input$variavel_3)], ":</b> ' + this.point.y;",
          "}"
        ))
      )
  })
  
  
  # fendas orais
  output$barras_fendas_top10 <- renderHighchart({
    
    req(input$ano_anomalias, input$variavel_3)
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      select(codestab, nome, ANONASC, cid_5)
    
    aux2 <- aux %>%
      group_by(codestab, nome) %>%
      summarise(
        n_anomalias = sum(cid_5, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codestab = sprintf("%07d", as.numeric(codestab)))
    
    nasc_total <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(CODESTAB = sprintf("%07d", as.numeric(CODESTAB)))
    
    aux_final <- aux2 %>%
      left_join(nasc_total, by = c("codestab" = "CODESTAB")) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux_final <- aux_final %>%
      left_join(
        hosp_mapa %>%
          mutate(CODESTAB_c = sprintf("%07d", as.numeric(CODESTAB))) %>%
          select(CODESTAB_c, MUNICIPIO),
        by = c("codestab" = "CODESTAB_c")
      )
    
    
    if(input$botao_POA_fendas == "Sem considerar POA"){
      aux_final <- aux_final %>%
        filter(MUNICIPIO != "PORTO ALEGRE")
    }
    
    
    if(input$variavel_3 == 1){
      aux_final <- aux_final %>%
        mutate(valor = n_anomalias)
      titulo_y <- "Número de casos"
    } else {
      aux_final <- aux_final %>%
        mutate(valor = prevalencia)
      titulo_y <- "Prevalência por 1000 nascimentos"
    }
    
    
    top15 <- aux_final %>%
      arrange(desc(valor)) %>%
      slice_head(n = 20)
    
    hchart(top15, "bar", hcaes(x = nome, y = round(valor, 2))) %>%
      hc_colors("steelblue") %>%
      hc_title(text = paste("20 hospitais com maior ",variavel2[as.numeric(input$variavel_3)], "Fendas orais")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = variavel2[as.numeric(input$variavel_3)])) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return  this.point.nome + '<br>' +",
          "'<b>", variavel2[as.numeric(input$variavel_3)], ":</b> ' + this.point.y;",
          "}"
        ))
      )
  })
  
  
  # hipospadia
  output$barras_hipo_top10 <- renderHighchart({
    
    req(input$ano_anomalias, input$variavel_3)
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      select(codestab, nome, ANONASC, cid_6)
    
    aux2 <- aux %>%
      group_by(codestab, nome) %>%
      summarise(
        n_anomalias = sum(cid_6, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codestab = sprintf("%07d", as.numeric(codestab)))
    
    nasc_total <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(CODESTAB = sprintf("%07d", as.numeric(CODESTAB)))
    
    aux_final <- aux2 %>%
      left_join(nasc_total, by = c("codestab" = "CODESTAB")) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux_final <- aux_final %>%
      left_join(
        hosp_mapa %>%
          mutate(CODESTAB_c = sprintf("%07d", as.numeric(CODESTAB))) %>%
          select(CODESTAB_c, MUNICIPIO),
        by = c("codestab" = "CODESTAB_c")
      )
    
    
    if(input$botao_POA_hipo == "Sem considerar POA"){
      aux_final <- aux_final %>%
        filter(MUNICIPIO != "PORTO ALEGRE")
    }
    
    
    if(input$variavel_3 == 1){
      aux_final <- aux_final %>%
        mutate(valor = n_anomalias)
      titulo_y <- "Número de casos"
    } else {
      aux_final <- aux_final %>%
        mutate(valor = prevalencia)
      titulo_y <- "Prevalência por 1000 nascimentos"
    }
    
    
    top15 <- aux_final %>%
      arrange(desc(valor)) %>%
      slice_head(n = 20)
    
    hchart(top15, "bar", hcaes(x = nome, y = round(valor, 2))) %>%
      hc_colors("darkblue") %>%
      hc_title(text = paste("20 hospitais com maior ",variavel2[as.numeric(input$variavel_3)], "Hipospadia")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = variavel2[as.numeric(input$variavel_3)])) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return  this.point.nome + '<br>' +",
          "'<b>", variavel2[as.numeric(input$variavel_3)], ":</b> ' + this.point.y;",
          "}"
        ))
      )
  })
  
  
  
  # microcefalia
  output$barras_micro_top10 <- renderHighchart({
    
    req(input$ano_anomalias, input$variavel_3)
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      select(codestab, nome, ANONASC, cid_7)
    
    aux2 <- aux %>%
      group_by(codestab, nome) %>%
      summarise(
        n_anomalias = sum(cid_7, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codestab = sprintf("%07d", as.numeric(codestab)))
    
    nasc_total <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(CODESTAB = sprintf("%07d", as.numeric(CODESTAB)))
    
    aux_final <- aux2 %>%
      left_join(nasc_total, by = c("codestab" = "CODESTAB")) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux_final <- aux_final %>%
      left_join(
        hosp_mapa %>%
          mutate(CODESTAB_c = sprintf("%07d", as.numeric(CODESTAB))) %>%
          select(CODESTAB_c, MUNICIPIO),
        by = c("codestab" = "CODESTAB_c")
      )
    
    
    if(input$botao_POA_micro == "Sem considerar POA"){
      aux_final <- aux_final %>%
        filter(MUNICIPIO != "PORTO ALEGRE")
    }
    
    
    if(input$variavel_3 == 1){
      aux_final <- aux_final %>%
        mutate(valor = n_anomalias)
      titulo_y <- "Número de casos"
    } else {
      aux_final <- aux_final %>%
        mutate(valor = prevalencia)
      titulo_y <- "Prevalência por 1000 nascimentos"
    }
    
    
    top15 <- aux_final %>%
      arrange(desc(valor)) %>%
      slice_head(n = 20)
    
    hchart(top15, "bar", hcaes(x = nome, y = round(valor, 2))) %>%
      hc_colors("steelblue") %>%
      hc_title(text = paste("20 hospitais com maior ",variavel2[as.numeric(input$variavel_3)], "Microcefalia")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = variavel2[as.numeric(input$variavel_3)])) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return  this.point.nome + '<br>' +",
          "'<b>", variavel2[as.numeric(input$variavel_3)], ":</b> ' + this.point.y;",
          "}"
        ))
      )
  })
  
  
  # sexo indefinido
  output$barras_sexo_top10 <- renderHighchart({
    
    req(input$ano_anomalias, input$variavel_3)
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      select(codestab, nome, ANONASC, cid_8)
    
    aux2 <- aux %>%
      group_by(codestab, nome) %>%
      summarise(
        n_anomalias = sum(cid_8, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codestab = sprintf("%07d", as.numeric(codestab)))
    
    nasc_total <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(CODESTAB = sprintf("%07d", as.numeric(CODESTAB)))
    
    aux_final <- aux2 %>%
      left_join(nasc_total, by = c("codestab" = "CODESTAB")) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux_final <- aux_final %>%
      left_join(
        hosp_mapa %>%
          mutate(CODESTAB_c = sprintf("%07d", as.numeric(CODESTAB))) %>%
          select(CODESTAB_c, MUNICIPIO),
        by = c("codestab" = "CODESTAB_c")
      )
    
    
    if(input$botao_POA_sexo == "Sem considerar POA"){
      aux_final <- aux_final %>%
        filter(MUNICIPIO != "PORTO ALEGRE")
    }
    
    
    if(input$variavel_3 == 1){
      aux_final <- aux_final %>%
        mutate(valor = n_anomalias)
      titulo_y <- "Número de casos"
    } else {
      aux_final <- aux_final %>%
        mutate(valor = prevalencia)
      titulo_y <- "Prevalência por 1000 nascimentos"
    }
    
    
    top15 <- aux_final %>%
      arrange(desc(valor)) %>%
      slice_head(n = 20)
    
    hchart(top15, "bar", hcaes(x = nome, y = round(valor, 2))) %>%
      hc_colors("darkblue") %>%
      hc_title(text = paste("20 hospitais com maior ",variavel2[as.numeric(input$variavel_3)], "Sexo indefinido")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = variavel2[as.numeric(input$variavel_3)])) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return  this.point.nome + '<br>' +",
          "'<b>", variavel2[as.numeric(input$variavel_3)], ":</b> ' + this.point.y;",
          "}"
        ))
      )
  })
  
  
  # Síndrome de Down
  output$barras_down_top10 <- renderHighchart({
    
    req(input$ano_anomalias, input$variavel_3)
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      select(codestab, nome, ANONASC, cid_9)
    
    aux2 <- aux %>%
      group_by(codestab, nome) %>%
      summarise(
        n_anomalias = sum(cid_9, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codestab = sprintf("%07d", as.numeric(codestab)))
    
    nasc_total <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(CODESTAB = sprintf("%07d", as.numeric(CODESTAB)))
    
    aux_final <- aux2 %>%
      left_join(nasc_total, by = c("codestab" = "CODESTAB")) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux_final <- aux_final %>%
      left_join(
        hosp_mapa %>%
          mutate(CODESTAB_c = sprintf("%07d", as.numeric(CODESTAB))) %>%
          select(CODESTAB_c, MUNICIPIO),
        by = c("codestab" = "CODESTAB_c")
      )
    
    
    if(input$botao_POA_down == "Sem considerar POA"){
      aux_final <- aux_final %>%
        filter(MUNICIPIO != "PORTO ALEGRE")
    }
    
    
    if(input$variavel_3 == 1){
      aux_final <- aux_final %>%
        mutate(valor = n_anomalias)
      titulo_y <- "Número de casos"
    } else {
      aux_final <- aux_final %>%
        mutate(valor = prevalencia)
      titulo_y <- "Prevalência por 1000 nascimentos"
    }
    
    
    top15 <- aux_final %>%
      arrange(desc(valor)) %>%
      slice_head(n = 20)
    
    hchart(top15, "bar", hcaes(x = nome, y = round(valor, 2))) %>%
      hc_colors("steelblue") %>%
      hc_title(text = paste("20 hospitais com maior ",variavel2[as.numeric(input$variavel_3)], "Síndrome de Down")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = variavel2[as.numeric(input$variavel_3)])) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return  this.point.nome + '<br>' +",
          "'<b>", variavel2[as.numeric(input$variavel_3)], ":</b> ' + this.point.y;",
          "}"
        ))
      )
  })
  
  
  
  # outras anomalias
  output$barras_outras_top10 <- renderHighchart({
    
    req(input$ano_anomalias, input$variavel_3)
    
    aux <- anom_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      select(codestab, nome, ANONASC, cid_10)
    
    aux2 <- aux %>%
      group_by(codestab, nome) %>%
      summarise(
        n_anomalias = sum(cid_10, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(codestab = sprintf("%07d", as.numeric(codestab)))
    
    nasc_total <- nasc_hosp %>%
      filter(ANONASC %in% input$ano_anomalias) %>%
      group_by(CODESTAB) %>%
      summarise(
        n_nascimentos = sum(n_nascimentos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(CODESTAB = sprintf("%07d", as.numeric(CODESTAB)))
    
    aux_final <- aux2 %>%
      left_join(nasc_total, by = c("codestab" = "CODESTAB")) %>%
      mutate(prevalencia = n_anomalias / n_nascimentos * 1000)
    
    aux_final <- aux_final %>%
      left_join(
        hosp_mapa %>%
          mutate(CODESTAB_c = sprintf("%07d", as.numeric(CODESTAB))) %>%
          select(CODESTAB_c, MUNICIPIO),
        by = c("codestab" = "CODESTAB_c")
      )
    
    
    if(input$botao_POA_outras == "Sem considerar POA"){
      aux_final <- aux_final %>%
        filter(MUNICIPIO != "PORTO ALEGRE")
    }
    
    
    if(input$variavel_3 == 1){
      aux_final <- aux_final %>%
        mutate(valor = n_anomalias)
      titulo_y <- "Número de casos"
    } else {
      aux_final <- aux_final %>%
        mutate(valor = prevalencia)
      titulo_y <- "Prevalência por 1000 nascimentos"
    }
    
    
    top15 <- aux_final %>%
      arrange(desc(valor)) %>%
      slice_head(n = 20)
    
    hchart(top15, "bar", hcaes(x = nome, y = round(valor, 2))) %>%
      hc_colors("darkblue") %>%
      hc_title(text = paste("20 hospitais com maior ",variavel2[as.numeric(input$variavel_3)], "Outras anomalias")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = variavel2[as.numeric(input$variavel_3)])) %>%
      hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE))) %>%
      hc_tooltip(
        useHTML = TRUE,
        formatter = JS(paste0(
          "function() {",
          "return  this.point.nome + '<br>' +",
          "'<b>", variavel2[as.numeric(input$variavel_3)], ":</b> ' + this.point.y;",
          "}"
        ))
      )
  })
  
  
  
}