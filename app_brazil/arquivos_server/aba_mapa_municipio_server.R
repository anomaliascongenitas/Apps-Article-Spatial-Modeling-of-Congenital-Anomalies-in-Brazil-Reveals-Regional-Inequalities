observeEvent(input$desmarcar_uf, {
  updateCheckboxGroupInput(session = session,
                           inputId = "mapa_munic_uf",choiceValues = op_ufs_munic2,choiceNames = op_ufs_munic,
                           selected = c(),inline = TRUE)
})

observeEvent(input$marcar_uf, {
  updateCheckboxGroupInput(session = session,
                           inputId = "mapa_munic_uf",
                           selected = op_ufs_munic2,inline = TRUE)
})





banco_munic_cid = reactive({
  banco_anomalias_aux = banco_anomalias %>%
    filter(cid_num %in% input$mapa_munic_cid,
           substring(CODMUNRES,1,2) %in% input$mapa_munic_uf) %>%
    group_by(ID,ANONASC) %>%
    summarise(CODMUNRES = CODMUNRES[1],.groups = "drop") %>%
    group_by(CODMUNRES,ANONASC) %>%
    summarise(n_anomalias = n(),.groups = "drop")
  
  banco_nascimentos_aux = banco_nascimentos %>%
    filter(substring(CODMUNRES,1,2) %in% input$mapa_munic_uf) %>%
    group_by(CODMUNRES,ANONASC) %>%
    summarise(n_nascimentos = sum(n_nasc_vivos),NOMEMUN =NOMEMUN[1],.groups = "drop")
  
  banco = banco_nascimentos_aux %>%
    left_join(banco_anomalias_aux,by= c("CODMUNRES","ANONASC")) %>%
    mutate(prevalencia = n_anomalias*10^4/n_nascimentos)
  
  banco$CODUF = as.numeric(substring(banco$CODMUNRES,1,2))
  
  banco = banco %>%
    left_join(siglas_ufs,by = c("CODUF"))
  
  banco[is.na(banco)] = 0
  
  posicao = which(names(banco) == variavel_opcoes2[as.numeric(input$mapa_munic_variavel)])
  banco$variavel = unlist(banco[,posicao],use.names = F)
  
  return(banco)
  
}) 



banco_munic_cid_ano = reactive({
  banco = banco_munic_cid() %>%
    filter(ANONASC %in% input$mapa_munic_ano) %>%
    group_by(CODMUNRES) %>%
    summarise(n_nascimentos = sum(n_nascimentos),n_anomalias = sum(n_anomalias),
              prevalencia = n_anomalias/n_nascimentos*10^4,NOMEMUN = NOMEMUN[1],
              .groups = "drop")
  
  banco$CODUF = as.numeric(substring(banco$CODMUNRES,1,2))
  
  banco = banco %>%
    left_join(siglas_ufs,by = c("CODUF"))
  
  posicao = which(names(banco) == variavel_opcoes2[as.numeric(input$mapa_munic_variavel)])
  banco$variavel = unlist(banco[,posicao],use.names = F)
  return(banco)
}) 
















output$box_populacao_munic <- renderValueBox({
  
  valueBox(
    format(sum(banco_munic_cid_ano()$n_nascimentos),big.mark = "."),
    "Total nascidos vivos",
    icon = icon("baby",lib = "font-awesome"),
    color = "blue"
  )
})


output$box_numero_casos_munic <- renderValueBox({
  valueBox(
    format(sum(banco_munic_cid_ano()$n_anomalias),big.mark = "."),
    "Total nascidos vivos com anomalias congenitas",
    icon = icon("notes-medical",lib = "font-awesome"),
    color = "red"
  )
})

output$box_prevalencia_munic <- renderValueBox({
  valueBox(
    round(sum(banco_munic_cid_ano()$n_anomalias)/sum(banco_munic_cid_ano()$n_nascimentos)*10^4,3),
    "Prevalência ao nascimento no(s) estado(s) selecionado(s) por 10000",
    icon = icon("notes-medical"),
    color = "purple"
  )
})




output$gerar_input_quadradinhos_munic <- renderUI({
  banco = banco_munic_cid_ano() %>%
    mutate(op_nomes = str_c(SIGLAUF , " - ",NOMEMUN))
  
  banco_selecionados = banco %>%
    slice_max(n_nascimentos,n = 20,with_ties = FALSE)
  
  selectizeInput("quadradinhos_munic",
                 label = "Escolha a(s) munic(s)",
                 choices = banco$op_nomes,
                 multiple = T,
                 options = list(maxItems = 300, placeholder = 'Escolha a(s) UF(s)'),
                 selected = banco_selecionados$op_nomes)
})







output$grafico_mapa_munic <- renderLeaflet({
  dataset <- banco_munic_cid_ano() 
  
  
  bins_defalt = classInt::classIntervals(var = banco_munic_cid_ano()$variavel, n = 7, style = "fisher")
  if(as.numeric(input$mapa_munic_variavel) %in% 1:2) {
    bins_defalt[["brks"]] = round(bins_defalt[["brks"]],0)
  }
  pal <- colorBin("YlOrRd", domain = dataset$variavel, bins = unique(bins_defalt$brks))
  
  
  tidy <- mapa_municipios %>%
    filter(substring(CD_GEOCMU,1,2) %in% input$mapa_munic_uf) %>%
    left_join(dataset,by = c("codigo_ibge" = "CODMUNRES")) 
  tidy = st_as_sf(tidy)
  tidy <- st_transform(tidy, "+init=epsg:4326") 
  
  leaflet(tidy) %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
    addPolygons(fillColor = ~pal(variavel), 
                weight = 1.5,
                opacity = 0.7,
                fillOpacity = 0.7,
                color = "gray",
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = sprintf("<strong>%s - %s</strong><br/>
                                Número nascidos vivos: %s<br/>
                                Casos anomalias: %s<br/>
                                Prevalência ao nascimento:%s",
                                tidy$SIGLAUF,tidy$NM_MUNICIP, tidy$n_nascimentos, 
                                tidy$n_anomalias,round(tidy$prevalencia,3)) %>%
                  lapply(htmltools::HTML),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "6px 11px"),
                  textsize = "13px",
                  opacity = 0.75,
                  direction = "bottom")) %>%
    leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, 
                       title = variavel_opcoes[as.numeric(input$mapa_munic_variavel)],
                       labFormat = labelFormat(digits = 3,big.mark = " "),
                       position = "bottomright") %>%
    addScaleBar(position = 'bottomleft')
  
})

output$grafico_barras_munic <- renderPlotly({
  aux <- banco_munic_cid_ano() 
  

  if(as.numeric(input$mapa_munic_variavel) == 3){
    media_brasil = sum(aux$n_anomalias)/sum(aux$n_nascimentos)*10000
  } else {
    media_brasil = mean(aux$variavel)
  }
  aux <- aux %>%
    filter(CODMUNRES %% 10000 != 0) %>%
    slice_max(variavel,n = 20,with_ties = F) %>%
    mutate(NOMEMUN = str_c(SIGLAUF ," - ", NOMEMUN))
  
  
  ordem <- aux$NOMEMUN[order(aux$variavel)]
  limites = c(0,max(aux$variavel))
  
  
  plot_barras <- ggplot(aux, aes(y = NOMEMUN, x = variavel)) +
    geom_col(fill = "darkmagenta", alpha = 1) +
    labs(y = "", title = variavel_opcoes[as.numeric(input$mapa_munic_variavel)]) +
    scale_y_discrete(limits = ordem,expand = expansion(add = c(0, 0.5))) +
    geom_vline( xintercept = media_brasil) +
    annotate(geom = "text", x =  max(1.35*media_brasil,max(aux$variavel)/3), y = 8, 
             label = "Média \nUFs selecionadas", color = "red",
    )+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
          axis.ticks.length = unit(0, "mm"),
          axis.title = element_blank())
  
  
  ggplotly(plot_barras)
  
})
















output$grafico_serie_munic <- renderPlotly({
  if(length(input$mapa_munic_cid) != 0 & length(input$mapa_munic_uf) != 0){
    
    banco_serie = banco_munic_cid() %>%
      group_by(ANONASC) %>%
      summarise(n_nascimentos = sum(n_nascimentos),n_anomalias = sum(n_anomalias),
                prevalencia = n_anomalias/n_nascimentos*10^4,
                .groups = "drop")
      
    posicao = which(names(banco_serie) == variavel_opcoes2[as.numeric(input$mapa_munic_variavel)])
    banco_serie$variavel = unlist(banco_serie[,posicao],use.names = F)
    
    banco_serie$ANONASC = as.character(banco_serie$ANONASC)
    
    
    plotar = ggplot(banco_serie) +
      geom_point(aes(x = ANONASC, y = variavel), color = "darkmagenta", alpha = 1) +
      geom_line(aes(x = ANONASC, y = variavel, group = 1), color = "darkmagenta", alpha = 1) +
      labs(x = "", y = "",
           title =  variavel_opcoes[as.numeric(input$mapa_munic_variavel)]) +
      ylim(0, max(banco_serie$variavel)*1.2)+
      theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
    ggplotly(plotar)
    
    
  }
})










output$plot_dots_munic <- renderPlotly({
  if(length(input$mapa_munic_cid) != 0 & length(input$mapa_munic_uf) != 0){
    banco = banco_munic_cid()

    banco = banco %>%
      filter(n_nascimentos >= input$limite_dots_cid_munic)
    
    
    banco$ANONASC <- as.factor(banco$ANONASC)
    
    grafico <- ggplot(banco, aes(y = variavel, x = ANONASC,fill = ANONASC)) +
      geom_violin(position = position_dodge(width = 0.9),
                  alpha = 1) +
      geom_quasirandom(dodge.width = 0.2, varwidth = TRUE,mapping = 
                         aes(
                           text=sprintf("%s - %s <br>Prevalências ao nascimento: %s <br>Ano: %s", SIGLAUF,NOMEMUN,round(prevalencia,3),
                                        ANONASC)), 
                       alpha = 1) +
      #ylim( input$limite_dots_cid_uf) +
      labs(x = "",y = "", title = variavel_opcoes[as.numeric(input$mapa_munic_variavel)]) +
      scale_fill_manual(values = rep("darkmagenta",length(anos)))+
      theme(legend.position = "none")  
    
    ggplotly(grafico,tooltip = "text")
    
    
  }
})










output$plot_quadradinhos_munic <- renderPlotly({
  if(length(input$mapa_munic_cid) != 0){
    
    
    banco <- banco_munic_cid() %>%
      mutate(NOMEMUN = str_c(SIGLAUF , " - ",NOMEMUN)) %>%
      filter(NOMEMUN %in% input$quadradinhos_munic)
    
    
    cidades_banco_quadradinhos = banco %>%
      filter(ANONASC == max(anos)) %>%
      arrange(desc(variavel)) %>%
      select(NOMEMUN)
    

    
    
    quadradinho=ggplot(banco,aes(x=ANONASC, y=reorder(NOMEMUN, variavel, FUN = sum), fill=variavel))+
      geom_raster(aes(text=sprintf("Ano nascimento: %s<br>Nome Município: %s  <br>Número de nascidos vivos: %s<br> Nº de nascidos vivos com anomalia: %s <br> Prevalência ao nascimento: %s",
                                   ANONASC, NOMEMUN,n_nascimentos,n_anomalias,round(prevalencia,2)))) +
      scale_fill_viridis_c(option = "A",  name= "",alpha = 1)+
      labs(x="",y="",title = variavel_opcoes[as.numeric(input$mapa_munic_variavel)]) +
      theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))+
      scale_x_continuous(breaks=anos,labels=anos)
    
    ggplotly(quadradinho,tooltip="text")
    
  }
  
})








banco_tabela_munic_1 <- reactive({
  banco_anomalias_aux = banco_anomalias %>%
    filter(substring(CODMUNRES,1,2) %in% input$mapa_munic_uf) %>%
    group_by(cid_num,ANONASC) %>%
    summarise(cid = cid[1],n_anomalias = n(),.groups = "drop")
  banco_nascimentos_aux = banco_nascimentos %>%
    filter(substring(CODMUNRES,1,2) %in% input$mapa_munic_uf) %>%
    group_by(ANONASC) %>%
    summarise(n_nascimentos = sum(n_nasc_vivos),.groups = "drop")
  
  
  banco_anomalias_aux2 = banco_anomalias_aux %>%
    left_join(banco_nascimentos_aux,by = c("ANONASC")) %>%
    mutate(prevalencia  = n_anomalias*10^4/n_nascimentos)
  
  
  final = banco_anomalias_aux2 %>%
    select(cid,ANONASC,prevalencia) %>%
    spread( key = ANONASC, value = prevalencia)
  final = final[c(1:7,9:10,8),]
  final
})





output$downloadData_munic_1 <- downloadHandler(
  filename = function() {
    paste("banco_anomalias_congenitas_munics_cid_", Sys.Date(),".csv", sep="")
  },
  content = function(file) {
    write.csv( banco_tabela_munic_1() ,file,row.names = FALSE)
  }
)

output$tabela_munic_1 <- renderDataTable({
  banco = banco_tabela_munic_1()
  
  
  banco %>%
    datatable(
      rownames = FALSE,
      colnames = c("Grupo de Anomalias",names(banco)[-1]),
      options = list(
        info = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        rowCallback = JS(rowCallback),
        paging = FALSE
      )
    )%>%
    formatCurrency(2:17,' ', digits = 3, interval = 3, mark = "", dec.mark = ",")
})












output$downloadData_munic_2 <- downloadHandler(
  filename = function() {
    paste("banco_anomalias_congenitas_munics_", Sys.Date(),".csv", sep="")
  },
  content = function(file) {
    write.csv( banco_munic_cid() ,file,row.names = FALSE)
  }
)

output$tabela_munic_2 <- renderDataTable({
  banco = banco_munic_cid() #%>%
    #select(-variavel)
  
  banco %>%
    datatable(
      rownames = FALSE,
      #colnames = c("Grupo de Anomalias",names(banco)[-1]),
      options = list(
        #info = TRUE,
        scrollX = TRUE,
        #searching = TRUE,
        rowCallback = JS(rowCallback)
      )
    )%>%
    formatCurrency(7,' ', digits = 3, interval = 3, mark = "", dec.mark = ",")
})
