output$grafico_mapa_uf <- renderLeaflet({
  dataset <- banco_uf_2_aba_1() 
  
  
  bins_defalt = bins_ufs()
  pal <- colorBin("YlOrRd", domain = dataset$variavel, bins = bins_defalt$brks)
  
  
  tidy <- dataset %>%
    left_join(mapa_uf,by = c("CODUF")) 
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
                label = sprintf("<strong>%s</strong><br/>
                                Número nascidos vivos: %s<br/>
                                Casos anomalias: %s<br/>
                                Prevalência ao nascimento:%s",
                                tidy$UF, tidy$n_nascimentos, 
                                tidy$n_anomalias,round(tidy$prevalencia,3)) %>%
                  lapply(htmltools::HTML),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "6px 11px"),
                  textsize = "13px",
                  opacity = 0.75,
                  direction = "bottom")) %>%
    leaflet::addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, 
                       title = variavel_opcoes[as.numeric(input$mapa_uf_variavel)],
                       labFormat = labelFormat(digits = 3,big.mark = " "),
                       position = "bottomright") %>%
    addScaleBar(position = 'bottomleft')
  
})






output$box_populacao_uf <- renderValueBox({
  
  valueBox(
    format(sum(banco_uf_aba_1()$n_nascimentos),big.mark = "."),
    "Total nascidos vivos",
    icon = icon("baby",lib = "font-awesome"),
    color = "blue"
  )
})


output$box_numero_casos_uf <- renderValueBox({
  valueBox(
    format(sum(banco_uf_aba_1()$n_anomalias),big.mark = "."),
    "Total nascidos vivos com anomalias congenitas",
    icon = icon("notes-medical",lib = "font-awesome"),
    color = "red"
  )
})

output$box_prevalencia_uf <- renderValueBox({
  valueBox(
    round(sum(banco_uf_aba_1()$n_anomalias)/sum(banco_uf_aba_1()$n_nascimentos)*10^4,3),
    "Prevalência ao nascimento no Brasil por 10000",
    icon = icon("notes-medical"),
    color = "purple"
  )
})





output$grafico_barras_uf <- renderPlotly({
  aux <- banco_uf_2_aba_1()
  
  ordem <- aux$UF[order(aux$variavel)]
  limites = c(0,max(aux$variavel))
  
  if(as.numeric(input$mapa_uf_variavel) == 3){
    media_brasil = sum(aux$n_anomalias)/sum(aux$n_nascimentos)*10000
  } else {
    media_brasil = mean(aux$variavel)
  }
  
  
  plot_barras <- ggplot(aux, aes(y = UF, x = variavel)) +
    geom_col(fill = "darkmagenta", alpha = 1) +
    labs(y = "", title = variavel_opcoes[as.numeric(input$mapa_uf_variavel)]) +
    #labs(x = "Municipio", y = "Gráfico das 20 cidades com maiores valores de Prevalencia por 10000") +
    scale_y_discrete(limits = ordem,expand = expansion(add = c(0, 0.5))) +
    geom_vline( xintercept = media_brasil) +
    #geom_text(label="Média Brasileira",mapping = aes(x = media_brasil,y=10), position = position_dodge(0.9),
    #   vjust = 0,angle=90) +
    annotate(geom = "text", x =  max(1.35*media_brasil,max(aux$variavel)/3), y = 8, label = "Média \nBrasileira", color = "red",
             #angle = 90
    )+
    #scale_x_discrete(expand = c(0, 0)) +
    #      axis.text.x = element_text(angle=90,size=7, vjust = 0.5)
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
          axis.ticks.length = unit(0, "mm"),
          axis.title = element_blank())
  
  
  ggplotly(plot_barras)
  
})


output$grafico_serie_uf <- renderPlotly({
  if(length(input$mapa_uf_cid) != 0){
    
    aux <- banco_uf_aba_1_serie()
    
    aux$ANONASC = as.character(aux$ANONASC)
    
    
    plotar = ggplot(aux) +
      geom_point(aes(x = ANONASC, y = variavel), color = "darkmagenta", alpha = 1) +
      geom_line(aes(x = ANONASC, y = variavel, group = 1), color = "darkmagenta", alpha = 1) +
      labs(x = "", y = "",
           title =  variavel_opcoes[as.numeric(input$mapa_uf_variavel)]) +
      ylim(0, max(aux$variavel)*1.2)+
      theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))
    ggplotly(plotar)
    
    
  }
})









output$plot_dots_uf <- renderPlotly({
  if(length(input$mapa_uf_cid) != 0){
    banco <- banco_uf_aba_1_dots() 
    
    banco$ANONASC <- as.factor(banco$ANONASC)
    
    grafico <- ggplot(banco, aes(y = variavel, x = ANONASC,fill = ANONASC)) +
      geom_violin(position = position_dodge(width = 0.9),
                  alpha = 1) +
      geom_quasirandom(dodge.width = 0.2, varwidth = TRUE,mapping = 
                         aes(
                           text=sprintf(" %s <br>Prevalências ao nascimento: %s <br>Ano: %s", UF,round(prevalencia,3),
                                        ANONASC)), 
                       alpha = 1) +
      #ylim( input$limite_dots_cid_uf) +
      labs(x = "",y = "", title = variavel_opcoes[as.numeric(input$mapa_uf_variavel)]) +
      scale_fill_manual(values = rep("darkmagenta",length(anos)))+
      theme(legend.position = "none")  
    
    ggplotly(grafico,tooltip = "text")
    
    
  }
})


output$gerar_limite_dots_cid_uf <- renderUI({
  if(length(input$mapa_uf_cid) != 0){
  banco <- banco_uf_aba_1_dots() 
  maximo <- round(max(banco$variavel*1.1),0)
  sliderInput(
    "limite_dots_cid_uf",
    "Limites do eixo vertical",
    min = 0,
    max = maximo,
    value = c(0, maximo),
    step = maximo/100
  )
  }
})




output$teste <- renderUI({
  ufs_selecionadas = siglas_ufs$SIGLAUF[which( op_ufs %in%  input$input_quadradinhos_uf )]
  h2(paste(ufs_selecionadas,collapse = " "))
})









output$plot_quadradinhos_uf <- renderPlotly({
  if(length(input$mapa_uf_cid) != 0){
    
    
    ufs_selecionadas = siglas_ufs$SIGLAUF[which( op_ufs %in%  input$input_quadradinhos_uf )]
    
    banco <- banco_uf_aba_1_dots() %>%
      filter(SIGLAUF %in% ufs_selecionadas)
    
    
    cidades_banco_quadradinhos = banco %>%
      filter(ANONASC == max(anos)) %>%
      arrange(desc(variavel)) %>%
      select(UF)
    
    #bins_defalt = bins_ufs()
    
    
    quadradinho=ggplot(banco,aes(x=ANONASC, y=reorder(UF, variavel, FUN = sum), fill=variavel))+
      geom_raster(aes(text=sprintf("Ano nascimento: %s<br>UF: %s  <br>Número de nascidos vivos: %s <br>Nº de nascidos vivos com anomalia: %s <br>Prevalência ao nascimento: %s",
                                   ANONASC, UF,n_nascimentos,n_anomalias,round(prevalencia,2)))) +
      scale_fill_viridis_c(option = "A",  name= "",alpha = 1)+
      labs(x="",y="",title = variavel_opcoes[as.numeric(input$mapa_uf_variavel)]) +
      theme(axis.text.x = element_text(angle=45,size=9, vjust = 0.5))+
      scale_x_continuous(breaks=anos,labels=anos)
    
    ggplotly(quadradinho,tooltip="text")
    
  }
  
})







output$downloadData_uf_1 <- downloadHandler(
  filename = function() {
    paste("banco_anomalias_congenitas_ufs_cid_", Sys.Date(),".csv", sep="")
  },
  content = function(file) {
    write.csv( banco_tabela_uf_1() ,file,row.names = FALSE)
  }
)

output$tabela_uf_1 <- renderDataTable({
  
  banco = banco_tabela_uf_1()
  
  banco %>%
    datatable(
      rownames = FALSE,
      colnames = c("Grupo de Anomalias",names(banco)[-1]),
      options = list(
        info = FALSE,
        scrollX = FALSE,
        scrollY = "500px",
        searching = FALSE,
        rowCallback = JS(rowCallback),
        paging = FALSE
      )
    )%>%
    formatCurrency(2:17,' ', digits = 3, interval = 3, mark = "", dec.mark = ",")
})





output$downloadData_uf_2 <- downloadHandler(
  filename = function() {
    paste("banco_anomalias_congenitas_ufs_", Sys.Date(),".csv", sep="")
  },
  content = function(file) {
    write.csv( banco_tabela_uf_2() ,file,row.names = FALSE)
  }
)

output$tabela_uf_2 <- renderDataTable({
  banco_tabela_uf_2()  %>%
    datatable(
      rownames = FALSE,
      options = list(
        info = TRUE,
        scrollX = TRUE,
        scrollY = "400px",
        searching = TRUE,
        rowCallback = JS(rowCallback),
        paging = TRUE
      )
    )%>%
    formatCurrency(7,' ', digits = 3, interval = 3, mark = "", dec.mark = ",")
})
