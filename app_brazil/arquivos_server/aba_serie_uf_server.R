output$input_quadradinhos_html_cidade1 <- renderUI({ 
  
  teste <- dataset_anomalia_analise_ano_filtro()  %>%
    select(NOMEMUN,numero_nascidos_vivos)
  
  teste2 <- teste %>%
    slice_max(numero_nascidos_vivos,n = 1) %>%
    select(NOMEMUN)
  
  
  selectizeInput("cidade1",
                 label = "Escolha o(s) município(s)",
                 choices = unique(teste$NOMEMUN),
                 multiple = T,
                 options = list(maxItems = 300, placeholder = 'Escolha as cidades'),
                 selected = teste2$NOMEMUN)
})

output$aba_serie_ufs <- renderPlotly({
  
  cod_ufs_selecionadas = op_ufs_munic2[which(op_ufs %in% input$serie_uf)]
  
  banco_anomalias_aux = banco_anomalias %>%
    filter(cid_num %in% input$serie_uf_cid, as.numeric(substring(CODMUNRES,1,2)) %in% cod_ufs_selecionadas) %>%
    group_by(ID) %>%
    summarise(CODMUNRES = CODMUNRES[1],.groups = "drop",ANONASC = ANONASC[1]) %>%
    group_by(CODUF = as.numeric(substring(CODMUNRES,1,2)),ANONASC) %>%
    summarise(n_anomalias = n(),.groups = "drop")
  
  banco_nascimentos_aux = banco_nascimentos %>%
    filter(as.numeric(substring(CODMUNRES,1,2)) %in% cod_ufs_selecionadas) %>%
    group_by(CODUF = as.numeric(substring(CODMUNRES,1,2)),ANONASC) %>%
    summarise(n_nascimentos = sum(n_nasc_vivos),.groups = "drop")
  
  banco = banco_nascimentos_aux %>%
    left_join(banco_anomalias_aux,by= c("CODUF","ANONASC")) %>%
    mutate(prevalencia = n_anomalias*10^4/n_nascimentos)
  
  
  banco = banco %>%
    left_join(siglas_ufs,by = c("CODUF"))
  
  banco[is.na(banco)] = 0
  
  banco$variavel = unlist(banco[,which(names(banco) == input$serie_uf_variavel)],use.names = F)
  
   
  ggplotly(
    ggplot(banco, aes(x = ANONASC , y = variavel, colour = UF)) +
      geom_line() +
      geom_point(size = 2
      ,aes(text=sprintf("Ano nascimento: %s<br>UF: %s<br>Nº nascimentos com anomalia: %s<br>Nº nascimentos: %s <br>Prevalência ao nascimento: %s", 
                                           ANONASC, UF,
                                           n_anomalias,n_nascimentos,round(prevalencia,2)))
      )+
      ylim(0,max(banco$variavel)*1.1)+
      labs(x = "", y = variavel_opcoes[which(variavel_opcoes2 == input$serie_uf_variavel)],color = "UFs")+
      scale_x_continuous(breaks=2010:2025,labels=2010:2025),
    tooltip="text")
})
