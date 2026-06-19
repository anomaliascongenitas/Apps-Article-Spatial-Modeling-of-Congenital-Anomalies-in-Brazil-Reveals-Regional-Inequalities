
output$gerar_munic_serie <- renderUI({
  
  
  if(input$serie_munic_cod_ibge){
    old_nomes_munic = nomes_munic = with(mapa_municipios2,str_c(SIGLAUF," - ",NM_MUNICIP))
    nomes_munic = with(mapa_municipios2,str_c(codigo_ibge," ",SIGLAUF," - ",NM_MUNICIP))
  } else{
    old_nomes_munic = with(mapa_municipios2,str_c(codigo_ibge," ",SIGLAUF," - ",NM_MUNICIP))
    nomes_munic = with(mapa_municipios2,str_c(SIGLAUF," - ",NM_MUNICIP))
  }
  
  if(contador != 0){
    if(input$serie_munic_cod_ibge - old == 0){
      nomes_selecionados = nomes_munic[nomes_munic %in% input$serie_munic]
    } else{
      nomes_selecionados = nomes_munic[old_nomes_munic %in% input$serie_munic]
    }
  } else {
    nomes_selecionados = nomes_munic[1]
  }
  assign("old", input$serie_munic_cod_ibge,pos = 1)
  assign("contador", contador +1, pos=1)
  
  selectizeInput("serie_munic",
                 label = "Escolha a(s) Município(s)",
                 choices = nomes_munic,
                 multiple = T,
                 options = list(maxItems = 100, placeholder = 'Escolha a(s) Município(s)'),
                 selected = nomes_selecionados)
})
























# output$teste2 <- renderUI({
#   cod_selecionadas = mapa_municipios2 %>%
#     filter(str_c(SIGLAUF," - ",NM_MUNICIP) %in%  input$serie_munic | str_c(codigo_ibge," ",SIGLAUF," - ",NM_MUNICIP) %in% input$serie_munic)
# 
# 
#   cod_selecionadas2 = cod_selecionadas$codigo_ibge
# 
#   h3("oi",cod_selecionadas$codigo_ibge)
# })



output$aba_serie_munic <- renderPlotly({
  cod_selecionadas2 = cod_munic[nomes_munic  %in% input$serie_munic]
  
  banco_anomalias_aux = banco_anomalias %>%
    filter(cid_num %in% input$serie_munic_cid, as.numeric(substring(CODMUNRES,1,6)) %in% cod_selecionadas2 ) %>%
    group_by(ID) %>%
    summarise(CODMUNRES = CODMUNRES[1],.groups = "drop",ANONASC = ANONASC[1]) %>%
    group_by(CODMUNRES,ANONASC) %>%
    summarise(n_anomalias = n(),.groups = "drop")
  
  banco_nascimentos_aux = banco_nascimentos %>%
    filter(as.numeric(substring(CODMUNRES,1,6)) %in% cod_selecionadas2) %>%
    group_by(CODMUNRES,ANONASC) %>%
    summarise(n_nascimentos = sum(n_nasc_vivos),NOMEMUN = NOMEMUN[1],.groups = "drop")
  
  banco = banco_nascimentos_aux %>%
    left_join(banco_anomalias_aux,by= c("CODMUNRES","ANONASC")) %>%
    mutate(prevalencia = n_anomalias*10^4/n_nascimentos)
  
  banco[is.na(banco)] = 0
  
  banco$variavel = unlist(banco[,which(names(banco) == input$serie_munic_variavel)],use.names = F)
  
  
  ggplotly(
    ggplot(banco, aes(x = ANONASC , y = variavel, colour = NOMEMUN)) +
      geom_line() +
      geom_point(size = 2
                 ,aes(text=sprintf("Ano nascimento: %s<br>Cidade: %s<br>Nº nascimentos com anomalia: %s<br>Nº nascimentos: %s <br>Prevalência ao nascimento: %s", 
                                   ANONASC, NOMEMUN,
                                   n_anomalias,n_nascimentos,round(prevalencia,2)))
      )+
      #ylim(0,max(banco$variavel)*1.1)+
      labs(x = "", y = variavel_opcoes[which(variavel_opcoes2 == input$serie_munic_variavel)],color = "Municípios")+
      scale_x_continuous(breaks=2010:2025,labels=2010:2025),
    tooltip="text")
})


