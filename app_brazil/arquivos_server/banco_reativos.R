banco_uf_aba_1 = reactive({
  banco_anomalias_aux = banco_anomalias %>%
    filter(cid_num %in% input$mapa_uf_cid, ANONASC %in% input$mapa_uf_ano) %>%
    group_by(ID) %>%
    summarise(CODMUNRES = CODMUNRES[1],.groups = "drop") %>%
    group_by(CODUF = as.numeric(substring(CODMUNRES,1,2))) %>%
    summarise(n_anomalias = n(),.groups = "drop")
  
  banco_nascimentos_aux = banco_nascimentos %>%
    filter(ANONASC %in% input$mapa_uf_ano) %>%
    group_by(CODUF = as.numeric(substring(CODMUNRES,1,2))) %>%
    summarise(n_nascimentos = sum(n_nasc_vivos),.groups = "drop")
  
  banco = banco_nascimentos_aux %>%
    left_join(banco_anomalias_aux,by= c("CODUF")) %>%
    mutate(prevalencia = n_anomalias*10^4/n_nascimentos)
  
  
  banco = banco %>%
    left_join(siglas_ufs,by = c("CODUF"))
  
  banco[is.na(banco)] = 0
  
  banco
  
}) 

banco_uf_2_aba_1 = reactive({
  banco_teste = banco_uf_aba_1() 
  banco_teste$variavel = unlist(banco_teste[,as.numeric(input$mapa_uf_variavel)+1],use.names = F)
  
  banco_teste
}) 







### banco da serie temporal (ANONASC,n_nascimentos,n_anomalias,prevalencia,variavel)
banco_uf_aba_1_serie = reactive({
  banco_anomalias_aux = banco_anomalias %>%
    filter(cid_num %in% input$mapa_uf_cid) %>%
    group_by(ID) %>%
    summarise(ANONASC = ANONASC[1],.groups = "drop") %>%
    group_by(ANONASC) %>%
    summarise(n_anomalias = n(),.groups = "drop")
  
  banco_nascimentos_aux = banco_nascimentos %>%
    group_by(ANONASC) %>%
    summarise(n_nascimentos = sum(n_nasc_vivos),.groups = "drop")
  
  banco_serie = banco_nascimentos_aux %>%
    left_join(banco_anomalias_aux,by= c("ANONASC")) %>%
    mutate(prevalencia = n_anomalias*10^4/n_nascimentos)
  
  banco_serie$variavel = unlist(banco_serie[,as.numeric(input$mapa_uf_variavel)+1],use.names = F)
  
  banco_serie
}) 


banco_uf_aba_1_dots = reactive({
  banco_anomalias_aux = banco_anomalias %>%
    filter(cid_num %in% input$mapa_uf_cid) %>%
    group_by(ID) %>%
    summarise(CODMUNRES = CODMUNRES[1],ANONASC = ANONASC[1],.groups = "drop") %>%
    group_by(CODUF = as.numeric(substring(CODMUNRES,1,2)),ANONASC) %>%
    summarise(n_anomalias = n(),.groups = "drop")
  
  banco_nascimentos_aux = banco_nascimentos %>%
    group_by(CODUF = as.numeric(substring(CODMUNRES,1,2)),ANONASC) %>%
    summarise(n_nascimentos = sum(n_nasc_vivos),.groups = "drop")
  
  banco = banco_nascimentos_aux %>%
    left_join(banco_anomalias_aux,by= c("CODUF","ANONASC")) %>%
    mutate(prevalencia = n_anomalias*10^4/n_nascimentos)
  
  
  banco = banco %>%
    left_join(siglas_ufs,by = c("CODUF"))
  
  banco[is.na(banco)] = 0
  
  
  banco$variavel = unlist(banco[,as.numeric(input$mapa_uf_variavel)+2],use.names = F)
  
  banco
}) 





bins_ufs <- reactive({
  bins_defalt = classInt::classIntervals(var = banco_uf_2_aba_1()$variavel, n = 7, style = "fisher")
  if(as.numeric(input$mapa_uf_variavel) %in% 1:2) {
    bins_defalt[["brks"]] = round(bins_defalt[["brks"]],0)
  }
  bins_defalt
})



banco_tabela_uf_2 <- reactive({
  banco_aux =  banco_uf_aba_1_dots() %>%
    filter(ANONASC %in% input$mapa_uf_ano) %>%
    select(SIGLAUF,UF,CODUF,ANONASC,n_nascimentos,n_anomalias,prevalencia)
})

banco_tabela_uf_1 <- reactive({
  banco_anomalias_aux = banco_anomalias %>%
    group_by(cid_num,ANONASC) %>%
    summarise(cid = cid[1],n_anomalias = n(),.groups = "drop")
  banco_nascimentos_aux = banco_nascimentos %>%
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


