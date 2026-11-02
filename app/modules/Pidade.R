PidadeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    
    fluidRow(
      box(
        title = h1('Faixa Etaria x Denuncias - Dados disk 100', align = 'center'), #trocar titulo
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        fluidRow(
          width = 12,
          column(offset = 1, 2, uiOutput(ns("regiao_ui"))),
          column(2, uiOutput(ns("estado_ui"))),
          column(2, uiOutput(ns("intermediaria_ui"))),
          column(2, uiOutput(ns("imediata_ui"))),
          column(2, uiOutput(ns("municipio_ui")))
        )
      )
    ),
    
    fluidRow(
      
      box(
        title = h1('Distribuição de Escolaridade', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_1")), type = 1, color = "#ffae00", size = 2)
      ),
      
      box(
        title = h1('trocar', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_2")), type = 1, color = "#ffae00", size = 2)
      )
    ),
    
    fluidRow(
      
      box(
        title = h1('Análise temporal', align = 'center'), #trocar titulo
        width = 12,
        style = "overflow-x: auto;",
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("tabela")), type = 1, color = "#ffae00", size = 2)
      )
    )
  )
}

PidadeServer <- function(input, output, session) {
  ns <- session$ns
  
  dados_faixa_etaria <- read_xlsx("dados/faixa_etaria.xlsx")
  # REATIVOS ----
  ##filtros----
  filtros <- reactiveValues(
    Regiao = NULL,
    Estado = NULL,
    Intermediaria = NULL,
    Imediata = NULL,
    Municipio = NULL
  )
  ##dados ----
  df <- reactiveValues(
    filtrado = NULL
  )
  
  # UI DINÂMICA ----
  ## regiao ----
  output$regiao_ui <- renderUI({
    req(dados_faixa_etaria)
    selectizeInput(ns("Regiao"), "Região",
                   choices = c("TODOS", sort(unique(dados_faixa_etaria$regiao_ibge))),
                   selected = "TODOS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  ## estados ----
  output$estado_ui <- renderUI({
    req(dados_faixa_etaria)
    selectizeInput(ns("Estado"), "Estado",
                   choices = c("TODAS", sort(unique(dados_faixa_etaria$uf_final))),
                   selected = "TODAS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  ## regioes intermediarias ----
  output$intermediaria_ui <- renderUI({
    req(dados_faixa_etaria)
    selectizeInput(ns("Intermediaria"), "Região Intermediaria",
                   choices = c("TODAS", sort(unique(dados_faixa_etaria$rgint))),
                   selected = "TODAS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  ## regioes imediatas ----
  output$imediata_ui <- renderUI({
    req(dados_faixa_etaria)
    selectizeInput(ns("Imediatas"), "Região Imediata",
                   choices = c("TODAS", sort(unique(dados_faixa_etaria$rgi))),
                   selected = "TODAS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  ##municipio ----
  output$municipio_ui <- renderUI({
    req(dados_faixa_etaria)
    selectizeInput(ns("Municipio"), "Município",
                   choices = c("TODOS", sort(unique(dados_faixa_etaria$municipio_final))),
                   selected = "TODOS", multiple = TRUE, options = list(maxOptions = 10000))
  })
  
  # VALIDAÇÕES ----
  ## choices ----
  observeEvent(input$Regiao, {
    if (!is.null(input$Regiao) && "TODOS" %in% input$Regiao && length(input$Regiao) > 1) {
      sendSweetAlert(session, "Erro",
                     "A opção 'TODOS' não pode ser combinada com outras regiões",
                     type = "warning")
      updateSelectizeInput(session, "Estado", selected = "TODOS")
    }
  })
  
  observeEvent(input$Estado, {
    if (!is.null(input$Estado) && "TODOS" %in% input$Estado && length(input$Estado) > 1) {
      sendSweetAlert(session, "Erro",
                     "A opção 'TODOS' não pode ser combinada com outros estados",
                     type = "warning")
      updateSelectizeInput(session, "Estado", selected = "TODOS")
    }
  })
  
  observeEvent(input$Intermediaria, {
    if (!is.null(input$Intermediaria) && "TODAS" %in% input$Intermediaria && length(input$Intermediaria) > 1) {
      sendSweetAlert(session, "Erro",
                     "A opção 'TODAS' não pode ser combinada com outras regiçoes intermediarias",
                     type = "warning")
      updateSelectizeInput(session, "Intermediaria", selected = "TODAS")
    }
  })
  
  observeEvent(input$Imediatas, {
    if (!is.null(input$Imediatas) && "TODAS" %in% input$Imediatas && length(input$Imediatas) > 1) {
      sendSweetAlert(session, "Erro",
                     "A opção 'TODAS' não pode ser combinada com outras regioes intermediarias",
                     type = "warning")
      updateSelectizeInput(session, "Imediatas", selected = "TODAS")
    }
  })
  
  observeEvent(input$Municipio, {
    if (!is.null(input$Municipio) && "TODOS" %in% input$Municipio && length(input$Municipio) > 1) {
      sendSweetAlert(session, "Erro",
                     "A opção 'TODOS' não pode ser combinada com outros municípios",
                     type = "warning")
      updateSelectizeInput(session, "Municipio", selected = "TODOS")
    }
  })
  
  ##filtros ----
  observe({
    req(dados_faixa_etaria, input$Regiao)
    filtros$Regiao <- if ("TODOS" %in% input$Regiao) unique(dados_faixa_etaria$regiao_ibge) else input$Regiao
  })
  
  observe({
    req(dados_faixa_etaria, input$Estado)
    filtros$Estado <- if ("TODOS" %in% input$Estado) unique(dados_faixa_etaria$uf_final) else input$Estado
  })
  
  observe({
    req(dados_faixa_etaria, input$Intermediaria)
    filtros$Intermediaria <- if ("TODAS" %in% input$Intermediaria) unique(dados_faixa_etaria$rgint) else input$Intermediaria
  })
  
  observe({
    req(dados_faixa_etaria, input$Imediatas)
    filtros$Imediatas <- if ("TODAS" %in% input$Imediatas) unique(dados_faixa_etaria$rgi) else input$Imediatas
  })
  
  observe({
    req(dados_faixa_etaria, input$Municipio)
    filtros$Municipio <- if ("TODOS" %in% input$Municipio) unique(dados_faixa_etaria$municipio_final) else input$Municipio
  })
  
  # OUTPUT ----
  ## tabela ----
  # output$tabela = renderDataTable({
  #   req(dados)
  # 
  #   # Aplicar filtros
  #   dados_aux = dados
  #   if (!is_empty(filtros$estado)) {
  #     dados_aux = dados_aux %>% filter(estado %in% filtros$estado)
  #   }
  #   if (!is_empty(filtros$meso)) {
  #     dados_aux = dados_aux %>% filter(mesorregioes %in% filtros$meso)
  #   }
  #   if (!is_empty(filtros$micro)) {
  #     dados_aux = dados_aux %>% filter(microrregioes %in% filtros$micro)
  #   }
  #   if (!is_empty(filtros$municipio)) {
  #     dados_aux = dados_aux %>% filter(municipio %in% filtros$municipio)
  #   }
  # 
  #   # Salvar no reativo
  #   df$filtrado <- dados_aux
  # 
  #   # Selecionar colunas
  #   dados_aux <- dados_aux %>%
  #     select(
  #       ano,
  #       estado,
  #       uf,
  #       municipio,
  #       mesorregioes,
  #       microrregioes,
  #       total_pop_em_situacao_de_rua,
  #       masculino,
  #       feminino,
  #       sem_instrucao,
  #       fundamental_incompleto,
  #       fundamental_completo,
  #       medio_incompleto,
  #       medio_completo,
  #       superior_incompleto_ou_mais,
  #       sem_resposta_10,
  #       branca,
  #       preta,
  #       amarela,
  #       parda,
  #       indigena,
  #       sem_resposta_16
  #     )
  # 
  #   # Aplicar ordenação condicional
  #   if (
  #     is_empty(filtros$estado) &&
  #     is_empty(filtros$meso) &&
  #     is_empty(filtros$micro) &&
  #     is_empty(filtros$municipio)
  #   ) {
  #     dados_aux <- dados_aux %>% arrange(municipio)  # Ordem alfabética
  #   } else {
  #     dados_aux <- dados_aux %>% arrange(desc(total_pop_em_situacao_de_rua))  # Ordem decrescente
  #   }
  # 
  #   # Renomear colunas
  #   colnames(dados_aux) <- c(
  #     "Ano",
  #     "Estado",
  #     "UF",
  #     "Município",
  #     "Mesorregião",
  #     "Microrregião",
  #     "População em Situação de Rua",
  #     "Masculino",
  #     "Feminino",
  #     "Sem Instrução",
  #     "Fundamental Incompleto",
  #     "Fundamental Completo",
  #     "Médio Incompleto",
  #     "Médio Completo",
  #     "Superior Incompleto ou +",
  #     "Sem Resposta (Escolaridade)",
  #     "Branca",
  #     "Preta",
  #     "Amarela",
  #     "Parda",
  #     "Indígena",
  #     "Sem Resposta (Raça)"
  #   )
  # 
  #   # Exibir tabela
  #   DT::datatable(
  #     dados_aux,
  #     rownames = FALSE,
  #     filter = "none",
  #     style = "bootstrap",
  #     class = "stripe hover cell-border compact",
  #     options = list(
  #       dom = 'fltip',
  #       pageLength = 10,
  #       lengthMenu = c(5, 10, 25, 50, 100),
  #       scrollX = TRUE,
  #       autoWidth = TRUE,
  #       searchHighlight = TRUE,
  #       columnDefs = list(list(className = 'dt-center', targets = "_all"))
  #     )
  #   ) %>%
  #     DT::formatStyle(
  #       columns = names(dados_aux),
  #       fontSize = '12px',
  #       color = 'black',
  #       fontWeight = 'normal',
  #       textAlign = 'center'
  #     )
  # })
  # 
  #   
  # 
  # # banco de dados tratado para sexo
  #   
  #   observe({
  #     req(df$filtrado)
  #     # 1. Selecionar apenas colunas necessárias
  #     dadossexo <- df$filtrado %>%
  #       select(
  #         ano,
  #         estado,
  #         uf,
  #         municipio,
  #         mesorregioes,
  #         microrregioes,
  #         total_pop_em_situacao_de_rua,
  #         masculino, 
  #         feminino,
  #         sem_instrucao, 
  #         fundamental_incompleto, 
  #         fundamental_completo,
  #         medio_incompleto, 
  #         medio_completo, 
  #         superior_incompleto_ou_mais,
  #         sem_resposta_10,
  #         branca,                                                                             
  #         preta,                                                                             
  #         amarela,                                                                         
  #         parda,                                                                              
  #         indigena,                                                                         
  #         sem_resposta_16
  #       )
  #     #----------------------
  #     
  #     
  #     # 1. Garantir que colunas sejam numéricas
  #     dadossexo <- dadossexo %>%
  #       mutate(across(
  #         c(masculino,
  #           feminino,
  #           total_pop_em_situacao_de_rua,
  #           sem_instrucao,
  #           fundamental_incompleto,
  #           fundamental_completo,
  #           medio_incompleto,
  #           medio_completo,
  #           superior_incompleto_ou_mais,
  #           sem_resposta_10,
  #           branca,
  #           preta,
  #           amarela,
  #           parda,
  #           indigena,
  #           sem_resposta_16),
  #         ~ as.numeric(.)
  #       ))
  #     
  #     dados_proporcional <- dadossexo %>%
  #       mutate(
  #         prop_masc = ifelse(total_pop_em_situacao_de_rua > 0,
  #                            masculino / total_pop_em_situacao_de_rua, 0),
  #         prop_fem  = ifelse(total_pop_em_situacao_de_rua > 0,
  #                            feminino  / total_pop_em_situacao_de_rua, 0)
  #       )
  #     
  #     # 3. Calcular total de População Negra e Não Negra
  #     dados_raca <- dados_proporcional %>%
  #       mutate(
  #         pop_negra = preta + parda,
  #         pop_nao_negra = branca + amarela + indigena + sem_resposta_16,
  #         prop_negra = ifelse(total_pop_em_situacao_de_rua > 0,
  #                             pop_negra / total_pop_em_situacao_de_rua, 0),
  #         prop_nao_negra = ifelse(total_pop_em_situacao_de_rua > 0,
  #                                 pop_nao_negra / total_pop_em_situacao_de_rua, 0)
  #       )
  #     
  #     # 4. Transformar graus de instrução em formato long
  #     dados_long <- dados_raca %>%
  #       pivot_longer(
  #         cols = c(
  #           sem_instrucao,
  #           fundamental_incompleto,
  #           fundamental_completo,
  #           medio_incompleto,
  #           medio_completo,
  #           superior_incompleto_ou_mais,
  #           sem_resposta_10
  #         ),
  #         names_to = "GrauInstrucao",
  #         values_to = "PopulacaoGrau"
  #       ) %>%
  #       mutate(
  #         PopulacaoGrau = as.numeric(PopulacaoGrau),
  #         # estimar por grupo racial e sexo
  #         pop_masc_negra     = PopulacaoGrau * prop_masc * prop_negra,
  #         pop_fem_negra      = PopulacaoGrau * prop_fem  * prop_negra,
  #         pop_masc_nao_negra = PopulacaoGrau * prop_masc * prop_nao_negra,
  #         pop_fem_nao_negra  = PopulacaoGrau * prop_fem  * prop_nao_negra
  #       )
  #     
  #     # 5. Selecionar, juntar e formatar os dois bancos
  #     
  #     # População Negra
  #     dados_negra <- dados_long %>%
  #       select(
  #         ano,
  #         municipio,
  #         mesorregioes,
  #         microrregioes,
  #         estado,
  #         uf,
  #         GrauInstrucao,
  #         pop_masc_negra,
  #         pop_fem_negra
  #       ) %>%
  #       pivot_longer(
  #         cols = c(pop_masc_negra, pop_fem_negra),
  #         names_to = "Sexo", values_to = "Populacao"
  #       ) %>%
  #       mutate(
  #         Sexo = ifelse(Sexo == "pop_masc_negra", "Masculino", "Feminino"),
  #         GrupoRacial = "Negra"
  #       )
  #     
  #     # População Não Negra
  #     dados_nao_negra <- dados_long %>%
  #       select(
  #         ano,
  #         municipio,
  #         mesorregioes,
  #         microrregioes,
  #         estado,
  #         uf,
  #         GrauInstrucao,
  #         pop_masc_nao_negra,
  #         pop_fem_nao_negra
  #       ) %>%
  #       pivot_longer(
  #         cols = c(pop_masc_nao_negra, pop_fem_nao_negra),
  #         names_to = "Sexo", values_to = "Populacao"
  #       ) %>%
  #       mutate(
  #         Sexo = ifelse(Sexo == "pop_masc_nao_negra", "Masculino", "Feminino"),
  #         GrupoRacial = "Não negra"
  #       )
  #     
  #     # 6. Juntar em um único banco final
  #     dados_raca_tidy <- bind_rows(dados_negra, dados_nao_negra)
  #     
  #     # Reorganizar nomes dos graus de instrução para ficarem mais legíveis
  #     grau_nomes <- c(
  #       "sem_instrucao" = "Sem instrução",
  #       "fundamental_incompleto" = "Fund. incompleto",
  #       "fundamental_completo" = "Fund. completo",
  #       "medio_incompleto" = "Médio incompleto",
  #       "medio_completo" = "Médio completo",
  #       "superior_incompleto_ou_mais" = "Superior ou +",
  #       "sem_resposta_10" = "Sem resposta"
  #       
  #     )
  #     # Definir a ordem desejada como fator ordenado
  #     
  #     # Aplicar nomes bonitos
  #     dados_plot <- dados_raca_tidy %>%
  #       mutate(GrauInstrucao = recode(GrauInstrucao, !!!grau_nomes))
  #       
  #     # Vetor com a ordem desejada
  #     ordem_graus <- c(
  #       "Sem instrução",
  #       "Fund. incompleto",
  #       "Fund. completo",
  #       "Médio incompleto",
  #       "Médio completo",
  #       "Superior ou +",
  #       "Sem resposta"
  #     )
  #     
  #     # Preparar os dados
  #     dados_negros <- dados_plot %>%
  #       filter(GrupoRacial == "Negra") %>%
  #       group_by(ano, GrauInstrucao, Sexo) %>%
  #       summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
  #       mutate(GrauInstrucao = factor(GrauInstrucao, levels = ordem_graus)) %>%
  #       arrange(GrauInstrucao)
  #     
  #     dados_nao_negros <- dados_plot %>%
  #       filter(GrupoRacial == "Não negra") %>%
  #       group_by(ano, GrauInstrucao, Sexo) %>%
  #       summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop") %>%
  #       mutate(GrauInstrucao = factor(GrauInstrucao, levels = ordem_graus)) %>%
  #       arrange(GrauInstrucao)
  #     
  #     df$plot_negros <- dados_negros
  #     df$plot_n_negros <- dados_nao_negros
  #   }) 
  #   
  # output$plot_1 <- renderHighchart({
  #   req(df$plot_negros)
  #   
  #   negros_2024 <- df$plot_negros %>%
  #     filter(ano == "2024" )
  #   
  #   categorias <- levels(negros_2024$GrauInstrucao)
  #   
  #   serie_masculino <- round(negros_2024 %>%
  #                              filter(Sexo == "Masculino") %>%
  #                              arrange(GrauInstrucao) %>%
  #                              pull(Populacao))
  #   
  #   serie_feminino <- round(negros_2024 %>%
  #                             filter(Sexo == "Feminino") %>%
  #                             arrange(GrauInstrucao) %>%
  #                             pull(Populacao))
  #   
  #   highchart() %>%
  #     hc_chart(type = "column") %>%
  #     hc_title(text = "População Negra por Sexo e Grau de Instrução") %>%
  #     hc_xAxis(categories = categorias,
  #              title = list(text = "Grau de Instrução")) %>%
  #     hc_yAxis(
  #       title = list(text = "População estimada"),
  #       labels = list(format = "{value:,.0f}")) %>%
  #     hc_plotOptions(column = list(grouping = TRUE)) %>%
  #     hc_add_series(name = "Masculino", data = serie_masculino, color = "#1f77b4") %>%
  #     hc_add_series(name = "Feminino",  data = serie_feminino,  color = "#ff7f0e") %>%
  #     hc_legend(enabled = TRUE) %>%
  #     hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y:,.0f}<br/>")
  # })
  # 
  # 
  # output$plot_2 <- renderHighchart({
  #   req(df$plot_negros)
  #   
  #   negros_2025 <- df$plot_negros %>%
  #     filter(ano == "2025")
  #   
  #   categorias <- levels(negros_2025$GrauInstrucao)
  #   
  #   serie_masculino <- round(negros_2025 %>%
  #                              filter(Sexo == "Masculino") %>%
  #                              arrange(GrauInstrucao) %>%
  #                              pull(Populacao))
  #   
  #   serie_feminino <- round(negros_2025 %>%
  #                             filter(Sexo == "Feminino") %>%
  #                             arrange(GrauInstrucao) %>%
  #                             pull(Populacao))
  #   
  #   highchart() %>%
  #     hc_chart(type = "column") %>%
  #     hc_title(text = "População Negra por Sexo e Grau de Instrução") %>%
  #     hc_xAxis(categories = categorias,
  #              title = list(text = "Grau de Instrução")) %>%
  #     hc_yAxis(
  #       title = list(text = "População estimada"),
  #       labels = list(format = "{value:,.0f}")) %>%
  #     hc_plotOptions(column = list(grouping = TRUE)) %>%
  #     hc_add_series(name = "Masculino", data = serie_masculino, color = "#1f77b4") %>%
  #     hc_add_series(name = "Feminino",  data = serie_feminino,  color = "#ff7f0e") %>%
  #     hc_legend(enabled = TRUE) %>%
  #     hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y:,.0f}<br/>")
  # })
  # 
  # output$plot_3 <- renderHighchart({
  #   req(df$plot_n_negros)
  #   
  #   nao_negros_2024 <- df$plot_n_negros %>%
  #     filter(ano == "2024")
  #   
  #   categorias <- levels(nao_negros_2024$GrauInstrucao)
  #   
  #   serie_masculino <- round(nao_negros_2024 %>%
  #                              filter(Sexo == "Masculino") %>%
  #                              arrange(GrauInstrucao) %>%
  #                              pull(Populacao))
  #   
  #   serie_feminino <- round(nao_negros_2024 %>%
  #                             filter(Sexo == "Feminino") %>%
  #                             arrange(GrauInstrucao) %>%
  #                             pull(Populacao))
  #   
  #   highchart() %>%
  #     hc_chart(type = "column") %>%
  #     hc_title(text = "População Não Negra por Sexo e Grau de Instrução") %>%
  #     hc_xAxis(categories = categorias,
  #              title = list(text = "Grau de Instrução")) %>%
  #     hc_yAxis(
  #       title = list(text = "População estimada"),
  #       labels = list(format = "{value:,.0f}")) %>%
  #     hc_plotOptions(column = list(grouping = TRUE)) %>%
  #     hc_add_series(name = "Masculino", data = serie_masculino, color = "#1f77b4") %>%
  #     hc_add_series(name = "Feminino",  data = serie_feminino,  color = "#ff7f0e") %>%
  #     hc_legend(enabled = TRUE) %>%
  #     hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y:,.0f}<br/>")
  # })
  # 
  # output$plot_4 <- renderHighchart({
  #   req(df$plot_n_negros)
  #   
  #   nao_negros_2025 <- df$plot_n_negros %>%
  #     filter(ano == "2025")
  #   
  #   categorias <- levels(nao_negros_2025$GrauInstrucao)
  #   
  #   serie_masculino <- round(nao_negros_2025 %>%
  #                              filter(Sexo == "Masculino") %>%
  #                              arrange(GrauInstrucao) %>%
  #                              pull(Populacao))
  #   
  #   serie_feminino <- round(nao_negros_2025 %>%
  #                             filter(Sexo == "Feminino") %>%
  #                             arrange(GrauInstrucao) %>%
  #                             pull(Populacao))
  #   
  #   highchart() %>%
  #     hc_chart(type = "column") %>%
  #     hc_title(text = "População Não Negra por Sexo e Grau de Instrução") %>%
  #     hc_xAxis(categories = categorias,
  #              title = list(text = "Grau de Instrução")) %>%
  #     hc_yAxis(
  #       title = list(text = "População estimada"),
  #       labels = list(format = "{value:,.0f}")) %>%
  #     hc_plotOptions(column = list(grouping = TRUE)) %>%
  #     hc_add_series(name = "Masculino", data = serie_masculino, color = "#1f77b4") %>%
  #     hc_add_series(name = "Feminino",  data = serie_feminino,  color = "#ff7f0e") %>%
  #     hc_legend(enabled = TRUE) %>%
  #     hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y:,.0f}<br/>")
  # })
}