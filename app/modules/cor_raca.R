Pcor_racaUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    
    fluidRow(
      box(
        title = h1('Cor/raca x Denuncias - Dados disk 100', align = 'center'), #trocar titulo
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
        title = h1('Distribuição de cor/raça 1/2026', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_1")), type = 1, color = "#ffae00", size = 2)
      ),
      
      box(
        title = h1('diferenca mesmo semestre ano passado', align = 'center'), #trocar titulo
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
        withSpinner(highchartOutput(ns("plot_3")), type = 1, color = "#ffae00", size = 2)
      )
    )
  )
}


Pcor_racaServer <- function(input, output, session) {
  
  ns <- session$ns
  
  # CARREGAMENTO DOS DADOS ----
  
  dados_cor_raca <- readxl::read_xlsx("dados/cor_raca.xlsx")
  
  source("functions/plot_linha.R")
  source("functions/plot_barra.R")
  
  # REATIVOS ----
  
  ## Filtros ----
  
  filtros <- reactiveValues(
    Regiao = NULL,
    Estado = NULL,
    Intermediaria = NULL,
    Imediata = NULL,
    Municipio = NULL
  )
  
  ## Dados ----
  
  df <- reactiveValues(
    filtrado = NULL
  )
  
  # UI DINÂMICA ----
  
  ## Região ----
  
  output$regiao_ui <- renderUI({
    
    req(dados_cor_raca)
    
    selectizeInput(
      ns("Regiao"),
      "Região",
      choices = c(
        "TODOS",
        sort(unique(dados_cor_raca$regiao_ibge))
      ),
      selected = "TODOS",
      multiple = TRUE,
      options = list(maxOptions = 10000)
    )
    
  })
  
  ## Estados ----
  
  output$estado_ui <- renderUI({
    
    req(dados_cor_raca)
    
    selectizeInput(
      ns("Estado"),
      "Estado",
      choices = c(
        "TODAS",
        sort(unique(dados_cor_raca$uf_final))
      ),
      selected = "TODAS",
      multiple = TRUE,
      options = list(maxOptions = 10000)
    )
    
  })
  
  ## Regiões intermediárias ----
  
  output$intermediaria_ui <- renderUI({
    
    req(dados_cor_raca)
    
    selectizeInput(
      ns("Intermediaria"),
      "Região Intermediária",
      choices = c(
        "TODAS",
        sort(unique(dados_cor_raca$rgint))
      ),
      selected = "TODAS",
      multiple = TRUE,
      options = list(maxOptions = 10000)
    )
    
  })
  
  ## Regiões imediatas ----
  
  output$imediata_ui <- renderUI({
    
    req(dados_cor_raca)
    
    selectizeInput(
      ns("Imediatas"),
      "Região Imediata",
      choices = c(
        "TODAS",
        sort(unique(dados_cor_raca$rgi))
      ),
      selected = "TODAS",
      multiple = TRUE,
      options = list(maxOptions = 10000)
    )
    
  })
  
  ## Municípios ----
  
  output$municipio_ui <- renderUI({
    
    req(dados_cor_raca)
    
    selectizeInput(
      ns("Municipio"),
      "Município",
      choices = c(
        "TODOS",
        sort(unique(dados_cor_raca$municipio_final))
      ),
      selected = "TODOS",
      multiple = TRUE,
      options = list(maxOptions = 10000)
    )
    
  })
  
  # VALIDAÇÕES ----
  
  ## Região ----
  
  observeEvent(input$Regiao, {
    
    if (
      !is.null(input$Regiao) &&
      "TODOS" %in% input$Regiao &&
      length(input$Regiao) > 1
    ) {
      
      sendSweetAlert(
        session,
        "Erro",
        "A opção 'TODOS' não pode ser combinada com outras regiões",
        type = "warning"
      )
      
      updateSelectizeInput(
        session,
        "Regiao",
        selected = "TODOS"
      )
      
    }
    
  })
  
  ## Estado ----
  
  observeEvent(input$Estado, {
    
    if (
      !is.null(input$Estado) &&
      "TODAS" %in% input$Estado &&
      length(input$Estado) > 1
    ) {
      
      sendSweetAlert(
        session,
        "Erro",
        "A opção 'TODAS' não pode ser combinada com outros estados",
        type = "warning"
      )
      
      updateSelectizeInput(
        session,
        "Estado",
        selected = "TODAS"
      )
      
    }
    
  })
  
  ## Região intermediária ----
  
  observeEvent(input$Intermediaria, {
    
    if (
      !is.null(input$Intermediaria) &&
      "TODAS" %in% input$Intermediaria &&
      length(input$Intermediaria) > 1
    ) {
      
      sendSweetAlert(
        session,
        "Erro",
        "A opção 'TODAS' não pode ser combinada com outras regiões intermediárias",
        type = "warning"
      )
      
      updateSelectizeInput(
        session,
        "Intermediaria",
        selected = "TODAS"
      )
      
    }
    
  })
  
  ## Região imediata ----
  
  observeEvent(input$Imediatas, {
    
    if (
      !is.null(input$Imediatas) &&
      "TODAS" %in% input$Imediatas &&
      length(input$Imediatas) > 1
    ) {
      
      sendSweetAlert(
        session,
        "Erro",
        "A opção 'TODAS' não pode ser combinada com outras regiões imediatas",
        type = "warning"
      )
      
      updateSelectizeInput(
        session,
        "Imediatas",
        selected = "TODAS"
      )
      
    }
    
  })
  
  ## Município ----
  
  observeEvent(input$Municipio, {
    
    if (
      !is.null(input$Municipio) &&
      "TODOS" %in% input$Municipio &&
      length(input$Municipio) > 1
    ) {
      
      sendSweetAlert(
        session,
        "Erro",
        "A opção 'TODOS' não pode ser combinada com outros municípios",
        type = "warning"
      )
      
      updateSelectizeInput(
        session,
        "Municipio",
        selected = "TODOS"
      )
      
    }
    
  })
  
  # ATUALIZAÇÃO DOS FILTROS ----
  
  ## Região ----
  
  observe({
    
    req(input$Regiao)
    
    filtros$Regiao <- if ("TODOS" %in% input$Regiao) {
      unique(dados_cor_raca$regiao_ibge)
    } else {
      input$Regiao
    }
    
  })
  
  ## Estado ----
  
  observe({
    
    req(input$Estado)
    
    filtros$Estado <- if ("TODAS" %in% input$Estado) {
      unique(dados_cor_raca$uf_final)
    } else {
      input$Estado
    }
    
  })
  
  ## Região intermediária ----
  
  observe({
    
    req(input$Intermediaria)
    
    filtros$Intermediaria <- if ("TODAS" %in% input$Intermediaria) {
      unique(dados_cor_raca$rgint)
    } else {
      input$Intermediaria
    }
    
  })
  
  ## Região imediata ----
  
  observe({
    
    req(input$Imediatas)
    
    filtros$Imediata <- if ("TODAS" %in% input$Imediatas) {
      unique(dados_cor_raca$rgi)
    } else {
      input$Imediatas
    }
    
  })
  
  ## Município ----
  
  observe({
    
    req(input$Municipio)
    
    filtros$Municipio <- if ("TODOS" %in% input$Municipio) {
      unique(dados_cor_raca$municipio_final)
    } else {
      input$Municipio
    }
    
  })
  
  # OUTPUTS ----
  
  ## Filtragem dos dados ----
  
  observe({
    
    req(
      filtros$Regiao,
      filtros$Estado,
      filtros$Intermediaria,
      filtros$Imediata,
      filtros$Municipio
    )
    
    dados_aux <- dados_cor_raca
    
    # Região
    dados_aux <- dados_aux %>%
      dplyr::filter(
        regiao_ibge %in% filtros$Regiao
      )
    
    # Estado
    dados_aux <- dados_aux %>%
      dplyr::filter(
        uf_final %in% filtros$Estado
      )
    
    # Região intermediária
    dados_aux <- dados_aux %>%
      dplyr::filter(
        rgint %in% filtros$Intermediaria
      )
    
    # Região imediata
    dados_aux <- dados_aux %>%
      dplyr::filter(
        rgi %in% filtros$Imediata
      )
    
    # Município
    dados_aux <- dados_aux %>%
      dplyr::filter(
        municipio_final %in% filtros$Municipio
      )
    
    # Salvar dados filtrados
    df$filtrado <- dados_aux
    
  })
  
  ## Gráfico 1 ----
  
  output$plot_1 <- renderHighchart({
    
    req(df$filtrado)
    
    validate(
      need(
        nrow(df$filtrado) > 0,
        "Nenhum registro encontrado para os filtros selecionados."
      )
    )
    
    dataplot <- df$filtrado %>%
      dplyr::filter(
        !is.na(data_atendimento),
        !is.na(cor_raca_padrao),
        data_atendimento == "01/2026"
      ) %>%
      dplyr::group_by(
        cor_raca_padrao,
        data_atendimento
      ) %>%
      dplyr::summarise(
        freq = sum(freq, na.rm = TRUE),
        .groups = "drop"
      )
    
    validate(
      need(
        nrow(dataplot) > 0,
        "Não existem dados para construir o gráfico."
      )
    )
    
    grafico_barras_horizontal(
      dados = dataplot,
      frequencia = freq,
      grupo = cor_raca_padrao,
      titulo_x = "Cor/raca",
      titulo_y = "Número de denúncias"
    )
    
  })
  
  ## Gráfico 2 ----
  
  output$plot_2 <- renderHighchart({
    
    req(df$filtrado)
    
    validate(
      need(
        nrow(df$filtrado) > 0,
        "Nenhum registro encontrado para os filtros selecionados."
      )
    )
    
    aux1 <- df$filtrado %>%
      dplyr::filter(
        !is.na(data_atendimento),
        !is.na(cor_raca_padrao),
        data_atendimento == "01/2026"
      ) %>%
      dplyr::group_by(
        cor_raca_padrao,
        data_atendimento
      ) %>%
      dplyr::summarise(
        freq = sum(freq, na.rm = TRUE),
        .groups = "drop"
      )
    
    aux2 <- df$filtrado %>%
      dplyr::filter(
        !is.na(data_atendimento),
        !is.na(cor_raca_padrao),
        data_atendimento == "01/2025"
      ) %>%
      dplyr::group_by(
        cor_raca_padrao,
        data_atendimento
      ) %>%
      dplyr::summarise(
        freq = sum(freq, na.rm = TRUE),
        .groups = "drop"
      )
    
    dataplot <- dplyr::full_join(
      aux1 %>%
        dplyr::select(cor_raca_padrao, freq_2026 = freq),
      aux2 %>%
        dplyr::select(cor_raca_padrao, freq_2025 = freq),
      by = "cor_raca_padrao"
    ) %>%
      dplyr::mutate(
        dplyr::across(
          c(freq_2026, freq_2025),
          ~ tidyr::replace_na(.x, 0)
        ),
        diferenca = freq_2026 - freq_2025
      ) %>%
      dplyr::arrange(dplyr::desc(diferenca))
    
    validate(
      need(
        nrow(dataplot) > 0,
        "Não existem dados para construir o gráfico."
      )
    )
    
    grafico_barras_horizontal(
      dados = dataplot,
      frequencia = diferenca,
      grupo = cor_raca_padrao,
      titulo_x = "Cor/raca",
      titulo_y = "variação"
    )
    
  })
  
  ## Gráfico 3 - Análise temporal ----
  
  output$plot_3 <- renderHighchart({
    
    req(df$filtrado)
    
    validate(
      need(
        nrow(df$filtrado) > 0,
        "Nenhum registro encontrado para os filtros selecionados."
      )
    )
    
    dataplot <- df$filtrado %>%
      dplyr::filter(
        !is.na(data_atendimento),
        !is.na(cor_raca_padrao)
      ) %>%
      dplyr::group_by(
        cor_raca_padrao,
        data_atendimento
      ) %>%
      dplyr::summarise(
        freq = sum(freq, na.rm = TRUE),
        .groups = "drop"
      )
    
    validate(
      need(
        nrow(dataplot) > 0,
        "Não existem dados para construir o gráfico."
      )
    )
    
    grafico_linha_grupo(
      dados = dataplot,
      x = data_atendimento,
      y = freq,
      grupo = cor_raca_padrao,
      titulo_x = "Mês/Ano",
      titulo_y = "Número de denúncias"
    )
    
  })
  
}