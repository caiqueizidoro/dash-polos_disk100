
grafico_linha_grupo <- function(
    dados,
    x,
    y,
    grupo,
    titulo = NULL,
    subtitulo = NULL,
    titulo_x = NULL,
    titulo_y = "Contagem",
    formato_y = "0,0",
    cores = NULL,
    legenda = TRUE,
    altura = NULL
) {
  
  # Validação dos pacotes
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("highcharter", quietly = TRUE) ||
      !requireNamespace("rlang", quietly = TRUE) ||
      !requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Instale dplyr, highcharter, rlang e htmlwidgets.")
  }
  
  # Captura das colunas
  x_col <- rlang::ensym(x)
  y_col <- rlang::ensym(y)
  grupo_col <- rlang::ensym(grupo)
  
  # Preparação dos dados
  dados_grafico <- dados %>%
    dplyr::transmute(
      periodo = as.character(!!x_col),
      contagem = as.numeric(!!y_col),
      grupo = as.character(!!grupo_col)
    ) %>%
    dplyr::filter(
      !is.na(periodo),
      !is.na(contagem),
      !is.na(grupo)
    )
  
  if (nrow(dados_grafico) == 0) {
    stop("Não existem observações válidas para construir o gráfico.")
  }
  
  # Extrai semestre e ano
  dados_grafico <- dados_grafico %>%
    dplyr::mutate(
      semestre = as.integer(sub("/.*$", "", periodo)),
      ano = as.integer(sub("^.*/", "", periodo))
    )
  
  # Valida o formato semestre/ano
  if (any(
    is.na(dados_grafico$semestre) |
    is.na(dados_grafico$ano) |
    !dados_grafico$semestre %in% c(1, 2)
  )) {
    stop("A variável temporal deve estar no formato semestre/ano (ex.: 01/2011 ou 02/2011).")
  }
  
  # Agregação
  dados_grafico <- dados_grafico %>%
    dplyr::group_by(grupo, periodo, semestre, ano) %>%
    dplyr::summarise(
      contagem = sum(contagem),
      .groups = "drop"
    )
  
  # Ordenação cronológica: ano e semestre
  ordem_periodos <- dados_grafico %>%
    dplyr::distinct(periodo, semestre, ano) %>%
    dplyr::arrange(ano, semestre) %>%
    dplyr::pull(periodo)
  
  # Ordem dos grupos
  grupos <- unique(dados_grafico$grupo)
  
  # Construção do gráfico
  grafico <- highcharter::highchart() %>%
    highcharter::hc_chart(
      type = "line",
      height = altura
    ) %>%
    highcharter::hc_title(
      text = titulo
    ) %>%
    highcharter::hc_subtitle(
      text = subtitulo
    ) %>%
    highcharter::hc_xAxis(
      categories = ordem_periodos,
      title = list(text = titulo_x),
      crosshair = TRUE
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = titulo_y),
      min = 0,
      allowDecimals = FALSE,
      labels = list(
        formatter = htmlwidgets::JS(
          "function() {
            return Highcharts.numberFormat(this.value, 0, ',', '.');
          }"
        )
      )
    ) %>%
    highcharter::hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE,
      valueDecimals = 0
    ) %>%
    highcharter::hc_legend(
      enabled = legenda
    ) %>%
    highcharter::hc_plotOptions(
      series = list(
        connectNulls = FALSE
      ),
      line = list(
        marker = list(
          enabled = TRUE,
          radius = 3
        ),
        lineWidth = 2
      )
    )
  
  # Uma série para cada grupo
  for (g in grupos) {
    
    serie <- dados_grafico %>%
      dplyr::filter(grupo == g)
    
    # Alinhamento dos valores ao eixo X
    valores <- rep(NA_real_, length(ordem_periodos))
    
    posicoes <- match(
      serie$periodo,
      ordem_periodos
    )
    
    valores[posicoes] <- serie$contagem
    
    grafico <- grafico %>%
      highcharter::hc_add_series(
        name = g,
        data = unname(valores),
        type = "line"
      )
  }
  
  # Paleta de cores
  if (!is.null(cores)) {
    grafico <- grafico %>%
      highcharter::hc_colors(cores)
  }
  
  return(grafico)
}
