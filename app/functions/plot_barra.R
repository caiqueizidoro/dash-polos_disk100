
grafico_barras_horizontal <- function(
    dados,
    grupo,
    frequencia,
    titulo = NULL,
    subtitulo = NULL,
    titulo_x = NULL,
    titulo_y = NULL,
    formato_x = "0,0",
    cor = NULL,
    ordenar = TRUE,
    decrescente = TRUE,
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
  grupo_col <- rlang::ensym(grupo)
  freq_col <- rlang::ensym(frequencia)
  
  # Preparação dos dados
  dados_grafico <- dados %>%
    dplyr::transmute(
      grupo = as.character(!!grupo_col),
      frequencia = as.numeric(!!freq_col)
    ) %>%
    dplyr::filter(
      !is.na(grupo),
      !is.na(frequencia)
    ) %>%
    dplyr::group_by(grupo) %>%
    dplyr::summarise(
      frequencia = sum(frequencia),
      .groups = "drop"
    )
  
  if (nrow(dados_grafico) == 0) {
    stop("Não existem observações válidas para construir o gráfico.")
  }
  
  # Ordenação das barras
  if (ordenar) {
    dados_grafico <- dados_grafico %>%
      dplyr::arrange(
        if (decrescente) dplyr::desc(frequencia) else frequencia
      )
  }
  
  # Construção do gráfico
  grafico <- highcharter::highchart() %>%
    highcharter::hc_chart(
      type = "bar",
      height = altura
    ) %>%
    highcharter::hc_title(
      text = titulo
    ) %>%
    highcharter::hc_subtitle(
      text = subtitulo
    ) %>%
    highcharter::hc_xAxis(
      categories = dados_grafico$grupo,
      title = list(text = titulo_x),
      reversed = TRUE
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = titulo_y),
      labels = list(enabled = FALSE),
      min = 0
    ) %>%
    highcharter::hc_tooltip(
      pointFormat = "<b>{point.y:,.0f}</b>"
    ) %>%
    highcharter::hc_plotOptions(
      series = list(
        borderWidth = 0,
        dataLabels = list(
          enabled = TRUE,
          format = "{point.y:,.0f}",
          style = list(
            fontWeight = "bold",
            textOutline = "none"
          )
        )
      )
    ) %>%
    highcharter::hc_add_series(
      name = "Frequência",
      data = dados_grafico$frequencia,
      color = cor,
      showInLegend = FALSE
    )
  
  return(grafico)
}
