library(shiny)
library(bs4Dash)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinycssloaders)
library(dplyr)
library(readxl)
library(janitor)
library(stringr)
library(highcharter)
library(purrr)
library(DT)
library(tidyverse)
library(scales)
library(tidyr)

# # Carregar módulos
# source("modules/Psexo.R")
# source("modules/Pidade.R")
# source("modules/deficiente.R")
# source("modules/equipe.R")
# source("modules/cor_raca.R")
# source("modules/escolaridade.R")
# source("modules/grupo_vulneravel.R")
# source("modules/orientacao_sexual.R")
# source("modules/relacao_suspeito.R")
# source("modules/tipo_atendimento.R")

# Diagnóstico de carregamento
modulos <- c(
  "Psexo.R",
  "Pidade.R",
  "deficiente.R",
  "equipe.R",
  "cor_raca.R",
  "escolaridade.R",
  "grupo_vulneravel.R",
  "orientacao_sexual.R",
  "relacao_suspeito.R",
  "tipo_atendimento.R"
)

for (modulo in modulos) {
  
  caminho <- file.path("modules", modulo)
  
  cat("\nCarregando:", caminho, "\n")
  
  if (!file.exists(caminho)) {
    stop("Arquivo não encontrado: ", caminho)
  }
  
  tryCatch(
    source(caminho, local = .GlobalEnv),
    error = function(e) {
      stop(
        "Erro no módulo ", modulo, ": ",
        conditionMessage(e)
      )
    }
  )
  
  cat("OK:", modulo, "\n")
}

ui <- fluidPage(
  useShinyjs(),
  useShinyalert(force = TRUE),
  tags$head(
    tags$style(HTML("
      /* ... (seu código CSS para sidebar) ... */
      
      /* CSS para tela de login - AJUSTADO */
      .login-page {
        background-image: url('capa.jpeg');
        background-size: contain; /* Mudei de 'cover' para 'contain' */
        background-repeat: no-repeat;
        background-attachment: fixed;
        background-position: center center;
        background-color: #f8f9fa; /* Cor de fundo caso a imagem não preencha */
        height: 100vh;
        margin: 0;
        padding: 0;
        display: flex;
        justify-content: flex-end;
        align-items: flex-end;
      }
      
      #login_container {
        position: absolute;
        bottom: 30px; /* Reduzi a distância */
        right: 30px;  /* Reduzi a distância */
        padding: 15px;
        background-color: rgba(255, 255, 255, 0.1);
        border-radius: 10px;
      }
      
      #login_img_btn {
        cursor: pointer;
        transition: transform 0.2s;
        height: 100px !important; /* Diminuí o tamanho do botão */
      }
    "))
  ),
  uiOutput("main_ui")
)


server <- function(input, output, session) {
  
  user_logged <- reactiveVal(FALSE)
  
  #addResourcePath("www", "www")
  
  output$main_ui <- renderUI({
    
    if (!user_logged()) {
      
      # Tela de Login
      div(
        class = "login-page",
        div(
          id = "login_container",
          actionLink(
            inputId = "login_btn",
            label = tags$img(
              src = "Acesse-Aqui.jpeg",
              alt = "Entrar no Sistema",
              id = "login_img_btn",
              height = "80px"
            )
          )
        )
      )
      
    } else {
      
      # Interface principal após login
      bs4DashPage(
        fullscreen = TRUE,
        controlbar = NULL,
        
        header = bs4DashNavbar(title = "Categorias"),
        
        sidebar = bs4DashSidebar(
          collapsed = FALSE,
          skin = "light",
          status = "primary",
          title = "Menu",
          brandColor = "primary",
          
          bs4SidebarMenu(
            bs4SidebarMenuItem("Sexo", tabName = "Psexo", icon = icon("venus-mars")),
            bs4SidebarMenuItem("Faixa etária", tabName = "Pidade", icon = icon("calendar-alt")),
            bs4SidebarMenuItem("Pessoa possui Deficiência", tabName = "deficiente", icon = icon("wheelchair")),
            bs4SidebarMenuItem("Equipe", tabName = "equipe", icon = icon("users")),
            bs4SidebarMenuItem("Tipo Atendimento", tabName = "tipo_atendimento", icon = icon("clipboard-list")),
            bs4SidebarMenuItem("Cor/Raça", tabName = "cor_raca", icon = icon("palette")),
            bs4SidebarMenuItem("Escolaridade", tabName = "escolaridade", icon = icon("graduation-cap")),
            bs4SidebarMenuItem("Grupo Vulnerabilidade", tabName = "grupo_vulneravel", icon = icon("hands-helping")),
            bs4SidebarMenuItem("Orientação sexual", tabName = "orientacao_sexual", icon = icon("rainbow")),
            bs4SidebarMenuItem("Relação com o Suspeito", tabName = "relacao_suspeito", icon = icon("people-arrows"))
          )
        ),
        
        body = bs4DashBody(
          bs4TabItems(
            bs4TabItem(tabName = "Psexo", PsexoUI("Psexo")),
            bs4TabItem(tabName = "Pidade", PidadeUI("Pidade")),
            bs4TabItem(tabName = "deficiente", deficienteUI("deficiente")),
            bs4TabItem(tabName = "equipe", equipeUI("equipe")),
            bs4TabItem(tabName = "tipo_atendimento", Ptipo_atendimentoUI("tipo_atendimento")),
            bs4TabItem(tabName = "cor_raca", Pcor_racaUI("cor_raca")),
            bs4TabItem(tabName = "escolaridade", PescolaridadeUI("escolaridade")),
            bs4TabItem(tabName = "grupo_vulneravel", PgrupovUI("grupo_vulneravel")),
            bs4TabItem(tabName = "orientacao_sexual", PorientacaoUI("orientacao_sexual")),
            bs4TabItem(tabName = "relacao_suspeito", PsuspeitoUI("relacao_suspeito"))
          )
        ),
        
        footer = bs4DashFooter(
          left = "Fonte: Levantamento realizado pelo Observatório Brasileiro de Políticas Públicas com a População em Situação de Rua/POLOS-UFMG a partir da consulta ao CECAD (Mês de Referência: Dezembro)",
          right = Sys.Date()
        )
      )
    }
  })
  
  observeEvent(input$login_btn, {
    shinyalert("Login", "Você acessou o sistema!", type = "success")
    user_logged(TRUE)
  })
  
  callModule(PsexoServer, "Psexo")
  callModule(PidadeServer, "Pidade")
  callModule(deficienteServer, "deficiente")
  callModule(equipeServer, "equipe")
  callModule(PescolaridadeServer, "escolaridade")
  callModule(PgrupovServer, "grupo_vulneravel")
  callModule(PorientacaoServer, "orientacao_sexual")
  callModule(Pcor_racaServer, "cor_raca")
  callModule(PsuspeitoServer, "relacao_suspeito")
  callModule(Ptipo_atendimentoServer, "tipo_atendimento")
}

shinyApp(ui = ui, server = server)