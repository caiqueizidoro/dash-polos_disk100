equipeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
        .card-container {
          display: flex;
          flex-wrap: wrap;
          gap: 20px;
          justify-content: center;
        }

        .member-card {
          border: 1px solid #ccc;
          border-radius: 12px;
          padding: 20px;
          width: 300px;
          text-align: center;
          box-shadow: 2px 2px 6px rgba(0,0,0,0.1);
        }

        .member-name {
          font-size: 20px;
          font-weight: bold;
        }

        .member-role {
          font-size: 15px;
          color: #555;
          margin-bottom: 15px;
          list-style-type: none;
          padding-left: 0;
        }

        .member-role li::before {
          content: '• ';
          color: #1E90FF;
          font-weight: bold;
        }

        .icon-btn {
          display: inline-block;
          margin: 5px;
          padding: 10px 12px;
          border-radius: 50%;
          background-color: #1E90FF;
          color: white !important;
          text-decoration: none;
          font-size: 18px;
          transition: background-color 0.3s;
        }

        .icon-btn:hover {
          background-color: #0d6efd;
        }
      "))
    ),
    
    h2("Nossa Equipe", align = "center"),
    
    div(class = "card-container",
        
        # Coordenador
        div(class = "member-card",
            tags$img(src = "img/andre.jpeg", height = "100px", class = "member-photo"),
            div(class = "member-name", "André Luiz Freitas Dias"),
            tags$ul(class = "member-role",
                    tags$li("Organização e Coordenação Técnica/Científica")
            ),
            a(icon("file-alt"), href = "https://pos.direito.ufmg.br/andredias/", class = "icon-btn", target = "_blank")
           
        ),
        # Coordenador
        div(class = "member-card",
            tags$img(src = "img/maria.jpeg", height = "100px", class = "member-photo"),
            div(class = "member-name", "Maria Fernanda Salcedo Repolês"),
            tags$ul(class = "member-role",
                    tags$li("Organização e Coordenação Técnica/Científica")
            ),
            a(icon("file-alt"), href = "https://pos.direito.ufmg.br/mariafernandarepoles/", class = "icon-btn", target = "_blank")
            
        ),
            # Membro1
            div(class = "member-card",
                tags$img(src = "img/andrezza.jpeg", height = "100px", class = "member-photo"),
                div(class = "member-name", "Andrezza Cristina Santos"),
                tags$ul(class = "member-role",
                        tags$li("Pesquisador extensionista")
                ),
                a(icon("linkedin"), href = "https://www.linkedin.com/in/andrezza-cristina-santos-batista-b7828723a", class = "icon-btn", target = "_blank"),
                a(icon("file-alt"), href = "https://lattes.cnpq.br/9300533003335560", class = "icon-btn", target = "_blank"),
                a(icon("envelope"), href = "andrezza-II@hotmail.com", class = "icon-btn")
            ),
        # Membro 2
        div(class = "member-card",
            tags$img(src = "img/caique.jpeg", height = "100px", class = "member-photo"),
            div(class = "member-name", "Caique Izidoro Alvarenga"),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            a(icon("linkedin"), href = "https://linkedin.com/in/caiqueizidr", class = "icon-btn", target = "_blank"),
            a(icon("file-alt"), href = "https://lattes.cnpq.br/9300533003335560", class = "icon-btn", target = "_blank"),
            a(icon("envelope"), href = "caiqueizidoro369@gmail.com", class = "icon-btn")
        ),
        div(class = "member-card",
            tags$img(src = "img/cristiano.jpeg", height = "100px", class = "member-photo"),
            div(class = "member-name", "Cristiano Pereira da Silva"),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            a(icon("envelope"), href = "cristpsilva@gmail.com", class = "icon-btn")
        ),
        # Membro 3
        div(class = "member-card",
            tags$img(src = "img/debora.jpeg", height = "100px", class = "member-photo"),
            div(class = "member-name", "Deborah Aparecida Guimarães Teixeira "),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            
            a(icon("envelope"), href = "Deborahguimaraes30@gmail.com", class = "icon-btn"),
            a(icon("file-alt"), href = "http://lattes.cnpq.br/4857560113520256", class = "icon-btn", target = "_blank")
            
        ),
        # Membro 3
        div(class = "member-card",
            tags$img(src = "img/diogo.jpeg", height = "100px", class = "member-photo"),
            div(class = "member-name", "Diogo Gomes Pires"),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            a(icon("linkedin"), href = "https://www.linkedin.com/in/diogo-gomes-pires-802076206", class = "icon-btn", target = "_blank"),
            a(icon("file-alt"), href = "http://lattes.cnpq.br/5116149150044825", class = "icon-btn", target = "_blank"),
            a(icon("envelope"), href = "diogogomes1109@gmail.com", class = "icon-btn")
        ),
        # Membro 3
        div(class = "member-card",
            tags$img(src = "img/evelyn.jpeg", height = "100px", class = "member-photo"),
            div(class = "member-name", "Evelyn Laura Alves de Souza Costa"),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            a(icon("linkedin"), href = "https://www.linkedin.com/in/evelyn-laura-costa-38967b2b5", class = "icon-btn", target = "_blank"),
            a(icon("file-alt"), href = "http://lattes.cnpq.br/0782935478233117", class = "icon-btn", target = "_blank"),
            a(icon("envelope"), href = "evelynlauraasc@gmail.com", class = "icon-btn")
        ),
        # Membro 3
        div(class = "member-card",
            tags$img(src = "img/felipe.jpeg", height = "100px", class = "member-photo"),
            div(class = "member-name", "Felipe Gonçalves Balestrini"),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            a(icon("file-alt"), href = "http://lattes.cnpq.br/4298407275016858", class = "icon-btn", target = "_blank"),
            a(icon("envelope"), href = "balestrinifelipe@gmail.com", class = "icon-btn")
        ),
        
        # Membro 4
        div(class = "member-card",
            tags$img(src = "img/gabriel.jpeg", height = "100px", class = "member-photo"),
            div(class = "member-name", "Gabriel Moreira da Silva"),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            a(icon("linkedin"), href = "https://www.linkedin.com/in/gabriel-moreira-73341a23b/", class = "icon-btn", target = "_blank"),
            a(icon("file-alt"), href = "http://lattes.cnpq.br/987654321", class = "icon-btn", target = "_blank"),
            a(icon("envelope"), href = "gmoreira.silva2005@gmail.com", class = "icon-btn")
        ),
        # Membro 5
        div(class = "member-card",
            tags$img(src = "img/irlana.jpeg", height = "100px", class = "member-photo"),
            div(class = "member-name", "Irlana dos Santos"),
            tags$ul(class = "member-role",
                    tags$li("Pesquisador extensionista")
            ),
            a(icon("linkedin"), href = "https://www.linkedin.com/in/irlana-silva-905457185", class = "icon-btn", target = "_blank"),
            a(icon("file-alt"), href = "https://lattes.cnpq.br/9300533003335560", class = "icon-btn", target = "_blank"),
            a(icon("envelope"), href = "irlannasilva1720@gmail.com", class = "icon-btn")
        )
        
        #pra colocar mais só copiar a colar a div toda, NÃO ESQUECER DA VIRGULA DEPOIS DAS DIVs
    )
  )
}



equipeServer <- function(input, output, session, dados) {

}
