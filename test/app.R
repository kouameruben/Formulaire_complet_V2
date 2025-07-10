library(shiny)
library(shinyjs)
library(sodium)

# Fake hash pour mot de passe "admin123"
mot_de_passe_hash <- password_store("admin123")

ui <- fluidPage(
  useShinyjs(),
  
  # UI de login
  div(id = "login_panel",
      wellPanel(
        textInput("login_user", "Utilisateur"),
        passwordInput("login_pass", "Mot de passe"),
        actionButton("login_btn", "Connexion")
      )
  ),
  
  # UI principale (cachée au départ)
  hidden(
    div(id = "app_main_ui",
        h2("Bienvenue dans l'application !"),
        verbatimTextOutput("role_affiche")
    )
  )
)

server <- function(input, output, session) {
  user_session <- reactiveValues(logged = FALSE, role = NULL)
  
  observeEvent(input$login_btn, {
    if (input$login_user == "admin" &&
        password_verify(mot_de_passe_hash, input$login_pass)) {
      
      user_session$logged <- TRUE
      user_session$role <- "admin"
      
      hide("login_panel")
      show("app_main_ui")
    } else {
      showNotification("Login ou mot de passe invalide", type = "error")
    }
  })
  
  output$role_affiche <- renderPrint({
    paste("Rôle :", user_session$role)
  })
}

shinyApp(ui, server)

