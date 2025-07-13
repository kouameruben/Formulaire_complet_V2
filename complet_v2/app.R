# Chargement des bibliothÃ¨ques
library(shiny)
library(shinyalert)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(readr)
library(readxl)
library(DT)
library(openxlsx)
library(purrr)
library(rsconnect)
library(ggplot2)
library(DBI)
library(RSQLite)
library(pool)
library(shinydashboard)
library(tidyr)
library(shinydashboard)
library(plotly)
library(scales)
library(zoo)
library(sodium)
#library(rmarkdown)
library(knitr)
library(fs)




# Cree ou ouvre la base SQLite avec pool

db_file <- "bd_fatal_projet2.sqlite"

pool <- dbPool(
  drv         = RSQLite::SQLite(),
  dbname      = db_file,
  synchronous = "normal",
  timeout     = 15000L
)

conn <- dbConnect(RSQLite::SQLite(), db_file)

# Mode WAL pour performance/concurrence et cles etrangeres
dbExecute(pool, "PRAGMA journal_mode = WAL;")
dbExecute(pool, "PRAGMA foreign_keys = ON;")


# on charge *une seule fois* :
clients_df        <- dbGetQuery(pool, "SELECT * FROM clients;")
produits_df       <- dbGetQuery(pool, "SELECT * FROM produits;")
commerciaux_df    <- dbGetQuery(pool, "SELECT * FROM commerciaux;")
entrepots_df      <- dbGetQuery(pool, "SELECT * FROM entrepots;")
ventes_df         <- dbGetQuery(pool, "SELECT * FROM ventes;")
objectifs_df      <- dbGetQuery(pool, "SELECT * FROM objectifs;")
obj_journalier_df <- dbGetQuery(pool, "SELECT * FROM objectifs_journalier;")

# Chargement des donnees geographiques
donnees_geographiques <- read_excel("worldcities.xlsx")

######## Interface utilisateur ########
ui <- fluidPage(
  useShinyalert(force = TRUE),
  useShinyjs(),
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      body { background-color: #f8f9fa; }
      .well { 
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        padding: 15px; 
        margin-bottom: 20px;
      }
      .btn { 
        border-radius: 5px;
        font-weight: 500;
        transition: all 0.3s ease;
        width: 100%;
        margin-top: 10px;
      }
      .btn:hover { transform: translateY(-1px); }
      .form-control { border-radius: 4px; }
      table.dataTable tbody tr:hover { background-color: #f0f8ff !important; }
      .dataTables_filter input { border-radius: 20px; }
      .selectize-input { border-radius: 4px; height: 40px; }
      .shiny-spinner-container { margin-top: 50px; }
      img { 
        max-height: 100px !important; 
        border-radius: 4px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .dt-body-center { text-align: center; }
    "))
  )
  #,
  #### Utilisation d'un tabsetPanel pour regrouper les differents modules ####
  
  # div(
  #  id = "login_panel",
  #   wellPanel(
  #      textInput("login_user", "Nom d'utilisateur"),
  #     passwordInput("login_pass", "Mot de passe"),
  #     actionButton("login_btn", "Connexion")
  #    ))
  ,
  
  # hidden(div(id = "app_main_ui", h2("Bienvenue dans l'application !"),  verbatimTextOutput("role_affiche"),
  #### Onglet 0 : Gestion utilisateur ####
  
  tabsetPanel(
    
    
    
    #### Onglet 1 : Gestion Clients ####
    tabPanel("Gestion Clients",
             titlePanel(
               tags$div(
                 icon("users", class = "fa-2x"),
                 "Gestion Clients",
                 style = "display: flex; align-items: center; gap: 15px; color: #2c3e50;"
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 tags$div(class = "well",
                          h4(icon("user-plus"), "Nouveau Client", style = "color: #3498db;"),
                          textInput("name", tags$span("Nom", tags$span("*", style = "color: red;"))),
                          textInput("prenom", "Prenom"),
                          textInput("email", "Email"),
                          textInput("numero", tags$span("Telephone", tags$span("*", style = "color: red;"))),
                          selectInput("pays", "Pays", choices = unique(donnees_geographiques$pays)),
                          selectInput("ville", "Ville", choices = NULL),
                          textInput("quartier", "Quartier", placeholder = "Quartier"),
                          textAreaInput("adresse_livraison", "Adresse", placeholder = "Adresse complete de livraison...", rows = 3),
                          textAreaInput("preferences", "Preferences", placeholder = "Preferences particulieres...", rows = 3),
                          actionButton("submit", "Enregistrer", class = "btn-primary", icon = icon("save"))
                 ),
                 tags$div(class = "well",
                          h4(icon("edit"), "Modification", style = "color: #f39c12;"),
                          textInput("edit_id", "ID Client", placeholder = "Saisir l'ID..."),
                          actionButton("edit", "Modifier", class = "btn-warning", icon = icon("edit"))
                 ),
                 tags$div(class = "well",
                          h4(icon("trash"), "Suppression", style = "color: #e74c3c;"),
                          textInput("delete_id", "ID Client", placeholder = "Saisir l'ID..."),
                          actionButton("delete", "Supprimer", class = "btn-danger", icon = icon("trash-alt"))
                 ),
                 tags$div(class = "well",
                          h4(icon("file-export"), "Export", style = "color: #27ae60;"),
                          downloadButton("export", "Excel", class = "btn-success", icon = icon("file-excel"))
                 )
               ),
               mainPanel(
                 width = 8,
                 DTOutput("table_clients") %>% withSpinner(type = 6, color = "#3498db", hide.ui = FALSE),
                 tags$hr(),
                 tags$div(style = "font-size: 0.9em; color: #7f8c8d;",
                          icon("info-circle"), "Nombre total de clients :", textOutput("total_clients", inline = TRUE)
                 )
               )
             )
    ),
    
    #### Onglet 2 : Gestion Produits ####
    
    tabPanel("Gestion Produits",
             titlePanel(
               tags$div(
                 icon("shopping-cart", class = "fa-2x"),
                 "Gestion des Produits",
                 style = "display: flex; align-items: center; gap: 15px; color: #2c3e50;"
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 tags$div(class = "well",
                          h4(icon("plus-circle"), "Nouveau Produit", style = "color: #3498db;"),
                          textInput("nom_produit", tags$span("Nom du produit", tags$span("*", style = "color: red;"))),
                          selectInput("fournisseur_id", tags$span("ID Fournisseur", tags$span("*", style = "color: red;")), choices = NULL),
                          fileInput("photo", "Photo (PNG/JPG)", accept = c("image/png", "image/jpeg")),
                          selectInput("categorie", "Categorie", choices = c("Savon solide", "Savon liquide", "Gel douche", "Savon en poudre")),
                          textAreaInput("description", "Description", rows = 3),
                          selectInput("couleur", "Couleur", choices = c("Rouge","Bleu","Vert","Jaune","Noir","Blanc","Gris","Orange","Violet","Rose")),
                          numericInput("prix_achat", "Prix d'achat", value = 0, min = 0, step = 0.01),
                          textInput("marque", "Marque"),
                          numericInput("volume", "Volume (ml)", value = 100, min = 0),
                          numericInput("poids", "Poids (g)", value = 100, min = 0),
                          textInput("materiau", "Materiau"),
                          actionButton("submit_produit", "Enregistrer", class = "btn-primary", icon = icon("save"))
                 ),
                 
                 tags$div(class="well",
                          h4(icon("edit"), "Modification"),
                          textInput("edit_id_produit","ID Produit", placeholder = "EX: PROD-001"),
                          actionButton("edit_produit","Modifier", class="btn-warning", icon = icon("edit"))
                 )
                 
                 
                 ,
                 tags$div(class = "well",
                          h4(icon("trash"), "Suppression", style = "color: #e74c3c;"),
                          textInput("delete_id_produit", "ID Produit", placeholder = "PROD-001"),
                          actionButton("delete_btn", "Supprimer", class = "btn-danger", icon = icon("trash-alt"))
                 ),
                 tags$div(class = "well",
                          h4(icon("file-export"), "Export", style = "color: #27ae60;"),
                          downloadButton("export_produits_excel", "Excel", class = "btn-success", icon = icon("file-excel")),
                          downloadButton("export_produits_csv", "CSV", class = "btn-secondary", icon = icon("file-csv"))
                 )
               ),
               mainPanel(
                 width = 8,
                 DTOutput("table_produits") %>% withSpinner(type = 6, color = "#3498db"),
                 tags$hr()
               )
             )
    )
    ,
    
    #### Onglet 3 : Gestion Fournisseurs ####
    
    tabPanel("Gestion Fournisseurs",
             titlePanel(
               tags$div(
                 icon("truck", class = "fa-2x"),
                 "Gestion Fournisseurs",
                 style = "display: flex; align-items: center; gap: 15px; color: #2c3e50;"
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 tags$div(class = "well",
                          h4(icon("plus-circle"), "Nouveau Fournisseur", style = "color: #3498db;"),
                          textInput("fournisseur_nom", 
                                    tags$span("Nom Fournisseur", tags$span("*", style = "color: red;")),
                                    placeholder = "ABC Entreprise"),
                          textInput("fournisseur_email",
                                    tags$span("Email", tags$span("*", style = "color: red;")),
                                    placeholder = "contact@abc.com"),
                          textInput("fournisseur_telephone",
                                    tags$span("Telephone", tags$span("*", style = "color: red;")),
                                    placeholder = "+225 07 12 34 56 78"),
                          selectInput("fournisseur_pays", "Pays", choices = unique(donnees_geographiques$pays)),
                          selectInput("fournisseur_type", "Type", choices = c("Local", "International")),
                          textInput("fournisseur_adresse", "Adresse", placeholder = "Rue 123, Ville"),
                          textInput("fournisseur_site", "Site Web", placeholder = "https://www.abc.com"),
                          textInput("fournisseur_responsable", "Nom Responsable"),
                          textInput("fournisseur_contact", "Contact Responsable"),
                          selectInput("fournisseur_statut", "Statut", choices = c("Actif", "Inactif")),
                          actionButton("submit_fournisseur", "Enregistrer", class = "btn-primary", icon = icon("save"))
                 ),
                 tags$div(class = "well",
                          h4(icon("edit"), "Modification", style = "color: #f39c12;"),
                          textInput("edit_fournisseur_id", "ID Fournisseur", placeholder = "ID-12345"),
                          actionButton("edit_fournisseur", "Modifier", class = "btn-warning", icon = icon("edit"))
                 ),
                 tags$div(class = "well",
                          h4(icon("trash"), "Suppression", style = "color: #e74c3c;"),
                          textInput("delete_fournisseur_id", "ID Fournisseur", placeholder = "ID-12345"),
                          actionButton("delete_fournisseur", "Supprimer", class = "btn-danger", icon = icon("trash-alt"))
                 ),
                 tags$div(class = "well",
                          h4(icon("file-export"), "Export", style = "color: #27ae60;"),
                          downloadButton("export_fournisseur", "Excel", class = "btn-success", icon = icon("file-excel"))
                 )
               ),
               mainPanel(
                 width = 8,
                 DTOutput("table_fournisseurs") %>% withSpinner(type = 6, color = "#3498db"),
                 tags$hr(),
                 tags$div(style = "font-size: 0.9em; color: #7f8c8d;",
                          icon("box"), "Fournisseurs actifs : ", textOutput("total_actifs", inline = TRUE),
                          tags$br(),
                          icon("archive"), "Fournisseurs inactifs : ", textOutput("total_inactifs", inline = TRUE)
                 )
               )
             )
    ),
    
    #### Onglet 4 : Gestion Commerciaux ####
    
    tabPanel("Gestion Commerciaux",
             titlePanel(
               tags$div(
                 icon("users", class = "fa-2x"),
                 "Gestion Commerciaux",
                 style = "display: flex; align-items: center; gap: 15px; color: #2c3e50;"
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 tags$div(class = "well",
                          h4(icon("user-plus"), "Nouveau Commercial", style = "color: #3498db;"),
                          textInput("commerciaux_nom", tags$span("Nom", tags$span("*", style = "color: red;")), 
                                    placeholder = "Nom complet"),
                          selectInput("commerciaux_genre", tags$span("Genre", tags$span("*", style = "color: red;")), 
                                      choices = c("Masculin", "Feminin", "Autre")),
                          textInput("commerciaux_adresse", tags$span("Adresse", tags$span("*", style = "color: red;")), 
                                    placeholder = "Adresse complete"),
                          textInput("commerciaux_tel", tags$span("Telephone", tags$span("*", style = "color: red;")), 
                                    placeholder = "Ex: +255 07 ..."),
                          actionButton("commerciaux_submit", "Enregistrer", class = "btn-primary", icon = icon("save"))
                 ),
                 tags$div(class = "well",
                          h4(icon("edit"), "Modification", style = "color: #f39c12;"),
                          textInput("commerciaux_edit_id", "ID Commercial", placeholder = "Saisir l'ID..."),
                          actionButton("commerciaux_edit", "Modifier", class = "btn-warning", icon = icon("edit"))
                 ),
                 tags$div(class = "well",
                          h4(icon("trash"), "Suppression", style = "color: #e74c3c;"),
                          textInput("commerciaux_delete_id", "ID Commercial", placeholder = "Saisir l'ID..."),
                          actionButton("commerciaux_delete", "Supprimer", class = "btn-danger", icon = icon("trash-alt"))
                 ),
                 tags$div(class = "well",
                          h4(icon("file-export"), "Export", style = "color: #27ae60;"),
                          downloadButton("export_commerciaux", "Excel", class = "btn-success", icon = icon("file-excel"))
                 )
               ),
               mainPanel(
                 width = 8,
                 DTOutput("table_commerciaux") %>% withSpinner(type = 6, color = "#3498db", hide.ui = FALSE),
                 tags$hr(),
                 tags$div(style = "font-size: 0.9em; color: #7f8c8d;",
                          icon("info-circle"), "Nombre total de commerciaux :", textOutput("total_commerciaux", inline = TRUE))
               )
             )
    ),
    
    
    #### Onglet 5 : Gestion Stock ####
    
    tabPanel("Gestion Stock",
             titlePanel(
               tags$div(
                 icon("warehouse", class = "fa-2x"),
                 "Gestion des Stocks",
                 style = "display: flex; align-items: center; gap: 15px; color: #2c3e50;"
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 tags$div(class = "well",
                          h4(icon("plus-circle"), "Mouvement de Stock", style = "color: #3498db;"),
                          selectInput("stock_product", "Produit", choices = NULL),
                          selectInput("stock_depot", "Entrepot", choices = NULL),
                          selectInput("stock_movement", "Type de mouvement", choices = c("Livraison", "sortir")),
                          numericInput("stock_qty", "Quantite", value = 0, min = 0),
                          dateInput("stock_date", "Date du mouvement", value = Sys.Date()),
                          numericInput("stock_prix", "Prix d'achat", value = 0, min = 0, step = 0.01),
                          actionButton("submit_stock", "Enregistrer le mouvement", class = "btn-primary", icon = icon("save"))
                 ),
                 tags$div(class = "well",
                          h4(icon("exchange-alt"), "Transfert entre entrepots", style = "color: #3498db;"),
                          selectInput("transfer_product", "Produit", choices = NULL),
                          selectInput("transfer_from",   "Depuis", choices = NULL),
                          selectInput("transfer_to",     "Vers",   choices = NULL),
                          numericInput("transfer_qty", "Quantite", value = 0, min = 0),
                          actionButton("submit_transfer", "Transferer", class = "btn-warning", icon = icon("exchange-alt"))
                 )
               ),
               mainPanel(
                 width = 8,
                 selectInput("stock_history_product", "Filtrer par produit", choices = NULL),
                 DTOutput("table_stock") %>% withSpinner(type = 6, color = "#3498db"),
                 tags$hr())
             )
    ),
    
    
    
    ### Onglet 6 : l'onglet Entrepots ---#####
    
    tabPanel("Gestion Entrepots",
             titlePanel(
               tags$div(
                 icon("warehouse", class = "fa-2x"),
                 "Gestion des Entrepots",
                 style = "display: flex; align-items: center; gap: 15px; color: #2c3e50;"
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 tags$div(class = "well",
                          h4(icon("plus-circle"), "Nouveau Entrepot", style = "color: #3498db;"),
                          textInput("entrepot_nom", "Nom de l'entrepot"),
                          textInput("entrepot_responsable", "Nom du responsable"),
                          textInput("entrepot_tel", "Telephone"),
                          textInput("entrepot_adresse", "Adresse"),
                          actionButton("submit_entrepot", "Enregistrer", class = "btn-primary", icon = icon("save"))
                 ),
                 
                 tags$div(class = "well",
                          h4(icon("edit"), "Modification Entrepot", style = "color: #f39c12;"),
                          textInput("edit_entrepot_id", "ID Entrepot", placeholder = "ENT-001"),
                          actionButton("edit_entrepot", "Modifier", class = "btn-warning", icon = icon("edit"))
                 ),
                 
                 tags$div(class = "well",
                          h4(icon("trash"), "Suppression Entrepot", style = "color: #e74c3c;"),
                          textInput("delete_entrepot_id", "ID Entrepot", placeholder = "ENT-001"),
                          actionButton("delete_entrepot", "Supprimer", class = "btn-danger", icon = icon("trash-alt"))
                 )
                 
                 
                 
               ),
               mainPanel(
                 width = 8,
                 DTOutput("table_entrepots") %>% withSpinner(type = 6, color = "#3498db")
               )
             )
    )
    ,
    #### Onglet 7 : Gestion objectifs ####
    
    tabPanel(
      "Gestion Objectifs",
      titlePanel("Objectifs commerciaux"),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          selectInput("obj_commercial", "Commercial", choices = NULL),
          selectInput("obj_annee", "Annee", choices = 2020:2050),
          selectInput("obj_mois", "Mois", choices = sprintf("%02d", 1:12)),
          numericInput("obj_quantite", "Quantite cible (mois)", value = 0, min = 0),
          numericInput("obj_montant", "Montant cible (mois)", value = 0, min = 0, step = 0.01),
          actionButton("save_objectif", "Enregistrer", class = "btn-primary"),
          actionButton("edit_objectif", "Modifier", class = "btn-warning"),
          actionButton("delete_objectif", "Supprimer", class = "btn-danger")
        ),
        mainPanel(
          DTOutput("table_objectifs") %>% withSpinner(color="#3498db"),
          hr(),
          DTOutput("table_journalier") %>% withSpinner(color="#3498db")
        )
      )
    ),
    
    #### Onglet 8  : Gestion Ventes ####
    
    tabPanel("Gestion Ventes",
             titlePanel(
               tags$div(
                 icon("shopping-bag", class = "fa-2x"),
                 "Gestion des Ventes",
                 style = "display: flex; align-items: center; gap: 15px; color: #2c3e50;"
               )
             ),
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 tags$div(class = "well",
                          h4(icon("plus-circle"), "Nouvelle Vente", style = "color: #3498db;"),
                          selectInput("id_client", "ID Client", choices = NULL),
                          textInput("client_nom", "Nom Client", value = "", placeholder = "Nom"),
                          textInput("client_tel", "Telephone", value = "", placeholder = "Telephone"),
                          textInput("client_adresse", "Adresse Client", value = "", placeholder = "Adresse"),
                          selectInput("vente_entrepot", "Entrepot", choices = NULL),
                          selectInput("id_produit", "ID Produit", choices = NULL),
                          textInput("produit_nom", "Nom Produit", value = "", placeholder = "Nom Produit"),
                          selectInput("id_commercial", "Commercial (ID - Nom)", choices = NULL),
                          numericInput("prix_vente","Prix Vente (unitaire)", value = 0, min = 0),
                          selectInput("canal", "Canal de Vente", choices = c("PUB", "commercial", "recommandation")),
                          numericInput("quantite_vente", "Quantite", value = 1, min = 1),
                          numericInput("remise", "Remise (%)", value = 0, min = 0, max = 100),
                          textAreaInput("commentaire", "Commentaire", rows = 3),
                          selectInput("moy_livraison", "Moyen de Paiement", choices = c("ORANGE MONAY","MTN MONAY","MOOV MONAY","WAVE","LIQUIDE","CARTE BANCAIRE", "AUTRE")),
                          h4(textOutput("montant_total_display"), style = "color: #e74c3c; font-weight: bold;"),
                          br(),
                          actionButton("submit_vente","Enregistrer Vente",class = "btn-primary",icon = icon("save"),disabled = "disabled")),
                 
                 
                 tags$div(class = "well",
                          h4(icon("edit"), "Modification Vente", style = "color: #f39c12;"),
                          textInput("edit_vente_id", "ID Vente", placeholder = "VENTE-001"),
                          actionButton("edit_vente_btn", "Modifier Vente", class = "btn-warning", icon = icon("edit"))
                 ),
                 
                 tags$div(class = "well",
                          h4(icon("trash"), "Suppression Vente", style = "color: #e74c3c;"),
                          textInput("delete_vente_id", "ID Vente", placeholder = "VENTE-001"),
                          actionButton("delete_vente_btn", "Supprimer Vente", class = "btn-danger", icon = icon("trash-alt"))
                 ),
                 
                 tags$div(class = "well",
                          h4(icon("file-export"), "Export", style = "color: #27ae60;"),
                          downloadButton("export_vente_btn", "Excel", class = "btn-success", icon = icon("file-excel"))
                 ),
                 
                 tags$div(class = "well",
                          h4(icon("file-pdf"), "ReÃ§u vente PDF", style = "color: #e74c3c;"),
                          downloadButton("telecharger_recu", "Telecharger le reÃ§u PDF", class = "btn-danger"))
                 
                 
                 
               ),
               
               mainPanel(
                 width = 8,
                 DTOutput("table_ventes") %>% withSpinner(type = 6, color = "#3498db")
               )
             ),
             tags$head(
               tags$style(HTML("
                 .well { padding: 15px; margin-bottom: 20px; }
                 .btn { width: 100%; margin-top: 10px; }
                 .selectize-input { height: 40px; }
                 .shiny-spinner-container { margin-top: 50px; }
               "))
             )
    ),
    
    
    ## Onglet 9  : Tableau de bord----
    
    tabPanel(
      "Tableau de bord",
      titlePanel("Tableau de bord"),
      
      # Ã¢â‚¬â€ filtres Ã¢â‚¬â€
      fluidRow(
        column(3, dateRangeInput("dash_period",   "Periode", start = Sys.Date()-30, end = Sys.Date())),
        column(3, selectInput("dash_commercial","Commercial", choices = c("Tous", commerciaux_df$id_commercial), multiple=TRUE, selected="Tous")),
        column(3, selectInput("dash_produit",   "Produit",    choices = c("Tous", produits_df$product_id),      multiple=TRUE, selected="Tous")),
        column(3, selectInput("dash_entrepot",  "Entrepot",   choices = c("Tous", entrepots_df$entrepot_id),    multiple=TRUE, selected="Tous"))
      ),
      
      # Ã¢â‚¬â€ KPIs (2 lignes Ãƒâ€” 4) Ã¢â‚¬â€
      fluidRow(
        valueBoxOutput("vb_ca_realise", width=3),
        valueBoxOutput("vb_ca_attendu", width=3),
        valueBoxOutput("vb_taux_ca",    width=3),
        valueBoxOutput("vb_nb_rupture",     width=3)
      ),
      fluidRow(
        valueBoxOutput("vb_ventes_realise", width=3),
        valueBoxOutput("vb_ventes_attendu", width=3),
        valueBoxOutput("vb_taux_ventes",    width=3),
        valueBoxOutput("vb_nb_disponible",  width=3)
      ),
      
      # Ã¢â‚¬â€ Courbe des ventes par jour Ã¢â‚¬â€
      fluidRow(
        column(12, h4("Ãƒâ€°volution quotidienne : Ventes vs Objectifs"),
               plotlyOutput("sales_by_day", height = "450px"))
      ),
      hr(),
      # Ã¢â‚¬â€ Repartition en camemberts Ã¢â‚¬â€
      fluidRow(
        column(4, plotlyOutput("pie_entrepot", height="250px")),
        column(4, ""),
        column(4, plotlyOutput("pie_produit",  height="250px"))
        
      ),
      
      hr(),
      DTOutput("dash_table_detail") %>% withSpinner(color="#3498db")
    )
    
    
    # ----
    
  )
)
#)
#)
######## Serveur ########

server <- function(input, output, session) {
  # user_session <- reactiveValues(logged = FALSE, role = NULL, id = NULL, depot = NULL)
  # #chargement des tables ----
  # clients <-  reactiveVal(dbGetQuery(pool, "SELECT * FROM clients;"))
  # data_produits <- reactiveVal(dbGetQuery(pool, "SELECT * FROM produits;"))
  # fournisseurs <- reactiveVal(dbGetQuery(pool, "SELECT * FROM fournisseurs;"))
  # commerciaux_data <- reactiveVal(dbGetQuery(pool, "SELECT * FROM commerciaux;"))
  # entrepots <- reactiveVal({dbGetQuery(pool, "SELECT * FROM entrepots;")})
  # stock <- reactiveVal(dbGetQuery(pool, "SELECT * FROM stock;"))
  # stock_movements <- reactiveVal(dbGetQuery(pool, "SELECT * FROM stock_movements;"))
  # ventes <- reactiveVal(dbGetQuery(pool, "SELECT * FROM ventes;"))
  # objectifs       <- reactiveVal(dbGetQuery(pool, "SELECT * FROM objectifs;"))
  # obj_journalier  <- reactiveVal(dbGetQuery(pool, "SELECT * FROM objectifs_journalier;"))
  # dernier_recu_path <- reactiveVal(NULL)
  # sanitize_latex <- function(text) {
  #   text <- gsub("([%&$#_{}])", "\\\\\\1", text) # caractÃ¨res speciaux LaTeX
  #   text <- gsub("~", "\\\\textasciitilde{}", text)
  #   text <- gsub("\\^", "\\\\textasciicircum{}", text)
  #   text <- iconv(text, from = "", to = "ASCII//TRANSLIT") # accents
  #   return(text)
  # }
  # 
  # 
  # 
  # 
  # #### I) MODULE GESTION CLIENTS #####
  # #1.0) Mise Ã  jour des villes selon le pays selectionne ----
  # 
  # observe({
  #   req(input$pays)
  #   villes <- donnees_geographiques %>%
  #     filter(pays == input$pays) %>%
  #     pull(ville) %>%
  #     unique()
  #   updateSelectInput(session, "ville", choices = villes)
  # })
  # 
  # # Notification personnalisee pour les clients
  # myNotification <- function(msg, type = "message") {
  #   notif_type <- switch(type,
  #                        "message" = "success",
  #                        "warning" = "warning",
  #                        "error" = "error")
  #   shinyalert(
  #     title = "",
  #     text = msg,
  #     type = notif_type,
  #     timer = 3000,
  #     animation = "slide-from-top",
  #     closeOnClickOutside = TRUE
  #   )
  # }
  # 
  # 
  # # 1.1) Ajout d'un client----
  # observeEvent(input$submit, {
  #   tryCatch({
  #     req(input$name, input$numero)
  #     if (nzchar(input$email) && !grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", input$email)) {
  #       stop("L'adresse e-mail n'est pas valide.")
  #     }
  #     new_id <- paste0(sample(c(0:9, LETTERS), 7, replace = TRUE), collapse = "")
  #     
  #     params <- list(
  #       new_id, input$name, input$prenom, input$email, input$numero,
  #       input$pays, input$ville, input$quartier,
  #       input$adresse_livraison, input$preferences, as.character(Sys.Date()))
  #     
  #     
  #     poolWithTransaction(pool,function(conn)  {
  #       dbExecute(conn,
  #                 "INSERT INTO clients (
  #          customer_id, name, prenom, email, numero,
  #          pays, ville, quartier,
  #          adresse_livraison, preferences, date_enregistrement
  #        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);",params = params)
  #     })
  #     
  #     
  #     
  #     
  #     # Miroir memoire
  #     df <- clients()
  #     df <- bind_rows(df, tibble(
  #       customer_id        = new_id,
  #       name               = input$name,
  #       prenom             = input$prenom,
  #       email              = input$email,
  #       numero             = input$numero,
  #       pays               = input$pays,
  #       ville              = input$ville,
  #       quartier           = input$quartier,
  #       adresse_livraison  = input$adresse_livraison,
  #       preferences        = input$preferences,
  #       date_enregistrement = as.character(Sys.Date())
  #     ))
  #     clients(df)
  #     
  #     shinyalert("Client ajoute !", type = "success")
  #     
  #     # Reinitialisation des champs
  #     
  #     updateTextInput(session, "name", value = "")
  #     updateTextInput(session, "prenom", value = "")
  #     updateTextInput(session, "email", value = "")
  #     updateTextInput(session, "numero", value = "")
  #     updateTextInput(session, "pays", value = "")
  #     updateTextInput(session, "ville", value = "")
  #     updateTextInput(session, "quartier", value = "")
  #     updateTextAreaInput(session, "adresse", value = "")
  #     updateTextAreaInput(session, "preferences", value = "")
  #   }, error = function(e) {
  #     shinyalert(paste("Erreur :", e$message), type = "error")
  #   })
  # })
  # 
  # # 1.2.1) Modification d'un client-------
  # 
  # observeEvent(input$edit_pays, {
  #   req(input$edit_pays)
  #   villes_edit <- donnees_geographiques %>%
  #     filter(pays == input$edit_pays) %>%
  #     pull(ville) %>%
  #     unique()
  #   updateSelectInput(
  #     session,
  #     "edit_ville",
  #     choices  = villes_edit,
  #     selected = if (input$edit_ville %in% villes_edit) input$edit_ville else villes_edit[1]
  #   )
  # })
  # 
  # 
  # 
  # observeEvent(input$edit, {
  #   req(input$edit_id)
  #   df <- clients()
  #   row <- df %>% filter(customer_id == input$edit_id)
  #   if (nrow(row)==0) {
  #     shinyalert("ID introuvable", type = "error"); return()
  #   }
  #   
  #   showModal(modalDialog(
  #     title = "Modifier Client",
  #     textInput("edit_name", "Nom *", value = row$name),
  #     textInput("edit_prenom", "Prenom", value = row$prenom),
  #     textInput("edit_email", "Email", value = row$email),
  #     textInput("edit_numero", "Numero", value = row$numero),
  #     selectInput("edit_pays", "Pays", choices = unique(donnees_geographiques$pays), selected = row$pays),
  #     selectInput("edit_ville", "Ville", choices = NULL,     selected = row$ville),
  #     textInput("edit_quartier", "Quartier", value = row$quartier),
  #     textAreaInput("edit_adresse_livraison", "Adresse de livraison", value = row$adresse_livraison),
  #     textAreaInput("edit_preferences", "Preferences", value = row$preferences),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("confirm_edit_client", "Confirmer", class="btn-success"))
  #     ,
  #     size = "l"
  #   ))
  # })
  # 
  # # 1.2.2) Observeur qui capture la validation dans le modal----
  # 
  # observeEvent(input$confirm_edit_client, {
  #   # on utilise input$mod_* pour les nouveaux libelles
  #   params <- list(
  #     input$edit_name,
  #     input$edit_prenom,
  #     input$edit_email,
  #     input$edit_numero,
  #     input$edit_pays,
  #     input$edit_ville,
  #     input$edit_quartier,
  #     input$edit_adresse_livraison,
  #     input$edit_preferences,
  #     as.character(Sys.Date()),
  #     input$edit_id
  #   )
  #   
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn, "
  #       UPDATE clients SET
  #         name               = ?,
  #         prenom             = ?,
  #         email              = ?,
  #         numero             = ?,
  #         pays               = ?,
  #         ville              = ?,
  #         quartier           = ?,
  #         adresse_livraison  = ?,
  #         preferences        = ?,
  #         date_enregistrement = ?
  #       WHERE customer_id = ?;
  #     ", params = params)
  #     })
  #     
  #     
  #     
  #     # Miroir memoire
  #     
  #     df <- clients()
  #     
  #     idx <- which(df$customer_id == input$edit_id)
  #     
  #     df[idx, c("name","prenom","email","numero",
  #               "pays","ville","quartier",
  #               "adresse_livraison","preferences",
  #               "date_enregistrement")] <-  params[1:10]
  #     
  #     clients(df)
  #     shinyalert("Client mis Ã  jour !", type = "success")
  #   }, error = function(e) {
  #     shinyalert(paste("Erreur :", e$message), type = "error")
  #   })
  # })
  # 
  # ##1.3) Suppression d'un client ----
  # 
  # observeEvent(input$delete, {
  #   req(input$edit_id)
  #   showModal(modalDialog(
  #     title = "Confirmer suppression",
  #     paste("Supprimer le client", input$edit_id, "?"),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("do_delete", "Oui, supprimer", class = "btn-danger")
  #     )
  #   ))
  # })
  # 
  # observeEvent(input$do_delete, {
  #   removeModal()
  #   tryCatch({
  #     poolWithTransaction(pool,function(conn)  {
  #       dbExecute(conn, "
  #         DELETE FROM clients WHERE customer_id = ?;
  #       ", params = list(input$edit_id))
  #     })
  #     # Miroir memoire
  #     clients(clients() %>% filter(customer_id != input$edit_id))
  #     shinyalert("Client supprime !", type = "success")
  #   }, error = function(e) {
  #     shinyalert(paste("Erreur :", e$message), type = "error")
  #   })
  # })
  # 
  # # 1.4) Export des clients----
  # 
  # output$export <- downloadHandler(
  #   filename = function() {
  #     paste("clients-", Sys.Date(), ".xlsx", sep = "")
  #   },
  #   content = function(file) {
  #     write.xlsx(clients(), file)
  #   }
  # )
  # 
  # # 1.5) Affichage du tableau clients----
  # output$table_clients <- renderDT({
  #   datatable(clients(), options = list(searching = TRUE, paging = TRUE, pageLength = 10))
  # })
  # 
  # output$total_clients <- renderText({
  #   nrow(clients())
  # })
  # 
  # #II) MODULE GESTION PRODUITS #####
  # 
  # # 2.0.0) Chargement des fournisseurs pour selecteur
  # 
  # 
  # observe({updateSelectInput(session, "fournisseur_id",choices = fournisseurs()$supplier_id)})
  # 
  # next_product_id <- reactive({
  #   ids <- data_produits()$product_id
  #   if (length(ids) == 0) return("PROD-001")
  #   nmax <- max(as.numeric(gsub("PROD-", "", ids)))
  #   sprintf("PROD-%03d", nmax + 1)
  # })
  # 
  # 
  # # 2.0.1) Validation du prix
  # validate_prix <- function(val) {
  #   if (val <= 0) stop("Le prix d'achat doit ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Âªtre strictement positif.")
  # }
  # 
  # # 2.1) Ajout d'un produit----
  # 
  # 
  # observeEvent(input$submit_produit, {
  #   tryCatch({
  #     req(input$nom_produit, input$fournisseur_id, input$prix_achat> 0)
  #     
  #     validate_prix(input$prix_achat)
  #     
  #     # Gestion de l'image
  #     
  #     img_path <- NA_character_
  #     
  #     if (!is.null(input$photo) && nzchar(input$photo$name)) {
  #       dir.create(file.path("www","images"), showWarnings = FALSE, recursive = TRUE)
  #       ext <- tools::file_ext(input$photo$name)
  #       fname <- paste0(next_product_id(), ".", ext)
  #       file.copy(input$photo$datapath, file.path("www","images", fname))
  #       img_path <- file.path("images", fname)
  #     }
  #     
  #     
  #     pid <- next_product_id()
  #     params <- list(
  #       pid,
  #       input$fournisseur_id,
  #       input$nom_produit,
  #       input$categorie,
  #       input$description,
  #       input$couleur,
  #       input$prix_achat,
  #       input$marque,
  #       input$volume,
  #       input$poids,
  #       input$materiau,
  #       as.character(Sys.Date()),
  #       img_path
  #     )
  #     
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn, "
  #     INSERT INTO produits (
  #       product_id, supplier_id, nom_produit, categorie,
  #       description, couleur, prix_achat, marque,
  #       volume, poids, materiau, date_ajout, image
  #     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
  #   ", params = params)
  #     })
  #     
  #     # Miroir en memoire
  #     data_produits(bind_rows(data_produits(), tibble(
  #       product_id = pid,
  #       supplier_id= input$fournisseur_id,
  #       nom_produit = input$nom_produit,
  #       categorie   = input$categorie,
  #       description = input$description,
  #       couleur     = input$couleur,
  #       prix_achat  = input$prix_achat,
  #       marque      = input$marque,
  #       volume      = input$volume,
  #       poids       = input$poids,
  #       materiau    = input$materiau,
  #       date_ajout  = as.character(Sys.Date()),
  #       image       = img_path
  #     )))
  #     
  #     showNotification("Produit ajoute !", type = "message")
  #     
  #     # Reset UI
  #     updateTextInput(session, "nom_produit", value = "")
  #     updateSelectInput(session, "fournisseur_id", selected = NULL)
  #     reset("photo")
  #     updateTextAreaInput(session, "description", value = "")
  #     updateNumericInput(session, "prix_achat", value = 0)
  #     updateTextInput(session, "marque", value = "")
  #     updateNumericInput(session, "volume", value = 100)
  #     updateNumericInput(session, "poids", value = 100)
  #     updateTextInput(session, "materiau", value = "")
  #     
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type = "error")
  #   })
  # })
  # 
  # #2.2.1) Modification (modal + UPDATE)-----
  # 
  # observeEvent(input$edit_produit, {
  #   req(input$edit_id_produit)
  #   row <- data_produits() %>% filter(product_id == input$edit_id_produit)
  #   if (nrow(row)==0) {
  #     showNotification("Produit introuvable", type = "error"); return()
  #   }
  #   
  #   showModal(modalDialog(
  #     title = paste("Modifier produit", input$edit_id_produit),
  #     textInput("mod_nom_produit","Nom",      value = row$nom_produit),
  #     selectInput("mod_fournisseur_id","ID Fournisseur",
  #                 choices = fournisseurs()$supplier_id,
  #                 selected = row$supplier_id),
  #     selectInput("mod_categorie","Categorie",
  #                 choices = c("Savon solide","Savon liquide","Gel douche","Savon en poudre"),
  #                 selected = row$categorie),
  #     textAreaInput("mod_description","Description",value = row$description),
  #     selectInput("mod_couleur","Couleur",
  #                 choices = c("Rouge","Bleu","Vert","Jaune","Noir","Blanc","Gris","Orange","Violet","Rose"),
  #                 selected = row$couleur),
  #     numericInput("mod_prix_achat","Prix d'achat",value = row$prix_achat, min = 0, step = 0.01),
  #     textInput("mod_marque","Marque", value = row$marque),
  #     numericInput("mod_volume","Volume", value = row$volume, min = 0),
  #     numericInput("mod_poids","Poids", value = row$poids, min = 0),
  #     textInput("mod_materiau","Materiau", value = row$materiau),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("confirm_edit_produit","Confirmer", class="btn-success")
  #     ), size = "l"
  #   ))
  # })
  # 
  # #2.2.2) validation modification
  # 
  # observeEvent(input$confirm_edit_produit, {
  #   params <- list(
  #     input$mod_nom_produit,
  #     input$mod_fournisseur_id,
  #     input$mod_categorie,
  #     input$mod_description,
  #     input$mod_couleur,
  #     input$mod_prix_achat,
  #     input$mod_marque,
  #     input$mod_volume,
  #     input$mod_poids,
  #     input$mod_materiau,
  #     as.character(Sys.Date()),
  #     input$edit_id_produit
  #   )
  #   
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn, "
  #     UPDATE produits SET
  #       nom_produit = ?,
  #       supplier_id = ?,
  #       categorie   = ?,
  #       description = ?,
  #       couleur     = ?,
  #       prix_achat  = ?,
  #       marque      = ?,
  #       volume      = ?,
  #       poids       = ?,
  #       materiau    = ?,
  #       date_ajout  = ?
  #     WHERE product_id = ?;
  #   ", params = params)
  #     })
  #     
  #     df <- data_produits()
  #     idx <- which(df$product_id == input$edit_id_produit)
  #     df[idx, c("nom_produit","supplier_id","categorie","description",
  #               "couleur","prix_achat","marque","volume","poids",
  #               "materiau","date_ajout")] <- params[1:11]
  #     data_produits(df)
  #     
  #     removeModal()
  #     showNotification("Produit modifie !", type = "message")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type = "error")
  #   })
  # })
  # 
  # 
  # 
  # 
  # # 2.3) Suppression d'un produit----
  # 
  # observeEvent(input$delete_btn, {
  #   req(input$delete_id_produit)
  #   showModal(modalDialog(
  #     title = "Confirmer suppression",
  #     paste("Supprimer produit", input$delete_id_produit, "?"),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("confirm_delete_produit","Supprimer", class="btn-danger")
  #     )
  #   ))
  # })
  # observeEvent(input$confirm_delete_produit, {
  #   removeModal()
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn,
  #                 "DELETE FROM produits WHERE product_id = ?;",
  #                 params = list(input$delete_id_produit)
  #       )
  #     })
  #     data_produits(data_produits() %>% filter(product_id != input$delete_id_produit))
  #     showNotification("Produit supprime !", type = "message")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type = "error")
  #   })
  # })
  # observeEvent(input$confirm_delete, {
  #   updated <- data_produits() %>% filter(product_id != input$delete_id_produit)
  #   write_csv(updated, "produits.csv")
  #   data_produits(updated)
  #   removeModal()
  #   showNotification("Produit supprime.", type = "message")
  # })
  # 
  # # 2.4) Affichage du tableau produits----
  # 
  # output$table_produits <- renderDT({
  #   df <- data_produits() %>%
  #     mutate(image = ifelse(is.na(image)|image=="", "Pas d'image",
  #                           paste0('<img src="', image, '" style="max-height:100px;">')))
  #   datatable(df, escape = FALSE,
  #             options = list(scrollX = TRUE, pageLength = 10))
  # })
  # 
  # 
  # output$export_produits_excel <- downloadHandler(
  #   filename = function() paste0("produits_", Sys.Date(), ".xlsx"),
  #   content  = function(file) writexl::write_xlsx(data_produits(), path = file)
  # )
  # 
  # ##### III)  MODULE GESTION FOURNISSEURS #####
  # # 3.1) Rendu du tableau----
  # output$table_fournisseurs <- renderDT({
  #   datatable(fournisseurs(), options = list(pageLength=10))
  # })
  # 
  # # 3.2) Export Excel----
  # output$export_fournisseur <- downloadHandler(
  #   filename = function() paste0("fournisseurs_", Sys.Date(), ".xlsx"),
  #   content  = function(file) writexl::write_xlsx(fournisseurs(), file)
  # )
  # 
  # # 3.3) Ajout (INSERT)----
  # 
  # observeEvent(input$submit_fournisseur, {
  #   req(input$fournisseur_nom, input$fournisseur_telephone)
  #   new_id <- paste0("FOUR-", format(Sys.time(), "%Y%m%d-%H%M%S"))
  #   params <- list(
  #     new_id,
  #     input$fournisseur_nom,
  #     input$fournisseur_email,
  #     input$fournisseur_telephone,
  #     input$fournisseur_pays,
  #     input$fournisseur_type,
  #     input$fournisseur_adresse,
  #     input$fournisseur_site,
  #     input$fournisseur_responsable,
  #     input$fournisseur_contact,
  #     input$fournisseur_statut,
  #     as.character(Sys.Date())
  #   )
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn, "
  #       INSERT INTO fournisseurs (
  #         supplier_id, fournisseur_nom, fournisseur_email,
  #         fournisseur_telephone, fournisseur_pays, fournisseur_type,
  #         fournisseur_adresse, fournisseur_site,
  #         fournisseur_responsable, fournisseur_contact,
  #         fournisseur_statut, date_creation
  #       ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
  #     ", params = params)
  #     })
  #     # miroir en memoire
  #     df <- fournisseurs()
  #     df <- bind_rows(df, tibble(
  #       supplier_id             = new_id,
  #       fournisseur_nom         = input$fournisseur_nom,
  #       fournisseur_email       = input$fournisseur_email,
  #       fournisseur_telephone   = input$fournisseur_telephone,
  #       fournisseur_pays        = input$fournisseur_pays,
  #       fournisseur_type        = input$fournisseur_type,
  #       fournisseur_adresse     = input$cfg_adresse,
  #       fournisseur_site        = input$fournisseur_site,
  #       fournisseur_responsable = input$fournisseur_responsable,
  #       fournisseur_contact     = input$fournisseur_contact,
  #       fournisseur_statut      = input$fournisseur_statut,
  #       date_creation           = as.character(Sys.Date())
  #     ))
  #     fournisseurs(df)
  #     showNotification("Fournisseur ajoute !", type="message")
  #     # reset
  #     updateTextInput(session, "fournisseur_nom", "")
  #     updateTextInput(session, "fournisseur_email", "")
  #     updateTextInput(session, "fournisseur_telephone", "")
  #     updateTextInput(session, "fournisseur_pays", "")
  #     updateSelectInput(session, "fournisseur_type", selected = "Local")
  #     updateTextInput(session, "cfg_adresse", "")
  #     updateTextInput(session, "fournisseur_site", "")
  #     updateTextInput(session, "fournisseur_responsable", "")
  #     updateTextInput(session, "fournisseur_contact", "")
  #     updateSelectInput(session, "fournisseur_statut", selected = "Actif")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # # 3.4) Chargement pour modification (modal)----
  # observeEvent(input$edit_fournisseur, {
  #   req(input$edit_fournisseur_id)
  #   row <- fournisseurs() %>% filter(supplier_id == input$edit_fournisseur_id)
  #   if (nrow(row)==0) {
  #     showNotification("ID introuvable", type="error"); return()
  #   }
  #   showModal(modalDialog(
  #     title = paste("Modifier", input$edit_fournisseur_id),
  #     textInput("mod_nom",      "Nom *",            value = row$fournisseur_nom),
  #     textInput("mod_email",    "Email",            value = row$fournisseur_email),
  #     textInput("mod_tel",      "Telephone *",      value = row$fournisseur_telephone),
  #     textInput("mod_pays",     "Pays",             value = row$fournisseur_pays),
  #     selectInput("mod_type",   "Type",
  #                 choices=c("Local","International"),
  #                 selected=row$fournisseur_type),
  #     textInput("mod_adresse",  "Adresse",          value = row$fournisseur_adresse),
  #     textInput("mod_site",     "Site Web",         value = row$fournisseur_site),
  #     textInput("mod_resp",     "Nom Responsable",  value = row$fournisseur_responsable),
  #     textInput("mod_contact",  "Contact Responsable",value=row$fournisseur_contact),
  #     selectInput("mod_statut", "Statut",
  #                 choices=c("Actif","Inactif"),
  #                 selected=row$fournisseur_statut),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("cfg_confirm_mod","Valider", class="btn-success")
  #     ),
  #     size="l"
  #   ))
  # })
  # 
  # # 3.5) Enregistrement de la modification (UPDATE)----
  # 
  # observeEvent(input$cfg_confirm_mod, {
  #   params <- list(
  #     input$mod_nom,
  #     input$mod_email,
  #     input$mod_tel,
  #     input$mod_pays,
  #     input$mod_type,
  #     input$mod_adresse,
  #     input$mod_site,
  #     input$mod_resp,
  #     input$mod_contact,
  #     input$mod_statut,
  #     as.character(Sys.Date()),
  #     input$edit_fournisseur_id
  #   )
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn, "
  #       UPDATE fournisseurs SET
  #         fournisseur_nom         = ?,
  #         fournisseur_email       = ?,
  #         fournisseur_telephone   = ?,
  #         fournisseur_pays        = ?,
  #         fournisseur_type        = ?,
  #         fournisseur_adresse     = ?,
  #         fournisseur_site        = ?,
  #         fournisseur_responsable = ?,
  #         fournisseur_contact     = ?,
  #         fournisseur_statut      = ?,
  #         date_creation           = ?
  #       WHERE supplier_id = ?;
  #     ", params = params)
  #     })
  #     # miroir en memoire
  #     df <- fournisseurs()
  #     idx <- which(df$supplier_id == input$edit_fournisseur_id)
  #     df[idx, c("fournisseur_nom","fournisseur_email",
  #               "fournisseur_telephone","fournisseur_pays",
  #               "fournisseur_type","fournisseur_adresse",
  #               "fournisseur_site","fournisseur_responsable",
  #               "fournisseur_contact","fournisseur_statut",
  #               "date_creation")] <- params[1:11]
  #     fournisseurs(df)
  #     removeModal()
  #     showNotification("Fournisseur modifie !", type="message")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # 
  # 
  # # 3.6) Suppression (DELETE)----
  # 
  # observeEvent(input$delete_fournisseur, {
  #   req(input$delete_fournisseur_id)
  #   showModal(modalDialog(
  #     title = "Confirmer suppression",
  #     paste("Supprimer fournisseur", input$delete_fournisseur_id, "?"),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("cfg_confirm_del","Supprimer", class="btn-danger")
  #     )
  #   ))
  # })
  # observeEvent(input$cfg_confirm_del, {
  #   removeModal()
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn,
  #                 "DELETE FROM fournisseurs WHERE supplier_id = ?;",
  #                 params = list(input$delete_fournisseur_id)
  #       )
  #     })
  #     fournisseurs(fournisseurs() %>%
  #                    filter(supplier_id != input$delete_fournisseur_id))
  #     showNotification("Fournisseur supprime !", type="message")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # 
  # ##### IV)  MODULE GESTION COMMERCIAUX #####
  # 
  # # 4.1) Affichage-----
  # output$table_commerciaux <- renderDT({
  #   datatable(commerciaux_data(), options = list(pageLength = 10))
  # })
  # 
  # # 4.2) Export----
  # output$export_commerciaux <- downloadHandler(
  #   filename = function() paste0("commerciaux_", Sys.Date(), ".xlsx"),
  #   content  = function(file) writexl::write_xlsx(commerciaux_data(), file)
  # )
  # 
  # 
  # # 4.3) Ajout (INSERT)-----
  # 
  # observeEvent(input$commerciaux_submit, {
  #   req(input$commerciaux_nom, input$commerciaux_genre, input$commerciaux_tel)
  #   new_id <- paste0("COM-", paste0(sample(c(0:9, LETTERS), 6, TRUE), collapse=""))
  #   params <- list(
  #     new_id,
  #     input$commerciaux_nom,
  #     input$commerciaux_genre,
  #     input$commerciaux_adresse,
  #     input$commerciaux_tel,
  #     as.character(Sys.Date())
  #   )
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn, "
  #       INSERT INTO commerciaux (
  #         id_commercial, nom, genre, adresse, tel, date_enregistrement
  #       ) VALUES (?, ?, ?, ?, ?, ?);
  #     ", params = params)
  #     })
  #     # miroir memoire
  #     df <- commerciaux_data()
  #     df <- bind_rows(df, tibble(
  #       id_commercial      = new_id,
  #       nom                = input$commerciaux_nom,
  #       genre              = input$commerciaux_genre,
  #       adresse            = input$commerciaux_adresse,
  #       tel                = input$commerciaux_tel,
  #       date_enregistrement = as.character(Sys.Date())
  #     ))
  #     commerciaux_data(df)
  #     showNotification("Commercial ajoute !", type="message")
  #     # reset UI
  #     updateTextInput(session, "commerciaux_nom", "")
  #     updateSelectInput(session, "commerciaux_genre", selected = "Masculin")
  #     updateTextInput(session, "commerciaux_adresse", "")
  #     updateTextInput(session, "commerciaux_tel", "")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # 
  # # 4.4) Chargement pour modification (modal)-----
  # 
  # observeEvent(input$commerciaux_edit, {
  #   req(input$commerciaux_edit_id)
  #   row <- commerciaux_data() %>% filter(id_commercial == input$commerciaux_edit_id)
  #   if (nrow(row)==0) {
  #     showNotification("ID introuvable", type="error"); return()
  #   }
  #   showModal(modalDialog(
  #     title = paste("Modifier commercial", input$commerciaux_edit_id),
  #     textInput("mod_com_nom",    "Nom *",     value = row$nom),
  #     selectInput("mod_com_genre","Genre *",
  #                 choices = c("Masculin","Feminin","Autre"),
  #                 selected = row$genre),
  #     textInput("mod_com_adresse","Adresse",   value = row$adresse),
  #     textInput("mod_com_tel",    "Telephone *", value = row$tel),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("com_confirm_mod","Mettre Ã  jour", class="btn-success")
  #     ),
  #     size="l"
  #   ))
  # })
  # 
  # # 4.5) Enregistrement de la modif (UPDATE)----
  # 
  # observeEvent(input$com_confirm_mod, {
  #   params <- list(
  #     input$mod_com_nom,
  #     input$mod_com_genre,
  #     input$mod_com_adresse,
  #     input$mod_com_tel,
  #     as.character(Sys.Date()),
  #     input$commerciaux_edit_id
  #   )
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn, "
  #       UPDATE commerciaux SET
  #         nom                = ?,
  #         genre              = ?,
  #         adresse            = ?,
  #         tel                = ?,
  #         date_enregistrement = ?
  #       WHERE id_commercial = ?;
  #     ", params = params)
  #     })
  #     # miroir memoire
  #     df <- commerciaux_data()
  #     idx <- which(df$id_commercial == input$commerciaux_edit_id)
  #     df[idx, c("nom","genre","adresse","tel","date_enregistrement")] <- params[1:5]
  #     commerciaux_data(df)
  #     removeModal()
  #     showNotification("Commercial modifie !", type="message")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # # 4.6) Suppression (DELETE)----
  # 
  # 
  # observeEvent(input$commerciaux_delete, {
  #   req(input$commerciaux_edit_id)
  #   showModal(modalDialog(
  #     title = "Confirmer suppression",
  #     paste("Supprimer commercial", input$commerciaux_edit_id, "?"),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("com_confirm_del","Supprimer", class="btn-danger")
  #     )
  #   ))
  # })
  # observeEvent(input$com_confirm_del, {
  #   removeModal()
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn,
  #                 "DELETE FROM commerciaux WHERE id_commercial = ?;",
  #                 params = list(input$commerciaux_edit_id)
  #       )
  #     })
  #     commerciaux_data(commerciaux_data() %>%
  #                        filter(id_commercial != input$commerciaux_edit_id))
  #     showNotification("Commercial supprime !", type="message")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # ##### V) gestion des entrepots   #####
  # 
  # # 5.1) Affichage du tableau -----
  # 
  # output$table_entrepots <- renderDT({datatable(entrepots(), options = list(pageLength=10)) })
  # 
  # # 5.2) Export Excel -----
  # output$etp_export <- downloadHandler(
  #   filename = function() paste0("entrepots_", Sys.Date(), ".xlsx"),
  #   content  = function(file) writexl::write_xlsx(entrepots(), file)
  # )
  # 
  # # 5.3) Ajout (INSERT) -----
  # 
  # observeEvent(input$submit_entrepot, {
  #   req(input$entrepot_nom, input$entrepot_tel)
  #   new_id <- paste0("ENT-", sprintf("%03d", nrow(entrepots()) + 1))
  #   params <- list(
  #     new_id,
  #     input$entrepot_nom,
  #     input$entrepot_responsable,
  #     input$entrepot_tel,
  #     input$entrepot_adresse
  #   )
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn, "
  #     INSERT INTO entrepots (
  #       entrepot_id, nom_entrepot, responsable, telephone, adresse
  #     ) VALUES (?, ?, ?, ?, ?);
  #   ", params = params)
  #     })
  #     df <- entrepots()
  #     df <- bind_rows(df, tibble(
  #       entrepot_id  = new_id,
  #       nom_entrepot = input$entrepot_nom,
  #       responsable  = input$entrepot_responsable,
  #       telephone    = input$entrepot_tel,
  #       adresse      = input$entrepot_adresse
  #     ))
  #     entrepots(df)
  #     showNotification("Entrepot ajoute !", type="message")
  #     updateTextInput(session, "entrepot_nom", "")
  #     updateTextInput(session, "entrepot_responsable", "")
  #     updateTextInput(session, "entrepot_tel", "")
  #     updateTextAreaInput(session, "entrepot_adresse", "")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # # 5.4) Chargement pour modification (modal)----
  # 
  # observeEvent(input$edit_entrepot, {
  #   req(input$edit_entrepot_id)
  #   row <- entrepots() %>% filter(entrepot_id == input$edit_entrepot_id)
  #   if (nrow(row) == 0) {
  #     showNotification("ID introuvable", type="error")
  #     return()
  #   }
  #   showModal(modalDialog(
  #     title = paste("Modifier entrepot", input$edit_entrepot_id),
  #     textInput("mod_etp_nom",        "Nom de l'entrepot *", value = row$nom_entrepot),
  #     textInput("mod_etp_responsable","Nom du responsable",  value = row$responsable),
  #     textInput("mod_etp_tel",        "Telephone *",        value = row$telephone),
  #     textAreaInput("mod_etp_adresse","Adresse",            value = row$adresse, rows=2),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("etp_confirm_mod","Mettre Ã  jour", class="btn-success")
  #     ),
  #     size="l"
  #   ))
  # })
  # 
  # # 5.5) Enregistrement de la modification (UPDATE)-----
  # observeEvent(input$etp_confirm_mod, {
  #   params <- list(
  #     input$mod_etp_nom,
  #     input$mod_etp_responsable,
  #     input$mod_etp_tel,
  #     input$mod_etp_adresse,
  #     input$edit_entrepot_id
  #   )
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn, "
  #       UPDATE entrepots SET
  #         nom_entrepot = ?,
  #         responsable  = ?,
  #         telephone    = ?,
  #         adresse      = ?
  #       WHERE entrepot_id = ?;
  #     ", params = params)
  #     })
  #     df <- entrepots()
  #     idx <- which(df$entrepot_id == input$edit_entrepot_id)
  #     df[idx, c("nom_entrepot","responsable","telephone","adresse")] <- params[1:4]
  #     entrepots(df)
  #     removeModal()
  #     showNotification("Entrepot modifie !", type="message")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # # 5.6) Suppression (DELETE)-----
  # 
  # observeEvent(input$delete_entrepot, {
  #   req(input$delete_entrepot_id)
  #   showModal(modalDialog(
  #     title = "Confirmer suppression",
  #     paste("Supprimer entrepot", input$delete_entrepot_id, "?"),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("etp_confirm_del","Supprimer", class="btn-danger")
  #     )
  #   ))
  # })
  # observeEvent(input$etp_confirm_del, {
  #   removeModal()
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn,
  #                 "DELETE FROM entrepots WHERE entrepot_id = ?;",
  #                 params = list(input$delete_entrepot_id)
  #       )
  #     })
  #     entrepots(entrepots() %>%
  #                 filter(entrepot_id != input$delete_entrepot_id))
  #     showNotification("Entrepot supprime !", type="message")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # 
  # 
  # 
  # ##### VI) MODULE GESTION VENTES #####
  # 
  # #6.1) Peupler les dropdowns Ventes----
  # 
  # observe({
  #   updateSelectInput(session, "id_client",
  #                     choices = clients()$customer_id)
  #   updateSelectInput(session, "id_produit",
  #                     choices = data_produits()$product_id)
  #   updateSelectInput(session, "id_commercial",
  #                     choices = setNames(
  #                       commerciaux_data()$id_commercial,
  #                       commerciaux_data()$nom
  #                     ))
  #   updateSelectInput(session, "vente_entrepot",
  #                     choices = entrepots()$entrepot_id)
  # })
  # 
  # 
  # # Quand on selectionne un client, on complete son nom, son telephone et son adresse
  # 
  # observeEvent(input$id_client, {
  #   req(input$id_client)
  #   cl <- clients() %>% filter(customer_id == input$id_client)
  #   if (nrow(cl) == 1) {
  #     updateTextInput(session, "client_nom",     value = cl$name[1])
  #     updateTextInput(session, "client_tel",     value = cl$numero[1])
  #     updateTextInput(session, "client_adresse", value = cl$adresse_livraison[1])
  #   }
  # })
  # 
  # #  Quand on selectionne un produit, on complete son nom et on initialise le prix de vente
  # 
  # observeEvent(input$id_produit, {
  #   req(input$id_produit)
  #   pr <- data_produits() %>% filter(product_id == input$id_produit)
  #   if (nrow(pr) == 1) {
  #     updateTextInput(session,   "produit_nom", value = pr$nom_produit[1])
  #     updateNumericInput(session, "prix_vente", value = pr$prix_achat[1])
  #   }
  # })
  # 
  # 
  # 
  # #6.2) Creation d'une nouvelle vente----
  # 
  # observeEvent(input$submit_vente, {
  #   
  #   req(input$id_client, input$id_produit, input$id_commercial,
  #       input$quantite_vente > 0, input$prix_vente > 0, input$vente_entrepot)
  #   
  #   # 6.2.1) Verifier le stock disponible
  #   
  #   dispo <- stock() %>%
  #     filter(product_id == input$id_produit,
  #            depot      == input$vente_entrepot) %>%
  #     pull(quantite)
  #   dispo <- if (length(dispo)==0) 0 else dispo
  #   
  #   if (input$quantite_vente > dispo) {
  #     showNotification(
  #       paste0("Stock insuffisant : vous demandez ", input$quantite_vente,
  #              " unites mais il n'en reste que ", dispo, "."),
  #       type = "error"
  #     )
  #     return()
  #   }
  #   
  #   # 6.2.2) Calcul du montant
  #   qt   <- as.numeric(input$quantite_vente)
  #   rem  <- as.numeric(input$remise)/100
  #   total <- input$prix_vente * qt * (1 - rem)
  #   
  #   new_id <- sprintf("VENTE-%03d",as.integer(substr(max(c(ventes()$vente_id, "VENTE-000")), 7,9)) + 1)
  #   
  #   # 6.2.3) Parametres pour la transaction
  #   
  #   params <- list(
  #     new_id,
  #     input$id_client,
  #     input$client_nom,
  #     input$client_tel,
  #     input$client_adresse,
  #     input$id_produit,
  #     input$produit_nom,
  #     input$id_commercial,
  #     input$canal,
  #     qt,
  #     input$remise,
  #     total,
  #     input$moy_livraison,
  #     input$vente_entrepot,
  #     as.character(Sys.Date())
  #   )
  #   
  #   # 6.2.4) Transaction atomique : mise Ã  jour du stock puis insertion de la vente
  #   
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       # 6.2.4.1) Mettre Ã  jour le stock
  #       update_stock_db(conn,
  #                       input$id_produit,
  #                       input$vente_entrepot,
  #                       "sortie_vente",
  #                       qt,
  #                       as.character(Sys.Date()),
  #                       input$prix_vente
  #       )
  #       
  #       # 6.2.4.2) Inserer la vente
  #       dbExecute(conn, "
  #       INSERT INTO ventes (
  #         vente_id, client_id, nom_client, tel_client, adresse_client,
  #         produit_id, nom_produit, id_commercial, canal,
  #         quantite, remise, montant_total, moy_livraison,
  #         entrepot_id, date_vente
  #       ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);
  #     ", params = params)
  #     })
  #     
  #     # 6.2.5 miroir en memoire
  #     df <- ventes()
  #     df <- bind_rows(df, tibble(
  #       vente_id       = new_id,
  #       client_id      = input$id_client,
  #       nom_client     = input$client_nom,
  #       tel_client     = input$client_tel,
  #       adresse_client = input$client_adresse,
  #       produit_id     = input$id_produit,
  #       nom_produit    = input$produit_nom,
  #       id_commercial  = input$id_commercial,
  #       canal          = input$canal,
  #       quantite       = qt,
  #       remise         = input$remise,
  #       montant_total  = total,
  #       moy_livraison  = input$moy_livraison,
  #       entrepot_id    = input$vente_entrepot,
  #       date_vente     = as.character(Sys.Date())
  #     ))
  #     
  #     ventes(df)
  #     
  #     
  #     
  #     
  #     
  #     
  #     
  #     vente_info <- list(
  #       vente_id       = new_id,
  #       date_vente     = Sys.Date(),
  #       nom_client     = sanitize_latex(input$client_nom),
  #       tel_client     =sanitize_latex(input$client_tel),
  #       adresse_client = sanitize_latex(input$client_adresse),
  #       nom_produit    = sanitize_latex(input$produit_nom),
  #       quantite       = qt,
  #       prix_unitaire  = input$prix_vente,
  #       remise         = input$remise,
  #       montant_total  = total,
  #       id_commercial  = sanitize_latex(input$id_commercial),
  #       moy_livraison  = sanitize_latex(input$moy_livraison)
  #     )
  #     
  #     # Generation du recu PDF
  #     recu_file <- file.path("recus", paste0("recu_", new_id, ".pdf"))
  #     
  #     #rmarkdown::render(
  #     #input       = "recu_template.Rmd",
  #     # output_file = recu_file,
  #     # params      = list(vente = vente_info),
  #     #envir       = new.env(parent = globalenv()),
  #     #   quiet       = TRUE
  #     #)
  #     
  #     #dernier_recu_path(recu_file)
  #     
  #     # Notification (optionnelle)
  #     shinyalert(
  #       title = "vente enregistree",
  #       text = paste0("Vente ok et recu PDF est disponible "),
  #       type = "success"
  #     )
  #     
  #     
  #     
  #     
  #     showNotification("Vente enregistree !", type="message")
  #     # reset UI
  #     updateSelectInput(session, "id_client", selected = "")
  #     updateSelectInput(session, "id_produit", selected = "")
  #     updateSelectInput(session, "id_commercial", selected = "")
  #     updateSelectInput(session, "vente_entrepot", selected = "")
  #     updateNumericInput(session, "quantite_vente", value = 1)
  #     updateNumericInput(session, "remise", value = 0)
  #     updateNumericInput(session, "prix_vente", value = 0)
  #     updateTextAreaInput(session, "commentaire", value = "")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # 
  # 
  # 
  # # 4) Modification d'une vente
  # 
  # observeEvent(input$edit_vente_btn, {
  #   req(input$edit_vente_id)
  #   df  <- ventes()
  #   row <- df %>% filter(vente_id == toupper(input$edit_vente_id))
  #   if (nrow(row)!=1) {
  #     showNotification("ID de vente introuvable.", type="error")
  #     return()
  #   }
  #   showModal(modalDialog(
  #     title = paste("Modifier", row$vente_id),
  #     selectInput("mod_id_client",      "ID Client",      choices = clients()$customer_id, selected = row$client_id),
  #     selectInput("mod_id_produit",     "ID Produit",     choices = data_produits()$product_id, selected = row$produit_id),
  #     selectInput("mod_id_commercial",  "ID Commercial",  choices = commerciaux_data()$id_commercial, selected = row$id_commercial),
  #     selectInput("mod_entrepot",       "Entrepot",       choices = entrepots()$entrepot_id, selected = row$entrepot_id),
  #     numericInput("mod_quantite",      "Quantite",       value = row$quantite, min=1),
  #     numericInput("mod_remise",        "Remise (%)",     value = row$remise,   min=0, max=100),
  #     numericInput("mod_prix",          "Prix unitaire",  value = row$montant_total/row$quantite/(1-row$remise/100), min=0),
  #     selectInput("mod_canal",          "Canal",          choices = c("face-Ã -face","distributeurs ","reseaux sociaux","commercial","recommandation"), selected = row$canal),
  #     selectInput("mod_moy_livraison",  "Moyen Paiement", choices = c("ORANGE MONAY","MTN MONAY","LIQUIDE","CARTE BANCAIRE","AUTRE"), selected = row$moy_livraison),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("confirm_edit_vente2","Valider", class="btn-success")
  #     ),
  #     size="l"
  #   ))
  # })
  # 
  # observeEvent(input$confirm_edit_vente2, {
  #   req(input$edit_vente_id)
  #   df  <- ventes()
  #   idx <- which(df$vente_id == toupper(input$edit_vente_id))
  #   if (length(idx)!=1) {
  #     removeModal(); showNotification("ID introuvable", type="error"); return()
  #   } 
  #   
  #   # recalcul du total
  #   qt    <- input$mod_quantite
  #   pr    <- input$mod_prix
  #   rem   <- input$mod_remise/100
  #   total <- pr * qt * (1 - rem)
  #   
  #   params <- list(
  #     input$mod_id_client,
  #     input$mod_id_produit,
  #     input$mod_id_commercial,
  #     input$mod_canal,
  #     qt,
  #     input$mod_remise,
  #     total,
  #     input$mod_moy_livraison,
  #     input$mod_entrepot,
  #     as.character(Sys.Date()),
  #     toupper(input$edit_vente_id)
  #   )
  #   
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       
  #       dbExecute(conn, "
  #       UPDATE ventes SET
  #         client_id      = ?,
  #         produit_id     = ?,
  #         id_commercial  = ?,
  #         canal          = ?,
  #         quantite       = ?,
  #         remise         = ?,
  #         montant_total  = ?,
  #         moy_livraison  = ?,
  #         entrepot_id    = ?,
  #         date_vente     = ?
  #       WHERE vente_id = ?;
  #     ", params = params)
  #     })
  #     
  #     # miroir memoire
  #     df[idx, c(
  #       "client_id","produit_id","id_commercial","canal",
  #       "quantite","remise","montant_total","moy_livraison",
  #       "entrepot_id","date_vente"
  #     )] <- params[1:10]
  #     ventes(df)
  #     
  #     removeModal()
  #     showNotification("Vente modifiee !", type="message")
  #   }, error = function(e) {
  #     removeModal()
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # # 5) Suppression d'une vente   
  # 
  # observeEvent(input$delete_vente_btn, {
  #   req(input$delete_vente_id)
  #   showModal(modalDialog(
  #     title = "Confirmer suppression",
  #     paste("Supprimer la vente", input$delete_vente_id, "?"),
  #     footer = tagList(
  #       modalButton("Annuler"),
  #       actionButton("confirm_delete_vente2","Supprimer", class="btn-danger")
  #     )
  #   ))
  # })
  # observeEvent(input$confirm_delete_vente2, {
  #   req(input$delete_vente_id)
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       dbExecute(conn,
  #                 "DELETE FROM ventes WHERE vente_id = ?;",
  #                 params = list(toupper(input$delete_vente_id))
  #       )
  #     })
  #     ventes(ventes() %>% filter(vente_id != toupper(input$delete_vente_id)))
  #     removeModal()
  #     showNotification("Vente supprimee !", type="message")
  #   }, error = function(e) {
  #     removeModal()
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # output$table_ventes <- renderDT({
  #   datatable(ventes(), options = list(pageLength=10, scrollX=TRUE))
  # })
  # 
  # output$export_vente_btn <- downloadHandler(
  #   filename = function() paste0("ventes_", Sys.Date(), ".xlsx"),
  #   content  = function(file) writexl::write_xlsx(ventes(), file)
  # )
  # 
  # 
  # output$telecharger_recu <- downloadHandler(
  #   filename = function() {
  #     if (!is.null(dernier_recu_path())) basename(dernier_recu_path()) else "recu.pdf"
  #   },
  #   content = function(file) {
  #     req(dernier_recu_path())
  #     file.copy(dernier_recu_path(), file)
  #   }
  # )
  # 
  # 
  # ##### VII) MODULE GESTION STOCK #####
  # 
  # 
  # # 7.0.0.0) met Ã  jour stock_prix
  # observeEvent(input$stock_product, {
  #   req(input$stock_product)
  #   prod <- data_produits() %>% 
  #     filter(product_id == input$stock_product)
  #   if (nrow(prod) == 1) {
  #     updateNumericInput(session, "stock_prix", value = prod$prix_achat[1])
  #   }
  # })
  # 
  # 
  # 
  # #7.0.0) Helper de mise Ã  jour (INSERT/UPDATE) dans la base----
  # 
  # update_stock_db <- function(conn, product_id, depot, movement_type, qty, date, prix) {
  #   # 7.0.1) Lire quantite existante
  #   cur <- dbGetQuery(conn,"SELECT quantite FROM stock WHERE product_id = ? AND depot = ?;",params = list(product_id, depot))
  #   old_qty <- if (nrow(cur)==1) cur$quantite[1] else 0
  #   sign <- if (grepl("sortie|sortir", tolower(movement_type))) -1 else 1
  #   new_qty <- old_qty + sign * qty
  #   status  <- if (new_qty > 0) "Disponible" else "Rupture"
  #   
  #   # 7.0.2) Upsert dans stock
  #   if (nrow(cur)==1) {dbExecute(conn, "UPDATE stock SET quantite = ?, prix = ?, status = ?
  #     WHERE product_id = ? AND depot = ?;", params = list(new_qty, prix, status, product_id, depot))} 
  #   else {dbExecute(conn, "
  #     INSERT INTO stock (product_id, depot, quantite, prix, status)
  #     VALUES (?, ?, ?, ?, ?);", params = list(product_id, depot, new_qty, prix, status))}
  #   
  #   # 7.0.3) Historique
  #   
  #   sign <- if (grepl("sortie|sortir", tolower(movement_type))) -1 else 1
  #   
  #   dbExecute(conn, "
  #   INSERT INTO stock_movements
  #     (product_id, depot, movement_type, quantite, prix, date, status)
  #   VALUES (?, ?, ?, ?, ?, ?, ?);", params = list(product_id, depot, movement_type, sign*qty, prix, as.character(date), status))}
  # 
  # 
  # #7.1) Peupler les dropdowns produits <-> entrepots <-> transferts----
  # 
  # observe({# produits : afficher nom, stocker ID
  #   prods <- data_produits()
  #   updateSelectInput(session, "stock_product",choices = setNames(prods$product_id, prods$nom_produit))
  #   updateSelectInput(session, "transfer_product",choices = setNames(prods$product_id, prods$nom_produit))
  #   # entrepots : afficher ID
  #   ents <- entrepots()
  #   updateSelectInput(session, "stock_depot", choices = ents$entrepot_id)
  #   updateSelectInput(session, "transfer_from", choices = ents$entrepot_id)
  #   updateSelectInput(session, "transfer_to",   choices = ents$entrepot_id)
  #   # filtre historique
  #   updateSelectInput(session, "stock_history_product",choices = c("Tous", prods$product_id))
  # })
  # 
  # # 7.2) Mouvement manuel-----
  # 
  # observeEvent(input$submit_stock, {
  #   req(input$stock_product, input$stock_depot,
  #       input$stock_movement, input$stock_qty, input$stock_date)
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       update_stock_db(
  #         conn,
  #         input$stock_product,
  #         input$stock_depot,
  #         input$stock_movement,
  #         input$stock_qty,
  #         input$stock_date,
  #         input$stock_prix
  #       )
  #     })
  #     # rafraÃƒÆ’Ã‚Â®chir le cache
  #     stock(dbGetQuery(pool, "SELECT * FROM stock;"))
  #     stock_movements(dbGetQuery(pool, "SELECT * FROM stock_movements;"))
  #     showNotification("Mouvement de stock enregistre.", type="message")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # 
  # # 7.5) Transferts entre entrepots----
  # 
  # observeEvent(input$submit_transfer, {
  #   req(input$transfer_product, input$transfer_from,
  #       input$transfer_to, input$transfer_qty)
  #   
  #   # 7.5.1) Les entrepots doivent differer
  #   if (input$transfer_from == input$transfer_to) {
  #     showNotification("Les entrepots doivent etre differents.", type = "error")
  #     return()
  #   }
  #   
  #   # 7.5.2) Verifier le stock disponible dans le depot de depart
  #   
  #   dispo <- stock() %>%
  #     filter(product_id == input$transfer_product,
  #            depot      == input$transfer_from) %>%
  #     pull(quantite)
  #   dispo <- if (length(dispo) == 0) 0 else dispo
  #   
  #   # 7.5.3) EmpÃƒÆ’Ã‚Âªcher le transfert si quantite demandee > dispo
  #   
  #   if (input$transfer_qty > dispo) {
  #     showNotification(
  #       paste0(
  #         "Impossible de transferer ", input$transfer_qty,
  #         " ÃƒÂ¢Ã¢â€šÂ¬Ã¢â‚¬Å“ stock disponible seulement : ", dispo
  #       ),
  #       type = "error"
  #     )
  #     return()
  #   }
  #   
  #   
  #   # 7.5.4) Tout est OK, on execute la transaction
  #   tryCatch({
  #     poolWithTransaction(pool, function(conn) {
  #       update_stock_db(
  #         conn,
  #         input$transfer_product,
  #         input$transfer_from,
  #         "transfert_sortie",
  #         input$transfer_qty,
  #         as.character(Sys.Date()),
  #         input$stock_prix
  #       )
  #       update_stock_db(
  #         conn,
  #         input$transfer_product,
  #         input$transfer_to,
  #         "transfert_entree",
  #         input$transfer_qty,
  #         as.character(Sys.Date()),
  #         input$stock_prix
  #       )
  #     })
  #     
  #     # 7.5.5) RafraÃƒÆ’Ã‚Â®chir les caches
  #     stock(dbGetQuery(pool, "SELECT * FROM stock;"))
  #     stock_movements(dbGetQuery(pool, "SELECT * FROM stock_movements;"))
  #     showNotification("Transfert effectue avec succes.", type="message")
  #   }, error = function(e) {
  #     showNotification(paste("Erreur :", e$message), type="error")
  #   })
  # })
  # 
  # 
  # 
  # 
  # # 7.6) Affichage de l'historique----
  # output$table_stock <- renderDT({
  #   df <- stock_movements()
  #   if (!is.null(input$stock_history_product) &&
  #       input$stock_history_product != "Tous") {
  #     df <- df[df$product_id == input$stock_history_product, ]
  #   }
  #   datatable(
  #     df %>% rename(
  #       "Produit"       = product_id,
  #       "Entrepot"      = depot,
  #       "Type"          = movement_type,
  #       "Quantite"      = quantite,
  #       "Prix"          = prix,
  #       "Date"          = date,
  #       "Statut"        = status
  #     ),
  #     options = list(order = list(list(6, "desc"))),
  #     rownames = FALSE
  #   )
  # })
  # 
  # ##### 8) GESTION DES OBJECTIFS ##############  
  # 
  # # 8.1) Remplissage des dropdowns commerciaux----
  # observe({
  #   updateSelectInput(session, "obj_commercial", choices = commerciaux_data()$id_commercial)
  # })
  # 
  # # # 8.2) RafraÃƒÆ’Ã‚Â®chir les caches----
  # # refresh_objectifs <- function() {
  # #   objectifs(dbGetQuery(pool, "SELECT * FROM objectifs;"))
  # #   obj_journalier(dbGetQuery(pool, "SELECT * FROM objectifs_journalier;"))
  # # }
  # 
  # # 8.3) Enregistrement ou mise Ã  jour d'un objectif mensuel----
  # 
  # observeEvent(input$save_objectif, {
  #   req(input$obj_commercial, input$obj_annee, input$obj_mois,
  #       input$obj_quantite,   input$obj_montant)
  #   
  #   # 1) On recupÃƒÂ¨re l'annee / le mois en entier pour eviter sprintf %d sur un character
  #   year_i  <- as.integer(input$obj_annee)
  #   month_i <- as.integer(input$obj_mois)
  #   
  #   # 2) Generation de l'ID composite
  #   #    on peut garder l'annee telle quelle (chaine) ou lui appliquer sprintf si on veut
  #   id_obj <- paste(input$obj_commercial, input$obj_annee,
  #                   sprintf("%02d", month_i),
  #                   sep = "_")
  #   
  #   # 3) Calcul des bornes du mois
  #   start_dt <- as.Date(paste0(input$obj_annee, "-", sprintf("%02d", month_i), "-01"))
  #   end_dt   <- lubridate::ceiling_date(start_dt, "month") - lubridate::days(1)
  #   days     <- seq.Date(start_dt, end_dt, by = "day")
  #   
  #   # 4) Repartition journaliÃƒÂ¨re
  #   qty_per_day <- as.numeric(input$obj_quantite) / length(days)
  #   amt_per_day <- as.numeric(input$obj_montant) / length(days)
  #   df_journ <- data.frame(
  #     journalier_id   = paste0(id_obj, "_", format(days, "%Y%m%d")),
  #     objectif_id     = id_obj,
  #     date_jour       = as.character(days),
  #     quantite_target = qty_per_day,
  #     montant_target  = amt_per_day,
  #     stringsAsFactors = FALSE
  #   )
  #   
  #   # 5) Ecriture en base
  #   poolWithTransaction(pool, function(conn) {
  #     dbExecute(conn,
  #               "DELETE FROM objectifs_journalier WHERE objectif_id = ?;",
  #               params = list(id_obj))
  #     dbExecute(conn,
  #               "DELETE FROM objectifs WHERE objectif_id = ?;",
  #               params = list(id_obj))
  #     dbExecute(conn, "
  #     INSERT INTO objectifs
  #       (objectif_id, id_commercial, annee, mois, quantite_cible, montant_cible, date_modif)
  #     VALUES (?,?,?,?,?,?,?);",
  #               params = list(
  #                 id_obj,
  #                 input$obj_commercial,
  #                 year_i,
  #                 month_i,
  #                 as.numeric(input$obj_quantite),
  #                 as.numeric(input$obj_montant),
  #                 as.character(Sys.Date())
  #               ))
  #     DBI::dbWriteTable(
  #       conn,
  #       name      = "objectifs_journalier",
  #       value     = df_journ,
  #       append    = TRUE,
  #       row.names = FALSE
  #     )
  #   })
  #   
  #   # 6) Rafraichir les reactives
  #   objectifs(      dbGetQuery(pool, "SELECT * FROM objectifs;"))
  #   obj_journalier( dbGetQuery(pool, "SELECT * FROM objectifs_journalier;"))
  #   
  #   showNotification("Objectif enregistre !", type = "message")
  # })
  # 
  # 
  # 
  # 
  # # 4) Affichage des tableaux
  # output$table_objectifs <- renderDT({
  #   datatable(objectifs(), options = list(pageLength = 10, scrollX = TRUE))
  # })
  # output$table_journalier <- renderDT({
  #   datatable(obj_journalier(), options = list(pageLength = 10, scrollX = TRUE))
  # })
  # 
  # # 5) ÃƒÆ’Ã¢â‚¬Â°dition et suppression (similaire Ã  vos patterns existants)
  # observeEvent(input$edit_objectif, {
  #   req(input$save_objectif)
  #   # remplir modal avec les valeurs courantes
  #   row <- objectifs() %>% filter(objectif_id == paste(input$obj_commercial, input$obj_annee, input$obj_mois, sep = "_"))
  #   if (nrow(row)!=1) { showNotification("Objectif introuvable", type="error"); return() }
  #   showModal(modalDialog(
  #     title = "Modifier objectif mensuel",
  #     selectInput("obj_commercial", "Commercial", choices = commerciaux_data()$id_commercial, selected = row$id_commercial),
  #     selectInput("obj_annee", "Annee", choices = 2020:2050, selected = row$annee),
  #     selectInput("obj_mois", "Mois", choices = sprintf("%02d",1:12), selected = sprintf("%02d", row$mois)),
  #     numericInput("obj_quantite", "Quantite cible (mois)", value = row$quantite_cible, min = 0),
  #     numericInput("obj_montant", "Montant cible (mois)", value = row$montant_cible, min = 0, step=0.01),
  #     footer = tagList(modalButton("Annuler"), actionButton("save_objectif","Enregistrer", class="btn-success")),
  #     size = "l"
  #   ))
  # })
  # observeEvent(input$delete_objectif, {
  #   req(input$obj_commercial, input$obj_annee, input$obj_mois)
  #   id_obj <- paste(input$obj_commercial, input$obj_annee, input$obj_mois, sep = "_")
  #   showModal(modalDialog(
  #     title = "Confirmer suppression",
  #     paste("Supprimer l'objectif", id_obj, "?"),
  #     footer = tagList(modalButton("Annuler"), actionButton("confirm_delete_obj","Supprimer", class="btn-danger"))
  #   ))
  # })
  # observeEvent(input$confirm_delete_obj, {
  #   removeModal()
  #   poolWithTransaction(pool, function(conn) {
  #     dbExecute(conn, "DELETE FROM objectifs_journalier WHERE objectif_id = ?;", params=list(paste(input$obj_commercial,input$obj_annee,input$obj_mois,sep="_")))
  #     dbExecute(conn, "DELETE FROM objectifs WHERE objectif_id = ?;", params=list(paste(input$obj_commercial,input$obj_annee,input$obj_mois,sep="_")))
  #   })
  #   objectifs(dbGetQuery(pool, "SELECT * FROM objectifs;"))
  #   obj_journalier(dbGetQuery(pool, "SELECT * FROM objectifs_journalier;"))
  #   showNotification("Objectif supprime !", type="message")
  # })
  # 
  # 
  # ##### 9) DASBOARD --------------
  # 
  # # formateur FCFA
  # format_fcfa <- function(x) {
  #   paste0(formatC(x, format="f", big.mark=" ", digits=0), " FCFA")
  # }
  # 
  # # Top-5 helper (pour pie / bar si besoin)
  # make_top5 <- function(df, col, val) {
  #   df %>% group_by(.data[[col]]) %>%
  #     summarise(total = sum(.data[[val]], na.rm=TRUE)) %>%
  #     arrange(desc(total)) %>% slice_head(n=5)
  # }
  # 
  # # donnees filtrees
  # dash_data <- reactive({
  #   df <- ventes() %>%
  #     filter(date_vente >= input$dash_period[1],
  #            date_vente <= input$dash_period[2])
  #   if (!("Tous" %in% input$dash_commercial))
  #     df <- df %>% filter(id_commercial %in% input$dash_commercial)
  #   if (!("Tous" %in% input$dash_produit))
  #     df <- df %>% filter(produit_id %in% input$dash_produit)
  #   if (!("Tous" %in% input$dash_entrepot))
  #     df <- df %>% filter(entrepot_id %in% input$dash_entrepot)
  #   df
  # })
  # 
  # # CA realise / CA attendu / taux de realisation du CA
  # output$vb_ca_realise <- renderValueBox({
  #   ca_r <- if_else(is.na(sum(dash_data()$montant_total, na.rm=TRUE)),0,sum(dash_data()$montant_total, na.rm=TRUE))
  #   valueBox(format_fcfa(ca_r), "CA realise", icon=icon("money-bill"), color="green")
  # })
  # output$vb_ca_attendu <- renderValueBox({
  #   per <- input$dash_period
  #   obj <- objectifs() %>%
  #     mutate(mois_date = as.Date(sprintf("%04d-%02d-01", annee, mois))) %>%
  #     filter(mois_date >= per[1], mois_date <= per[2])
  #   ca_a <- sum(obj$montant_cible, na.rm=TRUE)
  #   valueBox(format_fcfa(ca_a), "CA attendu", icon=icon("target"), color="yellow")
  # })
  # output$vb_taux_ca <- renderValueBox({
  #   cr <- sum(dash_data()$montant_total, na.rm=TRUE)
  #   per <- input$dash_period
  #   ca_a <- objectifs() %>%
  #     mutate(mois_date = as.Date(sprintf("%04d-%02d-01", annee, mois))) %>%
  #     filter(mois_date >= per[1], mois_date <= per[2]) %>%
  #     summarise(x=sum(montant_cible,na.rm=TRUE)) %>% pull(x)
  #   pct <- if(ca_a>0) round(cr/ca_a*100,1) else 0
  #   valueBox(paste0(pct,"%"), "Taux realisation CA", icon=icon("percent"), color=if(pct>=100)"olive"else"red")
  # })
  # 
  # # Ventes realise / Ventes attendu / taux de realisation des ventes
  # 
  # output$vb_ventes_realise <- renderValueBox({
  #   vr <- sum(dash_data()$quantite, na.rm=TRUE)
  #   valueBox(vr, "Ventes realisees (unites)", icon=icon("shopping-cart"), color="purple")
  # })
  # 
  # output$vb_ventes_attendu <- renderValueBox({
  #   per <- input$dash_period
  #   obj <- objectifs() %>%
  #     mutate(mois_date = as.Date(sprintf("%04d-%02d-01", annee, mois))) %>%
  #     filter(mois_date >= per[1], mois_date <= per[2])
  #   va <- sum(obj$quantite_cible, na.rm=TRUE)
  #   valueBox(va, "Ventes attendues", icon=icon("calendar-check"), color="light-blue")
  # })
  # output$vb_taux_ventes <- renderValueBox({
  #   vr <- sum(dash_data()$quantite, na.rm=TRUE)
  #   per <- input$dash_period
  #   va <- objectifs() %>%
  #     mutate(mois_date = as.Date(sprintf("%04d-%02d-01", annee, mois))) %>%
  #     filter(mois_date >= per[1], mois_date <= per[2]) %>%
  #     summarise(x=sum(quantite_cible,na.rm=TRUE)) %>% pull(x)
  #   pct <- if(va>0) round(vr/va*100,1) else 0
  #   valueBox(paste0(pct,"%"), "Taux realisation ventes", icon=icon("chart-line"), color=if(pct>=100)"teal"else"orange")
  # })
  # 
  # # Nombre de produits en rupture / disponibles
  # output$vb_nb_rupture <- renderValueBox({
  #   nr <- stock() %>% filter(status=="Rupture") %>% nrow()
  #   valueBox(nr, "Produits en rupture", icon=icon("exclamation-triangle"), color="red")
  # })
  # output$vb_nb_disponible <- renderValueBox({
  #   nd <- stock() %>% filter(status=="Disponible") %>% nrow()
  #   valueBox(nd, "Produits disponibles", icon=icon("check-circle"), color="green")
  # })
  # 
  # # courbe des ventes par jour 
  # output$sales_by_day <- renderPlotly({
  #   df_ventes <- dash_data() %>%
  #     group_by(date_vente) %>%
  #     summarise(ventes = sum(quantite, na.rm = TRUE), .groups = "drop")
  #   
  #   df_objectifs <- obj_journalier() %>%
  #     filter(date_jour >= input$dash_period[1],
  #            date_jour <= input$dash_period[2]) %>%
  #     group_by(date_jour) %>%
  #     summarise(objectif = sum(quantite_target, na.rm = TRUE), .groups = "drop") %>%
  #     rename(date_vente = date_jour)
  #   
  #   # Cas oÃƒÂ¹ il n'y a ni vente ni objectif
  #   if (nrow(df_ventes) == 0 && nrow(df_objectifs) == 0) {
  #     return(plotly_empty(type = "scatter", mode = "lines") %>%
  #              layout(title = "Aucune donnee de ventes ou d'objectifs"))
  #   }
  #   
  #   # Fusion des deux datasets
  #   df_all <- full_join(df_ventes, df_objectifs, by = "date_vente") %>%
  #     arrange(date_vente)
  #   
  #   # Remplacement des NA par 0 pour eviter erreurs ggplot
  #   df_all <- df_all %>%
  #     mutate(across(c(ventes, objectif), ~replace_na(., 0)))
  #   
  #   p <- ggplot(df_all, aes(x = as.Date(date_vente))) +
  #     geom_line(aes(y = ventes, color = "Ventes realisees"), size = 1.2) +
  #     geom_line(aes(y = objectif, color = "Objectif"), linetype = "dashed", size = 1.2) +
  #     scale_color_manual(values = c("Ventes realisees" = "blue", "Objectif" = "orange")) +
  #     labs(x = "Date", y = "Quantite", color = "") +
  #     theme_minimal()
  #   
  #   ggplotly(p, tooltip = c("x", "y", "color"))
  # })
  # 
  # 
  # 
  # 
  # # camembert des ventes par entrepot
  # output$pie_entrepot <- renderPlotly({
  #   df <- dash_data() %>% count(entrepot_id)
  #   plot_ly(df, labels=~entrepot_id, values=~n, type="pie") %>%
  #     layout(title="Repartition ventes par entrepot")
  # })
  # 
  # # camembert des ventes par produit
  # output$pie_produit <- renderPlotly({
  #   df <- dash_data() %>% count(produit_id)
  #   plot_ly(df, labels=~produit_id, values=~n, type="pie") %>%
  #     layout(title="Repartition ventes par produit")
  # })
  # 
  # # (facultatif) tableau detaille
  # output$dash_table_detail <- renderDT({
  #   datatable(
  #     dash_data() %>%
  #       select(date_vente, vente_id, id_commercial, produit_id, quantite, montant_total, entrepot_id),
  #     options = list(pageLength=10, scrollX=TRUE)
  #   )
  # })
  # 
  # ##### 10) users -------------- 
  # 
  # 
  # observeEvent(input$login_btn, {
  #   req(input$login_user, input$login_pass)
  #   
  #   user_query <- dbGetQuery(pool, "SELECT * FROM utilisateurs WHERE id_utilisateur = ?", params = list(input$login_user))
  #   
  #   if (nrow(user_query) == 1 && 
  #       sodium::password_verify(user_query$mot_de_passe[1], input$login_pass)) {
  #     
  #     user_session$logged <- TRUE
  #     user_session$role <- user_query$role[1]
  #     user_session$id <- user_query$id_utilisateur[1]
  #     user_session$depot <- user_query$entrepot_id[1]
  #     
  #     hide("login_panel")
  #     show("app_main_ui")
  #   } else {
  #     showNotification("Nom d'utilisateur ou mot de passe incorrect", type = "error")
  #   }
  # })
  # 
  # output$role_affiche <- renderPrint({
  #   paste("R?le :", user_session$role)
  # })
  # 
  
  
}


shinyApp(ui, server)
