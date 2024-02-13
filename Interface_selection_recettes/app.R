## Essai code interface sélection recettes en fonction de choix d'ingrédients et de plats

#install.packages("DT")

library(shiny)
library(DT)

# Données au hasard pour les ingrédients, les recettes et les temps de préparation pour essayer le code
ingredients <- c("Oeufs", "Farine", "Sucre", "Lait", "Beurre", "Sel", "Poivre", "Tomate", "Oignon", "Ail")
recettes <- data.frame(
  Nom = c("Omelette", "Crêpes", "Tarte aux pommes", "Sauce tomate", "Soupe à l'oignon"),
  Type = c("Entrée", "Dessert", "Dessert", "Plat principal", "Entrée"),
  Ingrédients = c("Oeufs, Lait, Sel, Poivre", "Farine, Oeufs, Lait, Sucre", "Farine, Beurre, Sucre, Pommes", "Tomate, Oignon, Ail, Sel, Poivre", "Oignon, Beurre, Farine, Bouillon"),
  Instructions = c("Battez les oeufs, ajoutez le lait, assaisonnez avec sel et poivre, faites cuire dans une poêle chaude.", "Mélangez la farine, les oeufs, le lait et le sucre, faites cuire dans une poêle.", "Préparez une pâte avec la farine, le beurre, le sucre et les pommes, faites cuire au four.", "Faites revenir les tomates, les oignons et l'ail dans une casserole, assaisonnez avec sel et poivre.", "Faites revenir les oignons dans du beurre, ajoutez la farine, puis le bouillon, laissez mijoter."),
  Temps_de_preparation = c("15 minutes", "30 minutes", "45 minutes", "60 minutes", "45 minutes"),
  Origine = c("France", "France", "France", "Italie", "France"),
  stringsAsFactors = FALSE
)

allergenes <- c("Gluten", "Lactose", "Oeufs", "Arachides", "Fruits à coque")
regimes <- c("Tous", "Végétarien", "Végétalien", "Sans gluten", "Cétogène")
types_plat <- c("Tous", "Entrée", "Plat principal", "Dessert", "Apéritif")
durees <- c("Tous", "Express (<20 min)", "Rapide (<30 min)", "Normale (<1 heure)", "Longue (> 1 heure)")
origine <- c("Tous", "France", "Italie")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        margin: 1cm; # Ajoute des marges de 1 cm autour de la page
      }
    "))
  ),
  titlePanel("Sélection de recettes", windowTitle = "Recettes délicieuses"),
  fluidRow(
    style = "background-color: #FFA500; padding: 20px;",
    column(
      width = 2,
      h3("Ingrédients", align = "center", style = "color: #FFFFFF;"),
      selectizeInput("ingredient", "", choices = ingredients, multiple = TRUE, options = list(
        placeholder = "Sélectionnez les ingrédients...",
        plugins = list('remove_button')
      ))
    ),
    column(
      width = 2,
      h3("Temps", align = "center", style = "color: #FFFFFF;"),
      selectInput("duree", "", choices = durees, selectize = FALSE)
    ),
    column(
      width = 2,
      h3("Allergènes", align = "center", style = "color: #FFFFFF;"),
      selectizeInput("allergene", "", choices = allergenes, multiple = TRUE, options = list(
        placeholder = "Sélectionnez les allergènes...",
        plugins = list('remove_button')
      ))
    ),
    column(
      width = 2,
      h3("Diète", align = "center", style = "color: #FFFFFF;"),
      selectInput("regime", "", choices = regimes, selectize = FALSE)
    ),
    column(
      width = 2,
      h3("Type de plat", align = "center", style = "color: #FFFFFF;"),
      selectizeInput("type_recette", "", choices = types_plat, options = list(
        placeholder = "Sélectionnez le type de plat...",
        plugins = list('remove_button')
      ))
    ),
    column(
      width = 2,
      h3("Origine", align = "center", style = "color: #FFFFFF;"),  
      selectizeInput("origine", "", choices = origine, options = list(
        placeholder = "Sélectionnez...",
        plugins = list('remove_button')
      ))
    )
  ),
  fluidRow(
    DTOutput("recettes_table")
  ),
  tags$style(HTML("
    .dataTables_wrapper {
      background-color: #FFDAB9; # couleur arrière-plan qui englobe la tableau
      padding: 20px;
      border-radius: 10px;
      box-shadow: none;
    }
  ")),
  tags$style(HTML("
    .modal-dialog {
      max-width: 800px;
      color: black; #FFDAB9; # couleur du texte du pop up
    }
    .modal-content {
      background-color: #FFDAB9; # couleur de l'arrière plan du pop up
    }
    .modal-header {
      border-bottom: none;
      color: black; # couleur du titre du pop up
    }
    .selected-row {
    background-color: #FFDAB9; # Couleur de fond pour la sélection
    }
    .modal-footer {
      border-top: none;
    }
    .dataTables_filter {
      text-align: right;
    }
    .dataTables_paginate {
      float: right;
    }
    th, td {
      border: 1px solid #ddd;
      padding: 8px;
      color: black; # couleur du texte du tableau
    }
    th {
      padding-top: 12px;
      padding-bottom: 12px;
      text-align: left;
      background-color: #FFA500; # couleur des en-têtes de colonnes du tableau
    }
    tr:nth-child(even) {
      background-color: #FFE4B2; #couleurs des lignes du tableau
    }
    tr:hover {
      background-color: #FFD699; #couleur surbrillance d'une ligne du tableau lorsque souris passe dessus
    }
  "))
)

server <- function(input, output, session) {
  recettes_filtrées <- reactive({
    filtered_recipes <- recettes
    
    # Filtrer par ingrédients sélectionnés
    if (!is.null(input$ingredient) && length(input$ingredient) > 0) {
      filtered_recipes <- filtered_recipes[sapply(filtered_recipes$Ingrédients, function(x) all(input$ingredient %in% strsplit(x, ", ")[[1]])), ]
    }
    
    # Filtrer par allergènes à éviter
    if (!is.null(input$allergene) && length(input$allergene) > 0) {
      for (i in input$allergene) {
        filtered_recipes <- filtered_recipes[!grepl(i, filtered_recipes$Ingrédients), ]
      }
    }
    
    # Filtrer par types de recettes
    if (!is.null(input$type_recette) && input$type_recette != "Tous") {
      filtered_recipes <- filtered_recipes[filtered_recipes$Type == input$type_recette, ]
    }
    
    # Filtrer par diète
    if (!is.null(input$regime) && input$regime != "Tous") {
      for (i in input$regime) {
        if (i == "Végétarien") {
          filtered_recipes <- filtered_recipes[!grepl("Viande", filtered_recipes$Type), ]
        }
        if (i == "Végétalien") {
          filtered_recipes <- filtered_recipes[!grepl("Oeufs|Lait|Fromage|Miel", filtered_recipes$Ingrédients), ]
        }
        if (i == "Sans gluten") {
          filtered_recipes <- filtered_recipes[!grepl("Farine|Pâtes|Pain", filtered_recipes$Ingrédients), ]
        }
        if (i == "Cétogène") {
        }
      }
    }
    
    # Filtrer par Temps de préparation
    if (input$duree != "Tous") {
      if (input$duree == "Express (<20 min)") {
        filtered_recipes <- filtered_recipes[grepl("minutes|minute", filtered_recipes$Temps_de_preparation) & as.numeric(gsub("\\D", "", filtered_recipes$Temps_de_preparation)) <= 20, ]
      } else if (input$duree == "Rapide (<30 min)") {
        filtered_recipes <- filtered_recipes[grepl("minutes|minute", filtered_recipes$Temps_de_preparation) & as.numeric(gsub("\\D", "", filtered_recipes$Temps_de_preparation)) <= 30, ]
      } else if (input$duree == "Normale (<1 heure)") {
        filtered_recipes <- filtered_recipes[grepl("minutes|minute", filtered_recipes$Temps_de_preparation) & as.numeric(gsub("\\D", "", filtered_recipes$Temps_de_preparation)) <= 60, ]
      } else if (input$duree == "Longue (> 1 heure)") {
        filtered_recipes <- filtered_recipes[grepl("hour|hours", filtered_recipes$Temps_de_preparation) & as.numeric(gsub("\\D", "", filtered_recipes$Temps_de_preparation)) >= 60, ]
      }
    }
    
    # Filtrer par Origine
    if (!is.null(input$origine) && input$origine != "Tous") {  
      filtered_recipes <- filtered_recipes[filtered_recipes$Origine == input$origine, ]
    }
      
    
    filtered_recipes
  })
  
  # Afficher le tableau des recettes
  output$recettes_table <- renderDT({
    datatable(
      recettes_filtrées(),  # Utilise les données filtrées des recettes
      selection = "single",  # Permet à l'utilisateur de sélectionner une seule ligne à la fois
      options = list(  # Définit les options du DataTable
        searching = TRUE,  # Active la barre de recherche
        lengthMenu = list(c(5, 10, 15), c("5", "10", "15")),  # Options de longueur du tableau par page
        scrollY = "400px",  # Hauteur maximale du tableau avec une barre de défilement verticale
        scrollCollapse = TRUE,  # Réduit la hauteur du tableau lorsqu'il y a moins de lignes que la hauteur maximale
        fixedHeader = TRUE,  # Fixe l'en-tête du tableau en haut lors du défilement
        language = list(  # Définit la langue des éléments d'interface utilisateur
          search = "Rechercher :",  # Texte pour la barre de recherche
          lengthMenu = "Afficher _MENU_ entrées par page",  # Texte pour le menu de longueur
          info = "Affichage de l'élément _START_ à _END_ sur un total de _TOTAL_ entrées",  # Information sur le nombre d'entrées affichées
          infoEmpty = "Affichage de l'élément 0 à 0 sur un total de 0 entrées",  # Message affiché lorsque le tableau est vide
          infoFiltered = "(filtré à partir de _MAX_ entrées totales)",  # Information sur le nombre d'entrées filtrées
          zeroRecords = "Aucune recette sous ce nom disponible dans le tableau",  # Message affiché lorsque aucun résultat ne correspond à la recherche
          emptyTable = "Aucune recette disponible dans le tableau",  # Message affiché lorsque le tableau est vide
          first = "Premier",  # Texte pour le bouton 'Premier'
          last = "Dernier"  # Texte pour le bouton 'Dernier'
        )
      )
    )
  })
  
  # Afficher une fenêtre détail recette lorsqu'une recette est cliquée
  observeEvent(input$recettes_table_rows_selected, {
    showModal(
      modalDialog(
        title = "Détails de la recette",
        HTML(paste0("<h2>Recette : ", recettes_filtrées()$Nom[input$recettes_table_rows_selected, ], "</h2><p><strong>Type :</strong> ", recettes_filtrées()$Type[input$recettes_table_rows_selected, ], "</p><p><strong>Ingrédients :</strong> ", recettes_filtrées()$Ingrédients[input$recettes_table_rows_selected, ], "</p><p><strong>Instructions :</strong> ", recettes_filtrées()$Instructions[input$recettes_table_rows_selected, ], "</p><p><strong>Temps_de_preparation :</strong> ", recettes_filtrées()$`Temps_de_preparation`[input$recettes_table_rows_selected, ])),
        footer = actionButton("close_modal_button", "Fermer", style = "background-color: #FFA500; color: white; border: none; padding: 10px 20px; text-align: center; text-decoration: none; display: inline-block; font-size: 16px; border-radius: 5px;"),
        easyClose = TRUE
      )
    )
  })
  
  # Fermer la fenêtre détail recette
  observeEvent(input$close_modal_button, {
    removeModal()
  })
}


shinyApp(ui = ui, server = server)
