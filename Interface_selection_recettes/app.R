## Essai code interface sélection recettes en fonction de choix d'ingrédients et de plats

# install.packages("shiny")
# install.packages("DT")

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
  stringsAsFactors = FALSE
)

# Allergènes
allergenes <- c("Gluten", "Lactose", "Oeufs", "Arachides", "Fruits à coque")

# Régimes alimentaires
regimes <- c("Végétarien", "Végétalien", "Sans gluten", "Cétogène")


ui <- fluidPage(
  titlePanel("Sélection de recettes", windowTitle = "Recettes délicieuses"),
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #FFA500; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);",
      h3("Sélection d'ingrédients et de plats", align = "center", style = "color: #FFFFFF;"),
      selectInput("ingredient", "Ingrédients :", choices = ingredients, multiple = TRUE),
      selectInput("type_recette", "Type de plats :", choices = c("Tous", "Entrée", "Plat principal", "Dessert", "Apéritif")),
      h3("Allergènes à éviter", align = "center", style = "color: #FFFFFF;"),
      selectInput("allergene", "Allergènes :", choices = allergenes, multiple = TRUE),
      h3("Régimes alimentaires", align = "center", style = "color: #FFFFFF;"),
      selectInput("regime", "Régimes :", choices = regimes, multiple = TRUE)
    ),
    mainPanel(
      style = "padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);",
      h2("Recettes disponibles", align = "center", style = "color: #000000;"),
      DTOutput("recettes_table"),
      tags$style(HTML("
        .dataTables_wrapper {
          background-color: #FFDAB9;
          padding: 20px;
          border-radius: 10px;
          box-shadow: none;
        }
      "))
    )
  ),
  tags$style(HTML("
    .modal-dialog {
      max-width: 800px;
    }
    .modal-content {
      background-color: #f8f9fa;
    }
    .modal-header {
      border-bottom: none;
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
    }
    th {
      padding-top: 12px;
      padding-bottom: 12px;
      text-align: left;
      background-color: #FFA500;
      color: white;
    }
    tr:nth-child(even) {
      background-color: #FFE4B2;
    }
    tr:hover {
      background-color: #FFD699;
    }
  "))
)


server <- function(input, output, session) {
  # Filtrer les recettes en fonction des sélections
  recettes_filtrées <- reactive({
    filtered_recipes <- recettes
    
    # Filtrer par ingrédients sélectionnés
    if (!is.null(input$ingredient) && length(input$ingredient) > 0) {
      filtered_recipes <- filtered_recipes[sapply(filtered_recipes$Ingrédients, function(x) all(input$ingredient %in% strsplit(x, ", ")[[1]])), ]
    }
    
    # Filtrer par type de recette sélectionné
    if (input$type_recette != "Tous") {
      filtered_recipes <- filtered_recipes[filtered_recipes$Type == input$type_recette, ]
    }
    
    # Filtrer par allergènes à éviter
    if (!is.null(input$allergene) && length(input$allergene) > 0) {
      for (i in input$allergene) {
        filtered_recipes <- filtered_recipes[!grepl(i, filtered_recipes$Ingrédients), ]
      }
    }
    
    # Filtrer par régimes alimentaires
    if (!is.null(input$regime) && length(input$regime) > 0) {
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
          # Exemple de filtre pour régime cétogène
        }
      }
    }
    
    filtered_recipes
  })
  
  # Afficher la table des recettes
  output$recettes_table <- renderDT({
    datatable(
      recettes_filtrées(),
      selection = "single",
      options = list(
        searching = TRUE,
        lengthMenu = list(c(5, 10, 15), c("5", "10", "15")),
        scrollY = "400px",
        scrollCollapse = TRUE,
        fixedHeader = TRUE, # Ajout de l'option fixedHeader pour fixer les noms de colonnes
        language = list(
          search = "Recherche",
          lengthMenu = "Montre _MENU_ entrées"
        )
      )
    )
  })
  
  # Afficher une fenêtre détail recette lorsqu'une recette est cliquée
  observeEvent(input$recettes_table_rows_selected, {
    showModal(
      modalDialog(
        title = "Détails de la recette",
        HTML(paste0("<h2>Recette : ", recettes_filtrées()$Nom[input$recettes_table_rows_selected], "</h2><p><strong>Type :</strong> ", recettes_filtrées()$Type[input$recettes_table_rows_selected], "</p><p><strong>Ingrédients :</strong> ", recettes_filtrées()$Ingrédients[input$recettes_table_rows_selected], "</p><p><strong>Instructions :</strong> ", recettes_filtrées()$Instructions[input$recettes_table_rows_selected], "</p><p><strong>Temps de préparation :</strong> ", recettes_filtrées()$Temps_de_preparation[input$recettes_table_rows_selected])),
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

# Lancement application
shinyApp(ui = ui, server = server)
