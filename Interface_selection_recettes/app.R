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
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  titlePanel("Sélection de recettes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ingredient", "Sélectionnez les ingrédients :", choices = ingredients, multiple = TRUE),
      selectInput("type_recette", "Sélectionnez le type de recette :", choices = c("Tous", "Entrée", "Plat principal", "Dessert", "Apéritif"))
    ),
    mainPanel(
      DTOutput("recettes_table")
    )
  )
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
    
    filtered_recipes
  })
  
  # Afficher la table des recettes
  output$recettes_table <- renderDT({
    datatable(
      recettes_filtrées(),
      selection = "single",
      options = list(
        searching = TRUE,
        lengthMenu = c(5, 10, 15),
        scrollY = "400px",
        scrollCollapse = TRUE
      )
    )
  })
  
  # Afficher une fenêtre détail recette lorsqu'une recette est cliquée
  observeEvent(input$recettes_table_rows_selected, {
    showModal(
      modalDialog(
        title = "Détails de la recette",
        HTML(paste0("<h2>Recette : ", recettes_filtrées()$Nom[input$recettes_table_rows_selected], "</h2><p><strong>Type :</strong> ", recettes_filtrées()$Type[input$recettes_table_rows_selected], "</p><p><strong>Ingrédients :</strong> ", recettes_filtrées()$Ingrédients[input$recettes_table_rows_selected], "</p><p><strong>Instructions :</strong> ", recettes_filtrées()$Instructions[input$recettes_table_rows_selected], "</p><p><strong>Temps de préparation :</strong> ", recettes_filtrées()$Temps_de_preparation[input$recettes_table_rows_selected])),
        footer = actionButton("close_modal_button", "Fermer"),
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
