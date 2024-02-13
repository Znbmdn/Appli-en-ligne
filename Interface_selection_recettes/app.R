library(shiny)
library(DT)

ingredients <- c("Oeufs", "Farine", "Sucre", "Lait", "Beurre", "Sel", "Poivre", "Tomate", "Oignon", "Ail")
recettes <- data.frame(
  Nom = c("Omelette", "Crêpes", "Tarte aux pommes", "Sauce tomate", "Soupe à l'oignon"),
  Type = c("Entrée", "Dessert", "Dessert", "Plat principal", "Entrée"),
  Ingrédients = c("Oeufs, Lait, Sel, Poivre", "Farine, Oeufs, Lait, Sucre", "Farine, Beurre, Sucre, Pommes", "Tomate, Oignon, Ail, Sel, Poivre", "Oignon, Beurre, Farine, Bouillon"),
  Instructions = c("Battez les oeufs, ajoutez le lait, assaisonnez avec sel et poivre, faites cuire dans une poêle chaude.", "Mélangez la farine, les oeufs, le lait et le sucre, faites cuire dans une poêle.", "Préparez une pâte avec la farine, le beurre, le sucre et les pommes, faites cuire au four.", "Faites revenir les tomates, les oignons et l'ail dans une casserole, assaisonnez avec sel et poivre.", "Faites revenir les oignons dans du beurre, ajoutez la farine, puis le bouillon, laissez mijoter."),
  Temps_de_preparation = c("15 minutes", "30 minutes", "45 minutes", "60 minutes", "45 minutes"),
  stringsAsFactors = FALSE
)

allergenes <- c("Gluten", "Lactose", "Oeufs", "Arachides", "Fruits à coque")
regimes <- c("Végétarien", "Végétalien", "Sans gluten", "Cétogène")
types_plat <- c("Tous", "Entrée", "Plat principal", "Dessert", "Apéritif")

ui <- fluidPage(
  titlePanel("Sélection de recettes", windowTitle = "Recettes délicieuses"),
  fluidRow(
    style = "background-color: #FFA500; padding: 20px;",
    column(
      width = 3,
      h3("Ingrédients", align = "center", style = "color: #FFFFFF;"),
      selectizeInput("ingredient", "", choices = ingredients, multiple = TRUE, options = list(
        placeholder = "Sélectionnez les ingrédients...",
        plugins = list('remove_button')
      ))
    ),
    column(
      width = 3,
      h3("Allergènes", align = "center", style = "color: #FFFFFF;"),
      selectizeInput("allergene", "", choices = allergenes, multiple = TRUE, options = list(
        placeholder = "Sélectionnez les allergènes...",
        plugins = list('remove_button')
      ))
    ),
    column(
      width = 3,
      h3("Diète", align = "center", style = "color: #FFFFFF;"),
      selectizeInput("regime", "", choices = regimes, multiple = TRUE, options = list(
        placeholder = "Sélectionnez la diète...",
        plugins = list('remove_button')
      ))
    ),
    column(
      width = 3,
      h3("Type de plat", align = "center", style = "color: #FFFFFF;"),
      selectizeInput("type_recette", "", choices = types_plat, options = list(
        placeholder = "Sélectionnez le type de plat...",
        plugins = list('remove_button')
      ))
    )
  ),
  fluidRow(
    DTOutput("recettes_table")
  ),
  tags$style(HTML("
    .dataTables_wrapper {
      background-color: #FFDAB9;
      padding: 20px;
      border-radius: 10px;
      box-shadow: none;
    }
  ")),
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
  recettes_filtrées <- reactive({
    filtered_recipes <- recettes
    
    if (!is.null(input$ingredient) && length(input$ingredient) > 0) {
      filtered_recipes <- filtered_recipes[sapply(filtered_recipes$Ingrédients, function(x) all(input$ingredient %in% strsplit(x, ", ")[[1]])), ]
    }
    
    if (!is.null(input$allergene) && length(input$allergene) > 0) {
      for (i in input$allergene) {
        filtered_recipes <- filtered_recipes[!grepl(i, filtered_recipes$Ingrédients), ]
      }
    }
    
    if (!is.null(input$type_recette) && input$type_recette != "Tous") {
      filtered_recipes <- filtered_recipes[filtered_recipes$Type == input$type_recette, ]
    }
    
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
  
  output$recettes_table <- renderDT({
    datatable(
      recettes_filtrées(),
      selection = "single",
      options = list(
        searching = TRUE,
        lengthMenu = list(c(5, 10, 15), c("5", "10", "15")),
        scrollY = "400px",
        scrollCollapse = TRUE,
        fixedHeader = TRUE,
        language = list(
          search = "Recherche",
          lengthMenu = "Montre _MENU_ recettes"
        )
      ),
      callback = JS("table.on('click', 'tr', function() {
                      var data = table.row(this).data();
                      Shiny.setInputValue('selected_recipe', data[0]);
                      });")
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

shinyApp(ui = ui, server = server)
