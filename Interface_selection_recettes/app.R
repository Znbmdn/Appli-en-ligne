## Essai code interface sélection recettes en fonction de choix d'ingrédients et de plats

# install.packages("shiny")
# install.packages("DT")

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
      h3("Régimes alimentaires", align = "center", style = "color: #FFFFFF;"),
      selectizeInput("regime", "", choices = regimes, multiple = TRUE, options = list(
        placeholder = "Sélectionnez les régimes alimentaires...",
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
  )
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
          lengthMenu = "Montre _MENU_ entrées"
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)

