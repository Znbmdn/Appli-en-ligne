## Essai code interface sélection recettes en fonction de choix d'ingrédients et de plats

#install.packages("shinydashboard")
#install.packages("DT")

library(shinydashboard)
library(shiny)
library(DT)

ingredients <- c("Oeufs", "Légumes variés", "Sel", "Poivre", "Pâtes", "Lardons", "Crème fraîche",
                 "Parmesan", "Laitue", "Croûtons", "Poulet", "Sauce césar", "Poulet entier",
                 "Herbes aromatiques", "Aubergine", "Courgette", "Poivron", "Tomate", "Oignon",
                 "Ail", "Huile d'olive", "Herbes de Provence", "Viande hachée", "Persil",
                 "Chapelure", "Viande de boeuf", "Fromage blanc", "Fruits rouges", "Citron",
                 "Farine", "Fruits frais variés", "Crème pâtissière", "Sucre glace", "Menthe",
                 "Riz arborio", "Champignons", "Vin blanc", "Beurre", "Pâte brisée", "Saumon fumé",
                 "Poireaux", "Quinoa", "Vinaigre balsamique", "Farine de sarrasin", "Fromage de chèvre",
                 "Crème fraîche épaisse", "Crème fraîche liquide", "Levure chimique", "Gingembre",
                 "Yaourt nature", "Épices indiennes", "Coriandre", "Carottes", "Bouillon de légumes",
                 "Cumin", "Fromage à fondue", "Pain", "Cornichons", "Pommes de terre", "Lentilles",
                 "Avocat", "Lait de coco", "Curry", "Cerises", "Vanille", "Tortillas", "Haricots rouges",
                 "Crème fraîche allégée", "Mélange de légumes mexicains")
recettes <- data.frame(
  Nom = c("Omelette aux Légumes", "Pâtes à la Carbonara", "Salade César", "Poulet rôti aux Herbes", "Ratatouille Provençale",
          "Boulettes de Viande à la Provençale", "Gâteau au Fromage Blanc et Fruits Rouges", "Risotto aux Champignons et Parmesan",
          "Tarte aux Poireaux et Saumon Fumé", "Salade de Quinoa aux Légumes Rôtis", "Muffins aux Myrtilles et Amandes",
          "Poulet Tikka Masala", "Galette de Sarrasin aux Champignons et Fromage de Chèvre", "Velouté de Carottes au Cumin",
          "Fondue au Fromage et Vin Blanc", "Salade de Lentilles et Avocat", "Poulet au Curry et Lait de Coco",
          "Clafoutis aux Cerises et Amandes", "Tacos Mexicains aux Haricots et Avocat", "Gâteau de Crêpes aux Fruits Frais"),
  Type = c("Entrée", "Plat principal", "Entrée", "Plat principal", "Plat principal",
           "Plat principal", "Dessert", "Plat principal", "Plat principal", "Entrée",
           "Dessert", "Plat principal", "Plat principal", "Entrée", "Plat principal",
           "Entrée", "Plat principal", "Dessert", "Plat principal", "Dessert"),
  Ingrédients = c("Oeufs, Légumes variés, Sel, Poivre", "Pâtes, Lardons, Oeufs, Crème fraîche, Parmesan, Poivre",
                  "Laitue, Croûtons, Poulet, Parmesan, Sauce césar", "Poulet entier, Herbes aromatiques, Sel, Poivre",
                  "Aubergine, Courgette, Poivron, Tomate, Oignon, Ail, Huile d'olive, Herbes de Provence",
                  "Viande hachée, Oignon, Ail, Persil, Chapelure, Oeuf, Sel, Poivre",
                  "Fromage blanc, Oeufs, Sucre, Farine, Fruits rouges, Citron, Sel",
                  "Riz arborio, Champignons, Oignon, Vin blanc, Bouillon de légumes, Parmesan, Beurre, Sel, Poivre",
                  "Pâte brisée, Poireaux, Saumon fumé, Crème fraîche, Oeufs, Sel, Poivre",
                  "Quinoa, Courgette, Poivron, Tomate, Oignon, Huile d'olive, Vinaigre balsamique, Sel, Poivre",
                  "Farine, Sucre, Oeufs, Lait, Beurre, Myrtilles, Amandes, Levure chimique, Sel",
                  "Poulet, Tomate, Oignon, Ail, Gingembre, Yaourt nature, Épices indiennes, Coriandre, Sel, Poivre",
                  "Farine de sarrasin, Oeufs, Champignons, Fromage de chèvre, Crème fraîche, Sel, Poivre",
                  "Carottes, Oignon, Ail, Bouillon de légumes, Crème fraîche, Cumin, Sel, Poivre",
                  "Fromage à fondue, Vin blanc, Ail, Pain, Cornichons, Pommes de terre, Sel, Poivre",
                  "Lentilles, Avocat, Tomate, Persil, Citron, Huile d'olive, Sel, Poivre",
                  "Poulet, Lait de coco, Curry, Oignon, Ail, Gingembre, Coriandre, Sel, Poivre",
                  "Cerises, Oeufs, Farine, Sucre, Lait, Amandes, Beurre, Sel, Vanille",
                  "Tortillas, Haricots rouges, Avocat, Tomate, Oignon, Coriandre, Fromage râpé, Crème fraîche, Sel, Poivre",
                  "Crêpes, Fruits frais variés, Crème pâtissière, Sucre glace, Menthe"),
  Instructions = c("Battez les oeufs, ajoutez les légumes, assaisonnez avec sel et poivre, faites cuire dans une poêle chaude.",
                   "Faites cuire les pâtes, faites revenir les lardons, mélangez avec les pâtes cuites, les oeufs battus, la crème fraîche, le parmesan et le poivre.",
                   "Mélangez la laitue, les croûtons, le poulet, le parmesan, ajoutez la sauce césar, mélangez.",
                   "Assaisonnez le poulet avec des herbes aromatiques, du sel et du poivre, faites rôtir au four.",
                   "Faites revenir les légumes dans l'huile d'olive avec l'ail et l'oignon, assaisonnez avec les herbes de Provence.",
                   "Mélangez la viande hachée avec l'oignon, l'ail, le persil, la chapelure, l'oeuf, formez des boulettes, faites cuire dans une poêle.",
                   "Mélangez le fromage blanc, les oeufs, le sucre, la farine, le citron, ajoutez les fruits rouges, faites cuire au four.",
                   "Faites revenir l'oignon dans du beurre, ajoutez le riz et le vin blanc, puis le bouillon de légumes, les champignons, le parmesan, assaisonnez.",
                   "Faites revenir les poireaux, étalez-les sur la pâte brisée, ajoutez le saumon fumé, le mélange d'oeufs et de crème fraîche, assaisonnez, faites cuire au four.",
                   "Faites rôtir les légumes dans l'huile d'olive, mélangez avec le quinoa cuit, assaisonnez avec le vinaigre balsamique, le sel et le poivre.",
                   "Mélangez la farine, le sucre, les oeufs, le lait, le beurre fondu, les myrtilles, les amandes, la levure chimique, le sel, faites cuire au four.",
                   "Faites revenir le poulet, les oignons, l'ail et le gingembre, ajoutez les tomates, les épices, le yaourt, la coriandre, faites mijoter.",
                   "Faites cuire les galettes de sarrasin, garnissez-les avec les champignons poêlés et le fromage de chèvre, ajoutez la crème fraîche, assaisonnez.",
                   "Faites cuire les carottes, l'oignon et l'ail dans le bouillon de légumes, mixez avec la crème fraîche et le cumin, assaisonnez.",
                   "Faites fondre le fromage dans le vin blanc avec l'ail, trempez les morceaux de pain dans le fromage fondu, servez avec les cornichons et les pommes de terre.",
                   "Faites cuire les lentilles, mélangez avec les avocats, les tomates, le persil, le citron, l'huile d'olive, assaisonnez.",
                   "Faites cuire le poulet avec l'oignon, l'ail, le curry, ajoutez le lait de coco, laissez mijoter, assaisonnez.",
                   "Dénoyautez les cerises, mélangez les oeufs, la farine, le sucre, le lait, les amandes, faites cuire au four.",
                   "Réchauffez les tortillas, garnissez de haricots, d'avocat, de tomate, d'oignon, de coriandre, de fromage râpé, de crème fraîche, assaisonnez.",
                   "Alternez les crêpes avec la crème pâtissière et les fruits frais, saupoudrez de sucre glace, décorez de feuilles de menthe."),
  Temps = c("15 minutes", "20 minutes", "30 minutes", "1 heure", "45 minutes",
            "45 minutes", "1 heure", "30 minutes", "45 minutes", "40 minutes",
            "30 minutes", "45 minutes", "25 minutes", "40 minutes", "30 minutes",
            "20 minutes", "50 minutes", "30 minutes", "20 minutes", "45 minutes"),
  Origine = c("France", "Italie", "États-Unis", "France", "France",
              "France", "France", "Italie", "France", "International",
              "France", "Inde", "France", "Suisse", "France",
              "International", "France", "France", "Mexique", "France"),
  stringsAsFactors = FALSE
)

allergenes <- c("Gluten", "Lactose", "Oeufs", "Arachides", "Fruits à coque")
regimes <- c("Tous", "Végétarien", "Végétalien", "Sans gluten", "Cétogène")
types_plat <- c("Tous", "Entrée", "Plat principal", "Dessert", "Apéritif")
durees <- c("Tous", "Express (<20 min)", "Rapide (<30 min)", "Normale (<1 heure)", "Longue (<2 heures)")
origine <- c("Tous", "France", "Italie", "États-Unis", "Inde", "Suisse", "Mexique", "International")

liste_pays <- c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Côte d'Ivoire", "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo (Congo-Brazzaville)", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Democratic Republic of the Congo", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini (Swaziland)", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Holy See", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar (Burma)", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea", "North Macedonia (formerly Macedonia)", "Norway", "Oman", "Pakistan", "Palau", "Palestine State", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", "Syria", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States of America", "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe")

ui <- dashboardPage(
  dashboardHeader(title = "NutriPlaisirs"),
  dashboardSidebar(
    sidebarMenu(
      # Onglets dans le menu
      menuItem("Informations utilisateur", tabName = "user", icon = icon("user")), 
      menuItem("Choix de Recettes", tabName = "filter_recipes", icon = icon("cutlery")),
      menuItem("Recommandations", tabName = "aides", icon = icon("search")),
      menuItem("À propos", tabName = "about", icon = icon("info-circle"))
    )
  ),
  #style de l'interface
  dashboardBody(
    tags$head(tags$style(HTML('
                              /* Styles du logo dans len-tête */
                                .skin-blue .main-header .logo {
                                  background-color: #354B61; /* Couleur de fond du logo */
                                    color: #ECF0F5; /* Couleur du texte des autres liens dans le menu latéral */
                                }
                              
                              /* Styles du logo lorsquil est survolé */
                                .skin-blue .main-header .logo:hover {
                                  background-color: #2E4154; /* Couleur de fond du logo lorsquil est survolé */
                                                              }
                              
                              /* Styles de la barre de navigation */
                                .skin-blue .main-header .navbar {
                                  background-color: #496785; /* Couleur de fond de la barre de navigation */
                                }
                              
                              /* Styles de la barre latérale principale */
                                .skin-blue .main-sidebar {
                                  background-color: #354B61; /* Couleur de fond de la barre latérale principale */
                                }
                              
                              /* Styles de longlet sélectionné dans le menu latéral */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
                                  background-color: #1D2A36; /* Couleur de fond de longlet sélectionné dans le menu latéral */
                                }
                              
                              /* Styles des autres liens dans le menu latéral */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a {
                                  background-color: #263545; /* Couleur de fond des autres liens dans le menu latéral */
                                    color: #FFFFFF; /* Couleur du texte des autres liens dans le menu latéral */
                                }
                              
                              /* Styles des autres liens dans le menu latéral lorsquils sont survolés */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
                                  background-color: #1D2A36; /* Couleur de fond des autres liens dans le menu latéral lorsquils sont survolés */
                                }
                              
                              /* Styles du bouton de basculement lorsquil est survolé */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover {
                                  background-color: #3E5770; /* Couleur de fond du bouton de basculement lorsquil est survolé */
                                }
                              
                              /* Styles de la zone de contenu */
                                .content-wrapper, .right-side {
                                  background-color: #ECF0F5; /* Couleur de fond de la zone de contenu */
                                }
                              
                              /* Styles des éléments de menu dans la barre latérale */
                                .treeview-menu > li > a {
                                  width: 240px; /* Définir une largeur fixe pour les éléments de menu */
                                }
                              
                              /* Styles de la boîte personnalisée */
                                .custom-box {
                                  background-color: #E1E9F5; /* Couleur de fond de la boîte personnalisée */
                                }
                              
                              /* Styles du popup */
                                .modal-content {
                                  background-color: #E1E9F5; /* Couleur de fond du popup */
                                }
                              
                              /* Styles de len-tête du popup */
                                .modal-header {
                                  background-color: #3C8DBC; /* Couleur de fond de len-tête du popup */
                                color: #fff; /* Couleur du texte de len-tête du popup */
                                }

                              '))),
    
    # Définir la largeur du corps du tableau de bord
    width = "100%",
    tabItems(
      # 2ème onglet Choix de Recettes
      tabItem(
        tabName = "filter_recipes",
        h2("Choix de Recettes"),
        fluidRow(
          box(
            width = 12, # Utiliser la largeur complète de la rangée
            title = "Filtrer les Recettes",
            status="primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(2, selectizeInput("ingredient", "Ingrédients", choices = ingredients, multiple = TRUE, options = list(
                placeholder = "Sélectionnez les ingrédients...",
                plugins = list('remove_button')
              ))),
              column(2, selectizeInput("allergene", "Allergènes", choices = allergenes, multiple = TRUE, options = list(
                placeholder = "Sélectionnez les allergènes...",
                plugins = list('remove_button')
              ))),
              column(2, selectizeInput("regime", "Diète", choices = setNames(regimes, regimes), options = list(
                placeholder = "Sélectionnez la diète...",
                plugins = list('remove_button')
              ))),
              column(2, selectizeInput("type_recette", "Type de plat", choices = types_plat, options = list(
                placeholder = "Sélectionnez le type de plat...",
                plugins = list('remove_button')
              ))),
              column(2, selectizeInput("duree", "Durée de préparation", choices = setNames(durees, durees), options = list(
                placeholder = "Sélectionnez la durée de préparation...",
                plugins = list('remove_button')
              ))),
              column(2, selectizeInput("origine", "Origine", choices = origine, options = list(
                placeholder = "Sélectionnez le type d'origine...",
                plugins = list('remove_button')
              )))
            ),
            class = "custom-box" # Appliquer couleur du fond de box
          )
        ),
        fluidRow(
          box(
            width = 12, # Utiliser la largeur complète de la rangée
            title = "Recettes",
            status="primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            DTOutput("recettes_table"),
            class = "custom-box" # Appliquer couleur du fond de box
          )
        )
      ),
      # 4ème onglet About l'appli
      tabItem(
        tabName = "about",
        h2("À propos de NutriPlaisirs"),
        h3("NutriPlaisirs : Des Recettes Santé à Votre Goût"),
        p("Cette application a été développée pour vous aider à trouver des recettes saines et délicieuses."),
        p("Elle vous permet de filtrer les recettes en fonction de divers critères tels que les ingrédients, les allergènes, le type de plat, la durée de préparation et l'origine."),
        p("Profitez de NutriPlaisirs pour découvrir de nouvelles idées culinaires et ravir vos papilles !")
      ),
      # 1er onglet Informations utilisateur
      tabItem(
        tabName = "user",
        h2("Informations utilisateur"),
        sidebarLayout(
          sidebarPanel(
            radioButtons("sexe", "Sexe :", choices = c("M" = "M.", "F" = "Mme")),
            textInput("nom", "Nom:", placeholder = "Entrez votre nom"),
            textInput("prenom", "Prénom:", placeholder = "Entrez votre prénom"),
            numericInput("poids", "Poids (en kg):", value = NULL, min = 10, max = 500, step = 0.1),
            sliderInput("taille", "Taille (en mètres):", min = 0.5, max = 2.5, value = 1.7, step = 0.01),
            selectInput("pays", "Pays :", choices = liste_pays),
            actionButton("calculer", "Calculer l'IMC")
          ),
          mainPanel(
            h3("Votre IMC est :"),
            textOutput("resultat_imc"),
            br(),
            h4("Conseils :"),
            fluidRow(
              column(6, textOutput("imcAdvice")),
              column(6, uiOutput("physique_image"))
            )
          )
        )
      ),
      # 3ème onglet Recommandations
      tabItem(
        tabName = "aides",
        h2("Recommandations"),
        p("Suggestions à mettre")
      )
    )
  )
)