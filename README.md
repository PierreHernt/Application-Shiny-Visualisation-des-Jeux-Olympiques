
# Application Shiny: Visualisation des Jeux Olympiques

Cette application Shiny a été développée pour visualiser des données sur les Jeux Olympiques de 1896 à 2022. Elle permet d'explorer les évolutions des athlètes, disciplines, caractéristiques physiques, et médailles.

## Structure du projet

- **`ui.R`** : Définit l'interface utilisateur de l'application.
- **`server.R`** : Contient la logique serveur.
- **`www/`** : Contient les ressources statiques (images, CSS, etc.).
- **`data/`** : (optionnel) Peut contenir des fichiers de données nécessaires.

## Utilisation

1. Installez les packages R nécessaires :
   ```R
   install.packages(c("shiny", "ggplot2", "plotly", "DT", "reactable"))
   ```

2. Lancez l'application depuis R :
   ```R
   library(shiny)
   runApp(".")
   ```

## Auteur(s)
- **Anthony Place**
- **Pierre Hernot**
