# Application-Shiny-Visualisation-des-Jeux-Olympiques

Cette application Shiny a été développée pour visualiser des données sur les Jeux Olympiques de 1896 à 2022. Elle permet d'explorer les évolutions des athlètes, disciplines, caractéristiques physiques, et médailles.

## Application Déployée"
Lien vers l'application Shiny déployée (délai important) : https://jovisualisation.shinyapps.io/Appli_R/

## Structure du projet

- **`ui.R`** : Définit l'interface utilisateur de l'application.
- **`server.R`** : Contient la logique serveur.
- **`www/`** : Contient les ressources statiques (images, CSS, etc.).
- **`Source_deploy`** : Permet de calculer certaines variables.

## Utilisation

Lancez l'application depuis R :
   ```R
   library(shiny)
   runApp(".")
   ```

## Auteur(s)
- **Anthony Place**
- **Pierre Hernot**
