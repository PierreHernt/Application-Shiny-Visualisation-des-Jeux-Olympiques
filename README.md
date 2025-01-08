
# Application Shiny: Visualisation des Jeux Olympiques

Cette application Shiny a été développée pour visualiser des données sur les Jeux Olympiques de 1896 à 2022. Elle permet d'explorer les évolutions des athlètes, disciplines, caractéristiques physiques, et médailles.
Le fichier de données de base : https://www.kaggle.com/datasets/piterfm/olympic-games-medals-19862018

Lien de l'application déployée :  https://jovisualisation.shinyapps.io/Appli_R/

## Structure du projet

- **`ui.R`** : Définit l'interface utilisateur de l'application.
- **`server.R`** : Contient la logique serveur.
- **`www/`** : Contient les ressources statiques (images, CSS, etc.).
- **`data/`** : Contient des fichiers de données nécessaires.
- **`R/`** : Contient le traitement de données préalable. 
## Utilisation

1. Installez les packages R nécessaires :

2. Lancez l'application depuis R :
   ```R
   library(shiny)
   runApp(".")
   ```

## Auteur(s)
- **Anthony Place**
- **Pierre Hernot**
