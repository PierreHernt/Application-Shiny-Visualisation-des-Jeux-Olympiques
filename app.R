library(shiny)

# Charger les fichiers sources
source("global.R")
source("ui.R")
source("server.R")

# Lancer l'application
shinyApp(ui = ui, server = server)