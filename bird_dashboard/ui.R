source("tabs/welcome.R")
source("tabs/dashboard.R")
source("tabs/method.R")
source("tabs/model_access.R")
source("tabs/contact.R")

ui <- navbarPage(
  title = "Bird Distribution Dashboard",
  id = "top_tabs",
  
  theme = bs_theme(
    version = 5,
    primary = "#153B40",
    secondary = "#1E5055",
    base_font = font_google("Inter")
  ),
  
  header = tags$head(
    tags$style(HTML("
      .navbar {
        background-color: #153B40 !important;
      }

      .navbar .navbar-brand,
      .navbar .nav-link {
        color: white !important;
      }

      .navbar .nav-link.active {
        background-color: #1E5055 !important;
      }

      body {
        background-color: #F6F9F9;
      }
    "))
  ),
  
  tabPanel("Welcome", welcomeUI()),
  tabPanel("Dashboard", dashboardUI()),
  tabPanel("Method", methodUI()),
  tabPanel("Model Access", modelAccessUI()),
  tabPanel("Contact", contactUI())
)