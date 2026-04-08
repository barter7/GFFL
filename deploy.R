# deploy.R
# Deploy the GFFL Dashboard to shinyapps.io
#
# Prerequisites:
#   1. Install rsconnect: install.packages("rsconnect")
#   2. Configure your shinyapps.io account:
#      rsconnect::setAccountInfo(
#        name   = "your-account-name",
#        token  = "your-token",
#        secret = "your-secret"
#      )
#      (Find these at https://www.shinyapps.io/admin/#/tokens)
#
# Usage: Rscript deploy.R

library(rsconnect)

deployApp(
  appDir = ".",
  appName = "gffl-dashboard",
  appTitle = "GFFL Historical Dashboard",
  appFiles = c("app.R", "helpers.R"),
  forceUpdate = TRUE
)
