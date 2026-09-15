library(PriorRhythm)

# Get logo path
logo_path <- get_package_logo_path()
cat("Logo file exists:", file.exists(logo_path), "\n")

# In a Shiny UI, add the logo to the sidebar:
if (FALSE) {
  shiny::sidebarPanel(
    embed_spba_logo()
  )
}