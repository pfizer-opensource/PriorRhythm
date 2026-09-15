library(PriorRhythm)

# Normal use: get path to the package logo
logo_path <- get_package_logo_path()
cat("Logo path:", logo_path, "\n")
cat("Logo file exists:", file.exists(logo_path), "\n")

# Custom package name (would return "" if not installed)
custom_path <- get_package_logo_path(package = "PriorRhythm")
cat("Custom path result:", custom_path, "\n")
