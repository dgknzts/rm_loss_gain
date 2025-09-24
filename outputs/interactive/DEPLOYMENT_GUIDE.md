---
editor_options: 
  markdown: 
    wrap: 72
---

# Shiny App Deployment Guide

## Quick Deployment to shinyapps.io

### Step 1: Install rsconnect (if not already installed)

``` r
install.packages("rsconnect")
```

### Step 2: Create shinyapps.io account

-   Go to: <https://www.shinyapps.io/>
-   Sign up for free account (25 hours/month, 5 apps)

### Step 3: Get your deployment credentials

-   Login to shinyapps.io
-   Go to: <https://www.shinyapps.io/admin/#/tokens>
-   Click "Show" next to your token
-   Copy the `rsconnect::setAccountInfo()` command

### Step 4: Configure deployment in R

``` r
# Paste your credentials (from step 3)
rsconnect::setAccountInfo(name='dgknzts', 
                          token='xxx', 
                          secret='xxx')
```

### Step 5: Deploy your app

\# Navigate to the interactive_plot directory

setwd("G:/My Drive/Projects/RM/RM_loss_n_gain/interactive_plot")

\# Deploy the app

rsconnect::deployApp(

appDir = ".",

appName = "rm-redundancy-masking",

appTitle = "Redundancy Masking Interactive Analysis"

)

``` r
# Navigate to the interactive_plot directory
setwd("G:/My Drive/Projects/RM/RM_loss_n_gain/interactive_plot")

# Deploy the app
rsconnect::deployApp(
  appDir = ".",
  appName = "rm-redundancy-masking",
  appTitle = "Redundancy Masking Interactive Analysis"
)
```

## What Gets Deployed

Your app deployment will include: - ✅ `app.R` (main application) - ✅
`../datasets/processed.csv` (data file - automatically found by app) -
✅ `../preAnalysis/helpers/theme_scientific.R` (if exists - optional)

The app's `find_data_file()` function will automatically locate the data
file.

## After Deployment

-   Your app will be available at:
    `https://YOUR_USERNAME.shinyapps.io/rm-redundancy-masking/`
-   Monitor usage at: <https://www.shinyapps.io/admin/#/applications>
-   Free tier: 25 active hours/month, 5 applications max

## Troubleshooting

If deployment fails: 1. Check all required packages are installed 2.
Test app locally first: `shiny::runApp("app.R")` 3. Check shinyapps.io
logs in the admin dashboard
