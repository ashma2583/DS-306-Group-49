#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# ============================================================
# Title: The Cardiometabolic Profile of Low Testosterone in Adult Males
# Dataset: cleaned_health_data (from NHANES)
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(broom)

# ------------------------------------------------------------
# DATA PREPARATION
# ------------------------------------------------------------
cleaned_health_data <- NHANES |>
  filter(Gender == "male", !is.na(Testosterone)) |>
  select(Testosterone, 
         Age, BPSysAve, BPDiaAve, Pulse, TotChol, 
         BMI, Weight, UrineFlow1, 
         Diabetes, PhysActive, HealthGen) |>
  distinct() |>
  drop_na() |>
  mutate(
    Diabetes = factor(Diabetes, levels = c("No", "Yes")),
    PhysActive = factor(PhysActive, levels = c("No", "Yes")),
    HealthGen = factor(HealthGen, 
                       levels = c("Poor", "Fair", "Good", "Vgood", "Excellent"))
  )

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("The Cardiometabolic Profile of Low Testosterone in Adult Males"),
  p("Explore how physiological and lifestyle factors—such as BMI, Pulse, Blood Pressure, and Physical Activity—
    relate to measured testosterone levels among adult males in NHANES."),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filter Options"),
      selectInput("diabetes", "Filter by Diabetes Status:", 
                  choices = c("All", "Yes", "No"), selected = "All"),
      selectInput("phys_active", "Filter by Physical Activity:", 
                  choices = c("All", "Yes", "No"), selected = "All"),
      
      selectInput("xvar", "Select X Variable:",
                  choices = c("Age", "BMI", "Weight", "Pulse", 
                              "BPDiaAve", "BPSysAve", "UrineFlow1", "TotChol"),
                  selected = "BMI"),
      
      checkboxInput("logT", "Log-transform Testosterone", value = FALSE),
      hr(),
      
      h4("Model Options"),
      checkboxGroupInput("controls", "Add Control Variables:",
                         choices = c("Age", "BMI", "Weight", "Pulse", 
                                     "PhysActive", "Diabetes", 
                                     "TotChol", "BPDiaAve", "BPSysAve", 
                                     "UrineFlow1", "HealthGen"),
                         selected = c("BMI", "Pulse")),
      
      actionButton("run_model", "Run Regression", class = "btn-primary"),
      hr(),
      helpText("Data source: NHANES (U.S. National Health and Nutrition Examination Survey)")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotOutput("scatterPlot"),
                 verbatimTextOutput("corrText")),
        tabPanel("Regression Summary",
                 h4("Current Model Formula:"),
                 verbatimTextOutput("formulaText"),
                 h4("Regression Output:"),
                 verbatimTextOutput("modelSummary")),
        tabPanel("About",
                 h4("About This App"),
                 p("This Shiny app visualizes the relationships between testosterone levels and key
                   physiological and lifestyle indicators from the NHANES dataset. It allows users
                   to subset the data interactively and view regression outputs in real time."),
                 p("The purpose is to highlight how cardiovascular, metabolic, and behavioral factors
                   together shape male hormonal health."))
      )
    )
  )
)
