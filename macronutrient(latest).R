# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(RSQLite)
library(lubridate)
library(plotly)
library(shinyBS)
library(shinyjs)

# JavaScript code for shinyjs
jsCode <- "
shinyjs.disableButton = function(id) {
  $('#' + id).attr('disabled', true);
}
shinyjs.enableButton = function(id) {
  $('#' + id).removeAttr('disabled');
}
"

# Food database
food_db <- data.frame(
  food = c("Egg", "Chicken Breast", "Rice", "Apple", "Milk", "Almonds", "Broccoli", "Salmon", "Quinoa", "Greek Yogurt", "Avocado", "Sweet Potato", "Oats", "Peanut Butter", "Banana"),
  calories = c(78, 165, 204, 95, 103, 575, 55, 208, 120, 59, 160, 86, 389, 588, 105),
  protein = c(6, 31, 4.2, 0.5, 8, 21, 3.7, 20, 4.1, 10, 2, 1.6, 17, 25, 1.3),
  carbs = c(0.6, 0, 44.5, 25, 12, 22, 11, 0, 21, 3.6, 8.5, 20, 66, 20, 27),
  fats = c(5.3, 3.6, 0.4, 0.3, 2.4, 49, 0.6, 13, 1.9, 0.4, 15, 0.1, 7, 50, 0.3),
  stringsAsFactors = FALSE
)

# Define UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = tags$b("Macro Tracker Plus")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Recommendations", tabName = "recommend", icon = icon("calculator")),
      menuItem("Food Log", tabName = "log", icon = icon("utensils")),
      menuItem("Progress & Analytics", tabName = "progress", icon = icon("chart-line")),
      menuItem("Settings", tabName = "settings", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("disableButton", "enableButton")),
    tabItems(
      tabItem(tabName = "home",
        fluidRow(
          box(title = "Welcome to Macro Tracker Plus!", width = 12, status = "info", solidHeader = TRUE,
              tags$h3("Track your nutrition effortlessly and efficiently."),
              tags$p("Navigate via the menu to log foods, get personalized recommendations, and analyze your progress."),
              tags$img(src = "https://cdn-icons-png.flaticon.com/512/1046/1046771.png", height = "100px")
          )
        )
      ),
      tabItem(tabName = "recommend",
        fluidRow(
          box(
            title = "Personalized Recommendations", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(6,
                     numericInput("weight", "Weight (kg)", value = 70, min = 1, step = 0.1),
                     numericInput("height", "Height (cm)", value = 170, min = 1, step = 0.1),
                     numericInput("age", "Age (years)", value = 25, min = 1, step = 1)
              ),
              column(6,
                     selectInput("sex", "Sex", choices = c("Male", "Female")),
                     selectInput("activity", "Activity Level", choices = c(
                       "Sedentary" = 1.2,
                       "Lightly Active" = 1.375,
                       "Moderately Active" = 1.55,
                       "Very Active" = 1.725,
                       "Extra Active" = 1.9)),
                     selectInput("goal", "Goal", choices = c("Lose Weight", "Maintain", "Bulk"))
              )
            ),
            actionButton("calc", "Calculate Recommendations", class = "btn-primary"),
            actionButton("save_profile", "Save Profile", class = "btn-info"),
            actionButton("load_profile", "Load Profile", class = "btn-secondary"),
            br(), br(),
            verbatimTextOutput("recommendations")
          )
        )
      ),
      tabItem(tabName = "log",
        fluidRow(
          box(
            title = "Add Food Entry", width = 12, status = "success", solidHeader = TRUE,
            fluidRow(
              column(7, 
                     selectizeInput("food_choice", "Choose Food", choices = c(food_db$food, "Custom..."))),
              column(3, numericInput("qty", "Quantity (servings)", value = 1, min = 0.1, step = 0.1)),
              column(2, actionButton("add", "Add Entry", class = "btn-success"))
            ),
            conditionalPanel(
              condition = "input.food_choice == 'Custom...'",
              fluidRow(
                column(4, textInput("custom_name", "Custom Food Name")),
                column(2, numericInput("custom_cal", "Calories", value = NA, min = 0)),
                column(2, numericInput("custom_protein", "Protein (g)", value = NA, min = 0)),
                column(2, numericInput("custom_carbs", "Carbs (g)", value = NA, min = 0)),
                column(2, numericInput("custom_fats", "Fats (g)", value = NA, min = 0))
              )
            )
          )
        )
      ),
      tabItem(tabName = "progress",
        fluidRow(
          box(
            title = "View Past Intake by Date", width = 12, status = "warning", solidHeader = TRUE,
            dateInput("view_date", "Select Date", value = Sys.Date()),
            DTOutput("past_log")
          ),
          box(
            title = "Macronutrient Breakdown", width = 12, status = "warning", solidHeader = TRUE,
            plotlyOutput("macro_pie")
          )
        ),
        fluidRow(
          box(
            title = "Calories Consumed Today", width = 12, status = "danger", solidHeader = TRUE,
            plotOutput("cal_chart")
            )
          )
        ),
      
      tabItem(tabName = "settings",
        fluidRow(
          box(
            title = "Settings & Info", width = 12, status = "primary", solidHeader = TRUE,
            tags$h4("Application Version: 1.0.0"),
            tags$p("This is a prototype macro tracker application built in R Shiny.")
            )
          )
        )
      )
    )
  )


# Define server
server <- function(input, output, session) {
  db <- dbConnect(SQLite(), "macro_tracker.db")
  
  # Create tables if they don't exist
  dbExecute(db, "CREATE TABLE IF NOT EXISTS intake_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    date TEXT, food TEXT, calories REAL, protein REAL, carbs REAL, fats REAL)")
  
  dbExecute(db, "CREATE TABLE IF NOT EXISTS user_profile (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    weight REAL, height REAL, age INTEGER, sex TEXT, activity REAL, goal TEXT)")
  
  user_targets <- reactiveValues(cal = NA, protein = NA, carbs = NA, fats = NA)
  today_data <- reactiveVal(data.frame())
  
  update_today_data <- function() {
    today_data(dbGetQuery(db, sprintf("SELECT * FROM intake_log WHERE date = '%s'", Sys.Date())))
  }
  update_today_data()
  
  observeEvent(input$calc, {
    shinyjs::disable("calc")
    on.exit(shinyjs::enable("calc"))
    
    bmr <- if (input$sex == "Male") {
      10 * input$weight + 6.25 * input$height - 5 * input$age + 5
    } else {
      10 * input$weight + 6.25 * input$height - 5 * input$age - 161
    }
    tdee <- bmr * as.numeric(input$activity)
    cal <- switch(input$goal, "Lose Weight" = tdee - 500, "Bulk" = tdee + 300, tdee)
    protein <- input$weight * switch(input$goal, "Lose Weight" = 2.0, "Bulk" = 2.2, 1.8)
    fat <- (0.25 * cal) / 9
    carbs <- max(0, (cal - (protein * 4 + fat * 9)) / 4)
    
    user_targets$cal <- cal
    user_targets$protein <- protein
    user_targets$carbs <- carbs
    user_targets$fats <- fat
    
    output$recommendations <- renderText({
      paste0("Calories Goal: ", round(cal), " kcal\n",
             "Protein Goal: ", round(protein), " g\n",
             "Carbs Goal: ", round(carbs), " g\n",
             "Fats Goal: ", round(fat), " g")
    })
  })
  
  observeEvent(input$add, {
    req(input$qty > 0)
    
    if (input$food_choice == "Custom...") {
      req(input$custom_name, input$custom_cal, input$custom_protein, input$custom_carbs, input$custom_fats)
      food_name <- input$custom_name
      cal <- input$custom_cal * input$qty
      prot <- input$custom_protein * input$qty
      carb <- input$custom_carbs * input$qty
      fat <- input$custom_fats * input$qty
    } else {
      food <- food_db[food_db$food == input$food_choice, ]
      if (nrow(food) == 0) return()
      food_name <- input$food_choice
      cal <- food$calories * input$qty
      prot <- food$protein * input$qty
      carb <- food$carbs * input$qty
      fat <- food$fats * input$qty
    }
    
    dbExecute(db, "INSERT INTO intake_log (date, food, calories, protein, carbs, fats) VALUES (?, ?, ?, ?, ?, ?)",
              params = list(as.character(Sys.Date()), food_name, cal, prot, carb, fat))
    update_today_data()
    output$add_status <- renderText("Food entry added successfully!")
  })
  
  output$today_log <- renderDT({
    data <- today_data()
    if (nrow(data) == 0) {
      datatable(data.frame(Message = "No food entries today"), options = list(dom = 't'))
    } else {
      datatable(data[, c("date", "food", "calories", "protein", "carbs", "fats")],
                colnames = c("Date", "Food", "Calories", "Protein", "Carbs", "Fats"))
    }
  })
  
  output$totals <- renderText({
    data <- today_data()
    if (nrow(data) == 0) return("No intake recorded today")
    totals <- data %>% summarise(across(c(calories, protein, carbs, fats), sum, na.rm = TRUE))
    
    paste0("Total Intake:\n",
           "Calories: ", round(totals$calories), " kcal\n",
           "Protein: ", round(totals$protein), " g\n",
           "Carbs: ", round(totals$carbs), " g\n",
           "Fats: ", round(totals$fats), " g\n\n",
           if (is.na(user_targets$cal)) "Set your targets in the Recommendations tab" else
             paste0("Remaining:\n",
                    "Calories: ", round(user_targets$cal - totals$calories), " kcal\n",
                    "Protein: ", round(user_targets$protein - totals$protein), " g\n",
                    "Carbs: ", round(user_targets$carbs - totals$carbs), " g\n",
                    "Fats: ", round(user_targets$fats - totals$fats), " g"))
  })
  
  output$past_log <- renderDT({
    data <- dbGetQuery(db, sprintf("SELECT * FROM intake_log WHERE date = '%s'", input$view_date))
    if (nrow(data) == 0) {
      datatable(data.frame(Message = "No entries found for selected date"), options = list(dom = 't'))
    } else {
      datatable(data[, c("date", "food", "calories", "protein", "carbs", "fats")],
                colnames = c("Date", "Food", "Calories", "Protein", "Carbs", "Fats"))
    }
  })
  
  output$macro_pie <- renderPlotly({
    data <- dbGetQuery(db, sprintf("SELECT SUM(protein) as protein, SUM(carbs) as carbs, SUM(fats) as fats FROM intake_log WHERE date = '%s'", input$view_date))
    if (sum(unlist(data), na.rm = TRUE) == 0) {
      plot_ly() %>% add_annotations(text = "No data available", x = 0.5, y = 0.5, showarrow = FALSE)
    } else {
      plot_ly(labels = c("Protein", "Carbs", "Fats"),
              values = c(data$protein, data$carbs, data$fats),
              type = "pie") %>%
        layout(title = "Macronutrient Breakdown")
    }
  })
  
  output$cal_chart <- renderPlot({
    data <- today_data()
    if (nrow(data) == 0 || is.na(user_targets$cal)) {
      ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available") + theme_void()
    } else {
      total_cal <- sum(data$calories, na.rm = TRUE)
      remaining <- max(0, user_targets$cal - total_cal)
      df <- data.frame(Category = c("Consumed", "Remaining"), Calories = c(total_cal, remaining))
      ggplot(df, aes(x = Category, y = Calories, fill = Category)) +
        geom_col() + theme_minimal()
    }
  })
  
  observeEvent(input$save_profile, {
    dbExecute(db, "DELETE FROM user_profile")
    dbExecute(db, "INSERT INTO user_profile (weight, height, age, sex, activity, goal) VALUES (?, ?, ?, ?, ?, ?)",
              params = list(input$weight, input$height, input$age, input$sex, as.numeric(input$activity), input$goal))
    showNotification("Profile saved.")
  })
  
  observeEvent(input$load_profile, {
    data <- dbGetQuery(db, "SELECT * FROM user_profile LIMIT 1")
    if (nrow(data) > 0) {
      updateNumericInput(session, "weight", value = data$weight)
      updateNumericInput(session, "height", value = data$height)
      updateNumericInput(session, "age", value = data$age)
      updateSelectInput(session, "sex", selected = data$sex)
      updateSelectInput(session, "activity", selected = as.character(data$activity))
      updateSelectInput(session, "goal", selected = data$goal)
      showNotification("Profile loaded.")
    }
  })
  
  session$onSessionEnded(function() {
    dbDisconnect(db)
  })
}

# Run app
shinyApp(ui, server)
