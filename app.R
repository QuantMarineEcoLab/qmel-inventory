<<<<<<< Updated upstream
# load in packages
library(shiny) # core Shiny framework to recognize the app 
library(dplyr)
library(data.table)
library(DT)
library(shinythemes)
library(lubridate)
library(googlesheets4)
library(shinyFeedback) # adds feedback messages for user inputs (ex: checkout completed! prompt) 
# Set up Google Sheets 
gs4_deauth()
sheet_id <- "https://docs.google.com/spreadsheets/d/1sDfqiv1SF6aebmXRdlyvGeByDgkp67X2PvLJ6tz0-Eg/edit?gid=0#gid=0"

# Set this up but for QMEL?
 gs4_auth(cache = ".secrets", email = "quantmarineecolab@gmail.com")


# Helper function to create empty checkout table --------
empty_checkout <- function() {
  data.frame(
    checkout_id = character(),
    checked_out_by = character(),
    item = character(),
    # quantity = numeric(),
    quantity = character(),
    location = character(),
    checkout_start = character(),
    checked_out_until = character(),
=======


# Load required packages
library(shiny)          # Core Shiny framework
library(dplyr)     
library(data.table)     
library(DT)             # Interactive tables
library(shinyFeedback)  # Feedback messages for user inputs
library(googlesheets4) # adds google sheets as the shared backend inputs


# Helper function to create an empty checkout table -------
# This guarantees structure + column order never changes

empty_checkout <- function() {
  data.frame(
    checked_out_by = character(),       
    item = character(),                 
    quantity = numeric(),              
    location = character(),             
    checkout_start = character(),       
    checked_out_until = character(),    
>>>>>>> Stashed changes
    stringsAsFactors = FALSE
  )
}

<<<<<<< Updated upstream
# load in datasets
# Set column type to be character only for simplicity
users <- read_sheet(sheet_id, sheet = "users", col_types = "c")
# Create avail_users as a reactiveValues data frame that can be modified

inventory <- read_sheet(sheet_id, sheet = "inventory", col_types = "c")
checkout <- read_sheet(sheet_id, sheet = "checkout", col_types = "c")

# UI (user interface... what the people see!) --------
ui <- fluidPage( theme = shinytheme("journal"),
  useShinyFeedback(),
    # App title
  titlePanel("QMEL Inventory App", windowTitle = "QMEL Inventory App"),
  # Creating tabbed layout
  tabsetPanel(id = "mainTabset",
              
              # -----    WELCOME TAB UI  --------
=======

# LOAD in datasets ------
# users and inventory CSVs 
# set up link to spreadsheet in google sheets

sheet_url <- "https://docs.google.com/spreadsheets/d/1mHr9UwnSpJv8lLR9dXC8shMHeGZyNA51-ayzFfctlUA"

users <- read_sheet(sheet_url, sheet = "users")
inventory <- read_sheet(sheet_url, sheet = "inventory")   

# Checkout CSV handling
# This section FORCE-CONTROLS column types to prevent crashes
if (!file.exists("checkout.csv")) {
  
  checkout <- empty_checkout()
  fwrite(checkout, "checkout.csv")
  
} else {
  
  checkout <- fread(
    "checkout.csv",
    colClasses = list(
      character = c("checked_out_by",
                    "item",
                    "location",
                    "checkout_start",
                    "checked_out_until"),
      numeric = "quantity"
    )
  )
  
  # If file exists but is empty
  if (nrow(checkout) == 0) {
    checkout <- empty_checkout()
  }
}

# UI (USER INTERFACE) -------
ui <- fluidPage(
  
  useShinyFeedback(),
  
  titlePanel("QMEL Lab Supplies Inventory"),
  
  tabsetPanel(id = "mainTabset",
              
              # -------------------
              # Welcome tab
              # -------------------
>>>>>>> Stashed changes
              tabPanel(
                icon = icon("house"),
                "Welcome",
                
<<<<<<< Updated upstream
                tags$h3("Welcome to the QMEL Inventory and Checkout System!"),
                tags$br(),
=======
                h3("Welcome to the QMEL Supply Inventory Checkout System and Record"),
                br(),
                h4("A quick overview of how the app works:"),
                p("This app helps track lab inventory, checkouts, and returns in a coordinated way."),
                br(),
>>>>>>> Stashed changes
                
                h4(HTML('<i class = "fa-solid fa-box-open"></i> Inventory Overview Tab:')),
                p("View available inventory and what users have checked out."),
                br(),
                
                h4(HTML('<i class = "fa-solid fa-cart-shopping"></i> Checkout Tab:')),
                p("Check out items to yourself, add to cart, and submit checkout."),
                br(),
                
                h4(HTML('<i class = "fa-solid fa-users"></i> Modify Users Tab:')),
                p("Add or remove users. Removing users deletes their checkout history."),
                br(),
                
                h4(HTML('<i class = "fa-solid fa-box"></i> Full Inventory Tab:')),
                p("Shows full inventory along with who has each item checked out."),
                br()
              ),
              
<<<<<<< Updated upstream
              # ------ Inventory Overview Tab UI --------
              tabPanel(icon = icon("box-open"), "Inventory Overview",
                       DTOutput("inventory")
              ),
              
              # ------ Checkout/Return Tab UI ----------
              tabPanel(
                icon = icon("shopping-cart"),
                "Checkout / Return",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("checkout_name","User", choices = users$name, selected=""), # select user
                           selectInput("checkout_item","Item", choices = inventory$item), # select item
                           numericInput("checkout_quant","Quantity",1,min=1), # select quantity of items 
                           textInput("checkout_location","Location"),
                           dateRangeInput("checkout_dates","Checkout timeframe"),
                           actionButton("addToCart","Add to cart", style="color: #fff; background-color: #337ab7; border-color: #2e6da4") # ADD to cart button
                         ),
                         
                         # table of items currently checked out by selected user 
                         mainPanel(
                           h4("Items currently checked out"),
                           DTOutput("myItems"),
                           actionButton("returnItems","Return selected", style="color: #fff; background-color: #C23E3E; border-color: #A81D1D"), # return items button 
                           
                           br(), br(),
                           
                           # temporary cart table
                           h4("Cart"),
                           DTOutput("cart"),
                           
                           #submit checkout button 
                           actionButton("submitCheckout","Submit checkout", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                         )
                       )
=======
              
              # Inventory Overview -----
              tabPanel(
                icon = icon("box-open"),
                "Inventory Overview",
                h3("Available Inventory"),
                DTOutput("inventory")
              ),
              
              
              # Checkout / Return ---------
              tabPanel(
                icon = icon("shopping-cart"),
                "Checkout / Return",
                
                sidebarLayout(
                  
                  sidebarPanel(
                    
                    selectInput("checkout_name", "User", choices = users$name),
                    selectInput("checkout_item", "Item", choices = inventory$item),
                    numericInput("checkout_quant", "Quantity", 1, min = 1),
                    textInput("checkout_location", "Location"),
                    dateRangeInput("checkout_dates", "Checkout timeframe"),
                    actionButton("addToCart", "Add to cart", class = "btn-primary")
                  ),
                  
                  mainPanel(
                    
                    h4("Items currently checked out"),
                    DTOutput("myItems"),
                    actionButton("returnItems", "Return selected", class = "btn-danger"),
                    
                    br(), br(),
                    
                    h4("Cart"),
                    DTOutput("cart"),
                    
                    actionButton("submitCheckout", "Submit checkout", class = "btn-primary")
                  )
                )
>>>>>>> Stashed changes
              ),
              # ------- USERS (UI) -------
              tabPanel(icon = icon("users"), "Modify Users",
                       sidebarLayout(
                         sidebarPanel(
                           verticalLayout(
                             tags$h3("Add users"),
                             # Text input for name and email
                             fluidRow(column(12, textInput("name", "First and last name", placeholder = "Joe Shmoe"))),
                             fluidRow(column(12, textInput("email", "UNH email", placeholder = "Joe.Shmoe@unh.edu"))),
                             # Action Button to add user
                             fluidRow(column(12, actionButton("addName", "Add User",
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                           )),
                         mainPanel(
                           # Show table of available users
                           tags$h3("Available users"),
                           tags$h5("Select rows in table and click 'Remove' button to remove users"),
                           fluidRow(column(12, DT::DTOutput("newUser"))),
                           # Create Action button to remove users (based on selected rows)
                           fluidRow(column(12, actionButton("removeName", "Remove Selected Users",
                                                            style="color: #fff; background-color: #C23E3E; border-color: #A81D1D"))))
                       )),
              
<<<<<<< Updated upstream
              # Full Inventory Tab UI
              tabPanel(icon = icon("box"), "Full Inventory",
                       DTOutput("fullInventory")
=======
              
              # Modify Users -----
              tabPanel(
                icon = icon("users"),
                "Modify Users",
                
                sidebarLayout(
                  
                  sidebarPanel(
                    textInput("name", "Name"),
                    textInput("email", "Email"),
                    actionButton("addUser", "Add user", class = "btn-primary")
                  ),
                  
                  mainPanel(
                    DTOutput("userTable"),
                    actionButton("removeUser", "Remove selected", class = "btn-danger")
                  )
                )
              ),
              
              # Full Inventory -----
              tabPanel(
                icon = icon("box"),
                "Full Inventory",
                h3("Full Inventory + Checkout Status"),
                DTOutput("fullInventory")
>>>>>>> Stashed changes
              )
              
  )
)

<<<<<<< Updated upstream
# ---- SERVER -------
server <- function(input, output, session) {
  
  # Observe user list. If it changes, update input in checkout list
  observe({
    avail_users$df

    updateSelectInput(
      session = session, 
      inputId = "checkout_name",
      choices = avail_users$df$name,
    )
  })
  
  # reactivate storage for the app 
=======
# SERVER panel (BACK END LOGIC) ------

server <- function(input, output, session) {
  
  # Reactive storage
>>>>>>> Stashed changes
  checkout_list <- reactiveVal(checkout)
  cart_items    <- reactiveVal(empty_checkout())
  user_list     <- reactiveVal(users)
  
<<<<<<< Updated upstream
  # ----- ADD TO CART FUNCTION (background for the button) --------
=======
 
 # ADD TO CART ------
>>>>>>> Stashed changes
  observeEvent(input$addToCart, {
    
    # If there is no name or email, add feedback to the input fields that say you need to enter values
    shinyFeedback::feedbackDanger("checkout_name", input$checkout_name == "", "Please enter a user")
    shinyFeedback::feedbackDanger("checkout_item", input$checkout_item == "", "Please enter an item")
    shinyFeedback::feedbackDanger("checkout_dates", input$checkout_dates == "", "Please enter expected time of check out")
    
    req(input$checkout_name,
        input$checkout_item,
        input$checkout_dates)
    
<<<<<<< Updated upstream
    # WIP ---- Add feedback here about if requested quantity is more than available
    # Maybe add feedback about date needs to be in the future, and end date needs to be after start date
    
    # create a new row from the user input
    new_row <- data.frame(
      checkout_id = as.character((nrow(checkout_list())+1 + nrow(cart_items()))),
      checked_out_by = input$checkout_name,
      item = input$checkout_item,
      # quantity = as.numeric(input$checkout_quant),
      quantity = as.character(input$checkout_quant),
      location = input$checkout_location,
      checkout_start = as.character(input$checkout_dates[1]),
=======
    # Create new row from input ------
    new_row <- data.frame(
      checked_out_by    = as.character(input$checkout_name),
      item              = as.character(input$checkout_item),
      quantity          = as.numeric(input$checkout_quant),
      location          = as.character(input$checkout_location),
      checkout_start    = as.character(input$checkout_dates[1]),
>>>>>>> Stashed changes
      checked_out_until = as.character(input$checkout_dates[2]),
      stringsAsFactors = FALSE
    )
    
    # Force column order EXACTLY ------
    new_row <- new_row[, names(checkout_list())]
    
    cart_items(bind_rows(cart_items(), new_row))
  })
  
<<<<<<< Updated upstream
  # ----- Display cart --------
  output$cart <- renderDT({
    datatable(cart_items() %>% select(-checkout_id),
              rownames = FALSE,
              options = list(pageLength = 5))
  }, server = TRUE)
  
  # ------ Submit checkout  --------
=======
  
  # CART TABLE --------
  output$cart <- renderDT({
    datatable(cart_items(), rownames = FALSE, options = list(pageLength = 5))
  })
  
  
 
  # SUBMIT CHECKOUT -------
>>>>>>> Stashed changes
  observeEvent(input$submitCheckout, {
    
    req(nrow(cart_items()) > 0)
    
    current <- checkout_list()
    cart    <- cart_items()
    
    # Guarantee same structure
    cart <- cart[, names(current)]
    
    updated <- bind_rows(current, cart)
    
    checkout_list(updated)
<<<<<<< Updated upstream
    
    # Save new merged table to CSV
    # fwrite(updated, "checkout.csv")
    sheet_write(data = updated, ss = sheet_id, sheet = "checkout")
=======
    fwrite(updated, "checkout.csv")
>>>>>>> Stashed changes
    
    cart_items(empty_checkout())
    
<<<<<<< Updated upstream
    # Notification banner that your checkout has been approved
    showNotification("Items are now checked out to you!")
  })
  
  # ------ My items: overview of the items that you have checked out  --------
=======
    showNotification("Checkout submitted!")
  })
  
  

  # MY ITEMS ------ 
>>>>>>> Stashed changes
  output$myItems <- renderDT({
    req(input$checkout_name)
    
    datatable(
      checkout_list() %>%
        filter(checked_out_by == input$checkout_name) %>% select(-checkout_id),
      rownames = FALSE,
      options = list(pageLength = 5)
    )
  })
  
<<<<<<< Updated upstream
  # Return selected items feature -----
=======
  
  # RETURN ITEMS ------
>>>>>>> Stashed changes
  observeEvent(input$returnItems, {
    
    # If you have not selected any rows, make a pop up to prompt users to select rows
    if(length(input$myItems_rows_selected) == 0){
      showNotification("You have not selected any items. Please select rows to return.")
    }
    
    req(input$myItems_rows_selected)
    
    df <- checkout_list()
    
    # Get checkout_ids from filtered list
    return_ids <- checkout_list() %>%
      filter(checked_out_by == input$checkout_name)
    
    return_ids <- return_ids[input$myItems_rows_selected, "checkout_id"]
    
    # Get rows of items 
    df <- df %>% 
      filter(checkout_id %in% return_ids == F)
  
    checkout_list(df)
<<<<<<< Updated upstream
    sheet_write(data = df, ss = sheet_id, sheet = "checkout")
    # fwrite(df, "checkout.csv")
=======
    fwrite(df, "checkout.csv")
>>>>>>> Stashed changes
    
    showNotification("Items returned.")
  })
  
<<<<<<< Updated upstream
# ------ Inventory SERVER --------
=======
  
  # USER MANAGEMENT -------
  output$userTable <- renderDT({
    datatable(user_list(), rownames = FALSE, options = list(pageLength = 5))
  })
  
  observeEvent(input$addUser, {
    req(input$name, input$email)
    
    new_user <- data.frame(name = input$name, email = input$email)
    user_list(bind_rows(user_list(), new_user))
    
    write.csv(user_list(), "users.csv", row.names = FALSE)
    
    updateTextInput(session, "name", value = "")
    updateTextInput(session, "email", value = "")
  })
  
  observeEvent(input$removeUser, {
    req(input$userTable_rows_selected)
    
    updated <- user_list()[-input$userTable_rows_selected, ]
    user_list(updated)
    
    write.csv(updated, "users.csv", row.names = FALSE)
    
    showNotification("Users removed.")
  })
  
  

  # FULL INVENTORY MERGE VIEW --------
>>>>>>> Stashed changes
  inventory_view <- reactive({
    
    inv <- inventory
    chk <- checkout_list()
    
    if (nrow(chk) == 0) {
      inv$checked_out_by <- NA
      inv$checked_out_until <- NA
      return(inv)
    }
    
    checkout_summary <- chk %>%
      group_by(item) %>%
      summarise(
        checked_out_by =
          paste(unique(checked_out_by), collapse = ", "),
        checked_out_until =
          max(checked_out_until, na.rm = TRUE),
        .groups = "drop"
      )
    
    inv %>%
      left_join(checkout_summary, by = "item")
  })
  
  
  output$inventory <- renderDT({
    datatable(inventory, rownames = FALSE, options = list(pageLength = 10))
  })
  
  output$fullInventory <- renderDT({
<<<<<<< Updated upstream
    datatable(inventory_view(),
              rownames = FALSE,
              options = list(pageLength = 10))
  }, server = TRUE)
  
  # ----- USER (SERVER) -------
  # Create avail_users as a reactiveValues data frame that can be modified
  avail_users <- reactiveValues(df = users)
  
  # Display user list
  output$newUser <- DT::renderDT({
    datatable(avail_users$df, rownames= FALSE)
  })
  
  # Code to modify user list
  # When you ADD USERS....
  observeEvent(input$addName, {
    # If there is no name or email, add feedback to the input fields that say you need to enter values
    shinyFeedback::feedbackDanger("name", input$name == "", "Please enter a name")
    shinyFeedback::feedbackDanger("email", input$email == "", "Please enter an email")
    
    # Require name and email to proceed with following steps
    req(input$name, input$email)
    
    # Check for duplicates of name and email inputs
    shinyFeedback::feedbackDanger("email", tolower(input$email) %in% tolower(avail_users$df[["email"]]) == T, 
                                  "That email is already in the system. See list on the right.")
    
    shinyFeedback::feedbackDanger("name", tolower(input$name) %in% tolower(avail_users$df[["name"]]) == T, 
                                  "That name is already in the system. See list on the right.")
    
    # If there are no duplicate names or emails, proceed
    if(tolower(input$name) %in% tolower(avail_users$df[["name"]]) == F & 
       tolower(input$email) %in% tolower(avail_users$df[["email"]]) == F){
      
      # Clear text input boxes
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "email", value = "")
      
      # Create data frame from input
      row <- data.frame(name = input$name, email = input$email)
      
      # Rbind new input to existing user list
      new_table <- rbind(avail_users$df, row)
      
      # Update existing user list
      avail_users$df <- (new_table)
      
      # Save new user list
      # Test appending to sheet
      sheet_write(data = avail_users$df, ss = sheet_id, sheet = "users")
      # write.csv(avail_users$df, "users.csv", row.names=F)
      
      showNotification("User added.")
    }
  })
  
  # When you REMOVE USERS....
  # Make an object that is an interactive window asking users to click yes or no to proceed with an action 
  modal_confirm <- modalDialog(
    "Removing users also deletes all checkouts associated with those users. Are you sure you want to continue?",
    title = "Removing users",
    footer = tagList(
      actionButton("cancel", "Cancel"),
      actionButton("ok", "Remove", class = "btn btn-danger")
    )
  )
  
  # Now when the remove button is pressed...
  observeEvent(input$removeName, {
    # If you have not selected any rows, make a pop up to prompt users to select rows
    if(length(input$newUser_rows_selected) == 0){
      showNotification("You have not selected any users. Please select rows to remove")
    }
    # Require selected rows to proceed
    req(input$newUser_rows_selected)
    
    # Make pop up to ask users to confirm whether they actually want to remove users
    showModal(modal_confirm)
  })
  
  # When users select "Remove", 
  observeEvent(input$ok, {
    # Require that there are selected rows to proceed with below code
    req(input$newUser_rows_selected)
    
    # Filter out selected rows from data frame
    new_table <-  avail_users$df %>% 
      filter((row_number() %in% input$newUser_rows_selected) == F)
    
    # Update existing user list
    avail_users$df <- new_table
    
    # Save new user list
    sheet_write(data = avail_users$df, ss = sheet_id, sheet = "users")
    # write.csv( avail_users$df, "users.csv", row.names=F)
    
    ## WIP: Build in functionality to remove all user entries from check out list when name is deleted
    
    # Give notification that users were removed and remove dialog box
    showNotification("Users removed")
    removeModal()
  })
  
  # When users press cancel, remove dialog box
  observeEvent(input$cancel, {
    removeModal()
  })
}

# RUN APP ------
shinyApp(ui, server)

=======
    datatable(inventory_view(), rownames = FALSE, options = list(pageLength = 10))
  })
}


# RUN APP
shinyApp(ui, server)

# to publish new version of the app, run below in command:
# library(rsconnect)
# 
#   rsconnect::deployApp(
#         appDir = ".",
#        appFiles = c(
#              "app.R",
#              "users.csv",
#              "inventory.csv",
#             "checkout.csv"
#           )
#      )

>>>>>>> Stashed changes
