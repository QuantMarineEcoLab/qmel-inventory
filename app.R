library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(data.table)
library(lubridate)
library(DT)
library()
# library(shinycssloaders)
# library(shinyjs)
# library(shinyWidgets)

# Load input tables
users <- fread("users.csv")
checkout <- fread("checkout.csv")
inventory <- fread("inventory.csv")

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  
  # Set main title
  titlePanel(title = "", windowTitle = "QMEL Inventory App"),
  tags$h3("QMEL Inventory App"),
  tags$br(),
  # This creates the tabs at the top of the app
      tabsetPanel(id = "mainTabset",
      # WELCOME (UI) ----
        tabPanel(icon = icon("house"), "Welcome",
                 tags$h3("Welcome to the QMEL Inventory App!"),
                 tags$br(),
                 tags$h4("A quick overview of how the app works:"),
                 tags$p("The idea is that this app will help us easily view and search what items we have in the lab. Hopefully this will help us manage and keep track of our tools in a more coordinated way than last-minute Slack messages when we can't find what we need."),
                 tags$br(),
                 tags$h4(HTML('<i class = "fa-solid fa-box-open"></i> Inventory Overview Tab:')),
                 tags$p("In this tab, you can view the currently-available inventory as well as see what individual users have checked out and when they expect to return their items. You can also view upcoming checkouts for users who have planned far in advance."),
                 tags$br(),
                 tags$h4(HTML('<i class = "fa-solid fa-cart-shopping"></i> Checkout Tab:')),
                 tags$p("In this tab, the intended use is that you can check out items one by one and add them to your cart, which also creates a 'packing list' for yourself as you plan your fieldwork or labwork. When you're done packing/adding to your cart, you press submit to finish checking out your items. You can also view what items you currently have checked out."),
                 tags$br(),
                 tags$h4(HTML('<i class = "fa-solid fa-users"></i> Modify Users Tab:')),
                 tags$p("In this tab, you can add and remove the list of available users for this app. 
                         Useful for when folks join the lab or graduate. Keep in mind that when you remove 
                         users, it will delete all of their equipment checkout records too."),
                 tags$br(),
                 tags$h4(HTML('<i class = "fa-solid fa-box"></i> Full Inventory Tab:')),
                 tags$p("This tab just has a table with the full inventory list."),
                 tags$br()),

      # INVENTORY (SPREAD OUT) (UI) -----
      # Previously called "Browse Items"?
      tabPanel(icon = icon("box-open"), "Inventory Overview",
               tags$h3("WORK IN PROGRESS...."),
               tags$h3("Available inventory"),
               fluidRow(column(12, DT::DTOutput("inventory")))),
      
      # View available inventory (sort ability?)
      # Should we be able to add inventory in the app, or should that just be a CSV/Excel entry thing?
      # Show category, item, quantity, unique ID, and location
      
      # Category, item, quant, id, location, expected return date for current items out
      
      # CHECK OUT (UI) ----
      tabPanel(icon = icon("shopping-cart"), "Checkout/Return (WIP)", 
               tags$br(),
               sidebarLayout(
                 sidebarPanel(
                   verticalLayout(
                     # Check out: need to fill out Name, Item, Quantity, ID(?), and date range
                     tags$h3("Checkout items"),
                     # Fill out name
                     fluidRow(column(12, selectInput("checkout_name", 
                                                     "Select user from dropdown",
                                                     choices = users$name,
                                                     selected = NULL,
                                                     multiple = F))),
                     # Fill out item
                     fluidRow(column(12, selectInput("checkout_item",
                                                     "Select item from dropdown",
                                                     choices = inventory$item,
                                                     selected = NULL,
                                                     multiple = F))),
                     
                     # ADD CONDITIONAL PANEL....IF SELECTED ITEM HAS A UNIQUE ID, 
                     # ALLOW UNIQUE ID FIELD TO POP UP ---------
                     
                     # Fill out quantity
                     fluidRow(column(12, numericInput("checkout_quant",
                                                      "Fill in item quantity", 1,
                                                      min = 1))),
                     
                     fluidRow(column(12, textInput("checkout_location",
                                                      "Fill in intended location"))),
                     
                     # Fill out unique ID? WIP ----
                     
                     # Fill out date range
                     fluidRow(column(12, dateRangeInput("checkout_return", 
                                                        "Select timeframe of item checkout"))),
                     fluidRow(column(12, actionButton("addToCart", "Add to cart",
                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                   )
                 ),
                 mainPanel(
                   # Show table of items already in users' possesion
                   tags$h3("Items that you already have checked out"),
                   tags$h5("Select rows in table and click 'Return' button to return items"),
                   tags$br(),
                   fluidRow(column(12, DT::DTOutput("myItems"))),
                   fluidRow(column(12, actionButton("returnItems", "Return items",
                                                    style="color: #fff; background-color: #C23E3E; border-color: #A81D1D"))),
                   # tags$br(),
                   tags$h3("Items in your cart"),
                   tags$h5("You MUST press 'Submit checkout' for your items to save."),
                   fluidRow(column(12, DT::DTOutput("cart"))),
                   # How to incorporate removal/check in? ------
                   fluidRow(column(12, actionButton("addItems", "Submit checkout",
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                 )
               )
               ),
      
        # USERS (UI)-------
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
      
      # FULL INVENTORY (JUST A LIST) ------
      tabPanel(icon = icon("box"), "Full Inventory",
               tags$h3("Full inventory list"),
               fluidRow(column(12, DT::DTOutput("fullInventory"))))
      )
)

server <- function(input, output, session) {
  
# CHECKOUT (SERVER)
  checkout_list <- reactiveValues(df = checkout)
  
  # And create a reactive table???
  
# Display user list
  output$myItems <- DT::renderDT({
    datatable(checkout_list$df %>% filter(name == input$checkout_name) %>%
                select(-name, -email, -category), rownames= FALSE,
              width = "70%")
  })
  
# USER (SERVER) -------
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
    write.csv( avail_users$df, "users.csv", row.names=F)
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
    write.csv( avail_users$df, "users.csv", row.names=F)
    
    ## WIP: Build in functionality to remove all user entries from check out list when name is deleted
    
    # Give notification that users were removed and remove dialog box
    showNotification("Users removed")
    removeModal()
    })
  
  # When users press cancel, remove dialog box
  observeEvent(input$cancel, {
    removeModal()
    })
  
# INVENTORY OVERVIEW (SERVER) ---------
  # Create avail_inventory as a reactiveValues data frame that can be modified
  avail_inventory <- reactiveValues(df = inventory)
  
  # Display inventory list
  output$inventory <- DT::renderDT({
    datatable(avail_inventory$df, rownames= FALSE)
  })
  
# FULL INVENTORY (SERVER) ---------
  # Display inventory list
  output$fullInventory <- DT::renderDT({
    datatable(inventory, rownames= FALSE)
  })
}

# Deploy app
shinyApp(ui, server)

