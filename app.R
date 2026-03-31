# load in packages
library(shiny) # core Shiny framework to recognize the app 
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
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
    stringsAsFactors = FALSE
  )
}

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
              tabPanel(
                icon = icon("house"),
                "Welcome",
                
                tags$h3("Welcome to the QMEL Inventory and Checkout System!"),
                tags$br(),
                
                tags$h4("A quick overview of how the app works:"),
                tags$p("The idea is that this app will help us easily view and search what items we have in the lab. Hopefully this will help us manage and keep track of our tools in a more coordinated way than last-minute Slack messages when we can't find what we need."),
                tags$br(),
                
                tags$h4(HTML('<i class="fa-solid fa-box-open"></i> Inventory Overview Tab:')),
                tags$p("In this tab, you can view the currently-available inventory as well as see what individual users have checked out and when they expect to return their items. You can also view upcoming checkouts for users who have planned far in advance."),
                tags$br(),
                
                tags$h4(HTML('<i class="fa-solid fa-cart-shopping"></i> Checkout Tab:')),
                tags$p("In this tab, the intended use is that you can check out items one by one and add them to your cart, which also creates a 'packing list' for yourself as you plan your fieldwork or labwork. When you're done packing/adding to your cart, you press submit to finish checking out your items. You can also view what items you currently have checked out."),
                tags$br(),
                
                tags$h4(HTML('<i class="fa-solid fa-users"></i> Modify Users Tab:')),
                tags$p("In this tab, you can add and remove the list of available users for this app. Useful for when folks join the lab or graduate. Keep in mind that when you remove users, it will delete all of their equipment checkout records too."),
                tags$br(),
                
                tags$h4(HTML('<i class="fa-solid fa-box"></i> Full Inventory Tab:')),
                tags$p("This tab just has a table with the full inventory list."),
                tags$br()
              ),
              
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
              
              # Full Inventory Tab UI
              tabPanel(icon = icon("box"), "Full Inventory",
                       DTOutput("fullInventory")
              )
  )
)

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
  checkout_list <- reactiveVal(checkout)
  cart_items <- reactiveVal(empty_checkout())
  
  # ----- ADD TO CART FUNCTION (background for the button) --------
  observeEvent(input$addToCart, {
    
    # If there is no name or email, add feedback to the input fields that say you need to enter values
    shinyFeedback::feedbackDanger("checkout_name", input$checkout_name == "", "Please enter a user")
    shinyFeedback::feedbackDanger("checkout_item", input$checkout_item == "", "Please enter an item")
    shinyFeedback::feedbackDanger("checkout_dates", input$checkout_dates == "", "Please enter expected time of check out")
    
    req(input$checkout_name,
        input$checkout_item,
        input$checkout_dates)
    
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
      checked_out_until = as.character(input$checkout_dates[2]),
      stringsAsFactors = FALSE
    )
    
    # append to cart (reactiveVal)
    cart_items(bind_rows(cart_items(), new_row))
  })
  
  # ----- Display cart --------
  output$cart <- renderDT({
    datatable(cart_items() %>% select(-checkout_id),
              rownames = FALSE,
              options = list(pageLength = 5))
  }, server = TRUE)
  
  # ------ Submit checkout  --------
  observeEvent(input$submitCheckout, {
    
    req(nrow(cart_items()) > 0)
    
    # Merge cart into main checkout list
    updated <- bind_rows(checkout_list(), cart_items())
    
    checkout_list(updated)
    
    # Save new merged table to CSV
    # fwrite(updated, "checkout.csv")
    sheet_write(data = updated, ss = sheet_id, sheet = "checkout")
    
    # clear cart after your cart has been merged and the table has been "submitted" 
    cart_items(empty_checkout())
    
    # Notification banner that your checkout has been approved
    showNotification("Items are now checked out to you!")
  })
  
  # ------ My items: overview of the items that you have checked out  --------
  output$myItems <- renderDT({
    
    req(input$checkout_name)
    
    datatable(
      checkout_list() %>%
        filter(checked_out_by == input$checkout_name) %>% select(-checkout_id),
      rownames = FALSE,
      options = list(pageLength = 5)
    )
    
  }, server = TRUE)
  
  # Return selected items feature -----
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
    sheet_write(data = df, ss = sheet_id, sheet = "checkout")
    # fwrite(df, "checkout.csv")
    
    showNotification("Items returned.")
  })
  
# ------ Inventory SERVER --------
  inventory_view <- reactive({
    
    chk <- checkout_list()
    
    if (nrow(chk) == 0) {
      inventory %>%
        mutate(
          checked_out_by = NA,
          checked_out_until = NA
        )
    } else {
      
      summary_tbl <- chk %>%
        group_by(item) %>%
        summarise(
          checked_out_by = paste(unique(checked_out_by),
                                 collapse = ", "),
          checked_out_until = max(checked_out_until),
          .groups = "drop"
        )
      
      left_join(inventory, summary_tbl, by = "item")
    }
  })
  
  output$inventory <- renderDT({
    datatable(inventory,
              rownames = FALSE,
              options = list(pageLength = 10))
  }, server = TRUE)
  
  output$fullInventory <- renderDT({
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

