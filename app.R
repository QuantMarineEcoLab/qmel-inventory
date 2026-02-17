# load in packages
library(shiny) # core Shiny freamework to recognize the app 
library(dplyr)
library(data.table)
library(DT)
library(shinyFeedback) # adds feedback messages for user inputs (ex: checkout completed! prompt) 


# Helper function to create empty checkout table --------

empty_checkout <- function() {
  data.frame(
    checked_out_by = character(),
    item = character(),
    quantity = numeric(),
    location = character(),
    checkout_start = character(),
    checked_out_until = character(),
    stringsAsFactors = FALSE
  )
}

# Checkout CSV... if it doesn't exist, create an empty checkout table that can be used in the panel 
safe_read_checkout <- function(file) {
  
  if (!file.exists(file)) {
    df <- empty_checkout()
    fwrite(df, file)
    return(df)
  }
  
  df <- tryCatch(
    fread(file),
    error = function(e) empty_checkout()
  )
  # If checkout.csv exists but is empty, reset to empty

  if (nrow(df) == 0) return(empty_checkout())
 
   # Force correct types to prevent bind_rows errors
  df %>%
    mutate(
      checked_out_by = as.character(checked_out_by),
      item = as.character(item),
      quantity = as.numeric(quantity),
      location = as.character(location),
      checkout_start = as.character(checkout_start),   # Keep dates as character for now to avoid type mismatches... this was creating many errors
      checked_out_until = as.character(checked_out_until)
    )
}

# load in datasets

users <- fread("users.csv")
inventory <- fread("inventory.csv")
checkout <- safe_read_checkout("checkout.csv")


# UI (user interface... what the people see!) --------

ui <- fluidPage(
  
  useShinyFeedback(),
    # App title
  titlePanel("QMEL Inventory App"),
  # Creating tabbed layout
  tabsetPanel(id = "mainTabset",
              
              # WELCOME TAB UI
              tabPanel(
                icon = icon("house"),
                "Welcome",
                
                tags$h3("Welcome to the QMEL Inventory App!"),
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
              
              # Inventory Overview Tab UI
              tabPanel("Inventory Overview",
                       DTOutput("inventory")
              ),
              
              # Checkout/Return Tab UI
              tabPanel(
                icon = icon("shopping-cart"),
                "Checkout / Return",
                       
                       sidebarLayout(
                         
                         sidebarPanel(
                           selectInput("checkout_name","User", choices = users$name), # select user
                           selectInput("checkout_item","Item", choices = inventory$item), # select item
                           numericInput("checkout_quant","Quantity",1,min=1), # select quantity of items 
                           textInput("checkout_location","Location"),
                           dateRangeInput("checkout_dates","Checkout timeframe"),
                           actionButton("addToCart","Add to cart") # ADD to cart button
                         ),
                         
                         # table of items currently checked out by selected user 
                         mainPanel(
                           h4("Items currently checked out"),
                           DTOutput("myItems"),
                           actionButton("returnItems","Return selected"), # return items button 
                           
                           br(), br(),
                           
                           # temporary cart table
                           h4("Cart"),
                           DTOutput("cart"),
                           
                           #submit checkout button 
                           actionButton("submitCheckout","Submit checkout")
                         )
                       )
              ),
              
              # Full Inventory Tab UI
              tabPanel("Full Inventory",
                       DTOutput("fullInventory")
              )
  )
)


# SERVER -------


server <- function(input, output, session) {
  
  # reactivate storage for the app 
  checkout_list <- reactiveVal(checkout)
  cart_items <- reactiveVal(empty_checkout())
  
  # ADD TO CART FUNCTION (background for the button) --------
  observeEvent(input$addToCart, {
    
    req(input$checkout_name,
        input$checkout_item,
        input$checkout_dates)
    
    # create a new row from the user input
    new_row <- data.frame(
      checked_out_by = input$checkout_name,
      item = input$checkout_item,
      quantity = as.numeric(input$checkout_quant),
      location = input$checkout_location,
      checkout_start = as.character(input$checkout_dates[1]),
      checked_out_until = as.character(input$checkout_dates[2]),
      stringsAsFactors = FALSE
    )
    
    # append to cart (reactiveVal)
    cart_items(bind_rows(cart_items(), new_row))
  })
  
  # Display cart --------
  output$cart <- renderDT({
    datatable(cart_items(),
              rownames = FALSE,
              options = list(pageLength = 5))
  }, server = TRUE)
  
  # Submit checkout  --------
  observeEvent(input$submitCheckout, {
    
    req(nrow(cart_items()) > 0)
    
    # Merge cart into main checkout list
    updated <- bind_rows(checkout_list(), cart_items())
    
    checkout_list(updated)
    
    # Save new merged table to CSV
    fwrite(updated, "checkout.csv")
    
    # clear cart after your cart has been merged and the table has beem "submitted" 
    cart_items(empty_checkout())
    
    # Notification banner that your checkout has been approved
    showNotification("Checkout submitted!")
  })
  
  # My items: overview of the items that you have checked out  --------
  output$myItems <- renderDT({
    
    req(input$checkout_name)
    
    datatable(
      checkout_list() %>%
        filter(checked_out_by == input$checkout_name),
      rownames = FALSE,
      options = list(pageLength = 5)
    )
    
  }, server = TRUE)
  
  # Return selected items feature
  observeEvent(input$returnItems, {
    
    req(input$myItems_rows_selected)
    
    df <- checkout_list()
    df <- df[-input$myItems_rows_selected, ]
    
    checkout_list(df)
    fwrite(df, "checkout.csv")
  })
  
  # Merging the intertnory page with the updated information from the checkout panel --------
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
}


# RUN APP ------

shinyApp(ui, server)


# # to publish new version of the app, run below in command:
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

