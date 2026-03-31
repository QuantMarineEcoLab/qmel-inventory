### Archive code for making checkout unique ID show up
# I wanted a conditional panel to show up though, instead of having Item ID there all the time

## This code goes in checkout UI
uiOutput("select_unique_id")

## This code goes in checkout server
# If item has a unique ID, fill in unique ID column
output$select_unique_id <- renderUI({
  if("" %in% (inventory %>% filter(item == input$checkout_item) %>% pull(unique_id)) == F){
    selectInput("checkout_uniqueID", "Item ID", 
                choices = inventory %>% filter(item == input$checkout_item) %>% pull(unique_id))
  } else {
    NULL
  }
})

