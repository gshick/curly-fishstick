library(shiny)

# UI
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .title-panel {
      background-color: #009EDC;
      color: white;
      padding: 10px;
      text-align: left;
      font-size: 24px;
      display: flex;
      align-items: center;
      justify-content: space-between;
    }
    .center-dropdown {
      display: flex;
      justify-content: center;
      margin-bottom: 15px;
    }
    .image-frame {
      display: flex;
      justify-content: center;
      padding: 10px;
    }
    .image-shadow {
      box-shadow: 4px 4px 10px rgba(0, 0, 0, 0.3);
      border: 2px solid #ccc;
      padding: 5px;
    }
    .header-image {
      float: right;
      height: 40px;
    }
  "))),
  
  div(class="title-panel", 
      h2("Shiny App with Products & Decision Tool"),
      tags$img(src="header_icon.png", class="header-image")  # Right-aligned image
  ),
  
  tabsetPanel(
    tabPanel("Products",
             mainPanel(
               div(class="center-dropdown",
                   selectInput("selected_product", "Select a product:", 
                               choices = c("All Products", "BigQuery", "Product 2", "Product 3"))
               ),
               uiOutput("product_image")
             )
    ),
    tabPanel("Decision Tool",
             sidebarLayout(
               sidebarPanel(
                 selectInput("question1", "Select a category:", 
                             choices = c("", "Data Analytics", "Web Development", "Machine Learning")),
                 
                 conditionalPanel(
                   condition = "input.question1 !== ''",
                   selectInput("question2", "Select a related topic:", choices = NULL)
                 ),
                 
                 conditionalPanel(
                   condition = "input.question2 !== ''",
                   selectInput("question3", "Select a sub-topic:", choices = NULL)
                 )
               ),
               mainPanel(
                 uiOutput("decision_output"),
                 uiOutput("bigquery_image")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$product_image <- renderUI({
    req(input$selected_product)
    product_images <- list(
      "BigQuery" = div(class="image-frame",
                       tags$a(href="https://example.com/bigquery", target="_blank",
                              tags$img(src="BigQuery.png", class="image-shadow", width="100%"))),
      "Product 2" = tags$a(href="https://example.com/product2", target="_blank",
                           tags$img(src="product2.jpg", width="100%")),
      "Product 3" = tags$a(href="https://example.com/product3", target="_blank",
                           tags$img(src="product3.jpg", width="100%"))
    )
    
    if (input$selected_product == "All Products") {
      do.call(tagList, product_images)
    } else {
      product_images[[input$selected_product]]
    }
  })
  
  observeEvent(input$question1, {
    updateSelectInput(session, "question2", selected = "", choices = NULL)
    updateSelectInput(session, "question3", selected = "", choices = NULL)
  })
  
  observeEvent(input$question1, {
    updateSelectInput(session, "question2", choices = switch(input$question1,
                                                             "Data Analytics" = c("BigQuery", "SQL Optimization", "Data Visualization"),
                                                             "Web Development" = c("Shiny Apps", "CSS Frameworks", "JavaScript Libraries"),
                                                             "Machine Learning" = c("Neural Networks", "Random Forests", "Gradient Boosting")
    ))
  })
  
  observeEvent(input$question2, {
    updateSelectInput(session, "question3", choices = switch(input$question2,
                                                             "BigQuery" = c("Query Optimization", "Dataset Management", "Cloud Pricing"),
                                                             "SQL Optimization" = c("Indexing Strategies", "Query Execution Plans", "Stored Procedures"),
                                                             "Data Visualization" = c("ggplot2", "Shiny Dashboards", "Interactive Charts"),
                                                             "Shiny Apps" = c("UI Design", "Server Functions", "Dynamic Rendering"),
                                                             "CSS Frameworks" = c("Bootstrap", "Material UI", "Tailwind CSS"),
                                                             "JavaScript Libraries" = c("React", "Vue.js", "D3.js"),
                                                             "Neural Networks" = c("CNNs", "RNNs", "Backpropagation"),
                                                             "Random Forests" = c("Tree Splitting", "Feature Importance", "Hyperparameter Tuning"),
                                                             "Gradient Boosting" = c("XGBoost", "LightGBM", "CatBoost")
    ))
  })
  
  output$decision_output <- renderUI({
    req(input$question1, input$question2, input$question3)
    div(style="padding: 10px; border: 1px solid #ccc;", 
        paste("Category:", input$question1, "| Topic:", input$question2, "| Sub-Topic:", input$question3))
  })
  
  output$bigquery_image <- renderUI({
    req(input$question3)
    if (input$question2 == "BigQuery") {
      div(class="image-frame",
          tags$img(src="BigQuery.png", class="image-shadow", width="50%"))
    }
  })
}

# Run the app
shinyApp(ui, server)