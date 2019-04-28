# Load packages -----------------------------------------------------
library(tidyverse)
library(ggplot2)
library(shiny)

# Load data ---------------------------------------------------------
load("/cloud/project/data/student-new.rdata")


# Define UI ---------------------------------------------------------
ui <- fluidPage(
  
  # App title
  titlePanel("Student Achievement"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Influencing Variables",
                  choices = c("failures", "Fjob", "goout", "higher", "Mjob", "sex","studytime"), 
                  selected = "failures"),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "Comparison Group",
                  choices = c("first_gen"), 
                  selected = "first_gen"),
      
      radioButtons(inputId="choice", label="What would you like to see?", 
                   choices=c("None","First Generation","Non-First Generation")),
      
      plotOutput(outputId = "maingraph")
    ),
    
    # Output: Show graph
    mainPanel(
      plotOutput(outputId = "graph1"), 
      textOutput(outputId = "influential"),
      plotOutput(outputId = "graph2")
    )
  )
)

# Define server function --------------------------------------------
server <- function(input, output) {
  
  # Create plot object the plotOutput function is expecting
  output$maingraph <- renderPlot({
    ggplot (students, mapping = aes(x = first_gen, y = avg_score)) +
      geom_boxplot() + 
      labs(title = "Average Scores", subtitle = "First and Non-First Generation Students", 
           x = "First Generation", y = "Average Score")  
  })
  
  #Top Graph
  output$graph1 <- renderPlot({
    if(input$y == "failures"){
      ggplot(data = students) +
        geom_bar(mapping = aes(x = first_gen, fill = as.character(failures)), position = "fill") +
        labs(title = "Proportion of Failures",
             x = "First Generation", y = "Proportion", fill = "Number of Failures")
    }
    else if(input$y == "goout"){
      ggplot(data = students) +
        geom_bar(mapping = aes(x = first_gen, fill = as.character(goout)), position = "fill") +
        labs(title = "Proportion of Number of Times Going Out",
             x = "First Generation", y = "Proportion", fill = "Number of Times Go Out")
    }
    else if(input$y == "studytime"){
      ggplot(data = students) +
        geom_bar(mapping = aes(x = first_gen, fill = as.character(studytime)), position = "fill") +
        labs(title = "Proportion of Study Time",
             x = "First Generation", y = "Proportion", fill = "Study Time")
    }
    else if(input$y == "Mjob"){
      ggplot(data = students) +
        geom_bar(mapping = aes(x = first_gen, fill = as.character(Mjob)), position = "fill") +
        labs(title = "Proportion of Mother's Job",
             x = "First Generation", y = "Proportion", fill = "Mother's Job")
    }
    else if(input$y == "Fjob"){
      ggplot(data = students) +
        geom_bar(mapping = aes(x = first_gen, fill = as.character(Fjob)), position = "fill") +
        labs(title = "Proportion of Father's Job",
             x = "First Generation", y = "Proportion", fill = "Father's Job")
    }
    else if(input$y == "higher"){
      ggplot(data = students) +
        geom_bar(mapping = aes(x = first_gen, fill = as.character(higher)), position = "fill") +
        labs(title = "Proportion of Wanting Higher Education",
             x = "First Generation", y = "Proportion", fill = "Higher Education")
    }
    else if(input$y == "sex"){
      ggplot(data = students) +
        geom_bar(mapping = aes(x = first_gen, fill = as.character(sex)), position = "fill") +
        labs(title = "Proportion of Genders",
             x = "First Generation", y = "Proportion", fill = "Gender")
    }
    
  })
  
  # Text
  output$influential <- renderText({
    if(input$choice == "First Generation"){
      if(input$y == "failures" || input$y == "sex" || input$y == "studytime" || input$y == "goout"){
        ""
      }
      else{
        "This variable is not influential in this generation."
      }
    }
    else if(input$choice == "Non-First Generation"){
      if(input$y == "failures" || input$y == "Mjob" || input$y == "Fjob" || input$y == "higher"){
        ""
      }
      else{
        "This variable is not influential in this generation."
      }
    }
  })
  
  # Bottom Graph
  # First Generation ONLY
  output$graph2 <- renderPlot({
    if(input$choice == "First Generation"){
      if(input$y == "goout"){
        ggplot(data = first, mapping = aes(x = as.character(goout), y = avg_score)) +
          geom_boxplot() + labs(title = "Effect of Going Out on Average Score",
                                subtitle = "First Generation",
                                x = "Number of Times Go Out", y = "Average Score")
      }
      else if(input$y == "sex"){
        ggplot(data = first, mapping = aes(x = sex, y = avg_score)) +
          geom_boxplot() + labs(title = "Effect of Gender on Average Score",
                                subtitle = "First Generation",
                                x = "Gender", y = "Average Score")
      }
      else if(input$y == "studytime"){
        ggplot(data = first, mapping = aes(x = as.character(studytime), y = avg_score)) +
          geom_boxplot() + labs(title = "Effect of Study Time on Average Score",
                                subtitle = "First Generation",
                                x = "Study Time", y = "Average Score")
      }
      else if(input$y == "failures"){
        ggplot(data = first, mapping = aes(x = as.character(failures), y = avg_score)) +
          geom_boxplot() + labs(title = "Effect of Failures on Average Score",
                                subtitle = "First Generation",
                                x = "Failures", y = "Average Score")
      }
    }
    
    # Non-First Generation ONLY
    else if(input$choice == "Non-First Generation"){
      if(input$y == "Mjob"){
        ggplot(data = second, mapping = aes(x = Mjob, y = avg_score)) +
          geom_boxplot() + labs(title = "Effect of Mother's Job on Average Score",
                                subtitle = "Non-First Generation",
                                x = "Mother's Job", y = "Average Score")
      }
      else if(input$y == "Fjob"){
        ggplot(data = second, mapping = aes(x = Fjob, y = avg_score)) +
          geom_boxplot() + labs(title = "Effect of Father's Job on Average Score",
                                subtitle = "Non-First Generation",
                                x = "Father's Job", y = "Average Score")
      }
      else if(input$y == "higher"){
        ggplot(data = second, mapping = aes(x = higher, y = avg_score)) +
          geom_boxplot() + labs(title = "Effect of Wanting Higher Education on Average Score",
                                subtitle = "Non-First Generation",
                                x = "Want Higher Education", y = "Average Score")
      }
      else if(input$y == "failures"){
        ggplot(data = second, mapping = aes(x = as.character(failures), y = avg_score)) +
          geom_boxplot() + labs(title = "Effect of Failures on Average Score",
                                subtitle = "Non-First Generation",
                                x = "Failures", y = "Average Score")
      }
    }
  })
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)