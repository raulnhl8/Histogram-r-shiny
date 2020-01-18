library(shiny)
library(readr)


generate_curve <- function(text)
{
  if (length(text) > 0)
  {
    words <- strsplit(text, " ")[[1]]
    
    tabela <- data.frame(words)
    colnames(tabela) <- "word"
    
    if (dim(tabela)[1] != 0)
    {
      tabela$count <- 1
      duplicados <- duplicated(tabela)
      
      for (i in 1:length(duplicados))
      {
        if (duplicados[i] == TRUE)
        {
          row <- tabela[i,]
          print(row)
          for (j in 1:(i-1))
          {
            if (tabela[j,]$word == row$word)
            {
              tabela[j,]$count = tabela[j,]$count + 1
              break
            }
          }
        }
      }
      
      tabela <- tabela[order(-tabela$count),]
      
      tabela <- tabela[!duplicated(tabela$word), ]
      
      barplot(height = tabela$count, names.arg = tabela$word, las=2, cex.names=1, width=1, xlim=c(0, 30),
              ylab="Count per word"
      )
    }
  }
}


ui <- fluidPage(
  titlePanel("Histogram"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("select_var", h4("Select box"), 
                  choices = list("Text" = 1, "File" = 2), selected = 1),
      textInput("text_var", h4("Text input"), value = ""),
      fileInput("file_var", h4("File input"))
    ),
    mainPanel(
      textOutput("num_chars"),
      textOutput("num_words"),
      textOutput("texto"),
      plotOutput("curve")
    )
  )
)
  
server <- function(input, output) {
  
  output$num_chars <- renderText({
    if (input$select_var == 1)
    {
      paste("Number of characters: ", nchar(input$text_var))
    }
    else
    {
      if (!is.null(input$file_var))
      {
        paste("Number of characters: ", nchar(read_file(toString(input$file_var[[4]]))))
      }
    }
  })
  
  output$num_words <- renderText({
    if (input$select_var == 1)
    {
      paste("Number of words: ", sapply(strsplit(input$text_var, " "), length))
    }
    else
    {
      #print(nchar(input$file_var))
      if (!is.null(input$file_var))
      {
        paste("Number of words: ", sapply(strsplit(read_file(toString(input$file_var[[4]])), " "), length))
      }
    }
  })

  output$curve <- renderPlot({
    if (input$select_var == 1)
    {
      generate_curve(input$text_var)
    }
    else
    {
      if (!is.null(input$file_var))
      {
        generate_curve(read_file(toString(input$file_var[[4]])))
      }
      
    }
  })
    
  output$texto <- renderText({
    if (input$select_var == 1)
    {
      input$text_var
    }
    else
    {
      if (!is.null(input$file_var))
      {
        read_file(toString(input$file_var[[4]]))
      }
      
    }
  })
  
}

shinyApp(ui = ui, server = server)