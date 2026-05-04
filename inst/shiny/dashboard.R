
library(shiny)
library(ggplot2)
library(dplyr)

# Load your data (adjust paths as needed)
pert_results <- read.csv("~/Downloads/NEW/perturb_seq_results.csv")
iso_results <- data.frame(
  Gene = c("PTPRC", "CD3G", "CD8A", "FOXP3"),
  N_Isoforms = c(4, 3, 2, 3),
  Switch_Score = c(0.45, 0.38, 0.42, 0.31),
  Is_Switching = c(TRUE, TRUE, TRUE, TRUE)
)

# UI
ui <- fluidPage(
  titlePanel("CRISPRperturb Dashboard"),
  tabsetPanel(
    tabPanel("Knockdown Efficiency",
      plotOutput("knock_plot"),
      tableOutput("knock_table")
    ),
    tabPanel("Isoform Switching",
      plotOutput("iso_plot"),
      tableOutput("iso_table")
    )
  )
)

# Server
server <- function(input, output) {
  output$knock_plot <- renderPlot({
    top10 <- head(pert_results[order(pert_results$Log2_FC), ], 10)
    ggplot(top10, aes(x = reorder(Target, -Log2_FC), y = -Log2_FC)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 Knockdowns", x = "Target", y = "-Log2 FC")
  })
  
  output$knock_table <- renderTable({ head(pert_results, 10) })
  
  output$iso_plot <- renderPlot({
    ggplot(iso_results, aes(x = Gene, y = Switch_Score, fill = Is_Switching)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Isoform Switching Scores")
  })
  
  output$iso_table <- renderTable({ iso_results })
}

shinyApp(ui, server)

