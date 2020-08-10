library(shiny)
library(igraph)
library(gplots)

setwd("~/Desktop/EnronCorporation_Email_NetworkAnalysis/RShiny app_Network dynamics")

# load data
load("networks.RData")
#(->4 dataset of type "list": "n_all", "n_bcc", "n_cc", "n_to")

####UI#####

shinyUI <- pageWithSidebar(
    headerPanel("Enron plots"),
    
    # choose which mails should be displayed
    sidebarPanel(
      selectInput("chosen_emails", h4("Select network"),
                  choices = c("all emails", "direct emails", "CC emails", "BCC emails")),
      h4("Change details"),
      
      # all emails
      sliderInput("degree", "Shown nodes by degree (in & out):",
                  min = 0, max = 66, value = c(0,66), step = 2),
      sliderInput("edge_weight", "Shown edge weights:",
                  min = 14, max = 500, value = c(14, 500)),
      helpText("All edge weights (sent emails between two persons) below 14 were removed to provide more clarity."),
      
      # SubmitButton - to avoid to much load on shiny server (glimmer.rstudio.com)
      submitButton("Apply Changes"),
      
      # Legend
      h4("Legend"),
      #img(src = "legend.png")
      img(src = "legend.png")
      
    ),
    
    # output
    mainPanel(
      plotOutput("network"),
      plotOutput("communities")
    )
    
)



#####Server side#######

shinyServer <- function(input, output) {
    #switch chosen data
    dataset <- reactive({
      switch(input$chosen_emails,
             "all emails" = n_all,
             "direct emails" = n_to,
             "CC emails" = n_cc,
             "BCC emails" = n_bcc)
    })
    
    #test & debug
    # dataset <- function() {
    #   switch(input$chosen_emails,
    #          "all emails" = n_all,
    #          "direct emails" = n_to,
    #          "CC emails" = n_cc,
    #          "BCC emails" = n_bcc)    
    # }
    
    #degrees <- reactive(function() {
    #  
    #})
    
    # generate chosen plot
    output$network <- reactivePlot(function() {
      data <- dataset()
      #print(class(data))        #DEBUGGING
      
      # color the edges (see igraph.R)
      edge_col <- colorpanel(length(table(E(data)$count)), low = "#2C7BB6", high = "#FFFFBF")  
      E(data)$color <- edge_col[factor(E(data)$count)]
      
      # all edge_weight above 500 cut down to 500
      #E(data)$count[E(data)$count > 500] <- 500
      
      # select only nodes with given degrees and edge weights
      data <- delete.vertices(data, V(data)[V(data)$degree < input$degree[1] | V(data)$degree > input$degree[2]])
      data <- delete.edges(data, E(data)[E(data)$count < input$edge_weight[1] | E(data)$count > input$edge_weight[2]])
      
      # let's make a picture
      par(bg = "#F5F5F5", mar = c(1,1,1,1), oma= c(1,1,1,1))
      #print(class(data))         #debug
      if (gorder(data) <= 0) {
        #plot an empty graph but with only a line of text show
        par(mar = c(0,0,0,0))
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x=0.5, y=0.5, "No valid cluster graph is shown here.")
      } else {
      plot(data, main = input$chosen_emails, layout = layout.fruchterman.reingold(data, params= list(niter = 1000, area = vcount(data)^4, weights = E(data)$count), repulserad= vcount(data)^3),
           vertex.label = NA, vertex.size = 3+3*log10(V(data)$degree), 
           edge.arrow.size = E(data)$count/150, edge.width = 1.5*log10(E(data)$count), edge.curved = T, edge.color = E(data)$color)
      }
    })
    
    output$communities <- reactivePlot(function() {
      data <- dataset()
      
      #E(data)$count[E(data)$count > 500] <- 500
      
      # select only nodes with given degrees and edge weights
      data <- delete.vertices(data, V(data)[V(data)$degree < input$degree[1] | V(data)$degree > input$degree[2]])
      data <- delete.edges(data, E(data)[E(data)$count < input$edge_weight[1] | E(data)$count > input$edge_weight[2]])
      
      # Community detection
      com <- edge.betweenness.community(data, E(data)$counts)
      
      par(bg = "#F5F5F5", mar = c(1,1,1,1), oma= c(1,1,1,1))
      #print("com:")      #debug
      #class(com)    #debug
      #print(com)    #debug
      #print(length(com)<=0)       #debug
      #print("data:")      #debug
      #class(data)       #debug
      #print(data)       #debug
      #print(gorder(data)<=0)        #debug
      if (length(com) <= 0 | gorder(data) <= 0) {
        #plot an empty graph but with only a line of text show
        par(mar = c(0,0,0,0))
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x=0.5, y=0.5, "No valid cluster graph is shown here.")
      } else {
        plot(com, data, main = "Communities")
      }
    })
}


#usage:
shinyApp(ui=shinyUI, server=shinyServer)

