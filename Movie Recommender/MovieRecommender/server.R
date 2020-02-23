source("plots.R")
source("recommender.R") # Constructing movie x user matrix function is provided by Philipp Spachtholz
                        # (https://github.com/pspachtholz/BookRecommender/blob/master/server.R）
source("cf_algorithm.R") # Collaborative filtering function is provided by Stefan Nikolic
                         # (https://github.com/smartcat-labs/collaboratory/blob/master/R/cf_algorithm.R)
source("similarity_measures.R") # Similarity measures function is provided by Stefan Nikolic
                                # (https://github.com/smartcat-labs/collaboratory/blob/master/R/similarity_measures.R)


function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot1 <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = 1, max.words= input$words,
                  random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Accent"))
  })
  
  output$plot2 <- renderPlotly({
    ggplotly({
    data <- subset(db_sub2, year >= input$year[1] & year <= input$year[2])
    ggplot(data, aes(vote_average, vote_count, color = vote_count)) +
      geom_point(shape = 16, size = 4, show.legend = FALSE, alpha = .4) +
      labs(x = "Rating", y = "Vote Count") +
      theme_minimal() +
      scale_color_gradient(low = "#5994f9", high = "#f0650e")
    })
  })
  
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 4 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 3,
                 div(style = "text-align:center", img(src = movies$poster_path[(i - 1) * num_movies + j], style = "max-height:150")),
                 div(style = "text-align:center", strong(movies$original_title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #C44F4F;", ratingInput(paste0("select_", movies$id[(i - 1) * num_movies + j]), label = "", dataStop = 5)))
             )
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      # add user's ratings as first column to rating matrix
      rmat <- cbind(user_ratings, ratingmat)
      
      # get the indices of which cells in the matrix should be predicted
      # predict all movies the current user has not yet rated
      items_to_predict <- which(rmat[, 1] == 0)
      prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
      
      # run the ubcf-alogrithm
      res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
      
      # sort, organize, and return the results
      user_results <- sort(res[, 1], decreasing = TRUE)[1:20]
      user_predicted_ids <- as.numeric(names(user_results))
      recom_results <- data.table(Rank = 1:20, 
                                  Movie_id = user_predicted_ids, 
                                  Title = movies$original_title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    }) # still busy
  }) # clicked on button
  
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 4
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 3, status = "primary", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            div(style = "text-align:center", img(src = movies$poster_path[which(movies$id == recom_result$Movie_id[(i - 1) * num_movies + j])], style = "max-height:150")),
            div(style= "text-align:center; font-size: 100%", 
                strong(movies$original_title[which(movies$id == recom_result$Movie_id[(i - 1) * num_movies + j])])
            )
        )        
      }))) # columns
    }) # rows
  })
}

  