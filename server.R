library(shiny)
library(shinyjs)
library(ggplot2)
library(ggthemes)
library(plotly)
library(wordcloud)

shinyServer(function(input, output) {
  
  #table tab
  
  datasetInput <- reactive({
    get_level(read_file(), input$reader_level)
  })
  
  output$bookTable <- renderDataTable({
    datasetInput()
  })
  
  #book selector
  output$catalogue_books <- renderUI({
    listed_books <- datasetInput()$title
    selectInput(inputId = "chosenBook",
                label = p("These books correspond to your level. Choose one:"),
                choices = listed_books)
  })
  
  reactive_seek_and_get_book <- reactive({
    bookTitle <- input$chosenBook
    return(seek_and_get_book(bookTitle))
  })
  
  #plot tab  
  reactive_parse_book <- reactive({
    req(input$show_books)
    bookTitle <- input$chosenBook
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        book_details <- seek_and_get_book(bookTitle)
        result <- parse_book(book_details[[1]], 
                             book_details[[2]]$author, 
                             book_details[[2]]$book_id, 
                             book_details[[2]]$title)
        return(result)
      })
    })
  })
  
  observeEvent(input$show_books, {
    show("coverage_slider")
  })
  
  #serving the slider on the server side to be able to change max depending on the book length
  output$coverage_slider <- renderUI({
    max_pages <- reactive_parse_book()[[2]]$pages
    sliderInput(
      inputId = "coverage",
      label = "Which pages of the book you'd like to read?",
      min = 1,
      max = max_pages,
      value = c(1,20),
      step = 5
    )
  })
  
  reactive_calculate_words <- reactive({
    req(input$coverage)
    calculate_words(reactive_parse_book()[[1]], input$coverage[1], input$coverage[2])
  })
  
  #plot
  output$distPlot <- renderPlotly({
    ggplotly(
      ggplot(data = reactive_calculate_words(),
             aes(x = Pages, y = Uniques)) +
        xlab("# of pages") +
        ylab("# of new unique words") +
        geom_point() +
        geom_smooth(method = "auto") +
        theme_few()
    )
  })
  
  #text statistics under the plot
  output$textStats <- renderText({
    stats_frame <- reactive_parse_book()[[2]]
    sprintf(
      "%s by %s consists of %s words over %s pages. There are %s different words, of which %s appear only once.",
      stats_frame$title, stats_frame$author,
      stats_frame$words, stats_frame$pages,
      stats_frame$different_words, stats_frame$unique_words
    )
  })
  
  #all books in this category
  output$booksPlot <- renderPlotly({
    ggplotly(
      ggplot(data=datasetInput(),
             aes(x=flesch_value, y=average_goodreads_rating)) +
        xlab("Flesch value") +
        ylab("Average Goodreads rating") +
        geom_point(aes(color=title)) +  
        scale_x_discrete(breaks = round(seq(0, 100, by = 20),1)) +
        scale_y_discrete(breaks = round(seq(0, 5, by = 0.5),1)) +
        theme_few()
    ) %>%
      layout(showlegend = FALSE)
  })
  
  #nngrams
  
  output$nngram_type <- renderUI({
    selectInput("nngramType", 
                label=p("Choose your ngram type:"), 
                choices = c("singlegram", "bigram", "trigram"),
                selected = NULL)
  })
  
  nngrams <- reactive({
    req(input$nngramType)
    if(input$nngramType == "singlegram") {
      return(create_1gram_df(reactive_seek_and_get_book()[[1]]))
    }
    else if(input$nngramType == "bigram") {
      return(create_2gram_df(reactive_seek_and_get_book()[[1]]))
    }
    else if(input$nngramType == "trigram") {
      return(create_3gram_df(reactive_seek_and_get_book()[[1]]))
    }
  })
  
  output$freq_slider <- renderUI({
    min_freq <- nngrams()$n[1]
    sliderInput(
      inputId = "freq_slider",
      label = p("Minimum frequency of ngram:"),
      min = 1,
      max = min_freq,
      value = c(1,20),
      step = 5
    )
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$nngram_wordcloud <- renderPlot({
    v <- nngrams()
    wordcloud_rep(v$nngram, v$n, scale=c(4,0.5),
                  min.freq = input$freq_slider,
                  max.words = input$max_slider,
                  colors=brewer.pal(8, "Dark2"))
  })
  
})