library(shiny)
library(shinyjs)
library(ggplot2)
library(ggthemes)
library(plotly)
library(wordcloud)

shinyServer(function(input, output) {
  
  #table tab
  
  datasetInput <- reactive({
    GetUserLevel(ReadData(), input$reader.level)
  })
  
  output$book.table <- renderDataTable({
    datasetInput()
  })
  
  #book selector
  output$catalogue.books <- renderUI({
    listed.books <- datasetInput()$title
    selectInput(inputId = ".book",
                label = p("These books correspond to your level. Choose one:"),
                choices = listed.books)
  })
  
  GetBookReactive <- reactive({
    book.title <- input$.book
    return(GetBook(book.title))
  })
  
  #plot tab  
  ParseBookReactive <- reactive({
    book.title <- input$.book
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        book.details <- GetBook(book.title)
        result <- ParseBook(book.details[[1]], 
                             book.details[[2]]$author, 
                             book.details[[2]]$book.id, 
                             book.details[[2]]$title)
        return(result)
      })
    })
  })
  
  observeEvent(input$show.books, {
    show("coverage.slider")
  })
  
  #serving the slider on the server side to be able to change max depending on the book length
  output$coverage.slider <- renderUI({
    max.pages <- ParseBookReactive()[[2]]$pages
    sliderInput(
      inputId = "coverage",
      label = "Which pages of the book you'd like to read?",
      min = 1,
      max = max.pages,
      value = c(1,20),
      step = 5
    )
  })
  
  CalculateWordsReactive <- reactive({
    req(input$coverage)
    CalculateWords(ParseBookReactive()[[1]], input$coverage[1], input$coverage[2])
  })
  
  #plot
  output$uniques.plot <- renderPlotly({
    ggplotly(
      ggplot(data = CalculateWordsReactive(),
             aes(x = Pages, y = Uniques)) +
        xlab("# of pages") +
        ylab("# of new unique words") +
        geom_point() +
        geom_smooth(method = "auto") +
        theme_few()
    )
  })
  
  #text statistics under the plot
  output$text.stats <- renderText({
    stats.frame <- ParseBookReactive()[[2]]
    sprintf(
      "%s by %s consists of %s words over %s pages. 
      There are %s different words, of which %s appear only once.",
      stats.frame$title, 
      stats.frame$author,
      stats.frame$words, 
      stats.frame$pages,
      stats.frame$different.words, 
      stats.frame$unique.words
    )
  })
  
  #all books in this category
  output$books.plot <- renderPlotly({
    ggplotly(
      ggplot(data = datasetInput(),
             aes(x= flesch.value, 
                 y = average.goodreads.rating)) +
        labs(title = "A steadily descending trend line suggests the book gets easier with time, 
             as there are fewer and fewer new words introduced",
             x = "Flesch value",
             y = "Average Goodreads rating") +
        geom_point(aes(color = title)) +  
        scale_x_discrete(breaks = round(seq(0, 100, by = 20), 1)) +
        scale_y_discrete(breaks = round(seq(0, 5, by = 0.5), 1)) +
        theme_few()
    ) %>%
    layout(showlegend = FALSE)
  })
  
  #nngrams
  
  output$nngram.type <- renderUI({
    selectInput("nngram.type", 
                label=p("Choose your ngram type:"), 
                choices = c("singlegram", "bigram", "trigram"),
                selected = NULL)
  })
  
  nngrams <- reactive({
    req(input$nngram.type)
    if (input$nngram.type == "singlegram") {
      return(Create1Gram(GetBookReactive()[[1]]))
    }
    else if (input$nngram.type == "bigram") {
      return(Create2Gram(GetBookReactive()[[1]]))
    }
    else if (input$nngram.type == "trigram") {
      return(Create3Gram(GetBookReactive()[[1]]))
    }
  })
  
  output$freq.slider <- renderUI({
    min.freq <- nngrams()$n[1]
    sliderInput(
      inputId = "freq.slider",
      label = p("Minimum frequency of ngram:"),
      min = 1,
      max = min.freq,
      value = c(1, 20),
      step = 5
    )
  })
  
  MakeRepeatableWordcloud <- repeatable(wordcloud)
  
  output$nngram.wordcloud <- renderPlot({
    v <- nngrams()
    MakeRepeatableWordcloud(v$nngram, v$n, scale=c(4,0.5),
                  min.freq = input$freq.slider,
                  max.words = input$max.slider,
                  colors=brewer.pal(8, "Dark2"))
  })
  
})