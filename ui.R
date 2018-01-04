library(shiny)
library(shinyjs)
library(plotly)

shinyUI(fluidPage(
  useShinyjs(),
  
  # Application title
  titlePanel("Project Gutenberg books analyzer"),
  p("How many different words appear in a book? Can you measure the book difficulty by calculating how many unique words appear in it?
    Search for a volume from the Project Gutenberg to display graphs and statistical info."),
  
  # Sidebar with the upload panel
  sidebarLayout(
    sidebarPanel(
      selectInput("reader_level", label = p("Choose your English level"), 
                  choices = c("A1", "A2", "B1", "B2", "C1", "C2"), 
                  selected = "A1"),
      uiOutput("catalogue_books"),
      actionButton("show_books", label = "Show me the stats!"),
      textOutput("sumtext")
      # hr(),
      # p("Or find your book by the author/title (may not include all book information):"),
      # textInput("book_title", label = p("Book title"), value="Christmas Carol"),
      # textInput("book_author", label = p("Author"), value="Dickens"),
      # actionButton("find_book", label = "Search!")

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Book info",
          dataTableOutput("bookTable")
        ),
        tabPanel("New words plot",
          plotlyOutput("distPlot"),
          textOutput("textStats"),
          uiOutput("coverage_slider")
        ),
        tabPanel("Word ngrams",
                 p("Word ngrams give you a hint on what's the book about by providing you with the most frequent words and word clusters."),
                 uiOutput("nngram_type"),
                 plotOutput("nngram_wordcloud"),
                 hr(),
                 sliderInput("max_slider", label=p("Maximum number of ngrams:"),
                             min = 1, max = 40, value = 10),
                 uiOutput("freq_slider")
        ),
        tabPanel("About")
      )
    )
  )
  ))
