library(shiny)
library(shinyjs)
library(plotly)

shinyUI(fluidPage(
  useShinyjs(),
  
  # Application title
  headerPanel("Book recommendation by readability"),
  
  # Sidebar with the upload panel
  sidebarLayout(
    sidebarPanel(
      selectInput("reader_level", label = p("Choose your English level"), 
                  choices = c("A1", "A2", "B1", "B2", "C1", "C2", "All"), 
                  selected = "A1"),
      uiOutput("catalogue_books"),
      actionButton("show_books", label = "Show me the stats!"),
      textOutput("sumtext")
      
    ),
    
    # Tabsets
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
        tabPanel("Books on this level",
                 br(),
                 p("These are all the books on this level. Best & easiest book in this category are in the upper right corner."),
                 plotlyOutput("booksPlot")
        ),
        tabPanel("Word ngrams",
                 br(),
                 p("Word ngrams give you a hint on what's the book about by providing you with the most frequent words and word clusters."),
                 uiOutput("nngram_type"),
                 plotOutput("nngram_wordcloud"),
                 hr(),
                 sliderInput("max_slider", label=p("Maximum number of ngrams:"),
                             min = 1, max = 40, value = 10),
                 uiOutput("freq_slider")
        ),
        tabPanel("About",
                 h3("About the project"),
                 p("Reading books in a foreign language is one of the most popular methods of learning. This site aims to help leaners find the books that fit their English level."),
                 p("Partially inspired by ",a(href="https://blog.vocapouch.com/do-20-pages-of-a-book-gives-you-90-of-its-words-795a405afe70","this article,"), 
                   " I decided to check the complexity of some of the books provided by the Project Gutenberg website, 
                   since it serves the books that have passed into the public domain and can be freely used by all users."), 
                 p("The books have been pre-processed to get a list of 480 books from top 100 PG authors. 
                   Using the treetagger software and several R packages, I calculated the ", a(href="https://en.wikipedia.org/wiki/Flesch%E2%80%93Kincaid_readability_tests", "Flesch-Kincaid readability scores, "),
                   "as they can be ",a(href="https://linguapress.com/teachers/flesch-kincaid.htm","translated into EFL proficiency levels pretty easily."), 
                   "However, all the graphs are rendered on the go. It is possible for the user to feed the graphs their own data, as long as the file contains the mandatory information."),
                 h3("Limitations"),
                 p("Readability scores themselves are not a perfect book difficulty delimiter.
                   Since they focus on the easily computable values (number of paragraphs, words and syllables, and their respective ratios)
                   they are obviously unreliable when used as the sole source of information in comparing e.g. the difficulty of books from different literary periods.
                   They miss out on several variables, such as relative vocabulary frequency, topic complexity, writing style, and more."),
                 p("For instance, according to the readability scores only, the plays by William Shakespeare can be read by A1 (beginner) level readers.
                   Indeed, the language of Shakespeare is pretty transparent -- if you're from the 16th century, that is.")
        )
      )
    )
  )
))
