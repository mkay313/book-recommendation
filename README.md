# book-recommendation

### How to run
1. install the required packages
2. import into R
3. run app

### About the project
Reading books in a foreign language is one of the most popular methods of learning. This site aims to help leaners find the books that fit their English level. Partially inspired by [this article](https://blog.vocapouch.com/do-20-pages-of-a-book-gives-you-90-of-its-words-795a405afe70), I decided to check the complexity of some of the books provided by the Project Gutenberg website, since it serves the books that have passed into the public domain and can be freely used by all users. 

The books have been pre-processed to get a list of 480 books from top 100 PG authors. Using the treetagger software and several R packages, I calculated the [Flesch-Kincaid readability scores](https://en.wikipedia.org/wiki/Flesch%E2%80%93Kincaid_readability_tests), as they can be [translated into EFL proficiency levels pretty easily](https://linguapress.com/teachers/flesch-kincaid.htm). However, all the graphs are rendered on the go. It is possible for the user to feed the graphs their own data, as long as the file contains the mandatory information. A file with the data for the mentioned 480 books is available in the repo.

### Limitations
Readability scores themselves are not a perfect book difficulty delimiter. Since they focus on the easily computable values (number of paragraphs, words and syllables, and their respective ratios) they are obviously unreliable when used as the sole source of information in comparing e.g. the difficulty of books from different literary periods. They miss out on several variables, such as relative vocabulary frequency, topic complexity, writing style, and more."),

For instance, according to the readability scores only, the plays by William Shakespeare can be read by A1 (beginner) level readers. Indeed, the language of Shakespeare is pretty transparent -- if you're from the 16th century, that is.