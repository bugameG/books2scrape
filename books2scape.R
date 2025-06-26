# LOADING PACKAGES
library(tidyverse)
library(rvest)

catalog <- tibble()

for(page_number in seq(from=1,to=50,by=1)){

# MAIN PAGE    
link <- paste("https://books.toscrape.com/catalogue/page-",page_number,".html",sep="")

books_html <- read_html(link)

# Book title
title <- books_html |> 
  html_nodes(".product_pod a") |> 
  html_attr("title") |> 
  na.omit() |> 
  unlist()

# Price
price_in_pounds <- books_html |> 
  html_nodes(".price_color") |> 
  html_text2() |> 
  parse_number()


# Scrappable book links (SECONDARY LINK TO OBTAIN UPC CODE ,GENRE AND BOOK COUNT)
book_links1 <- books_html |>
  html_nodes(".product_pod a") |>
  html_attr("href") |> unique()
book_links2 <- paste("https://books.toscrape.com/catalogue/",book_links1,sep = "")


# Book genre
get_genre <- function(book_link){
  book_page <- read_html(book_link)
  book_genre <- book_page |> html_nodes("li~ li+ li a") |>
html_text() |> nth(n=1) |> unlist()
return(book_genre)
}

genre <- sapply(book_links2, FUN = get_genre)

# Book upc code
get_upc <- function(book_link){
   book_page <- read_html(book_link)
   book_upc <- book_page |> html_nodes("tr:nth-child(1) td") |>
   html_text()
   return(book_upc)
}

upc_code <- sapply(book_links2, FUN = get_upc)


# Available number of books
get_count <- function(book_link){
  book_page <- read_html(book_link)
  book_count <- book_page |> html_nodes(".product_main .availability") |>
    html_text() |> str_extract("\\d+") |> as.double()
  return(book_count)
}

books_available <- sapply(book_links2, FUN = get_count)


catalog <- rbind(catalog, tibble(upc_code, genre, title, books_available, price_in_pounds))

}

# WRITING BOOK DATA TO CSV
# write_csv(catalog, "book_catalog.csv")
