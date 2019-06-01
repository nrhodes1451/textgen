library(gutenbergr)
library(tidyverse)

id <- gutenberg_authors %>% filter(author=="Dickens, Charles") %>%
  .$gutenberg_author_id
# Save Dickens works list to CSV
gutenberg_metadata %>% filter(gutenberg_author_id==id) %>% write_csv("works_full.csv")

# Manually filtered works to remove duplicates/translations/compilations etc.
works <- read_csv("works_subset.csv")
corpus <- gutenberg_download(works) %>%
  left_join(works) %>%
  select(-gutenberg_id)

books = corpus$title %>% unique
books <- books[-2]

text <- books %>% lapply(function(book){
  print(book)
  df = corpus %>% filter(title == book) %>%
    filter(text!="") %>%
    filter(substr(tolower(text), 1, 7)!="chapter") %>%
    filter(substr(tolower(text), 1, 8)!="\"chapter") %>%
    mutate(text = trimws(text))
  text = df$text %>% paste(collapse = " ") %>%
    str_replace_all("Mr\\. ", "Mr\\.") %>%
    str_replace_all("Mrs\\. ", "Mrs\\.") %>%
    str_replace_all("Miss\\. ", "Miss\\.")
  text <- text %>%
    str_split("\\. ") %>%
    data.frame %>% as_tibble
  names(text) = "text"
  text$text <- text$text %>%
    str_replace_all("Mrs\\.", "Mrs\\. ") %>%
    str_replace_all("Mr\\.", "Mr\\. ") %>%
    str_replace_all("Miss\\.", "Miss\\. ")
  text$text <- as.character(text$text)
  text$title = book
  return(text)
})
text <- do.call(rbind, text)

# Save books as csv
text %>% write_csv("dickens.csv")
