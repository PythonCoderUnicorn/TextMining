
library(tidyverse)
library(rvest)

roster <- tibble(url = c(
  "https://github.com/AmeliaMN",
  "https://github.com/juliasilge",
  "https://github.com/evelinag",
  "https://github.com/massaraevi",
  "https://github.com/SatenikS",
  "https://github.com/spbail",
  "https://github.com/amberjrivera",
  "https://github.com/trallard",
  "https://github.com/lesley2958",
  "https://github.com/kjam",
  "https://github.com/PratheepaJ"
))
session = session("https://github.com")
commits = function(url, session){
  session %>% 
    session_jump_to(url) %>% 
    read_html() %>% 
    # use the SelectorGadget Chrome extension to find the html tags 
    html_nodes("h2.f4.text-normal.mb-2") %>% 
    html_text() %>% 
    purrr::pluck(2) %>% 
    readr::parse_number()
}
women_GitHub_commits <- roster %>% 
  mutate(
    commits= map_dbl(url, commits, session = session ),
    user = str_remove(url, "https://github.com/")
  )

ggplot(data= women_GitHub_commits, aes(x = user, y = commits)) +
  geom_col()
