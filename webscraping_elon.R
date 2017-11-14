library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)

# created field for elon date
elon_date <- read_html("http://shitelonsays.com/transcript") %>% 
  html_nodes("div > p") %>%
  html_text() %>% 
  as_tibble() %>% 
  slice(5:n()) %>% 
  mutate(date_ = ymd(str_replace_all(value,pattern = "\\s.*",replacement = "")),
         rowId = row_number()) %>% 
  filter(!is.na(date_))

#extracting title from value column
elon_date$title <- regmatches(x = elon_date$value,
                              m = gregexpr(elon_date$value,pattern = "\\s.*"))

  
#getting links for all transcripts
elon_links <- read_html("http://shitelonsays.com/transcript") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  as_tibble() %>%
  rename("links" = value) %>% 
  slice(7:n()) %>% 
  unlist()


#creating a function to get all the text from the speech links
elon_trans_function <- function(links) {
read_html(links) %>% 
  html_nodes("div > p") %>%
  html_text() %>% 
  as_tibble() %>% 
  rename("links" = value) %>% 
  slice(8:n())
}

#getting all the text
elon_transcript <- lapply(elon_links, elon_trans_function)

#creating a variable that allows date and text to be joined together
elon_transcript <- map2(.x = elon_transcript,
                       .y = seq_len(length(elon_transcript)),
                       ~ mutate(.x,paraId = row_number(),rowId = .y))

#bind all the rows together
elon <- bind_rows(elon_transcript)

#joining the data to get dates for each speech
elon <- elon %>% 
  left_join(elon_date[,c("rowId","date_","title")],by = "rowId") %>% 
  rename("text"="links" )

#removing text between the brackets and the brackets as well
elon$text <- str_replace_all(string = elon$text,pattern = "\\[[^\\]]*\\]",replacement = "")
#removing carriage returns
elon$text <- str_replace_all(string = elon$text,pattern = "\n",replacement = "")
#removing page rendered text  
elon$text <- str_replace_all(string = elon$text,pattern = "[pP]age rend.*",replacement = "")
#taking away some unneccesary periods
elon$text <- str_replace_all(string = elon$text,pattern = "^.$",replacement = "NA")

elon <- elon %>%
  mutate(title = str_trim(string = title)) %>% 
  filter(text != "NA")
