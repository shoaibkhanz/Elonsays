#16/11/2017
#This code scrapes data from http://shitelonsays.com/transcript.
#The output contains only what Elon said and words of other people have been deleted. 

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
elon$text <- str_replace_all(string = elon$text,pattern = "\\(.*\\)$",replacement = "")
#removing carriage returns
elon$text <- str_replace_all(string = elon$text,pattern = "\n",replacement = "")
#removing page rendered text  
elon$text <- str_replace_all(string = elon$text,pattern = "[pP]age rend.*",replacement = "")
#taking away some unneccesary periods
elon$text <- str_replace_all(string = elon$text,pattern = "^.$",replacement = "N/A")


elon_80 <- elon %>% 
  filter(rowId == 80) %>%
  mutate(text = gsub(x=text,pattern = "\\([0-9]{1,2}:[0-9]{1,2}\\)",
                     replacement = "N/A",perl = TRUE)) %>%
  mutate(text = gsub(x=text,pattern = "N/A",
                     replacement = "",perl = TRUE))


elon_82 <- elon %>% 
  filter(rowId == 82) %>%
  slice(5:n()) %>% 
  mutate(text = str_replace_all(string = text,pattern = "[0-9]\\:[0-9]{1,2}\\:[0-9]{1,2}\\s[^EM\\:].*",replacement = "N/A")) %>%
  mutate(text = str_replace_all(string = text,
                                pattern = "[0-9]\\:[0-9]{1,2}\\:[0-9]{1,2}\\sEM\\:"
                                ,replacement = "")) %>%
  mutate(text = str_replace_all(string = text,pattern = "^\\*\\*.*",replacement = "N/A")) %>% 
  filter(text != "N/A")


elon_78 <- elon %>% 
  filter(rowId == 78) %>%
  mutate(text = gsub(x=text,pattern = "Q\\:.*",
                     replacement = "N/A",perl = TRUE)) %>%
  mutate(text = gsub(x=text,pattern = "Musk:",
                     replacement = "",perl = TRUE)) %>%
  filter(text != " N/A",text != "N/A")
  
  
elon_77 <- elon %>% 
  filter(rowId == 77) %>%
  slice(2:n()) %>% 
  mutate(text = gsub(x=text,pattern = "[0-9]{1,2}\\:[0-9]{1,2}\\s(?!Elon\\sMusk).*",
                     replacement = "N/A",perl = TRUE)) %>%
  mutate(text = gsub(x=text,pattern = "[0-9]{1,2}\\:[0-9]{1,2}\\sElon\\sMusk:",
                     replacement = "",perl = TRUE)) 

elon_77 <- elon_77[elon_77$text!="N/A",]


elon_74 <- elon %>% 
  filter(rowId == 74) %>%
  slice(6:n()) %>% 
  mutate(text = gsub(x=text,pattern = "[0-9]{1,2}\\:[0-9]{1,2}\\s(?!Elon\\sMusk).*",
                     replacement = "N/A",perl = TRUE)) %>% 
  mutate(text = str_replace_all(string = text,
                                pattern = "[0-9]{1,2}\\:[0-9]{1,2}\\sElon\\sMusk:"
                                ,replacement = "")) %>% 
  filter(text != "N/A")

elon_73 <- elon %>% 
  filter(rowId == 73) %>%
  slice(7:n()) %>% 
  mutate(text = gsub(x=text,pattern = "[0-9]{1,2}\\:[0-9]{1,2}\\s(?!Musk).*",
                     replacement = "N/A",perl = TRUE)) %>% 
  mutate(text = str_replace_all(string = text,
                                pattern = "[0-9]{1,2}\\:[0-9]{1,2}\\sMusk:"
                                ,replacement = "")) %>% 
  filter(text != "N/A")


#special not in function
#https://stackoverflow.com/questions/34444295/how-to-specify-does-not-contain-in-dplyr-filter-in-r
`%notin%` = function(x,y) !(x %in% y) 

elon_23 <- elon %>% 
  filter(rowId == 23) %>%
  filter(paraId %notin% (c(1,4:6)))

elon <- elon %>% 
  filter(rowId %notin% c(82,80,78,77,74,73,23)) %>% 
  bind_rows(elon_82,elon_80,elon_78,elon_77,elon_74,elon_73,elon_23) %>% 
  mutate(text = gsub(x = text,pattern = "%\\w\\d{4}",replacement = ""))


#removing all Quarters call where no text is avaiable
#except Tesla Q1 2011 Earnings Call

elon <- elon %>% 
  mutate(Q_call =  str_detect(title,pattern = "Tesla Q.*")) %>% 
  filter((Q_call == FALSE | rowId ==71)) 


#removing non-ELON speeches
#NTSB Commercial Space Launch Accident - SpaceShipTwo talk
#Commercial Crew's Path to Flight
#Interview at Code Conference 2016
#A Conversation With Elon Musk - Khan Academy(Transcript incomplete)
# Singapore Satellite Industry Forum 2013 - Opening Keynote by Gwynne Shotwell

elon <- elon %>%
  mutate(title = str_trim(string = title)) %>% 
  filter(text != "N/A") %>% 
  filter(rowId != 82,rowId != 76,rowId != 75,rowId != 28,rowId != 24,rowId != 22) %>% 
  filter(!(rowId == 26 & paraId == 1))


blank <- which(elon$text == "")
elon <- elon[-blank,]
