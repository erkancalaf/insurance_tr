# URL ----------------------------------------------------------------------------------------
url <- "https://www.tsb.org.tr/resmi-istatistikler.aspx?pageID=909"

#1. libs --------------------------------------------------------------------------------------
library(tibble)
library(jsonlite)
library(xml2)
library(rvest)
library(stringr)
library(readxl)
library(dplyr)
library(httr)
library(magrittr)


# 2. funs ------------------------------------------------------------------------------------
# get links returns dataframe for links and cat texts ----------------------------------------
get_links <- function(url){
  web <- read_html(url)
  nodes <- html_nodes(web, ".istatistik .alt .kaydirmaAlani .kaydirak .element a")
  links <- html_attr(nodes, "href")
  text <- html_text(nodes) %>% str_replace_all(" ", "") %>% str_replace_all("\r\n\r\n", "")
  # text <- html_text(nodes)
  df <- tibble(link = links, text = text)
  # add_spaces between capital letters of text in df
  for (i in 1:nrow(df)) {
    df[i,2] <- gsub("([a-z])([A-Z])", "\\1 \\2", df[i,2 ])
  }
  # return what?
  return(df)
}

# get only .contains links -------------------------------------------------------------------
get_contains_links <- function(df, contains){
  df %<>%  mutate(flag = "N")
  for (i in 1:nrow(df)) {
    temp <- as.character(str_extract_all(df[i,1] %>% pull(1), paste0(contains)))
    if (temp == paste0(contains)) {
      df[i,3] <- "T"
    } else {
      df[i,3] <- "F"
    }
  }
  df %<>% filter(flag == "T") %>% select(link, text)
  return(df)
}

# save links to .xlsx files w/ source names --------------------------------------------------
saver_ <- function(df){
  dummy_url <- "https://www.tsb.org.tr" 
  if (!file.exists("data_download")) {
    dir.create("data_download")
  }
  
  for (i in 1:nrow(df)) {
    temp_df_url <- as.character(df[i,1])
    temp_url <- paste0(dummy_url, temp_df_url)
    destfile_temp = paste0(str_sub(string = temp_df_url, start = 25, end = nchar(temp_df_url)))
    download.file(url = temp_url, destfile = paste0("data_download\\", destfile_temp) , mode = "wb")  
  }
}

# category filter options ---------------------------------------------------------------------------
print_categories <- function(df){
  df <- df %>% select(2) %>% unique() %>% pull()
  return(df)
}

# enter url, enter category type, download all category .xlsx files ----------------------------------
download_data <- function(url){
  df <- get_links(url)
  df_contains <- get_contains_links(df = df, contains = ".xlsx")
  temp_cat <- print_categories(df)
  
  print(temp_cat)
  
  temp_cat_no <- as.numeric(readline("Please enter the row number for the selected category to download!"))
  if (is.numeric(temp_cat_no)) {
    if (temp_cat_no <= nchar(temp_cat)) {
      temp_cat_val <-  temp_cat[temp_cat_no]
    }
  }
  df_to_download <- df_contains %>% filter(text == temp_cat_val)
  df_return <- saver_(df = df_to_download)
  return(df_return)
}



# RUN ----------------------------------------------------------------------------------------
download_data(url = url)