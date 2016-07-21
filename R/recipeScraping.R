# RECIPE SCRAPING

#'********************
#' BREAKFAST RECIPES *
#'********************

library(tm)
library(stringr)
library(httr)
library(rvest)

breakfast <- read_html("http://allrecipes.com/recipes/155/everyday-cooking/vegetarian/breakfast-and-brunch/?internalSource=hub%20nav&referringId=87&referringContentType=recipe%20hub&linkName=hub%20nav%20daughter&clickId=hub%20nav%202")
breakfast_titles <- breakfast %>% html_nodes("h3") %>% html_text(trim = TRUE)
breakfast_titles <- breakfast_titles[-c(2, 4)]

# Remove empty ""
breakfast_titles[breakfast_titles == ""] = NA
breakfast_titles <- breakfast_titles[!is.na(breakfast_titles)]
breakfast_titles

# Retrieve all recipe links (and some extra)
breakfast_links <- breakfast %>% html_nodes("article > a") %>% html_attr("href")
recipe_links <- paste0("http://allrecipes.com",unique(breakfast_links))
head(recipe_links)

recipe_number <- readline("Recipe Number:") %>% as.numeric()
recipe <- read_html(recipe_links[recipe_number])
recipe %>% html_nodes(".checklist.dropdownwrapper > li > label > span") %>% html_text()

steps = c()
for (i in 1:10) {
  steps = c(steps, recipe %>%
              html_nodes(xpath = paste("/html/body/div[1]/div[2]/div/div[3]/section/section[3]/div/div[1]/ol[1]/li[", i, "]/span/text()", sep = "") ) %>%
              html_text()) 
  print(steps)
}

#'*****************
#' LUNCH RECIPES  *
#'*****************

dinner <- read_html("http://allrecipes.com/recipes/87/everyday-cooking/vegetarian/?internalSource=hub%20nav&referringId=155&referringContentType=recipe%20hub&referringPosition=4&linkName=hub%20nav%20exposed&clickId=hub%20nav%204")
dinner_titles <- dinner %>% html_nodes("h3") %>% html_text(trim = TRUE)
dinner_titles <- dinner_titles[-c(6, 16)]
dinner_titles
# Remove empty ""
dinner_titles[dinner_titles == ""] = NA
dinner_titles <- dinner_titles[!is.na(dinner_titles)]
dinner_titles

# Retrieve all recipe links (and some extra)
dinner_links <- dinner %>% html_nodes("article > a") %>% html_attr("href")
recipe_links2 <- paste0("http://allrecipes.com",unique(dinner_links))
head(recipe_links2)

recipe_number
recipe
recipe %>% html_nodes(".checklist.dropdownwrapper > li > label > span") %>% html_text()

steps = c()
for (i in 1:10) {
  steps = c(steps, recipe %>%
              html_nodes(xpath = paste("/html/body/div[1]/div[2]/div/div[3]/section/section[3]/div/div[1]/ol[1]/li[", i, "]/span/text()", sep = "") ) %>%
              html_text()) 
  print(steps)
}