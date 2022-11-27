# Source:
# University of Connecticut Plant Database,
# http://hort.uconn.edu/plants,
# Mark H. Brand,
# Department of Plant Science and Landscape Architecture,
# Storrs, CT 06269-4067 USA.

library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(rvest)
library(stringr)

fix_list <- function(x) {
  list(x)
}

plant_description <- function(node) {
  headings <- node %>% html_elements("p font") %>% html_text() %>% str_squish() %>%
    str_remove_all(":") %>% str_replace_all("[[:punct:] ]{1,}", "_") %>% str_to_lower() %>%
    str_replace_all(c("habitat_and_form" = "habit_and_form",
                      "landscape_uses" = "landscape_use",
                      "autumn_foliage" = "fall_foliage"))
  lists <- node %>% html_elements("p + ul")
  list_entries <- lists %>% map(~html_elements(., "li") %>% html_text() %>% str_squish())
  variety <- node %>% html_elements("p:last-child") %>% html_text() %>% str_squish()

  append(list_entries, variety) %>%
    set_names(headings) %>%
    map(fix_list) %>%
    as_tibble()
}

get_plant_data <- function(session, url) {
  tmp <- session_jump_to(session, url)
  res <- tmp %>%
    html_element("#maincontainer")
  description <- res %>% html_element("#description") %>% plant_description()
  tibble(
    latin = res %>% html_element(".latinName") %>% html_text(),
    nickname = res %>% html_element("h2") %>% html_text(),
    family = res %>% html_element("h3") %>% html_text(),
    picture = res %>% html_elements("#photoStrip a img") %>% html_attr("src") %>% list(),
    desc = list(description)
  )

}

get_plant_data2 <- safely(get_plant_data)

sess <- session("https://plantdatabase.uconn.edu/list.php") %>%
  session_follow_link("List by Name")

initial_data <- fromJSON("https://plantdatabase.uconn.edu/api/_search.php?public=1") %>%
  as_tibble() %>%
  unnest("results") %>%
  mutate(url = paste0("https://plantdatabase.uconn.edu/detail.php?pid=", id)) %>%
  mutate(data = map(url, get_plant_data2, session = sess)) %>%
  mutate(data = map(data, "result"), error = map(data, "error"))

# initial_data %>% unnest(data) %>% unnest(description)

cleaned_data <- initial_data %>%
  unnest(data) %>%
  unnest(description) %>%
  filter(map_int(error, length) == 0)

cleaned_data %>%
  mutate(zone = map(habitat, paste, collapse = ". ") %>%
           map(., str_extract_all, pattern = "zone \\d{1,}", simplify = T) %>%
           map(., readr::parse_number),
         zone_min = map_dbl(zone, min),
         zone_max = map_dbl(zone, max),
         foliage = pmap_chr(., .f = function(summer_foliage, fall_foliage, ...) {
           paste(paste(summer_foliage, collapse = ". "),
                 paste(fall_foliage, collapse = ". "),
                 sep = ". ")
         }),
         foliage_length = map(foliage, str_extract_all, pattern = "(?:(leaves|leaflets|needles)?.*?)(?:up to )?[\\d\\.]{1,}[\"\']? ?(?:to [\\d\\.]{1,}[\"\']? )long")
  )

# usethis::use_data(DATASET, overwrite = TRUE)
