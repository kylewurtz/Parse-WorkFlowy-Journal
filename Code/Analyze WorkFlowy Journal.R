# Setup -------------------------------------------------------------------

## load packages
require(tidyverse)
require(magrittr)
require(xml2)
require(stringr)
require(lubridate)

## read in data in opml format
day = read_xml("Dropbox/R/WorkFlowy Journal/day.txt")
day = xml_children(day)[2]

day = xml_child(day, 1)

# get string for date
date = xml_attr(day, "text")

# get all days in the year
days_in_year = as.character(seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "days"))
days_in_year = gsub(pattern = "-", replacement = "", x = days_in_year)
num_days = length(days_in_year)

# set up days data frame
days = data_frame(
  day = days_in_year,
  poms_tot = rep(-1, num_days),
  poms_tot_w = rep(-1, num_days),
  poms_tot_g = rep(-1, num_days),
  poms_tot_h = rep(-1, num_days),
  poms_tot_m = rep(-1, num_days)
)

# Pomodoros ---------------------------------------------------------------

# get pomodoros
poms = xml_children(day)[1]

## pomodoros total
poms_tot = xml_child(poms, 1)
getTotPom = function(t, n) {
  t = xml_child(t, n)
  t %<>% xml_attr("text") %>% str_split(":")
  t = t[[1]][2] %>% str_trim() %>% as.integer()
  return(t)
}
poms_tot_w = getTotPom(poms_tot, 1) # work
poms_tot_g = getTotPom(poms_tot, 2) # growth
poms_tot_h = getTotPom(poms_tot, 3) # health
poms_tot_m = getTotPom(poms_tot, 4) # misc

## pomodoro records
pom_records = data_frame(
  day = rep(date, 100),
  id = 1:100,
  type = rep(NA, 100),
  proj = rep(NA, 100),
  proj_count = rep(NA, 100),
  desc = rep(NA, 100)
)

for (r in 2:length(xml_children(poms))) {
  n = r - 1
  
  # get individual record
  pom_rec = xml_child(poms, r) %>%
    xml_child() %>%
    xml_attr("text") %>%
    str_split(":", 2) %>%
    .[[1]] %>%
    str_trim()
  
  # get pomodoro type
  pom_rec_type = pom_rec[1]
  
  # get pomodoro description
  pom_rec_desc = pom_rec[2]
  
  # get project type (if applicable)
  proj_check = pom_rec_desc %>%
    str_split("@") %>%
    .[[1]] %>%
    length()
  if (proj_check == 2) {
    pom_rec_proj = pom_rec_desc %>%
      str_split("@") %>%
      .[[1]] %>%
      .[2] %>%
      str_split(" ") %>%
      .[[1]] %>%
      .[1]
    
    if (grepl("\\[", pom_rec_desc)) {
      pom_rec_proj_count = pom_rec_desc %>%
        str_split("\\[", 2) %>%
        .[[1]] %>%
        .[2] %>%
        str_split("\\]", 2) %>%
        .[[1]] %>%
        .[1] %>%
        as.integer()
      
      pom_rec_desc %<>%
        str_split("]") %>%
        .[[1]] %>%
        .[2] %>%
        str_trim()
    } else {
      pom_rec_proj_count = NA
    }
    
    proj_bool = pom_rec_desc %>%
      str_split(" ") %>%
      .[[1]] %>%
      grepl("@", .)
    pom_rec_desc %<>%
      str_split(" ") %>%
      .[[1]] %>%
      subset(!proj_bool) %>%
      paste(collapse = " ")
    
  } else {
    pom_rec_proj = NA
    pom_rec_proj_count = NA
  }
  
  # add results to pom_records
  pom_records$type[n] = pom_rec_type
  pom_records$proj[n] = pom_rec_proj
  pom_records$proj_count[n] = pom_rec_proj_count
  pom_records$desc[n] = pom_rec_desc
}

# filter out extra rows
pom_records %<>% filter(!is.na(type))

# get date fields
pom_records %<>% 
  separate(day, c("dow", "month", "dom")) %>% 
  mutate(
    month = str_pad(month, 2, pad = "0"),
    dom = str_pad(dom, 2, pad = "0"),
    date = paste0(2016, month, dom)
  ) %>% 
  select(date, id, dow:dom, type:desc)

## add totals to days data frame
pom_records %>% 
  count(type)

# Interruptions -----------------------------------------------------------

# get interruptions
inters = xml_children(day)[2]

inters %>% xml_children()
