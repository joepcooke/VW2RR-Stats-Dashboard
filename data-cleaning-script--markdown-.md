---
title: "VW2RR Dataset Cleaning Script"
author: "Joe Cooke"
date: "2026-02-27"
output: 
  html_document: 
    keep_md: true
---


``` r
#moved this over to a markdown file from a regular r script, I know I could put all these in separate code chunks and format this as a proper report but it's not something I can be bothered to go back and change; will be a learning lesson for future instead. note to self though: the green +c lets you add new chunks

#ANOTHER NOTE TO SELF: keep markdown documents can be a helpful setting to tick, to allow people to read stuff easily without downloading the full repo

#read file

library("readxl")
library("writexl")
library("tidyverse")
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.2.0     ✔ readr     2.2.0
## ✔ forcats   1.0.1     ✔ stringr   1.6.0
## ✔ ggplot2   4.0.2     ✔ tibble    3.3.0
## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
## ✔ purrr     1.2.1     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library("dplyr")

vw2rr_data <- read_excel("data_cols_format.xlsx")

#rename columns

vw2rr_data %>%
  rename(dex_no = "Dex No.")
```

```
## # A tibble: 681 × 16
##    dex_no Pokemon       HP   Atk   Def   SpA   SpD   Spe Total `Ability 1`
##    <chr>  <chr>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>      
##  1 #001   Bulbasaur     45    49    49    65    65    45   318 Solar Power
##  2 #002   Ivysaur       60    62    63    80    80    60   405 Solar Power
##  3 #003   Venusaur      80    87    85   103   100    80   535 Solar Power
##  4 #004   Charmander    39    52    43    60    50    65   309 Defiant    
##  5 #005   Charmeleon    58    64    58    80    65    80   405 Defiant    
##  6 #006   Charizard     75   101    75   109    75   100   535 Defiant    
##  7 #007   Squirtle      44    48    65    50    64    43   314 Shell Armor
##  8 #008   Wartortle     59    63    80    65    80    58   405 Shell Armor
##  9 #009   Blastoise     79    83   100    90   105    78   535 Shell Armor
## 10 #010   Caterpie      45    30    35    20    20    45   195 Shield Dust
## # ℹ 671 more rows
## # ℹ 6 more variables: `Ability 2` <chr>, `Hidden Ability` <chr>,
## #   `Type 1` <chr>, `Type 2` <chr>, `Evolution(s)` <chr>,
## #   `Additional Notes` <chr>
```

``` r
new_col_names <- c(dex_no = "Dex No.", name = "Pokemon",
                   hp = "HP", atk = "Atk", def = "Def", sp_atk = "SpA", sp_def = "SpD", 
                   speed = "Spe", bst = "Total", ability_1 = "Ability 1", ability_2 = "Ability 2",
                   hidden_ability = "Hidden Ability", type_1 = "Type 1", type_2 = "Type 2",
                   evolutions = "Evolution(s)", notes =  "Additional Notes")

vw2rr_data <- vw2rr_data %>%
rename(all_of(new_col_names))


#define total number of rows
n <- nrow(vw2rr_data)


#mutate dex_no to remove #

vw2rr_data_2 <- vw2rr_data %>%
  mutate(dex_no_conc = gsub("#", "", dex_no))


#remove trailing zeros from "00X" numbers, i.e. detect if first two characters are "00"

vw2rr_data_3 <- vw2rr_data_2 %>%
  mutate(dex_no_conc = ifelse(substr(dex_no_conc, 1, 2) == "00", gsub("00", "", dex_no_conc), dex_no_conc))


#remove trailing zeros from "0XX" numbers, i.e. detect if first character is a "0"

vw2rr_data_4 <- vw2rr_data_3 %>%
  mutate(dex_no_conc = ifelse(substr(dex_no_conc, 1, 1) == "0", substr(dex_no_conc, 2, nchar(dex_no_conc)), dex_no_conc))

vw2rr_data_5 <- vw2rr_data_4 %>%
  mutate(dex_no_int = as.integer(dex_no_conc), temp_no = seq.int(n))

#replace na values with max value of dex_no_int up until that point
#i.e. filter rows (pokemon) up until the ith value and calculate pokemon with highest dex no
#put more simply, find most recent non na value. probably much cleaner ways to do this but
#this is what i did

#in fact i actually thought of a cleaner way, can also filter up until that ith value and then filter out all non NA values, and choose last value from that dataset

for (i in 1:n){
  
  temp_data <- vw2rr_data_5 %>%
    filter(temp_no <= i)
  
  vw2rr_data_5$dex_no_int[i] = max(temp_data$dex_no_int, na.rm = TRUE)
  
}

#filter to find forms that are duplicated

dupe_data <- vw2rr_data_5 %>%
  group_by(dex_no_int) %>%
  filter(n() > 1)

#View(dupe_data)

#ok so want to replace dex_no values with #"0" + as.str(dex_no_int) or "00" + as.str(dex_no_int)

# "00" if number of digits of pokedex number is 1, and "0" if number of digits is 2. and "" if digits is 3 i.e. no change

vw2rr_data_6 <- vw2rr_data_5 %>%
  mutate(dex_no_new = paste("#", strrep("0", 3 - nchar(as.character(dex_no_int))), dex_no_int, sep = ""))

#for duplicate forms/pre-evos concatenate all evolution information together
#this compiles all relevant evolution information into the correct row, so now can just
#filter out the ones that are otherwise NA

vw2rr_data_7 <- vw2rr_data_6 %>%
  group_by(dex_no_new) %>%
  mutate(evolutions_new = paste(evolutions, collapse = ", "))


#some alternate forms have no dex number for some reason but we want to keep these

#these forms are fully evolved so the duplicate forms we want to keep have 'NA' in their
#evolutions column (as in, no info about evolutions) whereas the duplicates that arose
#from pre-evos having multiple evos DON'T HAVE NAs anymore due to the concatenation of
#groups

#so the ones we keep either have valid dex numbers, or for those that don't they're fully
#evolved and hence have NAs in their evo information from previous cleanup
#then readjust these NAs so they're consistent

vw2rr_data_8 <- vw2rr_data_7 %>%
  filter(!is.na(dex_no) | str_detect(evolutions_new, "NA")) %>%
  mutate(evolutions_new = ifelse(str_detect(evolutions_new, "NA"), NA, evolutions_new))


#FOR THE FINAL CLEANED DATASET FOR POWER BI, select the relevant columns and rename
#specifically we don't want dex_no and evolutions anymore, we want dex_no_new and evolutions_new

vw2rr_data_cleaned <- vw2rr_data_8 %>%
  select(temp_no, dex_no_new, name, hp, atk, def, sp_atk, sp_def, speed, bst, ability_1, ability_2, hidden_ability, type_1, type_2, evolutions_new, notes) %>%
  rename(dex_no = "dex_no_new", evolutions = "evolutions_new", id = "temp_no")

#view original data for reference
View(vw2rr_data)

#view edited data
View(vw2rr_data_cleaned)

dupe_data_2 <- vw2rr_data_8 %>%
  group_by(dex_no_int) %>%
  filter(n() > 1)


#write new data to files (commented out because this has already been done)

#write.csv(vw2rr_data_cleaned,"vw2_redux_cleaned.csv", row.names = FALSE)
#write_xlsx(vw2rr_data_cleaned, "vw2_redux_cleaned.xlsx")

#View(dupe_data_2)
```

