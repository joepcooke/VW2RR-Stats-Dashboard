
#'title: "VW2RR Dataset Cleaning Script"
#'author: "Joe Cooke"
#'date: "2026-02-27"
#'output: 
#'  html_document: 
#'    keep_md: true

#+ r, include = FALSE

#'For the sake of exercise this file has been reformatted multiple times to further understanding on how to compile reports.
#'However, because it originally started out as an R script I have now reformatted it *again* to once again be one; I apologise
#'for the overall poor formatting though, this code was not intended to be viewed in report style. I did however, go through it quickly to format the comments to be (slightly) nicer. We begin the cleaning by importing libraries:


library("readxl")
library("writexl")
library("tidyverse")
library("dplyr")

#'Then, read the data file:

vw2rr_data <- read_excel("data_cols_format.xlsx")

#Then, rename the columns:

vw2rr_data %>%
  rename(dex_no = "Dex No.")

new_col_names <- c(dex_no = "Dex No.", name = "Pokemon",
                   hp = "HP", atk = "Atk", def = "Def", sp_atk = "SpA", sp_def = "SpD", 
                   speed = "Spe", bst = "Total", ability_1 = "Ability 1", ability_2 = "Ability 2",
                   hidden_ability = "Hidden Ability", type_1 = "Type 1", type_2 = "Type 2",
                   evolutions = "Evolution(s)", notes =  "Additional Notes")

vw2rr_data <- vw2rr_data %>%
rename(all_of(new_col_names))


#Next, define the total number of rows
n <- nrow(vw2rr_data)


#Now we mutate dex_no to remove the "#" from the front:

vw2rr_data_2 <- vw2rr_data %>%
  mutate(dex_no_conc = gsub("#", "", dex_no))


#Then we remove trailing zeros from the "00X" numbers, i.e. we check if first two characters are "00"

vw2rr_data_3 <- vw2rr_data_2 %>%
  mutate(dex_no_conc = ifelse(substr(dex_no_conc, 1, 2) == "00", gsub("00", "", dex_no_conc), dex_no_conc))


#Then, remove trailing zeros from "0XX" numbers, i.e. we check if first character is a "0"

vw2rr_data_4 <- vw2rr_data_3 %>%
  mutate(dex_no_conc = ifelse(substr(dex_no_conc, 1, 1) == "0", substr(dex_no_conc, 2, nchar(dex_no_conc)), dex_no_conc))

vw2rr_data_5 <- vw2rr_data_4 %>%
  mutate(dex_no_int = as.integer(dex_no_conc), temp_no = seq.int(n))

#'Now we replace NA values with the max value of dex_no_int up until that point
#'i.e. we filter the rows (pokemon) up until the ith value and calculate the pokemon with highest dex no.
#'Put more simply, we find the most recent non na value. There are probably much cleaner ways to do this but
#'this is what I did. EDIT: in fact I did think of a cleaner way, we can also filter up until that ith value and then filter out all non NA values, and choose the last value from that dataset.

for (i in 1:n){
  
  temp_data <- vw2rr_data_5 %>%
    filter(temp_no <= i)
  
  vw2rr_data_5$dex_no_int[i] = max(temp_data$dex_no_int, na.rm = TRUE)
  
}

#'Then, we filter to find forms that are duplicated

dupe_data <- vw2rr_data_5 %>%
  group_by(dex_no_int) %>%
  filter(n() > 1)

#'We want to replace dex_no values with #"0" + as.str(dex_no_int) or "00" + as.str(dex_no_int).
#'It should become "00" if number of digits of pokedex number is 1, and "0" if number of digits is 2. And, "" if digits is 3 i.e. no change.

vw2rr_data_6 <- vw2rr_data_5 %>%
  mutate(dex_no_new = paste("#", strrep("0", 3 - nchar(as.character(dex_no_int))), dex_no_int, sep = ""))

#'For duplicate forms/pre-evos we concatenate all the evolution information together.
#'This compiles all relevant evolution information into the correct row, so now we can just
#'filter out the ones that are otherwise NA

vw2rr_data_7 <- vw2rr_data_6 %>%
  group_by(dex_no_new) %>%
  mutate(evolutions_new = paste(evolutions, collapse = ", "))


#'Some alternate forms have no dex number for some reason but we want to keep these.
#'These forms are fully evolved so the duplicate forms we want to keep have 'NA' in their
#'evolutions column (as in, no info about evolutions) whereas the duplicates that arose
#'from pre-evos having multiple evos DON'T HAVE NAs anymore due to the concatenation of
#'groups.

#'So the ones we keep either have valid dex numbers, or for those that don't they're fully
#'evolved and hence have NAs in their evo information from previous cleanup;
#'we then readjust these NAs so they're consistent.

vw2rr_data_8 <- vw2rr_data_7 %>%
  filter(!is.na(dex_no) | str_detect(evolutions_new, "NA")) %>%
  mutate(evolutions_new = ifelse(str_detect(evolutions_new, "NA"), NA, evolutions_new))


#'For the final cleaned dataset for PowerBI, we select the relevant columns and rename them.
#'Specifically, we don't want dex_no and evolutions anymore, we want dex_no_new and evolutions_new instead.

vw2rr_data_cleaned <- vw2rr_data_8 %>%
  select(temp_no, dex_no_new, name, hp, atk, def, sp_atk, sp_def, speed, bst, ability_1, ability_2, hidden_ability, type_1, type_2, evolutions_new, notes) %>%
  rename(dex_no = "dex_no_new", evolutions = "evolutions_new", id = "temp_no")

#'Here is a view of the original data for reference:
View(vw2rr_data)

#'And this is a view of the edited data:
View(vw2rr_data_cleaned)

dupe_data_2 <- vw2rr_data_8 %>%
  group_by(dex_no_int) %>%
  filter(n() > 1)


#'Finally, we write the new data to an xlsx file.

write.csv(vw2rr_data_cleaned,"vw2_redux_cleaned.csv", row.names = FALSE)
write_xlsx(vw2rr_data_cleaned, "vw2_redux_cleaned.xlsx")

