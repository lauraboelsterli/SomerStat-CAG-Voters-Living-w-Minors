library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(tools)


# for pipe delim and excel 
read_file <- function(file) {
  ext <- file_ext(file) # match the file extension to read in correctly
  if (ext %in% c("csv", "txt")) {
    df <- read_delim(file, delim = "|", show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    df <- read_excel(file)
  } 
  return(df)
}

pres_election <- read_file("11052024presidentialelection.txt")
state_election <- read_file("11082022stateelection.txt")
city_election <- read_file("11072023cityelection.txt")
minor_households <- read_file("address with minord.xlsx")

# clean and standardize all dfs 
cleaning_cols <- function(df1, df2) {
  # strip trailing spaces from column names
  names(df1) <- str_squish(names(df1))
  names(df2) <- str_squish(names(df2))
  
  # rename columns in df2 to match df1
  df2 <- df2 %>%
    rename(
      `Residential Address - Street Number` = `Res. - St. #`,
      `Residential Address - Street Suffix` = `Res. - St. # Suffix`,
      `Residential Address - Street Name` = `Res. - St. Name`,
      `Residential Address - Apartment Number` = `Res. - Apt #`,
      `Residential Address - Zip Code` = `Res. - Zip Code`,
      `Ward Number` = `Ward #`,
      `Precinct Number` = `Precinct #`
    )
  
  # lowercase text columns for matching
  df1 <- df1 %>%
    mutate(
      `Residential Address - Street Suffix` = str_to_lower(`Residential Address - Street Suffix`),
      `Residential Address - Street Name` = str_to_lower(`Residential Address - Street Name`)
    )
  df2 <- df2 %>%
    mutate(
      `Residential Address - Street Suffix` = str_to_lower(`Residential Address - Street Suffix`),
      `Residential Address - Street Name` = str_to_lower(`Residential Address - Street Name`)
    )
  
  # if the street suffix has a number in it then im assuming its a data entry mistake 
  # since theres fractions and numbers such as 45659 there as suffixes that 
  # is messing with the matchin process (nothing signifcant but like 4-5 households are 
  # not matches accurately bc of this, like college ave)
  # Set street suffix to NA if it contains any digits (likely a data entry error)
  df1 <- df1 %>%
    mutate(`Residential Address - Street Suffix` = if_else(
      str_detect(`Residential Address - Street Suffix`, "\\d"),
      NA_character_,
      `Residential Address - Street Suffix`
    ))
  df2 <- df2 %>%
    mutate(`Residential Address - Street Suffix` = if_else(
      str_detect(`Residential Address - Street Suffix`, "\\d"),
      NA_character_,
      `Residential Address - Street Suffix`
    ))
  
  # convert all text cols to str
  columns_to_str <- c(
    "Residential Address - Street Suffix",
    "Residential Address - Street Name",
    "Precinct Number",
    "Residential Address - Apartment Number"
  )
  df1 <- df1 %>% mutate(across(all_of(columns_to_str), ~ str_trim(as.character(.))))
  df2 <- df2 %>% mutate(across(all_of(columns_to_str), ~ str_trim(as.character(.))))
  
  # convert number cols to int
  columns_to_int <- c("Residential Address - Street Number", "Residential Address - Zip Code", "Ward Number")
  df1 <- df1 %>% mutate(across(all_of(columns_to_int), ~ as.integer(.)))
  df2 <- df2 %>% mutate(across(all_of(columns_to_int), ~ as.integer(.)))
  
  # concat address cols into one for easier matching
  address_cols <- c(
    "Residential Address - Street Number",
    "Residential Address - Street Suffix",
    "Residential Address - Street Name",
    "Residential Address - Apartment Number",
    "Residential Address - Zip Code",
    "Ward Number",
    "Precinct Number"
  )
  df1 <- df1 %>% mutate(address_key = do.call(paste, c(across(all_of(address_cols)), sep = "_")))
  df2 <- df2 %>% mutate(address_key = do.call(paste, c(across(all_of(address_cols)), sep = "_")))
  
  return(list(df1 = df1, df2 = df2))
}


cleaned_pres_election_list <- cleaning_cols(pres_election, minor_households)
pres_voters <- cleaned_pres_election_list$df1
minors_addresses <- cleaned_pres_election_list$df2

cleaned_state_election_list <- cleaning_cols(state_election, minor_households)
state_voters <- cleaned_state_election_list$df1

cleaned_city_election_list <- cleaning_cols(city_election, minor_households)
city_voters <- cleaned_city_election_list$df1

write_csv(pres_voters, "presidential_voters.csv")
write_csv(state_voters,"state_voters.csv")
write_csv(city_voters, "city_voters.csv")


check_4_dupes <- function(df) {
  dupes <- duplicated(df$address_key)
  df_name <- deparse(substitute(df))  # get variable name (solely for printing purposes)
  cat("There are", sum(dupes), "duplicates in", df_name, "\n")
}

remove_dupes <- function(df) {
  df %>% distinct(address_key, .keep_all = TRUE)
}

# checked with my python version, and I'm getting 
# the same num of dupes - yay! 
# check_4_dupes(minors_addresses)

# remove dupes from the minor df 
minor_households_deduped <- remove_dupes(minors_addresses)
# find total households - cross-checked dimensions and they match with python
presidential_household_votes <- remove_dupes(pres_voters)
state_household_votes <- remove_dupes(state_voters)
city_household_votes <- remove_dupes(city_voters)




# with deduped voting sets, run it against the minor df for flagging
# left join minors with election datasets and flag where its matched 
# FIXED THIS
flag_households_children <- function(df1, df2) {
  df2 %>%
    mutate(Have_Children = if_else(address_key %in% df1$address_key, "Yes", "No"))
}


#check flags
# number matches the py version as well!
flagged_presidential_households <- flag_households_children(minor_households_deduped, presidential_household_votes)
flagged_state_households <- flag_households_children(minor_households_deduped, state_household_votes)
flagged_city_households <- flag_households_children(minor_households_deduped, city_household_votes)



write_csv(flagged_presidential_households, "presidential_votes_flagged.csv")
write_csv(flagged_state_households, "state_votes_flagged.csv")
write_csv(flagged_city_households,"city_votes_flagged.csv")
write_csv(minor_households_deduped, "minors_households.csv")


calc_percent_household_voters <- function(household_w_child, households){
  # df_name <- deparse(substitute(df))  # get variable name (solely for printing purposes)
  households_w_children <- (household_w_child/households)*100
  return(cat(households_w_children, "% of households w children voted"))
}

count_households_children_pres <- sum(flagged_presidential_households$Have_Children == "Yes")
count_households_children_state <- sum(flagged_state_households$Have_Children == "Yes")
count_households_children_city <- sum(flagged_city_households$Have_Children == "Yes")


calc_percent_household_voters(sum(flagged_city_households$Have_Children == "No"), nrow(city_household_votes))
# calc_percent_household_voters(count_households_children_city, nrow(city_household_votes))

# python counts of households that votes (to cross check):
# pres
# 2234
# city
# 1820
# local
# 1473


count_total_minor_households <- nrow(minor_households_deduped)
# presidential_child_household_vote_percentage 
# calc_percent_household_voters(count_households_children_pres, minor_households_deduped)
calc_percent_household_voters(count_households_children_pres, count_total_minor_households)
calc_percent_household_voters(count_households_children_state, count_total_minor_households)
calc_percent_household_voters(count_households_children_city, count_total_minor_households)


# avg people per household 
# groupby addres_key and find the avg the num of voters
avg_voters <- function(election_individual_votes){
  election_individual_votes %>%
  group_by(address_key) %>%
  summarise(voter_count = n()) %>%
  summarise(avg_voters_per_household = mean(voter_count)) %>%
  pull(avg_voters_per_household)}


avg_voters(pres_voters)
avg_voters(state_voters)
avg_voters(city_voters)