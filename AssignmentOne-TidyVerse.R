library(tidyverse)
library(janitor)

data <- read_csv("myhivdata1.csv")
head(data)


# Complete data cleaning script
library(tidyverse)
library(janitor)

# Read the data
data <- read_csv("myhivdata1.csv")

# Step 1: Clean names
data_clean <- data %>%
  clean_names()

# Step 2: Replace all "N/A" strings with NA
data_clean <- data_clean %>%
  mutate(across(where(is.character), ~na_if(., "N/A")))

# Step 3: Check for placeholder dates (like 1/1/1980)
data_clean <- data_clean %>%
  mutate(date_submitted = ifelse(date_submitted == "1/1/1980 00:00", 
                                 NA, 
                                 date_submitted))

# Step 4: Fix sibling columns (they should be numeric)
data_clean <- data_clean %>%
  mutate(
    how_many_siblings_do_you_have_include_yourself_brothers = 
      as.numeric(how_many_siblings_do_you_have_include_yourself_brothers),
    how_many_siblings_do_you_have_include_yourself_sisters = 
      as.numeric(how_many_siblings_do_you_have_include_yourself_sisters)
  )

# Step 5: Check the cleaned data
cat("Original data dimensions:", dim(data), "\n")
cat("Cleaned data dimensions:", dim(data_clean), "\n")

# View a summary
glimpse(data_clean)

# Check missing values
cat("\nMissing values in key columns:\n")
data_clean %>%
  select(what_is_your_gender, which_program_are_you_pursuing, 
         who_sponsors_your_studies) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  print()


# QUESTION1: How many variables and cases does the dataset have?
cat("Q1: Dataset dimensions\n")
cat("Number of cases (rows):", nrow(data_clean), "\n")
cat("Number of variables (columns):", ncol(data_clean), "\n\n")


# QUESTION2: How many of the students were Males and how many were females?
cat("Q2: Gender distribution\n")
gender_counts <- data_clean %>%
  filter(!is.na(what_is_your_gender)) %>%
  count(what_is_your_gender) %>%
  rename(Gender = what_is_your_gender, Count = n)

print(gender_counts)
cat("\n")


# QUESTION3: What percentage of the students were taking Undergraduate degrees?
cat("Q3: Undergraduate percentage\n")
total_students <- data_clean %>%
  filter(!is.na(which_program_are_you_pursuing)) %>%
  nrow()

undergrad_count <- data_clean %>%
  filter(which_program_are_you_pursuing == "Undergraduate") %>%
  nrow()

undergrad_percentage <- (undergrad_count / total_students) * 100
cat("Total students with valid program info:", total_students, "\n")
cat("Undergraduate students:", undergrad_count, "\n")
cat("Percentage of Undergraduate students:", round(undergrad_percentage, 2), "%\n\n")




# QUESTION4: What percentage of the Undergraduate students were females?
cat("Q4: Female percentage among Undergraduate students\n")
undergrad_gender <- data_clean %>%
  filter(which_program_are_you_pursuing == "Undergraduate" &
           !is.na(what_is_your_gender)) %>%
  count(what_is_your_gender) %>%
  mutate(Percentage = (n / sum(n)) * 100)

print(undergrad_gender)
cat("\n")



# Q5: Where do the male students mostly reside while in session? (Give the percentage)
cat("Q5: Male students' residence during session\n")
male_residence <- data_clean %>%
  filter(what_is_your_gender == "Male" &
           !is.na(where_do_your_reside_while_in_session)) %>%
  count(where_do_your_reside_while_in_session) %>%
  mutate(Percentage = (n / sum(n)) * 100) %>%
  arrange(desc(Percentage))

print(male_residence)
cat("\n")



# QUESTION6: While not in session, what percentage of the students stayed with both parents?
cat("Q6: Percentage of students staying with both parents out of session\n")
both_parents_count <- data_clean %>%
  filter(who_do_you_usually_live_with_while_not_in_session_mother == "Yes" &
           who_do_you_usually_live_with_while_not_in_session_father == "Yes") %>%
  nrow()

total_valid <- data_clean %>%
  filter(!is.na(who_do_you_usually_live_with_while_not_in_session_mother) &
           !is.na(who_do_you_usually_live_with_while_not_in_session_father)) %>%
  nrow()

both_parents_percentage <- (both_parents_count / total_valid) * 100
cat("Total valid responses for parent questions:", total_valid, "\n")
cat("Students staying with both parents:", both_parents_count, "\n")
cat("Percentage staying with both parents:", round(both_parents_percentage, 2), "%\n\n")



# QUESTION7: Is there an association between gender and where they reside while out of session?
cat("Q7: Association between gender and residence out of session\n")
gender_residence_table <- data_clean %>%
  filter(!is.na(what_is_your_gender) &
           !is.na(where_do_you_reside_while_out_of_session)) %>%
  select(what_is_your_gender, where_do_you_reside_while_out_of_session)

if(nrow(gender_residence_table) > 0) {
  # Chi-square test for association
  chi_test <- chisq.test(table(gender_residence_table))
  cat("Chi-square test results:\n")
  cat("X-squared =", round(chi_test$statistic, 3), 
      ", df =", chi_test$parameter, 
      ", p-value =", format.pval(chi_test$p.value, digits = 3), "\n\n")
} else {
  cat("Not enough data for chi-square test\n\n")
}


# QUESTION8: How many siblings on average did self-sponsored students have?
cat("Q8: Average siblings for self-sponsored students\n")
self_sponsored_siblings <- data_clean %>%
  filter(who_sponsors_your_studies == "Self sponsored" &
           !is.na(how_many_siblings_do_you_have_include_yourself_brothers) &
           !is.na(how_many_siblings_do_you_have_include_yourself_sisters)) %>%
  mutate(
    brothers = how_many_siblings_do_you_have_include_yourself_brothers,
    sisters = how_many_siblings_do_you_have_include_yourself_sisters,
    total_siblings = brothers + sisters - 1  # Subtract 1 because question says "include yourself"
  )

if(nrow(self_sponsored_siblings) > 0) {
  avg_siblings <- mean(self_sponsored_siblings$total_siblings, na.rm = TRUE)
  cat("Number of self-sponsored students analyzed:", nrow(self_sponsored_siblings), "\n")
  cat("Average number of siblings (excluding self) for self-sponsored students:", round(avg_siblings, 2), "\n\n")
} else {
  cat("No self-sponsored students with valid sibling data found\n\n")
}


#Question 9.What was the source of the money that student spent while in session? Provide a table summary for the average amount from each source

money_sources_table <- data_clean %>%
  select(
    what_is_the_source_of_the_money_that_you_normally_spend_while_in_session_salary_and_wages_from_a_job_including_self_employment,
    what_is_the_source_of_the_money_that_you_normally_spend_while_in_session_charities,
    what_is_the_source_of_the_money_that_you_normally_spend_while_in_session_parents_guardians,
    what_is_the_source_of_the_money_that_you_normally_spend_while_in_session_friends,
    what_is_the_source_of_the_money_that_you_normally_spend_while_in_session_helb,
    what_is_the_source_of_the_money_that_you_normally_spend_while_in_session_bursary,
    what_is_the_source_of_the_money_that_you_normally_spend_while_in_session_other
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "source_of_money",
    values_to = "response"
  ) %>%
  filter(response == "Yes") %>%
  group_by(source_of_money) %>%
  summarise(
    number_of_students = n()
  ) %>%
  mutate(
    percentage = round(100 * number_of_students / sum(number_of_students), 1)
  )

money_sources_table


# QUESTION10: How many students have ever taken drug(s)?
cat("Q10: Students who have ever taken drugs\n")
# Find the drug use column
drug_col <- names(data_clean)[str_detect(names(data_clean), "have_you_ever_used_any_of_the_following_drugs")][1]

if(!is.na(drug_col)) {
  drug_takers <- data_clean %>%
    filter(.data[[drug_col]] == "Yes") %>%
    nrow()
  
  total_valid_drug <- data_clean %>%
    filter(!is.na(.data[[drug_col]])) %>%
    nrow()
  
  drug_percentage <- ifelse(total_valid_drug > 0, (drug_takers / total_valid_drug) * 100, 0)
  cat("Total valid responses about drug use:", total_valid_drug, "\n")
  cat("Number of students who have ever taken drugs:", drug_takers, "\n")
 
} else {
  cat("Drug use column not found\n\n")
  
}


# QUESTION11: Is students gender associated with drug/substance abuse?
cat("Q11: Association between gender and drug use\n")
if(!is.na(drug_col)) {
  gender_drug_table <- data_clean %>%
    filter(!is.na(what_is_your_gender) &
             !is.na(.data[[drug_col]])) %>%
    select(what_is_your_gender, drug_col)
  
  if(nrow(gender_drug_table) > 0) {
    # Create contingency table
    contingency_table <- table(gender_drug_table)
    cat("Contingency table:\n")
    print(contingency_table)
    
    # Chi-square test
    chi_drug_test <- chisq.test(contingency_table)
    cat("\nChi-square test results:\n")
    cat("X-squared =", round(chi_drug_test$statistic, 3), 
        ", df =", chi_drug_test$parameter, 
        ", p-value =", format.pval(chi_drug_test$p.value, digits = 3), "\n\n")
  } else {
    cat("Not enough data for chi-square test\n\n")
  }
} else {
  cat("Drug use column not found\n\n")
}


# QUESTION12: Among the reasons why drug/substance use/abuse was increasing, 
# how many mentioned peer pressure?
cat("Q12: Students mentioning 'peer pressure' in reasons\n")
# Find the reasons column
reasons_col <- names(data_clean)[str_detect(names(data_clean), "give_reasons_for_your_answer")][1]

if(!is.na(reasons_col)) {
  total_with_reasons <- sum(!is.na(data_clean[[reasons_col]]))
  cat("Total students who gave reasons:", total_with_reasons, "\n")
  
  if(total_with_reasons > 0) {
    # Count mentions of "peer" (case-insensitive)
    peer_mentions <- data_clean %>%
      filter(!is.na(.data[[reasons_col]])) %>%
      mutate(mentions_peer = str_detect(tolower(.data[[reasons_col]]), "peer")) %>%
      filter(mentions_peer == TRUE) %>%
      nrow()
    
    cat("Number of students mentioning 'peer' in their reasons:", peer_mentions, "\n")
    
    # Also count variations like "peer pressure", "peer influence", etc.
    peer_variations <- data_clean %>%
      filter(!is.na(.data[[reasons_col]])) %>%
      mutate(
        reasons_lower = tolower(.data[[reasons_col]]),
        mentions_peer_pressure = str_detect(reasons_lower, "peer pressure"),
        mentions_peer_influence = str_detect(reasons_lower, "peer influence"),
        mentions_peers = str_detect(reasons_lower, "peers"),
        any_peer_mention = mentions_peer_pressure | mentions_peer_influence | mentions_peers
      ) %>%
      filter(any_peer_mention == TRUE)
    
    cat("Number of students mentioning peer-related terms (pressure/influence/peers):", nrow(peer_variations), "\n")
    
    # Show some reasons
    if(nrow(peer_variations) > 0) {
      cat("\nNumber of responses mentioning peer pressure:\n")
      examples <- peer_variations %>%
        select(all_of(reasons_col)) %>%
        head(3)
      print(examples)
    }
  } else {
    cat("No students provided reasons\n")
  }
} else {
  cat("Reasons column not found\n")
}