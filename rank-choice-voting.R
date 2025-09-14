#####################################################
#### GUYS & PIES: RANKED-CHOICE VOTING ALGORITHM ####
#####################################################

# ---- load packages ----
pacman::p_load(
  here, # project management
  config, # credentials
  tidyverse, # data cleaning and wrangling
  googledrive, # working with Google Drive
  googlesheets4 # working with Google Sheets
  )

# ---- credentials and authorization  ----
google <- config::get("google")
drive_auth(email = google$email)
gs4_auth(email = google$email)

# ---- get World's Best Pie voting sheet ID ----
wbp_voting <- gs4_find() %>% 
  filter(name == "WBP Ranked-Choice Voting History")

# ---- load meeting params ----
source(here("meeting-params.R"))
# list available meetings
meeting_params$meeting

# ---- choose meeting, set parameters ----
# chosen_meeting <- meeting_params %>% filter(meeting == "Meeting 4")
chosen_meeting <- meeting_params %>% filter(meeting == "Meeting 5")
# set parameters from chosen meeting
meeting <- chosen_meeting$meeting
read_range <- chosen_meeting$read_range
write_range <- chosen_meeting$write_range

# ---- pull voting data ----
votes_raw <- read_sheet(
  ss = wbp_voting$id, 
  sheet = meeting, 
  range = read_range
  ) 

# pivot longer to help create ballots   
votes <- votes_raw %>% 
  rename(vote = Vote) %>% 
  pivot_longer(
    cols = !vote, 
    names_to = "pie",
    values_to = "rank"
    ) %>%
  # assumes up to a top-5 ranking, can adjust as necessary
  mutate(rank_name = case_when(
    rank == 1 ~ "first", 
    rank == 2 ~ "second",
    rank == 3 ~ "third",
    rank == 4 ~ "fourth", 
    rank == 5 ~ "fifth",
    TRUE ~ "unranked"
    ))

# ---- summary stats ---- 
# baseline ranked order of pies within each ballot
ballots <- votes %>% 
  filter(!is.na(rank)) %>%
  arrange(vote, rank, pie) %>% 
  group_by(vote) %>% 
  mutate(top_choice = if_else(rank == min(rank), TRUE, FALSE))

# total number of unique ballots (to calculate percentages)
n_ballots <- length(unique(ballots$vote))

# generate initial voting results
initial_results <- votes %>% 
  group_by(pie, rank_name) %>% 
  summarise(votes = n_distinct(vote), .groups = "drop") %>% 
  arrange(rank_name) %>% 
  pivot_wider(names_from = rank_name, values_from = votes) %>% 
  select(-unranked) %>% 
  mutate(
    across(where(is.integer), ~replace_na(., 0)),
    # % of total 1st place votes (need 50%+ to win)
    percent = first / n_ballots,
    # indicates "total support" (sum of 1st, 2nd, 3rd votes)
    support = rowSums(across(where(is.integer))),
    # vote status at the beginning of a voting round
    initial_status = "Contesting",
    # round status after eliminations occur
    round_status = as.character(NA),
    voting_round = 1
    ) %>% 
  arrange(desc(percent), desc(first), desc(support), pie) %>% 
  select(
    voting_round, 
    pie, 
    initial_status, 
    round_status, 
    percent, 
    everything()
    )

# print initial results
initial_results

# ---- to avoid re-calculating total pie support each time ----
pie_support <- initial_results %>% 
  select(pie, support)

# ---- load functions ----
source(here("functions.R"))
# list available functions
lsf.str()

# ---- combine initial voting results with all elimination rounds ----
full_results <- ranked_choice_alg() %>% 
  bind_rows() %>% 
  select(-initial_status) %>% 
  rename_with(~str_to_title(str_replace_all(., "_", " ")))

# print full results
full_results

# ---- save raw votes, ballot choices, and final results to csv files ---- 
write_csv(
  votes_raw,
  here("voting results", glue::glue("{meeting} - Raw Votes.csv"))
  )
write_csv(
  ballots,
  here("voting results", glue::glue("{meeting} - Ballot Choices.csv"))
  )
write_csv(
  full_results,
  here("voting results", glue::glue("{meeting} - Voting Results.csv"))
  )

# ---- write results back to WBP spreadsheet for league visibility ----
range_write(
  ss = wbp_voting$id, 
  data = full_results,
  sheet = meeting, 
  range = write_range,
  reformat = FALSE
)