############################
#### MEETING PARAMETERS ####
############################

# ---- load packages ----
pacman::p_load(
  tidyverse # data cleaning and wrangling
  )

# ---- set params ----
m4 <- list(meeting = "Meeting 4", read_range = "A1:H8", write_range = "A11")
m5 <- list(meeting = "Meeting 5", read_range = NA, write_range = NA)
m6 <- list(meeting = "Meeting 6", read_range = NA, write_range = NA)
m7 <- list(meeting = "Meeting 7", read_range = NA, write_range = NA)
m8 <- list(meeting = "Meeting 8", read_range = NA, write_range = NA)

# ---- collect params in tidy format ----
meeting_params <- bind_rows(m4, m5, m6, m7, m8)