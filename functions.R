#############################
#### FUNCTIONS & HELPERS ####
#############################

# ---- function to calculate latest results after any eliminations ---- 
get_results <- function(df) {
  
  df %>% 
    group_by(pie, rank_name) %>% 
    summarise(votes = n_distinct(vote), .groups = "drop") %>% 
    pivot_wider(names_from = rank_name, values_from = votes) %>% 
    mutate(
      across(where(is.integer), ~replace_na(., 0)),
      # % of total 1st place votes (need 50%+ to win)
      percent = first / n_ballots,
      voting_round = as.integer(NA),
      initial_status = "Contesting",
      round_status = as.character(NA)
      ) %>% 
    left_join(., pie_support, "pie") %>% 
    select(pie, initial_status, round_status, percent, everything())
  
}

# ---- ranked-choice algorithm ----
ranked_choice_alg <- function() {

  # initialize voting round number
  round_number <- 1
  # initialize voting results list
  results_list <- list()
  
  # start recursive loop
  repeat {
  
    # determine latest eliminations
    update_status <- initial_results %>% 
    # calculation helpers to determine round status
      mutate(
        is_min_votes = first == min(first),
        is_min_support = support == min(support),
        elim_pies = n_distinct(pie[is_min_votes == TRUE]),
        non_elim_pies = n_distinct(pie[is_min_votes == FALSE]),
        is_winning_pie = percent >= 0.5,
        is_winning_pie_round = max(percent) >= 0.5
      ) %>% 
      mutate(
        round_status = case_when(
          # when pie eventually reaches 50% threshold
          percent >= 0.5 ~ "Winner",
          # pie in contention during winner round automatically eliminated
          is_winning_pie_round == TRUE &
            is_winning_pie == FALSE ~ "Eliminated",
          # typical pie elimination when pie has next lowest votes
          is_min_votes == TRUE & 
            non_elim_pies > 1 ~ "Eliminated",
          # tiebreaker elimination when next lowest voted pies are tied
          # pie with lowest support of tied pies is eliminated first
          is_min_votes == TRUE & 
            elim_pies > 1 &
            non_elim_pies == 1 & 
            is_min_support == TRUE ~ "Eliminated",
          TRUE ~ initial_status
        )
      )%>% 
      select(voting_round:support)
    
    # capture results of all relevant voting rounds in a list
    results_list[[round_number]] <- update_status
    
    # stop ranked-choice voting if a winner is declared; otherwise, continue
    if (max(update_status$round_status) == "Winner") { break }
    
    # isolate recently-eliminated pies  
    elim_join <- update_status %>% 
      filter(round_status == "Eliminated") %>% 
      select(pie)
    
    # recalculate ballot choices based on latest eliminations
    new_ballots <- ballots %>% 
      anti_join(elim_join, "pie") %>% 
      mutate(
        top_choice = if_else(rank == min(rank), TRUE, FALSE),
        rank = if_else(top_choice == TRUE, 1, rank),
        rank_name = if_else(top_choice == TRUE, "first", rank_name)
      )
    # replace ballot input with new output
    ballots <- new_ballots
    
    # initialize new voting round
    round_number <- round_number + 1
    
    # recalculate voting results based on latest eliminations
    new_results <- new_ballots %>% get_results() %>% 
      mutate(voting_round = round_number) %>% 
      relocate(voting_round, .before = everything()) %>% 
      arrange(desc(percent), desc(first), desc(support), pie)
    # replace results input with new output
    initial_results <- new_results
    
  }
  
  # return results of all relevant voting rounds
  results_list
  
}