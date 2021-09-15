# Analysis of dyad RPS data from Brockbank & Vul (2020)
# Used for submission to Games journal special issue on psychology of games
# Aug. 2021



rm(list = ls())
setwd("/Users/erikbrockbank/web/vullab/rps-games-submission/")


library(tidyverse)
library(viridis)



# GLOBALS ====

FILE_PATH = "data"
DATA_FILE = "rps_v1_data.csv"


MOVE_SET = c("rock", "paper", "scissors")
TRANSITION_SET = c("+", "-", "0")
OUTCOME_SET = c("win", "loss", "tie")

GAME_ROUNDS = 300
# Win count differential outcome for each move combination (player in rows, opponent in cols)
OUTCOME_MATRIX = matrix(c(0, -1, 1, 1, 0, -1, -1, 1, 0), nrow = 3, byrow = T)
rownames(OUTCOME_MATRIX) = c("rock", "paper", "scissors")
colnames(OUTCOME_MATRIX) = c("opp_rock", "opp_paper", "opp_scissors")



# FUNCTIONS: analysis ====

# Function to read in and structure data appropriately
read_data = function(filename, fp) {
  data = read_csv(paste(fp, filename, sep = "/"))

  # Remove incomplete data
  incomplete = data %>%
    group_by(player_id) %>%
    summarize(rounds = max(round_index)) %>%
    filter(rounds < 300) %>%
    select(player_id)
  data = data %>%
    filter(!(player_id %in% incomplete$player_id))

  return(data)
}



# Function to get marginal probability of each move for each participant
get_player_move_dist = function(data, moves) {
  data %>%
    filter(player_move != "none") %>% # ignore "none" moves for this aggregation
    group_by(game_id, player_id) %>%
    count(player_move) %>%
    mutate(total = sum(n),
           pmove = n / total) %>%
    # order by "rock", "paper", "scissors"
    arrange(game_id, player_id, factor(player_move, levels = moves))
}

# Function to get marginal probability of each transition (+/-/0) for each participant
get_player_transition_dist = function(data) {
  data %>%
    group_by(game_id, player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move), # lag call above sets NA for lag on first move: ignore it here
           prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.transition = case_when(prev.move == player_move ~ "0",
                                         ((prev.move == "rock" & player_move == "paper") |
                                            (prev.move == "paper" & player_move == "scissors") |
                                            (prev.move == "scissors" & player_move == "rock")) ~ "+",
                                         ((prev.move == "rock" & player_move == "scissors") |
                                            (prev.move == "paper" & player_move == "rock") |
                                            (prev.move == "scissors" & player_move == "paper")) ~ "-")) %>%
    count(player.transition) %>%
    mutate(total.transitions = sum(n),
           p.transition = n / total.transitions)
}

# Function to get marginal probability of each transition (+/-/0) *relative to opponent's previous move* for each player
get_player_transition_cournot_dist = function(data) {
  data %>%
    group_by(game_id, player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move)) %>% # lag call above sets NA for lag on first move: ignore it here
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.move = ifelse(is.na(lag(player_move, 1)), lead(prev.move, 1), lag(prev.move, 1))) %>% # opponent's one move back (previous move)
    filter(opponent.prev.move != "none" & player_move != "none") %>% # ignore "none" moves for this aggregation
    group_by(game_id, player_id) %>%
    # NB: this can be slow to execute
    mutate(player.transition.cournot = case_when(opponent.prev.move == player_move ~ "0",
                                                 ((opponent.prev.move == "rock" & player_move == "paper") |
                                                    (opponent.prev.move == "paper" & player_move == "scissors") |
                                                    (opponent.prev.move == "scissors" & player_move == "rock")) ~ "+",
                                                 ((opponent.prev.move == "rock" & player_move == "scissors") |
                                                    (opponent.prev.move == "paper" & player_move == "rock") |
                                                    (opponent.prev.move == "scissors" & player_move == "paper")) ~ "-")) %>%
    count(player.transition.cournot) %>%
    mutate(total.transitions = sum(n),
           p.transition = n / total.transitions)
}

# Function to summarize probability of each move for each participant, conditioned on their *opponent's* previous move
get_opponent_prev_move_cond = function(data) {
  data %>%
    # add each player's previous move, then use that when referencing opponent's previous move
    group_by(game_id, player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move)) %>% # lag call above sets NA for lag on very first move: ignore it here
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.move = ifelse(is.na(lag(player_move, 1)), lead(prev.move, 1), lag(prev.move, 1)),
           # category of move given opponent previous move, e.g. "rock-paper"
           move_opponent.prev.move = paste(player_move, opponent.prev.move, sep = "-")) %>%
    filter(player_move != "none", opponent.prev.move != "none") %>% # ignore "none" moves for this aggregation
    group_by(game_id, player_id) %>%
    count(move_opponent.prev.move) %>%
    group_by(game_id, player_id, move_opponent.prev.move) %>%
    mutate(player_move = strsplit(move_opponent.prev.move, "-")[[1]][1], # add player_move back in because we lose it in the count() call above
           opponent.prev.move = strsplit(move_opponent.prev.move, "-")[[1]][2]) %>% # add opponent.prev.move back in because we lose it in the count() call above
    group_by(game_id, player_id, opponent.prev.move) %>%
    mutate(row.totals = sum(n),
           # probability of this player move, conditioned on opponent previous move
           pmove_opponent.prev.move = n / row.totals)
}

# Function to summarize probability of each move for each participant, conditioned on their *own* previous move
get_player_prev_move_cond = function(data) {
  data %>%
    group_by(game_id, player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           # category of move given previous move, e.g. "rock-paper"
           move_prev.move = paste(player_move, prev.move, sep = "-")) %>%
    filter(!is.na(prev.move), # lag call above sets NA for lag on very first move: ignore it here
           player_move != "none", prev.move != "none") %>% # ignore "none" moves for this aggregation
    count(move_prev.move) %>%
    group_by(game_id, player_id, move_prev.move) %>%
    mutate(player_move = strsplit(move_prev.move, "-")[[1]][1], # add player_move back in because we lose it in the count() call above
           prev.move = strsplit(move_prev.move, "-")[[1]][2]) %>% # add prev.move back in because we lose it in the count() call above
    group_by(game_id, player_id, prev.move) %>%
    mutate(row.totals = sum(n),
           # probability of this player move, conditioned on previous move
           pmove_prev.move = n / row.totals)
}

# Function to get conditional distribution of each player's transition (+/-/0), given their previous outcome (win, tie, loss)
get_player_transition_outcome_cond = function(data) {
  sep = "_"
  data %>%
    group_by(game_id, player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           prev.outcome = lag(player_outcome, 1)) %>%
    filter(!is.na(prev.outcome), # lag call above sets NA for lag on first oucome: ignore it here
           !is.na(prev.move), # lag call above sets NA for lag on first move: ignore it here
           prev.move != "none", player_move != "none") %>%
    # NB: this can be slow to execute
    mutate(player.transition = case_when(prev.move == player_move ~ "0",
                                         ((prev.move == "rock" & player_move == "paper") |
                                            (prev.move == "paper" & player_move == "scissors") |
                                            (prev.move == "scissors" & player_move == "rock")) ~ "+",
                                         ((prev.move == "rock" & player_move == "scissors") |
                                            (prev.move == "paper" & player_move == "rock") |
                                            (prev.move == "scissors" & player_move == "paper")) ~ "-"),
           player.outcome.transition = paste(prev.outcome, player.transition, sep = sep)) %>%
    count(player.outcome.transition) %>%
    group_by(game_id, player_id, player.outcome.transition) %>%
    mutate(prev.outcome = strsplit(player.outcome.transition, sep)[[1]][1], # add prev.outcome back in because we lose it in the count() call above
           player.transition = strsplit(player.outcome.transition, sep)[[1]][2]) %>% # add player.transition back in because we lose it in the count() call above
    group_by(game_id, player_id, prev.outcome) %>%
    mutate(row.totals = sum(n),
           # probability of this player transition, conditioned on previous outcome
           p.transition.outcome = n / row.totals)
}

#' Function to get conditional distribution of each player's *Cournot* transition (+/-/0),
#' given their previous outcome (win, tie, loss)
get_player_cournot_transition_outcome_cond = function(data) {
  sep = "_"
  data %>%
    group_by(game_id, player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           prev.outcome = lag(player_outcome, 1)) %>%
    filter(!is.na(prev.outcome), # lag call above sets NA for lag on first oucome: ignore it here
           !is.na(prev.move), # lag call above sets NA for lag on first move: ignore it here
           prev.move != "none", player_move != "none") %>%
    # this code copied from `get.player.transition.cournot.dist` above
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.move = ifelse(is.na(lag(player_move, 1)), lead(prev.move, 1), lag(prev.move, 1))) %>% # opponent's one move back (previous move)
    filter(opponent.prev.move != "none" & player_move != "none") %>% # ignore "none" moves for this aggregation
    # end of copied section
    # NB: this can be slow to execute
    group_by(game_id, player_id) %>%
    mutate(player.cournot.transition = case_when(opponent.prev.move == player_move ~ "0",
                                                 ((opponent.prev.move == "rock" & player_move == "paper") |
                                                    (opponent.prev.move == "paper" & player_move == "scissors") |
                                                    (opponent.prev.move == "scissors" & player_move == "rock")) ~ "+",
                                                 ((opponent.prev.move == "rock" & player_move == "scissors") |
                                                    (opponent.prev.move == "paper" & player_move == "rock") |
                                                    (opponent.prev.move == "scissors" & player_move == "paper")) ~ "-"),
           player.outcome.cournot.transition = paste(prev.outcome, player.cournot.transition, sep = sep)) %>%
    count(player.outcome.cournot.transition) %>%
    group_by(game_id, player_id, player.outcome.cournot.transition) %>%
    mutate(prev.outcome = strsplit(player.outcome.cournot.transition, sep)[[1]][1], # add prev.outcome back in because we lose it in the count() call above
           player.cournot.transition = strsplit(player.outcome.cournot.transition, sep)[[1]][2]) %>% # add player.cournot.transition back in because we lose it in the count() call above
    group_by(game_id, player_id, prev.outcome) %>%
    mutate(row.totals = sum(n),
           # probability of this player transition, conditioned on previous outcome
           p.transition.outcome = n / row.totals)
}


# Function to summarize probability of each move for each participant, conditioned on their *own* previous *two* moves.
get_player_prev_2move_cond = function(data, moves) {
  game_player_set = data %>% distinct(game_id, player_id)
  prev.2move.df = data.frame(game_id = character(), player_id = character(),
                             player_move = character(), prev.move = character(), prev.move2 = character(),
                             n = numeric(), row.totals = numeric(),
                             stringsAsFactors = F)
  # TODO can we do this without a nested loop....
  # Bayesian smoothing, put count of 1 in each combination before adding true counts
  for (player.move in moves) {
    for (prev.move in moves) {
      for (prev.move2 in moves) {
        prev.2move.df = rbind(prev.2move.df, data.frame(game_id = game_player_set$game_id,
                                                        player_id = game_player_set$player_id,
                                                        player_move = player.move,
                                                        prev.move = prev.move,
                                                        prev.move2 = prev.move2,
                                                        n = 1, row.totals = length(moves),
                                                        stringsAsFactors = F))

      }
    }
  }
  tmp = data %>%
    group_by(game_id, player_id) %>%
    mutate(prev.move = lag(player_move, 1), # one move back (previous move)
           prev.move2 = lag(player_move, 2), # two moves back
           move_2prev.move = paste(player_move, prev.move, prev.move2, sep = "-")) %>% # category of move given previous two moves, e.g. "rock-paper-rock"
    filter(!is.na(prev.move), !is.na(prev.move2), # lag calls above set NA for lag on first move and second moves: ignore it here
           player_move != "none", prev.move != "none", prev.move2 != "none") %>% # ignore "none" moves for this aggregation
    count(move_2prev.move) %>%
    group_by(game_id, player_id, move_2prev.move) %>%
    mutate(player_move = strsplit(move_2prev.move, "-")[[1]][1], # add player_move back in because we lose it in the count() call above
           prev.move = strsplit(move_2prev.move, "-")[[1]][2], # add prev.move back in because we lose it in the count() call above
           prev.move2 = strsplit(move_2prev.move, "-")[[1]][3]) %>% # add prev.move2 back in because we lose it in the count() call above
    group_by(game_id, player_id, prev.move, prev.move2) %>%
    mutate(row.totals = sum(n))

  # return initial counts set to 1 in prev.2move.df plus counts calculated in tmp above
  left_join(prev.2move.df, tmp, by = c("game_id", "player_id", "player_move", "prev.move", "prev.move2")) %>%
    mutate(n.agg = ifelse(is.na(n.y), n.x, n.x + n.y),
           row.totals.agg = ifelse(is.na(row.totals.y), row.totals.x, row.totals.x + row.totals.y),
           pmove_2prev.move = n.agg / row.totals.agg) %>%
    select(game_id, player_id, player_move, prev.move, prev.move2,
           n.agg, row.totals.agg, pmove_2prev.move) %>%
    arrange(game_id, player_id, player_move, prev.move, prev.move2)
}

# Function to get conditional distribution of each player's transition (+/-/0), given the combination of *their previous transition and their previous outcome*
get_player_transition_prev_transition_prev_outcome_cond = function(data, transitions, outcomes) {
  game_player_set = data %>% distinct(game_id, player_id)
  player.transition.prev.transition.prev.outcome.df = data.frame(game_id = character(), player_id = character(),
                                                                 player.transition = character(), player.prev.transition = character(), prev.outcome = character(),
                                                                 n = numeric(), row.totals = numeric(),
                                                                 stringsAsFactors = F)
  # TODO can we do this without a nested loop....
  # Bayesian smoothing, put count of 1 in each combination before adding true counts
  for (player.trans in transitions) {
    for (prev.trans in transitions) {
      for (prev.outcome in outcomes) {
        player.transition.prev.transition.prev.outcome.df = rbind(player.transition.prev.transition.prev.outcome.df,
                                                                  data.frame(game_id = game_player_set$game_id,
                                                                             player_id = game_player_set$player_id,
                                                                             player.transition = player.trans,
                                                                             player.prev.transition = prev.trans,
                                                                             prev.outcome = prev.outcome,
                                                                             n = 1, row.totals = length(transitions),
                                                                             stringsAsFactors = F))

      }
    }
  }
  sep = "_"
  tmp = data %>%
    group_by(game_id, player_id) %>%
    mutate(prev.outcome = lag(player_outcome, 1),
           prev.move = lag(player_move, 1),
           prev.move2 = lag(player_move, 2)) %>%
    filter(!is.na(prev.outcome), # lag call above sets NA for lag on first outcome: ignore it here
           !is.na(prev.move), !is.na(prev.move2), # lag call above sets NA for lag on first two moves: ignore it here
           prev.move2 != "none", prev.move != "none", player_move != "none") %>%
    # TODO move to a model where we add all these cols once at the beginning then just summarize in each analysis
    mutate(player.transition = case_when(prev.move == player_move ~ "0",
                                         ((prev.move == "rock" & player_move == "paper") |
                                            (prev.move == "paper" & player_move == "scissors") |
                                            (prev.move == "scissors" & player_move == "rock")) ~ "+",
                                         ((prev.move == "rock" & player_move == "scissors") |
                                            (prev.move == "paper" & player_move == "rock") |
                                            (prev.move == "scissors" & player_move == "paper")) ~ "-"),
           player.prev.transition = case_when(prev.move2 == prev.move ~ "0",
                                              ((prev.move2 == "rock" & prev.move == "paper") |
                                                 (prev.move2 == "paper" & prev.move == "scissors") |
                                                 (prev.move2 == "scissors" & prev.move == "rock")) ~ "+",
                                              ((prev.move2 == "rock" & prev.move == "scissors") |
                                                 (prev.move2 == "paper" & prev.move == "rock") |
                                                 (prev.move2 == "scissors" & prev.move == "paper")) ~ "-"),
           player.transition.prev.transition.prev.outcome = paste(player.transition, player.prev.transition, prev.outcome, sep = sep)) %>%
    count(player.transition.prev.transition.prev.outcome) %>%
    group_by(game_id, player_id, player.transition.prev.transition.prev.outcome) %>%
    mutate(player.transition = strsplit(player.transition.prev.transition.prev.outcome, sep)[[1]][1], # add transition back in because we lose it in the count() call above
           player.prev.transition = strsplit(player.transition.prev.transition.prev.outcome, sep)[[1]][2], # add prev. transition back in because we lose it in the count() call above
           prev.outcome = strsplit(player.transition.prev.transition.prev.outcome, sep)[[1]][3]) %>% # add prev. outcome back in because we lose it in the count() call above
    group_by(game_id, player_id, player.prev.transition, prev.outcome) %>%
    mutate(row.totals = sum(n))

  # return initial counts set to 1 in smoothing df plus counts calculated in tmp above
  left_join(player.transition.prev.transition.prev.outcome.df, tmp, by = c("game_id", "player_id", "player.transition", "player.prev.transition", "prev.outcome")) %>%
    mutate(n.agg = ifelse(is.na(n.y), n.x, n.x + n.y),
           row.totals.agg = ifelse(is.na(row.totals.y), row.totals.x, row.totals.x + row.totals.y),
           p.transition.prev.transition.prev.outcome = n.agg / row.totals.agg) %>%
    select(game_id, player_id, player.transition, player.prev.transition, prev.outcome,
           n.agg, row.totals.agg, p.transition.prev.transition.prev.outcome) %>%
    arrange(game_id, player_id, player.transition, player.prev.transition, prev.outcome)
}



# Get maximum expected win count differential based on move probabilities in player_summary
get_expected_win_count_differential_moves = function(player_summary, outcomes, game_rounds) {
  player_summary %>%
    group_by(game_id, player_id) %>%
    summarize(max_util = max(
      rowSums(matrix(rep(pmove, 3), nrow = 3, byrow = T) * outcomes))) %>%
    mutate(win_diff = max_util * game_rounds)
}

# Get maximum expected win count differential based on transition probabilities in player_summary
# TODO can we unify this with the get_expected_win_count_differential_moves function above?
# only difference is column name (pmove, p.transition)
get_expected_win_count_differential_trans = function(player_summary, outcomes, game_rounds) {
  player_summary %>%
    group_by(game_id, player_id) %>%
    summarize(max_util = max(
      rowSums(matrix(rep(p.transition, 3), nrow = 3, byrow = T) * outcomes))) %>%
    mutate(win_diff = max_util * game_rounds)
}

# Get maximum expected win count differential based on distribution of moves given opponent's previous move
get_expected_win_count_differential_opponent_prev_move = function(player_summary, outcomes, game_rounds) {
  player_summary %>%
    group_by(game_id, player_id, opponent.prev.move) %>%
    # get expected value for each previous move conditional distribution
    summarize(max_util = max(
      rowSums(matrix(rep(pmove_opponent.prev.move, 3), nrow = 3, byrow = T) * outcomes))) %>%
    # normalize expected value for each opponent previous move (uniform)
    mutate(max_util_norm = max_util * (1 / 3)) %>%
    # get overall expected value by summing over (normalized) expected values for each previous move
    group_by(game_id, player_id) %>%
    summarize(win_diff = sum(max_util_norm) * game_rounds)
}

# Get maximum expected win count differential based on distribution of moves given player's previous move
get_expected_win_count_differential_prev_move = function(player_summary, outcomes, game_rounds) {
  player_summary %>%
    group_by(game_id, player_id, prev.move) %>%
    # get expected value for each previous move conditional distribution
    summarize(max_util = max(
      rowSums(matrix(rep(pmove_prev.move, 3), nrow = 3, byrow = T) * outcomes))) %>%
    # normalize expected value for each previous move (uniform)
    mutate(max_util_norm = max_util * (1 / 3)) %>%
    # get overall expected value by summing over (normalized) expected values for each previous move
    group_by(game_id, player_id) %>%
    summarize(win_diff = sum(max_util_norm) * game_rounds)
}

# Get maximum expected win count differential based on distribution of transitions given player's previous outcome
get_expected_win_count_differential_prev_outcome = function(player_summary, outcomes, game_rounds) {
  player_summary %>%
    group_by(game_id, player_id, prev.outcome) %>%
    # get expected value for each previous move conditional distribution
    summarize(max_util = max(
      rowSums(matrix(rep(p.transition.outcome, 3), nrow = 3, byrow = T) * outcomes))) %>%
    # normalize expected value for each previous outcome (uniform)
    mutate(max_util_norm = max_util * (1 / 3)) %>%
    # get overall expected value by summing over (normalized) expected values for each previous move
    group_by(game_id, player_id) %>%
    summarize(win_diff = sum(max_util_norm) * game_rounds)
}

# Get maximum expected win count differential based on distribution of moves given player's previous two moves
get_expected_win_count_differential_prev_2moves = function(player_summary, outcomes, game_rounds) {
  player_summary %>%
    group_by(game_id, player_id, prev.move, prev.move2) %>%
    # get expected value for each previous move conditional distribution
    summarize(max_util = max(
      rowSums(matrix(rep(pmove_2prev.move, 3), nrow = 3, byrow = T) * outcomes))) %>%
    # normalize expected value for each previous two-move combination (uniform)
    mutate(max_util_norm = max_util * (1 / 9)) %>%
    # get overall expected value by summing over (normalized) expected values for each previous move
    group_by(game_id, player_id) %>%
    summarize(win_diff = sum(max_util_norm) * game_rounds)
}

# Get maximum expected win count differential based on distribution of moves given player's previous transition, previous outcome
get_expected_win_count_differential_prev_transition_prev_outcome = function(player_summary, outcomes, game_rounds) {
  player_summary %>%
    group_by(game_id, player_id, player.prev.transition, prev.outcome) %>%
    # get expected value for each previous move conditional distribution
    summarize(max_util = max(
      rowSums(matrix(rep(p.transition.prev.transition.prev.outcome, 3), nrow = 3, byrow = T) * outcomes))) %>%
    # normalize expected value for each previous two-move combination (uniform)
    mutate(max_util_norm = max_util * (1 / 9)) %>%
    # get overall expected value by summing over (normalized) expected values for each previous move
    group_by(game_id, player_id) %>%
    summarize(win_diff = sum(max_util_norm) * game_rounds)
}


# Function to extract win count differentials from empirical game data
get_empirical_win_count_differential = function(data) {
  win_diff = data %>%
    group_by(game_id, player_id) %>%
    count(win_count = player_outcome == "win") %>%
    filter(win_count == TRUE) %>%
    group_by(game_id) %>%
    mutate(opp_win_count = lag(n, 1)) %>%
    filter(!is.na(opp_win_count)) %>%
    summarize(empirical_wcd = abs(n - opp_win_count))
  return(win_diff)
}


# FUNCTIONS: plots ====

individ_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 24),
  axis.title.y = element_text(face = "bold", size = 20),
  axis.title.x = element_text(face = "bold", size = 20),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 14, face = "bold"),
  axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5, face = "bold"),
  # legend text
  legend.text = element_text(size = 16, face = "bold"),
  # facet text
  strip.text = element_text(size = 12),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),

  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom",
  legend.key = element_rect(colour = "transparent", fill = "transparent")
)




# INITIALIZATION: read data ====

data = read_data(DATA_FILE, FILE_PATH)
# unique(data$game_id) # sanity check


# ANALYSIS: Expected win count differentials ====

## Distribution of moves (3 cells)
# get overall probability of each move (for each player)
player_summary = get_player_move_dist(data, MOVE_SET)
# get max utility value for opponent of each player based on each player's move probabilities
player_utils_summary = get_expected_win_count_differential_moves(player_summary, OUTCOME_MATRIX, GAME_ROUNDS) %>%
  group_by(game_id) %>%
  summarize(max_exp_wcd_move_dist = mean(win_diff))


## Distribution of transitions (3 cells)
# get overall probability of each transition (for each player)
player_transition_summary = get_player_transition_dist(data)
# get max utility value for opponent of each player based on each player's transition probabilities
player_transition_utils_summary = get_expected_win_count_differential_trans(player_transition_summary, OUTCOME_MATRIX, GAME_ROUNDS) %>%
  group_by(game_id) %>%
  summarize(max_exp_wcd_transition = mean(win_diff))


## Distribution of transitions *relative to opponent*, i.e. Cournot responses (3 cells)
# get overall probability of each transition (for each player)
player_transition_cournot_summary = get_player_transition_cournot_dist(data)
# get max utility value for opponent of each player based on each player's transition probabilities
player_transition_cournot_utils_summary = get_expected_win_count_differential_trans(player_transition_cournot_summary, OUTCOME_MATRIX, GAME_ROUNDS) %>%
  group_by(game_id) %>%
  summarize(max_exp_wcd_transition_cournot = mean(win_diff))


## Distribution of moves given player's previous move (9 cells)
# get probability of each move for each player given their own previous move
player_prev_move_summary = get_player_prev_move_cond(data)
# get max utility value for opponent of each player based on each player's move probabilities *given their previous move*
player_prev_move_utils_summary = get_expected_win_count_differential_prev_move(player_prev_move_summary, OUTCOME_MATRIX, GAME_ROUNDS) %>%
  group_by(game_id) %>%
  summarize(max_exp_wcd_player_prev_move = mean(win_diff))


## Distribution of moves given opponent's previous move (9 cells)
# get probability of each move for each player given their opponent's previous move
opponent_prev_move_summary = get_opponent_prev_move_cond(data)
# get max utility value for opponent of each player based on each player's move probabilities *given their opponent's previous move*
opponent_prev_move_utils_summary = get_expected_win_count_differential_opponent_prev_move(opponent_prev_move_summary, OUTCOME_MATRIX, GAME_ROUNDS) %>%
  group_by(game_id) %>%
  summarize(max_exp_wcd_opponent_prev_move = mean(win_diff))



## Distribution of transitions given previous outcome (9 cells)
# get probability of each transition for each player given their previous outcome
player_transition_prev_outcome_summary = get_player_transition_outcome_cond(data)
# get max utility value for opponent of ech player based on each player's transition probabilities *given their previous outcome*
player_transition_prev_outcome_utils_summary = get_expected_win_count_differential_prev_outcome(player_transition_prev_outcome_summary, OUTCOME_MATRIX, GAME_ROUNDS) %>%
  group_by(game_id) %>%
  summarize(max_exp_wcd_transition_outcome = mean(win_diff))


## Distribution of Cournot transitions given previous outcome (9 cells)
# get probability of each Cournot transition for each player given their previous outcome
player_cournot_transition_prev_outcome_summary = get_player_cournot_transition_outcome_cond(data)
# get max utility value for opponent of each player based on each player's transition probabilities *given their previous outcome*
player_cournot_transition_prev_outcome_utils_summary = get_expected_win_count_differential_prev_outcome(player_cournot_transition_prev_outcome_summary, OUTCOME_MATRIX, GAME_ROUNDS) %>%
  group_by(game_id) %>%
  summarize(max_exp_wcd_cournot_transition_outcome = mean(win_diff))


## Distribution of moves given player's previous two moves (27 cells)
# get probability of each move for each player given their previous two moves
player_prev_2move_summary = get_player_prev_2move_cond(data, MOVE_SET)
# get max utility value for opponent of each player based on each player's move probabiliteis *given their previous two moves*
player_prev_2move_utils_summary = get_expected_win_count_differential_prev_2moves(player_prev_2move_summary, OUTCOME_MATRIX, GAME_ROUNDS) %>%
  group_by(game_id) %>%
  summarize(max_exp_wcd_player_prev_2move = mean(win_diff))


## Distribution of transitions given player's previous transition and previous outcome (27 cells)
# get probability of each transition for each player given their previous transition and player's previous outcome
player_transition_prev_transition_prev_outcome_summary = get_player_transition_prev_transition_prev_outcome_cond(data, TRANSITION_SET, OUTCOME_SET)
# get max utility value for opponent of each player based on each player's transition probabilities *given their previous transition and previous outcome*
player_transition_prev_transition_prev_outcome_utils_summary = get_expected_win_count_differential_prev_transition_prev_outcome(player_transition_prev_transition_prev_outcome_summary, OUTCOME_MATRIX, GAME_ROUNDS) %>%
  group_by(game_id) %>%
  summarize(max_exp_wcd_transition_prev_transition_prev_outcome = mean(win_diff))



# ANALYSIS: Regression based on win count differentials ====

# Get empirical win count diff
empirical_win_count_diff = get_empirical_win_count_differential(data)

# Combine with expected win count diff
summary_win_diff = empirical_win_count_diff %>%
  inner_join(player_utils_summary, by = "game_id") %>%
  inner_join(player_transition_utils_summary, by = "game_id") %>%
  inner_join(player_transition_cournot_utils_summary, by = "game_id") %>%
  inner_join(opponent_prev_move_utils_summary, by = "game_id") %>%
  inner_join(player_prev_move_utils_summary, by = "game_id") %>%
  inner_join(player_transition_prev_outcome_utils_summary, by = "game_id") %>%
  inner_join(player_cournot_transition_prev_outcome_utils_summary, by = "game_id") %>%
  inner_join(player_prev_2move_utils_summary, by = "game_id") %>%
  inner_join(player_transition_prev_transition_prev_outcome_utils_summary, by = "game_id")

# Regression using residuals from composed predictors
summary_win_diff = summary_win_diff %>%
  mutate(opponent_prev_move_resid = residuals(lm(max_exp_wcd_opponent_prev_move ~ max_exp_wcd_move_dist + max_exp_wcd_transition_cournot)),
         player_prev_move_resid = residuals(lm(max_exp_wcd_player_prev_move ~ max_exp_wcd_move_dist + max_exp_wcd_transition)),
         player_prev_2move_resid = residuals(lm(max_exp_wcd_player_prev_2move ~ max_exp_wcd_move_dist + max_exp_wcd_player_prev_move + max_exp_wcd_transition)),
         transition_outcome_resid = residuals(lm(max_exp_wcd_transition_outcome ~ max_exp_wcd_transition + max_exp_wcd_transition_cournot)),
         transition_prev_trans_outcome_resid = residuals(lm(max_exp_wcd_transition_prev_transition_prev_outcome ~ max_exp_wcd_transition_outcome + max_exp_wcd_cournot_transition_outcome)))

# Full model based on residuals
# This should tell us whether variables composed of other predictors (e.g. transition as subset of prev move) are significant
# above and beyond the compressed predictors they're made up of
mod_resids = with(summary_win_diff,
                  lm(empirical_wcd ~ max_exp_wcd_move_dist +
                       max_exp_wcd_transition +
                       max_exp_wcd_transition_cournot +
                       opponent_prev_move_resid +
                       player_prev_move_resid +
                       # player_opponent_prev_move_resid +
                       player_prev_2move_resid +
                       transition_outcome_resid +
                       transition_prev_trans_outcome_resid))
summary(mod_resids)


# ANALYSIS: correlation of expected and empirical win count differentials ====

# Correlations based on expected win count differentials only (uncorrected)
move_cor = with(summary_win_diff,
                cor.test(max_exp_wcd_move_dist, empirical_wcd))
trans_cor = with(summary_win_diff,
                 cor.test(max_exp_wcd_transition, empirical_wcd))
opp_trans_cor = with(summary_win_diff,
                     cor.test(max_exp_wcd_transition_cournot, empirical_wcd))
prev_move_cor = with(summary_win_diff,
                     cor.test(max_exp_wcd_player_prev_move, empirical_wcd))
opp_prev_move_cor = with(summary_win_diff,
                         cor.test(max_exp_wcd_opponent_prev_move, empirical_wcd))
trans_outcome_cor = with(summary_win_diff,
                         cor.test(max_exp_wcd_transition_outcome, empirical_wcd))
prev_2move_cor = with(summary_win_diff,
                      cor.test(max_exp_wcd_player_prev_2move, empirical_wcd))
trans_outcome_prev_trans_cor = with(summary_win_diff,
                                    cor.test(max_exp_wcd_transition_prev_transition_prev_outcome, empirical_wcd))


# Correlations based on residuals (corrected)
prev_move_resid_cor = with(summary_win_diff,
                           cor.test(player_prev_move_resid, empirical_wcd))
opp_prev_move_resid_cor = with(summary_win_diff,
                               cor.test(opponent_prev_move_resid, empirical_wcd))
trans_outcome_resid_cor = with(summary_win_diff,
                               cor.test(transition_outcome_resid, empirical_wcd))
prev_2move_resid_cor = with(summary_win_diff,
                            cor.test(player_prev_2move_resid, empirical_wcd))
trans_outcome_prev_trans_resid_cor = with(summary_win_diff,
                                          cor.test(transition_prev_trans_outcome_resid, empirical_wcd))



# FIGURE: Correlations between empirical and expected win count differentials ====

wrap_width_narrow = 10
n = length(unique(summary_win_diff$game_id))

corr_diffs = data.frame(dependency = factor(), correction = factor(),
                        corr = double(), ci_lower = double(), ci_upper = double()
)


corr_diffs = rbind(corr_diffs,
                   data.frame(dependency = str_wrap("Choice base rate (R/P/S)", wrap_width_narrow), correction = "Uncorrected",
                              corr = move_cor$estimate[1], se = sqrt((1-(move_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Transition base rate (+/\U2212/0)", wrap_width_narrow), correction = "Uncorrected",
                              corr = trans_cor$estimate[1], se = sqrt((1-(trans_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Opponent transition base rate (+/\U2212/0)", wrap_width_narrow), correction = "Uncorrected",
                              corr = opp_trans_cor$estimate[1], se = sqrt((1-(opp_trans_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Choice given player's prior choice", wrap_width_narrow), correction = "Uncorrected",
                              corr = prev_move_cor$estimate[1], se = sqrt((1-(prev_move_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Choice given player's prior choice", wrap_width_narrow), correction = "Corrected",
                              corr = prev_move_resid_cor$estimate[1], se = sqrt((1-(prev_move_resid_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Choice given opponent's prior choice", wrap_width_narrow), correction = "Uncorrected",
                              corr = opp_prev_move_cor$estimate[1], se = sqrt((1-(opp_prev_move_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Choice given opponent's prior choice", wrap_width_narrow), correction = "Corrected",
                              corr = opp_prev_move_resid_cor$estimate[1], se = sqrt((1-(opp_prev_move_resid_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Transition given prior outcome (W/L/T)", wrap_width_narrow), correction = "Uncorrected",
                              corr = trans_outcome_cor$estimate[1], se = sqrt((1-(trans_outcome_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Transition given prior outcome (W/L/T)", wrap_width_narrow), correction = "Corrected",
                              corr = trans_outcome_resid_cor$estimate[1], se = sqrt((1-(trans_outcome_resid_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Choice given player's prior two choices", wrap_width_narrow), correction = "Uncorrected",
                              corr = prev_2move_cor$estimate[1], se = sqrt((1-(prev_2move_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Choice given player's prior two choices", wrap_width_narrow), correction = "Corrected",
                              corr = prev_2move_resid_cor$estimate[1], se = sqrt((1-(prev_2move_resid_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Transition given prior transition & prior outcome", wrap_width_narrow), correction = "Uncorrected",
                              corr = trans_outcome_prev_trans_cor$estimate[1], se = sqrt((1-(trans_outcome_prev_trans_cor$estimate[1]^2))/(n-2))),
                   data.frame(dependency = str_wrap("Transition given prior transition & prior outcome", wrap_width_narrow), correction = "Corrected",
                              corr = trans_outcome_prev_trans_resid_cor$estimate[1], se = sqrt((1-(trans_outcome_prev_trans_resid_cor$estimate[1]^2))/(n-2)))
)


corr_diffs$dependency = factor(corr_diffs$dependency,
                               levels = c(str_wrap("Choice base rate (R/P/S)", wrap_width_narrow),
                                          str_wrap("Transition base rate (+/\U2212/0)", wrap_width_narrow),
                                          str_wrap("Opponent transition base rate (+/\U2212/0)", wrap_width_narrow),
                                          str_wrap("Choice given player's prior choice", wrap_width_narrow),
                                          str_wrap("Choice given opponent's prior choice", wrap_width_narrow),
                                          str_wrap("Transition given prior outcome (W/L/T)", wrap_width_narrow),
                                          str_wrap("Choice given player's prior two choices", wrap_width_narrow),
                                          str_wrap("Transition given prior transition & prior outcome", wrap_width_narrow)
                               ))


corr_diffs %>%
  ggplot(aes(x = dependency, y = corr, color = correction)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = corr - se, ymax = corr + se),
                width = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_viridis(discrete = T, begin = 0.2, end = 0.8) +
  labs(x = "", y = str_wrap("Correlation with empirical win count differentials", 30)) +
  individ_plot_theme +
  theme(
    axis.text.x = element_text(size = 12, face = "bold", angle = 0),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.title = element_blank()
  ) +
  ggsave(filename = "correlation_correction.png",
         path = "img",
         width = 10, height = 6.5)
