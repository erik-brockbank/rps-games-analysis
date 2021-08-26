
# Analysis of dyad RPS data from Brockbank & Vul (2020)
# Used for submission to Games journal speciall issue on psychology of games
# Aug. 2021

rm(list = ls())
setwd("/Users/erikbrockbank/web/vullab/rps-games-submission/")


library(tidyverse)
library(viridis)
library(patchwork)


# GLOBALS ====

DATA_FILE = "rps_v1_data.csv"
DATA_PATH = "/Users/erikbrockbank/web/vullab/rps/analysis"

MOVE_SET = c("rock", "paper", "scissors")


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
get_player_move_dist = function(data) {
  data %>%
    filter(player_move != "none") %>% # ignore "none" moves for this aggregation
    group_by(player_id) %>%
    count(player_move) %>%
    mutate(total = sum(n),
           pmove = n / total) %>%
    # order by "rock", "paper", "scissors"
    arrange(player_id, factor(player_move, levels = MOVE_SET))
}

# Function to get marginal probability of each *previous* move for each participant
# Note this is almost identical to the above function except that it looks at fewer
# moves (since it's only evaluating what was at one point the previous move)
get_player_prev_move_dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move), # lag call above sets NA for lag on first move: ignore it here
           prev.move != "none") %>% # ignore "none" moves for this aggregation
    count(prev.move) %>%
    mutate(total.moves = sum(n),
           p.prev.move = n / total.moves)
}

# Function to get marginal probability of each previous move for each
# participant's *opponent*
get_opponent_prev_move_dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move)) %>% # lag call above sets NA for lag on first move: ignore it here
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.move = ifelse(is.na(lag(player_move, 1)), lead(prev.move, 1), lag(prev.move, 1))) %>% # opponent's one move back (previous move)
    filter(opponent.prev.move != "none") %>% # ignore "none" moves for this aggregation
    group_by(player_id) %>%
    count(opponent.prev.move) %>%
    mutate(total.moves = sum(n),
           p.opponent.prev.move = n / total.moves)
}

# Function to get marginal probability of each set of previous two move
# combinations for each participant
get_player_2move_dist = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1), # one move back (previous move)
           prev.move2 = lag(player_move, 2), # two moves back
           prev.2moves = paste(prev.move, prev.move2, sep = "-")) %>% # category of previous two moves, e.g. "rock-paper"
    filter(!is.na(prev.move), !is.na(prev.move2), # lag calls above set NA for lag on first move and second moves: ignore it here
           prev.move != "none", prev.move2 != "none") %>% # ignore "none" moves for this aggregation
    count(prev.2moves) %>%
    mutate(total.moves = sum(n),
           p.2prev.moves = n / total.moves)
}


# Function to summarize probability of each move for each participant,
# conditioned on their *own* previous move
get_player_prev_move_cond_probs = function(data) {
  data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1),
           # category of move given previous move, e.g. "rock-paper"
           move_prev.move = paste(player_move, prev.move, sep = "-")) %>%
    filter(!is.na(prev.move), # lag call above sets NA for lag on very first move: ignore it here
           player_move != "none", prev.move != "none") %>% # ignore "none" moves for this aggregation
    count(move_prev.move) %>%
    group_by(player_id, move_prev.move) %>%
    mutate(player_move = strsplit(move_prev.move, "-")[[1]][1], # add player_move back in because we lose it in the count() call above
           prev.move = strsplit(move_prev.move, "-")[[1]][2]) %>% # add prev.move back in because we lose it in the count() call above
    group_by(player_id, prev.move) %>%
    mutate(row.totals = sum(n),
           # probability of this player move, conditioned on previous move
           pmove_prev.move = n / row.totals)
}


# Function to summarize probability of each move for each participant,
# conditioned on their *opponent's* previous move
get_opponent_prev_move_cond_probs = function(data) {
  data %>%
    # add each player's previous move, then use that when referencing opponent's previous move
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1)) %>%
    filter(!is.na(prev.move)) %>% # lag call above sets NA for lag on very first move: ignore it here
    group_by(game_id, round_index) %>%
    # opponent's previous move is previous row's prev.move for one of the players, next row's prev.move for the other
    mutate(opponent.prev.move = ifelse(is.na(lag(player_move, 1)), lead(prev.move, 1), lag(prev.move, 1)),
           # category of move given opponent previous move, e.g. "rock-paper"
           move_opponent.prev.move = paste(player_move, opponent.prev.move, sep = "-")) %>%
    filter(player_move != "none", opponent.prev.move != "none") %>% # ignore "none" moves for this aggregation
    group_by(player_id) %>%
    count(move_opponent.prev.move) %>%
    group_by(player_id, move_opponent.prev.move) %>%
    mutate(player_move = strsplit(move_opponent.prev.move, "-")[[1]][1], # add player_move back in because we lose it in the count() call above
           opponent.prev.move = strsplit(move_opponent.prev.move, "-")[[1]][2]) %>% # add opponent.prev.move back in because we lose it in the count() call above
    group_by(player_id, opponent.prev.move) %>%
    mutate(row.totals = sum(n),
           # probability of this player move, conditioned on opponent previous move
           pmove_opponent.prev.move = n / row.totals)
}


# Function to summarize probability of each move for each participant,
# conditioned on their *own* previous *two* moves.
get_player_prev_2move_cond_probs = function(data) {
  player_set = unique(data$player_id)
  prev.2move.df = data.frame(player_id = character(), player_move = character(), prev.move = character(), prev.move2 = character(),
                             n = numeric(), row.totals = numeric(),
                             stringsAsFactors = F)
  # TODO can we do this without a nested loop....
  # Bayesian smoothing, put count of 1 in each combination before adding true counts
  for (player.move in MOVE_SET) {
    for (prev.move in MOVE_SET) {
      for (prev.move2 in MOVE_SET) {
        prev.2move.df = rbind(prev.2move.df, data.frame(player_id = player_set,
                                                        player_move = player.move,
                                                        prev.move = prev.move,
                                                        prev.move2 = prev.move2,
                                                        n = 1, row.totals = length(MOVE_SET),
                                                        stringsAsFactors = F))

      }
    }
  }
  tmp = data %>%
    group_by(player_id) %>%
    mutate(prev.move = lag(player_move, 1), # one move back (previous move)
           prev.move2 = lag(player_move, 2), # two moves back
           move_2prev.move = paste(player_move, prev.move, prev.move2, sep = "-")) %>% # category of move given previous two moves, e.g. "rock-paper-rock"
    filter(!is.na(prev.move), !is.na(prev.move2), # lag calls above set NA for lag on first move and second moves: ignore it here
           player_move != "none", prev.move != "none", prev.move2 != "none") %>% # ignore "none" moves for this aggregation
    count(move_2prev.move) %>%
    group_by(player_id, move_2prev.move) %>%
    mutate(player_move = strsplit(move_2prev.move, "-")[[1]][1], # add player_move back in because we lose it in the count() call above
           prev.move = strsplit(move_2prev.move, "-")[[1]][2], # add prev.move back in because we lose it in the count() call above
           prev.move2 = strsplit(move_2prev.move, "-")[[1]][3]) %>% # add prev.move2 back in because we lose it in the count() call above
    group_by(player_id, prev.move, prev.move2) %>%
    mutate(row.totals = sum(n))

  # return initial counts set to 1 in prev.2move.df plus counts calculated in tmp above
  left_join(prev.2move.df, tmp, by = c("player_id", "player_move", "prev.move", "prev.move2")) %>%
    mutate(n.agg = ifelse(is.na(n.y), n.x, n.x + n.y),
           row.totals.agg = ifelse(is.na(row.totals.y), row.totals.x, row.totals.x + row.totals.y),
           pmove_2prev.move = n.agg / row.totals.agg) %>%
    select(player_id, player_move, prev.move, prev.move2,
           n.agg, row.totals.agg, pmove_2prev.move) %>%
    arrange(player_id, player_move, prev.move, prev.move2)
}



# FUNCTIONS: plots

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
data = read_data(DATA_FILE, DATA_PATH)
# unique(data$game_id)







# ANALYSIS: information gain ====

### First order move analysis: player's marginal move distribution
# p(move = x) -> 3 cells

# get overall probability of each move (for each player)
player_summary = get_player_move_dist(data)

# calculate entropy over the distribution of each move (for each player)
player_entropy = player_summary %>%
  group_by(player_id) %>%
  summarize(entropy_move_dist = -sum(pmove * log2(pmove))) # get entropy for distribution of each move


### Second order move analysis: player's move given their previous move
# p(move = x | prev. move = y) -> 9 cells

# get probability of each move, given previous move
player_prev_move_summary = get_player_prev_move_cond_probs(data)

# get marginal probability of each participant's previous moves
player_prev_move_marginal = get_player_prev_move_dist(data)

# calculate entropy based on probability above, including marginal probability of each previous move calculated in first order analysis above
player_prev_move_entropy = player_prev_move_summary %>%
  group_by(player_id, prev.move) %>%
  # get entropy for distribution of each move, for each prev. move
  summarize(prev.move.entropy = -sum(pmove_prev.move * log2(pmove_prev.move))) %>%
  inner_join(player_prev_move_marginal, by = "player_id") %>%
  # normalize entropy of each move given each prev. move by the probability of that prev. move
  mutate(prev.move.entropy.norm = prev.move.entropy * p.prev.move) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above (entropy values for a move scaled by the probability of that move)
  filter(prev.move.x == prev.move.y) %>%
  # sum normalized entropy for distribution of each move given each prev. move
  summarize(entropy_prev_move = sum(prev.move.entropy.norm))



### Second order move analysis: player's move given their opponent's previous move
# p(move = x | opponent prev. move = y) -> 9 cells

# get probability associated with a move, given each of the opponent's possible previous moves
opponent_prev_move_summary = get_opponent_prev_move_cond_probs(data)
# get marginal probability of each of the opponent's possible previous moves
opponent_prev_move_marginal = get_opponent_prev_move_dist(data)

# calculate entropy of the distribution of each move given opponent's previous move, scaled by opponent's marginal probability of each previous move
opponent_prev_move_entropy = opponent_prev_move_summary %>%
  group_by(player_id, opponent.prev.move) %>%
  summarize(opponent.prev.move.entropy = -sum(pmove_opponent.prev.move * log2(pmove_opponent.prev.move))) %>% # get entropy for distribution of each move, for each possible opponent's prev. move
  inner_join(opponent_prev_move_marginal, by = "player_id") %>%
  # normalize entropy of each move given each prev. move by the probability of that prev. move
  mutate(opponent.prev.move.entropy.norm = opponent.prev.move.entropy * p.opponent.prev.move) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above (entropy values for a move scaled by the probability of that move)
  filter(opponent.prev.move.x == opponent.prev.move.y) %>%
  # sum normalized entropy for distribution of each move given each prev. move
  summarize(entropy_opponent_prev_move = sum(opponent.prev.move.entropy.norm))


### Third order move analysis: player's move given their previous two moves
# p(move = x | prev. move 1 = y, prev. move 2 = z) -> 27 cells
# get probability of each move, given previous two moves
player_prev_2move_summary = get_player_prev_2move_cond_probs(data)

# get marginal probability of each unique set of previous two moves
player_prev_2move_marginal = get_player_2move_dist(data)

# calculate entropy based on probability above, including marginal probability of each set of previous two moves
player_prev_2move_entropy = player_prev_2move_summary %>%
  group_by(player_id, prev.move, prev.move2) %>%
  # get entropy for distribution of each move, for each set of prev. two moves
  summarize(prev.2move.entropy = -sum(pmove_2prev.move * log2(pmove_2prev.move))) %>%
  # add column for coalescing previous two moves
  mutate(prev.2moves = paste(prev.move, prev.move2, sep = "-")) %>%
  inner_join(player_prev_2move_marginal, by = "player_id") %>%
  # normalize entropy of each move given each set of previous two moves by the probability of those previous 2 moves
  mutate(prev.2move.entropy.norm = prev.2move.entropy * p.2prev.moves) %>%
  group_by(player_id) %>%
  # select only relevant rows from normalization process above (entropy values for a move scaled by the probability of that move)
  filter(prev.2moves.x == prev.2moves.y) %>%
  # sum normalized entropy for distribution of each move given each set of 2 prev. moves
  summarize(entropy_prev_2move = sum(prev.2move.entropy.norm))





# ANALYSIS: information gain correction ====

# NB: We exclude player move given player's previous move, opponent's previous move
# The dependency structure for this one is so dense, it produces negative
# information gain, presumably because we are actually over-counting our shared
# information a bit with this method

information_gain_uncorrected = player_entropy %>%
  # move distributions
  left_join(player_prev_move_entropy, by = "player_id") %>%
  left_join(opponent_prev_move_entropy, by = "player_id") %>%
  left_join(player_prev_2move_entropy, by = "player_id") %>%
  #
  gather(entropy_type, entropy_val, entropy_move_dist:entropy_prev_2move) %>%
  mutate(entropy_val = log2(length(MOVE_SET)) - entropy_val) %>%
  rename(information_gain = entropy_val) %>%
  mutate(corrected = "Uncorrected")


information_gain_corrected = player_entropy %>%
  # move distributions
  left_join(player_prev_move_entropy, by = "player_id") %>%
  left_join(opponent_prev_move_entropy, by = "player_id") %>%
  left_join(player_prev_2move_entropy, by = "player_id") %>%
  #
  gather(entropy_type, entropy_val, entropy_move_dist:entropy_prev_2move) %>%
  mutate(entropy_val = log2(length(MOVE_SET)) - entropy_val) %>%
  rename(information_gain = entropy_val) %>%
  spread(entropy_type, information_gain) %>%
  mutate(
         # TODO fix this one to subtract cournot transition
         entropy_opponent_prev_move = entropy_opponent_prev_move - entropy_move_dist, # opponent prev move = "" - cournot transition - move dist
         # TODO fix this one to subtract transition
         entropy_prev_move = entropy_prev_move - entropy_move_dist, # player prev move = "" - transition - move dist
         # transition.entropy.1.player.outcome = transition.entropy.1.player.outcome - transition.entropy.0 - transition.entropy.0.5, # transition|outcome = "" - transition - cournot transition
         # transition.entropy.1.5.player.outcome = transition.entropy.1.5.player.outcome - transition.entropy.0 - transition.entropy.0.5, # cournot transition|outcome = "" - transition - cournot transition
         # TODO fix this one to also subtract transition
         entropy_prev_2move = entropy_prev_2move - entropy_prev_move - entropy_move_dist # player prev 2 moves = "" - corrected player prev move - uncorrected move dist - uncorrected transition dist
         # transition.entropy.2.player.prev.transition.prev.outcome = transition.entropy.2.player.prev.transition.prev.outcome - transition.entropy.1.player.outcome - transition.entropy.1.5.player.outcome - transition.entropy.0 - transition.entropy.0.5 # trans | prior trans, prior outcome = "" - corrected transition|outcome - corrected cournot transition|outcome - uncorrected transition - uncorrected cournot
         ) %>%
  gather(entropy_type, information_gain, entropy_move_dist:entropy_prev_move) %>%
  mutate(corrected = "Corrected")


information_gain_combined = rbind(information_gain_uncorrected,
                                  information_gain_corrected)

information_gain_summary = information_gain_combined %>%
  group_by(corrected, entropy_type) %>%
  summarize(mean_information_gain = mean(information_gain),
            se_information_gain = sd(information_gain) / sqrt(length(unique(player_id))))




# ANALYSIS: information gain plots ====

legend_width = 10
graph_labels_final = c("entropy_move_dist" = str_wrap("Choice base rate (R/P/S)", legend.width),
                       # "transition.entropy.0" = str_wrap("Transition base rate (+/-/0)", legend.width),
                       # "transition.entropy.0.5" = str_wrap("Opponent transition base rate (+/-/0)", legend.width),
                       "entropy_prev_move" = str_wrap("Choice given player's prior choice", legend.width),
                       "entropy_opponent_prev_move" = str_wrap("Choice given opponent's prior choice", legend.width),
                       # "transition.entropy.1.player.outcome" = str_wrap("Transition given prior outcome (W/L/T)", legend.width),
                       "entropy_prev_2move" = str_wrap("Choice given player's prior two choices", legend.width)
                       # "transition.entropy.2.player.prev.transition.prev.outcome" = str_wrap("Transition given prior transition & prior outcome", legend.width)
)

information_gain_summary = information_gain_summary %>%
  mutate(entropy_type = graph_labels_final[entropy_type])

information_gain_summary$entropy_type = factor(information_gain_summary$entropy_type,
                                               levels =
                                                 c(str_wrap("Choice base rate (R/P/S)", legend.width),
                                                   # str_wrap("Transition base rate (+/-/0)", legend.width),
                                                   # str_wrap("Opponent transition base rate (+/-/0)", legend.width),
                                                   str_wrap("Choice given player's prior choice", legend.width),
                                                   str_wrap("Choice given opponent's prior choice", legend.width),
                                                   # str_wrap("Transition given prior outcome (W/L/T)", legend.width),
                                                   str_wrap("Choice given player's prior two choices", legend.width)
                                                   # str_wrap("Transition given prior transition & prior outcome", legend.width)
                                                   )
                                               )

ggplot(data = information_gain_summary,
       aes(x = entropy_type, y = mean_information_gain, color = corrected)) +
  geom_point(size = 5) +
  geom_errorbar(
    aes(ymin = mean_information_gain - se_information_gain, ymax = mean_information_gain + se_information_gain),
    width = 0.25) +
  labs(y = "Information gain (bits)") +
  scale_color_viridis(discrete = T, begin = 0.2, end = 0.8) +
  individ_plot_theme +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold", angle = 0),
        axis.text.y = element_text(size = 12, face = "bold")
  )



