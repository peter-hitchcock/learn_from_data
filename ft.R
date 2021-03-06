########################### LEARNING ###########################
UpdateCriticValue <- function(c_values, sidx, critic_LR, outcome, verbose=0) {
  ### Update the appropriate critic value based on the state we're in ###
  # Calc's a PE based on state value. Then passes this to actor to update its weights 
  # Args: sidx=state value
  c_values <- as.numeric(unlist(c_values))
  AC_PE <- as.nuermic(outcome - c_values[sidx]) #as.numeric(unlist(c_values))[sidx]
  c_values[sidx] <- as.numeric(c_values[sidx] + critic_LR * AC_PE)
  if (!is.null(verbose)) cat("\n c_values", as.numeric(unlist(c_values))[sidx])
  list("critic_values"=c_values, "AC_PE"=AC_PE)
} 
UpdateActorWeights <- function(a_weights, sidx, action, actor_LR, AC_PE) {
  ### Update the action weight only in the wake of a pos. PE. Args: sidx=state value ###
  # ** Check this.. If I understood from Geana paper this is only when PE is non-negative
  if (AC_PE > 0) a_weights[sidx, action] <- a_weights[sidx, action] + actor_LR * AC_PE
  a_weights  
}
CalcQVals <- function(q_vals, q_LR, sidx, action, outcome) {
  ### Calc Q vals tracking full reward info. Args: sidx=state value ###
  qPE <- outcome - q_vals[sidx, action]
  q_vals[sidx, action] <- q_vals[sidx, action] + q_LR * qPE
  q_vals
}
### Mix q values and AC values for this state outputting hybrid values ###
MixACAndQVals <- function(qv_row, aw_row, value_mix_par) (1-value_mix_par) * aw_row + value_mix_par * qv_row
# **Not implementing decay yet because just starting with training phase
######################################################################

########################### CHOICE ###########################
### Softmax choice fx. Outputs chance of picking left stimulus ###
CalcSoftmaxProbLeft <- function(values, beta) exp(beta*values[1])/sum(exp(beta*values[1]), exp(beta*values[2]))
### Mix the left choice probability with nondirected random choice (reflecting lapsing) with 
# contribution scaled by lapsines ###
MixLeftChoiceWRandom <- function(left_c_prob_sm, lapsiness) (1 - lapsiness) * left_c_prob_sm + lapsiness * .5
######################################################################

## Simulate training phase ##
training_trials <- unlist(lapply(1:160, function(x) sample(training_trials, 1)))
train_df <- data.frame("trial"=1:160, "stimuli"=training_trials)
stim_set <- key$stim # Vectorize 
## Par inits ##
beta <- 5
lapsiness <- .1 
value_mix_par <- .5
actor_LR <- .1 
q_LR <- .2
critic_LR <- .1
sim <- 1 # Do you want to simulate?
verbose <- 1 # Trial-wise print out?

## Loop through trials ##
for (tidx in seq_along(training_trials)) {
  sidx <- as.numeric(which(as.character(train_df$stimuli)[tidx]==states)) # State index
  state <- states[sidx] # Character repr of state 
  if (!is.null(verbose)) cat("\n ### Trial ", tidx, "####",
                             "\n ---State", state, "---")
  ## Mix values and find choice probs ## 
  mix_values <- MixACAndQVals(q_vals[sidx, ], a_weights[sidx, ], value_mix_par)
  left_prob_sm <- CalcSoftmaxProbLeft(mix_values, beta)
  left_full_prob <- MixLeftChoiceWRandom(left_prob_sm, lapsiness)
  
  if (sim) {
    ## Simulate choice and action.. 
    choice <- ifelse(left_full_prob < runif(1, 0, 1), "left", "right")
    action <- ifelse(choice=="left", 1, 2) # Just a numerical code for choice
    if (choice=="left") stim <- unlist(map(str_extract_all(state, boundary("character")), 1))
    if (choice=="right") stim <- unlist(map(str_extract_all(state, boundary("character")), 2))
    row_idx <- which(stim_set==stim)
    
    # .. and outcome ##
    p_nz <- key[row_idx, "p_nz"]
    pos_or_neg <- key[row_idx, "pos_or_neg"]
    outcome_str <- ifelse(p_rew < runif(1, 0, 1), "non-zero", "zero")
    outcome <- ifelse(outcome_str=="non_zero" & pos_or_neg==1, .05, -.05) 
  }
  if (!is.null(verbose)) cat("\n Mixed values ", unlist(mix_values),
                             "\n Full choice prob (softmax + undirected)", unlist(left_full_prob),
                             "\n Choose", choice,
                             "\n Probability of non-zero outcome", unlist(p_nz),
                             "\n Outcome", outcome)
  ## Learn based on result ## 
  # Critic who has RPEs just on state values.. 
  critic_out <- UpdateCriticValue(critic_dynamics["critic_values"], sidx, critic_LR, outcome)
  critic_PE <- critic_out["AC_PE"]
  # .. and actor who computes on s, a pairs but just has access to the critic's values 
  a_weights <- UpdateActorWeights(a_weights, sidx, action, actor_LR, critic_PE) 
  
  q_vals <- CalcQVals(q_vals, q_LR, sidx, action, outcome) 
  
  if (!is.null(verbose)) { cat("\nA-C|Q v \n")
    write.table(format(cbind(a_weights, q_vals), justify="right"),
                row.names=F, col.names=F, quote=F) }
  
  ## Package up outputs this trial ##
  data.table("state"=state,
             "choice"=choice,
             "outcome"=outcome)
}
