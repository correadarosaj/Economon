generate_random_market_v2 = function(


  condition = "",# just a label
    # SET DEFAULT VALUES
    n_trials = 100,# how long to run the simulation
    alpha_b0 = .01, alpha_s0 = .05, # baseline alpha values

    # proportional changes in alpha: when +, then alpha increases; when -, then alpha decreases
    alpha_b1 = -.1, alpha_s1 = 0,   # failed transaction
    alpha_b2 = .1, alpha_s2 = 0,     # successful transaction
    coef.var = 0.1
){

  require(tidyverse)

  #----------------------------------
  # SET INITIAL VALUES
  alpha_b = max(0.001,rnorm(alpha_b0,coef.var*alpha_b0))
  alpha_s = alpha_s0
  market = NULL
  time_since = 0 # time since last transaction
  run_number = 1

  #----------------------------------
  # SIMULATION
  for(i in 1:n_trials){


    # set price for this transaction
    C = calculate_price(alpha_s)

    # calculate probability of buying/selling
    theta_B = calculate_QB(C,alpha_B = alpha_b)/100
    theta_S = calculate_QS(C,alpha_S = alpha_s)/100

    # CURRENT TRANSACTION
    current_transaction = data.frame(
      # save important data
      condition = condition,
      trial = i,
      alpha_b0 = alpha_b0,
      alpha_s0 = alpha_s0,
      alpha_b1 = alpha_b1,
      alpha_b2 = alpha_b2,
      alpha_s1 = alpha_s1,
      market.price = C,
      buyer_alpha = alpha_b,
      seller_alpha = alpha_s,
      prob.buy = theta_B,
      prob.sell = theta_S,
      time_since = NA,
      run = NA) %>%
      # determine outcome of transaction
      mutate(buy = rbinom(1,1,prob.buy),   # randomly draw a 0 or 1 for whether buyer wants transaction to occur
             sell = rbinom(1,1,prob.sell), # randomly draw a 0 or 1 for whether seller wants transaction to occur
             prob.joint = prob.buy*prob.sell,
             transaction = ifelse(buy == 1 & sell == 1, # if both agree to make transaction
                                  1,  # then successful transaction
                                  0)) # else, failed transaction

    # determine how alphas change
    if(current_transaction$transaction == 1){ # SUCCESSFUL TRANSACTION
      alpha_b = alpha_b + alpha_b*alpha_b2
      alpha_s = alpha_s + alpha_s*alpha_s2
      current_transaction$time_since = time_since
      current_transaction$run = run_number

      time_since = 0
      run_number = run_number + 1
    }else{                                    # FAILED TRANSACTION
      alpha_b = alpha_b + alpha_b*alpha_b1
      alpha_s = alpha_s + alpha_s*alpha_s1
      time_since = time_since + 1
      run_number = run_number
    }

    # Controlling Market Explosion - Reset alphas
    if(C>100){
      C=50
      alpha_b = alpha_b0
      alpha_s = alpha_s0
    }
    # Bind current transaction with other transactions
    market = rbind(market, current_transaction)
  }
  return(market)
}
