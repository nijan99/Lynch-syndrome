n_t <- 25
n_s <- 11
n_c <- 10000

discount_c <- 0.03
discount_q <- 0.01

## All cause mortality 2 year
acm_20_2 <- 1 - (1-0.0046)^2
acm_30_2 <- 1 - (1-0.0058)^2
acm_40_2 <- 1 - (1-0.0108)^2
acm_50_2 <- 1 - (1-0.0296)^2
acm_60_2 <- 1 - (1-0.0844)^2

## Costs
c_colo <- 9715.134
c_surgeryI <- 138797.3
c_surgeryII <- 166991.6
c_surgeryIII <- 244576.8
c_chemo <- 12277.77
c_palliative <- 918700.47

v_state_names <- c(
  "LS",
  "IC I", "CC I",
  "IC II", "CC II", "TC II",
  "IC III", "CC III", "TC III",
  "Dead",  "Death from surgery")

trans_mat_2 <- array(NA_real_, dim = c(n_s, n_s, n_t), 
                   dimnames = list(from  = v_state_names, to    = v_state_names, cycle = 1:n_t))

trans_mat_2[2:10, 1, ] <- 0
trans_mat_2[10, 10, ] <- 1

trans_mat_2[1, 10, 1:5] <- acm_20_2
trans_mat_2[1, 10, 6:10] <- acm_30_2
trans_mat_2[1, 10, 11:15] <- acm_40_2
trans_mat_2[1, 10, 16:20] <- acm_50_2
trans_mat_2[1, 10, 21:25] <- acm_60_2

#Stage I

trans_mat_2[1, 2, ] <- 0.004901961
trans_mat_2[2:10, 2, ] <- 0
trans_mat_2[2, 11, 1:20] <- 0.03
trans_mat_2[2, 11, 21:25] <- 0.06
trans_mat_2[2, 3, 1:20] <- 0.97
trans_mat_2[2, 3, 21:25] <- 0.94
trans_mat_2[2, 10, ] <- 0

trans_mat_2[1, 3, ] <- 0
trans_mat_2[4:10, 3, ] <- 0
trans_mat_2[3, 10,  1:5 ] <- acm_20_2
trans_mat_2[3, 10,  6:10] <- acm_30_2
trans_mat_2[3, 10, 11:15] <- acm_40_2
trans_mat_2[3, 10, 16:20] <- acm_50_2
trans_mat_2[3, 10, 21:25] <- acm_60_2

trans_mat_2[3, 3, ] <- (1- trans_mat_2[3, 10, ])

#Stage II

#IC
trans_mat_2[1, 4, ] <- 0.006535948
trans_mat_2[2:10, 4, ] <- 0
trans_mat_2[4, 11, 1:20] <- 0.03
trans_mat_2[4, 11, 21:25] <- 0.06
trans_mat_2[4, 5, 1:20] <- 0.97
trans_mat_2[4, 5, 21:25] <- 0.94
trans_mat_2[4, 10, ] <- 0

#CC 
trans_mat_2[1:3, 5, ] <- 0
trans_mat_2[6:10, 5, ] <- 0
trans_mat_2[5, 10,  1:5 ] <- acm_20_2
trans_mat_2[5, 10,  6:10] <- acm_30_2
trans_mat_2[5, 10, 11:15] <- acm_40_2
trans_mat_2[5, 10, 16:20] <- acm_50_2
trans_mat_2[5, 10, 21:25] <- acm_60_2

#TC
trans_mat_2[5, 6, ] <- 1 - (1 - 0.019)^2
trans_mat_2[1:4, 6, ] <- 0
trans_mat_2[6:10, 6, ] <- 0
trans_mat_2[6, 10, ] <- 1

trans_mat_2[5, 5, ] <- (1-(trans_mat_2[5, 6, ] + trans_mat_2[5, 10, ]))

#Stage III 
#IC
trans_mat_2[1, 7, ] <- 0.006535948
trans_mat_2[2:10, 7, ] <- 0
trans_mat_2[7, 10, 1:20] <- 0.03
trans_mat_2[7, 10, 21:25] <- 0.06
trans_mat_2[7, 8, 1:20] <- 0.97
trans_mat_2[7, 8, 21:25] <- 0.94

#CC
trans_mat_2[1:6, 8, ] <- 0
trans_mat_2[9:10, 8, ] <- 0
trans_mat_2[8, 10,  1:5 ] <- acm_20_2
trans_mat_2[8, 10,  6:10] <- acm_30_2
trans_mat_2[8, 10, 11:15] <- acm_40_2
trans_mat_2[8, 10, 16:20] <- acm_50_2
trans_mat_2[8, 10, 21:25] <- acm_60_2

##TC
trans_mat_2[8, 9, ] <- 1 - (1 - 0.069)^2
trans_mat_2[1:7, 9, ] <- 0
trans_mat_2[9:10, 9, ] <- 0
trans_mat_2[9, 10, ] <- 1

trans_mat_2[8, 8, ] <- (1-(trans_mat_2[8, 9, ] + trans_mat_2[8, 10, ]))

trans_mat_2[1 , 1, ] <- (1- (trans_mat_2[1, 10, ] + trans_mat_2[1, 2, ] + trans_mat_2[1, 4, ] + trans_mat_2[1, 7, ]))

## Death from surgery tunnel state
trans_mat_2[11, 10, ] <- 1
trans_mat_2[1,  11, ] <- 0
trans_mat_2[3,  11, ] <- 0
trans_mat_2[5:10,11,] <- 0
trans_mat_2[11, 1:9,] <- 0
trans_mat_2[11, 11, ] <- 0

state_membership_2 <- array(NA_real_,
                          dim = c(n_t, n_s),
                          dimnames = list(cycle = 1:n_t,
                                          state = v_state_names))
state_membership_2[1, ] <- c(n_c, 
                           0, 0,
                           0, 0, 0,
                           0, 0, 0,
                           0, 0)
for (i in 2:n_t) {
  state_membership_2[i, ] <- state_membership_2[i - 1, ] %*% trans_mat_2[, , i - 1]
}

payoffs_2 <- matrix(c(
  c_colo, 
  (c_surgeryI + c_colo), 2*c_colo,
  (c_surgeryII + c_colo), 2*c_colo, (c_colo+c_palliative),
  (c_surgeryIII + c_chemo + c_colo), 2*c_colo, (c_colo+c_palliative),
  0, 0,
  
  (0.95 * 2), 
  (0.88 + 0.95), (0.95 * 2),
  (0.82 + 0.95), (0.95 * 2), -1.6,
  (0.76 * 2), (0.76 * 2), -1.22,
  0, -0.95),
  nrow = n_s, ncol = 2, byrow = FALSE,
  dimnames = list(state  = v_state_names,
                  payoff = c("Cost", "QALY")))

payoff_trace_2 <- array(NA_real_,
                      dim = c(n_t, 2),
                      dimnames = list(cycle  = 1:n_t,
                                      payoff = c("Cost", "QALY")))
for (i in 1:n_t) {
  payoff_trace_2[i, ] <- state_membership_2[i, ] %*% payoffs_2[ , ]
}

discount_factors_2 <- array(NA_real_,
                            dim = c(n_t, 2),
                            dimnames = list(cycle  = 1:n_t,
                                            payoff = c("Cost", "QALY")))
for (i in 1:n_t) {
  discount_factors_2[i, ] <- c(
    (1 /(1 + discount_c))^(2 * (i-1)), 
    (1 /(1 + discount_q))^(2 * (i-1))
    )
}

discounted_payoffs_2 <- array(NA_real_,
                              dim = c(n_t, 2),
                              dimnames = list(cycle  = 1:n_t,
                                              payoff = c("Cost", "QALY")))
for (i in 1:n_t) {
  discounted_payoffs_2[i, ] <- payoff_trace_2[i, ] * discount_factors_2[i, ]
}

ppp_2 <- colSums(discounted_payoffs_2) / n_c