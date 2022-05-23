n_t <- 50
n_s <- 10
n_c <- 10000

discount_c <- 0.03
discount_q <- 0.01

## All cause mortality
acm_20 <- 0.0046
acm_30 <- 0.0058
acm_40 <- 0.0108
acm_50 <- 0.0296
acm_60 <- 0.0844

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
  "Dead")

trans_mat <- array(NA_real_, dim = c(n_s, n_s, n_t), 
                   dimnames = list(from  = v_state_names, to    = v_state_names, cycle = 1:n_t))

trans_mat[2:10, 1, ] <- 0
trans_mat[10, 10, ] <- 1

trans_mat[1, 10,  1:10] <- acm_20
trans_mat[1, 10, 11:20] <- acm_30
trans_mat[1, 10, 21:30] <- acm_40
trans_mat[1, 10, 31:40] <- acm_50
trans_mat[1, 10, 41:50] <- acm_60

#Stage I

trans_mat[1, 2, ] <- 0.005641749
trans_mat[2:10, 2, ] <- 0
trans_mat[2, 10, 1:40] <- 0.03
trans_mat[2, 10, 41:50] <- 0.06
trans_mat[2, 3, 1:40] <- 0.97
trans_mat[2, 3, 41:50] <- 0.94

trans_mat[1, 3, ] <- 0
trans_mat[4:10, 3, ] <- 0
trans_mat[3, 10,  1:10] <- acm_20
trans_mat[3, 10, 11:20] <- acm_30
trans_mat[3, 10, 21:30] <- acm_40
trans_mat[3, 10, 31:40] <- acm_50
trans_mat[3, 10, 41:50] <- acm_60

trans_mat[3, 3, ] <- (1- trans_mat[3, 10, ])

#Stage II

#IC
trans_mat[1, 4, ] <- 0.004231312
trans_mat[2:10, 4, ] <- 0
trans_mat[4, 10, 1:40] <- 0.03
trans_mat[4, 10, 41:50] <- 0.06
trans_mat[4, 5, 1:40] <- 0.97
trans_mat[4, 5, 41:50] <- 0.94

#CC 
trans_mat[1:3, 5, ] <- 0
trans_mat[6:10, 5, ] <- 0
trans_mat[5, 10,  1:10] <- acm_20
trans_mat[5, 10, 11:20] <- acm_30
trans_mat[5, 10, 21:30] <- acm_40
trans_mat[5, 10, 31:40] <- acm_50
trans_mat[5, 10, 41:50] <- acm_60

#TC
trans_mat[5, 6, ] <- 0.019
trans_mat[1:4, 6, ] <- 0
trans_mat[6:10, 6, ] <- 0
trans_mat[6, 10, ] <- 1

trans_mat[5, 5, ] <- (1-(trans_mat[5, 6, ] + trans_mat[5, 10, ]))

#Stage III 
#IC
trans_mat[1, 7, ] <- 0.001410437
trans_mat[2:10, 7, ] <- 0
trans_mat[7, 10, 1:40] <- 0.03
trans_mat[7, 10, 41:50] <- 0.06
trans_mat[7, 8, 1:40] <- 0.97
trans_mat[7, 8, 41:50] <- 0.94

#CC
trans_mat[1:6, 8, ] <- 0
trans_mat[9:10, 8, ] <- 0
trans_mat[8, 10,  1:10] <- acm_20
trans_mat[8, 10, 11:20] <- acm_30
trans_mat[8, 10, 21:30] <- acm_40
trans_mat[8, 10, 31:40] <- acm_50
trans_mat[8, 10, 41:50] <- acm_60

trans_mat[8, 9, ] <- 0.069
trans_mat[1:7, 9, ] <- 0
trans_mat[9:10, 9, ] <- 0
trans_mat[9, 10, ] <- 1

trans_mat[8, 8, ] <- (1-(trans_mat[8, 9, ] + trans_mat[8, 10, ]))

trans_mat[1 , 1, ] <- (1- (trans_mat[1, 10, ] + trans_mat[1, 2, ] + trans_mat[1, 4, ] + trans_mat[1, 7, ]))

state_membership_1 <- array(NA_real_,
                          dim = c(n_t, n_s),
                          dimnames = list(cycle = 1:n_t,
                                          state = v_state_names))
state_membership_1[1, ] <- c(n_c, 
                           0, 0,
                           0, 0, 0,
                           0, 0, 0,
                           0)
for (i in 2:n_t) {
  state_membership_1[i, ] <- state_membership_1[i - 1, ] %*% trans_mat[, , i - 1]
}

payoffs_1 <- matrix(c(
  c_colo, 
  c_surgeryI, c_colo,
  c_surgeryII, c_colo, c_palliative,
  (c_surgeryIII+c_chemo), c_colo, c_palliative,
  0,
  
  0.95, 
  0.88, 0.95,
  0.82, 0.95, -0.65,
  0.76, 0.76, -0.46,
  0),
  nrow = n_s, ncol = 2, byrow = FALSE,
  dimnames = list(state  = v_state_names,
                  payoff = c("Cost", "QALY")))
payoff_trace_1 <- array(NA_real_,
                      dim = c(n_t, 2),
                      dimnames = list(cycle  = 1:n_t,
                                      payoff = c("Cost", "QALY")))
for (i in 1:n_t) {
  payoff_trace_1[i, ] <- state_membership_1[i, ] %*% payoffs_1[ , ]
}

discount_factors_1 <- array(NA_real_,
                      dim = c(n_t, 2),
                      dimnames = list(cycle  = 1:n_t,
                                      payoff = c("Cost", "QALY")))
for (i in 1:n_t) {
  discount_factors_1[i, ] <- c(
    (1 /(1 + discount_c))^(i-1), 
    (1 /(1 + discount_q))^(i-1)
  )
}

discounted_payoffs_1 <- array(NA_real_,
                          dim = c(n_t, 2),
                          dimnames = list(cycle  = 1:n_t,
                                          payoff = c("Cost", "QALY")))
for (i in 1:n_t) {
  discounted_payoffs_1[i, ] <- payoff_trace_1[i,  ] * discount_factors_1 [i, ]
}

ppp_1 <- colSums(discounted_payoffs_1) / n_c

x2=seq(0,48,2)
x1=seq(0,49,1)
plot(x1,(cumsum(discounted_payoffs_1[,1]))/1000000, 
     pch = 16, cex = 0.75,
     ylab = "Cost [Million SEK]" ,
     xlab = "Time [Years]",
     )
matpoints(x2,(cumsum(discounted_payoffs_2[,1]))/ 1000000 , pch = 18)
legend(30, 1000, legend = c("1-year","2-year"), cex=0.8, pch = c(16, 18)
       )

plot(x1,(cumsum(discounted_payoffs_1[,2])), 
     pch = 16, cex = 0.75,
     ylab = "QALY" ,
     xlab = "Time [Years]",
)
points(x2,(cumsum(discounted_payoffs_2[,2])), pch = 18)
legend(30, 100, legend = c("1-year","2-year"), cex=0.8, pch = c(16, 18)
)

