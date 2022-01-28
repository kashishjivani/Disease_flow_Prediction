no_cycle <- 20
no_state <- 7
no_custome <- 2000

state_names <- c("LWSPC", "UC","ECC","RFPC","ESC","LWUDAPQ","Dead")


# --------------------------------------------SF------------------------------------------------------------------------------------------
tr_pr_m_sf <- matrix(c(
                0.2 ,0.1	,0.2	,0.22	,0.1	,0.1	,0.08, 
                0	,0.22	,0.12	,0.15	,0.23	,0.16	,0.12,
                0	,0	    ,0.23	,0.15	,0.26	,0.13	,0.23
               ,  0	,0    	,0	    ,0.25	,0.14	,0.23	,0.38
               , 0	,0    	,0	    ,0	    ,0.19	,0.36	,0.45
              ,    0	,0    	,0	    ,0	    ,0	    ,0.45	,0.55
               ,   0	,0    	,0	    ,0	    ,0	    ,0	    ,1), nrow = 7, ncol = 7, byrow = TRUE,dimnames = list(from = state_names,to = state_names))

membership_in_state_sf <- array(NA_real_,
                          dim = c(no_cycle, no_state),
                          dimnames = list(cycle = 1:no_cycle,
                                          state = state_names))
membership_in_state_sf[1, ] <- c(no_custome,0,0,0, 0, 0,0)

for (i in 2:no_cycle) {
  membership_in_state_sf[i, ] <- membership_in_state_sf[i - 1, ] %*% tr_pr_m_sf
}


m_payoffs_sf <- matrix(c(5000,15000,15000,5000,20000,5000,0,
                      2,2.5,2	, 4,5, 6, 0),
                    nrow = 7, ncol = 2, byrow = FALSE,
                    dimnames = list(state = state_names,
                                    payoff = c("Cost", "QALM")))
payoff_trace_sf <- membership_in_state_sf %*% m_payoffs_sf



#------------------------------------------------------------NAT-----------------------------------------------------------------------------

m_P_nat <- matrix(c(
                0.1,0.15,	0.18,	0.16,	0.18,	0.2	,    0.03,
                0,	0.18,	0.16,	0.15,	0.18,	0.15,	0.18,
                0,	0,  	0.2,	0.21,	0.12,	0.2	 ,   0.27,
                0,	0,	    0,	    0.23,	0.15,	0.23,	0.39,
                0,	0,	    0,	    0   ,	0.29,	0.25,	0.46,
                0,	0,	    0,	    0	,   0	 ,   0.35,	0.65,
                0,	0,	    0,	    0   ,	0	 ,   0	  ,  1
), nrow = 7, ncol = 7, byrow = TRUE,dimnames = list(from = state_names,
                              to = state_names))

membership_in_state_nat <- array(NA_real_,
                          dim = c(no_cycle, no_state),
                          dimnames = list(cycle = 1:no_cycle,
                                          state = state_names))
  membership_in_state_nat [1, ] <- c(2000,0,0,0,0, 0,0)
# membership_in_state_nat 
for (i in 2:no_cycle) {
  membership_in_state_nat [i, ] <- membership_in_state_nat [i - 1, ] %*% m_P_nat
}
# membership_in_state_nat 


m_payoffs_nat <- matrix(c(5000,15000,15000,5000,20000,5000,0,
                      2,3,4	, 5,7, 6, 0),
                    nrow = 7, ncol = 2, byrow = FALSE,
                    dimnames = list(state = state_names,
                                    payoff = c("Cost", "QALM")))
                                    
 cat("For SF\n")

 m_payoffs_sf
 
 cat("\n\nFor NAT\n")

  m_payoffs_nat

payoff_trace_nat <- membership_in_state_nat  %*% m_payoffs_nat
# payoff_trace_nat





cat("\n\n\n\nFor SF\n")
colSums(payoff_trace_sf) / no_custome


cat("\n\nFor NAT\n")
colSums(payoff_trace_nat) / no_custome