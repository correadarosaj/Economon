calculate_QS = function(C, Q0_B = 100, alpha_S = .005){
  Z = 1/C
  y= log10(Q0_B) * exp(-alpha_S*Q0_B*Z)
  return(10^y)
}
