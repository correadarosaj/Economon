calculate_QB = function(C, Q0_B = 100, alpha_B = .0005){
  y= log10(Q0_B) * exp(-alpha_B*Q0_B*C)
  return(10^y)
}
