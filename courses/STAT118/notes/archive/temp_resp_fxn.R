calc_Pg_fxn_T_Tk_rowwise <- function(temp, T_k, Pgmax){
  out_P <- Pgmax*(1-exp(-temp/T_k))
  return(out_P)
}

f_H <- function(Tv, delta_S, delta_Hd, R_gas = 8.314) {
  (1 + exp((298.15 * delta_S - delta_Hd) / (298.15 * R_gas))) /
    (1 + exp((delta_S * (Tv+273.15) - delta_Hd) / (R_gas * (Tv+273.15))))
}

#total temp response is equal to A*calc_Pg_fxn_T_Tk_rowwise*f_H