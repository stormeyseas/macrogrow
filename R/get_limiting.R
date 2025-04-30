#' Determine limiting factor in growth
#' 
#' @description
#' For each timestep in the growth output matrix, determine the factor most likely to be limiting growth.
#'
#' @param output matrix, direct output of 'grow_macroalgae' function (can be sparse)
#' @param spec_params a named vector of site-specific parameters - same as input to 'grow_macroalgae' function
#'
#' @return vector of limiting factors at each timestep
#' @export 
#' 
#' @details
#' This function gives an idea of what factor(s) limit growth at each timestep. The choices (in order) are:
#' 1. T_lim
#' 2. I_lim
#' 3. S_lim
#' 4. Q_lim - internal nutrient quotient
#' 5. Ns_to_Nf - not enough stored nutrients (Ns) were available to be fixed at maximum rate
#' 6. Nf_loss - more Nf was lost than fixed
#' 7. conc_nitrate/conc_ammonium/conc_other - there were not enough ambient nutrients to be taken up at the maximum rate
#' 8. Ns_fixed_not_replaced - the Ns fixed last timestep was not replaced this timestep (Q_rel is declining)
#' 9. Ns_loss_not_replaced - the Ns lost last timestep was not replaced this timestep (Q_rel is declining)
#'
#' @seealso [grow_macroalgae()]
get_limiting <- function(output, spec_params) {
  
  limiting <- rep(NA, nrow(output))
  first_lim <- second_lim <- NA

  for (i in 1:nrow(output)) {
    # growth_rate[i]  <- unname(spec_params['mu'] * min(T_lim[i], I_lim[i], S_lim[i]) * Q_lim[i])
    
    if (output[i,'growth_rate']/spec_params['mu'] == 1) {
      limiting[i] <- "No_limit"
    } else {
      # 1. Temperature, light, salinity, or nutrients
      first_lim <- min(output[i,'T_lim'], output[i,'I_lim'], output[i,'S_lim'], output[i,'Q_lim'])
      first_lim <- which(c(output[i,'T_lim'], output[i,'I_lim'], output[i,'S_lim'], output[i,'Q_lim']) == first_lim)
      first_lim <- names(first_lim)
      
      # 2. Internal nutrients (ie there's not enough Ns to fix to Nf)
      second_lim_1 <- ifelse(output[i,'Ns_to_Nf'] > output[i, 'Ns'], "Ns_to_Nf", NA)

      # 3. Loss (ie more Nf is lost than fixed)
      second_lim_2 <- ifelse(output[i,'Nf_loss'] > output[i, 'Ns_to_Nf'], "Nf_loss", NA)
      
      # 4. Nitrogen uptake (ie not enough nitrogen was available to be taken up last step)
      check_conc <- c(
        output[i-1,'conc_nitrate'] - output[i-1,'up_Ni'],
        output[i-1,'conc_ammonium'] - output[i-1,'up_Am'],
        output[i-1,'conc_other'] - output[i-1,'up_Ot']
      )
      second_lim_3 <- ifelse(any(check_conc < 0, na.rm = T), paste(names(check_conc)[which(check_conc < 0)], collapse = " & "), NA)
      
      # 5. Nitrogen uptake (ie not enough nitrogen was taken up last step to replace fixed Ns)
      uptake <- sum(c(output[i-1,'up_Ni'], output[i-1,'up_Am'], output[i-1,'up_Ot']), na.rm = T)
      second_lim_4 <- ifelse(output[i,'Ns_to_Nf'] > uptake, "Ns_fixed_not_replaced", NA)

      # 6. Nitrogen uptake (ie not enough nitrogen was taken up last step to replace lost Ns)
      second_lim_5 <- ifelse(output[i,'Ns_loss'] > uptake, "Ns_loss_not_replaced", NA)
      
      second_lim <- c(second_lim_1, second_lim_2, second_lim_3, second_lim_4, second_lim_5)
      second_lim <- second_lim[!is.na(second_lim)]
      second_lim <- paste(second_lim, collapse = " & ")
        
      lims <- c(first_lim, second_lim)
      lims <- lims[!is.na(lims)]
      limiting[i] <- paste(lims, collapse = " & ")
      second_lim <- second_lim_1 <- second_lim_2 <- second_lim_3 <- second_lim_4 <- second_lim_5 <- NA
    }
  }
  return(limiting)
}