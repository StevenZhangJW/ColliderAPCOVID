# # R code bias analysis
# Jiawei Zhang 
# Under the project of "Association between Long-Term Exposure to Air Pollution and COVID-19 Mortality: Investigation of potential collider bias"


RHRcal <- function(HR1, HR2 ,se1, se2, Scor=NULL){
  # HR1 indicate the beta obtained from the sampling population
  # HR2 indicate the beta obtained from the source population
  RHR <- HR1/HR2
  # rse1 is the standard error estimated by assuming a correlation matrix
  if(!is.null(Scor)){
  rse1 <- sqrt(se1^2+se2^2-2*Scor*se1*se2)
  } else {rse1 = NA}
  
  # rse2 is the standard error estimated using Nohr's equation methods, see (Nohr EA, Frydenberg M, Henriksen TB, Olsen J. 2006. Does low participation in cohort studies induce bias? Epidemiology 17.)
  rse2 <- sqrt(se1^2-se2^2)
  
  res <- as.data.frame(matrix(0,1,3))
  names(res) <- c("Relative Hazard Ratio","Standard Error 1","Standard Error 2")
  res[1] <- RHR
  res[2] <- rse1
  res[3] <- rse2
  
  return(res)
}


RHR_bs <- function(Simresults){
  RHR_res  <- Simresults %>%
  group_by(Population, Pollutant) %>%
  summarise(RHR_median=median(RHR),
            RHR_lowerCI=quantile(RHR,0.025),
            RHR_upperCI=quantile(RHR,0.975))
  return(RHR_res)
 }
