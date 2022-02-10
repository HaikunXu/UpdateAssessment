library(r4ss)
library(tidyverse)
library(ss3diags)

Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

for (m in 1:1) {
  for (s in 1) {
    if(converge[m,s]) {
      Path <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
      
      myreplist = r4ss::SS_output(dir = Path, ncols = 500, covar = F, verbose = FALSE, printstats = FALSE)
      
      sspar(mfrow = c(1, 2), plot.cex = 0.8)
      rt = SSplotRunstest(myreplist, add = T, legendcex = 0.8, verbose = F)
      
      # sspar(mfrow = c(4, 4), plot.cex = 0.5)
      # rt = SSplotRunstest(myreplist, add = T, legendcex = 0.8, subplot = "len", verbose = F)
    }
  }
}
# 
# profilemodels <- SSgetoutput(dirvec = JitterPath, keyvec = 1:numjitter, getcovar = FALSE)
# # summarize output
# profilesummary <- SSsummarize(profilemodels)
# # Likelihoods
# Tot_likelihood <- as.numeric(profilesummary[["likelihoods"]][1, 1:numjitter])
# # Parameters
# a <- profilesummary[["pars"]]
# 
# 
# pick <- which(Tot_likelihood-min(Tot_likelihood,na.rm = TRUE)<10)
# profilemodels2 <- SSgetoutput(dirvec = JitterPath, keyvec = pick, getcovar = FALSE)
# profilesummary2 <- SSsummarize(profilemodels2)
# 
# SSplotComparisons(profilesummary2,legendlabels=pick,print = TRUE,plotdir = JitterPath)
