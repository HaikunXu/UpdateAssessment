library(r4ss)
library(tidyverse)

Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
JitterDir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/Jitter/"
model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

numjitter <- 10
dir.create(JitterDir)
converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

for (m in 4:6) {
  for (s in 1:1) {
    if(converge[m,s]) {
      Path <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
      JitterPath <- paste0(JitterDir,model[m],"-",toString(steepness[s]),"/")
      
      # unlink(JitterPath, recursive = TRUE, force = TRUE)
      dir.create(JitterPath)
      files = c(
        paste0(Path, "/go_nohess.bat"),
        paste0(Path, "/starter.ss"),
        paste0(Path, "/forecast.ss"),
        paste0(Path, "/BET-EPO.ctl"),
        paste0(Path, "/BET-EPO.dat"),
        paste0(Path, "/ss.exe"),
        paste0(Path, "/ss.par")
      )
      file.copy(from = files, to = JitterPath)
      
      # start from the par file
      # starterFile <- readLines(paste0(JitterPath, "/starter.ss"), warn = F)
      # starterFile[6] <- toString(1) # start from initial condition
      # writeLines(starterFile, paste0(JitterPath, "/starter.ss"))
      
      jit.likes <- SS_RunJitter(
        mydir = JitterPath,
        Njitter = numjitter,
        extras = "-nox -cbs 4000000000 -gbs 4000000000 -ams 400000000 -maxfn 20000 -nohess",
        jitter_fraction = 0.05, init_values_src = 1)
      
      # compare likelihood
      profilemodels <- SSgetoutput(dirvec = JitterPath, keyvec = 1:numjitter, getcovar = FALSE)
      # summarize output
      profilesummary <- SSsummarize(profilemodels)
      # Likelihoods
      Tot_likelihood <- as.numeric(profilesummary[["likelihoods"]][1, 1:numjitter])
      
      myreplist <- SS_output(dir=Path,ncols=400,covar=F,verbose = FALSE, printstats = FALSE)
      
      NLL <- data.frame("Jitter"=1:numjitter, 
                        "NLL"=Tot_likelihood, 
                        "NLL_Diff"=sign(Tot_likelihood-myreplist$likelihoods_used$values[1]))
      
      f <- ggplot(data=NLL) +
        geom_point(aes(x=Jitter,y=NLL,color=factor(NLL_Diff))) +
        geom_hline(yintercept = myreplist$likelihoods_used$values[1])
      ggsave(f,file=paste0(JitterDir,model[m],"-",toString(steepness[s]),".png"),width = 8,height=6)
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
