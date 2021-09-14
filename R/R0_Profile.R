library(r4ss)

Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
R0Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/R0/"
model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

dir.create(R0Dir)
converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

for (m in 1:1) {
  for (s in 1:1) {
    if(converge[m,s]) {
      Path <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
      R0Path <- paste0(R0Dir,model[m],"-",toString(steepness[s]),"/")
      
      # unlink(JitterPath, recursive = TRUE, force = TRUE)
      dir.create(R0Path)
      
      myreplist <- SS_output(dir=Path,ncols=400,covar=F,verbose = FALSE, printstats = FALSE)
      R0 <- myreplist$parameters$Value[which(myreplist$parameters$Label=="SR_LN(R0)")] 
      
      r0 <- c(R0-0.8,R0-0.4,R0,R0+0.4,R0+0.8)
      
      for(r in r0) {
        r0Path <- paste0(R0Path,toString(round(r,2)))
        dir.create(r0Path)
        
        files = c(
          paste0(Path, "/go_nohess.bat"),
          paste0(Path, "/starter.ss"),
          paste0(Path, "/forecast.ss"),
          paste0(Path, "/control.ss_new"),
          paste0(Path, "/BET-EPO.dat"),
          paste0(Path, "/ss.exe")
        )
        file.copy(from = files, to = r0Path, overwrite = TRUE)
        
        if(r==R0) {
          file.rename(paste0(r0Path, "/control.ss_new"),
                      paste0(r0Path, "/BET-EPO.ctl"))
        }
        else {
          dat <- SS_readdat_3.30(file = paste0(r0Path, "/BET-EPO.dat"), verbose = FALSE)
          ctl <- SS_readctl_3.30(
            file = paste0(r0Path, "/control.ss_new"),
            verbose = FALSE,
            datlist = dat,
            use_datlist = TRUE
          )
          
          ctl$SR_parms$INIT[1] <- r
          SS_writectl_3.30(ctl,outfile = paste0(r0Path, "/BET-EPO.ctl"),overwrite = TRUE,verbose = FALSE)
        }
        # run the model
        
        setwd(r0Path)
        print(r0Path)
        
        command <- paste("cd", r0Path, "& go_noHess.bat", sep = " ")
        ss <- shell(cmd = command, intern = T, wait = T)
        
        # check the max gradient
        myreplist <- SS_output(dir=r0Path,ncols=400,covar=F,verbose = FALSE, printstats = FALSE)
        print(paste0("Max gradient = ",myreplist$maximum_gradient_component))
        
      }
    }
  }
}
