library(IATTCassessment)

Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
ImpactDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/ImpactPlot/"
SSDir <- "D:/OneDrive - IATTC/Git/UpdateAssessment/Document/Kobe/"

n_fishery=23
BaseName = "Base"
model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
dir.create(ImpactDir)

for (m in 1:12) {
  Path <- paste0(Dir,model[m],"-1/")
  ImpactPath <- paste0(ImpactDir,model[m],"-1/")
  dir.create(ImpactPath)
  dir.create(paste0(ImpactPath,"Base"))
  # create a base folder
  files = c(
    paste0(Path, "/go_nohess.bat"),
    paste0(SSDir, "/starter.ss"),
    paste0(Path, "/forecast.ss"),
    paste0(Path, "/BET-EPO.ctl"),
    paste0(Path, "/data.ss_new"),
    paste0(Path, "/ss.exe"),
    paste0(Path, "/ss.par"),
    paste0(Path, "/Report.sso"),
    paste0(Path, "/CompReport.sso"))
  file.copy(from = files, to = paste0(ImpactPath,"Base"))
  
  file.rename(paste0(ImpactPath,"Base/data.ss_new"),
              paste0(ImpactPath,"Base/BET-EPO.dat"))
  
  n_year <- ifelse(m %in% seq(9,12), 180-100, 180-16)
  if(m==1) f1 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
  if(m==2) f2 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
  if(m==3) f3 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
  if(m==4) f4 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
  if(m==5) f5 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
  if(m==6) f6 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
  if(m==7) f7 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
  if(m==8) f8 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
  if(m==9) f9 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
  if(m==10) f10 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
  if(m==11) f11 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
  if(m==12) f12 <- impact_plot(Dir=ImpactPath, n_year=n_year, BaseName = BaseName, n_fishery = n_fishery,title=model[m])
}

F1 <- f1 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()
F2 <- f2 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()
F3 <- f3 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()
F4 <- f4 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()
F5 <- f5 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()
F6 <- f6 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()
F7 <- f7 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()
F8 <- f8 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()
F9 <- f9 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()
F10 <- f10 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()
F11 <- f11 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()
F12 <- f12 + coord_cartesian(ylim = c(0,1e6),xlim=c(1978,2021),expand = FALSE) + xlab("") + ylab("") + ggeasy::easy_center_title()

F_all <- gridExtra::grid.arrange(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,nrow = 4)
ggsave(F_all, file = paste0(ImpactDir,"ImpactPlot.png"), width = 10, height = 12)
ggsave(F_all, file = paste0(ImpactDir,"ImpactPlot.eps"), width = 10, height = 12)