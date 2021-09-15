library(IATTCassessment)
library(r4ss)

Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
FstdDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(Fstd)/"
FlimitDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(Flimit)/"
dMSYDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(dmsy)/"
model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")

steepness <- seq(1,0.7,-0.1)
converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0
fyear <- 1979 # first year
lyear <- 2019 # last year
FFleets <- c(1:23) # fishery fleets
STD_only <- TRUE # no Kobe table is generated

for (m in 1:3) {
  if(m %in% 9:12) fyear <- 2000 # first year
  for (s in 1:1) {
    BasePath <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
    FstdPath <- paste0(FstdDir,model[m],"-",toString(steepness[s]),"/")
    FlimitPath <- paste0(FlimitDir,model[m],"-",toString(steepness[s]),"/")
    DynamicPath <- paste0(dMSYDir,model[m],"-",toString(steepness[s]),"/")
    KobePath=""
    print(BasePath)
    if(converge[m,s]) {
      Kobe.Out <- make_kobetable_SAC11(BasePath,KobePath="",FFleets,STD_only=TRUE,newSS=TRUE,FstdPath = FstdPath,FlimitPath = FlimitPath, DynamicPath = DynamicPath)
      if(m==1&s==1) {
        Mgmt <- data.frame("F_low"=Kobe.Out$STD$FrecentFmsy[1],
                           "F"=Kobe.Out$STD$FrecentFmsy[2],
                           "F_high"=Kobe.Out$STD$FrecentFmsy[3],
                           "SB_low"=Kobe.Out$STD$SB[1],
                           "SB"=Kobe.Out$STD$SB[2],
                           "SB_high"=Kobe.Out$STD$SB[3],
                           "Model"=model[m],
                           "Steepness"=steepness[s])
      }
      else {
        Mgmt <- rbind(Mgmt,data.frame("F_low"=Kobe.Out$STD$FrecentFmsy[1],
                                      "F"=Kobe.Out$STD$FrecentFmsy[2],
                                      "F_high"=Kobe.Out$STD$FrecentFmsy[3],
                                      "SB_low"=Kobe.Out$STD$SB[1],
                                      "SB"=Kobe.Out$STD$SB[2],
                                      "SB_high"=Kobe.Out$STD$SB[3],
                                      "Model"=model[m],
                                      "Steepness"=steepness[s]))
      }
    }
  }
}

Mgmt$Steepness <- as.factor(Mgmt$Steepness)

write.csv(Mgmt,file=paste0(Dir,"Mgmt.csv"),row.names = FALSE)


d=data.frame(x1=c(0,0,1,1), x2=c(1,1,4,4), y1=c(0,1,1,0), y2=c(1,4,4,1), r=c(1,2,3,4))
limit <- data.frame("F"=1.787855, "SB"=0.353868)

Kobe <- ggplot(data=Mgmt) +
  geom_rect(data=d %>% filter(r==1), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="yellow", color="white", alpha=0.5) +
  geom_rect(data=d %>% filter(r==2), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="red", color="white", alpha=0.5) +
  geom_rect(data=d %>% filter(r==3), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="yellow",color="white", alpha=0.5) +
  geom_rect(data=d %>% filter(r==4), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),  fill="green",color="white", alpha=0.5) +
  geom_point(aes(x=SB,y=F,color=Steepness,shape=Steepness),stroke = 2,size=2) +
  scale_shape_manual(values=c(1,2,4,5)) +
  geom_errorbar(aes(x=SB,ymin=F_low, ymax=F_high,color=Steepness),width=0.01) +
  geom_errorbarh(aes(xmin=SB_low,xmax=SB_high,y=F,color=Steepness),height=0.01) +
  geom_hline(yintercept = 1.787855,linetype="dashed",size=1) +
  geom_vline(xintercept = 0.353868,linetype="dashed",size=1) +
  scale_colour_manual(values = c("cyan","deepskyblue","blue","darkblue")) + # https://www.r-graph-gallery.com/ggplot2-color.html
  theme_bw(18) +
  xlab("S/Smsy") +
  ylab("F/Fmsy") +
  coord_cartesian(xlim = c(0,3), ylim = c(0,3), expand = FALSE)

ggsave(file = paste0(Dir, "Kobe plot.png"), width = 10, height = 8)
ggsave(file = paste0(Dir, "Kobe plot.eps"), width = 10, height = 8, device = cairo_ps)
save(Kobe,file = paste0(Dir,"Kobe.RData"))


### MSY and SMSY
Dir_Kobe <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/Kobe/"
for (m in 1:3) {
  if(m %in% 9:12) fyear <- 2000 # first year
  else fyear <- 1979
  for (s in 1:1) {
    Path <- paste0(Dir_Kobe,model[m],"-",toString(steepness[s]),"/")
    # print(Path)
    if(converge[m,s]) {
      Kobe <- read.csv(paste0(Path,"KobePlotOut.csv"))
      if(m==1&s==1) {
        MSY_ts <- data.frame("MSY"=as.numeric(Kobe[1,2:ncol(Kobe)]),
                             "Year"=seq(fyear+2,lyear),
                             "Model"=model[m],
                             "Steepness"=steepness[s])
        SMSY_ts <- data.frame("SMSY"=as.numeric(Kobe[3,2:ncol(Kobe)]),
                              "Year"=seq(fyear+2,lyear),
                              "Model"=model[m],
                              "Steepness"=steepness[s])
      }
      else {
        MSY_ts <- rbind(MSY_ts,data.frame("MSY"=as.numeric(Kobe[1,2:ncol(Kobe)]),
                                          "Year"=seq(fyear+2,lyear),
                                          "Model"=model[m],
                                          "Steepness"=steepness[s]))
        SMSY_ts <- rbind(SMSY_ts,data.frame("SMSY"=as.numeric(Kobe[3,2:ncol(Kobe)]),
                                            "Year"=seq(fyear+2,lyear),
                                            "Model"=model[m],
                                            "Steepness"=steepness[s]))
      }
    }
  }
}

MSY_ts$Steepness <- factor(MSY_ts$Steepness)
SMSY_ts$Steepness <- factor(SMSY_ts$Steepness)

f1 <- ggplot(data=MSY_ts %>% filter(Steepness==1)) +
  geom_line(aes(x=Year,y=MSY,color=Model)) +
  # geom_point(aes(x=Year,y=F_mult,shape=Steepness,color=Model)) +
  # geom_hline(yintercept=0,linetype="dashed") +
  theme_bw(15) +
  ylab("MSY (tons)") +xlab("") +
  coord_cartesian(ylim=c(0,4e5),expand = FALSE)


f2 <- ggplot(data=SMSY_ts %>% filter(Steepness==1)) +
  geom_line(aes(x=Year,y=SMSY,color=Model)) +
  # geom_point(aes(x=Year,y=F_mult,shape=Steepness,color=Model)) +
  # geom_hline(yintercept=0,linetype="dashed") +
  theme_bw(15) +
  ylab("SMSY (tons)") +xlab("")+
  # scale_y_continuous(breaks = seq(4.5,6,0.25),labels = round(10^seq(4.5,6,0.25),1)) +
  coord_cartesian(ylim=c(0,1.5e5),expand = FALSE)

f_all <- gridExtra::grid.arrange(f1, f2, nrow = 2)

ggsave(f_all, file = paste0(Dir,"MSY_trend.png"), width = 8, height = 8)
ggsave(f_all, file = paste0(Dir,"MSY_trend.eps"), width = 8, height = 8)