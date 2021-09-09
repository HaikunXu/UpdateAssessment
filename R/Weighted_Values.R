library(tidyverse)
library(IATTCassessment)
library(r4ss)

model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel",
           "Gro","Mov","Mrt","Sel",
           "Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

Weight_M <- data.frame("Model"=model,"Weight_M"=c(0.01,0.13,0.02,0.05,0.24,0.01,0.02,0.09,0.04,0.22,0.07,0.11))
Weight_S <- data.frame("Steepness"=steepness,"Weight_S"=c(0.44,0.31,0.21,0.04))

for (m in 1:12) {
  if(m==1) Dir <- paste0("D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/",
                         model[m],"-",toString(1),"/")
  else Dir <- c(Dir,paste0("D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/",
                           model[m],"-",toString(1),"/"))
}

Save_Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
Last_Year <- 2019
xlim <- c(1978,2021)
ylim <- c(0,1)

# FAA
for (i in 1:3) { #length(model)) {
  print(i)
  myreplist = r4ss::SS_output(dir = Dir[i], ncols = 500, covar = F, verbose = FALSE, printstats = FALSE)
  
  Z <- myreplist$Z_at_age
  M <- myreplist$M_at_age
  # M_Matrix <- rbind(matrix(rep(data.matrix(M[1, ]), nrow(Z)/2), nrow = nrow(Z)/2, byrow = T), matrix(rep(data.matrix(M[2, 
  #     ]), nrow(Z)/2), nrow = nrow(Z)/2, byrow = T))
  
  F_M <- Z
  F_M[, 4:43] <- Z[, 4:43] - data.matrix(M[, 4:43])
  
  F_Matrix <- F_M %>% gather("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                             "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
                             "34", "35", "36", "37", "38", "39", "40", key = "Age", value = "FAA")
  F_Matrix$Age <- as.numeric(F_Matrix$Age)
  F_Matrix$Year2 <- ceiling(F_Matrix$Yr/4) + 1974
  
  # F_Matrix <- na.omit(F_Matrix %>% mutate('Group'=cut(Age, breaks = c(-1,4,8,12,20,39))))
  
  F_vector <- F_Matrix %>% group_by(Sex, Year2, Age) %>% summarise(F_annual = sum(FAA)) %>% mutate(Age = cut(Age, 
                                                                                                             breaks = c(0, 4, 8, 12, 19, 40), labels = c("1-4 quarters", "5-8 quarters", "9-12 quarters", "13-19 quarters", "20+ quarters")))
  F_vector <- na.omit(F_vector)
  
  if(i==1) {
    FAA <- F_vector %>% group_by(Age, Year2) %>% summarise(F_group = mean(F_annual)) %>% mutate(Model=model[i])
  } else {
    FAA <- rbind(FAA, F_vector %>% group_by(Age, Year2) %>% summarise(F_group = mean(F_annual)) %>% mutate(Model=model[i]))
  }
}

data <- left_join(FAA,Weight_M) %>% group_by(Age,Year2) %>%
  mutate(Weight=Weight_M/sum(Weight_M))
Data <- data %>% filter(Year2 <= Last_Year) %>% group_by(Age,Year2) %>%
  mutate(Combined=sum(F_group*Weight))

f <- ggplot(data = Data) +
  geom_line(aes(x = Year2, y = F_group, color=Model)) +
  geom_point(aes(x = Year2, y = Combined),data=Data,size=2) +
  facet_wrap(~Age,nrow=5) +
  theme_bw(20) + ylab("Average annual F") + xlab("Year") +
  coord_cartesian(xlim=xlim,ylim=ylim,expand=FALSE)

ggsave(f,file = paste0(Save_Dir, "faa_a.png"), width = 12, height = 15)
ggsave(f,file = paste0(Save_Dir, "faa_a.pdf"), width = 12, height = 15)


# model-weighted reference points
Mgmt <- read.csv(paste0(Save_Dir, "Mgmt.csv"))

R <- Mgmt[,c(1,2,7,8)] %>% group_by(Model,Steepness) %>%
  summarise(Est=F,Std=(F-F_low)/1.96)

R <- left_join(left_join(R,Weight_M),Weight_S) %>%
  group_by(Model) %>% mutate(Weight_S2=Weight_S/sum(Weight_S)) %>%
  mutate(Weight=Weight_M*Weight_S)

sum(R$Est*R$Weight)

# find the 2.5% and 97.5% quantiles

R_series <- seq(0,3,0.001)
df <- pdf_cdf(R_series,R,n_model=44)
plot(R_series,df$cdf)
lines(R_series,df$pdf)

Low <- R_series[which(abs(df$cdf-0.025)==min(abs(df$cdf-0.025)))]
Medium <- R_series[which(abs(df$cdf-0.5)==min(abs(df$cdf-0.5)))]
High <- R_series[which(abs(df$cdf-0.975)==min(abs(df$cdf-0.975)))]
