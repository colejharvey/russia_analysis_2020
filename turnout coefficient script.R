##Need all Russian datasets combined
##Give same headings
##Give unique electionid


###Starting here
electoral.all <- read.csv("C:/Users/Cole/Documents/Python projects/py-scrape-and-download/ukraine elections combined 2002-2014.csv")
electoral.all <- as_tibble(electoral.all)

electoral.all <- electoral.all %>% mutate(electionid = as.factor(paste(year, presidential, round.2, sep="_")))

for(i in unique(electoral.all$electionid)){
  election.sub <- subset(electoral.all, electoral.all$electionid == i)
  main.west.party.abs<-(election.sub$main_west / election.sub$voter_list)
  main.east.party.abs<-(election.sub$main_east / election.sub$voter_list)
  turnout<-(election.sub$votes_cast / election.sub$voter_list)
  turnout[turnout=="Inf"]<-NA  #Replaces one Inf with NA
  election.sub<-cbind(election.sub, main.west.party.abs, main.east.party.abs, turnout)
  
  model.west<-lmer(main.west.party.abs~turnout+
                     (1+turnout|regionid), data=election.sub, REML=FALSE)
  model.west <- update(model.west, control = lmerControl(optimizer = "Nelder_Mead"))
  summary(model.west) #Nelder-Mead removes convergence error
  
  
  model.east<-lmer(main.east.party.abs~turnout+
                     (1+turnout|regionid), data=election.sub, REML=FALSE)
  model.east <- update(model.east, control = lmerControl(optimizer = "Nelder_Mead"))
  summary(model.east)
  
  ###Getting the coefficients##
  coefs.west<-coef(model.west)$regionid[,2]
  coefs.east<-coef(model.east)$regionid[,2]
  coefmatrix<-matrix(NA,nrow=length(coefs.west), ncol=5)
  coefmatrix[,2]<-as.numeric(coefs.west)
  coefmatrix[,4]<-as.numeric(coefs.east)
  
  ##Getting regionids##
  regionids<-unique(model.west@frame[["regionid"]])
  coefmatrix[,1]<-regionids
  ##Getting standard errors##
  ranse.west<-se.ranef(model.west)$regionid[,2]
  ranse.east<-se.ranef(model.east)$regionid[,2]
  coefmatrix[,3]<-ranse.west
  coefmatrix[,5]<-ranse.east
  
  colnames(coefmatrix) <- c("regionid","coefs.west","se.west", "coefs.east", "se.east")
  write.csv(coefmatrix, paste("", i, ".csv"))
  
}
