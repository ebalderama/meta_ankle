tab3 <- read.csv("C:\\Users\\lknap_he3eox\\Documents\\Grad School\\Loyola\\Research\\Balderama\\tab3.csv", fill=T, header=F, skip=5, stringsAsFactors=F, na.strings = c("NR"))
tab4 <- read.csv("C:\\Users\\lknap_he3eox\\Documents\\Grad School\\Loyola\\Research\\Balderama\\tab4.csv", fill=T, header=F, skip=5, stringsAsFactors=F, na.strings = c("NR"))
tab5 <- read.csv("C:\\Users\\lknap_he3eox\\Documents\\Grad School\\Loyola\\Research\\Balderama\\tab5.csv", fill=T, header=F, skip=5, stringsAsFactors=F, na.strings = c("NR"))

tab3 <- tab3[-c(24:26,42:44),]
tab4 <- tab4[-c(24:26,42:44),]
tab5 <- tab5[-c(24:26,42:44),]

taarows <- aaorows <- aaarows <- rep(FALSE, nrow(tab4))
taarows[1:23] <- TRUE
aaorows[24:32] <- TRUE
aaarows[33:38] <- TRUE

#STUDY (SAMPLE) SIZES
n <- tab3[,5]
total <- as.numeric(n)

#GROUPING VARIABLE
Prosthesis <- (rep(c("Other", "Open", "Arthroscopic"), c(23,9,6)))

#CONTINUOUS VARIABLES - what's with the "regexpr(...)+1" part?
Age <- as.numeric(substr(tab3[,7], 1, regexpr("[[:punct:]]", tab3[,7])+1))
Age.sd <- as.numeric(substr(tab3[,7], regexpr("[[:punct:]]", tab3[,7])+7, regexpr("\\+", tab3[,7])+7))
Followup <- as.numeric(substr(tab3[,8], 1, regexpr("[[:punct:]]", tab3[,8])+1))
Followup.sd <- as.numeric(substr(tab3[,8], regexpr("[[:punct:]]", tab3[,8])+7, regexpr("\\+", tab3[,8])+7))

#COUNT VARIABLES
taa <- aao <- aaa <- list()
counts345 <- list()

for(i in 1:4){
  counts345[[i]] <- as.numeric(substr(tab3[,8+i], regexpr("\\(", tab3[,8+i])+1, regexpr(")", tab3[,8+i])-1))
}

for(i in 1:6){
  counts345[[4+i]] <- as.numeric(substr(tab4[,4+i], regexpr("\\(", tab4[,4+i])+1, regexpr(")", tab4[,4+i])-1))
  taa[[i]] <- as.numeric(substr(tab4[taarows, 4+i], regexpr("\\(", tab4[taarows, 4+i])+1, regexpr(")", tab4[taarows, 4+i])-1))
  aao[[i]] <- as.numeric(substr(tab4[aaorows, 4+i], regexpr("\\(", tab4[aaorows, 4+i])+1, regexpr(")", tab4[aaorows, 4+i])-1))
  aaa[[i]] <- as.numeric(substr(tab4[aaarows, 4+i], regexpr("\\(", tab4[aaarows, 4+i])+1, regexpr(")", tab4[aaarows, 4+i])-1))
}

for(i in 1:5){
  counts345[[10+i]] <- as.numeric(substr(tab5[,4+i], regexpr("\\(", tab5[,4+i])+1, regexpr(")", tab5[,4+i])-1))
}

failx <- as.numeric(as.character(unlist(counts345[[10]])))
comp.ovr <- as.numeric(as.character(unlist(counts345[[9]])))
rev.to.fus <- as.numeric(as.character(unlist(counts345[[14]])))
overall.rev <- as.numeric(as.character(unlist(counts345[[12]])))
loose_nonun<-as.numeric(as.character(unlist(counts345[[8]])))
loose<-c(loose_nonun[1:sum(taarows)],rep(0,length(loose_nonun)-sum(taarows)))
nonun<-c(rep(0,sum(taarows)),loose_nonun[(sum(taarows)+1):length(loose_nonun)])

startyr <- endyr <- 0

for(i in 1:38){
  startyr[[i]] <- as.numeric(substr(tab3[i,4], 1,4))
  endyr[[i]] <- as.numeric(substr(tab3[i,4],6,9))
}
