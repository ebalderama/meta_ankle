library(meta)
library(metafor)
library(gtools)

metaprop(counts345[[10]],n,method="Inverse",incr=0.1,allincr=TRUE)
metaprop(counts345[[10]],n,method="GLMM",incr=0.1,allincr=TRUE)

#inverse method
#fixed effects
iallf<-logit(0.0901)
#random effects
iallr<-logit(0.0751)

#GLMM
#fixed effects
gallf<-logit(0.0757)
#random effects
gallr<-logit(0.0657)

metaprop(counts345[[10]][1:23],n[1:23],method="Inverse",incr=0.1,allincr=TRUE)
metaprop(counts345[[10]][1:23],n[1:23],method="GLMM",incr=0.1,allincr=TRUE)

#inverse taa
#fixed
itaaf<-logit(0.0898)
#random
itaar<-logit(0.0730)

#glmm taa
#fixed
gtaaf<-logit(0.0754)
#random
gtaar<-logit(0.0656)

metaprop(counts345[[10]][24:32],n[24:32],method="Inverse",incr=0.1,allincr=TRUE)
metaprop(counts345[[10]][24:32],n[24:32],method="GLMM",incr=0.1,allincr=TRUE)

#inverse aao
#fixed
iaaof<-logit(0.1028)
#random
iaaor<-logit(0.1000)

#GLMM aao
gaaof<-logit(0.0843)
#random
gaaor<-logit(0.0720)

metaprop(counts345[[10]][33:38],n[33:38],method="Inverse",incr=0.1,allincr=TRUE)
metaprop(counts345[[10]][33:38],n[33:38],method="GLMM",incr=0.1,allincr=TRUE)

#inverse aaa
#fixed
iaaaf<-logit(0.0646)
#random
iaaar<-logit(0.0646)

#GLMM aaa
#fixed
gaaaf<-logit(0.0588)
#random
gaaar<-logit(0.0588)

iaaor/itaar
iaaar/itaar
