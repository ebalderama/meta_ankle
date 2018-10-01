library(meta)
library(metafor)
library(gtools)

## Metaprop on all studies

#Inverse method
all.inv<-metaprop(counts345[[10]],n,method="Inverse",incr=0.1,allincr=TRUE)
#Proportion for Fixed Effects model
all.inv.fix<-inv.logit(all.inv$TE.fixed)
#Proportion for Random Effects model
all.inv.ran<-inv.logit(all.inv$TE.random)

#GLMM method
all.glmm<-metaprop(counts345[[10]],n,method="GLMM",incr=0.1,allincr=TRUE)
#Proportion for Fixed Effects model
all.glmm.fix<-inv.logit(all.glmm$TE.fixed)
#Proportion for Random Effects model
all.glmm.ran<-inv.logit(all.glmm$TE.random)

# #inverse method
# #fixed effects
# iallf<-logit(0.0901)
# #random effects
# iallr<-logit(0.0751)
# 
# #GLMM
# #fixed effects
# gallf<-logit(0.0757)
# #random effects
# gallr<-logit(0.0657)

## Metaprop on TAA studies (following the same structure as above)

taa.inv<-metaprop(counts345[[10]][1:23],n[1:23],method="Inverse",incr=0.1,allincr=TRUE)
taa.inv.fix<-inv.logit(taa.inv$TE.fixed)
taa.inv.ran<-inv.logit(taa.inv$TE.random)

taa.glmm<-metaprop(counts345[[10]][1:23],n[1:23],method="GLMM",incr=0.1,allincr=TRUE)
taa.glmm.fix<-inv.logit(taa.glmm$TE.fixed)
taa.glmm.ran<-inv.logit(taa.glmm$TE.random)

# #inverse taa
# #fixed
# itaaf<-logit(0.0898)
# #random
# itaar<-logit(0.0730)
# 
# #glmm taa
# #fixed
# gtaaf<-logit(0.0754)
# #random
# gtaar<-logit(0.0656)

## Metaprop on AAO studies (still following the same structure)

aao.inv<-metaprop(counts345[[10]][24:32],n[24:32],method="Inverse",incr=0.1,allincr=TRUE)
aao.inv.fix<-inv.logit(aao.inv$TE.fixed)
aao.inv.ran<-inv.logit(aao.inv$TE.random)

aao.glmm<-metaprop(counts345[[10]][24:32],n[24:32],method="GLMM",incr=0.1,allincr=TRUE)
aao.glmm.fix<-inv.logit(aao.glmm$TE.fixed)
aao.glmm.ran<-inv.logit(aao.glmm$TE.random)

# #inverse aao
# #fixed
# iaaof<-logit(0.1028)
# #random
# iaaor<-logit(0.1000)
# 
# #GLMM aao
# gaaof<-logit(0.0843)
# #random
# gaaor<-logit(0.0720)

aaa.inv<-metaprop(counts345[[10]][33:38],n[33:38],method="Inverse",incr=0.1,allincr=TRUE)
aaa.inv.fix<-inv.logit(aaa.inv$TE.fixed)
aaa.inv.ran<-inv.logit(aaa.inv$TE.random)

aaa.glmm<-metaprop(counts345[[10]][33:38],n[33:38],method="GLMM",incr=0.1,allincr=TRUE)
aaa.glmm.fix<-inv.logit(aaa.glmm$TE.fixed)
aaa.glmm.ran<-inv.logit(aaa.glmm$TE.random)

# #inverse aaa
# #fixed
# iaaaf<-logit(0.0646)
# #random
# iaaar<-logit(0.0646)
# 
# #GLMM aaa
# #fixed
# gaaaf<-logit(0.0588)
# #random
# gaaar<-logit(0.0588)

# Now looking at odds ratios
aao.inv.fix/taa.inv.fix
aao.inv.ran/taa.inv.ran
aao.glmm.fix/taa.glmm.fix
aao.glmm.ran/taa.glmm.ran

aaa.inv.fix/taa.inv.fix
aaa.inv.ran/taa.inv.ran
aaa.glmm.fix/taa.glmm.fix
aaa.glmm.ran/taa.glmm.ran

#Next up: getting confidence intervals