# POMA
---
title: "Word position effect on the perception of non-native consonant sequence"
output: html_document
---
```{r setup, include=FALSE}
rm(list=ls())
setwd("~/Desktop/POMA")
data<- read.table("data_trans_positioneffect.txt", header=T)
summary(data)
head(data)

library("nnet")
require(lme4)
require(lsmeans)
require(plyr)
require(ggplot2)
# require(pbkrtest) did not use in this study

data_cc<-data[data$Structure == "CC",]
data_cvc<-data[data$Structure == "CVC",]

```

#########################################
#########################################
#Epenthesis between CC
##Predictions: initial != medial, ss>sn>sl  

###Global sonority effect (cluster types) and interaction with Word Position 

The effects of sonority preference will be different in word-initial positions and in word-medial positions. 
H3-1: In word-initial sequences, Mandarin listeners will perceive less epenthetic vowels between C1 and C2 when C1 is less sonorous than C2, replicating on Berent et al.???s (2006) findings. 
We do not form a hypothesis with regard to the word-medial sequences /VCCV/, because it is implausible to decide how Mandarin listeners syllabified those sequences.

And in this model, we conceptually separated the fixed effects into control variable (C1 place of articulation) and the test variables (Cluster type, Word Position and interaction between them). And we included a term for random intercepts for participants and items as well as by-Speaker random slopes for Cluster type and Word Position. 

########################################
########################################

## Interaction effects for Cluster type, Word_position and C1 place of articulation of the vowel peceived 
```{r, echo= FALSE}

#full model 
m_h2_full <- glmer(Vowel ~  Cluster_type + Word_position + C1 + Cluster_type: Word_position + Cluster_type : C1 + Word_position :C1 + Cluster_type : Word_position :C1  + (1 + Word_position + Cluster_type |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa"), data=data_cc)

# without one of interactions 
m_h2_1<- update(m_h2_full,.~.-Cluster_type : Word_position) # n.s. (p = 1)
m_h2_2<- update(m_h2_full,.~.-Cluster_type : C1) # n.s.
m_h2_3<- update(m_h2_full,.~.-Word_position : C1)  # n.s.
m_h2_4<- update(m_h2_full,.~.-Cluster_type : Word_position : C1) 

# likelihood ratio test 
anova(m_h2_1, m_h2_full,test="Chisq")
anova(m_h2_2, m_h2_full,test="Chisq")
anova(m_h2_3, m_h2_full,test="Chisq")
anova(m_h2_4, m_h2_full,test="Chisq")

```

##Cluster type main effect
```{r,echo=FALSE}

# word position and cluster type main effects 
m_h2_maineffects <- glmer(Vowel ~  Cluster_type + Word_position + C1   + (1 + Word_position + Cluster_type |subject) + (1  |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa", nAGQ=25), data=data_cc)  
# when add random slopes (Word_position + Cluster_type) into tokens, the p-value is 0.051

m_h2_null_type<- update(m_h2_maineffects,.~.-Cluster_type  ) # without Cluster_type 

#likelihood ratio test
anova(m_h2_null_type,m_h2_maineffects,test="Chisq") 

# PBmodcomp(m_h1_null_position, m_h1_maineffects,seed=1)
```

##Word position main effect
```{r, echo=FALSE}
m_h2_null_position<- update(m_h2_maineffects,.~.-Word_position) 

###likelihood ratio test
anova(m_h2_null_position, m_h2_maineffects, test ="Chisq") 
```

##C1 place of articulation
```{r, echo=FALSE}
m_h2_null_C1<- update(m_h2_maineffects,.~.-C1) 

###likelihood ratio test
anova(m_h2_null_C1, m_h2_maineffects, test ="Chisq") # n.s. (p=.93) no word position main effect
```

```{r,include= FALSE}
#check tests
quartz(width=11,height=6);par(mfrow=c(1,2))
hist(residuals(m_h2_maineffects));qqnorm(residuals(m_h2_maineffects));qqline(residuals(m_h2_maineffects))
plot(fitted(m_h2_maineffects),residuals(m_h2_maineffects))
```

```{r,include=FALSE}
#check slope
m_h2_noslope <- glmer(Vowel ~  Cluster_type + Word_position + C1   + (1  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa", nAGQ=25), data=data_cc)

m_h2_null_type<- update(m_h2_maineffects,.~.-Cluster_type)

##likelihood ration test
anova(m_h2_null_type,m_h2_noslope,test="Chisq") # n.s. (p=0.27)
```

##Plot
```{r, include = FALSE}

library(plyr) # to calculate the mean, sd, ci using ddply()

conf <- ddply(data_cc, c("Word_position","Cluster_type"),summarize,
              n=sum(!is.na(Vowel)),
              m=mean(Vowel),
              sd=sd(Vowel),
              se=sd/sqrt(n),
              ci=se * qt (.975,n-1)
)
conf
```
```{r,echo= FALSE}

ggplot(conf, aes(x=Cluster_type, y=m, fill=Word_position)) +              
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.3) +        
  geom_errorbar(aes(ymin=m-ci,ymax=m+ci), width=0.2, size=.3, position=position_dodge(.9)) +   #for errorbar
  scale_x_discrete(limits=c("SL","SN","SS")) + # for x aes order 
  labs (x ="Cluster types") +            # for x axis label
  labs (y = "% of vowel perceived") +
  scale_fill_manual(name="Position", labels=c("initial", "medial"), values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + 
  theme(legend.title=element_text(size=11),
        legend.position="right", 
        legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(color = "grey")) + 
  theme(axis.title.x = element_text(vjust= 0), axis.title.y = element_text(vjust=0.7)) 
  # facet_grid(. ~ C1)

```

#######################################
# vowel perceived between CVC
#######################################

## Main effect of Structure (CC & CVC)
```{r,echo=FALSE}
m_h2_stru <- glmer(Vowel ~   Structure + Cluster_type + Word_position + C1 + (1 |subject) + (1  |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa"), data=data)
m_h2_stru_null <- update(m_h2_stru,.~.-Structure)

#likelihood ratio test
anova(m_h2_stru_null,m_h2_stru)
```
## Interaction effects for Cluster type, Word_position and C1 place of articulation of the vowel peceived 
```{r, echo= FALSE}

#full model 
m_h2_full <- glmer(Vowel ~  Cluster_type + Word_position + C1 + Cluster_type: Word_position + Cluster_type : C1 + Word_position :C1 + Cluster_type : Word_position :C1  + (1 + Word_position + Cluster_type |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa"), data=data_cvc)

# without one of interactions 
m_h2_1<- update(m_h2_full,.~.-Cluster_type : Word_position) # n.s. (p = 1)
m_h2_2<- update(m_h2_full,.~.-Cluster_type : C1) # n.s.
m_h2_3<- update(m_h2_full,.~.-Word_position : C1)  # n.s.
m_h2_4<- update(m_h2_full,.~.-Cluster_type : Word_position : C1) #n.s.

# likelihood ratio tests
anova(m_h2_1, m_h2_full,test="Chisq")
anova(m_h2_2, m_h2_full,test="Chisq")
anova(m_h2_3, m_h2_full,test="Chisq")
anova(m_h2_4, m_h2_full,test="Chisq")
```

##Cluster type main effect
```{r,echo=FALSE}
## word position and cluster type main effects 
m_h2_maineffects <- glmer(Vowel ~  Cluster_type + Word_position + C1   + (1 + Word_position + Cluster_type |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa"), data=data_cvc)

# without Cluster_type
m_h2_null_type<- update(m_h2_maineffects,.~.-Cluster_type)

##likelihood ration test
anova(m_h2_null_type,m_h2_maineffects,test="Chisq") 

```

##Word position main effect
```{r, echo=FALSE}
#without Word_position
m_h2_null_position<- update(m_h2_maineffects,.~.-Word_position) 

###likelihood ratio test
anova(m_h2_null_position, m_h2_maineffects, test ="Chisq")
```

##C1 place of articulation
```{r, echo=FALSE}
#Without C1
m_h2_null_C1<- update(m_h2_maineffects,.~.-C1) 

###likelihood ratio test
anova(m_h2_null_C1, m_h2_maineffects, test ="Chisq")
```

##plot
```{r, include=FALSE}

library(plyr) # to calculate the mean, sd, ci using ddply()

conf_cvc <- ddply(data_cvc, c("Word_position","Cluster_type"),summarize,
              n=sum(!is.na(Vowel)),
              m=mean(Vowel),
              sd=sd(Vowel),
              se=sd/sqrt(n),
              ci=se * qt (.975,n-1)
)
conf_cvc
```
```{r,echo=FALSE}

ggplot(conf_cvc, aes(x=Cluster_type, y=m, fill=Word_position)) +              
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.3) +        
  geom_errorbar(aes(ymin=m-ci,ymax=m+ci), width=0.2, size=.3, position=position_dodge(.9)) +   #for errorbar
  scale_x_discrete(limits=c("SL","SN","SS")) + # for x aes order 
  labs (x ="Cluster types") +            # for x axis label
  labs (y = "% of vowel perceived") +
  scale_fill_manual(name="Position", labels=c("initial", "medial"), values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + 
  theme(legend.title=element_text(size=11),
        legend.position="right", 
        legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(color = "grey")) + 
  theme(axis.title.x = element_text(vjust= 0), axis.title.y = element_text(vjust=0.7)) 
  #facet_grid(. ~ C1)
```

####################################
####################################
#Consonants misperceived 
##Predictions: initial < medial
####################################
####################################

###Global position effect 

H1: If Mandarin speakers are sensitive to the word position effect, more misperception of the consonants of C1C2 will occur in word initial position than in word medial position. 
H2: If Mandarin speakers are not sensitive to the word position effect, due to their phonotactic constrain that there is no consonant sequences in Mandarin, they will have difficulty to perceive in both positions. 

And in this model, we conceptually separated the fixed effects into control variables (Cluster type and C1 place of articulation) and the test variable (Word Position) and interaction between the test variable and the control variables. And we included a term for random intercepts for participants and items as well as by-Speaker random slopes for Word Position. 

#########################################
## consonant deletion: C1
#########################################
## test interaction effects for Cluster type, Word_position and C1 place of articulation of the vowel peceived 
```{r, echo= FALSE}

#full model 
m_h1_deletionC1_full <- glmer(C1_deletion ~  Cluster_type + Word_position + C1 + Cluster_type: Word_position  + Word_position :C1 + Cluster_type : Word_position :C1  + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa"), data=data_cc)

# interactions 
m_h1_1<- update(m_h1_deletionC1_full,.~.-Word_position : Cluster_type) 
m_h1_2<- update(m_h1_deletionC1_full,.~.-Word_position : C1) 
m_h1_3<- update(m_h1_deletionC1_full,.~.-Cluster_type : Word_position : C1) 

#likelihood ratio tests
anova(m_h1_1, m_h1_deletionC1_full,test="Chisq")
anova(m_h1_2, m_h1_deletionC1_full,test="Chisq")
anova(m_h1_3, m_h1_deletionC1_full,test="Chisq")

```

##Cluster type main effect
```{r,echo=FALSE}
## word position and cluster type main effects 
m_h1_deletionC1_maineffects <- glmer(C1_deletion ~  Cluster_type + Word_position + C1   + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa", nAGQ=25), data=data_cc)

#without Cluster_type
m_h1_null_type<- update(m_h1_deletionC1_maineffects,.~.-Cluster_type)

##likelihood ration test
anova(m_h1_null_type,m_h1_deletionC1_maineffects,test="Chisq")

#post-hoc
summary(lsmeans(m_h1_deletionC1_maineffects, pairwise~  Cluster_type , adjust="tukey"))
summary(lsmeans(m_h1_deletionC1_maineffects, pairwise~  C1 , adjust="tukey"))

```

##Word position main effect
```{r, echo=FALSE}
#without Word_position
m_h1_null_position<- update(m_h1_deletionC1_maineffects,.~.-Word_position) 

###likelihood ratio test
anova(m_h1_null_position, m_h1_deletionC1_maineffects, test ="Chisq") 
```

##C1 place of articulation
```{r, echo=FALSE}
#without C1
m_h1_null_C1<- update(m_h1_deletionC1_maineffects,.~.-C1) 

###likelihood ratio test
anova(m_h1_null_C1, m_h1_deletionC1_maineffects, test ="Chisq") 
```

```{r,include= FALSE}
#check models 
#dd <- fortify(m_h1_deletionC1_full)
#ggplot(dd, aes(sample=.resid))+stat_qq()
#quartz(width=11,height=6);par(mfrow=c(1,2))
#hist(residuals(m_h1_deletionC1_maineffects));qqnorm(residuals(m_h1_deletionC1_maineffects));qqline(residuals(m_h1_deletionC1_maineffects))
#plot(fitted(m_h1_deletionC1_maineffects),residuals(m_h1_deletionC1_maineffects))
```

##Plot of C1 deletion
```{r, include=FALSE}
require (plyr)
deletionC1<- ddply(data_cc, c("Word_position","Cluster_type"),summarize,
              n=sum(!is.na(C1_deletion)),
              m=mean(C1_deletion),
              sd=sd(C1_deletion),
              se=sd/sqrt(n),
              ci=se * qt (.975,n-1))

deletionC1

```
```{r, echo=FALSE}

ggplot(deletionC1, aes(x=Cluster_type, y=m, fill=Word_position)) +              
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.3) +        
  geom_errorbar(aes(ymin=m-ci,ymax=m+ci), width=0.2, size=.3, position=position_dodge(.9)) +   #for errorbar
  scale_x_discrete(limits=c("SL","SN","SS")) + # for x aes order 
  labs (x ="Cluster types") +            # for x axis label
  labs (y = "% of C1 deletion ") +
  scale_fill_manual(name="Position", labels=c("initial", "medial"), values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c (0,1)) +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + 
  theme(legend.title=element_text(size=11),
        legend.position="right", 
        legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(color = "grey")) + 
  theme(axis.title.x = element_text(vjust= 0), axis.title.y = element_text(vjust=0.7)) 
  # facet_grid(. ~ C1)
```

#########################################
## consonant deletion: C2
#########################################
## test interaction effects for Cluster type, Word_position and C1 place of articulation of the vowel peceived 
```{r, echo= FALSE}

#full model 
m_h1_deletionC2_full <- glmer(C2_deletion ~  Cluster_type + Word_position + C1 + Cluster_type: Word_position  + Word_position :C1 + Cluster_type : Word_position :C1  + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa"), data=data_cc)

# interactions 
m_h1_1<- update(m_h1_deletionC2_full,.~.-Word_position : Cluster_type) 
m_h1_2<- update(m_h1_deletionC2_full,.~.-Word_position : C1) 
m_h1_3<- update(m_h1_deletionC2_full,.~.-Cluster_type : Word_position : C1) 

#likelihood ratio tests
anova(m_h1_1, m_h1_deletionC2_full,test="Chisq")
anova(m_h1_2, m_h1_deletionC2_full,test="Chisq")
anova(m_h1_3, m_h1_deletionC2_full,test="Chisq")

```

##Cluster type main effect
```{r,echo=FALSE}
## word position and cluster type main effects 
m_h1_deletionC2_maineffects <- glmer(C2_deletion ~  Cluster_type + Word_position + C1   + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa", nAGQ=25), data=data_cc)

#without Cluster_type
m_h1_null_type<- update(m_h1_deletionC2_maineffects,.~.-Cluster_type)

##likelihood ration test
anova(m_h1_null_type,m_h1_deletionC2_maineffects,test="Chisq")

```

##Word position main effect
```{r, echo=FALSE}

#without Word_position
m_h1_null_position<- update(m_h1_deletionC2_maineffects,.~.-Word_position) 

###likelihood ratio test
anova(m_h1_null_position, m_h1_deletionC2_maineffects, test ="Chisq") 

```

##C1 place of articulation
```{r, echo=FALSE}
#without C1
m_h1_null_C1<- update(m_h1_deletionC2_maineffects,.~.-C1) 

###likelihood ratio test
anova(m_h1_null_C1, m_h1_deletionC2_maineffects, test ="Chisq") 
```

```{r,include= FALSE}
#quartz(width=11,height=6);par(mfrow=c(1,2))
#hist(residuals(m_h1_deletionC2_maineffects));qqnorm(residuals(m_h1_deletionC2_maineffects));qqline(residuals(m_h1_deletionC2_maineffects))
#plot(fitted(m_h1_deletionC2_maineffects),residuals(m_h1_deletionC2_maineffects))
```

##Plot of C2 deletion
```{r, include=FALSE}
require (plyr)
deletionC2<- ddply(data_cc, c("Word_position","Cluster_type"),summarize,
              n=sum(!is.na(C2_deletion)),
              m=mean(C2_deletion),
              sd=sd(C2_deletion),
              se=sd/sqrt(n),
              ci=se * qt (.975,n-1))

deletionC2

```
```{r, echo=FALSE}

ggplot(deletionC2, aes(x=Cluster_type, y=m, fill=Word_position)) +              
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.3) +        
  geom_errorbar(aes(ymin=m-ci,ymax=m+ci), width=0.2, size=.3, position=position_dodge(.9)) +   #for errorbar
  scale_x_discrete(limits=c("SL","SN","SS")) + # for x aes order 
  labs (x ="Cluster types") +            # for x axis label
  labs (y = "% of C1 deletion ") +
  scale_fill_manual(name="Position", labels=c("initial", "medial"), values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c (0,1)) +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + 
  theme(legend.title=element_text(size=11),
        legend.position="right", 
        legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(color = "grey")) + 
  theme(axis.title.x = element_text(vjust= 0), axis.title.y = element_text(vjust=0.7)) 
  # facet_grid(. ~ C1)
```

#########################################
## consonant change: C1
#########################################
## test interaction effects for Cluster type, Word_position and C1 place of articulation of the vowel peceived 
```{r, echo= FALSE}

#full model 
m_h1_changeC1_full <- glmer(C1_change ~  Cluster_type + Word_position + C1 + Cluster_type: Word_position  + Word_position :C1 + Cluster_type : Word_position :C1  + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa"), data=data_cc)

#without one of interactions 
m_h1_1<- update(m_h1_changeC1_full,.~.-Word_position : Cluster_type) # n.s. (p = 1)
m_h1_2<- update(m_h1_changeC1_full,.~.-Word_position : C1)  # n.s.
m_h1_3<- update(m_h1_changeC1_full,.~.-Cluster_type : Word_position : C1) # p= .052

#likelihood ratio tests
anova(m_h1_1, m_h1_changeC1_full,test="Chisq")
anova(m_h1_2, m_h1_changeC1_full,test="Chisq")
anova(m_h1_3, m_h1_changeC1_full,test="Chisq")

```

##Cluster type main effect
```{r,echo=FALSE}
# word position and cluster type main effects 
m_h1_changeC1_maineffects <- glmer(C1_change ~  Cluster_type + Word_position + C1   + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa", nAGQ=25), data=data_cc)

# without Cluster_type
m_h1_null_type<- update(m_h1_changeC1_maineffects,.~.-Cluster_type)

# likelihood ration test
anova(m_h1_null_type,m_h1_changeC1_maineffects,test="Chisq")  # n.s. p= .0709

```

##Word position main effect
```{r, echo=FALSE}
#without Word_position
m_h1_null_position<- update(m_h1_changeC1_maineffects,.~.-Word_position) 

#likelihood ratio test
anova(m_h1_null_position, m_h1_changeC1_maineffects, test ="Chisq") #n.s. p= 0.14

```

##C1 place of articulation
```{r, echo=FALSE}
#without C1
m_h1_null_C1<- update(m_h1_changeC1_maineffects,.~.-C1) 

#likelihood ratio test
anova(m_h1_null_C1, m_h1_changeC1_maineffects, test ="Chisq") 
```

```{r,include= FALSE}
#quartz(width=11,height=6);par(mfrow=c(1,2))
#hist(residuals(m_h1_changeC1_maineffects));qqnorm(residuals(m_h1_changeC1_maineffects));qqline(residuals(m_h1_changeC1_maineffects))
#plot(fitted(m_h1_changeC1_maineffects),residuals(m_h1_changeC1_maineffects))
```

##Plot
```{r, include=FALSE}
require (plyr)
ChangeC1<- ddply(data_cc, c("Word_position","Cluster_type"),summarize,
              n=sum(!is.na(C1_change)),
              m=mean(C1_change),
              sd=sd(C1_change),
              se=sd/sqrt(n),
              ci=se * qt (.975,n-1))

ChangeC1

```
```{r, echo=FALSE}

ggplot(ChangeC1, aes(x=Cluster_type, y=m, fill=Word_position)) +              
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.3) +        
  geom_errorbar(aes(ymin=m-ci,ymax=m+ci), width=0.2, size=.3, position=position_dodge(.9)) +   #for errorbar
  scale_x_discrete(limits=c("SL","SN","SS")) + # for x aes order 
  labs (x ="Cluster types") +            # for x axis label
  labs (y = "% of C1 change ") +
  scale_fill_manual(name="Position", labels=c("initial", "medial"), values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c (0,1)) +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + 
  theme(legend.title=element_text(size=11),
        legend.position="right", 
        legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(color = "grey")) + 
  theme(axis.title.x = element_text(vjust= 0), axis.title.y = element_text(vjust=0.7)) 
  # facet_grid(. ~ C1)
```

#########################################
## consonant change: C2
#########################################

## test interaction effects for Cluster type, Word_position and C1 place of articulation of the vowel peceived 
```{r, echo= FALSE}

#full model 
m_h1_changeC2_full <- glmer(C2_change ~  Cluster_type + Word_position + C1 + Cluster_type: Word_position  + Word_position :C1 + Cluster_type : Word_position :C1  + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa"), data=data_cc)

# interactions 
m_h1_1<- update(m_h1_changeC2_full,.~.-Word_position : Cluster_type) 
m_h1_2<- update(m_h1_changeC2_full,.~.-Word_position : C1) 
m_h1_3<- update(m_h1_changeC2_full,.~.-Cluster_type : Word_position : C1) 

#likelihood ratio tests 
anova(m_h1_1, m_h1_changeC2_full,test="Chisq")
anova(m_h1_2, m_h1_changeC2_full,test="Chisq")
anova(m_h1_3, m_h1_changeC2_full,test="Chisq")

```

##Cluster type main effect
```{r,echo=FALSE}
## word position and cluster type main effects 

m_h1_changeC2_maineffects <- glmer(C2_change ~  Cluster_type + Word_position + C1   + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa", nAGQ=25), data=data_cc)

#without Cluster_type
m_h1_null_type<- update(m_h1_changeC2_maineffects,.~.-Cluster_type)

##likelihood ration test
anova(m_h1_null_type,m_h1_changeC2_maineffects,test="Chisq")

```

##Word position main effect
```{r, echo=FALSE}
#without Word_position
m_h1_null_position<- update(m_h1_changeC2_maineffects,.~.-Word_position) 

###likelihood ratio test
anova(m_h1_null_position, m_h1_changeC2_maineffects, test ="Chisq") 

```

##C1 place of articulation
```{r, echo=FALSE}
#without C1
m_h1_null_C1<- update(m_h1_changeC2_maineffects,.~.-C1) 

###likelihood ratio test
anova(m_h1_null_C1, m_h1_changeC2_maineffects, test ="Chisq") 
```

```{r,include= FALSE}
#dd <- fortify(m_h1_changeC2_full)
#ggplot(dd, aes(sample=.resid))+stat_qq() 
#quartz(width=11,height=6);par(mfrow=c(1,2))
#hist(residuals(m_h1_changeC2_maineffects));qqnorm(residuals(m_h1_changeC2_maineffects));qqline(residuals(m_h1_changeC2_maineffects))
#plot(fitted(m_h1_changeC2_maineffects),residuals(m_h1_changeC2_maineffects))
```

##Plot 
```{r, include=FALSE}
require (plyr)
changeC2<- ddply(data_cc, c("Word_position","Cluster_type"),summarize,
              n=sum(!is.na(C2_change)),
              m=mean(C2_change),
              sd=sd(C2_change),
              se=sd/sqrt(n),
              ci=se * qt (.975,n-1))

changeC2

```
```{r, echo=FALSE}
ggplot(changeC2, aes(x=Cluster_type, y=m, fill=Word_position)) +              
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.3) +        
  geom_errorbar(aes(ymin=m-ci,ymax=m+ci), width=0.2, size=.3, position=position_dodge(.9)) +   #for errorbar
  scale_x_discrete(limits=c("SL","SN","SS")) + # for x aes order 
  labs (x ="Cluster types") +            # for x axis label
  labs (y = "% of C2 change ") +
  scale_fill_manual(name="Position", labels=c("initial", "medial"), values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c (0,1)) +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + 
  theme(legend.title=element_text(size=11),
        legend.position="right", 
        legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(color = "grey")) + 
  theme(axis.title.x = element_text(vjust= 0), axis.title.y = element_text(vjust=0.7)) 
  # facet_grid(. ~ C1)
```

#########################################
# Consonant misperceived in CVC
#########################################
#########################################
## consonant deletion: C1
#########################################
###no C1 deletion in CVC
##Plot of C1 deletion
```{r, include=FALSE}

deletionC1<- ddply(data_cvc, c("Word_position","Cluster_type"),summarize,
              n=sum(!is.na(C1_deletion)),
              m=mean(C1_deletion),
              sd=sd(C1_deletion),
              se=sd/sqrt(n),
              ci=se * qt (.975,n-1))

deletionC1

```
```{r, echo=FALSE}

ggplot(deletionC1, aes(x=Cluster_type, y=m, fill=Word_position)) +              
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.3) +        
  geom_errorbar(aes(ymin=m-ci,ymax=m+ci), width=0.2, size=.3, position=position_dodge(.9)) +   #for errorbar
  scale_x_discrete(limits=c("SL","SN","SS")) + # for x aes order 
  labs (x ="Cluster types") +            # for x axis label
  labs (y = "% of C1 deletion ") +
  scale_fill_manual(name="Position", labels=c("initial", "medial"), values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c (0,1)) +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + 
  theme(legend.title=element_text(size=11),
        legend.position="right", 
        legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(color = "grey")) + 
  theme(axis.title.x = element_text(vjust= 0), axis.title.y = element_text(vjust=0.7)) 
  # facet_grid(. ~ C1)
```

#########################################
## consonant deletion: C2
#########################################
## test interaction effects for Cluster type, Word_position and C1 place of articulation of the vowel peceived 
```{r, echo= FALSE}

#full model 
m_h1_deletionC2_full <- glmer(C2_deletion ~  Cluster_type + Word_position + C1 + Cluster_type: Word_position  + Word_position :C1 + Cluster_type : Word_position :C1  + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa"), data=data_cvc)

# interactions 
m_h1_1<- update(m_h1_deletionC2_full,.~.-Word_position : Cluster_type) 
m_h1_2<- update(m_h1_deletionC2_full,.~.-Word_position : C1) 
m_h1_3<- update(m_h1_deletionC2_full,.~.-Cluster_type : Word_position : C1) 

#likelihood ratio tests
anova(m_h1_1, m_h1_deletionC2_full,test="Chisq")
anova(m_h1_2, m_h1_deletionC2_full,test="Chisq")
anova(m_h1_3, m_h1_deletionC2_full,test="Chisq")

```

##Cluster type main effect
```{r,echo=FALSE}
## word position and cluster type main effects 
m_h1_deletionC2_maineffects <- glmer(C2_deletion ~  Cluster_type + Word_position + C1   + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa", nAGQ=25), data=data_cvc)

#without Cluster_type
m_h1_null_type<- update(m_h1_deletionC2_maineffects,.~.-Cluster_type)

##likelihood ration test
anova(m_h1_null_type,m_h1_deletionC2_maineffects,test="Chisq")

```

##Word position main effect
```{r, echo=FALSE}

#without Word_position
m_h1_null_position<- update(m_h1_deletionC2_maineffects,.~.-Word_position) 

###likelihood ratio test
anova(m_h1_null_position, m_h1_deletionC2_maineffects, test ="Chisq") 

```

##C1 place of articulation
```{r, echo=FALSE}
#without C1
m_h1_null_C1<- update(m_h1_deletionC2_maineffects,.~.-C1) 

###likelihood ratio test
anova(m_h1_null_C1, m_h1_deletionC2_maineffects, test ="Chisq") 
```

```{r,include= FALSE}
#quartz(width=11,height=6);par(mfrow=c(1,2))
#hist(residuals(m_h1_deletionC2_maineffects));qqnorm(residuals(m_h1_deletionC2_maineffects));qqline(residuals(m_h1_deletionC2_maineffects))
#plot(fitted(m_h1_deletionC2_maineffects),residuals(m_h1_deletionC2_maineffects))
```

##Plot of C2 deletion
```{r, include=FALSE}

deletionC2<- ddply(data_cvc, c("Word_position","Cluster_type"),summarize,
              n=sum(!is.na(C2_deletion)),
              m=mean(C2_deletion),
              sd=sd(C2_deletion),
              se=sd/sqrt(n),
              ci=se * qt (.975,n-1))

deletionC2

```
```{r, echo=FALSE}

ggplot(deletionC2, aes(x=Cluster_type, y=m, fill=Word_position)) +              
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.3) +        
  geom_errorbar(aes(ymin=m-ci,ymax=m+ci), width=0.2, size=.3, position=position_dodge(.9)) +   #for errorbar
  scale_x_discrete(limits=c("SL","SN","SS")) + # for x aes order 
  labs (x ="Cluster types") +            # for x axis label
  labs (y = "% of C1 deletion ") +
  scale_fill_manual(name="Position", labels=c("initial", "medial"), values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c (0,1)) +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + 
  theme(legend.title=element_text(size=11),
        legend.position="right", 
        legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(color = "grey")) + 
  theme(axis.title.x = element_text(vjust= 0), axis.title.y = element_text(vjust=0.7)) 
  # facet_grid(. ~ C1)
```
#########################################
## consonant change: C1
#########################################
## test interaction effects for Cluster type, Word_position and C1 place of articulation of the vowel peceived 
```{r, echo= FALSE}

#full model 
m_h1_changeC1_full <- glmer(C1_change ~  Cluster_type + Word_position + C1 + Cluster_type: Word_position  + Word_position :C1 + Cluster_type : Word_position :C1  + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa"), data=data_cvc)

#without one of interactions 
m_h1_1<- update(m_h1_changeC1_full,.~.-Word_position : Cluster_type) # n.s. (p = 1)
m_h1_2<- update(m_h1_changeC1_full,.~.-Word_position : C1)  # n.s.
m_h1_3<- update(m_h1_changeC1_full,.~.-Cluster_type : Word_position : C1) # p= .052

#likelihood ratio tests
anova(m_h1_1, m_h1_changeC1_full,test="Chisq")
anova(m_h1_2, m_h1_changeC1_full,test="Chisq")
anova(m_h1_3, m_h1_changeC1_full,test="Chisq")

```

##Cluster type main effect
```{r,echo=FALSE}
# word position and cluster type main effects 
m_h1_changeC1_maineffects <- glmer(C1_change ~  Cluster_type + Word_position + C1   + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa", nAGQ=25), data=data_cvc)

# without Cluster_type
m_h1_null_type<- update(m_h1_changeC1_maineffects,.~.-Cluster_type)

# likelihood ration test
anova(m_h1_null_type,m_h1_changeC1_maineffects,test="Chisq")  # n.s. p= .0709

```

##Word position main effect
```{r, echo=FALSE}
#without Word_position
m_h1_null_position<- update(m_h1_changeC1_maineffects,.~.-Word_position) 

#likelihood ratio test
anova(m_h1_null_position, m_h1_changeC1_maineffects, test ="Chisq") #n.s. p= 0.14

```

##C1 place of articulation
```{r, echo=FALSE}
#without C1
m_h1_null_C1<- update(m_h1_changeC1_maineffects,.~.-C1) 

#likelihood ratio test
anova(m_h1_null_C1, m_h1_changeC1_maineffects, test ="Chisq") 
```

```{r,include= FALSE}
#quartz(width=11,height=6);par(mfrow=c(1,2))
#hist(residuals(m_h1_changeC1_maineffects));qqnorm(residuals(m_h1_changeC1_maineffects));qqline(residuals(m_h1_changeC1_maineffects))
#plot(fitted(m_h1_changeC1_maineffects),residuals(m_h1_changeC1_maineffects))
```

##Plot
```{r, include=FALSE}
require (plyr)
ChangeC1<- ddply(data_cvc, c("Word_position","Cluster_type"),summarize,
              n=sum(!is.na(C1_change)),
              m=mean(C1_change),
              sd=sd(C1_change),
              se=sd/sqrt(n),
              ci=se * qt (.975,n-1))

ChangeC1

```
```{r, echo=FALSE}

ggplot(ChangeC1, aes(x=Cluster_type, y=m, fill=Word_position)) +              
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.3) +        
  geom_errorbar(aes(ymin=m-ci,ymax=m+ci), width=0.2, size=.3, position=position_dodge(.9)) +   #for errorbar
  scale_x_discrete(limits=c("SL","SN","SS")) + # for x aes order 
  labs (x ="Cluster types") +            # for x axis label
  labs (y = "% of C1 change ") +
  scale_fill_manual(name="Position", labels=c("initial", "medial"), values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c (0,1)) +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + 
  theme(legend.title=element_text(size=11),
        legend.position="right", 
        legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(color = "grey")) + 
  theme(axis.title.x = element_text(vjust= 0), axis.title.y = element_text(vjust=0.7)) 
  # facet_grid(. ~ C1)
```

#########################################
## consonant change: C2
#########################################

## test interaction effects for Cluster type, Word_position and C1 place of articulation of the vowel peceived 
```{r, echo= FALSE}

#full model 
m_h1_changeC2_full <- glmer(C2_change ~  Cluster_type + Word_position + C1 + Cluster_type: Word_position  + Word_position :C1 + Cluster_type : Word_position :C1  + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa"), data=data_cvc)

# interactions 
m_h1_1<- update(m_h1_changeC2_full,.~.-Word_position : Cluster_type) 
m_h1_2<- update(m_h1_changeC2_full,.~.-Word_position : C1) 
m_h1_3<- update(m_h1_changeC2_full,.~.-Cluster_type : Word_position : C1) 

#likelihood ratio tests 
anova(m_h1_1, m_h1_changeC2_full,test="Chisq")
anova(m_h1_2, m_h1_changeC2_full,test="Chisq")
anova(m_h1_3, m_h1_changeC2_full,test="Chisq")

```

##Cluster type main effect
```{r,echo=FALSE}
## word position and cluster type main effects 

m_h1_changeC2_maineffects <- glmer(C2_change ~  Cluster_type + Word_position + C1   + (1 + Word_position  |subject) + (1 |tokens), family=binomial, control = glmerControl(optCtrl = list(maxfun=10000000), optimizer="bobyqa", nAGQ=25), data=data_cvc)

#without Cluster_type
m_h1_null_type<- update(m_h1_changeC2_maineffects,.~.-Cluster_type)

##likelihood ration test
anova(m_h1_null_type,m_h1_changeC2_maineffects,test="Chisq")

```

##Word position main effect
```{r, echo=FALSE}
#without Word_position
m_h1_null_position<- update(m_h1_changeC2_maineffects,.~.-Word_position) 

###likelihood ratio test
anova(m_h1_null_position, m_h1_changeC2_maineffects, test ="Chisq") 

```

##C1 place of articulation
```{r, echo=FALSE}
#without C1
m_h1_null_C1<- update(m_h1_changeC2_maineffects,.~.-C1) 

###likelihood ratio test
anova(m_h1_null_C1, m_h1_changeC2_maineffects, test ="Chisq") 
```

```{r,include= FALSE}
#dd <- fortify(m_h1_changeC2_full)
#ggplot(dd, aes(sample=.resid))+stat_qq() 
#quartz(width=11,height=6);par(mfrow=c(1,2))
#hist(residuals(m_h1_changeC2_maineffects));qqnorm(residuals(m_h1_changeC2_maineffects));qqline(residuals(m_h1_changeC2_maineffects))
#plot(fitted(m_h1_changeC2_maineffects),residuals(m_h1_changeC2_maineffects))
```

##Plot 
```{r, include=FALSE}
require (plyr)
changeC2<- ddply(data_cvc, c("Word_position","Cluster_type"),summarize,
              n=sum(!is.na(C2_change)),
              m=mean(C2_change),
              sd=sd(C2_change),
              se=sd/sqrt(n),
              ci=se * qt (.975,n-1))

changeC2

```
```{r, echo=FALSE}
ggplot(changeC2, aes(x=Cluster_type, y=m, fill=Word_position)) +              
  geom_bar(stat="identity", position=position_dodge(), color="black", size=.3) +        
  geom_errorbar(aes(ymin=m-ci,ymax=m+ci), width=0.2, size=.3, position=position_dodge(.9)) +   #for errorbar
  scale_x_discrete(limits=c("SL","SN","SS")) + # for x aes order 
  labs (x ="Cluster types") +            # for x axis label
  labs (y = "% of C2 change ") +
  scale_fill_manual(name="Position", labels=c("initial", "medial"), values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c (0,1)) +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + 
  theme(legend.title=element_text(size=11),
        legend.position="right", 
        legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(color = "grey")) + 
  theme(axis.title.x = element_text(vjust= 0), axis.title.y = element_text(vjust=0.7)) 
  # facet_grid(. ~ C1)
```
