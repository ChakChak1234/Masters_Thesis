# Load .csv files for pre/post for the three conditions
PANAS<-read.csv("PANAS.csv")
library(vegan)
library(MASS)
library(ggplot2)
library(reshape2)
library(directlabels)
library(dplyr)
library(tidyr)
library(smacof)

##### Use MDS to obtain original scale values ---------------------------------
## Anger Condition
ang<-PANAS[PANAS$Scene.Order %in% c(1,2),]
ang.ori<-metaMDS(na.omit(ang[,5:44]), stress=.05, k=4, trymax=1000,trace = TRUE)
## Anxiety Condition
anx<-PANAS[PANAS$Scene.Order %in% c(3,4),]
anx.ori<-metaMDS(na.omit(anx[,5:44]), stress=.05, k=4, trymax=1000,trace = TRUE)
## Neutral Condition
neu<-PANAS[PANAS$Scene.Order %in% c(5,6),]
neu.ori<-metaMDS(na.omit(neu[,5:44]), stress=.05, k=4, trymax=1000,trace = TRUE)

##### Bootstrap samples to estimate standard errors of scale values -----------
### Bootstrap 1000 dataset simulations, and metaMDS() on each dataset ---------
## Anger Condition
ang.boot.1000<-list() # empty list to store 1000 bootstrapped samples
ang.MDS.1000<-list() # empty list to store 1000 metaMDS() outputs
for(i in 1:1000){
  # Sample 200 cases from original data
  a<-ang[sample(nrow(ang), replace=T),]
  ang.boot.1000[[i]]<-a # Store in list(ang.boot.1000)
  # Apply metaMDS() using euclidean distance and 6 dimensions
  b<-metaMDS(na.omit(ang.boot.1000[[i]][,5:44]), stress=.05, k=4, trymax=1000,trace = 1)
  ang.MDS.1000[[i]]<-b # Store in list(ang.MDS.1000)
  # Report Progress at every 50th Iteration
  if(i %% 50==0) {
    # Print on the screen some message
    cat(paste0("<<<iteration: ", i, ">>>\n"))
  }
  Sys.sleep(0.1) # Just for waiting a bit in this example
}
## Anxiety Condition
anx.boot.1000<-list() # empty list to store 1000 bootstrapped samples
anx.MDS.1000<-list() # empty list to store 1000 metaMDS() outputs
for(i in 1:1000){
  # Sample 200 cases from original data
  a<-ang[sample(nrow(anx), replace=T),]
  anx.boot.1000[[i]]<-a # Store in list(anx.boot.1000)
  # Apply metaMDS() using euclidean distance and 6 dimensions
  b<-metaMDS(na.omit(anx.boot.1000[[i]][,5:44]), stress=.05, k=4, trymax=1000,trace = 1)
  anx.MDS.1000[[i]]<-b # Store in list(anx.MDS.1000)
  # Report Progress at every 50th Iteration
  if(i %% 50==0) {
    # Print on the screen some message
    cat(paste0("<<<iteration: ", i, ">>>\n"))
  }
  Sys.sleep(0.1) # Just for waiting a bit in this example
}
## Neutral Condition
neu.boot.1000<-list() # empty list to store 1000 bootstrapped samples
neu.MDS.1000<-list() # empty list to store 1000 metaMDS() outputs
for(i in 1:1000){
  # Sample 200 cases from original data
  a<-ang[sample(nrow(neu), replace=T),]
  neu.boot.1000[[i]]<-a # Store in list(neu.boot.1000)
  # Apply metaMDS() using euclidean distance and 6 dimensions
  b<-metaMDS(na.omit(neu.boot.1000[[i]][,5:44]), stress=.05, k=4, trymax=1000,trace = 1)
  neu.MDS.1000[[i]]<-b # Store in list(neu.MDS.1000)
  # Report Progress at every 50th Iteration
  if(i %% 50==0) {
    # Print on the screen some message
    cat(paste0("<<<iteration: ", i, ">>>\n"))
  }
  Sys.sleep(0.1) # Just for waiting a bit in this example
}


##### Determine Statistical Significance of Scale-values ----------------------
## Anger Condition
ang.dim<-list()
dim1<-list() # empty list to store dimension 1's output
for(i in 1:1000) {
  a <- ang.MDS.1000[[i]]$species[,1]
  dim1[[i]] <- a
}
ang.m1 <- do.call(cbind, lapply(dim1, c, recursive=TRUE))
# Obtain standard error
se<-apply(ang.m1, 1, sd)
ang.dim[[1]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((ang.ori$species[,1]) - 0)/(se)))
ang.dim[[2]]<-pvalue

dim2<-list() # empty list to store dimension 2's output
for(i in 1:1000) {
  a <- ang.MDS.1000[[i]]$species[,2]
  dim2[[i]] <- a
}
ang.m2 <- do.call(cbind, lapply(dim2, c, recursive=TRUE))
# Obtain standard error
se<-apply(ang.m2, 1, sd)
ang.dim[[3]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((ang.ori$species[,2]) - 0)/(se)))
ang.dim[[4]]<-pvalue

dim3<-list() # empty list to store dimension 3's output
for(i in 1:1000) {
  a <- ang.MDS.1000[[i]]$species[,3]
  dim3[[i]] <- a
}
ang.m3 <- do.call(cbind, lapply(dim3, c, recursive=TRUE))
# Obtain standard error
se<-apply(ang.m3, 1, sd)
ang.dim[[5]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((ang.ori$species[,3]) - 0)/(se)))
ang.dim[[6]]<-pvalue

dim4<-list() # empty list to store dimension 4's output
for(i in 1:1000) {
  a <- ang.MDS.1000[[i]]$species[,4]
  dim4[[i]] <- a
}
ang.m4 <- do.call(cbind, lapply(dim4, c, recursive=TRUE))
# Obtain standard error
se<-apply(ang.m4, 1, sd)
ang.dim[[7]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((ang.ori$species[,4]) - 0)/(se)))
ang.dim[[8]]<-pvalue

## Anxiety Condition
anx.dim<-list()
dim1<-list() # empty list to store dimension 1's output
for(i in 1:1000) {
  a <- anx.MDS.1000[[i]]$species[,1]
  dim1[[i]] <- a
}
anx.m1 <- do.call(cbind, lapply(dim1, c, recursive=TRUE))
# Obtain standard error
se<-apply(anx.m1, 1, sd)
anx.dim[[1]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((anx.ori$species[,1]) - 0)/(se)))
anx.dim[[2]]<-pvalue

dim2<-list() # empty list to store dimension 2's output
for(i in 1:1000) {
  a <- anx.MDS.1000[[i]]$species[,2]
  dim2[[i]] <- a
}
anx.m2 <- do.call(cbind, lapply(dim2, c, recursive=TRUE))
# Obtain standard error
se<-apply(anx.m2, 1, sd)
anx.dim[[3]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((anx.ori$species[,2]) - 0)/(se)))
anx.dim[[4]]<-pvalue

dim3<-list() # empty list to store dimension 3's output
for(i in 1:1000) {
  a <- anx.MDS.1000[[i]]$species[,3]
  dim3[[i]] <- a
}
anx.m3 <- do.call(cbind, lapply(dim3, c, recursive=TRUE))
# Obtain standard error
se<-apply(anx.m3, 1, sd)
anx.dim[[5]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((anx.ori$species[,3]) - 0)/(se)))
anx.dim[[6]]<-pvalue

dim4<-list() # empty list to store dimension 4's output
for(i in 1:1000) {
  a <- anx.MDS.1000[[i]]$species[,4]
  dim4[[i]] <- a
}
anx.m4 <- do.call(cbind, lapply(dim4, c, recursive=TRUE))
# Obtain standard error
se<-apply(anx.m4, 1, sd)
anx.dim[[7]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((anx.ori$species[,4]) - 0)/(se)))
anx.dim[[8]]<-pvalue

## Neutral Condition
neu.dim<-list()
dim1<-list() # empty list to store dimension 1's output
for(i in 1:1000) {
  a <- neu.MDS.1000[[i]]$species[,1]
  dim1[[i]] <- a
}
neu.m1 <- do.call(cbind, lapply(dim1, c, recursive=TRUE))
# Obtain standard error
se<-apply(neu.m1, 1, sd)
neu.dim[[1]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((neu.ori$species[,1]) - 0)/(se)))
neu.dim[[2]]<-pvalue

dim2<-list() # empty list to store dimension 2's output
for(i in 1:1000) {
  a <- neu.MDS.1000[[i]]$species[,2]
  dim2[[i]] <- a
}
neu.m2 <- do.call(cbind, lapply(dim2, c, recursive=TRUE))
# Obtain standard error
se<-apply(neu.m2, 1, sd)
neu.dim[[3]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((neu.ori$species[,2]) - 0)/(se)))
neu.dim[[4]]<-pvalue

dim3<-list() # empty list to store dimension 3's output
for(i in 1:1000) {
  a <- neu.MDS.1000[[i]]$species[,3]
  dim3[[i]] <- a
}
neu.m3 <- do.call(cbind, lapply(dim3, c, recursive=TRUE))
# Obtain standard error
se<-apply(neu.m3, 1, sd)
neu.dim[[5]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((neu.ori$species[,3]) - 0)/(se)))
neu.dim[[6]]<-pvalue

dim4<-list() # empty list to store dimension 4's output
for(i in 1:1000) {
  a <- neu.MDS.1000[[i]]$species[,4]
  dim4[[i]] <- a
}
neu.m4 <- do.call(cbind, lapply(dim4, c, recursive=TRUE))
# Obtain standard error
se<-apply(neu.m4, 1, sd)
neu.dim[[7]]<-se
# z test to see if the distribution of scale-values is significantly different from 0
pvalue<-2*pnorm(-abs(((neu.ori$species[,4]) - 0)/(se)))
neu.dim[[8]]<-pvalue

#### Table.1 Original scale-values and Standard Errors ------------------------
## Anger Condition
ang.final<-cbind(Condition=c(rep("Pre",20),rep("Post",20)), 
                 ang.dim[[2]],ang.dim[[1]],ang.dim[[4]],ang.dim[[3]],
                 ang.dim[[6]],ang.dim[[5]],ang.dim[[8]],ang.dim[[7]])
colnames(ang.final)<-c("Condition", "dim1.pval", "dim1.se", "dim2.pval", "dim2.se",
                       "dim3.pval", "dim3.se", "dim4.pval", "dim4.se")
row.names(ang.final) = substr(row.names(ang.final),3,nchar(row.names(ang.final)))
row.names(ang.final)[1:20] = substr(row.names(ang.final)[1:20],2,nchar(row.names(ang.final)[1:20]))
# Table with scale values and error estimates
test<-as.data.frame(unlist(ang.final[,seq(1,9,2)]))
testt<-as.data.frame(cbind(unlist(ang.ori$species[,1]),unlist(ang.ori$species[,2]),
                           unlist(ang.ori$species[,3]),unlist(ang.ori$species[,4])))
ang.dim.sv.se<-as.data.frame(cbind(ang.final[,1],
                                   testt[,1],ang.final[,3],testt[,2],ang.final[,5],
                                   testt[,3],ang.final[,7],testt[,4],ang.final[,9]))
colnames(ang.dim.sv.se)<-c("Condition", "dim1.scale.value", "dim1.se", "dim2.scale.value", "dim2.se",
                           "dim3.scale.value", "dim3.se", "dim4.scale.value", "dim4.se")

## Anxiety Condition
anx.final<-cbind(Condition=c(rep("Pre",20),rep("Post",20)), 
                 anx.dim[[2]],anx.dim[[1]],anx.dim[[4]],anx.dim[[3]],
                 anx.dim[[6]],anx.dim[[5]],anx.dim[[8]],anx.dim[[7]])
colnames(anx.final)<-c("Condition", "dim1.pval", "dim1.se", "dim2.pval", "dim2.se",
                       "dim3.pval", "dim3.se", "dim4.pval", "dim4.se")
row.names(anx.final) = substr(row.names(anx.final),3,nchar(row.names(anx.final)))
row.names(anx.final)[1:20] = substr(row.names(anx.final)[1:20],2,nchar(row.names(anx.final)[1:20]))
# Table with scale values and error estimates
test<-as.data.frame(unlist(anx.final[,seq(1,9,2)]))
testt<-as.data.frame(cbind(unlist(anx.ori$species[,1]),unlist(anx.ori$species[,2]),
                           unlist(anx.ori$species[,3]),unlist(anx.ori$species[,4])))
anx.dim.sv.se<-as.data.frame(cbind(anx.final[,1],
                                   testt[,1],anx.final[,3],testt[,2],anx.final[,5],
                                   testt[,3],anx.final[,7],testt[,4],anx.final[,9]))
colnames(anx.dim.sv.se)<-c("Condition", "dim1.scale.value", "dim1.se", "dim2.scale.value", "dim2.se",
                           "dim3.scale.value", "dim3.se", "dim4.scale.value", "dim4.se")

## Neutral Condition
neu.final<-cbind(Condition=c(rep("Pre",20),rep("Post",20)), 
                 neu.dim[[2]],neu.dim[[1]],neu.dim[[4]],neu.dim[[3]],
                 neu.dim[[6]],neu.dim[[5]],neu.dim[[8]],neu.dim[[7]])
colnames(neu.final)<-c("Condition", "dim1.pval", "dim1.se", "dim2.pval", "dim2.se",
                       "dim3.pval", "dim3.se", "dim4.pval", "dim4.se")
row.names(neu.final) = substr(row.names(neu.final),3,nchar(row.names(neu.final)))
row.names(neu.final)[1:20] = substr(row.names(neu.final)[1:20],2,nchar(row.names(neu.final)[1:20]))
# Table with scale values and error estimates
test<-as.data.frame(unlist(neu.final[,seq(1,9,2)]))
testt<-as.data.frame(cbind(unlist(neu.ori$species[,1]),unlist(neu.ori$species[,2]),
                           unlist(neu.ori$species[,3]),unlist(neu.ori$species[,4])))
neu.dim.sv.se<-as.data.frame(cbind(neu.final[,1],
                                   testt[,1],neu.final[,3],testt[,2],neu.final[,5],
                                   testt[,3],neu.final[,7],testt[,4],neu.final[,9]))
colnames(neu.dim.sv.se)<-c("Condition", "dim1.scale.value", "dim1.se", "dim2.scale.value", "dim2.se",
                           "dim3.scale.value", "dim3.se", "dim4.scale.value", "dim4.se")

#### Table.2 Compare pre- and post-test scores --------------------------------
### Question Pre.Mean(SD) Post.Mean(SD) Pre-Post.Diff Paired-T.test P.value
## Anger Condition
multi.fun <- function(x) { # Function to compute mean and standard deviation
  c(Mean = mean(x,na.rm=TRUE), SD = sd(x,na.rm=TRUE))}
ang.mean<-cbind(unlist(lapply(ang[,5:44], multi.fun)))
ang.sum.stat<-data.frame(ang.mean[seq(1,40,2),],
                         ang.mean[seq(2,40,2),],
                         ang.mean[seq(41,80,2),],
                         ang.mean[seq(42,80,2),])
colnames(ang.sum.stat) <- c("Pre.Mean","Pre.SD","Post.Mean","Post.SD")
row.names(ang.sum.stat) = substr(row.names(ang.sum.stat),4,nchar(row.names(ang.sum.stat)))
ang.sum.stat$Pre.Post.Diff<-ang.sum.stat[,3]-ang.sum.stat[,1]
a<-ang[,5:24]
b<-ang[,25:44]
ab<-NULL
p<-NULL
for(i in 1:20){
  meh<-t.test(a[,i],b[,i], paired=TRUE)
  ab[i]<-meh$statistic
  p[i]<-meh$p.value
}
ang.sum.stat$Paired.t.test<-ab
ang.sum.stat$P.value<-p

## Anxiety Condition
anx.mean<-cbind(unlist(lapply(anx[,5:44], multi.fun)))
anx.sum.stat<-data.frame(anx.mean[seq(1,40,2),],
                         anx.mean[seq(2,40,2),],
                         anx.mean[seq(41,80,2),],
                         anx.mean[seq(42,80,2),])
colnames(anx.sum.stat) <- c("Pre.Mean","Pre.SD","Post.Mean","Post.SD")
row.names(anx.sum.stat) = substr(row.names(anx.sum.stat),4,nchar(row.names(anx.sum.stat)))
anx.sum.stat$Pre.Post.Diff<-anx.sum.stat[,3]-anx.sum.stat[,1]
a<-anx[,5:24]
b<-anx[,25:44]
ab<-NULL
p<-NULL
for(i in 1:20){
  meh<-t.test(a[,i],b[,i], paired=TRUE)
  ab[i]<-meh$statistic
  p[i]<-meh$p.value
}
anx.sum.stat$Paired.t.test<-ab
anx.sum.stat$P.value<-p

## Neutral Condition
neu.mean<-cbind(unlist(lapply(neu[,5:44], multi.fun)))
neu.sum.stat<-data.frame(neu.mean[seq(1,40,2),],
                         neu.mean[seq(2,40,2),],
                         neu.mean[seq(41,80,2),],
                         neu.mean[seq(42,80,2),])
colnames(neu.sum.stat) <- c("Pre.Mean","Pre.SD","Post.Mean","Post.SD")
row.names(neu.sum.stat) = substr(row.names(neu.sum.stat),4,nchar(row.names(neu.sum.stat)))
neu.sum.stat$Pre.Post.Diff<-neu.sum.stat[,3]-neu.sum.stat[,1]
a<-neu[,5:24]
b<-neu[,25:44]
ab<-NULL
p<-NULL
for(i in 1:20){
  meh<-t.test(a[,i],b[,i], paired=TRUE)
  ab[i]<-meh$statistic
  p[i]<-meh$p.value
}
neu.sum.stat$Paired.t.test<-ab
neu.sum.stat$P.value<-p

#### Display Pre-Post Core Profiles of original scale-values ------------------
## Anger Condition
# Plot Dimension 1
options(digit=2)
df <- as.matrix(ang.dim.sv.se)
rn <- row.names(ang.dim.sv.se)
df <- data.frame(df)
dat <- df %>% select(Condition,dim1.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim1.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="Ang.Dim1.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 1") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()
# Plot Dimension 2
dat <- df %>% select(Condition,dim2.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim2.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="Ang.Dim2.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 2") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()
# Plot Dimension 3
dat <- df %>% select(Condition,dim3.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim3.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="Ang.Dim3.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 3") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()
# Plot Dimension 4
dat <- df %>% select(Condition,dim4.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim4.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="Ang.Dim4.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 4") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()

## Anxiety Condition
# Plot Dimension 1
options(digit=2)
df <- as.matrix(anx.dim.sv.se)
rn <- row.names(anx.dim.sv.se)
df <- data.frame(df)
dat <- df %>% select(Condition,dim1.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim1.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="anx.Dim1.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 1") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()
# Plot Dimension 2
dat <- df %>% select(Condition,dim2.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim2.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="anx.Dim2.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 2") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()
# Plot Dimension 3
dat <- df %>% select(Condition,dim3.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim3.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="anx.Dim3.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 3") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()
# Plot Dimension 4
dat <- df %>% select(Condition,dim4.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim4.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="anx.Dim4.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 4") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()

## Neutral Condition
# Plot Dimension 1
options(digit=2)
df <- as.matrix(neu.dim.sv.se)
rn <- row.names(neu.dim.sv.se)
df <- data.frame(df)
dat <- df %>% select(Condition,dim1.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim1.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="neu.Dim1.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 1") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()
# Plot Dimension 2
dat <- df %>% select(Condition,dim2.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim2.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="neu.Dim2.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 2") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()
# Plot Dimension 3
dat <- df %>% select(Condition,dim3.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim3.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="neu.Dim3.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 3") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()
# Plot Dimension 4
dat <- df %>% select(Condition,dim4.scale.value) %>% mutate(Subscale=rn) 
dat <- dat %>% spread(Condition,dim4.scale.value) %>% rename(Post = `Post`,Pre = `Pre`) 
dat<- dat %>% mutate(diff=abs(as.numeric(dat$Post)-as.numeric(dat$Pre))) %>%
  gather(Time,value,-Subscale,-diff)
png(filename="neu.Dim4.png", width = 1300, height = 700)
ggplot(aes(x = Subscale, y = as.numeric(as.character(unlist(dat$value))), color = Time, group=Time), 
       data = dat, label = value) + 
  geom_point(aes(shape=Time, size=4)) + geom_line() + 
  geom_text(aes(label = round(as.numeric(as.character(unlist(dat$value))), 2)), 
            method="smart.grid", color = "black",hjust=1,vjust=1) +
  xlab("Subscales") + ylab("Scale-value") + ggtitle("Core Profile Dimension 4") +
  geom_point(data=filter(dat,diff>1,Time=="Post"),aes(x=Subscale,y=as.numeric(value)),color="black",size=4)
dev.off()
