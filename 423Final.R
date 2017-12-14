file<-"http://www.math.mcgill.ca/dstephens/Regression/Data/Project/nhanes-sub.csv"
data<-read.csv(file)


gender<-data$gender
age<-data$age
race3<-data$race3
educ<-data$educ
married<-data$married
hhinc<-data$hhinc
poverty<-data$poverty
homerooms<-data$homerooms
homeown<-data$homeown
weight<-data$weight
bmi<-data$bmi
pulse<-data$pulse
bpsys<-data$bpsys
bpdia<-data$bpdia
dirchol<-data$dirchol
totchol<-data$totchol
physact<-data$physact
alcday<-data$alcday
alcyear<-data$alcyear

n<-length(bpsys)

bpdia.fit1<-lm(bpdia ~ age + alcday + alcyear + bmi + dirchol + educ + gender + hhinc + homeown + homerooms + married + physact + poverty+ pulse+ race3 + totchol + weight)

# remove some effects
bpdia.fit2<-step(bpdia.fit1,test='F')
anova(bpdia.fit1,bpdia.fit2,test='F')
# high p value fit 2 is adequate

# add some second order interactions
bpdia.fit3<-step(bpdia.fit2,test='F', scope= list(upper=update(bpdia.fit2, ~.*.), lower= bpdia.fit2))
anova(bpdia.fit2,bpdia.fit3, test='F')
# low p value fit 3 is adequate

# model adequacy criteria
criteria.eval<-function(fit.obj,nv,bigsig.hat){
    cvec<-rep(0,5)
    SSRes<-sum(residuals(fit.obj)^2)
    p<-length(coef(fit.obj))
    cvec[1]<-summary(fit.obj)$r.squared
    cvec[2]<-summary(fit.obj)$adj.r.squared
    cvec[3]<-SSRes/bigsig.hat^2-n+2*p
    #AIC in R computes
    # n*log(sum(residuals(fit.obj)^2)/n)+2*(length(coef(fit.obj))+1)+n*log(2*pi)+n
    cvec[4]<-AIC(fit.obj)
    #BIC in R computes
    # n*log(sum(residuals(fit.obj)^2)/n)+log(n)*(length(coef(fit.obj))+1)+n*log(2*pi)+n
    cvec[5]<-BIC(fit.obj)
    return(cvec)
}
bigs.hat<-summary(bpdia.fit3)$sigma
cvals<-matrix(0,nrow=3,ncol=5)
cvals[1,]<-criteria.eval(bpdia.fit1,n,bigs.hat)
cvals[2,]<-criteria.eval(bpdia.fit2,n,bigs.hat)
cvals[3,]<-criteria.eval(bpdia.fit3,n,bigs.hat)
Criteria<-data.frame(cvals)
names(Criteria)<-c('Rsq','Adj.Rsq','Cp','AIC','BIC')
rownames(Criteria)<-c('1', '2', '3')
round(Criteria,4)

# plots
par(mfrow=c(3,3),mar=c(4,2,1,2))
plot(age,residuals(bpdia.fit3),pch=19,cex=0.75, main="Age", xlab ="")
abline(h=0, lty=2)
plot(gender,residuals(bpdia.fit3),pch=19,cex=0.75, main = "Gender")
abline(h=0, lty=2)
plot(hhinc,residuals(bpdia.fit3),pch=19,cex=0.75,xaxt="n", main="Household Income", xlab ="")
axis(1, at=1:12, labels=levels(hhinc), las = 2, cex.axis = 0.8)
abline(h=0, lty=2)
plot(married,residuals(bpdia.fit3),pch=19,cex=0.75,xaxt="n" , main="Marital Status", xlab ="")
axis(1, at=1:6, labels=levels(married), las = 2, cex.axis = 0.8)
abline(h=0, lty=2)
plot(poverty,residuals(bpdia.fit3),pch=19,cex=0.75, main="Poverty")
abline(h=0, lty=2)
plot(pulse,residuals(bpdia.fit3),pch=19,cex=0.75, main="Pulse")
abline(h=0, lty=2)
plot(race3,residuals(bpdia.fit3),pch=19,cex=0.75,xaxt="n", main="Race", xlab = "")
axis(1, at=1:6, labels=levels(race3), las = 2, cex.axis = 0.8)
abline(h=0, lty=2)
plot(totchol,residuals(bpdia.fit3),pch=19,cex=0.75, main="Total HDL Cholesterol")
abline(h=0, lty=2)
plot(weight,residuals(bpdia.fit3),pch=19,cex=0.75, main="Weight")
abline(h=0, lty=2)

coef(bpdia.fit3)




bpsys.fit1<-lm(bpsys ~ age + alcday + alcyear + bmi + dirchol + educ + gender + hhinc + homeown + homerooms + married + physact + poverty+ pulse+ race3 + totchol + weight)
bpsys.fit2<-step(bpsys.fit1,test='F')
anova(bpsys.fit1,bpsys.fit2,test='F')
# high P value, fit2 is adequate

drop1(bpsys.fit2, test='F')
bpsys.fit3<-update(bpsys.fit2, ~. - dirchol - physact)
anova(bpsys.fit2,bpsys.fit3,test='F')
# add some second order interactions
add1(bpsys.fit3,test='F', scope= update(bpsys.fit3, ~.*.))
bpsys.fit4<-update(bpsys.fit3, ~. +age:gender+age:poverty + age:race3 + age:totchol + alcday:bmi + alcday:educ + alcday:hhinc + alcday:poverty + alcday:pulse + alcday:race3 + educ:hhinc + educ:married + gender:hhinc + gender:married + hhinc:married + hhinc:race3 + homeown:married + married:race3 + married:totchol)
anova(bpsys.fit4,bpsys.fit3,test='F')

# drop some variable
drop1(bpsys.fit4, test='F')
bpsys.fit5<-update(bpsys.fit4, ~. - age:race3 - alcday:bmi - alcday:poverty - alcday:pulse - alcday:race3   )
anova(bpsys.fit5,bpsys.fit4,test='F')

# add some
add1(bpsys.fit5, test="F", scope = update(bpsys.fit3, ~.*.))
bpsys.fit6<- update(bpsys.fit5, ~. + alcyear:hhinc+alcyear:married +alcyear:totchol + educ:poverty + pulse:totchol)
anova(bpsys.fit5, bpsys.fit6, test='F')


bigs.hat<-summary(bpsys.fit6)$sigma
cvals<-matrix(0,nrow=6,ncol=5)
cvals[1,]<-criteria.eval(bpsys.fit1,n,bigs.hat)
cvals[2,]<-criteria.eval(bpsys.fit2,n,bigs.hat)
cvals[3,]<-criteria.eval(bpsys.fit3,n,bigs.hat)
cvals[4,]<-criteria.eval(bpsys.fit4,n,bigs.hat)
cvals[5,]<-criteria.eval(bpsys.fit5,n,bigs.hat)
cvals[6,]<-criteria.eval(bpsys.fit6,n,bigs.hat)
Criteria<-data.frame(cvals)
names(Criteria)<-c('Rsq','Adj.Rsq','Cp','AIC','BIC')
rownames(Criteria)<-c('All main effects', '2', '3', '4', '5', '6')
round(Criteria,4)



plot(age,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(alcday,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(alcyear,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(bmi,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(educ,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(gender,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(hhinc,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(homeown,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(married,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(poverty,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(pulse,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(race3,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)
plot(totchol,residuals(bpsys.fit6),pch=19,cex=0.75)
abline(h=0, lty=2)

coef(bpsys.fit6)