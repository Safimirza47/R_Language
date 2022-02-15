
omega(dataset)
choose.files()
dataset<- read.spss("C:\\Users\\My Computer\\OneDrive\\Research work\\Student's Perception of facebook for Academic Purposes\\New Data.sav", to.data.frame = TRUE)
Gen.Model<- 'PU=~PU1+PU2+PU3+PU4+PU5
PEoU=~PEoU1+PEoU2+PEoU3+PEoU4+PEoU5
SI=~SI1+SI2+SI3+SI4+SI5
FC=~FC1+FC2+FC3+FC4+FC5+FC6+FC7
CI=~CI1+CI2+CI3+CI4
SR=~SR1+SR2+SR3+SR4+SR5+SR6
WR=~WR1+WR2
DA=~DA1+DA2
CM=~CM1+CM2+CM3+CM4+CM5+CM6
C=~C1+C2+C3
RMS=~RMS1+RMS2'

#Fitting the model
fit<-cfa(Gen.Model, data=dataset)
fit
summary(fit,fit.measures=TRUE)
summary(fit,standardized=TRUE)

#Standardized loadings
inspect(fit, what = "std")

#R-square 
inspect(fit,'r2')

##fit indices
fitmeasures(fit)
fitmeasures(fit, c("gfi","agfi","nfi","cfi","rmsea","srmr","tli"))

#modification indices
modindices(fit,sort.=TRUE)
semPaths(fit,"std")

choose.files()
df<-read.spss("C:\\Users\\My Computer\\OneDrive\\Research work\\Student's Perception of facebook for Academic Purposes\\DATA ANALYSIS.sav", to.data.frame = TRUE)
df=na.omit(df)
#Second Model 
Second.model<- 'adoption=~PU+PEou+SI+FC+CI
Purpose=~SR+WR+DA
Edu_Use=~CM+C+RMS
Purpose~adoption
Edu_Use~adoption+Purpose 

'

#Fitting the model
fit2<-cfa(Second.model, data=df)
fit2
summary(fit2,fit.measures=TRUE)
summary(fit2,standardized=TRUE)

#Standardized loadings
inspect(fit2, what = "std")

#R-square 
inspect(fit2,'r2')

##fit indices
fitmeasures(fit2)
fitmeasures(fit2, c("gfi","agfi","nfi","cfi","rmsea","srmr","tli"))

#modification indices
modindices(fit2,sort.=TRUE)

semPaths(fit2,"std")

#Correlation Plot
cor(df[,7:17])
corr = round(cor(df[,7:17]),3)
corr

ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method="circle",
           colors = c("tomato2","thistle","springgreen3"),
           title = "correlogram of facebook users",
           ggtheme = theme_bw)

