library(lmer4)
library(lmerTest)
library(readxl)
library(car)
library(emmeans)
install.packages("lsmeans")
install.packages("pbkrtest")
library(stargazer)
library(readxl)

library(readxl)
paired_SY38 <- read_excel("Dropbox/Thesis_MH/Excel_Docs/whole_image_density/paired_SY38.xlsx")
View(paired_SY38)


#change these to the right form 
paired_SY38$Mouse-as.factor(paired_SY38$Mouse)
paired_SY38$Image_type<-as.factor(paired_SY38$Image_type)
paired_SY38$Puncta_density<-as.numeric(paired_SY38$Puncta_density)
paired_SY38batch<-as.factor(paired_SY38$batch)


#mixed model comparison of puncta density and image type within paired data. 
MM<-lmer(Puncta_density~Image_type+(1|Mouse)+(1|batch), data=paired_SY38)
summary(MM) 

#check the residuals 

qqp(resid(MM))

#histogram - best I could get. 

hist(paired_vs_unpaired_R$Puncta_density)

# post-hoc test
posthoc<-emmeans(MM, list(pairwise ~ Image_type), adjust = "tukey")

summary(posthoc)

posthoc_data<-as.data.frame(posthoc[2:1])

# now do the same for the unpaired SY38 

library(readxl)
unpaired_SY38 <- read_excel("Dropbox/Thesis_MH/Excel_Docs/whole_image_density/unpaired_SY38.xlsx")
View(unpaired_SY38)



#change these to the right form 
unpaired_SY38$Mouse<-as.factor(unpaired_SY38$Mouse)
unpaired_SY38$Image_type<-as.factor(unpaired_SY38$Image_type)
unpaired_SY38$Puncta_density<-as.numeric(unpaired_SY38$Puncta_density)
unpaired_SY38batch<-as.factor(unpaired_SY38$batch)


#mixed model comparison of puncta density and image type within paired data. 
unpairedSY38MM<-lmer(Puncta_density~Image_type+(1|Mouse)+(1|batch), data=unpaired_SY38)
summary(unpairedSY38MM) 

#check the residuals 

qqp(resid(unpairedSY38MM))

#histogram - best I could get. 

hist(unpairedSY38$Puncta_density)

# post-hoc test
posthoc<-emmeans(unpairedSY38MM, list(pairwise ~ Image_type), adjust = "tukey")

summary(posthoc)

posthoc_data<-as.data.frame(posthoc[2:1])

# now do the same for the unpaired SY38 

library(readxl)
unpaired_SY38 <- read_excel("Dropbox/Thesis_MH/Excel_Docs/whole_image_density/unpaired_SY38.xlsx")
View(unpaired_SY38)



#change these to the right form 
unpaired_SY38$Mouse<-as.factor(unpaired_SY38$Mouse)
unpaired_SY38$Image_type<-as.factor(unpaired_SY38$Image_type)
unpaired_SY38$Puncta_density<-as.numeric(unpaired_SY38$Puncta_density)
unpaired_SY38batch<-as.factor(unpaired_SY38$batch)


#mixed model comparison of puncta density and image type within paired data. 
unpairedSY38MM<-lmer(Puncta_density~Image_type+(1|Mouse)+(1|batch), data=unpaired_SY38)
summary(unpairedSY38MM) 

#check the residuals 

qqp(resid(unpairedSY38MM))

#histogram - best I could get. 

hist(unpairedSY38$Puncta_density)

# post-hoc test
posthoc<-emmeans(unpairedSY38MM, list(pairwise ~ Image_type), adjust = "tukey")

summary(posthoc)

posthoc_data<-as.data.frame(posthoc[2:1])

# now do the same for the unpaired SY38 

library(readxl)
unpaired_SY38 <- read_excel("Dropbox/Thesis_MH/Excel_Docs/whole_image_density/unpaired_SY38.xlsx")
View(unpaired_SY38)



#change these to the right form 
unpaired_SY38$Mouse<-as.factor(unpaired_SY38$Mouse)
unpaired_SY38$Image_type<-as.factor(unpaired_SY38$Image_type)
unpaired_SY38$Puncta_density<-as.numeric(unpaired_SY38$Puncta_density)
unpaired_SY38batch<-as.factor(unpaired_SY38$batch)


#mixed model comparison of puncta density and image type within paired data. 
unpairedSY38MM<-lmer(Puncta_density~Image_type+(1|Mouse)+(1|batch), data=unpaired_SY38)
summary(unpairedSY38MM) 

#check the residuals 

qqp(resid(unpairedSY38MM))

#histogram - best I could get. 

hist(unpairedSY38$Puncta_density)

# post-hoc test
posthoc<-emmeans(unpairedSY38MM, list(pairwise ~ Image_type), adjust = "tukey")

summary(posthoc)

posthoc_data<-as.data.frame(posthoc[2:1])



# now do the same for the unpaired SY38 

library(readxl)
unpaired_SY38 <- read_excel("Dropbox/Thesis_MH/Excel_Docs/whole_image_density/unpaired_SY38.xlsx")
View(unpaired_SY38)



#change these to the right form 
unpaired_SY38$Mouse<-as.factor(unpaired_SY38$Mouse)
unpaired_SY38$Image_type<-as.factor(unpaired_SY38$Image_type)
unpaired_SY38$Puncta_density<-as.numeric(unpaired_SY38$Puncta_density)
unpaired_SY38$batch<-as.factor(unpaired_SY38$batch)


#mixed model comparison of puncta density and image type within paired data. 
unpairedSY38MM<-lmer(Puncta_density~Image_type+(1|Mouse)+(1|batch), data=unpaired_SY38)
summary(unpairedSY38MM) 

#check the residuals 

qqp(resid(unpairedSY38MM))


# post-hoc test
posthoc<-emmeans(unpairedSY38MM, list(pairwise ~ Image_type), adjust = "tukey")

summary(posthoc)

posthoc_data<-as.data.frame(posthoc[2:1])

## now do the same for the unpaired PSD95

library(readxl)
unpaired_PSD95 <- read_excel("Dropbox/Thesis_MH/Excel_Docs/whole_image_density/unpaired_PSD95.xlsx")
View(unpaired_PSD95)


#change these to the right form 
unpaired_PSD95$Mouse<-as.factor(unpaired_PSD95$Mouse)
unpaired_PSD95$Image_type<-as.factor(unpaired_PSD95$Image_type)
unpaired_PSD95$Puncta_density<-as.numeric(unpaired_PSD95$Puncta_density)
unpaired_PSD95$batch<-as.factor(unpaired_PSD95$batch)


#mixed model comparison of puncta density and image type within paired data. 
unpairedPSD95MM<-lmer(Puncta_density~Image_type+(1|Mouse)+(1|batch), data=unpaired_PSD95)
summary(unpairedPSD95MM) 

#check the residuals 

qqp(resid(unpairedPSD95MM))



# post-hoc test
posthoc<-emmeans(unpairedPSD95MM, list(pairwise ~ Image_type), adjust = "tukey")

summary(posthoc)

posthoc_data<-as.data.frame(posthoc[2:1])


# doing it for all of them in one 
library(readxl)
paired_unpaired_R <- read_excel("Dropbox/Thesis_MH/Excel_Docs/paired_unpaired_R.xlsx")
View(paired_unpaired_R)

paired_unpaired_R$Mouse<-as.factor(paired_unpaired_R$Mouse)
paired_unpaired_R$Image_type<-as.factor(paired_unpaired_R$Image_type)
paired_unpaired_R$Puncta_density<-as.numeric(paired_unpaired_R$Puncta_density)
paired_unpaired_R$batch<-as.factor(paired_unpaired_R$batch)

#mixed model comparison of puncta density and image type within paired data. 
MM<-lmer(Puncta_density~Image_type+(1|Mouse)+(1|batch), data=paired_unpaired_R)
summary(MM) 

posthoc<-emmeans(MM, list(pairwise ~ Image_type), adjust = "tukey")

summary(posthoc)

qqp(resid(MM))
