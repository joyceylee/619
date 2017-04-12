```{r}
#Final model for 619 project 

load("H:\\BSF Processing Data\\Datasets\\Dataset for R\\Data Processing\\semdataCtl\\Everything4.RData")

library(lavaan)
Model1_2<-'

###Measurement model
#Coparenting self-report by father
CopW2.d =~ CO1_A.d.W2 + CO1_B.d.W2 + CO1_C.d.W2 + CO1_D.d.W2 + CO1_E.d.W2 + CO1_F.d.W2 + CO1_G.d.W2 + CO1_H.d.W2 + CO1_I.d.W2 + CO1_J.d.W2 
CopW3.d =~ CO1_A.d.W3 + CO1_B.d.W3 + CO1_C.d.W3 + CO1_D.d.W3 + CO1_E.d.W3 + CO1_F.d.W3 + CO1_G.d.W3 + CO1_H.d.W3 + CO1_I.d.W3 + CO1_J.d.W3

#Coparenting self-report by mother
CopW2.m =~ CO1_A.m.W2 + CO1_B.m.W2 + CO1_C.m.W2 + CO1_m.d.W2 + CO1_E.m.W2 + CO1_F.m.W2 + CO1_G.m.W2 + CO1_H.m.W2 + CO1_I.m.W2 + CO1_J.m.W2 
CopW3.m =~ CO1_A.m.W3 + CO1_B.m.W3 + CO1_C.m.W3 + CO1_m.d.W3 + CO1_E.m.W3 + CO1_F.m.W3 + CO1_G.m.W3 + CO1_H.m.W3 + CO1_I.m.W3 + CO1_J.m.W3

#Second order coparenting latent variables 
CopW2 =~ CopW2.d + CopW2.m
CopW3 =~ CopW3.d + CopW3.m

#Father engagement in caregiving 
CareW2.d =~ CO3_F.W2 + CO3_G.W2 + CO3_H.W2 
CareW3.d =~ CO3_F.W3 + CO3_G.W3 + CO3_H.W3

#Father engagement in play 
PlayW2.d =~ CO3_B.W2 + CO3_C.W2 + CO3_D.W2 + CO3_E.W2 
PlayW3.d =~ CO3_B.W3 + CO3_C.W3 + CO3_D.W3 + CO3_E.W3

##Correlated residuals for repeated measures 
CO1_A.d.W2 ~~ CO1_A.d.W3 
CO1_B.d.W2 ~~ CO1_B.d.W3 
CO1_C.d.W2 ~~ CO1_C.d.W3 
CO1_D.d.W2 ~~ CO1_D.d.W3 
CO1_E.d.W2 ~~ CO1_E.d.W3 
CO1_F.d.W2 ~~ CO1_F.d.W3 
CO1_G.d.W2 ~~ CO1_G.d.W3 
CO1_H.d.W2 ~~ CO1_H.d.W3 
CO1_I.d.W2 ~~ CO1_I.d.W3 
CO1_J.d.W2 ~~ CO1_AJ.d.W3 

CO1_A.m.W2 ~~ CO1_A.m.W3 
CO1_B.m.W2 ~~ CO1_B.m.W3 
CO1_C.m.W2 ~~ CO1_C.m.W3 
CO1_D.m.W2 ~~ CO1_D.m.W3 
CO1_E.m.W2 ~~ CO1_E.m.W3 
CO1_F.m.W2 ~~ CO1_F.m.W3 
CO1_G.m.W2 ~~ CO1_G.m.W3 
CO1_H.m.W2 ~~ CO1_H.m.W3 
CO1_I.m.W2 ~~ CO1_I.m.W3 
CO1_J.m.W2 ~~ CO1_AJ.m.W3 

CO3_F.W2 ~~ CO3_F.W3 
CO3_G.W2 ~~ CO3_G.W3 
CO3_H.W2 ~~ CO3_H.W3 

CO3_B.W2 ~~ CO3_B.W3 
CO3_C.W2 ~~ CO3_C.W3 
CO3_D.W2 ~~ CO3_D.W3 
CO3_E.W2 ~~ CO3_E.W3 

##Suggestions from modification indices 
CO3_C.W2 ~~ CO3_D.W2
CO3_C.W3 ~~ CO3_D.W3
PlayW2.d ~~ CareW2.d 
CopW2.d ~~ CopW3.d
CopW2.m ~~ CopW3.m

###Structural model (controls upfront) 
CopW2 ~ BlackOnly + HispanicCombo + OtherCombo + Work + BSF + HSDiploma + EduOther + MPF + IPVW2.d2 + IPVW2.m2 
CareW2.d ~ BlackOnly + HispanicCombo + OtherCombo + Work + BSF + HSDiploma + EduOther + MPF + IPVW2.d2 + IPVW2.m2 
PlayW2.d ~ BlackOnly + HispanicCombo + OtherCombo + Work + BSF + HSDiploma + EduOther + MPF + IPVW2.d2 + IPVW2.m2 

CopW3 ~ BlackOnly + HispanicCombo + OtherCombo + Work + BSF + HSDiploma + EduOther + MPF + IPVW2.d2 + IPVW2.m2 + CopW2
CareW3.d ~ BlackOnly + HispanicCombo + OtherCombo + Work + BSF + HSDiploma + EduOther + MPF + IPVW2.d2 + IPVW2.m2 + CareW2.d 
PlayW3.d ~ BlackOnly + HispanicCombo + OtherCombo + Work + BSF + HSDiploma + EduOther + MPF + IPVW2.d2 + IPVW2.m2 + PlayW2.d
'

Model1Fit_2<-sem(Model1_2, data=semdataCtl5.IPV4, estimator="MLR", missing="fiml")
summary(MOdel1Fit_2, fit.measure=T, standardized=T, rsquare=T)
#residuals(Model1Fit_2, type='cor')
```
