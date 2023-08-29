
############## CONFUSION MATRIX ###############

############ GAP MONITORING - 1 y 

pred_gapfield <- as.factor(subset(gap, gap$GAP_Field_Monit == 1, select=c("classe_SR"), 2))
pred_gapfield

actual_gapfield <- as.factor(subset(gap, gap$GAP_Field_Monit == 1, select=c("classe_BRK"), 2))


results_gapmonit <- confusionMatrix(pred_gapfield, actual_gapfield)
results_gapmonit


###### ALL GAPS - 2 y 

pred <- as.factor(gap$classe_SR)
actual <- as.factor(gap$classe_BRK)


results <- confusionMatrix(pred, actual)
results


####################################################################
## Filtering pairs that were captured gaps 

dados_pares <- subset(gap, classe_SR == 1 & classe_BRK == 1 , select=c("Area_SR_m2", "Area_Brokaw_m2"))


####################################################################
## Testing for normality ##
dif <- dados_pares$Area_SR_m2 - dados_pares$Area_Brokaw_m2
results1 <- shapiro.test(dif)

####################################################################
## Taking the logarithm of 
Log_area_SR <- log(dados_pares$Area_SR_m2)
Log_area_BRK <- log(dados_pares$Area_Brokaw_m2)
Log_dif <- Log_area_SR - Log_area_BRK
####################################################################

## Paired t-test ##
t.test(dados_pares$Area_Brokaw_m2, dados_pares$Area_SR_m2, paired = TRUE, alternative = "two.sided")

# Log 
t.test(Log_area_BRK, Log_area_SR, paired = TRUE, alternative = "two.sided")

##Perimeter
## Filtering pairs that captured gaps 
dados_pares_perim <- subset(gap, classe_SR == 1 & classe_BRK == 1 , select=c("Perimeter_SR_m", "Perimeter_Brokaw_m"))


## Testing for normality ##
dif_perim <- dados_pares_perim$Perimeter_SR_m - dados_pares_perim$Perimeter_Brokaw_m
results3 <- shapiro.test(dif_perim)

results3

## Paired t-test ##
t.test(dados_pares_perim$Perimeter_Brokaw_m, dados_pares_perim$Perimeter_SR_m, paired = TRUE, alternative = "two.sided")

## GSCI 

## Filtering pairs that captured gaps 
dados_pares_gsci <- subset(gap, classe_SR == 1 & classe_BRK == 1 , select=c("GSCI_SR", "GSCI_BRK"))

## Testing for normality ##
dif_gsci <- dados_pares_gsci$GSCI_SR - dados_pares_gsci$GSCI_BRK
results4 <- shapiro.test(dif_gsci)


# Log 
Log_gsci_SR <- log(dados_pares_gsci$GSCI_SR)
Log_gsci_BRK <- log(dados_pares_gsci$GSCI_BRK)
Log_dif_gsci <- Log_gsci_SR - Log_gsci_BRK

shapiro.test(dif_gsci) 


## Paired t-test ##
t.test(dados_pares_gsci$GSCI_BRK, dados_pares_gsci$GSCI_SR, paired = TRUE, alternative = "two.sided")


