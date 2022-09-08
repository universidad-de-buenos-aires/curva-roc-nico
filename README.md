# curva-roc-nico
#CÓDIGO CRAFT STORY 21
#CURVA ROC

#instalo el paquete
install.packages("pROC")
library(pROC)
install.packages("knitr")
install.packages("tinytex")
install.packages("ipak")
install.packages("psych")
library(psych)
packages <- c("haven","apa","dplyr","tidyBF","BayesFactor","ggstatsplot")


#nombre de la base
craft1 <- BASE_CRAFT1


#Diferencias entre grupos
t.test(craft$edad~craft$dx_clinico)
t.test(craft$sex~craft$dx_clinico)
t.test(craft$escolaridad~craft$dx_clinico)

with(craft,tapply(edad,dx_clinico,sd))
with(craft,tapply(sex,dx_clinico,mean))
with(craft,tapply(escolaridad,dx_clinico,mean))

with(craft_normas,tapply(craft_inm_text,grupo_edad,sd))

#creación de la curva
#curva CRAFT inmediato
craft_inm_curva <- roc(craft$dx_clinico, craft$craft_inm_text)
auc(craft_inm_curva)
plot(craft_inm_curva)

#curva RAVLT inmediato
ravlt_inm_curva <- lines.roc(craft$dx_clinico, craft$ravlt_aprendizaje, col = "#32374c")
auc(ravlt_inm_curva)
plot(ravlt_inm_curva)

#curva CRAFT diferido
craft_dif_curva <- lines.roc(craft$dx_clinico, craft$craft_dif_text, col = "#32374c")
auc(craft_dif_curva)
plot(craft_dif_curva)

#curva RAVLT diferido
ravlt_dif_curva <- lines.roc(craft$dx_clinico, craft$ravlt_evocacion, col = "#32374c")
auc(ravlt_dif_curva)
plot(ravlt_dif_curva)

#curva CRAFT reconocimiento
craft_rec_curva <- lines.roc(craft$dx_clinico, craft$craft_total, col = "#32374c")
auc(craft_rec_curva)
plot(craft_rec_curva, col = "#fba31b")

#curva RAVLT reconocimiento
ravlt_rec_curva <- lines.roc(craft$dx_clinico, craft$ravlt_reconocimiento, col = "#32374c")
auc(ravlt_rec_curva)
plot(ravlt_rec_curva)


#graficamos ambas curvas (REC)
ravlt_rec_curva <- lines.roc(craft$dx_clinico, craft$ravlt_reconocimiento, col = "#32374c")
curvas_rec <- roc.test(craft_rec_curva,ravlt_rec_curva)
text(.5, .5, labels=paste("p-value =", format.pval(curvas_rec$p.value)), adj=c(0, .5))
legend("bottomright", legend=c("Craft Story 21 reconocimiento", "Reconocimiento - RAVLT"), col =c("#fba31b", "#32374c"), lwd=2)

#intervalos de confianza, punto de corte, etc.
rocobj <- roc(craft$dx_clinico, craft$craft_total, main = "Craft Story 21: Curva ROC con intervalos de confianza", percent = TRUE, ci = TRUE, print.auc = TRUE)
ciobj <- ci.se(rocobj, specificities = seq(0, 100, 5))
plot(ciobj, type="shape", col="#1c61b6AA")
plot(ciobj, of = "thresholds", thresholds = "best")   

corte <- plot.roc(craft$dx_clinico, craft$craft_total, main = "Curva ROC con IC", percent = TRUE, ci = TRUE, of = "thresholds", thresholds = "best", print.thres = "best")
corte <- plot.roc(craft$dx_clinico, craft$cra, smooth = TRUE, main = "Curva ROC con IC", percent = TRUE, ci = TRUE, of = "thresholds", thresholds = "best", print.thres = "best", print.auc = TRUE)
plot(ciobj, type="shape", col="#1c61b6AA")
