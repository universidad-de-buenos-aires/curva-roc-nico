library(pROC)


library(readxl)
craft <- read_excel("BASE CRAFT1.xlsx")

craft_inm <- roc(craft$dx_clinico, craft$craft_inm_text)
craft_dif <- roc(craft$dx_clinico, craft$craft_dif_text)
ravlt_dif<- roc(craft$dx_clinico, craft$ravlt_evocacion)
craft_recon<-roc(craft$dx_clinico, craft$craft_total)
ravlt_rec <- roc(craft$dx_clinico, craft$ravlt_reconocimiento)
ravlt_inme<-roc(craft$dx_clinico, craft$ravlt_aprendizaje)

plot(craft_inm, col = 1, lty = 1 )
plot(craft_dif, col = 4, lty = 3, add = TRUE) #si pones el argumento add lo suma al otro
plot(ravlt_dif, col = 4, lty = 2, add = TRUE)
plot(craft_recon, col = "#fba31b", lty = 1, main = "ROC",add = TRUE)
plot(ravlt_rec, col = 4, lty = 2, add = TRUE)
plot(ravlt_inme, col = 4, lty = 2, add = TRUE)