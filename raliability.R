library(tidyverse)
library(psych)
library(readxl)

craft <- read_excel("BASE CRAFT1.xlsx")
craft1<-craft %>% filter(dx_clinico==0)

names(craft1)

preguntas<-craft1 %>% select(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)

alpha(preguntas)
