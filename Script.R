################# carregar pacotes e ler os dados ################

#Limpa o Ambiente Global
rm(list=ls())


#Verificando o diretorio que o R esta direcionado
getwd()

#Local na máquina onde estão os dados e o arquivo LeBasesPosicaoFixa.R:
#setwd("C:\\POF 2008")


# carregar o pacote
library("janitor")
library("SAScii")
library("tidyverse")
library("dplyr")
library("caret")
library("Amelia")

#abrindo o arquivo

base_final<- readRDS("base_final.rds")
dim(base_final)
str(base_final)

##### declarando renda como um vetor numerico ####

base_final$pc_renda_monet<-as.numeric(base_final$pc_renda_monet)
summary(base_final$pc_renda_monet)

###### pobre vs nao pobre #####
attach(base_final)
i<-1
status<-0

for(i in 1:2674){
  status[i]<-ifelse(base_final$pc_renda_monet[i]>=499, "N_POBRE", "POBRE")
}

base_final2<-cbind(base_final, status)
table(base_final2$status)

#NA map

missmap(base_final2)
attach(base_final2)
base_final3<-subset(base_final2, select=-c(banheirosp, v02112, trabalha, v0407, escoadouro))
missmap(base_final3)

##### tudo fator

for(i in 1:74){
  base_final3[,i]<-as.factor(base_final3[,i])
}


### quais n se repetem

str(base_final3)
base_final4<-subset(base_final3, select=-c(cod_uf, v0306, crianca, condv_rendimento,
                                           condv_alimentacao, condv_moradia, condv_vestuario, condv_educacao,
                                           condv_saude, condv_lazer))
str(base_final4)


## quais dessa vari?veis tem um n?mero de levels alt?ssimo??

attach(base_final4)

i=1
for(i in 1:64){
  print("######################")
  print(i)
  print(levels(base_final4[,i]))
}

base_final5<-base_final4[,-c(1,2,3, 5, 9, 14, 15, 16, 19, 20, 21, 45, 51)]
str(base_final5)


#tabela de contig?ncia + teste de fisher + ordenar o vetor

i<-1
C<-0
for(i in 1:51){
  A<-chisq.test(table(base_final5[,i], base_final5[,51]), correct=F, simulate.p.value = F)
  print(i)
  print(A)
  A1<-A$p.value
  B<-colnames(base_final5)
  B1<-B[i]
  C1<-c(A1, B1)
  C<-rbind(C1, C)
}

C<-data.frame(C)

colnames(C) <- c("p_value","coluna")
C

library(dplyr)

tidy_eval_arrange <- function(.data, var) {
  .data %>%
    arrange({{ var }})
}
tidy_eval_arrange(C, p_value)




C_order = arrange(C, desc(p_value))

C_order <- C[order(C$p_value), ]; C_order


#removendo as independentes

#43,19,26,3,50,29,22,20,27


base_final5 = as.data.frame(base_final5)

base_final6 <- subset( base_final5, select = -c(maacmuc, emi, v61044, num_uc, seminstrucao, asc, masemc, efc, cod_informante))



# Risco relativo


library(epiR)

for(i in 1:41){
  A<-table(base_final6[,i], base_final6[,42])
  print(i)
  B<-colnames(base_final6)
  print(B[i])
  print(A)
  print(epi.2by2(A, method = "cohort.count", conf.level = 0.95))
}


#####machine learning svm#########

library("e1071")

#View(base_final3)

?svm

### embaralhar os dados

head(base_final6)
base_final7 <- base_final6[sample(nrow(base_final6)),]
head(base_final7)
dim(base_final7)
attach(base_final7)
base_final5<-base_final4[, c("status", "agua", "comodos", "raca", "energia", "numero_de_pessoas", "domicilioproprio")]

### treinamento e teste

dim(base_final7)
head(base_final7)

treino<-base_final7[1:2000,]
dim(treino)
teste<-base_final7[2001:2674,-42]
resultado<-base_final6[2001:2674,42]

table(treino$status)
table(resultado)

dim(treino)
dim(teste)
length(resultado)
#?svm
svmfit = svm(status ~ .,
             kernel = "linear", cost = 10,
             scale = FALSE, data=treino, type="C-classification")

test_pred <- predict(svmfit, newdata = teste)

confusionMatrix(table(test_pred, resultado))


table(test_pred)
table(resultado)
table(test_pred, resultado)


############# ksvm function ########

?ksvm


library(kernlab)


svmfit1 <- ksvm(status ~ ., data=treino,
                type = "C-svc", kernel = "rbfdot")

test_pred <- predict(svmfit1, newdata = teste)

confusionMatrix(table(test_pred, resultado))


table(test_pred)
table(resultado)
table(test_pred, resultado)

