#Universidade Federal de Juiz de Fora
#Programa de Pós Graduação em Saúde Coletiva
#Classificação automatizada de pares em relacionamentos probabilísticos de bancos de dados

#ler o banco de dados
library(foreign)
banco <- read.dbf("banco.dbf")

#selecionando uma amostra
library(dplyr)
bancoamostra<- sample_n(banco, 300)
write.dbf(bancoamostra,"bancoamostra.dbf")

#Obs.: Abrir o banco completo com todos os pares no Reclink, marcar todos os pares como verdadeiros, executar e criar arquivo com nomes.
#Renomear esse arquivo, exemplo:bancover

#No R fazer merge dos bancos amostra e verdadeiros para conferir no excel
#ler os bancos
bancoamostra<-read.dbf('bancoamostra.dbf')
bancover<-read.dbf('bancover.dbf')

#criar variável pares
bancoamostra$pares <- as.factor(paste(bancoamostra$REFREC,sep=" ",bancoamostra$COMPREC))
bancover$pares <- as.factor(paste(bancover$REFREC,sep=" ",bancover$COMPREC))

#verificar os nomes
names(bancoamostra)
names(bancover)

#Fazer o merge
bancomerge <- merge(bancoamostra,bancover,by="pares",all.x=TRUE)
write.dbf(bancomerge, "bancomerge.dbf")

names(bancomerge)

#Retirar variáveis excedentes
excluir<-c("COMPREC.y", "REFREC.y", "SCORE.y")
bancomerge <- bancomerge[,!(names(bancomerge)%in% excluir)]
write.dbf(bancomerge, "A16.dbf")

#Conferir o passo bancomerge no Excel
#Observação: 
#marcar em match: X para par e ! para não par

#Após conferir voltar com o bancomerge para o R
bancomerge<- read.csv2("bancomerge.csv")
names(bancomerge)

# Renomear variáveis
names(bancomerge)<-c("pares", "COMPREC", "REFREC", "SCORE","MATCH", "C_NOMEPAD", "C_PNOME",    "C_UNOME",  "C_MAEPAD","C_PMAE", "C_UMAE", "C_DTNASC", "C_D_EXAME2", "C_ANO_NASC", "C_CNSUS", "C_MUN_COD",  "C_FONTE", "C_MES_EXAM", "C_CNES",  "C_CNES_REQ", "R_NOMEPAD",  "R_PNOME", "R_UNOME", "R_MAEPAD", "R_PMAE", "R_UMAE", "R_DTNASC", "R_D_EXAME2", "R_ANO_NASC", "R_CNSUS", "R_MUN_COD", "R_FONTE", "R_MES_EXAM", "R_CNES", "R_CNES_REQ")
write.dbf(bancomerge,"bancomerge.dbf")

str(bancomerge)

#Converter variável SCORE número
bancomerge$SCORE<-as.character(bancomerge$SCORE)
bancomerge$SCORE <- as.numeric(bancomerge$SCORE)

write.dbf(bancomerge,"bancomerge.dbf")

str(bancomerge)

#Curva Precision-Recall e Acurácia

# lendo os bancos
library(foreign)
bancomerge<- read.dbf("bancomerge.dbf")

# ajeitando o banco (separando os escores dos pares verdadeiros e falsos em bancos diferentes)
bancomerge$par<-0
bancomerge$par[bancomerge$MATCH=="X"]<-1

write.dbf(bancomerge,"bancomerge.dbf")

# fazendo a curva precision-recall
library(PRROC)
pr <- pr.curve(scores.class0 = bancomerge[bancomerge$MATCH=="X",4], scores.class1 = bancomerge[bancomerge$MATCH=="!",4],  curve=TRUE, max.compute=T)
print(pr)
plot(pr,scale.color = rev(gray.colors(100)))
curva.pr <- as.data.frame(pr$curve)
names(curva.pr) <- c("Recall","Precision","Escore")
curva.pr$F1 = 2* (curva.pr$Recall*curva.pr$Precision / (curva.pr$Recall+curva.pr$Precision))
maximoF1 = max(curva.pr$F1)
best.cutoff = curva.pr$Escore[which(curva.pr$F1==maximoF1)]
print(best.cutoff)

#Acurácia do ponto de corte
library(ROCR)
pred <- prediction(predictions = bancomerge$SCORE, labels = bancomerge$par)

perf1 <- performance(pred, "acc")
plot(perf1, main="Acuracia")
acuracia <- as.data.frame(cbind(perf1@x.values[[1]],perf1@y.values[[1]]))
names(acuracia) <- c("Escore","Acuracia")
max.acuracia = max(acuracia$Acuracia)
best.cutoff <- acuracia$Escore[which(acuracia$Acuracia==max.acuracia)]
print(best.cutoff)

library(pROC)
curva.roc <- roc(bancomerge$par,bancomerge$SCORE,plot=T,levels=c("0","1"),ci=T,auc.polygon=T,
                 print.auc=T,print.thres="best",legacy.axes=T)
coords(curva.roc,x="best",transpose=F,ret=c("threshold","precision","recall","accuracy"))
ci.coords(curva.roc,x="best",transpose=F,ret=c("threshold","precision","recall","accuracy"))

#No RecLink aplicar o ponto de corte encontrado no banco. 