library("lattice")
dotplot(WC_AT$AT, Main = "Occurence of AT as per waist size",col "Blue")
dotplot(WC_AT$Waist, main ="Waist size", col ="Red")
boxplot(WC_AT$Waist, col= "Red")
boxplot(WC_AT$AT, col = " blue")
reg_model<- lm(AT~Waist, data = WC_AT)
summary (reg_model)
predict(reg_model, data.frame(waist= c(

         
