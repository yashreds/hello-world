if (!require("pacman"))install.packages("pacman")
p_load(psych)
p_depends(psych)
p_load(GPArotation)
b5=read.csv("b5.csv",header=TRUE)
colnames(b5)

pc=principal(b5,nfactors=5)
pc

pc1=principal(b5,nfactors=5,rotate="oblimin")
pc1

plot(pc1)
