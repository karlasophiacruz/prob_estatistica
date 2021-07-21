craibas.idade <- cut(obitos.craibas$idade, breaks)
plot.obitos.sexo <- table(obitos.craibas$sexo, obitos.craibas.idade)

barplot(plot.obitos.sexo, beside=TRUE,
        col=c("mistyrose2", "lightskyblue1", ylim=c(0, 500), names.arg= names,
        las=2,  main="Gráfico do número de obitos no estado de Craíbas")
        xlab="Idade dividida em anos", ylab= "Número de Óbitos")
          
legend(topright, pch=15, col=c("mistyrose2", "lightskyblue1"),
       legend= c("Homens", "Mulheres"), cex=1.25,
       bty="n")
###################################################################        
        
        
        



craibas.idade <- cut(dados.pacientes.craibas$idade, breaks)
plot..sexo <- table(dados.pacientes.craibas$sexo, craibas.idade)

barplot(plot.contaminacao.sexo, beside=TRUE,
        las=2, ylim=c(0, 12000)
        col=c("mistyrose2", "lightskyblue1", names.arg= names,
        xlab="Idade dividida em anos", ylab="Número de indivívuos contaminados",
        main="Gráfico do número de contaminados no estado de Craíbas")
        
legend(topright, pch=15, col=c("mistyrose2", "lightskyblue1"),
       legend= c("Homens", "Mulheres"), cex=1.25,
       bty="n")
######################################################################
