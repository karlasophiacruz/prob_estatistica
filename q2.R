dados <- read.csv("dadosPacientes2021.csv")

dados[dados == "Mascuino"] <- "Masculino"

check_20 <- grepl(("2020"), dados$data_resultado_exame, fixed = TRUE)
check_21 <- grepl(("2021"), dados$data_resultado_exame, fixed = TRUE)

dados <- subset(dados, check_20 == TRUE | check_21 == TRUE)
dados <- subset(dados, idade > 0 & idade < 120)

#############################################################

#LETRA A: GRÁFICO DE BARRAS DA SITUAÇÃO ATUAL DOS PACIENTES

grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)

pacientes <- table(dados$situacao_atual)

barplot_situacao = barplot(sort((table(dados$situacao_atual)), decreasing = TRUE), 
                           cex.names = 0.59, srt = 45, xpd = TRUE, add = TRUE,
                           legend = TRUE, args.legend = list(x = 10, y = 150000, cex = 0.8, text.font = 2, bg = "ghostwhite"),
                           col = c("blanchedalmond", "bisque", "bisque1", "burlywood1", "burlywood2", "bisque2", "burlywood", "bisque3"),
                           main = "GRÁFICO DE BARRAS DA SITUAÇÃO ATUAL DOS PACIENTES")

#LETRA B: 

obitos <- subset(dados, situacao_atual == "Óbito")
municipio.tb <- table(milsa$municipio_residencia)

obitostb <- table(obitos$municipio_residencia)

graf_obito <- barplot(obitostb)

#LETRA C
range(obitos$idade)
obitos_idade <- cut(obitos$idade, seq(0, 100, by = 10))

obitos_sexo_idade <- table(obitos$sexo, obitos_idade)

graf_c <- barplot(obitos_sexo_idade, beside = TRUE)

#LETRA D

obitos_idadee <- obitos$idade

str(obitos_idadee)

obitos_idadee

range(obitos_idadee)

hist_d <- hist(obitos_idadee)

#LETRA E

date <- as.Date(obitos$data_resultado_exame)
date <- table(format(date, '%y %m'))
prt <- barplot(date)