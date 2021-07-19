dados <- read.csv("dadosPacientes2021.csv")

dados[dados == "Mascuino"] <- "Masculino"

data <- dados$data_resultado_exame

check_20 <- grepl(("2020"), data, fixed = TRUE)
check_21 <- grepl(("2021"), data, fixed = TRUE)

dados <- subset(dados, check_20 == TRUE | check_21 == TRUE)
dados <- subset(dados, idade > 0 & idade < 120)

pacientes.tb <- table(milsa$situacao_atual)

str(dados)

range(dados$idade)

str(pacientes.tb)

#LETRA A

graf_barra = barplot(pacientes.tb, col = c("pink", "cyan"),
                     main = )

#LETRA B

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
