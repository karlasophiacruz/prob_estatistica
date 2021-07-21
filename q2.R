dados <- read.csv("dadosPacientes2021.csv")

dados[dados == "Mascuino"] <- "Masculino"

check_20 <- grepl(("2020"), dados$data_resultado_exame, fixed = TRUE)
check_21 <- grepl(("2021"), dados$data_resultado_exame, fixed = TRUE)

dados <- subset(dados, check_20 == TRUE | check_21 == TRUE)
dados <- subset(dados, idade > 0 & idade < 120)

grid <- grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)

#############################################################

#LETRA A: GRÁFICO DE BARRAS DA SITUAÇÃO ATUAL DOS PACIENTES

gb_situacao = barplot(sort((table(dados$situacao_atual)), decreasing = TRUE), 
                           cex.names = 0.59, srt = 45, ylim = c(0, 200000, 10000),
                           legend = TRUE, args.legend = list(x = 10, y = 150000, 
                           cex = 0.8, text.font = 2, bg = "ghostwhite"),
                           col = c("blanchedalmond", "bisque", "bisque1", "burlywood1", 
                                   "burlywood2", "bisque2", "burlywood", "bisque3"),
                           main = "GRÁFICO DE BARRAS DA SITUAÇÃO ATUAL DOS PACIENTES")

#############################################################

#LETRA B: GRÁFICO DE BARRAS DOS ÓBITOS POR MUNICÍPIO

obitos <- subset(dados, situacao_atual == "Óbito")

obitos_muni <- table(obitos$municipio_residencia)

gb_obitos_muni <- barplot(sort(obitos_muni, decreasing = TRUE), col = "bisque2",
                         main = "GRÁFICO DE BARRAS DOS ÓBITOS POR MUNICÍPIO",
                         ylab = "Número de óbitos", add = TRUE,
                         cex.axis = 1, cex.names = 0.5, las = 2, cex.lab = 0.8, srt = 45)

#############################################################

#LETRA C: GRÁFICO DOS ÓBITOS POR SEXO E IDADE

lim_idade <- c("0 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", 
               "60 - 70", "70 - 80", "80 - 90", "90 - 100")

obitos_idade <- cut(obitos$idade, seq(0, 100, by = 10))

obitos_sexo_idade <- table(obitos$sexo, obitos_idade)

gb_obitos_sexidade <- barplot(obitos_sexo_idade, beside = TRUE,  
                              col = c("mistyrose2", "lightskyblue1"),
                              main = "GRÁFICO DOS ÓBITOS POR SEXO E IDADE", 
                              ylim = c(0, 700), add = TRUE, cex.axis = 0.8,
                              names.arg = lim_idade, legend = TRUE,
                              args.legend = list(x = 10, y = 550, cex = 0.8, text.font = 2, 
                                                 bg = "ghostwhite"),
                              ylab = "Número de óbitos", xlab = "Sexo e Idade (em anos)")

#############################################################

#LETRA D: HISTOGRAMA DE ÓBITOS POR IDADE

h_obitos_idade <- hist(obitos$idade, col = "khaki", 
                       main = "HISTOGRAMA DE ÓBITOS POR IDADE",
                       xlab = "Idade (em anos)", ylab = "Óbitos", add= TRUE, 
                       xlim = c(0, 110), ylim = c(0, 1300),  xaxp = c(0, 110, 11))

xfit <- seq(min(obitos$idade), max(obitos$idade), length = 100)
yfit <- dnorm(xfit, mean(obitos$idade), sd(obitos$idade))
yfit <- yfit*diff(h_obitos_idade$mids[1:2])*length(obitos$idade)
lines(xfit, yfit, col = "midnightblue", lwd = 2)

#############################################################

#LETRA E: GRÁFICO DE BARRAS DOS ÓBITOS POR MÊS

lim_datas <- c("01 / 20", "02 / 20", "03 / 20", "04 / 20", "05 / 20", "06 / 20", 
             "07 / 20", "08 / 20", "09 / 20", "10 / 20", "11 / 20", "12 / 20",
             "01 / 21", "02 / 21", "03 / 21", "04 / 21", "05 / 21", "06 / 21")

obito_data <- as.Date(obitos$data_resultado_exame)
obito_data <- table(format(obito_data, '%y %m'))

gb_obitos_mes <- barplot(date, col = "khaki", add = TRUE,
                        main = "GRÁFICO DE BARRAS DOS ÓBITOS POR MÊS",
                        xlab = "Período (mês / ano)", ylab = "Óbitos",
                        ylim = c(0, 1000), names.arg = lim_datas)

#############################################################