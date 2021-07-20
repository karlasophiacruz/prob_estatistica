lista <- c(48, 58, 56, 63, 52, 50, 59, 51, 59, 38, 57, 56, 73, 61, 41, 55, 49, 61, 49, 49, 52, 55, 60, 52, 54,
           57, 47, 66, 60, 53, 59, 50, 45, 57, 64, 56, 57, 60, 47, 58, 53, 58, 66, 47, 40)

grid <- grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)

#############################################################

#LETRA A: HISTOGRAMA DA FREQUÊNCIA DA LISTA DE NÚMEROS AGRUPADOS

lim_intervalo <- c(38, 43, 48, 53, 58, 63, 68, 73)

lista_df <- as.data.frame(table(cut(lista, breaks = lim_intervalo)))

h_lista = hist(lista, col = "khaki",
              main = "HISTOGRAMA DA LISTA DE NÚMEROS AGRUPADOS",
              xlab = "Lista de Números Agrupados", ylab = "Frequência Absoluta",
              xlim = c(35, 75), ylim = c(0, 15),
              xaxp = c(38, 73, 7), add = TRUE,
              labels = as.character(lista_df$Freq), breaks = lim_intervalo)

xfit <- seq(min(lista), max(lista), length = 200)
yfit <- dnorm(xfit, mean(lista), sd(lista))
yfit <- yfit*diff(h_lista$mids[1:2])*length(lista)
lines(xfit, yfit, col = "midnightblue", lwd = 2)

#############################################################

#LETRA B: MÉDIA, MODA DE CZUBER, MEDIANA, TERCEIRO QUARTIL, PERCENTIS 8, 50 E 80

#################### FUNÇÃO MODA CZUBER #####################

moda.czuber <- function(dados){
  amp <- max(dados) - min(dados)
  nk <- round(1 + 3.222 * log10(length(dados)))
  amp_classe <- amp / nk
  
  lim_classe <- seq(min(dados), max(dados), amp_classe)
  tab_classe <- table(cut(dados, breaks = lim_classe))
  
  lim_inferior <- lim_classe[which.max(tab_classe)]
  
  d1 <- max(tab_classe) - as.numeric(tab_classe[which.max(tab_classe) - 1])
  d2 <- max(tab_classe) - as.numeric(tab_classe[which.max(tab_classe) + 1])
  
  return(lim_inferior + (d1 / (d1 + d2)) * amp_classe)
}

#############################################################

#MÉDIA
media <- mean(lista)

#MEDIANA
mediana <- median(lista)

#MODA DE CZUBER
moda_czuber <- moda.czuber(lista)

#TERCEIRO QUARTIL
terceiro_quartil <- quantile(lista, 0.75)

#PERCENTIL 8
percentil_8 <- quantile(lista, 0.08)

#PERCENTIL 50
percentil_50 <- quantile(lista, 0.5)

#PERCENTIL 80
percentil_80 <- quantile(lista, 0.8)

#############################################################

#LETRA C: BOXPLOT

#BOXPLOT VERTICAL

boxplot_lista_v <- boxplot(lista, add = TRUE, 
                         main = "Boxplot da Lista de Números",
                         ylab = "Lista de Números",
                         col = "khaki")

#BOXPLOT HORIZONTAL

boxplot_lista_h <- boxplot(lista, horizontal = TRUE, notch = TRUE,
                         main = "Boxplot da Lista de Números",
                         ylab = "Lista de Números",
                         col = "khaki")

#############################################################
