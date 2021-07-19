lista <- c(48, 58, 56, 63, 52, 50, 59, 51, 59, 38, 57, 56, 73, 61, 41, 55, 49, 61, 49, 49, 52, 55, 60, 52, 54,
           57, 47, 66, 60, 53, 59, 50, 45, 57, 64, 56, 57, 60, 47, 58, 53, 58, 66, 47, 40)

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

#LETRA A

lim_range <- c(38, 43, 48, 53, 58, 63, 68, 73)

range <- table(cut(lista, breaks = lim_range))

lista_df <- as.data.frame(range)

h_list = hist(lista, col = c("pink", "cyan"),  
              main = "Histograma da Lista",
              xlab = "Lista", ylab = "Frequência",
              xlim = c(35, 75), ylim = c(0, 15),
              xaxp = c(38, 73, 7),
              labels = as.character(lista_df$Freq), breaks = lim_range)

lines(c(min(h_list$breaks), h_list$mids, max(h_list$breaks)), 
      c(0,h_list$counts, 0), type = "l")

help("lines")

#LETRA B

media <- mean(lista)

mediana <- median(lista)

moda_czuber <- moda.czuber(lista)

terceiro_quartil <- quantile(lista, 0.75)

percentil_8 <- quantile(lista, 0.08)

percentil_50 <- quantile(lista, 0.5)

percentil_80 <- quantile(lista, 0.8)

#LETRA C

boxplot(lista)



