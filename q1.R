lista <- c(48, 58, 56, 63, 52, 50, 59, 51, 59, 38, 57, 56, 73, 61, 41, 55, 49, 61, 49, 49, 52, 55, 60, 52, 54,
           57, 47, 66, 60, 53, 59, 50, 45, 57, 64, 56, 57, 60, 47, 58, 53, 58, 66, 47, 40)

#LETRA A

lim_range <- c(38, 43, 48, 53, 58, 63, 68, 73)

range(lista)

length(lista)

range <- c("38 - 43", "43 - 48", "48 - 53", "53 - 58", 
           "58 - 63", "63 - 68", "68 - 73")

listatb <- table(lista)

h_list = hist(lista, col = c("pink", "cyan"),  
              main = "Histograma da Lista",
              xlab = "Lista", ylab = "Frequência",
              xlim = c(35, 75), ylim = c(0, 15),
              xaxp = c(38, 73, 5),
              labels = range, breaks = lim_range)

lines(c(min(h$breaks), h$mids, max(h$breaks)), 
      c(0,h$counts, 0), type = "l")

#LETRA B

media <- mean(lista)

moda <- names(listatb)[which.max(listatb)]

mediana <- median(lista)

freq = table(cut(lista, breaks = lim_range, right = FALSE, labels = range))

hist(freq)

#ESTOU TENTANDO FAZER A MODA DE CZUBER E AGRUPAR OS VALORES FORA DO HISTOGRAMA
#PRA PODER FUNCIONAR A MODA E ACHAR O QUARTIL TBM
