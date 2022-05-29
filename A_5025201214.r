# ============================ Soal 1 ===================================
  # 1A
    x <- c(78,75,67,77,70,72,78,74,77)
    y <- c(100,95,70,90,90,90,89,90,100)
    stdev_soal1 <- sd(x-y)
    print(stdev_soal1)
     
  # 1B
    var(x)
    var(y)
    t.test(x,y,paired = TRUE,var.equal = FALSE)
     
  # 1C
    # Pada soal 1B, didapatkan p-value sebesar `6.003e-05` 
    # yang mengakibatkan H0 ditolak. Hal tersebut dapat terjadi 
    # karena nilai dari p-value < H0. Oleh karena itu, H1 diterima 
    # menjadi hipotesis,yaitu "Terdapat pengaruh yang signifikan 
    # secara statistika dalam hal kadar saturasi oksigen, sebelum dan 
    # sesudah melakukan aktivitas A"
    
# ============================ Soal 2 ===================================
  # 2A
    # Setuju
    
  # 2B
    zsum.test(mean.x = 23500,sigma.x = 3900,n.x = 100,
              alternative = "less",mu = 20000)
    
  # 2C
    # Hipotesis awal (H0) diterima
    
# ============================ Soal 3 ===================================
  # 3A
    # Hipotesis awal (H0) adalah Rata-rata saham di bandung sama dengan di Bali
    # Hipotesis alternatif (H1) adalah Rata-rata saham di bandung tidak sama dengan di Bali
  
  # 3B
    tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, mean.y =2.79 , s.y = 1.32, 
              n.y = 27, alternative = "two.side", var.equal = TRUE)
    
  # 3C
    n1 <- 19
    n2 <- 27
    mean1 <- 3.64
    mean2 <- 2.79
    sd1 <- 1.67
    sd2 <- 1.32
    alpha <- 0.05
    df <- 2
    
    t_tabel <- qt(p=alpha/2,df=df,lower.tail = FALSE)
    
  # 3D
    sp <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(df))
    T_value <- (mean1-mean2)/(sp*sqrt((1/n1)+(1/n2)))
    
  # 3E
    # Nilai dari T_value adalah 0.41 masih berada di antara t_tabel (-4.303 < T_value < 4.303)
    
  # 3F
    # Hipotesis Awal (H0), yaitu "Rata-rata saham di bandung sama dengan di Bali" diterima
