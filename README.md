# P2_Probstat_D_5025201078
Praktikum Probabilitas dan Statistik D Modul 1 oleh Rere Arga Dewanata (5025201078)  

## SOAL 1  
Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴  

Responden | X | Y
-------   | - | -
1         | 78 | 100
2         | 75 | 95
3         | 67 | 70  
4         | 77 | 90
5         | 70 | 90
6         | 72 | 90
7         | 78 | 89
8         | 74 | 90
9         | 77 | 100  

### Soal 1A  
**Deskripsi Soal**  
Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas!  

**Kode Program**    
```R
x <- c(78,75,67,77,70,72,78,74,77)
y <- c(100,95,70,90,90,90,89,90,100)
stdev_soal1 <- sd(x-y)
```  

**Penjelasan**  
Memasukkan nilai-nilai sebelum melakukan aktivitas A pada variabel x dan memasukkan nilai-nilai setelah melakukan aktivitas A pada variabel y menggunakan `c()`. Setelah memasukkan seluruh nilai tersebut, melakukan pencarian nilai dari standar deviasi dari selisih pasangan data dengan menggunakan `sd(x-y)`.  

**Screenshot**  
![1A](https://user-images.githubusercontent.com/70679432/170860569-773b4668-fabd-4cee-b0e9-b22637e2d9ec.jpeg)  

### Soal 1B  
**Deksripsi Soal**  
Carilah nilai t (p-value)!  

**Kode Program**  
```R
var(x)
var(y)
t.test(x,y,paired = TRUE,var.equal = FALSE)
```  

**Penjelasan**  
Mengecek nilai dari varians variabel x dan variabel y dengan menggunakan `var()`. Setelah diketahui bahwa varians kedua variabel berbeda, dilakukan pencarian dengan menggunakan `t.test()`. Pada `t.test()`, nilai dari `paired` bernilai TRUE karena data x dan y adalah sepasang. Selain itu, nilai dari `var.equal` adalah FALSE karena variabel x dan variabel y memiliki varians yang berbeda.  

**Screenshot**  
![1B](https://user-images.githubusercontent.com/70679432/170860677-b169b528-99a7-4f06-b4f7-2316f51f3521.jpeg)  

## Soal 1C
**Deskripsi Soal**  
Tentukanlah apakah terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”  

**Penjelasan**  
Pada soal 1B, didapatkan p-value sebesar `6.003e-05` yang mengakibatkan H0 ditolak. Hal tersebut dapat terjadi karena nilai dari p-value < H0. Oleh karena itu, H1 diterima menjadi hipotesis,yaitu:   
"Terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen, sebelum dan sesudah melakukan aktivitas A"  

## SOAL 2  
Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun. Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata 23.500 kilometer dan standar deviasi 3900 kilometer.  

**Kode Program**  
```R
zsum.test(mean.x = 23500,sigma.x = 3900,n.x = 100,
          alternative = "less",mu = 20000)
```  

**Penjelasan Kode Program**    
Untuk menyelesaikan permasalahan ini, dapat digunakan `zsum.test` karena Uji Z dapat digunakan apabila data berdisitribusi normal dan jumlah sampel (n) lebih dari 30. Penelitian ini menggunakan uji satu arah dengan hipotesis alternatif adalah "mobil rata-rata dikemudikan kurang dari 20.000 kilometer per tahun". Sehingga, digunakan `alternative = less`.  

### Soal 2A  
**Deksripsi Soal**  
Apakah anda setuju dengan klaim tersebut?  

**Penjelasan**  
Setuju  

### Soal 2B  
**Deskripsi Soal**  
Jelaskan maksud dari output yang dihasilkan!  

**Screenshot**  
![2](https://user-images.githubusercontent.com/70679432/170865034-20b3e7aa-d5e1-4630-90f3-9ae760d1e647.jpeg)

**Penjelasan**  
Berdasarkan output yang dihasilkan, diperoleh nilai Zhitung adalah `8.9744` dan p-value adalah `1`. Selain itu, juga diperolah bahwa selang atas kepercayaan rata rata berada pada nilai `24141.49`.

### Soal 2C  
**Deskripsi Soal**  
Buatkan kesimpulan berdasarkan p-value yang dihasilkan!  

**Penjelasan**  
Nilai dari P adalah `1`  
P(Z > 8.9744) = 1 - P(Z < 8.977)  
              = 1 - 1  
              = 0  
Oleh karena itu, Hipotesis awal (H0) bahwa "Mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun" diterima.  

## SOAL 3  
Diketahui perusahaan memiliki seorang data analyst ingin memecahkan permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya
didapatkanlah data berikut dari perusahaan saham tersebut.  
Nama Kota/Atribut      | Bandung | Bali
--------------------   | ------- | --
Jumlah Saham           | 19      | 27
Sampel Mean            | 3.64    | 2.79
Sampel Standar Deviasi | 1.67    | 1.32  
  
Dari data diatas, berilah keputusan serta kesimpulan yang didapatkan dari hasil diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada rata-ratanya (α= 0.05)?  

### Soal 3A
**Deksripsi Soal**  
Buatlah H0 dan H1!  

**Penjelasan**  
H0 = Rata - rata saham di Bandung sama dengan rata - rata saham di Bali  
H1 = Rata - rata saham di Bandung tidak sama dengan rata - rata saham di Bali  

### Soal 3B  
**Deskripsi Soal**  
Hitunglah sampel statistik!  

**Kode Program**  
```R
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, mean.y =2.79 , s.y = 1.32, 
          n.y = 27, alternative = "two.side", var.equal = TRUE)
```  

**Screenshot**    
![3B](https://user-images.githubusercontent.com/70679432/170871150-98101c4a-2ded-4071-b2d5-33d82330a5af.jpeg)  

**Penjelasan**  
Digunakan t test dengan fungsi `tsum.test()` karena jumlah data < 30. Pada pengujian ini, dilakukan secara dua arah(two-sided) dengan menggunakan parameter `alternative = "two.side"` karena yang ingin diujikan adalah rata - rata saham kedua kota tidak sama  

### Soal 3C  
**Deskripsi Soal**  
Lakukan uji statistik! (df = 2)  

**Kode Program**  
```R
n1 <- 19
n2 <- 27
mean1 <- 3.64
mean2 <- 2.79
sd1 <- 1.67
sd2 <- 1.32
alpha <- 0.05
df <- 2

t_tabel <- qt(p=alpha/2,df=df,lower.tail = FALSE)
```  

**Penjelasan**  
Digunakan pencarian nilai dari t_tabel menggunakan fungsi `qt`. Nilai dari p dilakukan pembagian dua karena akan dilakukan pengujian secara dua arah. Nilai df yang dimasukkan sesuai dengan ketentuan soal, yaitu 2.  

**Screenshot**  
![3C](https://user-images.githubusercontent.com/70679432/170871493-faa4ea26-dece-4979-afe3-1b4c5540aba7.jpeg)  

### Soal 3D  
**Deskripsi Soal**  
Hitunglah nilai kritikal!  

**Kode Program**  
```R
 sp <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(df))
 T_value <- (mean1-mean2)/(sp*sqrt((1/n1)+(1/n2)))
```  

**Screenshot**  
![3D](https://user-images.githubusercontent.com/70679432/170871681-46c20890-4777-4b18-a38a-adc3677a8d87.jpeg)  

**Penjelasan**  
Berikut adalah implementasi dari kedua rumus ini.  
![3_Formula1](https://user-images.githubusercontent.com/70679432/170872073-8c02eec9-bd3d-4a76-a463-a6f979c45110.jpeg)  
![3_Formula2](https://user-images.githubusercontent.com/70679432/170872086-93f395bf-af69-42e1-9f2e-1165884c9a9e.jpeg)  

### Soal 3E  
**Deskripsi Soal**  
Keputusan  

**Penjelasan**  
Nilai dari T_value adalah 0.41 yang berarti masih berada di antara t_tabel (-4.303 < T_value < 4.303).  

### Soal 3F  
**Deskripsi Soal**  
Kesimpulan  

**Penjelasan**  
Hipotesis Awal (H0), yaitu "Rata-rata saham di bandung sama dengan rata - rata saham di Bali" diterima  





