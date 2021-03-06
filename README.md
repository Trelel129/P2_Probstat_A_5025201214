# P2_Probstat_A_5025201214
Praktikum Probabilitas dan Statistik A Modul 2 oleh Ferry Nur Alfian Eka Putra (5025201214)  

## SOAL 1  
Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap kadar saturasi oksigen pada manusia.    
Peneliti tersebut mengambil sampel sebanyak 9 responden.    
Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat kadar saturasi oksigen dari 9 responden tersebut.    
Kemudian, 9 responden tersebut diminta melakukan aktivitas 𝐴.    
Setelah 15 menit, peneliti tersebut mencatat kembali kadar saturasi oksigen dari 9 responden tersebut.    
Berikut data dari 9 responden mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴   

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
Memasukkan nilai-nilai sebelum melakukan aktivitas A pada variabel x dan memasukkan nilai-nilai setelah melakukan aktivitas A pada variabel y menggunakan `c()`.   Setelah memasukkan seluruh nilai tersebut, melakukan pencarian nilai dari standar deviasi dari selisih pasangan data dengan menggunakan `sd(x-y)`.     
     
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
Karena T value berada di antara t_tabel, maka Hipotesis Awal (H0), yaitu "Rata-rata saham di bandung sama dengan rata - rata saham di Bali" diterima  

## SOAL 4
**Soal**
Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan
kucing putih dengan panjangnya masing-masing.
Jika :
diketahui dataset https://intip.in/datasetprobstat1
H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya sama

a) Plot 3 subjek grup
<br>![4_plotgrup1](https://user-images.githubusercontent.com/99122278/170871024-176f8dda-e00b-4a7d-b884-2c656d3f5eeb.png) </br> Grup 1
<br>![4_plotgrup2](https://user-images.githubusercontent.com/99122278/170871022-23ee72c8-faac-4d1e-b6ec-de0f4aef87a0.png) </br> Grup 2
<br>![4_plotgrup3](https://user-images.githubusercontent.com/99122278/170871018-d0eefa5f-430d-4634-b80e-4afef20c4b87.png) </br> Grup 3

poin b hingga e:
<br>![4_bcde](https://user-images.githubusercontent.com/99122278/170871139-7f6ded22-e17f-4dda-84ae-9de888e7af26.png)</br>
sehingga dapat disimpulkan hasil dari test post-hoc model 1 adalah grup 1 lebih panjang dari grup yang lain

## Nomor 5
**Soal**
a) Plot sederhana
<br> ![5_plot](https://user-images.githubusercontent.com/99122278/170871178-8af95a2e-b18a-462c-9a7d-16b3dce1276a.png) </br>
b) Uji Anova dua arah dan c) Tabel dengan mean dan standar deviasi cahaya
<br> ![5_bc](https://user-images.githubusercontent.com/99122278/170871265-16049286-6c09-44aa-9e7e-78ce36e651ea.png) </br>
d) Uji Tukey
<br> ![5d](https://user-images.githubusercontent.com/99122278/170871293-bbc129dd-1aa8-45af-adff-700a6d94bfd9.png) </br>
e) Compact Letter Display
<br> ![5_e](https://user-images.githubusercontent.com/99122278/170871321-a01e3ea7-70a8-4d24-a58a-14c39610a8ef.png) </br>




