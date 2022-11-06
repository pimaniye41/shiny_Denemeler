#install.packages("tidyverse", "stringr", "RCurl", "XML", "plyr")
library(tidyverse)
library(stringr)
library(RCurl)
library(XML)
library(plyr)

#buraya kendi evds api anahtarınızı girin


anahtar <- "************"

#fonksiyon ile evds xml verisi otomasyonu


# install.packages("Rcurl", "XML","plyr")

library(RCurl) # EVDS'ye bağlantı için

library(XML) # XML veri kullanımı için

library(plyr) # XML veriyi data.frame'e çevirmek için


tcmb_evds<-function(veriseti,baslangic,son, anahtar) {
    
    adres="https://evds2.tcmb.gov.tr/service/evds/"
    
    seri=paste("series=",veriseti, sep="")
    
    tarihler=paste("&startDate=",baslangic,"&endDate=",son, sep="")
    
    tamamlayici=paste("&type=xml&key=",anahtar, sep="")
    
    veriadresi<-paste(adres,seri,tarihler,tamamlayici, sep="")
    
    xmlveri <- getURL(veriadresi, .opts = list(ssl.verifypeer = FALSE))
    
    return(xmlveri)
    
}

#buraya çekmek istediğiniz verinin evds2 kodunu, veri başlangıç tarihini, veri bitiş tarihini girin#
istenilen_veri <- "TP.MK.CUM.YTL"
baslangic_tarihi <- "01-08-1999"
bitis_tarihi <- "23-10-2022"
xmlveriler<-tcmb_evds(istenilen_veri, baslangic_tarihi, bitis_tarihi ,anahtar)

#xmlveriler = verilerin xml formatı

#xml' i dataframe'e çevirme

df <- ldply(xmlToDataFrame(xmlveriler))
df <- as.data.frame(df)


#filtreleme kısmında işe yaraması için değişken atama. Evds kodunda veriler arasında nokta varken, çekilen veride alttan tire var.


istenilen_veri_satir <- str_replace_all(istenilen_veri, fixed("."), "_" )
# istenilen_veri_satir


###########################!!!!! ÖNEMLİ!!!!!################### ##dataframe'i düzenleme## ##View(df) komutundan sonra istenilen satırların (Tarih ve Değer) satır numarasını aşağıdaki \### df \<- df[c(a,b)] komutuna yazalım.~~ **Artık buna gerek kalmadı çünkü bunu da otomatik olacak şekilde ayarladım**
    

rownames(df) <- c(df$.id)


#Bu chunk'ın tamamını iki defa çalıştırmamaya dikkat edin. Satır silme komutu veri silebilir dikkat. Öyle bir durumda en baştaki "df \<- " ksımından tekrar başlayın.

df <- df[c("Tarih", istenilen_veri_satir),]

df <- t(df)
df <- as.data.frame(df)
df <- df[-1,]
colnames(df)[2] <- istenilen_veri_satir

colnames(df) <- ifelse(colnames(df) %in% istenilen_veri_satir,"Değer",colnames(df) <- colnames(df))
df <- as.data.frame(df)
df$Değer <- as.numeric(df$Değer)
df <- df|>
    filter(!is.na(df$Değer))

####!!!Bu kısım tarih düzenlemesi ve deneysel. Veriden veriye farklılık olabilir , o yüzden dikkatli kullanın, asıl verinin gitmemesi için yeni kolon açtım!!!###.#########~~ **Bu kısımdaki karışıklığı da halletim fakat yine de her verideki tarih formatını bilmediğimden emin olmak için başka kolon ile devam edin**
    

df$Tarih1 <- ifelse((nchar(df$Tarih)<10),paste(df$Tarih,"-01",sep = ""),paste(df$Tarih,"",sep = ""))

#Asıl veriyi kaybetmemek için yeni kolondan devam ediyorum. Farklı tarih formatlarını tek formata çevirip kolonun yapısını date'e çevirmeye çalışıyorum. eğer "ambigous" hatası alıyorsanız dataframe'i yaparken satır numaralarını yanlış girmiş olabilirsiniz. Veya yapamadağım bir tarihi formatı da olabilir.

df$Tarih1 <- as.Date(df$Tarih1, tryFormats = c("%d-%m-%Y","%Y-%m-%d"))


#Verilerin grafiklenmesi

plot(df$Tarih1, df$Değer,type = "h", col = c("red","blue","green","yellow"))

ggplot(df)+
    geom_line(aes(x =Tarih1, y = Değer))+
    labs(title = "Altın Fiyatları",
         subtitle = "1999.08 - 2022.10",
         caption = "Efe Gülkan")


####Fikrin çıkış kaynağı ve veri çekme fonksiyonunu bulduğum site: <http://www.barissanli.com/calismalar/dersler/r/rders11.php> #kaynakta eski R ile çalışıldığı için verileri düzenlemesi farklıydı. Ders notlarım, stackoverflow ve adını sayamadığım birçok siteden araştırdım. Not: evds verileri için Hazır paket olduğunu sonradan öğrendim.

