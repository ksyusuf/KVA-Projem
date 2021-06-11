# KÜTÜPHANELER
{
library(readr)
library(corrplot)
library(zoo)
library(date)
library(psych)
library(DescTools)
library(ehaGoF)
library(raster)   # varyasyon katsayýsý
library(ggplot2)   # daha güzel pie grafikleri için
library(ggthemes)
library(GGally)
library(car)
library(MASS)
library(tidyverse)
library(hexbin)
library(fpp2)
library(rstatix)
library(vioplot)
library(dplyr)
library(dgof)  # Kolmogorov - Smirnov testi
library(gapminder)
library(ggExtra)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(sunburstR)
library(treemap)
library(tidyverse)
library(treemap)
library(reticulate) #the superpower bridges python and R
library(ISLR) # grafikleri üst üste yazdýrmak için
library(ggpubr)
library(graphics)
}

eksiltilmis_veriler <- read_delim("eksiltilmis_veriler.csv", ";",
                  escape_double = FALSE,
                  col_types = cols(trh = col_date(format = "%d.%m.%Y")), 
                  trim_ws = TRUE)
eksiltilmis_veriler

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#     TEK BOYUTLU ANALÝZLER
{
### merkezi eðilim ölçüleri
{
mean(eksiltilmis_veriler$rzgr)
mean(eksiltilmis_veriler$buhr)  #'ortalama'

geometric.mean(eksiltilmis_veriler$rzgr)   # geometrik ortalama
geometric.mean(eksiltilmis_veriler$buhr, na.rm = TRUE) # demek ki buharýn sýfýr olduðu zaman var

harmonic.mean(eksiltilmis_veriler$rzgr)    # harmonik ortalama
harmonic.mean(eksiltilmis_veriler$buhr)   # verilerimde sýfýr var

median(eksiltilmis_veriler$rzgr)
median(eksiltilmis_veriler$buhr)    # medyan

Mode(eksiltilmis_veriler$rzgr)   # mod
Mode(eksiltilmis_veriler$buhr)  
}

### merkezi yayýlým ölçüleri
{
    min(eksiltilmis_veriler$rzgr)   #min deðer
    min(eksiltilmis_veriler$buhr)
    
    max(eksiltilmis_veriler$rzgr)   # max deðr
    max(eksiltilmis_veriler$buhr)

    sd(eksiltilmis_veriler$rzgr)  #  standart sapmalar 
    sd(eksiltilmis_veriler$buhr)
    
    var(eksiltilmis_veriler$rzgr)
    var(eksiltilmis_veriler$buhr)   #varyans
    
    cv(eksiltilmis_veriler$rzgr)   # varyasyon katsayýsý
    cv(eksiltilmis_veriler$buhr)
    
    quantile(eksiltilmis_veriler$rzgr, c(.32, .57, .98))    # çeyreklikler
    quantile(eksiltilmis_veriler$rzgr, c(.25, .50, .75))
    quantile(eksiltilmis_veriler$buhr, c(.25, .50, .75))
    quantile(eksiltilmis_veriler$rzgr)
    quantile(eksiltilmis_veriler$buhr)
    
    IQR(eksiltilmis_veriler$rzgr)    # çeyreklikler açýklýðý
    IQR(eksiltilmis_veriler$buhr) # üst çeyreklik - alt çeyreklik  (%75-%25)
    
    mad(eksiltilmis_veriler$rzgr)  # medyan mutlak sapma
    mad(eksiltilmis_veriler$buhr)
    
    MeanAD(eksiltilmis_veriler$rzgr)  # ortalama mutlak sapma
    MeanAD(eksiltilmis_veriler$buhr)

    skew(eksiltilmis_veriler$rzgr, na.rm = FALSE, type = 3)   # çarpýklýk
    skew(eksiltilmis_veriler$buhr)
    
    Kurt(eksiltilmis_veriler$rzgr)   # basýklýk
    Kurt(eksiltilmis_veriler$buhr)
}

### P deðeri için bir takým testler
{ 
  shapiro.test(eksiltilmis_veriler$rzgr)  # Shapiro-Wilk Test
  shapiro.test(eksiltilmis_veriler$buhr)
  
  # Kolmogorov Smirnov Test
  ks.test(eksiltilmis_veriler$rzgr, eksiltilmis_veriler$buhr)
  ks.test(eksiltilmis_veriler$buhr, eksiltilmis_veriler$rzgr)  # hatalý
  ks.test(eksiltilmis_veriler$rzgr, eksiltilmis_veriler$buhr)
  plot(ecdf(eksiltilmis_veriler$rzgr), 
       xlim = range(c(eksiltilmis_veriler$rzgr, eksiltilmis_veriler$buhr)),
       xlab = "Deðerler",
       col = "blue",
       main = "Kolmogorov Smirnov Testi")
  plot(ecdf(eksiltilmis_veriler$buhr), 
       add = TRUE, 
       lty = "dashed",
       col = "red")
  ks.test(eksiltilmis_veriler[,-1], "pnorm")
  
  LillieTest(eksiltilmis_veriler$rzgr)  # Lilliefors test
  LillieTest(eksiltilmis_veriler$buhr)
  
  JarqueBeraTest(eksiltilmis_veriler$rzgr)
  JarqueBeraTest(eksiltilmis_veriler$buhr)
  
  AndersonDarlingTest(eksiltilmis_veriler$rzgr)   # Anderson-Darling Testi
  AndersonDarlingTest(eksiltilmis_veriler$buhr)
  
}

### tek boyut için grafikler
{

# histogram çalýþmalarý
  {
  hist(eksiltilmis_veriler$rzgr)  # histogram
  hist(eksiltilmis_veriler$buhr,
       breaks= c(30),
       main = "Buharlaþma Histogramý",
       xlab = "Buharlaþma",
       ylab = "Frekans")
  
  hist(eksiltilmis_veriler$rzgr,
       border = "red",
       col = "#4e1c7f",
       main = "Rüzgâr Hýzý Histogramý",
       xlab = "Rüzgâr Hýzý",
       ylab = "Frekans",
       breaks = c(40),
       density = NULL,
       freq = FALSE)
  
  hist(eksiltilmis_veriler$buhr,
       border = "red",
       col = "#4e1c7f",
       main = "Buharlaþma Histogramý",
       xlab = "Buharlaþma",
       ylab = "Frekans",
       breaks = c(30),
       density = NULL,
       freq = FALSE)
  
  hist(eksiltilmis_veriler$rzgr, breaks=c(70), ylim = c(0,0.4), freq=FALSE) 

  h <- hist(eksiltilmis_veriler$rzgr, ylim = c(0,1500), col=topo.colors(5), freq = TRUE)
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

    plotForecastErrors <- function(forecasterrors)
  {
    # make a histogram of the forecast errors:
    mybinsize <- IQR(forecasterrors)/4
    mysd   <- sd(forecasterrors)
    mymin  <- min(forecasterrors) - mysd*5
    mymax  <- max(forecasterrors) + mysd*3
    # generate normally distributed data with mean 0 and standard deviation mysd
    mynorm <- rnorm(100000, mean=0, sd=mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col="purple", freq=FALSE, breaks=mybins)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
    # plot the normal curve as a blue line on top of the histogram of forecast errors:
    points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
  }
  plotForecastErrors(eksiltilmis_veriler$rzgr)
  # neden üst üste çizmedi anlamadým. ama güzel biþe
  }
  
# gövde yaprak metodu
stem(eksiltilmis_veriler$rzgr, width = 70)
stem(eksiltilmis_veriler$buhr, width = 70)
stem(eksiltilmis_veriler$buhr, scale = 0.3)

# boxplot çalýþmalarý
{
boxplot(eksiltilmis_veriler$rzgr,
        ylab="Rüzgâr Hýzý",
        main="Rüzgâr Hýzý Kutu Grafiði",
        col = "#4e1c7f",
        border = "red",
        varwidth = TRUE,
        notch = FALSE,
        horizontal = TRUE)

boxplot(eksiltilmis_veriler$buhr,
        ylab="Buharlaþma",
        main="Buharlaþma Ýçin Kutu Grafiði",
        col = "#4e1c7f",
        border = "red",
        varwidth = TRUE,
        notch = FALSE,
        horizontal = TRUE)

boxplot(eksiltilmis_veriler[,-1],
        main="Rüzgâr Hýzý ve Buharlaþma Ýçin Ortak Kutu Grafiði",
        col = "#4e1c7f",
        border = "red",
        varwidth = TRUE,
        notch = FALSE,
        names=c("Rüzgâr Hýzý","Buharlaþma"),
                horizontal = TRUE)   # boxplot

boxplot(eksiltilmis_veriler$buhr, horizontal = TRUE)   # yatay boxplot
}


#----------------------------------------------------------

# barplot, buna çok da gerek yok zaten histogramýmýz var
{
barplot(eksiltilmis_veriler$rzgr)

barplot(table(eksiltilmis_veriler$rzgr),
        main = "Rüzgar Hýzý Bar Grafiði (Düþey)",
        xlab = "Rüzgar Hýzý",
        ylab = "Rüzgar Hýzý Tekrarý",
        col = "darkred",
        horiz = FALSE)

barplot(table(eksiltilmis_veriler$buhr),
        main = "Buharlaþma Bar Grafiði (Düþey)",
        xlab = "Buharlaþma",
        ylab = "Buharlaþma Tekrarý",
        col = "darkred",
        horiz = FALSE)

}

# KDE bu iþte bu da tek 
# çekirdek yoðunluðu tahmin grafiði
{
r_kde = plot(density(eksiltilmis_veriler$rzgr),
     pch = 3,
     main="Rüzgâr Hýzýnýn Yoðunluk Grafiði",
     ylab="Yoðunluk",
     col="red")

b_kde = plot(density(eksiltilmis_veriler$buhr),
     pch = 3,
     main="Buharlaþmanýn Yoðunluk Grafiði",
     ylab="Yoðunluk",
     col="red")

# kde + kde
data(eksiltilmis_veriler[,-1])
class(eksiltilmis_veriler[,-1]); dim(eksiltilmis_veriler[,-1]);str(eksiltilmis_veriler[,-1])
age2 <- dnorm(eksiltilmis_veriler$rzgr, mean(eksiltilmis_veriler$rzgr), sd(eksiltilmis_veriler$rzgr))
age1 <- dnorm(eksiltilmis_veriler$buhr, mean(eksiltilmis_veriler$buhr), sd(eksiltilmis_veriler$buhr))

ggplot(data=eksiltilmis_veriler[,-1]) +
  geom_line(mapping=aes(x=buhr, y=age1), fill="darkred", color="darkred") +
  geom_line(mapping=aes(x=rzgr, y=age2), fill="red", color="red") +
  labs(title = "Ortak KDE Grafiði", x = "Deðerler", y = "Yoðunluk") +
  theme_classic()
}


# Q-Q grafiði
{
qqPlot(eksiltilmis_veriler$rzgr,
       main="Rüzgâr Hýzý Q-Q Grafiði",
       ylab = "Rüzgâr Hýzý")
qqPlot(eksiltilmis_veriler$buhr, main="Buharlaþma Q-Q Grafiði")
qqnorm(eksiltilmis_veriler$rzgr)
qqnorm(eksiltilmis_veriler$buhr)

# ggplot kütüphanesi ile çizelim daha güzel
ggplot(eksiltilmis_veriler[,-1],
       aes(sample = rzgr, colour = factor(rzgr))) +
  stat_qq() +
  stat_qq_line() # rengarenk güzel bir þey

ggplot(eksiltilmis_veriler[,-1],
       aes(sample = rzgr)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Ortak KDE Grafiði", x = "Deðerler", y = "Yoðunluk")

ggqqplot(eksiltilmis_veriler[,-1], x = "rzgr",
         color = "red", 
         palette = c("#4e1c7f", "#4e1c7f"),
         ggtheme = theme_pubclean()) +
  labs(title = "Rüzgâr Hýzý Q-Q Grafiði", y = "Deðerler")

ggqqplot(eksiltilmis_veriler[,-1], x = "buhr",
         color = "red", 
         palette = c("#4e1c7f", "#4e1c7f"),
         ggtheme = theme_pubclean()) +
  labs(title = "Buharlaþma Q-Q Grafiði", y = "Deðerler")
}


# zaman serisi çalýþmalarý
{
# zaman serisini böyle de elde edebilyoruz
# zaman serisi içni ayrý bir fonksiyon vardýr (var ama, tarihi yapamadýk)
zaman_serisi = plot(zoo(eksiltilmis_veriler$rzgr,as.Date(eksiltilmis_veriler$trh,"%d.%m.%Y")),
     xlab="Zaman", ylab="Rüzgâ Hýzý",col="red",
     main="Rüzgâr Hýzý Zaman Serisi")

zaman_serisi = plot(zoo(eksiltilmis_veriler$buhr,as.Date(eksiltilmis_veriler$trh,"%d.%m.%Y")),
                    xlab="Zaman", ylab="Buharlaþma",col="red",
                    main="Buharlaþma Zaman Serisi")

plot.ts(log(eksiltilmis_veriler$buhr),
        main="Buharlaþma Logaritmik Zaman Serisi",
        col="Purple")

{
  ## rüzgar hýzý aylýk yýllýk haftalýk zaman serileri
  ruzgar_ts2 <- xts(eksiltilmis_veriler[,-1], order.by = eksiltilmis_veriler$trh)
  
  haftalik_zs <- apply.weekly(ruzgar_ts2,FUN = mean)
  aylik_zs <- apply.monthly(ruzgar_ts2, FUN = mean)
  yilllik_zs <- apply.yearly(ruzgar_ts2, FUN = mean)
  gunluk_zs <- apply.daily(ruzgar_ts2, FUN = mean)
  
  plot.ts(yilllik_zs[,1],
          xlab="Zaman", ylab="Yýllýk",col="red")
  plot.ts(aylik_zs[,1],
          xlab="Zaman", ylab="Aylýk",col="red",)
  
  plot.ts(haftalik_zs[,1],
          xlab="Zaman", ylab="Haftalýk",col="red",)
  
  plot.ts(gunluk_zs[,1],
          xlab="Zaman", ylab="Günlük",col="red",)
  
  ## eksen isimlerini deðiþtiremedim. manuel olarak ayarlayacaðým.
  
  
  eksiltilmis_aylik = apply.yearly(xts(eksiltilmis_veriler, order.by = eksiltilmis_veriler$trh), FUN=mean)
  plot.ts(aylik_zs, plot.type = "multiple")
  plot.ts(eksiltilmis_veriler[,-1], plot.type = "multiple")
  aa_kontrol = aylik_zs[,-1]
  
  ## buharlaþma aylýk yýllýk haftalýk zaman serileri
  buharlasma_ts2 <- xts(eksiltilmis_veriler$buhr, order.by = eksiltilmis_veriler$trh)
  
  haftalik_zs_b <- apply.weekly(buharlasma_ts2,FUN = mean)
  aylik_zs_b <- apply.monthly(buharlasma_ts2, FUN = mean)
  yilllik_zs_b <- apply.yearly(buharlasma_ts2, FUN = mean)
  gunluk_zs_b <- apply.daily(buharlasma_ts2, FUN = mean)
  
  plot.ts(yilllik_zs_b[,1],
          xlab="Zaman", ylab="Yýllýk",col="red")
  
  plot.ts(aylik_zs_b[,1],
          xlab="Zaman", ylab="Aylýk",col="red")
  
  plot.ts(haftalik_zs_b[,1],
          xlab="Zaman", ylab="Haftalýk",col="red")
  
  plot.ts(gunluk_zs_b[,1],
          xlab="Zaman", ylab="Günlük",col="red")
  
  
  
  plot(zoo(c(2), as.Date(eksiltilmis_veriler$trh, "%d.%m.%Y")),
       xlab="Zaman", ylab=" ",col="red")
}


}


# korelasyon grafik çalýþmalarý
{
# bir anda sýfýra gidiyorsa duraðandýr /// rüzgar duraðan yani
# otokrelasyon
acf(eksiltilmis_veriler$rzgr,
    main="Rüzgâr Hýzýnýn Otokorelasyon Grafiði")
acf(eksiltilmis_veriler$buhr,
    main="Buharlaþmanýn Otokorelasyon Grafiði")

# kýsmi otokrolasyon
pacf(eksiltilmis_veriler$rzgr,
     main="Rüzgâr Hýzýnýn Kýsmi Otokorelasyon ")
pacf(eksiltilmis_veriler$buhr,
     main="Buharlaþmanýn Kýsmi Otokorelasyon Grafiði")

}


# pch noktlarýn þeklini belirliyor pasta grafiði gibi
{
pie(table(eksiltilmis_veriler$rzgr), )
pie(eksiltilmis_veriler$rzgr)

df <- data.frame(
  group = eksiltilmis_veriler$rzgr,
  value = eksiltilmis_veriler$buhr
)
df <- head(df)
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie
}



# keman grafiði (violin plot)
{
vioplot(eksiltilmis_veriler$rzgr,
        main="Rüzgâr Hýzý Keman Grafiði",
        ylab="Rüzgâr Hýzý Deðerleri",
        col="#4e1c7f",
        border="red")

vioplot(eksiltilmis_veriler$buhr,
        main="Buharlaþma Keman Grafiði",
        col="#4e1c7f",
        ylab = "Buharlaþma Deðerleri",
        border="red")

#kemanlarýn birlikte incelenmesi
vioplot(eksiltilmis_veriler$rzgr,
        eksiltilmis_veriler$buhr,
        names=c("Rüzgâr Hýzý", "Buharlaþma"),
        main = "Ortak Keman Grafiði", col="#4e1c7f",
        ylab = "Deðerler",
        border = "red")

}
}
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#     ÝKÝ BOYUTLU ANALÝZLER
{
#'korelasyon uygulamak için bu fonk'
{
cor(eksiltilmis_veriler[,-1], method = "kendall")
cor.plot(cor(eksiltilmis_veriler[,-1]), method="kendall")

# projeme koyduðum gösterim
korelasyon <- cor(eksiltilmis_veriler[,-1], method = "kendall")
colnames(korelasyon) <- c("Rüzgâr Hýzý", "Buharlaþma")
rownames(korelasyon) <- c("Rüzgâr Hýzý", "Buharlaþma")
corrplot(korelasyon,
         method="pie",
         tl.srt=0,
         tl.col="black",
         type = "upper")

corrplot(cor(eksiltilmis_veriler[,-1]), method="ellipse", type="upper")
corrplot(cor(eksiltilmis_veriler[,-1]), method="number", type="lower")
corrplot.mixed(cor(eksiltilmis_veriler[,-1]))


ggcorr(eksiltilmis_veriler[,-1], method = "pairwise")
}

# kovaryans
{
plot(cov(eksiltilmis_veriler[,-1], use = "everything"),
     main="Kovaryans",
     xlab="Rüzgâr Hýzý",
     ylab="Buharlaþma") # sadece iki nokta çiziyor
cov(eksiltilmis_veriler[,-1])  # sayýsal veri
}

# regresyon -projede var ama geliþtirilecek-
{
  lmn <- lm(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr)
  abline(plot(lmn), main="Rüzgâr Hýzý ve Buharlaþma") #Add a regression line
  abline(lm(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr))
  lmTemp = lm(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr) #Create the linear regression
  # burada verdiði çizimler anova sonuçlarý imiþ
  
  abline(lmTemp, cex = 1.3, pch = 16, xlab = "Weight in Kg", ylab = "Height in cm")
  
  relation <- lm(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr)
  
  abline(plot(relation))
  
  plot(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr, data = eksiltilmis_veriler)
  qqnorm(eksiltilmis_veriler$buhr, main = "Normallik Grafiði")
  qqline(eksiltilmis_veriler$buhr)
  
  sonuc <- lm(eksiltilmis_veriler$rzgr ~ eksiltilmis_veriler$buhr)
  
  summary(sonuc)
  
  par(mfrow=c(2,2))
  plot(sonuc)
}


# daðýlým grafiði (skatter)
pairs(eksiltilmis_veriler[,-1],
      col = "#4e1c7f",
      main = "Daðýlým Grafikleri",
      labels = c("Rüzgâr Hýzý", "Buharlaþma"))   # bu R ýn kendi correlogramý

# bubble plot yapmaya çalýþtým
# büyüklükleri ayarlayama
{
data <- eksiltilmis_veriler[,-1] 
ggplot(data, aes(x=eksiltilmis_veriler$buhr,
                 y=eksiltilmis_veriler$rzgr)) +
  geom_point(alpha=0.5, col = "#4e1c7f") +
  xlab("Buharlaþma") +
  ylab("Rüzgâr Hýzý") +
  theme_bw() +
  ggtitle("Rüzgâr Hýzý ve Buharlaþma Daðýlým Grafiði") +
  theme(plot.title = element_text(hjust = 0.5))
}
  

#box plot + scatterplot
{
scatterplot(eksiltilmis_veriler$buhr,
            eksiltilmis_veriler$rzgr,
            main = "Daðýlým Grafiði + Kutu Grafiði",
            xlab = "Buaharlaþma",
            ylab = "Rüzgâr Hýzý",
            grid = FALSE,
            col = "#4e1c7f",
            ellipse=TRUE)

scatterplot(eksiltilmis_veriler$rzgr,
            eksiltilmis_veriler$buhr,
            main = "Daðýlým Grafiði + Kutu Grafiði",
            xlab = "Rüzgâr Hýzý",
            ylab = "Buaharlaþma",
            grid = FALSE,
            col = "#4e1c7f",
            ellipse=TRUE)
}

# scatterplor + histogram
{
  p <- eksiltilmis_veriler[,-1] %>%
    ggplot( aes(y=eksiltilmis_veriler$rzgr,
                x=eksiltilmis_veriler$buhr)) +
    geom_point(color="#4e1c7f", alpha=0.4) +
    theme_ipsum() +
    theme(legend.position="none") +
    xlab("Buharlaþma") +
    ylab("Rüzgâr Hýzý")
  
  # add marginal histograms
  ggExtra::ggMarginal(p, type = "histogram",
                      color="purple")
}

# hexbin  (2d 6gen falan)  #4e1c7f
{
  ggplot(eksiltilmis_veriler, aes(y=eksiltilmis_veriler$rzgr,
                                  x=eksiltilmis_veriler$buhr) ) +
    geom_density_2d() + theme_bw() +
    ggtitle("Ýzohips Daðýlýmý") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Buharlaþma") +
    ylab("Rüzgâr Hýzý")

  # geom_bin2d() ile kare kare yapýyor
  # geom_hex() ile altýgen olarak yapýyor
  # geom_density_2d() yapýnca da izohips veriyo
  # mükemmel amk renklerini deðiþtiremedim. mavi kaldý
}

# ggplot ile Scatterplot + smooth curve güzel versiyonu
{
ggplot(data=eksiltilmis_veriler[,-1]) +
  geom_point(mapping=aes(y=rzgr, x=buhr)) +
  geom_smooth(mapping=aes(y=rzgr, x=buhr)) +
  theme_bw() +
  xlab("Buharlaþma") +
  ylab("Rüzgâr Hýzý")

ggplot(data=eksiltilmis_veriler[,-1]) +
  geom_point(mapping=aes(y=buhr, x=rzgr)) +
  geom_smooth(mapping=aes(y=buhr, x=rzgr)) +
  theme_bw() +
  xlab("Rüzgâr Hýzý") +
  ylab("Buharlaþma")
}


# bu da ggplot2 nin correlogramý
ggpairs(eksiltilmis_veriler[,-1], title="Daðýlým Grafikleri") +
  theme_bw()

# ýsý haritasý -- çalýþtýramadým
{
heatmap(numeric(eksiltilmis_veriler$rzgr),eksiltilmis_veriler$buhr)
# çalýþmýyor
hv <- heatmap(eksiltilmis_veriler[,-1], col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")
}

# anova  abline dediði anova imiþ galiba. anlayamadým
{
aovdata = aov(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr)
aovdata
plot(aovdata)
# 1. residuals vs fitted (artýk(kalan) vs uydurulmuþ)
# 2. normal Q-Q
# 3. scale - location
# 4. Residuals vs Leverage
}

# Improving the scatter plot --- son kýsýmda çalýþtýramadým
{
data %>% 
  ggplot( aes(x=eksiltilmis_veriler$rzgr, y=eksiltilmis_veriler$buhr)) +
  geom_point(color="#69b3a2", alpha=0.8) +
  ggtitle("Ground living area partially explains sale price of apartments") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  ) +
  ylab('Sale price (k$)') +
  xlab('Ground living area')

p <- data %>% 
  ggplot( aes(x=eksiltilmis_veriler$rzgr, y=eksiltilmis_veriler$buhr)) +
  geom_point(color="#69b3a2", alpha=0.8) +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  ) +
  ylab('Sale price (k$)') +
  xlab('Ground living area')

p1 <- p + ggtitle("Linear regression") +
  geom_smooth(method='lm', color="black", alpha=0.8, size=0.5, fill="skyblue")

p2 <- p + ggtitle("Loess") +
  geom_smooth(method='loess', color="black", alpha=0.8, size=0.5, fill="skyblue")

p1 %+% p2
# bu da burada hata verdi anlamadýk
}

# güneþ
sunburst(eksiltilmis_veriler)

# testler, ki-z-var.test
{
# ki-kare testi
ki_kare_verileri <- table(eksiltilmis_veriler$rzgr, eksiltilmis_veriler$buhr)
ki_kare_sonuc <- chisq.test(ki_kare_verileri)
view(ki_kare_sonuc) # bu çýkan tabloda p.value kýsmý bizi ilgilendiriyor
# bu deðer 0,05 den büyükse normal daðýlýmdýr diyorsun geçiyorsun
# bizimki normal daðýlým deðilmiþ yani (yani bizimkisi h0 teoremini reddediyor)

# z - test
t_test(eksiltilmis_veriler[,-1], formula = eksiltilmis_veriler[,-1])
t.test(eksiltilmis_veriler[,-1], alternative = c("greater"))
#sayýsal bir çýktý veriyor. önemli olan p-value
anova_test(eksiltilmis_veriler[,-1])

var.test(eksiltilmis_veriler$rzgr, eksiltilmis_veriler$buhr)
# bu çoklu analiz için kullanýlacak, varyans testi. pas

}


}