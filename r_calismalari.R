# K�T�PHANELER
{
library(readr)
library(corrplot)
library(zoo)
library(date)
library(psych)
library(DescTools)
library(ehaGoF)
library(raster)   # varyasyon katsay�s�
library(ggplot2)   # daha g�zel pie grafikleri i�in
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
library(ISLR) # grafikleri �st �ste yazd�rmak i�in
library(ggpubr)
library(graphics)
}

eksiltilmis_veriler <- read_delim("eksiltilmis_veriler.csv", ";",
                  escape_double = FALSE,
                  col_types = cols(trh = col_date(format = "%d.%m.%Y")), 
                  trim_ws = TRUE)
eksiltilmis_veriler

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#     TEK BOYUTLU ANAL�ZLER
{
### merkezi e�ilim �l��leri
{
mean(eksiltilmis_veriler$rzgr)
mean(eksiltilmis_veriler$buhr)  #'ortalama'

geometric.mean(eksiltilmis_veriler$rzgr)   # geometrik ortalama
geometric.mean(eksiltilmis_veriler$buhr, na.rm = TRUE) # demek ki buhar�n s�f�r oldu�u zaman var

harmonic.mean(eksiltilmis_veriler$rzgr)    # harmonik ortalama
harmonic.mean(eksiltilmis_veriler$buhr)   # verilerimde s�f�r var

median(eksiltilmis_veriler$rzgr)
median(eksiltilmis_veriler$buhr)    # medyan

Mode(eksiltilmis_veriler$rzgr)   # mod
Mode(eksiltilmis_veriler$buhr)  
}

### merkezi yay�l�m �l��leri
{
    min(eksiltilmis_veriler$rzgr)   #min de�er
    min(eksiltilmis_veriler$buhr)
    
    max(eksiltilmis_veriler$rzgr)   # max de�r
    max(eksiltilmis_veriler$buhr)

    sd(eksiltilmis_veriler$rzgr)  #  standart sapmalar 
    sd(eksiltilmis_veriler$buhr)
    
    var(eksiltilmis_veriler$rzgr)
    var(eksiltilmis_veriler$buhr)   #varyans
    
    cv(eksiltilmis_veriler$rzgr)   # varyasyon katsay�s�
    cv(eksiltilmis_veriler$buhr)
    
    quantile(eksiltilmis_veriler$rzgr, c(.32, .57, .98))    # �eyreklikler
    quantile(eksiltilmis_veriler$rzgr, c(.25, .50, .75))
    quantile(eksiltilmis_veriler$buhr, c(.25, .50, .75))
    quantile(eksiltilmis_veriler$rzgr)
    quantile(eksiltilmis_veriler$buhr)
    
    IQR(eksiltilmis_veriler$rzgr)    # �eyreklikler a��kl���
    IQR(eksiltilmis_veriler$buhr) # �st �eyreklik - alt �eyreklik  (%75-%25)
    
    mad(eksiltilmis_veriler$rzgr)  # medyan mutlak sapma
    mad(eksiltilmis_veriler$buhr)
    
    MeanAD(eksiltilmis_veriler$rzgr)  # ortalama mutlak sapma
    MeanAD(eksiltilmis_veriler$buhr)

    skew(eksiltilmis_veriler$rzgr, na.rm = FALSE, type = 3)   # �arp�kl�k
    skew(eksiltilmis_veriler$buhr)
    
    Kurt(eksiltilmis_veriler$rzgr)   # bas�kl�k
    Kurt(eksiltilmis_veriler$buhr)
}

### P de�eri i�in bir tak�m testler
{ 
  shapiro.test(eksiltilmis_veriler$rzgr)  # Shapiro-Wilk Test
  shapiro.test(eksiltilmis_veriler$buhr)
  
  # Kolmogorov Smirnov Test
  ks.test(eksiltilmis_veriler$rzgr, eksiltilmis_veriler$buhr)
  ks.test(eksiltilmis_veriler$buhr, eksiltilmis_veriler$rzgr)  # hatal�
  ks.test(eksiltilmis_veriler$rzgr, eksiltilmis_veriler$buhr)
  plot(ecdf(eksiltilmis_veriler$rzgr), 
       xlim = range(c(eksiltilmis_veriler$rzgr, eksiltilmis_veriler$buhr)),
       xlab = "De�erler",
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

### tek boyut i�in grafikler
{

# histogram �al��malar�
  {
  hist(eksiltilmis_veriler$rzgr)  # histogram
  hist(eksiltilmis_veriler$buhr,
       breaks= c(30),
       main = "Buharla�ma Histogram�",
       xlab = "Buharla�ma",
       ylab = "Frekans")
  
  hist(eksiltilmis_veriler$rzgr,
       border = "red",
       col = "#4e1c7f",
       main = "R�zg�r H�z� Histogram�",
       xlab = "R�zg�r H�z�",
       ylab = "Frekans",
       breaks = c(40),
       density = NULL,
       freq = FALSE)
  
  hist(eksiltilmis_veriler$buhr,
       border = "red",
       col = "#4e1c7f",
       main = "Buharla�ma Histogram�",
       xlab = "Buharla�ma",
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
  # neden �st �ste �izmedi anlamad�m. ama g�zel bi�e
  }
  
# g�vde yaprak metodu
stem(eksiltilmis_veriler$rzgr, width = 70)
stem(eksiltilmis_veriler$buhr, width = 70)
stem(eksiltilmis_veriler$buhr, scale = 0.3)

# boxplot �al��malar�
{
boxplot(eksiltilmis_veriler$rzgr,
        ylab="R�zg�r H�z�",
        main="R�zg�r H�z� Kutu Grafi�i",
        col = "#4e1c7f",
        border = "red",
        varwidth = TRUE,
        notch = FALSE,
        horizontal = TRUE)

boxplot(eksiltilmis_veriler$buhr,
        ylab="Buharla�ma",
        main="Buharla�ma ��in Kutu Grafi�i",
        col = "#4e1c7f",
        border = "red",
        varwidth = TRUE,
        notch = FALSE,
        horizontal = TRUE)

boxplot(eksiltilmis_veriler[,-1],
        main="R�zg�r H�z� ve Buharla�ma ��in Ortak Kutu Grafi�i",
        col = "#4e1c7f",
        border = "red",
        varwidth = TRUE,
        notch = FALSE,
        names=c("R�zg�r H�z�","Buharla�ma"),
                horizontal = TRUE)   # boxplot

boxplot(eksiltilmis_veriler$buhr, horizontal = TRUE)   # yatay boxplot
}


#----------------------------------------------------------

# barplot, buna �ok da gerek yok zaten histogram�m�z var
{
barplot(eksiltilmis_veriler$rzgr)

barplot(table(eksiltilmis_veriler$rzgr),
        main = "R�zgar H�z� Bar Grafi�i (D��ey)",
        xlab = "R�zgar H�z�",
        ylab = "R�zgar H�z� Tekrar�",
        col = "darkred",
        horiz = FALSE)

barplot(table(eksiltilmis_veriler$buhr),
        main = "Buharla�ma Bar Grafi�i (D��ey)",
        xlab = "Buharla�ma",
        ylab = "Buharla�ma Tekrar�",
        col = "darkred",
        horiz = FALSE)

}

# KDE bu i�te bu da tek 
# �ekirdek yo�unlu�u tahmin grafi�i
{
r_kde = plot(density(eksiltilmis_veriler$rzgr),
     pch = 3,
     main="R�zg�r H�z�n�n Yo�unluk Grafi�i",
     ylab="Yo�unluk",
     col="red")

b_kde = plot(density(eksiltilmis_veriler$buhr),
     pch = 3,
     main="Buharla�man�n Yo�unluk Grafi�i",
     ylab="Yo�unluk",
     col="red")

# kde + kde
data(eksiltilmis_veriler[,-1])
class(eksiltilmis_veriler[,-1]); dim(eksiltilmis_veriler[,-1]);str(eksiltilmis_veriler[,-1])
age2 <- dnorm(eksiltilmis_veriler$rzgr, mean(eksiltilmis_veriler$rzgr), sd(eksiltilmis_veriler$rzgr))
age1 <- dnorm(eksiltilmis_veriler$buhr, mean(eksiltilmis_veriler$buhr), sd(eksiltilmis_veriler$buhr))

ggplot(data=eksiltilmis_veriler[,-1]) +
  geom_line(mapping=aes(x=buhr, y=age1), fill="darkred", color="darkred") +
  geom_line(mapping=aes(x=rzgr, y=age2), fill="red", color="red") +
  labs(title = "Ortak KDE Grafi�i", x = "De�erler", y = "Yo�unluk") +
  theme_classic()
}


# Q-Q grafi�i
{
qqPlot(eksiltilmis_veriler$rzgr,
       main="R�zg�r H�z� Q-Q Grafi�i",
       ylab = "R�zg�r H�z�")
qqPlot(eksiltilmis_veriler$buhr, main="Buharla�ma Q-Q Grafi�i")
qqnorm(eksiltilmis_veriler$rzgr)
qqnorm(eksiltilmis_veriler$buhr)

# ggplot k�t�phanesi ile �izelim daha g�zel
ggplot(eksiltilmis_veriler[,-1],
       aes(sample = rzgr, colour = factor(rzgr))) +
  stat_qq() +
  stat_qq_line() # rengarenk g�zel bir �ey

ggplot(eksiltilmis_veriler[,-1],
       aes(sample = rzgr)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Ortak KDE Grafi�i", x = "De�erler", y = "Yo�unluk")

ggqqplot(eksiltilmis_veriler[,-1], x = "rzgr",
         color = "red", 
         palette = c("#4e1c7f", "#4e1c7f"),
         ggtheme = theme_pubclean()) +
  labs(title = "R�zg�r H�z� Q-Q Grafi�i", y = "De�erler")

ggqqplot(eksiltilmis_veriler[,-1], x = "buhr",
         color = "red", 
         palette = c("#4e1c7f", "#4e1c7f"),
         ggtheme = theme_pubclean()) +
  labs(title = "Buharla�ma Q-Q Grafi�i", y = "De�erler")
}


# zaman serisi �al��malar�
{
# zaman serisini b�yle de elde edebilyoruz
# zaman serisi i�ni ayr� bir fonksiyon vard�r (var ama, tarihi yapamad�k)
zaman_serisi = plot(zoo(eksiltilmis_veriler$rzgr,as.Date(eksiltilmis_veriler$trh,"%d.%m.%Y")),
     xlab="Zaman", ylab="R�zg� H�z�",col="red",
     main="R�zg�r H�z� Zaman Serisi")

zaman_serisi = plot(zoo(eksiltilmis_veriler$buhr,as.Date(eksiltilmis_veriler$trh,"%d.%m.%Y")),
                    xlab="Zaman", ylab="Buharla�ma",col="red",
                    main="Buharla�ma Zaman Serisi")

plot.ts(log(eksiltilmis_veriler$buhr),
        main="Buharla�ma Logaritmik Zaman Serisi",
        col="Purple")

{
  ## r�zgar h�z� ayl�k y�ll�k haftal�k zaman serileri
  ruzgar_ts2 <- xts(eksiltilmis_veriler[,-1], order.by = eksiltilmis_veriler$trh)
  
  haftalik_zs <- apply.weekly(ruzgar_ts2,FUN = mean)
  aylik_zs <- apply.monthly(ruzgar_ts2, FUN = mean)
  yilllik_zs <- apply.yearly(ruzgar_ts2, FUN = mean)
  gunluk_zs <- apply.daily(ruzgar_ts2, FUN = mean)
  
  plot.ts(yilllik_zs[,1],
          xlab="Zaman", ylab="Y�ll�k",col="red")
  plot.ts(aylik_zs[,1],
          xlab="Zaman", ylab="Ayl�k",col="red",)
  
  plot.ts(haftalik_zs[,1],
          xlab="Zaman", ylab="Haftal�k",col="red",)
  
  plot.ts(gunluk_zs[,1],
          xlab="Zaman", ylab="G�nl�k",col="red",)
  
  ## eksen isimlerini de�i�tiremedim. manuel olarak ayarlayaca��m.
  
  
  eksiltilmis_aylik = apply.yearly(xts(eksiltilmis_veriler, order.by = eksiltilmis_veriler$trh), FUN=mean)
  plot.ts(aylik_zs, plot.type = "multiple")
  plot.ts(eksiltilmis_veriler[,-1], plot.type = "multiple")
  aa_kontrol = aylik_zs[,-1]
  
  ## buharla�ma ayl�k y�ll�k haftal�k zaman serileri
  buharlasma_ts2 <- xts(eksiltilmis_veriler$buhr, order.by = eksiltilmis_veriler$trh)
  
  haftalik_zs_b <- apply.weekly(buharlasma_ts2,FUN = mean)
  aylik_zs_b <- apply.monthly(buharlasma_ts2, FUN = mean)
  yilllik_zs_b <- apply.yearly(buharlasma_ts2, FUN = mean)
  gunluk_zs_b <- apply.daily(buharlasma_ts2, FUN = mean)
  
  plot.ts(yilllik_zs_b[,1],
          xlab="Zaman", ylab="Y�ll�k",col="red")
  
  plot.ts(aylik_zs_b[,1],
          xlab="Zaman", ylab="Ayl�k",col="red")
  
  plot.ts(haftalik_zs_b[,1],
          xlab="Zaman", ylab="Haftal�k",col="red")
  
  plot.ts(gunluk_zs_b[,1],
          xlab="Zaman", ylab="G�nl�k",col="red")
  
  
  
  plot(zoo(c(2), as.Date(eksiltilmis_veriler$trh, "%d.%m.%Y")),
       xlab="Zaman", ylab=" ",col="red")
}


}


# korelasyon grafik �al��malar�
{
# bir anda s�f�ra gidiyorsa dura�and�r /// r�zgar dura�an yani
# otokrelasyon
acf(eksiltilmis_veriler$rzgr,
    main="R�zg�r H�z�n�n Otokorelasyon Grafi�i")
acf(eksiltilmis_veriler$buhr,
    main="Buharla�man�n Otokorelasyon Grafi�i")

# k�smi otokrolasyon
pacf(eksiltilmis_veriler$rzgr,
     main="R�zg�r H�z�n�n K�smi Otokorelasyon ")
pacf(eksiltilmis_veriler$buhr,
     main="Buharla�man�n K�smi Otokorelasyon Grafi�i")

}


# pch noktlar�n �eklini belirliyor pasta grafi�i gibi
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



# keman grafi�i (violin plot)
{
vioplot(eksiltilmis_veriler$rzgr,
        main="R�zg�r H�z� Keman Grafi�i",
        ylab="R�zg�r H�z� De�erleri",
        col="#4e1c7f",
        border="red")

vioplot(eksiltilmis_veriler$buhr,
        main="Buharla�ma Keman Grafi�i",
        col="#4e1c7f",
        ylab = "Buharla�ma De�erleri",
        border="red")

#kemanlar�n birlikte incelenmesi
vioplot(eksiltilmis_veriler$rzgr,
        eksiltilmis_veriler$buhr,
        names=c("R�zg�r H�z�", "Buharla�ma"),
        main = "Ortak Keman Grafi�i", col="#4e1c7f",
        ylab = "De�erler",
        border = "red")

}
}
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#     �K� BOYUTLU ANAL�ZLER
{
#'korelasyon uygulamak i�in bu fonk'
{
cor(eksiltilmis_veriler[,-1], method = "kendall")
cor.plot(cor(eksiltilmis_veriler[,-1]), method="kendall")

# projeme koydu�um g�sterim
korelasyon <- cor(eksiltilmis_veriler[,-1], method = "kendall")
colnames(korelasyon) <- c("R�zg�r H�z�", "Buharla�ma")
rownames(korelasyon) <- c("R�zg�r H�z�", "Buharla�ma")
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
     xlab="R�zg�r H�z�",
     ylab="Buharla�ma") # sadece iki nokta �iziyor
cov(eksiltilmis_veriler[,-1])  # say�sal veri
}

# regresyon -projede var ama geli�tirilecek-
{
  lmn <- lm(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr)
  abline(plot(lmn), main="R�zg�r H�z� ve Buharla�ma") #Add a regression line
  abline(lm(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr))
  lmTemp = lm(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr) #Create the linear regression
  # burada verdi�i �izimler anova sonu�lar� imi�
  
  abline(lmTemp, cex = 1.3, pch = 16, xlab = "Weight in Kg", ylab = "Height in cm")
  
  relation <- lm(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr)
  
  abline(plot(relation))
  
  plot(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr, data = eksiltilmis_veriler)
  qqnorm(eksiltilmis_veriler$buhr, main = "Normallik Grafi�i")
  qqline(eksiltilmis_veriler$buhr)
  
  sonuc <- lm(eksiltilmis_veriler$rzgr ~ eksiltilmis_veriler$buhr)
  
  summary(sonuc)
  
  par(mfrow=c(2,2))
  plot(sonuc)
}


# da��l�m grafi�i (skatter)
pairs(eksiltilmis_veriler[,-1],
      col = "#4e1c7f",
      main = "Da��l�m Grafikleri",
      labels = c("R�zg�r H�z�", "Buharla�ma"))   # bu R �n kendi correlogram�

# bubble plot yapmaya �al��t�m
# b�y�kl�kleri ayarlayama
{
data <- eksiltilmis_veriler[,-1] 
ggplot(data, aes(x=eksiltilmis_veriler$buhr,
                 y=eksiltilmis_veriler$rzgr)) +
  geom_point(alpha=0.5, col = "#4e1c7f") +
  xlab("Buharla�ma") +
  ylab("R�zg�r H�z�") +
  theme_bw() +
  ggtitle("R�zg�r H�z� ve Buharla�ma Da��l�m Grafi�i") +
  theme(plot.title = element_text(hjust = 0.5))
}
  

#box plot + scatterplot
{
scatterplot(eksiltilmis_veriler$buhr,
            eksiltilmis_veriler$rzgr,
            main = "Da��l�m Grafi�i + Kutu Grafi�i",
            xlab = "Buaharla�ma",
            ylab = "R�zg�r H�z�",
            grid = FALSE,
            col = "#4e1c7f",
            ellipse=TRUE)

scatterplot(eksiltilmis_veriler$rzgr,
            eksiltilmis_veriler$buhr,
            main = "Da��l�m Grafi�i + Kutu Grafi�i",
            xlab = "R�zg�r H�z�",
            ylab = "Buaharla�ma",
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
    xlab("Buharla�ma") +
    ylab("R�zg�r H�z�")
  
  # add marginal histograms
  ggExtra::ggMarginal(p, type = "histogram",
                      color="purple")
}

# hexbin  (2d 6gen falan)  #4e1c7f
{
  ggplot(eksiltilmis_veriler, aes(y=eksiltilmis_veriler$rzgr,
                                  x=eksiltilmis_veriler$buhr) ) +
    geom_density_2d() + theme_bw() +
    ggtitle("�zohips Da��l�m�") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Buharla�ma") +
    ylab("R�zg�r H�z�")

  # geom_bin2d() ile kare kare yap�yor
  # geom_hex() ile alt�gen olarak yap�yor
  # geom_density_2d() yap�nca da izohips veriyo
  # m�kemmel amk renklerini de�i�tiremedim. mavi kald�
}

# ggplot ile Scatterplot + smooth curve g�zel versiyonu
{
ggplot(data=eksiltilmis_veriler[,-1]) +
  geom_point(mapping=aes(y=rzgr, x=buhr)) +
  geom_smooth(mapping=aes(y=rzgr, x=buhr)) +
  theme_bw() +
  xlab("Buharla�ma") +
  ylab("R�zg�r H�z�")

ggplot(data=eksiltilmis_veriler[,-1]) +
  geom_point(mapping=aes(y=buhr, x=rzgr)) +
  geom_smooth(mapping=aes(y=buhr, x=rzgr)) +
  theme_bw() +
  xlab("R�zg�r H�z�") +
  ylab("Buharla�ma")
}


# bu da ggplot2 nin correlogram�
ggpairs(eksiltilmis_veriler[,-1], title="Da��l�m Grafikleri") +
  theme_bw()

# �s� haritas� -- �al��t�ramad�m
{
heatmap(numeric(eksiltilmis_veriler$rzgr),eksiltilmis_veriler$buhr)
# �al��m�yor
hv <- heatmap(eksiltilmis_veriler[,-1], col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")
}

# anova  abline dedi�i anova imi� galiba. anlayamad�m
{
aovdata = aov(eksiltilmis_veriler$rzgr~eksiltilmis_veriler$buhr)
aovdata
plot(aovdata)
# 1. residuals vs fitted (art�k(kalan) vs uydurulmu�)
# 2. normal Q-Q
# 3. scale - location
# 4. Residuals vs Leverage
}

# Improving the scatter plot --- son k�s�mda �al��t�ramad�m
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
# bu da burada hata verdi anlamad�k
}

# g�ne�
sunburst(eksiltilmis_veriler)

# testler, ki-z-var.test
{
# ki-kare testi
ki_kare_verileri <- table(eksiltilmis_veriler$rzgr, eksiltilmis_veriler$buhr)
ki_kare_sonuc <- chisq.test(ki_kare_verileri)
view(ki_kare_sonuc) # bu ��kan tabloda p.value k�sm� bizi ilgilendiriyor
# bu de�er 0,05 den b�y�kse normal da��l�md�r diyorsun ge�iyorsun
# bizimki normal da��l�m de�ilmi� yani (yani bizimkisi h0 teoremini reddediyor)

# z - test
t_test(eksiltilmis_veriler[,-1], formula = eksiltilmis_veriler[,-1])
t.test(eksiltilmis_veriler[,-1], alternative = c("greater"))
#say�sal bir ��kt� veriyor. �nemli olan p-value
anova_test(eksiltilmis_veriler[,-1])

var.test(eksiltilmis_veriler$rzgr, eksiltilmis_veriler$buhr)
# bu �oklu analiz i�in kullan�lacak, varyans testi. pas

}


}