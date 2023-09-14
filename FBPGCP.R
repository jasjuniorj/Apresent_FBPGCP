########## VII Forum Brasileiro de Pós-Graduação em Ciência Política  ##############
########################## O Domínio da Média ###################################

# Pacotes

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)


# Base

baseg <- read_excel("BFundeb.xlsx")


## Modelo lineares 

## 09
baseg9 <- filter(baseg, NUM_ANO==2009)

Modlin9 <- lm(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg9)
summary(Modlin9)

## 14

baseg14 <- filter(baseg, NUM_ANO==2014)

Modlin14 <- lm(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg14)
summary(Modlin14)

## 19

baseg19 <- filter(baseg, NUM_ANO==2019)

Modlin19 <- lm(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                 DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg19)
summary(Modlin19)

## Modelos Quantílicos - Na mediana 
library(rqpd)
library(quantreg)

Modlin9q <- rq(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg9)
summary(Modlin9q)

Modlin14q <- rq(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                 DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg14)
summary(Modlin14q)

Modlin19q <- rq(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                  DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg19)
summary(Modlin19q)

## Modelos Quantílicos - Superior
library(rqpd)
library(quantreg)

Modlin9qSup <- rq(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                 DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg9,  tau = 0.75)
summary(Modlin9qSup)

Modlin14qSup <- rq(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                  DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg14,  tau = 0.75)
summary(Modlin14qSup)

Modlin19qSup <- rq(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                  DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg19,  tau = 0.75)
summary(Modlin19qSup)


## Modelos Quantílicos - Inferior
library(rqpd)
library(quantreg)

Modlin9qInf <- rq(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                    DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg9,  tau = 0.25)
summary(Modlin9qInf)

Modlin14qInf <- rq(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                     DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg14,  tau = 0.25)
summary(Modlin14qInf)

Modlin19qInf <- rq(IDEB~ MUNICIPAL+PROFISSIONAIS+INVESALUN+
                       DESPENSFUNDTOTAL+DESPEDUCTODAS, data = baseg19,  tau = 0.25)
summary(Modlin19qInf)



## Gráficos - Comparação dos coeficientes

Modcomp <- data.frame(Anos = c(2009, 2014, 2019),
              Invest = c(0.222, 0.090, 0.087, 0.270, 0.100, 0.080, 0.270,0.090,0.080,
                               0.240,0.100,0.080),
              Profis = c(0.017,0.050,0.054, 0.017, 0.054,0.059, 0.009,0.026,0.036,
                         0.023,0.072,0.061),
              
             Modelos = c("Linear", "Linear", "Linear","Quant-Mediana","Quant-Mediana","Quant-Mediana",
                         "Quant-Superior","Quant-Superior","Quant-Superior", 
                         "Quant-Inferior","Quant-Inferior","Quant-Inferior"))

Modcomp %>% filter(Modelos=="Linear"| Modelos=="Quant-Mediana") %>% 
ggplot(aes(x=Anos, y=Invest, group = Modelos, color=Modelos))+
  geom_line(size = 1.2)+scale_y_continuous(breaks = seq(0.0, 0.30, 0.03), limits = c(0.0, 0.30))+
  scale_x_continuous(breaks = seq(2009,2019,5))+theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = alpha("gray", .1)))+
  ylab("Investimento por Aluno")+
  theme_minimal()+theme(legend.position = "bottom")+scale_color_manual(values = c("#CD0000", "#062424"))+
  labs(title = "Investimento por Aluno e Ideb - Modelos Linear e 
                      Quantílico 0.5 (Mediana)")


Modcomp %>% filter(Modelos=="Linear"| Modelos=="Quant-Superior"|Modelos=="Quant-Inferior") %>% 
  ggplot(aes(x=Anos, y=Invest, group = Modelos, color=Modelos))+
  geom_line(size = 1.2)+scale_y_continuous(breaks = seq(0.0, 0.30, 0.02), limits = c(0.0, 0.30))+
  scale_x_continuous(breaks = seq(2009,2019,5))+theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = alpha("gray", .1)))+ylab("Investimento por Aluno")+
  theme_minimal()+theme(legend.position = "bottom")+scale_color_manual(values = c("#CD0000", "#062424", "#93A5B4"))+
  labs(title = "Investimento por Aluno e Ideb - Modelos Linear e
              Quantílico 0.25, 0.75")



#### Profissionais


Modcomp %>% filter(Modelos=="Linear"| Modelos=="Quant-Mediana") %>% 
  ggplot(aes(x=Anos, y=Profis, group = Modelos, color=Modelos))+
  geom_line(size = 1.2)+#scale_y_continuous(breaks = seq(0.0, 0.09, 0.01), limits = c(0.0, 0.09))+
  scale_x_continuous(breaks = seq(2009,2019,5))+theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = alpha("gray", .1)))+ylab("Invest Profissionais")+
  theme_minimal()+theme(legend.position = "bottom")+scale_color_manual(values = c("#CD0000", "#062424"))+
  labs(title = "Investimento em Profissionais e Ideb - Modelos Linear e
                    Quantílico 0.5 (Mediana)")


Modcomp %>% filter(Modelos=="Linear"| Modelos=="Quant-Superior"|Modelos=="Quant-Inferior") %>% 
  ggplot(aes(x=Anos, y=Profis, group = Modelos, color=Modelos))+
  geom_line(size = 1.2)+scale_y_continuous(breaks = seq(0.0, 0.08, 0.01), limits = c(0.0, 0.08))+
  scale_x_continuous(breaks = seq(2009,2019,5))+theme_minimal(base_size = 9) + ylab("Invest Profissionais")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = alpha("gray", .1)))+
  theme_minimal()+theme(legend.position = "bottom")+scale_color_manual(values = c("#CD0000", "#062424", "#93A5B4"))+
  labs(title = "Investimento em Profissionais - Modelos Linear e
                 Quantílico 0.25, 0.75")


## Modelos de Painel 

dados <- data.frame(
  Categoria = factor(c("A", "B", "C", "D", "E")),
  Valor = c(10, 15, 8, 12, 20)
)


ModCompPain <- data.frame(Tipo = c("Inferiror", "Mediana", "Superior", "Linear"),
                      Invest = c(0.0249,0.0249,0.0043,0.0233 ),
                      Profis = c(0.0031, 0.00055,0.00089,0.0033))


ggplot(ModCompPain,aes(x=as.factor(Tipo), y=Invest))+
  geom_line(aes(group = 1), size = 1.2)+theme(axis.text.x = element_text(angle = 90, hjust = 1),
                    legend.position = "bottom",
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_line(colour = alpha("gray", .1)))+
  scale_y_continuous(breaks = seq(0.0, 0.030, 0.005), limits = c(0.0, 0.030))+
  theme_minimal()+theme(legend.position = "bottom")+ ylab("Investimento por Aluno")+xlab(" ")+
  labs(title = "Investimento por Aluno e Ideb - Modelos com Dados de Painel - 
                                  Linear e Quantílicos")



ggplot(ModCompPain,aes(x=as.factor(Tipo), y=Profis))+
  geom_line(aes(group = 1), size = 1.2)+theme(axis.text.x = element_text(angle = 90, hjust = 1),
                                              legend.position = "bottom",
                                              panel.grid.major = element_blank(),
                                              panel.grid.minor = element_line(colour = alpha("gray", .1)))+
  scale_y_continuous(breaks = seq(0.0, 0.0050, 0.001), limits = c(0.0, 0.0050))+
  theme_minimal()+theme(legend.position = "bottom")+ ylab("Invest. Profissionais")+xlab(" ")+
  labs(title = "Investimento em Profissionais e Ideb - Modelos com Dados de Painel - 
                                  Linear e Quantílicos")







