# N Amostral para detectar uma diferen�a de m�dia de 50% com desvio padr�o de 1
power.t.test(delta=0.5, sd=1, power=0.8)
#Obs: N para cada grupo

#Mesmo de cima, mas para uma hip�tese alternativa unicaudal (uma m�dia maior, e n�o apenas diferente da outra)
power.t.test(delta=0.5, sd=1, power=0.8, alternative = c("one.sided"))

#Ex1: Qual seria o N amostral m�nimo para um poder de 90%, Ha: ??1 > ??2 e um delta de 50%?
power.t.test(delta=0.5, sd=1, power=0.9, alternative = c("one.sided"))

#Considerando um N=128, qual o poder?
power.t.test(n=70,delta=0.5, sd=1)

#Ex1: Qual seria o poder para um delta de 30% e um N=100

#Instalando o pacote samplesize
install.packages("samplesize")
library(samplesize)

#Vari�ncias diferentes ou iguais
n.ttest(power = 0.8, alpha = 0.05, mean.diff = 0.80, sd1 = 0.83, k = 0.5,
        design = "unpaired", fraction = "balanced", variance = "equal")

n.ttest(power = 0.8, alpha = 0.05, mean.diff = 0.80, sd1 = 0.83, sd2 =
          2.65, k = 0.7, design = "unpaired", fraction = "unbalanced", variance =
          "unequal")

#N amostral para dados ordinais
n.wilcox.ord(power = 0.8, alpha = 0.05, t = 0.53, p = c(0.66, 0.15, 0.19), q = c(0.61, 0.23, 0.16))

#N amostral para compara��o de propor��es

#Condi��o: Poder de 80%, erro tipo I de 5%, Delta de 0.75 para 0.5 entre tto e placebo
#N � o n�mero de cada grupo
power.prop.test(p1 = .75, p2 = .50, power = .80)

#Mesmo de cima, mas calcule o poder para um N=60
power.prop.test(n = 1146, p1 = .025, p2 = .0166)
library(pwr)
pwr.2p2n.test( n1=1146, n2=5087, sig.level = 0.05, alternative = "two.sided")

library(SampleSize4ClinicalTrials)

#C�lculo amostral para estudo SnakeLaser II
#Considera��es gerais
#Estudo do tipo:
#Ensaio cl�nico double-blind, 2-parallel-arm, 1:1 allocation ratio, randomized
#Hip�tese nula:
#Hip�tese alternativa:

ssc_propcomp(design = 2L,
             ratio = 1, 
             alpha = 0.05, 
             power = 0.9, 
             p1 = 0.3, 
             p2 = 0.4, 
             delta = 0.1)
#Resultados: 
#Treatment Control
#1        97      97
#Refs: https://github.com/QiHongchao/SampleSize4ClinicalTrials/blob/master/README.md
#Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series.
#Yin, G. 2012. Clinical Trial Design: Bayesian and Frequentist Adaptive Methods. John Wiley & Sons.

library(TrialSize)


TwoSampleProportion.NIS(alpha = 0.05,
                        beta = 0.2, 
                        p1 = 0.3, 
                        p2 = 0.4, 
                        k = 1 , 
                        delta = 0.2, 
                        margin = 0.05)

