# ANPOCS - 2022 ===============================================================

## Gabriel Avila Casalecchi - UFSCar
## Filipe Vicentini Faeti - UFSCar
## Daniel Leonel da Rocha - UFRRJ

# T?tulo: Uma teoria dinâmica do apoio à democracia ===========================

# 1. Setup ====================================================================
rm(list = ls()) # limpar ambiente
gc() 

# 2. Pacotes ==================================================================
library(tidyverse)
library(janitor)
library(rio)
library(Hmisc)
library(plm)

# 3. Banco de dados ===========================================================
q = import("~/TRABALHOS/FAETI - DAN - TRAB/TRABALHO LEGO II/DemocracyOnTheBallot_Brazil2018v3.dta")

# 3.1. Filtro [recortamos as ondas de 1 at? 4] ================================
q = q %>%
  filter(wave < 5)

# 4. Variáveis ================================================================

# 4.1. polyarchy_1_1 = Tolerância sobre manifetações legais ===================
#[1 = desaprova | 7 = aprova]

########## Outras variáveis possíveis neste bloco de perguntas ##########

# polyarchy_1_2 = organização ou grupos comunitários
# polyarchy_1_3 = campanhas eleitorais
# polyarchy_1_4 = bloqueio de ruas ou rodovias
# polyarchy_1_5 = invasão de propriedade

q = q %>%
  mutate(poli1 = polyarchy_1_1) # Selecionamos esta variável no artigo

# 4.2. ing4 = democracia ? a melhor forma de governo ========================== 
# [1 = discorda muito | 7 = concorda muito]

q = q %>%
  mutate(democracia = ing4)

# 4.3. conf_votecounting = o resultado reflete o voto depositado nas urnas ====
q = q %>%
  mutate(elei??es = conf_votecounting)

# 4.4. Dummy para identificar o período de tratamento =========================
q = q %>%
  mutate(data_trat = ifelse(wave == 4, 1,0))

# 4.5. Dummy para identificar tratamento 1 [voto em bolsonaro] ================
q = q %>%
  mutate(votoBolsonaro = ifelse(cand_vote == 2, 1, 0))

# 4.6. Dummy para identificar tratamento 2 [voto em Haddad] ===================
q = q %>%
  mutate(votoHaddad = ifelse(cand_vote == 1, 1, 0))

# 5. Regressões ===============================================================

# 5.1. Apoio a democracia =====================================================

# Tratamento 1 - Voto em Bolsonaro ============================================

trat1.1 = plm(democracia ~ votoBolsonaro + data_trat + votoBolsonaro*data_trat,
              weights = weight, model = "random", 
              index = c("idnumber", "wave"),
              data = q)

summary(trat1.1)

trat1 = plm(democracia ~ votoBolsonaro + data_trat + votoBolsonaro*data_trat,
            weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat1)

phtest(trat1.1,trat1) # o modelo de efeitos fixos é melhor

mean(fixef(trat1)) #Média dos interceptos


# calculo da diferença de médias (Diff-in-Diff)

c0 = 5.189077
t0 = 5.189077 +0.067726
c1 = 5.189077 +0.400405 
t1 = 5.189077 +0.509990 +0.067726 +0.400405

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(5.189077,5.256803,5.589482,6.167198)#segundo valor (y),último valor (yend)
voto = c("(G2) não votou","(G1) votou","(G2) não votou","(G1) votou")
objeto = data.frame(x, y, voto)


objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 5.256803, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 6.167198,
           color = "black") +
  annotate("text",y=6,x=0.2,label = "R-Squared = 0.083 \np-valor < 0.000") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Bolsonaro \n1º turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()

# Tratamento 2 - Voto em Haddad ===============================================

trat2.1 = plm(democracia ~ votoHaddad + data_trat +
              votoHaddad*data_trat, weights = weight, model = "random", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat2.1)

trat2 = plm(democracia ~ votoHaddad + data_trat +
              votoHaddad*data_trat, weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat2)

phtest(trat2,trat2.1) # o modelo de efeitos fixos é melhor

mean(fixef(trat2)) #Média dos interceptos

# calculo da diferença de médias (Diff-in-Diff)

c0 = 5.20458
t0 = 5.20458 +0.040446 
c1 = 5.20458 +0.705726    
t1 = 5.20458 -0.804497 +0.040446 +0.705726  

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(5.20458,5.245026,5.910306,5.146255)#segundo valor (y),?ltimo valor (yend)
voto = c("(G2) não votou","(G1) votou","(G2) não votou","(G1) votou")
objeto = data.frame(x, y, voto)

objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 5.245026, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 5.146255,
           color = "black") +
  annotate("text",y=5.8,x=0.2,label = "R-Squared = 0.081 \np-valor < 0.000") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Hadadd \n1º turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()

# 5.2. Tolerancia com manifesta??es legais ====================================

# Tratamento 1 - Voto em Bolsonaro ============================================

trat1.1 = plm(poli1 ~ votoBolsonaro + data_trat + votoBolsonaro*data_trat,
            weights = weight, model = "random", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat1.1)

trat1 = plm(poli1 ~ votoBolsonaro + data_trat + votoBolsonaro*data_trat,
            weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat1)

phtest(trat1,trat1.1) # o modelo de efeitos fixos ? melhor

mean(fixef(trat1)) #M?dia dos interceptos

# calculo da diferen?a de m?dias (Diff-in-Diff)

c0 = 5.356646
t0 = 5.356646 +0.541473
c1 = 5.356646 -0.119546 
t1 = 5.356646 +0.130695 +0.541473-0.119546

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(5.356646,5.898119,5.2371,5.909268)#segundo valor (y),?ltimo valor (yend)
voto = c("(G2) n?o votou","(G1) votou","(G2) n?o votou","(G1) votou")
objeto = data.frame(x, y, voto)


objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 5.898119, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 5.909268,
           color = "black") +
  annotate("text",y=5.7,x=0.2,label = "R-Squared = 0.001 \np-valor < 0.000") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Bolsonaro \n1? turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()

# Tratamento 2 - Voto em Haddad ===============================================

trat2.1 = plm(poli1 ~ votoHaddad + data_trat +
              votoHaddad*data_trat, weights = weight, model = "random", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat2.1)

trat2 = plm(poli1 ~ votoHaddad + data_trat +
              votoHaddad*data_trat, weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat2)

phtest(trat2,trat2.1) # o modelo de efeitos fixos ? melhor

mean(fixef(trat2)) #M?dia dos interceptos

# calculo da diferen?a de m?dias (Diff-in-Diff)

c0 = 5.635832
t0 = 5.635832 -0.855512 
c1 = 5.635832 -0.037202    
t1 = 5.635832 +0.103475-0.855512 -0.037202  

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(5.635832,4.78032,5.59863,4.846593)#segundo valor (y),?ltimo valor (yend)
voto = c("(G2) n?o votou","(G1) votou","(G2) n?o votou","(G1) votou")
objeto = data.frame(x, y, voto)

objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 4.78032, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 4.846593,
           color = "black") +
  annotate("text",y=5.2,x=0.2,label = "R-Squared = 0.005 \np-valor < 0.000") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Hadadd \n1? turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()

# 5.3. Confian?a [o resultado das urnas reflete o voto nas urnas] =============

# Tratamento 1 - Voto em Bolsonaro ============================================

trat1.1 = plm(elei??es ~ votoBolsonaro + data_trat + votoBolsonaro*data_trat,
            weights = weight, model = "random", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat1.1)

trat1 = plm(elei??es ~ votoBolsonaro + data_trat + votoBolsonaro*data_trat,
            weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat1)

phtest(trat1,trat1.1) # o modelo de efeitos fixos ? melhor

mean(fixef(trat1)) #M?dia dos interceptos

# calculo da diferen?a de m?dias (Diff-in-Diff)

c0 = 3.549046
t0 = 3.549046 +0.060306
c1 = 3.549046 +0.485984 
t1 = 3.549046 +0.864778 +0.060306 +0.485984

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(3.549046,3.609352,4.03503,4.960114)#segundo valor (y),?ltimo valor (yend)
voto = c("(G2) n?o votou","(G1) votou","(G2) n?o votou","(G1) votou")
objeto = data.frame(x, y, voto)


objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 3.609352, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 4.960114,
           color = "black") +
  annotate("text",y=4.8,x=0.2,label = "R-Squared = 0.079 \np-valor < 0.000") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Bolsonaro \n1? turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()

# Tratamento 2 - Voto em Haddad ===============================================

trat2.1 = plm(elei??es ~ votoHaddad + data_trat +
              votoHaddad*data_trat, weights = weight, model = "random", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat2.1)

trat2 = plm(elei??es ~ votoHaddad + data_trat +
              votoHaddad*data_trat, weights = weight, model = "within", 
            index = c("idnumber", "wave"),
            data = q)

summary(trat2)

phtest(trat2,trat2.1) # o modelo de efeitos fixos ? melhor

mean(fixef(trat2)) #M?dia dos interceptos

# calculo da diferen?a de m?dias (Diff-in-Diff)

c0 = 3.514687
t0 = 3.514687 +0.45773 
c1 = 3.514687 +0.92963    
t1 = 3.514687 -0.65285 +0.45773 +0.92963  

df_t = t1 - t0
df_c = c1 - c0
df_df = df_t - df_c

x = c(0,0,1,1)
y = c(3.514687,3.972417,4.444317,4.249197)#segundo valor (y),?ltimo valor (yend)
voto = c("(G2) n?o votou","(G1) votou","(G2) n?o votou","(G1) votou")
objeto = data.frame(x, y, voto)

objeto %>% ggplot(aes(x = x, y = y, 
                      color = voto)) +
  geom_point() +
  geom_line(aes(group = voto)) +
  annotate(geom = "segment", x = 0, xend = 1,
           y = 3.972417, yend = t1 - df_df,
           linetype = "dotted") +
  annotate(geom = "segment", x = 1, xend = 1,
           y = t1 - df_df, yend = 4.249197,
           color = "black") +
  annotate("text",y=4.5,x=0.2,label = "R-Squared = 0.069 \np-valor < 0.000") +
  scale_x_continuous(breaks = c(0,1)) +
  labs(
    y = "",
    x = "",
    color = "Voto em Hadadd \n1? turno 2018",
    caption = "Fonte: Democracy on the Ballot: Brazil 2018") +
  theme_bw()

# 6. Conclus?es ===============================================================

# As atitudes democr?ticas variam em um curto espa?o de tempo
# Houve um crescimento do apoio a valores democráticos entre eleitores que votaram no candidato vencedor
