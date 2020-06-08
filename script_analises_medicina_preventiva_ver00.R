########################################################################
## Description: Análise de dados sobre medicina preventiva...
##
## Maintainer: UNIVALI / ECS
## Author: Rodrigo Sant'Ana
## Created: Sun May  3 19:03:34 2020 (-0300)
## Version: 0.0.1
##
## URL:
## Doc URL:
##
## Database info:
##
### Commentary:
##
### Code:
########################################################################

########################################################################
######@> Carregando pacotes R...

######@> Lista de pacotes...
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(explor)
library(readxl)
library(DataExplorer)
library(MuMIn)
library(ggcorrplot)

########################################################################
######@> Carregando base de dados...

######@> Dados medicina preventiva...
db <- read_excel("data/dados.xlsx", sheet = "GERAL")

########################################################################
######@> Limpeza e padronização da base de dados...

######@> Alterando as referências de sexo para compor M para homens e F
######@> para mulheres...
db$SEXO <- ifelse(db$SEXO == 0, "M", "F")

######@> Criando uma nova variável que será composta pela diferença
######@> entre avaliação da dor ao final do tratamento em relação ao início
######@> do tratamento...
db$diff <- with(db, EVA_FIM - EVA_INICIO)

######@> Criando uma nova variável contendo um marcados booleano para
######@> melhora, onde 0 é a ausência de melhora e 1 é a observação de
######@> melhora...
db$diag <- ifelse(db$diff < 0, 1, 0)

######@> Criando uma segunda base contendo os dados de antes e depois
######@> empilhados...
tmp01 <- select(db, NOME:EVA_INICIO)
tmp02 <- select(db, NOME:FREQUÊNCIA, EVA_FIM)
names(tmp01) <- c("nome", "sexo", "idade", "freq", "escore")
names(tmp02) <- c("nome", "sexo", "idade", "freq", "escore")
tmp01$trat <- "Antes"
tmp02$trat <- "Depois"
db2 <- gtools::smartbind(as.data.frame(tmp01), as.data.frame(tmp02))

########################################################################
######@> Análise exploratória da base...

######@> Criando um report contendo um resumo descritivo completo dos
######@> dados...
create_report(data = db, output_dir = "figs",
              output_file = "explorer_report.html")

########################################################################
######@> Modelo de Comparação entre Inicío e Fim do tratamento...

######@>----------------------=== Modelo Binomial para diagnóstico final

######@> Modelo para a presença-ausência de melhora...
mod0 <- glm(diag ~ (SEXO * IDADE * FREQUÊNCIA), data = db,
            family = "binomial", na.action = "na.fail")

#####@> Selecionando variáveis...
mod.all <- dredge(mod0, extra = c("R^2", "deviance"))

######@>---------------=== Modelo Poisson para comparação antes e depois

######@> Modelo full...
mod0 <- glm(escore ~ sexo * idade * trat, data = db2,
            offset = log(freq), family = "poisson",
            na.action = "na.fail")

######@> Selecionando variáveis...
mod.all <- dredge(mod0, extra = c("R^2", "deviance"))
write.table(mod.all, "tabs/Model_choice.csv", row.names = FALSE,
            sep = ";", dec = ",")

######@> Modelo final...
mod.final <- glm(escore ~ idade + trat,  data = db2,
                 offset = log(freq), family = "poisson")

#####@> ANOVA do modelo...
anova(mod.final, test = "Chisq")

#####@> Resumo geral dos parâmetros do modelo final...
summary(mod.final)

#####@> Diagnóstico do modelo final...
res <- fortify(mod.final)
res$ID <- 1:nrow(res)

p00 <- ggplot(data = res, aes(x = .resid)) +
    geom_histogram(aes(y = ..count..), binwidth = 0.5,
                   fill = "white", colour = "black",
                   boundary = 0.5) +
    scale_x_continuous(limits = c(-3.5, 3.5),
                       breaks = seq(-3.5, 3.5, 0.5),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
    labs(x = "Resíduos", y = "Frequência") +
    theme_gray(base_size = 16)
p00

p01 <- ggplot(data = res, aes(x = ID, y = .resid)) +
    geom_point(pch = 21, colour = "black", fill = "white", size = 4) +
    geom_hline(yintercept = 0, colour = "red") +
    scale_y_continuous(limits = c(-3.5, 3.5),
                       breaks = seq(-3.5, 3.5, 1),
                       expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 270), expand = c(0, 0)) +
    labs(x = "Amostra", y = "Resíduos") +
    theme_gray(base_size = 16)
p01

png("figs/residuos_modelo_poisson_ver00.png", res = 200, units = "cm",
    w = 30, h = 15)
p00 | p01
dev.off()

######@> Comparando os resultados...
out <- data.frame(expand.grid(idade = seq(40, 60, 1), freq = 12,
                              trat = c("Antes", "Depois")))
out$pred <- predict(mod.final, out, type = "response")
out$se <- predict(mod.final, out, type = "response", se.fit = TRUE)$se.fit
out$ic <- 1.96 * out$se

p02 <- ggplot() +
    geom_line(data = out, aes(x = idade, y = pred - ic, colour = trat),
              linetype = "dashed", lwd = 1) +
    geom_line(data = out, aes(x = idade, y = pred + ic, colour = trat),
              linetype = "dashed", lwd = 1) +
    geom_line(data = out, aes(x = idade, y = pred, colour = trat),
              linetype = "solid", lwd = 1) +
    scale_colour_manual("Tratamento",
                        values = c("#C77CFF", "#00BFC4")) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2),
                       expand = c(0, 0)) +
    labs(x = "Idade", "Escala de dor") +
    theme_gray(base_size = 16)
p02

png("figs/comparacao_resultados_ver00.png", res = 200, units = "cm",
    w = 30, h = 15)
p02
dev.off()

########################################################################
######@> Correlação Múltipla...

######@> Visualizando os dados...


########################################################################
##
##                  Creative Commons License 4.0
##                       (CC BY-NC-SA 4.0)
##
##  This is a humam-readable summary of (and not a substitute for) the
##  license (https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode)
##
##  You are free to:
##
##  Share - copy and redistribute the material in any medium or format.
##
##  The licensor cannot revoke these freedoms as long as you follow the
##  license terms.
##
##  Under the following terms:
##
##  Attribution - You must give appropriate credit, provide a link to
##  license, and indicate if changes were made. You may do so in any
##  reasonable manner, but not in any way that suggests the licensor
##  endorses you or your use.
##
##  NonCommercial - You may not use the material for commercial
##  purposes.
##
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
##
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
##
########################################################################
