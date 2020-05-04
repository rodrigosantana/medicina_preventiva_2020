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

########################################################################
######@> Análise exploratória da base...

######@> Criando um report contendo um resumo descritivo completo dos
######@> dados...
create_report(data = db, output_dir = "figs",
              output_file = "explorer_report.html")

########################################################################
######@> Modelo de Comparação entre Inicío e Fim do tratamento...

######@> Modelo para a presença-ausência de melhora...
mod0 <- glm(diag ~ (SEXO * IDADE * FREQUÊNCIA), data = db,
            family = "binomial", na.action = "na.fail")

#####@> Comparando os modelos finais...
mod.all <- dredge(mod0, extra = c("R^2", "deviance"))



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
