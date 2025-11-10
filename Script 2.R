#Atividade de Prática Estatística sobre o GitHub

#Ativando o Pacote para realizar ações no GitHub
library(usethis)

#Entrando com as minhas informações do GitHub
use_git_config(
  user.name = "batistone5",          
  user.email = "samuelbatistone@id.uff.br"         
)

#9) Realizando as Análises Exploratórias 

#A)Importando a Base de Dados

library(readxl) #Ativando os pacotes necessários

base = read_excel("Base_trabalho.xlsx") #Criando a base

#Transformando as variáveis qualitativas em factor
base$escolaridade = factor(base$escolaridade,
                           levels = c(1,2,3),
                           labels = c("Fundamental", "Médio", "Superior"))

base$reincidente = factor(base$reincidente,
                          levels = c(0,1),
                          labels = c("Não","Sim"))

base$filhos = factor(base$filhos,
                     levels = c(0,1),
                     labels = c("Não","Sim"))

base$sexo = factor(base$sexo,
                   levels = c(0,1),
                   labels = c("Feminino","Masculino"))

base$casado = factor(base$casado,
                     levels = c(0,1),
                     labels = c("Não","Sim"))



#B) Obtendo as medidas de posição dos dados

#Medidas sobre o score de periculosidade
media_periculosidade = mean(base$score_periculosidade)
pquartil_periculosidade = quantile(base$score_periculosidade, 0.25)
mediana_periculosidade = median(base$score_periculosidade)
tquartil_periculosidade = quantile(base$score_periculosidade, 0.75)

#Medidas sobre a idade.
media_idade = mean(base$idade)
pquartil_idade = quantile(base$idade, 0.25)
mediana_idade = median(base$idade)
tquartil_idade = quantile(base$idade, 0.75)

#Medidas sobre o tempo de prisão.
media_tempo = mean(base$tempo_preso)
pquartil_tempo = quantile(base$tempo_preso, 0.25)
mediana_tempo = median(base$tempo_preso)
tquartil_tempo = quantile(base$tempo_preso, 0.75)


#C) Gráfico de dispersão entre o tempo preso e o score de periculosidade.

library(ggplot2) #Ativando o pacote necessário

base |> 
  ggplot(aes(x = tempo_preso, y = score_periculosidade))+
  geom_point()+
  labs(x = "Tempo Preso (em meses)",
       y = "Score de Periculosidade",
       title = "Gráfico de Dispersão entre o tempo preso (em meses) e o score de periculosidade.")+
  theme_minimal()

#D) Calculando a correlação 

cor(base$tempo_preso, base$score_periculosidade)


#E) Medidas de dispersão dos dados

#Medidas de dispersão sobre o score de periculosidade
var_periculosidade = var(base$score_periculosidade)
dp_periculosidade = sqrt(var_periculosidade)
ampli_periculosidade = max(base$score_periculosidade) - min(base$score_periculosidade)

#Medidas de dispersão sobre a idade
var_idade = var(base$idade)
dp_idade = sqrt(var_idade)
ampli_idade = max(base$idade) - min(base$idade)

#Medidas de dispersão sobre o tempo de prisão
var_tempo = var(base$tempo_preso)
dp_tempo = sqrt(var_tempo)
ampli_tempo = max(base$tempo_preso) - min(base$idade)




