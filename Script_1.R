#Atividade de Prática Estatística sobre o GitHub

#Ativando o Pacote para realizar ações no GitHub
library(usethis)

#Entrando com as minhas informações do GitHub
use_git_config(
  user.name = "batistone5",          
  user.email = "samuelbatistone@id.uff.br"         
)


#1) O commit é usado para salvar as mudanças feitas no diretório do GitHub, podendo
#incluir mensagens detalhando cada mudança, criando um hitórico das mudanças feitas.


#5)Análises Gráficas

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


#B) Analisando os dados faltantes

sum(is.na(base)) 
#Como podemos ver, a base não possui dados faltantes, então não são necessárias analises posteriores.

#C) Histograma da variável idade

library(ggplot2) #Ativando o pacote para criação dos gráficos

#Criando o gráfico
g1 = base |> 
  ggplot(mapping = aes(x = idade))+
  geom_histogram(binwidth = round(sqrt(length(base$idade))), #Dando o tamanho dos intervalos com base no tamnaho da amostra
                 color = "black", #Adcionando contorno às barras
                 fill = "lightblue")+ #Mudando a cor do interior das barras
  scale_x_continuous(breaks  = seq(min(base$idade),
                                  max(base$idade),
                                  by = round(sqrt(length(base$idade)))))+ #Definindo a escala de x de acordo com os limites da amostra
  labs(title = "Histograma da Idade dos Indivíduos Pesquisados",
       x = "Idade",
       y = "Frequência Absoluta")+ #Adcionando o título e legendas
  theme_minimal()



#D) Boxplot do Tempo Preso

g2 = base |> ggplot(mapping = aes(y = tempo_preso,
                             x = ""))+
  geom_boxplot(width = 0.4,
               fill = "lightblue")+
  labs(y = "Tempo (em meses)",
       x = NULL,
       title = "Boxplot do tempo (em meses) de prisão dos indivíduos pesquisados.")+
  theme_minimal()
  

#E) Boxplot da variável score_periculosidade por escolaridade.

g3 = base |> ggplot(mapping = aes(y = tempo_preso,
                             x = escolaridade))+
  geom_boxplot(width = 0.3,
               aes(fill = escolaridade))+
  labs(y = "Tempo (em meses)",
       x = "Nível de escolaridade",
       title = "Boxplot do tempo (em meses) de prisão dos indivíduos pesquisados por nível de escolaridade.")+
  theme_minimal()+
  guides(fill = "none")+ #Retirando a legenda desnecesária
  scale_fill_manual(values = c("Fundamental" = "tomato",
                               "Médio" = "lightblue",
                               "Superior" = "lightgreen")) #Escolhendo as cores da paleta


#F)Gráfico de barras para a variável reincidente.

g4 = base |> ggplot(mapping = aes(x = reincidente))+
  geom_bar(color = "black",
           aes(fill = reincidente))+
  labs(x = "É reincidente?",
       y = "Frequência Absoluta",
       title = "Gráfico de Barras para a reincidência de prisão do Indivíduos pesquisados.")+
  theme_minimal()+
  guides(fill = "none")+
  scale_fill_manual(values = c("lightblue","tomato"))


#G) Salvando os gráficos criados

ggsave("Histograma.png", g1)
ggsave("Boxplot1.png", g2)
ggsave("Boxplot2.png", g3)
ggsave("Barras.png", g4)
