# Organizing the data
# Picking the coluns we need and filtering the age and year we are going to use

library(readr)
library(dplyr)
library(ggplot2)
dados_orto <- read_csv("C:/Users/phabi/Desktop/P.U.C. RJ/21.2/TCC/DADOS/sih_aih_proc_orto_clean_2016_2021.csv")
dados_orto_filtrados <- dados_orto_filtrados %>%
   select(sexo, val_tot, dias_perm, morte, raça, Ano, idade_real_anos, mes_inter)
dados_orto_filtrados <- dados_orto_filtrados %>%
   rename(custo = val_tot, tinternado = dias_perm, idade = idade_real_anos, mes = mes_inter)
dados_orto_filtrados <- dados_orto_filtrados %>%
   filter(idade >17)
dados_orto_filtrados <- dados_orto_filtrados %>%
   filter(Ano <2021)

# Dividing the data by year
no_16 <- dados_orto_filtrados %>%
   filter(Ano == 2016)
ano_17 <- dados_orto_filtrados %>%
   filter(Ano == 2017)
ano_18 <- dados_orto_filtrados %>%
   filter(Ano == 2018)
ano_19 <- dados_orto_filtrados %>%
   filter(Ano == 2019)
ano_20 <- dados_orto_filtrados %>%
  filter(Ano == 2020)

# Creating a vector of the years
v_anos <- c(2016,2017,2018,2019,2020)

# Obtaining the number of internations by year and total
internacao_16 <- nrow(ano_16)
internacao_17 <- nrow(ano_17)
internacao_18 <- nrow(ano_18)
internacao_19 <- nrow(ano_19)
internacao_20 <- nrow(ano_20)
internacao_total <- sum(internacao_16,internacao_17,internacao_18,internacao_19,internacao_20)

#  Transforming internations into a vector to plot
vetor_inter <- c(internacao_16,internacao_17,internacao_18,internacao_19,internacao_20)

# Creating a data base to plot
inter_ano <- data.frame(vetor_inter, v_anos)
inter_ano <- inter_ano %>%
   rename(internacao = vetor_inter, Ano = v_anos)

# Plotting
inter_plot <- ggplot(data = inter_ano, aes(x=Ano, y= internacao)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() + labs(title = "Internações por Ano", x='Ano', y = "Internações")
inter_plot

# Obtaining the cost by year and the total cost
custo_16 <- sum(ano_16$custo)
custo_17 <- sum(ano_17$custo)
custo_18 <- sum(ano_18$custo)
custo_19 <- sum(ano_19$custo)
custo_20 <- sum(ano_20$custo)
custo_total <- sum(dados_orto_filtrados$custo)

#  Transforming the cost into a vector to plot
v_custo <- c(custo_16,custo_17,custo_18,custo_19,custo_20)

# Creating a data base to plot
custo_ano <- data.frame(v_custo, v_anos)

custo_ano <- custo_ano %>%
   rename(Custo = v_custo, Ano = v_anos)
custo_ano <- custo_ano %>%
   mutate(Custo_M = Custo/1000000)

# Plotting
custo_plot <- ggplot(data = custo_ano, aes(x= Ano, y= Custo_M))+geom_col()+ labs(title = "Custo por Ano", x= "Ano", y= "Custo em Milhões")
custo_plot

# Setting up the data to the death variable
morte_16 <- sum(ano_16$morte)
morte_17 <- sum(ano_17$morte)
morte_18 <- sum(ano_18$morte)
morte_19 <- sum(ano_19$morte)
morte_20 <- sum(ano_20$morte)
morte_vector <- c(morte_16,morte_17,morte_18, morte_19, morte_20)
total_morte <- sum(morte_vector)

# Creating a data base to plot
morte_ano = data.frame(morte_vector,v_anos)
morte_ano <- morte_ano %>%
   rename(Morte = morte_vector, Ano = v_anos)

# Plot
morte_plot <- ggplot(data = morte_ano, aes(x=Ano, y= Morte)) + geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() + labs(title = "Mortes por Ano", x='Ano', y = "Mortes")
morte_plot
