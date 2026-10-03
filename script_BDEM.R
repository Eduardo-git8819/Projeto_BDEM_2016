# script roteiro do BDEM - no repositório Projeto_BDEM_2016
# Antes de começar a fazer qualquer coisa:
# a) Coloque todos os arquivos postados no Classroom (já descompactados) dentro do repositório local Projeto_BDEM_2016
# b) commit este roteiro com a mensagem "dados, arquivos de texto e script roteiro BDEM" e envie para o repositório Projeto_BDEM_2016
# c) salve o script com outro nome (script_BDEM.R) e commit com a mensagem "script BDEM" e envie para o repositório Projeto_BDEM_2016

# Ao inserir os comandos em cada Tarefa de cada Etapa, mantenha as linhas de comentários e orientações colocadas pela professora


##################################
# ETAPA 1: BANCO DE DADOS DO SIM
##################################
# Você deve criar e estar na branch SIM antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1. Leitura do banco de dados SIM_2016 com 1309774 linhas e 87 colunas com o nome de dados_sim
# Verificar se a leitura foi feita corretamente e a estrutura dos dados

dados_sim<-read.csv("SIM_2016.csv", header = T, sep =";")
str(dados_sim)
summary(dados_sim)
View(dados_sim)

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SIM - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# As colunas serão: 1, 3, 9, 10, 11, 14, 17, 35, 47
# Nomes das respectivas variáveis: CONTADOR, TIPOBITO, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, CAUSABAS

dados_sim_1<-dados_sim[,c(1,3,9,10,11,14,17,35,47)]
View(dados_sim_1)

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SIM - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11:8344      12:3763     13:16799    14:2157      15:38557     16:2995     17:7490
# 21:34362     22:19187    23:54276    24:21922     25:28041     26:66928    27:20769    28:13516     29:88094
# 31:135257    32:22868    33:141089   35:296359
# 41:74740     42:40270    43:87583
# 50:16749     51:17535    52:38074    53:12050 

dados_sim_2<-subset(dados_sim_1, substr(dados_sim_1$CODMUNRES,1,2) == "22" )
View(dados_sim_2)

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SIM - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis:
# TIPOBITO, SEXO, RACACOR, ESC2010, TPMORTEOCO, CAUSABAS
# Avalie também os valores das variável IDADE (não estranhe mas idade é composta de um dígito inicial que indica a unidade de medida)
# Unidades de medida a serem consideradas em IDADE: 0: minutos, 1: horas, 2: dias, 3: meses, 4: anos, 5: idade maior que 100 anos
# Atenção: a unidade de medida de IDADE no DICIONÀRIO do SIM está errada
# O propósito das avaliações acima é verificar se as categorias estão de acordo com o dicionário do SIM ou se aparecem categorias estranhas

freqTIPOBITO<-table(dados_sim_2$TIPOBITO)
barplot(freqTIPOBITO, main="Frequência das categorias de TIPOBITO", col="Green")

freqSEXO<-table(dados_sim_2$SEXO)
barplot(freqSEXO, main="Frequência das categorias de SEXO", col="Blue")

freqRACACOR<-table(dados_sim_2$RACACOR)
barplot(freqRACACOR, main="Frequência das categorias de RACACOR", col="Yellow")

freqESC2010<-table(dados_sim_2$ESC2010)
barplot(freqESC2010, main="Frequência das categorias de ESC2010", col="Lightblue")

freqTPMORTEOCO<-table(dados_sim_2$TPMORTEOCO)
barplot(freqTPMORTEOCO, main="Frequência das categorias de TPMORTEOCO", col="DarkBlue")

freqCAUSABAS<-table(dados_sim_2$CAUSABAS)
barplot(freqCAUSABAS, main="Frequências das categorias de CAUSABAS", col="Darkgreen")

CID<-substr(as.character(dados_sim_2$CAUSABAS),1,1)
freqCID<-table(CID)
barplot(freqCID, main="Frequências das categorias de CAUSABAS, agrupadas em grupos", col="Darkorange")


unidadeIDADE<-substr(dados_sim_2$IDADE,1,1)
freqIDADE<-table(unidadeIDADE)
barplot(freqIDADE, main="Frequências agrupadas das categorias da unidade de IDADE", col="Purple")


# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SIM - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# Verifique o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 9999 para NA

dados_sim_2$TIPOBITO[dados_sim_2$TIPOBITO == 9] <- NA

dados_sim_2$SEXO[dados_sim_2$SEXO == 9] <- NA

dados_sim_2$RACACOR[dados_sim_2$RACACOR == 9] <- NA

dados_sim_2$ESC2010[dados_sim_2$ESC2010 == 9] <- NA

dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] <- NA

dados_sim_2$IDADE[substr(as.character(dados_sim_2$IDADE), 1, 1) == "9"] <- NA

summary(dados_sim_2$IDADE)

# Ao terminar a Tarefa 5 commit com a mensagem "script BDEM - SIM - tarefas 1 a 5" e envie para o repositório Projeto_BDEM_2016


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a PRIMEIRA LETRA da legenda é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis dentro do banco de dados

dados_sim_2$TIPOBITO <- factor(dados_sim_2$TIPOBITO, levels = c(1, 2), labels = c("Fetal", "Não fetal"))

dados_sim_2$SEXO <- factor(dados_sim_2$SEXO, levels = c(1, 2), labels = c("Masculino", "Feminino"))

dados_sim_2$RACACOR <- factor(dados_sim_2$RACACOR, levels = c(1, 2, 3, 4, 5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

dados_sim_2$ESC2010 <- factor(dados_sim_2$ESC2010, levels = c(0, 1, 2, 3, 4, 5), labels = c("Sem escolaridade", "Fundamental I", "Fundamental II", "Médio", "Superior incompleto", "Superior completo"))

dados_sim_2$TPMORTEOCO <- factor(dados_sim_2$TPMORTEOCO, levels = c(1, 2, 3, 4, 5, 8), labels = c("Gravidez", "Parto", "Aborto", "Puerpério", "Pós-parto tardio", "Não ocorreu nestas fases"))

# Ao terminar a Tarefa 6 commit com a mensagem "script BDEM - SIM - tarefas 1 a 6" e envie para o repositório Projeto_BDEM_2016


# Tarefa 7. Criar um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 - SIM.pdf”
# Atenção: a ordem das variáveis do arquivo deve ser respeitada

municipios <- sort(unique(dados_sim_2$CODMUNRES))

SIM_PI <- data.frame(
  ANO = rep(2016, length(municipios)),
  NIVEL = rep("MUNICIPIO", length(municipios)),
  CODMUNRES = municipios
)


SIM_PI$TO <- as.vector(table(dados_sim_2$CODMUNRES))
SIM_PI$TO_M <- as.vector(tapply(dados_sim_2$SEXO == "Masculino", dados_sim_2$CODMUNRES, sum, na.rm = TRUE))
SIM_PI$TO_F <- as.vector(tapply(dados_sim_2$SEXO == "Feminino", dados_sim_2$CODMUNRES, sum, na.rm = TRUE))
SIM_PI$TO_FT <- as.vector(tapply(dados_sim_2$TIPOBITO == "Fetal", dados_sim_2$CODMUNRES, sum, na.rm = TRUE))


vars_pendentes <- c(
  "TORC", "TORCR", "TO_NN", "TO_N", "TO_CB_I", "TO_CB_N", "TO_CB_C", 
  "TO_CB_R", "TO_CB_O", "TO_F_IF", "TO_NT", "TO_NT_P", "TO_NT_T", 
  "TO_PNT", "TONT_B", "TONT_PT", "TONT_A", "TONT_PD", "TONT_I", 
  "TO_MT", "TO_MT_DG", "TO_MT_PT", "TO_MT_AB", "TO_MT_42", "TO_MT_43", 
  "TO_MT_P", "TO_MT_P_I", "TO_MT_P_ES", "TO_MT_P_EFI", "TO_MT_P_EFII", 
  "TO_MT_P_EM", "TO_MT_P_ESI", "TO_MT_P_ESC"
)

for(v in vars_pendentes) {
  SIM_PI[[v]] <- NA
}

ordem_final <- c(
  "ANO", "NIVEL", "CODMUNRES", "TO", "TORC", "TORCR", 
  "TO_NN", "TO_N", "TO_CB_I", "TO_CB_N", "TO_CB_C", 
  "TO_CB_R", "TO_CB_O", "TO_M", "TO_F", "TO_F_IF", 
  "TO_FT", "TO_NT", "TO_NT_P", "TO_NT_T", "TO_PNT", 
  "TONT_B", "TONT_PT", "TONT_A", "TONT_PD", "TONT_I", 
  "TO_MT", "TO_MT_DG", "TO_MT_PT", "TO_MT_AB", "TO_MT_42", "TO_MT_43", 
  "TO_MT_P", "TO_MT_P_I", "TO_MT_P_ES", "TO_MT_P_EFI", "TO_MT_P_EFII", 
  "TO_MT_P_EM", "TO_MT_P_ESI", "TO_MT_P_ESC"
)

SIM_PI <- SIM_PI[, ordem_final]

# Ao terminar a Tarefa 7 commit com a mensagem "script BDEM - SIM - tarefas 1 a 7" e envie para o repositório Projeto_BDEM_2016


# Tarefa 8. Exportar o banco de dados com o nome SIM_UF.csv (Exemplo: SIM_RJ.csv)

write.csv(SIM_PI, file = "SIM_PI.csv", row.names = FALSE)

# Ao terminar a Tarefa 8 fazer um commit com o comentário "dados SIM_UF 2016 e script - SIM - tarefas 1 a 8"  e envie para o repositório Projeto_BDEM_2016



####################################
# ETAPA 2: BANCO DE DADOS DO SINASC
####################################
# Você deve criar e estar na branch SINASC antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1. Leitura do banco de dados SINASC_2016 com 2857800 linhas e 61 colunas com o nome de dados_sinasc
# Verificar se a leitura foi feita corretamente e a estrutura dos dados
# Por uma questão de padronização coloque todos os nomes das variáveis em letra maiúscula,
# usando o comando names(dados_sinasc) = toupper(names(dados_sinasc))

dados_sinasc<-read.csv("SINASC_2016.csv", header = T, sep=";")
str(dados_sinasc)
summary(dados_sinasc)
View(dados_sinasc)

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SINASC - tarefa 1" e envie para o repositório Projeto_BDEM_2016

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# As colunas serão 3, 4, 5, 6, 11, 12, 13, 14, 18, 20, 21, 22, 23, 34, 37, 43, 47, 58, 59, 60, 61
# Nomes das respectivas variáveis: CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO, 
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK, CONTADOR

dados_sinasc_1<-dados_sinasc[, c(3,4,5,6,11,12,13,14,18,20,21,22,
                                 23,34,37,43,47,58,59,60,61
                                                )]
summary(dados_sinasc_1)


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 26602     12: 15773     13: 76703     14: 11376     15: 137681    16: 15521      17: 23870
# 21: 110493    22: 46986     23: 126246    24: 45366     25: 56083     26: 130733     27: 48164     28: 32218     29: 199830
# 31: 253520    32: 53413     33: 219129    35: 601437     
# 41: 155066    42: 95313     43: 141411
# 50: 42432     51: 53531     52: 95563     53: 43340 

# Foi percebido ao ler a base dados, que suas colunas, isto é, variáveis não estavam
# em caixa alta, logo: 

names(dados_sinasc_1)<-toupper(names(dados_sinasc_1))

summary(dados_sinasc_1$CODMUNRES)

UF<-substr(as.character(dados_sinasc_1$CODMUNRES),1,2)

dados_sinasc_2<-dados_sinasc_1[UF=="22",]

# Checando se o novo banco de dados está selecionando corretamente a UF
# da variável CODMUNRES.

str(dados_sinasc_2$CODMUNRES)

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK
# Avalie também os valores das variáveis quantitativas de IDADEMAE, SEMAGESTAC, APGAR5 e PESO


# Verificando as frequências das categorias das variáveis pedidas. 

table(dados_sinasc_2$LOCNASC)

table(dados_sinasc_2$ESTCIVMAE)
        
table(dados_sinasc_2$GESTACAO)
        
table(dados_sinasc_2$GRAVIDEZ)

table(dados_sinasc_2$PARTO)

table(dados_sinasc_2$SEXO)
                        
table(dados_sinasc_2$RACACOR)
                                
table(dados_sinasc_2$IDANOMAL)
                                        
table(dados_sinasc_2$ESCMAE2010)
                                                
table(dados_sinasc_2$RACACORMAE)
                                                        
table(dados_sinasc_2$TPAPRESENT)
                                                                
table(dados_sinasc_2$TPROBSON)
        
table(dados_sinasc_2$PARIDADE)
        
table(dados_sinasc_2$KOTELCHUCK)

# Avaliando os valores das variáveis quantitativas pedidas.

summary(dados_sinasc_2$IDADEMAE)

summary(dados_sinasc_2$SEMAGESTAC)

summary(dados_sinasc_2$APGAR5)

summary(dados_sinasc_2$PESO)

# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# Verifique o dicionário do SINASC para identificar qual o código das categorias de cada variável
# KOTELCHUCK = 9 significa "Não informado"   TPROBSON = 11 significa "Não classificado por falta de informação"
# Em variáveis quantitativas como IDADEMAE verificar se existem valores como 9999 para NA

# Atribuindo a cada variável NA para a categoria informada como:
# "Não informado ou Ignorado"

dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == "9"] <- NA

dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == "9"] <- NA

dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == "9"] <- NA

dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == "9"] <- NA

dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == "9"] <- NA

dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == "0"] <- NA

dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == "9"] <- NA

dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == "9"] <- NA

dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == "9"] <- NA

# Foi visto, ao pesquisar os significados categóricos de cada variável
# e foi percebido que, as variáveis: TPROBSON e KOTELCHUCK, apresentaram
# categorias da forma, "num" signica "Não informado ou Ignorado"

dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == "11"] <- NA

dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == "9"] <- NA

# As variáveis que não apresentaram categorias explicitando que se
# referem a: "Não informado ou Ignorado" no dicionário
# foram mantidas com seus códigos originais.

# Conferindo se não possui valores como "999" ou "9999" 
# para as variáveis: IDADEMAE, PESO. Respectivamente:

summary(dados_sinasc_2$IDADEMAE)

summary(dados_sinasc_2$PESO)

# Ao terminar a Tarefa 5 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 5" e envie para o repositório Projeto_BDEM_2016


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da legenda é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis dentro do banco de dados

# Atribuindo as legendas para as categorias das variáveis qualitativas.

dados_sinasc_2$LOCNASC <- factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4,5), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros", "Aldeia indígena"))

dados_sinasc_2$ESTCIVMAE <- factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))

dados_sinasc_2$GESTACAO <- factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais"))

dados_sinasc_2$GRAVIDEZ <- factor(dados_sinasc_2$GRAVIDEZ, levels = c(1,2,3), labels = c("Única", "Dupla", "Tripla ou mais"))

dados_sinasc_2$PARTO <- factor(dados_sinasc_2$PARTO, levels = c(1,2), labels = c("Vaginal", "Cesáreo"))

dados_sinasc_2$SEXO <- factor(dados_sinasc_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))

dados_sinasc_2$RACACOR <- factor(dados_sinasc_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

dados_sinasc_2$IDANOMAL <- factor(dados_sinasc_2$IDANOMAL, levels = c(1,2), labels = c("Sim", "Não"))

dados_sinasc_2$ESCMAE2010 <- factor(dados_sinasc_2$ESCMAE2010, levels = c(0,1,2,3,4,5), labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo"))

dados_sinasc_2$RACACORMAE <- factor(dados_sinasc_2$RACACORMAE, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

dados_sinasc_2$TPAPRESENT <- factor(dados_sinasc_2$TPAPRESENT, levels = c(1,2,3), labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))

dados_sinasc_2$PARIDADE <- factor(dados_sinasc_2$PARIDADE, levels = c(0,1), labels = c("Nulípara", "Multípara"))

dados_sinasc_2$KOTELCHUCK <- factor(dados_sinasc_2$KOTELCHUCK, levels = c(1, 2, 3, 4, 5), labels = c("Não fez pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))

dados_sinasc_2$TPROBSON <- factor(dados_sinasc_2$TPROBSON, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))

# Ao terminar a Tarefa 6 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 6" e envie para o repositório Projeto_BDEM_2016


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$PEREG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator

# 1. Nova variável: F_PESO

dados_sinasc_2$F_PESO <- NA

dados_sinasc_2$F_PESO[dados_sinasc_2$PESO < 2500] <- "Baixo peso"
dados_sinasc_2$F_PESO[dados_sinasc_2$PESO >= 2500 & dados_sinasc_2$PESO < 4000] <- "Peso normal"
dados_sinasc_2$F_PESO[dados_sinasc_2$PESO >= 4000] <- "Macrossomia"

dados_sinasc_2$F_PESO <- factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso", "Peso normal", "Macrossomia"))


# 2. Nova variável: F_IDADE

dados_sinasc_2$F_IDADE <- NA

dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE < 15] <- "<15"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 19] <- "15-19"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 20 & dados_sinasc_2$IDADEMAE <= 24] <- "20-24"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 25 & dados_sinasc_2$IDADEMAE <= 29] <- "25-29"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 30 & dados_sinasc_2$IDADEMAE <= 34] <- "30-34"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 35 & dados_sinasc_2$IDADEMAE <= 39] <- "35-39"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 40 & dados_sinasc_2$IDADEMAE <= 44] <- "40-44"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 45 & dados_sinasc_2$IDADEMAE <= 49] <- "45-49"
dados_sinasc_2$F_IDADE[dados_sinasc_2$IDADEMAE >= 50] <- "50+"

dados_sinasc_2$F_IDADE <- factor(dados_sinasc_2$F_IDADE, levels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"))


# 3. Nova variável: F_APGAR5

dados_sinasc_2$F_APGAR5 <- NA

dados_sinasc_2$F_APGAR5[dados_sinasc_2$APGAR5 < 7] <- "Baixo"
dados_sinasc_2$F_APGAR5[dados_sinasc_2$APGAR5 >= 7] <- "Normal"

dados_sinasc_2$F_APGAR5 <- factor(dados_sinasc_2$F_APGAR5, levels = c("Baixo", "Normal"))


# 4. Nova variável: PEREG (Peregrinação / Deslocamento materno)

dados_sinasc_2$PEREG <- NA

dados_sinasc_2$PEREG[as.character(dados_sinasc_2$CODMUNNASC) == as.character(dados_sinasc_2$CODMUNRES)] <- "Não"
dados_sinasc_2$PEREG[as.character(dados_sinasc_2$CODMUNNASC) != as.character(dados_sinasc_2$CODMUNRES)] <- "Sim"

dados_sinasc_2$PEREG <- factor(dados_sinasc_2$PEREG, levels = c("Sim", "Não"))


# 5. Nova variável: ESTCIV (Estado Civil agrupado)
# Nota: Como transformamos ESTCIVMAE em factor na Tarefa 6, 
# buscamos pelos rótulos textuais criados.

dados_sinasc_2$ESTCIV <- NA

dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada")] <- "Sem companheiro"
dados_sinasc_2$ESTCIV[dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável")] <- "Com companheiro"

dados_sinasc_2$ESTCIV <- factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro", "Com companheiro"))


# Ao terminar a Tarefa 7 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 7" e envie para o repositório Projeto_BDEM_2016


# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# Criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

Tabela_PIG_Brasil<-read.csv("Tabela_PIG_Brasil.csv",header = T, sep =";")
str(Tabela_PIG_Brasil)

Tabela_PIG_Brasil$SEXO<-tolower(Tabela_PIG_Brasil$SEXO)
Tabela_PIG_Brasil$SEXO<-trimws(Tabela_PIG_Brasil$SEXO)

Tabela_PIG_Brasil$SEXO[Tabela_PIG_Brasil$SEXO == "masculino"] <- "Masculino"
Tabela_PIG_Brasil$SEXO[Tabela_PIG_Brasil$SEXO == "feminino"] <- "Feminino"

Tabela_PIG_Brasil$SEXO<-factor(Tabela_PIG_Brasil$SEXO, levels = c("Masculino", "Feminino"))

dados_sinasc_2 <- merge(x = dados_sinasc_2, y = Tabela_PIG_Brasil, by.x = c("SEMAGESTAC", "SEXO"),
                        by.y = c("SEMAGESTAC", "SEXO"), all.x = TRUE)

dados_sinasc_2$F_PIG <- ifelse(
  dados_sinasc_2$GRAVIDEZ != "Única" | is.na(dados_sinasc_2$GRAVIDEZ), NA,
  
  ifelse(
    is.na(dados_sinasc_2$SEMAGESTAC) | is.na(dados_sinasc_2$PESO) | is.na(dados_sinasc_2$SEXO), NA,
    
    ifelse(
      dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
      
      ifelse(
        dados_sinasc_2$PESO > dados_sinasc_2$PESO_P90, "GIG",
        "AIG"
      )
    )
  )
)

dados_sinasc_2$F_PIG <- as.factor(dados_sinasc_2$F_PIG)

# Ao terminar a Tarefa 8 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 8" e envie para o repositório Projeto_BDEM_2016


# Tarefa 9. Criar um banco de dados, de nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 9 - SINASC.pdf”
# Atenção: a ordem das variáveis do arquivo deve ser respeitada

# Base inicial (municípios do PI)
base <- data.frame(CODMUNRES = sort(unique(dados_sinasc_2$CODMUNRES)))


# TN - total de nascimentos
tab <- table(factor(dados_sinasc_2$CODMUNRES, levels = base$CODMUNRES))
TN <- as.data.frame(tab)
names(TN) <- c("CODMUNRES", "TN")
base <- merge(base, TN, by = "CODMUNRES", all.x = TRUE)


# TNRC - total de nascimentos com registros completos nas 61 variáveis do SINASC
dados_UF <- dados_sinasc[substr(as.character(dados_sinasc$CODMUNRES), 1, 2) == "22", ]
dados_UF_comp <- dados_UF[complete.cases(dados_UF), ]
tab <- table(factor(dados_UF_comp$CODMUNRES, levels = base$CODMUNRES))
TNRC <- as.data.frame(tab)
names(TNRC) <- c("CODMUNRES", "TNRC")
base <- merge(base, TNRC, by = "CODMUNRES", all.x = TRUE)


# TNRCR - total de nascimentos com registros completos nas variáveis selecionadas
dados_UF_1 <- dados_sinasc_1[substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2) == "22", ]
dados_UF_1_comp <- dados_UF_1[complete.cases(dados_UF_1), ]
tab <- table(factor(dados_UF_1_comp$CODMUNRES, levels = base$CODMUNRES))
TNRCR <- as.data.frame(tab)
names(TNRCR) <- c("CODMUNRES", "TNRCR")
base <- merge(base, TNRCR, by = "CODMUNRES", all.x = TRUE)


# TGI_15 - total de gestantes com idade inferior a 15 anos
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE < 15], levels = base$CODMUNRES))
TGI_15 <- as.data.frame(tab)
names(TGI_15) <- c("CODMUNRES", "TGI_15")
base <- merge(base, TGI_15, by = "CODMUNRES", all.x = TRUE)


# TGI_15_19 - total de gestantes com idade >= 15 e <= 19 anos
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 19], levels = base$CODMUNRES))
TGI_15_19 <- as.data.frame(tab)
names(TGI_15_19) <- c("CODMUNRES", "TGI_15_19")
base <- merge(base, TGI_15_19, by = "CODMUNRES", all.x = TRUE)


# TGI_20_24 - total de gestantes com idade >= 20 e <= 24 anos
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE >= 20 & dados_sinasc_2$IDADEMAE <= 24], levels = base$CODMUNRES))
TGI_20_24 <- as.data.frame(tab)
names(TGI_20_24) <- c("CODMUNRES", "TGI_20_24")
base <- merge(base, TGI_20_24, by = "CODMUNRES", all.x = TRUE)


# TGI_25_29 - total de gestantes com idade >= 25 e <= 29 anos
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE >= 25 & dados_sinasc_2$IDADEMAE <= 29], levels = base$CODMUNRES))
TGI_25_29 <- as.data.frame(tab)
names(TGI_25_29) <- c("CODMUNRES", "TGI_25_29")
base <- merge(base, TGI_25_29, by = "CODMUNRES", all.x = TRUE)


# TGI_30_34 - total de gestantes com idade >= 30 e <= 34 anos
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE >= 30 & dados_sinasc_2$IDADEMAE <= 34], levels = base$CODMUNRES))
TGI_30_34 <- as.data.frame(tab)
names(TGI_30_34) <- c("CODMUNRES", "TGI_30_34")
base <- merge(base, TGI_30_34, by = "CODMUNRES", all.x = TRUE)


# TGI_35_39 - total de gestantes com idade >= 35 e <= 39 anos
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE >= 35 & dados_sinasc_2$IDADEMAE <= 39], levels = base$CODMUNRES))
TGI_35_39 <- as.data.frame(tab)
names(TGI_35_39) <- c("CODMUNRES", "TGI_35_39")
base <- merge(base, TGI_35_39, by = "CODMUNRES", all.x = TRUE)


# TGI_40_44 - total de gestantes com idade >= 40 e <= 44 anos
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE >= 40 & dados_sinasc_2$IDADEMAE <= 44], levels = base$CODMUNRES))
TGI_40_44 <- as.data.frame(tab)
names(TGI_40_44) <- c("CODMUNRES", "TGI_40_44")
base <- merge(base, TGI_40_44, by = "CODMUNRES", all.x = TRUE)


# TGI_45_49 - total de gestantes com idade >= 45 e <= 49 anos
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE >= 45 & dados_sinasc_2$IDADEMAE <= 49], levels = base$CODMUNRES))
TGI_45_49 <- as.data.frame(tab)
names(TGI_45_49) <- c("CODMUNRES", "TGI_45_49")
base <- merge(base, TGI_45_49, by = "CODMUNRES", all.x = TRUE)


# TGI_50 - total de gestantes com idade >= 50
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE >= 50], levels = base$CODMUNRES))
TGI_50 <- as.data.frame(tab)
names(TGI_50) <- c("CODMUNRES", "TGI_50")
base <- merge(base, TGI_50, by = "CODMUNRES", all.x = TRUE)


# TGIF - total de gestantes em idade fértil (idade >= 15 e <= 49)
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 49], levels = base$CODMUNRES))
TGIF <- as.data.frame(tab)
names(TGIF) <- c("CODMUNRES", "TGIF")
base <- merge(base, TGIF, by = "CODMUNRES", all.x = TRUE)


# IM_P25 - percentil 25 da idade materna
vec_P25 <- tapply(dados_sinasc_2$IDADEMAE, factor(dados_sinasc_2$CODMUNRES, levels = base$CODMUNRES), function(x) quantile(x, 0.25, na.rm = TRUE))
IM_P25 <- data.frame(CODMUNRES = base$CODMUNRES, IM_P25 = as.numeric(vec_P25))
base <- merge(base, IM_P25, by = "CODMUNRES", all.x = TRUE)


# IM_P50 - percentil 50 da idade materna
vec_P50 <- tapply(dados_sinasc_2$IDADEMAE, factor(dados_sinasc_2$CODMUNRES, levels = base$CODMUNRES), function(x) quantile(x, 0.50, na.rm = TRUE))
IM_P50 <- data.frame(CODMUNRES = base$CODMUNRES, IM_P50 = as.numeric(vec_P50))
base <- merge(base, IM_P50, by = "CODMUNRES", all.x = TRUE)


# IM_P75 - percentil 75 da idade materna
vec_P75 <- tapply(dados_sinasc_2$IDADEMAE, factor(dados_sinasc_2$CODMUNRES, levels = base$CODMUNRES), function(x) quantile(x, 0.75, na.rm = TRUE))
IM_P75 <- data.frame(CODMUNRES = base$CODMUNRES, IM_P75 = as.numeric(vec_P75))
base <- merge(base, IM_P75, by = "CODMUNRES", all.x = TRUE)


# IM_MD - idade média materna
vec_MD <- tapply(dados_sinasc_2$IDADEMAE, factor(dados_sinasc_2$CODMUNRES, levels = base$CODMUNRES), function(x) mean(x, na.rm = TRUE))
IM_MD <- data.frame(CODMUNRES = base$CODMUNRES, IM_MD = as.numeric(vec_MD))
base <- merge(base, IM_MD, by = "CODMUNRES", all.x = TRUE)


# IM_DP - desvio-padrão da idade materna
vec_DP <- tapply(dados_sinasc_2$IDADEMAE, factor(dados_sinasc_2$CODMUNRES, levels = base$CODMUNRES), function(x) sd(x, na.rm = TRUE))
IM_DP <- data.frame(CODMUNRES = base$CODMUNRES, IM_DP = as.numeric(vec_DP))
base <- merge(base, IM_DP, by = "CODMUNRES", all.x = TRUE)


# EM_S - total de gestantes sem escolaridade
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Sem escolaridade"], levels = base$CODMUNRES))
EM_S <- as.data.frame(tab)
names(EM_S) <- c("CODMUNRES", "EM_S")
base <- merge(base, EM_S, by = "CODMUNRES", all.x = TRUE)


# EM_FI - total de gestantes com escolaridade fundamental I
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Fundamental I (1ª a 4ª série)"], levels = base$CODMUNRES))
EM_FI <- as.data.frame(tab)
names(EM_FI) <- c("CODMUNRES", "EM_FI")
base <- merge(base, EM_FI, by = "CODMUNRES", all.x = TRUE)


# EM_FII - total de gestantes com escolaridade fundamental II
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Fundamental II (5ª a 8ª série)"], levels = base$CODMUNRES))
EM_FII <- as.data.frame(tab)
names(EM_FII) <- c("CODMUNRES", "EM_FII")
base <- merge(base, EM_FII, by = "CODMUNRES", all.x = TRUE)


# EM_M - total de gestantes com escolaridade médio
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Médio (antigo 2º grau)"], levels = base$CODMUNRES))
EM_M <- as.data.frame(tab)
names(EM_M) <- c("CODMUNRES", "EM_M")
base <- merge(base, EM_M, by = "CODMUNRES", all.x = TRUE)


# EM_SI - total de gestantes com escolaridade superior incompleto
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Superior incompleto"], levels = base$CODMUNRES))
EM_SI <- as.data.frame(tab)
names(EM_SI) <- c("CODMUNRES", "EM_SI")
base <- merge(base, EM_SI, by = "CODMUNRES", all.x = TRUE)


# EM_SC - total de gestantes com escolaridade superior completo
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESCMAE2010 == "Superior completo"], levels = base$CODMUNRES))
EM_SC <- as.data.frame(tab)
names(EM_SC) <- c("CODMUNRES", "EM_SC")
base <- merge(base, EM_SC, by = "CODMUNRES", all.x = TRUE)


# TGRC_B - total de gestantes da raça/cor branca
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACORMAE == "Branca"], levels = base$CODMUNRES))
TGRC_B <- as.data.frame(tab)
names(TGRC_B) <- c("CODMUNRES", "TGRC_B")
base <- merge(base, TGRC_B, by = "CODMUNRES", all.x = TRUE)


# TGRC_PT - total de gestantes da raça/cor preta
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACORMAE == "Preta"], levels = base$CODMUNRES))
TGRC_PT <- as.data.frame(tab)
names(TGRC_PT) <- c("CODMUNRES", "TGRC_PT")
base <- merge(base, TGRC_PT, by = "CODMUNRES", all.x = TRUE)


# TGRC_A - total de gestantes da raça/cor amarela
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACORMAE == "Amarela"], levels = base$CODMUNRES))
TGRC_A <- as.data.frame(tab)
names(TGRC_A) <- c("CODMUNRES", "TGRC_A")
base <- merge(base, TGRC_A, by = "CODMUNRES", all.x = TRUE)


# TGRC_PD - total de gestantes da raça/cor parda
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACORMAE == "Parda"], levels = base$CODMUNRES))
TGRC_PD <- as.data.frame(tab)
names(TGRC_PD) <- c("CODMUNRES", "TGRC_PD")
base <- merge(base, TGRC_PD, by = "CODMUNRES", all.x = TRUE)


# TGRC_I - total de gestantes da raça/cor indígena
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACORMAE == "Indígena"], levels = base$CODMUNRES))
TGRC_I <- as.data.frame(tab)
names(TGRC_I) <- c("CODMUNRES", "TGRC_I")
base <- merge(base, TGRC_I, by = "CODMUNRES", all.x = TRUE)


# TGSC - total de gestantes sem companheiro
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESTCIV == "Sem companheiro"], levels = base$CODMUNRES))
TGSC <- as.data.frame(tab)
names(TGSC) <- c("CODMUNRES", "TGSC")
base <- merge(base, TGSC, by = "CODMUNRES", all.x = TRUE)


# TGCC - total de gestantes com companheiro
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESTCIV == "Com companheiro"], levels = base$CODMUNRES))
TGCC <- as.data.frame(tab)
names(TGCC) <- c("CODMUNRES", "TGCC")
base <- merge(base, TGCC, by = "CODMUNRES", all.x = TRUE)


# TGPRI - total de gestantes primíparas
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PARIDADE == "Nulípara"], levels = base$CODMUNRES))
TGPRI <- as.data.frame(tab)
names(TGPRI) <- c("CODMUNRES", "TGPRI")
base <- merge(base, TGPRI, by = "CODMUNRES", all.x = TRUE)


# TGNPRI - total de gestantes não primíparas
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PARIDADE == "Multípara"], levels = base$CODMUNRES))
TGNPRI <- as.data.frame(tab)
names(TGNPRI) <- c("CODMUNRES", "TGNPRI")
base <- merge(base, TGNPRI, by = "CODMUNRES", all.x = TRUE)


# TGU - total de gestações únicas
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$GRAVIDEZ == "Única"], levels = base$CODMUNRES))
TGU <- as.data.frame(tab)
names(TGU) <- c("CODMUNRES", "TGU")
base <- merge(base, TGU, by = "CODMUNRES", all.x = TRUE)


# TGG - total de gestações gemelares
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$GRAVIDEZ %in% c("Dupla", "Tripla ou mais")], levels = base$CODMUNRES))
TGG <- as.data.frame(tab)
names(TGG) <- c("CODMUNRES", "TGG")
base <- merge(base, TGG, by = "CODMUNRES", all.x = TRUE)


# TGD_22 - total de gestações com duração inferior a 22 semanas
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$GESTACAO == "Menos de 22 semanas"], levels = base$CODMUNRES))
TGD_22 <- as.data.frame(tab)
names(TGD_22) <- c("CODMUNRES", "TGD_22")
base <- merge(base, TGD_22, by = "CODMUNRES", all.x = TRUE)


# TGD_22_27 - total de gestações com duração >=22 e <=27
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$GESTACAO == "22 a 27 semanas"], levels = base$CODMUNRES))
TGD_22_27 <- as.data.frame(tab)
names(TGD_22_27) <- c("CODMUNRES", "TGD_22_27")
base <- merge(base, TGD_22_27, by = "CODMUNRES", all.x = TRUE)


# TGD_28_31 - total de gestações com duração >=28 e <=31
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$GESTACAO == "28 a 31 semanas"], levels = base$CODMUNRES))
TGD_28_31 <- as.data.frame(tab)
names(TGD_28_31) <- c("CODMUNRES", "TGD_28_31")
base <- merge(base, TGD_28_31, by = "CODMUNRES", all.x = TRUE)


# TGD_32_36 - total de gestações com duração >=32 e <=36
tab <- table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$GESTACAO == "32 a 36 semanas"], levels = base$CODMUNRES))
TGD_32_36 <- as.data.frame(tab)
names(TGD_32_36) <- c("CODMUNRES", "TGD_32_36")
base <- merge(base, TGD_32_36, by = "CODMUNRES", all.x = TRUE)


# Criando a linha da UF (Piauí)
linha_estado <- data.frame(matrix(ncol = ncol(base), nrow = 1))
names(linha_estado) <- names(base)

# Somando as colunas de contagem 
linha_estado[, -1] <- colSums(base[, -1], na.rm = TRUE)

# Substituindo as colunas de estatísticas (Percentis, Média, DP) que não podem ser apenas somadas
linha_estado$IM_P25 <- quantile(dados_sinasc_2$IDADEMAE, 0.25, na.rm = TRUE)
linha_estado$IM_P50 <- quantile(dados_sinasc_2$IDADEMAE, 0.50, na.rm = TRUE)
linha_estado$IM_P75 <- quantile(dados_sinasc_2$IDADEMAE, 0.75, na.rm = TRUE)
linha_estado$IM_MD  <- mean(dados_sinasc_2$IDADEMAE, na.rm = TRUE)
linha_estado$IM_DP  <- sd(dados_sinasc_2$IDADEMAE, na.rm = TRUE)

linha_estado$CODMUNRES <- "22"


# Juntando a UF com os Municípios
SINASC_UF <- rbind(linha_estado, base)


# Adicionando ANO e NIVEL
SINASC_UF$NIVEL <- c("UF", rep("MUNICIPIO", nrow(SINASC_UF) - 1))
SINASC_UF$ANO <- 2016


# Reordenando conforme o PDF exato
ordem_final <- c(
  "ANO", "NIVEL", "CODMUNRES", "TN", "TNRC", "TNRCR",
  "TGI_15", "TGI_15_19", "TGI_20_24", "TGI_25_29", "TGI_30_34",
  "TGI_35_39", "TGI_40_44", "TGI_45_49", "TGI_50", "TGIF",
  "IM_P25", "IM_P50", "IM_P75", "IM_MD", "IM_DP",
  "EM_S", "EM_FI", "EM_FII", "EM_M", "EM_SI", "EM_SC",
  "TGRC_B", "TGRC_PT", "TGRC_A", "TGRC_PD", "TGRC_I",
  "TGSC", "TGCC", "TGPRI", "TGNPRI",
  "TGU", "TGG", "TGD_22", "TGD_22_27", "TGD_28_31", "TGD_32_36"
)

SINASC_UF <- SINASC_UF[, ordem_final]

# Ao terminar a Tarefa 9 commit com a mensagem "script BDEM - SINASC - tarefas 1 a 9" e envie para o repositório Projeto_BDEM_2016


# Tarefa 10. Exportar o banco de dados com o nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv)

write.csv(SINASC_UF, file = "SINASC_PI.csv", row.names = FALSE)

# Ao terminar a Tarefa 10 commit com o comentário "dados SINASC_UF 2016 e script - SIM - tarefas 1 a 10"  e envie para o repositório Projeto_BDEM_2016



####################################
# ETAPA 3: BANCOS DE DADOS DO SIDRA
####################################
# Você deve criar e estar na branch SIDRA antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler os bancos de dados abaixo listados com os respectivos nomes
# dados_sidra_1 para população residente estimada - UF e municípios - 2016 - SIDRA - tabela_6579.csv
# dados_sidra_2 para população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv
# dados_sidra_3 para população residente censo 2010 - por faixa etária - UF - SIDRA - tabela_1552.csv
# dados_sidra_4 para população residente censo 2010 - por faixa etária e sexo - municípios - SIDRA - tabela_1552.csv
# Atenção que agora os arquivos têm nomes e códigos (com 7 dígitos) dos municípios (e alguns UF)

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados





# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SIDRA - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Criar uma nova variável de nome CODUF com os códigos da UF nos bancos dados_sidra_1, dados_sidra_2, dados_sidra_4


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Selecionar em dados_sidra_ 1 a dados_sidra_4 a UF de responsabilidade do aluno 
# e chamar os bancos de dados, respectivamente por sidra_1, sidra_2, sidra_3 e sidra_4


# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4: Criar um banco de dados, de nome SIDRA_UF.csv (Exemplo: SIDRA_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 4 - SIDRA.pdf”

# Ao terminar a Tarefa 4 commit com a mensagem "script BDEM - SIDRA - tarefas 1 a 4" e envie para o repositório Projeto_BDEM_2016


# Tarefa 5:Exportar o banco de dados com o nome SIDRA_UF.csv (Exemplo: SIDRA_RJ.csv)
# Ao terminar a Tarefa 5 commit com o comentário "dados SIDRA_UF 2016 e script - SIDRA - tarefas 1 a 5"  e envie para o repositório Projeto_BDEM_2016


####################################
# ETAPA 4: BANCOS DE DADOS DO ATLAS
####################################
# Você deve criar e estar na branch ATLAS antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler os bancos de dados abaixo listados com os respectivos nomes
# codigos_IBGE_2010 para códigos dos municípios - 2010.csv
# dados_atlas_1 para IDHM - 2010 (CENSO) e 2016 (PNAD) - total e por sexo - UF - Atlas Brasil.csv
# dados_atlas_2 para IDHM - 2010 - municípios - Atlas Brasil.csv
# Atenção que agora alguns arquivos só têm os nomes dos municípios e das UFs, mas não têm os códigos

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - ATLAS - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2: Manipular o banco de dados e criar o banco de dados ATLAS_UF

# Criar o banco UF_codigo tipo tabela de correspondência
UF_codigo = data.frame(
  UF = c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins",
         "Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba",
         "Pernambuco","Alagoas","Sergipe","Bahia","Minas Gerais",
         "Espírito Santo","Rio de Janeiro","São Paulo","Paraná",
         "Santa Catarina","Rio Grande do Sul","Mato Grosso do Sul",
         "Mato Grosso","Goiás","Distrito Federal"),
  
  SIGLA = c("RO","AC","AM","RR","PA","AP","TO",
            "MA","PI","CE","RN","PB","PE","AL",
            "SE","BA","MG","ES","RJ","SP",
            "PR","SC","RS","MS","MT","GO","DF"),
  
  CODUF = c(11,12,13,14,15,16,17,
            21,22,23,24,25,26,27,
            28,29,31,32,33,35,
            41,42,43,50,51,52,53)
)

# Retirar de dados_atlas_1 a linha do Brasil e adicionar (com merge by UF) as colunas de UF_codigo

# Criar o banco linha_estado somente com as linhas da UF e com as seguintes colunas:
# ANO=2016, NIVEL=UF, CODMUNRES, IDHM_A, IDHM_CA, IDHM_CA_M e IDHM_CA_F 

# Selecionar de linha_estado a UF da responsabilidade do aluno por CODMUNRES

# Criar em dados_atlas_2 a coluna com UF

# Retirar (UF) da variável município

# Acrescentar em codigos_IBGE_2010 a variável CODUF baseado nos dois primeiros dígitos de CODMUNRES

# Acrescentar a codigos_IBGE_2010 as variáveis de UF_codigo (merge by CODUF)

# Associar dados_atlas_2 a codigos_IBGE_2010 e nomear o novo arquivo por atlas_municipio
# Neste caso o merge será by.x = c("município","UF") e by.y = c("município","SIGLA")

# Remover de atlas_municipio a coluna UF.y criada no merge

# Selecionar somente a UF de responsabilidade do aluno através dos dois primeiros dógitos de CODMUNRES

# Criar banco ATLAS_MUNICIPIO com as linhas dos municípios e com as seguintes variáveis:
# ANO=2016, NIVEL=MUNICIPIO, CODMUNRES, IDHM_A=NA, IDHM_CA, IDHM_CA_M=NA, IDHM_CA_F=NA

# Criar banco final ATLAS_UF "juntando" os bancos linha_estado e ATLAS_MUNICIPIO


# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - ATLAS - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Exportar o banco de dados com o nome ATLAS_UF.csv (Exemplo: ATLAS_RJ.csv)
# Ao terminar a Tarefa 3 commit com o comentário "dados ATLAS_UF 2016 e script - ATLAS - tarefas 1 a 3"  e envie para o repositório Projeto_BDEM_2016



####################################
# ETAPA 5: BANCOS DE DADOS DO SINISA
####################################
# Você deve criar e estar na branch SINISA antes de inserir os comandos 
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Ler o bancos de dados abaixo listado com os respectivo nome
# dados_sinisa para agua e esgoto - município - 2016.csv
# Atenção que o arquivo tem códigos e nomes de municípios e muitos NAs. 
# Repare que os valores estão com o milhar indicado por ponto, o que não deve acontecer para o R não entender como decimal

# Verificar se a leitura de todos os bancos foi feita corretamente e a estrutura dos dados
# Remover a pontuação de milhar e converter para formato numérico

# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - SINISA - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2. Reduzir dados_sinisa apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinisa_1

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - SINISA - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3. Criar um banco de dados, de nome SINISA_UF.csv (Exemplo: SINISA_RJ.csv), contendo as variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 3 - SINISA.pdf”

# Ao terminar a Tarefa 3 commit com a mensagem "script BDEM - SINISA - tarefas 1 a 3" e envie para o repositório Projeto_BDEM_2016


# Tarefa 4. Exportar o banco de dados com o nome SINISA_UF.csv (Exemplo: SINISA_RJ.csv)
# Ao terminar a Tarefa 4 commit com o comentário "dados SINISA_UF 2016 e script - SINISA - tarefas 1 a 4"  e enviar para o repositório Projeto_BDEM_2016



################################
# ETAPA 6: CRIAÇÃO DE BDEM_UF
################################
# Você deve estar agora em main e antes de inserir qualquer comando desta ETAPA
# deverá fazer os merges de cada uma das 5 branches. A cada merge pode fazer o comentário "merge da branch TAL"
# NÃO altere as linhas de qualquer outra ETAPA do script e nem do cabeçalho

# Tarefa 1: Agregar os arquivos SIDRA_UF, ATLAS_UF, SINASC_UF, SIM_UF, SINISA_UF no banco BDEM_UF (Exemplo: BDEM_RJ)
# Leitura dos 5 bancos de dados expeortados das etapas anteriores

# Agregação dos bancos
# Lembre-se que SIDRA e ATLAS tem CODMUNRES com 7 dígitos e SINASC, SIM e SINISA com 6 dígitos
# Além disso dentro do merge all = TRUE garante a manutenção de qualquer município presente em um dos bancos envolvidos no merge


# Ao terminar a Tarefa 1 commit com a mensagem "script BDEM - BDEM - tarefa 1" e envie para o repositório Projeto_BDEM_2016


# Tarefa 2: Inserir os seguintes indicadores epidemiológicos (com apenas dias casas decimais) no BDEM_UF:
# TFG: Taxa de fecundidade geral
# TMG: Taxa de mortalidade geral
# RMM: Razão de mortalidade materna
# TMM: Taxa de mortalidade materna
# TMM_P: Taxa de mortalidade materna em até 42 dias
# TMN: Taxa de mortalidade neonatal
# TMN_P: Taxa de mortalidade neonatal precoce
# TMN_T: Taxa de mortalidade neonatal tardia
# TMI: Taxa de mortalidade infantil

# Conferir o banco BDEM_UF após inserção dos indicadores

# Ao terminar a Tarefa 2 commit com a mensagem "script BDEM - BDEM - tarefas 1 a 2" e envie para o repositório Projeto_BDEM_2016


# Tarefa 3: Exportar o banco de dados com o nome BDEM_UF.csv (Exemplo: BDEM_RJ.csv)
# Ao terminar a Tarefa 3 commit com o comentário "dados BDEM_UF 2016 e script - BDEM - tarefas 1 a 3"  e enviar para o repositório Projeto_BDEM_2016
 