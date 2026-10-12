options(repos = c(CRAN = "http://pkg.fecap.br/repository/r/"))
pacotes <- c("tidyverse", "scales")
novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos)) install.packages(novos)
library(tidyverse)

# Todos os números vêm de dados/dados_extraidos_pdfs.csv (extraídos dos artigos, com a página de origem)
arq <- "dados/dados_extraidos_pdfs.csv"
if (!file.exists(arq)) arq <- file.choose()
d <- read_csv(arq, show_col_types = FALSE)
# para a apresentação: Rscript codigo/graficos_dados_reais.R escuro
escuro <- "escuro" %in% commandArgs(trailingOnly = TRUE)
cores <- if (escuro) list(fundo = "#172231", fg = "#eef2f6", mudo = "#9db0c3", grade = "#2a3a4d") else
  list(fundo = "white", fg = "black", mudo = "grey35", grade = "grey92")
pasta <- if (escuro) "graficos_escuro" else "graficos"
dir.create(pasta, showWarnings = FALSE)

pal <- if (escuro) c(azul = "#5aa9e6", laranja = "#f2b45a", verde = "#6cc3a0", roxo = "#b58bd6", cinza = "#8a97a6", vermelho = "#e07a6a") else
  c(azul = "#3b6ea5", laranja = "#d98b2b", verde = "#4a9a6a", roxo = "#a0527c", cinza = "#7a7a7a", vermelho = "#b5503c")
fmt <- function(x) paste0(vapply(x, \(v) format(v, digits = 4, decimal.mark = ",", trim = TRUE), ""), "%")
cap <- function(...) str_wrap(paste0(...), 92)
tema <- theme_minimal(base_size = if (escuro) 15 else 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = cores$grade), legend.position = "bottom",
        plot.background = element_rect(fill = cores$fundo, colour = NA), panel.background = element_rect(fill = cores$fundo, colour = NA),
        text = element_text(colour = cores$fg), axis.text = element_text(colour = cores$mudo), strip.text = element_text(colour = cores$fg),
        plot.title = element_text(face = "bold"), plot.caption = element_text(colour = cores$mudo, hjust = 0))
if (escuro) tema <- tema + theme(plot.title = element_blank(), plot.caption = element_blank())
salva <- function(g, nome, w = 8, h = 5) ggsave(file.path(pasta, paste0(nome, ".png")), g, width = w, height = h, dpi = 200, bg = cores$fundo)
f <- function(i) filter(d, indicador == i)
SILVA <- "Silva, Vilela e Oliveira (2024)"

# 1. envolvimento por rede (PeNSE 2015)
g01 <- f("envolvimento_pense2015") %>%
  mutate(categoria = factor(categoria, c("Vítima", "Agressor", "Vítima-agressor")), grupo = factor(grupo, c("Pública", "Privada"))) %>%
  ggplot(aes(categoria, valor, fill = grupo)) +
  geom_col(position = position_dodge(.75), width = .7) +
  geom_text(colour = cores$fg, aes(label = fmt(valor)), position = position_dodge(.75), vjust = -.4, size = 4) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, .12))) +
  scale_fill_manual(values = unname(pal[c("azul", "laranja")])) +
  labs(x = NULL, y = "% dos estudantes", fill = NULL, title = "Envolvimento com bullying por rede de ensino",
       caption = cap("Fonte: ", SILVA, ", p. 11 (PeNSE 2015, 9º ano; 77.488 estudantes da rede pública e 20.358 da privada).")) + tema
salva(g01, "g01_envolvimento_rede", 8, 5)

# 2. vitimização na PeNSE ao longo do tempo
g02 <- f("vitimizacao_pense_serie") %>%
  mutate(nova_escala = categoria == "2019", ano = paste0(categoria, if_else(nova_escala, "*", ""))) %>%
  ggplot(aes(ano, valor, fill = nova_escala)) +
  geom_col(width = .65, show.legend = FALSE) +
  geom_text(colour = cores$fg, aes(label = fmt(valor)), vjust = -.4, size = 4.2) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, .12))) +
  scale_fill_manual(values = unname(pal[c("azul", "laranja")])) +
  labs(x = NULL, y = "% de vítimas", title = "Vitimização por bullying na PeNSE",
       caption = cap("Fonte: ", SILVA, ", p. 6. *Em 2019 a escala de resposta mudou; comparar com cautela.")) + tema
salva(g02, "g02_pense_serie", 6.5, 4.8)

# 3. vitimização em estudos brasileiros
geral <- bind_rows(
  f("estudos_vitimas") %>% transmute(local = case_when(str_detect(grupo, "Minas") ~ "Minas Gerais (2012)", str_detect(grupo, "Campina") ~ "Campina Grande (PB)",
                                                       str_detect(grupo, "São Paulo") ~ "Município de São Paulo (2017)", TRUE ~ "Brasil, PeNSE 2019"),
                                     rede = case_when(str_detect(grupo, "Minas|Campina") ~ "Pública", TRUE ~ "Rede não separada"), valor),
  f("vitimas_verbais_rede") %>% transmute(local = "Recife (PE), violência verbal", rede = grupo, valor),
  f("cidades_vitimas") %>% separate(grupo, c("local", "rede"), " \\| ") %>% select(local, rede, valor),
  f("envolvimento_pense2015") %>% filter(categoria == "Vítima") %>% transmute(local = "Brasil, PeNSE 2015 (9º ano)", rede = grupo, valor))
g03 <- geral %>%
  mutate(rede = factor(rede, c("Pública", "Privada", "Rede não separada")), local = fct_reorder(local, valor, .fun = max)) %>%
  ggplot(aes(valor, local, fill = rede)) +
  geom_col(position = position_dodge2(.8, preserve = "single"), width = .75) +
  geom_text(colour = cores$fg, aes(label = fmt(valor)), position = position_dodge2(.8, preserve = "single"), hjust = -.15, size = 3.8) +
  scale_x_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, .12))) +
  scale_fill_manual(values = unname(pal[c("azul", "laranja", "cinza")]), drop = FALSE) +
  labs(x = "% de estudantes vítimas", y = NULL, fill = NULL, title = "Vitimização por bullying em estudos brasileiros",
       caption = cap("Fontes: Silva, Vilela e Oliveira (2024), p. 6 e 11, e Moreno et al. (2012), p. 810. Estudos citados nos artigos. Definições, idades e anos diferem entre os estudos; não são diretamente comparáveis.")) + tema
salva(g03, "g03_estudos_brasileiros", 9, 5.8)

# 4. gênero
g04 <- f("vitimizacao_pense2019_sexo") %>%
  ggplot(aes(grupo, valor, fill = grupo)) +
  geom_col(width = .55, show.legend = FALSE) +
  geom_text(colour = cores$fg, aes(label = fmt(valor)), vjust = -.4, size = 4.5) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, .12))) +
  scale_fill_manual(values = unname(pal[c("roxo", "azul")])) +
  labs(x = NULL, y = "% de vítimas", title = "Vitimização por sexo, PeNSE 2019",
       caption = cap("Fonte: ", SILVA, ", p. 8 (IBGE, 2021).")) + tema
salva(g04, "g04_sexo_pense2019", 5.5, 4.8)

g05 <- f("sexo_masculino_aumento_prob") %>%
  mutate(categoria = factor(categoria, c("Sofrer bullying", "Praticar bullying", "Ser vítima-agressor")), grupo = factor(grupo, c("Pública", "Privada"))) %>%
  ggplot(aes(categoria, valor, fill = grupo)) +
  geom_col(position = position_dodge(.75), width = .7) +
  geom_text(colour = cores$fg, aes(label = paste0("+", valor, "%")), position = position_dodge(.75), vjust = -.4, size = 4) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, .12))) +
  scale_fill_manual(values = unname(pal[c("azul", "laranja")])) +
  labs(x = NULL, y = "Aumento da probabilidade", fill = NULL, title = "Meninos em relação a meninas",
       caption = cap("Fonte: ", SILVA, ", p. 11. Dois alunos iguais nas demais características (PeNSE 2015).")) + tema
salva(g05, "g05_sexo_probabilidade", 7.5, 4.8)

# 6. razões de chance da Tabela 1
g06 <- f("razao_chance_tabela1") %>%
  separate(categoria, c("fator", "desfecho"), "\\|") %>%
  mutate(significativo = !str_detect(observacao, "não significativa"),
         fator = factor(fator, rev(unique(fator))), grupo = factor(grupo, c("Pública", "Privada")),
         desfecho = factor(desfecho, c("Vítima", "Agressor"), labels = c("Ser vítima", "Ser agressor"))) %>%
  ggplot(aes(valor, fator, colour = grupo, shape = significativo)) +
  geom_vline(xintercept = 1, linetype = 2, colour = "grey50") +
  geom_point(position = position_dodge(.6), size = 3.2, stroke = 1.1) +
  facet_wrap(~desfecho) +
  scale_x_log10(breaks = c(.6, 1, 2, 3)) +
  scale_colour_manual(values = unname(pal[c("azul", "laranja")])) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), labels = c(`TRUE` = "significativa (p < 0,05)", `FALSE` = "não significativa")) +
  labs(x = "Razão de chance (escala logarítmica)", y = NULL, colour = NULL, shape = NULL,
       title = "Fatores associados a ser vítima e a ser agressor",
       caption = cap("Fonte: ", SILVA, ", Tabela 1, p. 12 (PeNSE 2015). O artigo não informa intervalos de confiança.")) + guides(shape = guide_legend(override.aes = list(colour = cores$fg))) + tema
salva(g06, "g06_razoes_de_chance", 10, 5.5)

# 7. Recife: escolaridade do chefe da família e perfil das vítimas
g07 <- f("escolaridade_chefe_familia") %>%
  mutate(categoria = factor(categoria, c("Analfabeto ou até 3ª série", "4ª série", "Fundamental completo", "Médio completo", "Superior completo")),
         grupo = factor(grupo, c("Pública", "Privada"))) %>%
  ggplot(aes(categoria, valor, fill = grupo)) +
  geom_col(position = position_dodge(.75), width = .7) +
  geom_text(colour = cores$fg, aes(label = fmt(valor)), position = position_dodge(.75), vjust = -.4, size = 3.8) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, .12))) +
  scale_x_discrete(labels = \(x) str_wrap(x, 14)) +
  scale_fill_manual(values = unname(pal[c("azul", "laranja")])) +
  labs(x = NULL, y = "% das vítimas", fill = NULL, title = "Escolaridade do chefe da família das vítimas, Recife",
       caption = cap("Fonte: Moreno et al. (2012), p. 810 (vítimas de violência verbal, 10 a 14 anos).")) + tema
salva(g07, "g07_recife_escolaridade", 8.5, 5)

g08 <- f("perfil_vitimas") %>%
  mutate(categoria = fct_reorder(categoria, valor)) %>%
  ggplot(aes(valor, categoria)) +
  geom_col(fill = pal["azul"], width = .7) +
  geom_text(colour = cores$fg, aes(label = fmt(valor)), hjust = -.15, size = 4) +
  scale_x_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, .12))) +
  labs(x = "% das vítimas", y = NULL, title = "Perfil das vítimas, Recife (n = 558)",
       caption = cap("Fonte: Moreno et al. (2012), p. 811.")) + tema
salva(g08, "g08_recife_perfil", 8.5, 4.8)

# 9. Portugal: tipos de bullying
tipos <- c("Chamar nomes", "Deixar de fora", "Bater", "Boatos", "Cor da pele", "Religião", "Piadas sexuais")
g09 <- f("media_bullying") %>%
  mutate(categoria = factor(categoria, rev(tipos))) %>%
  ggplot(aes(valor, categoria, colour = grupo)) +
  geom_point(position = position_dodge(.5), size = 3.4) +
  scale_x_continuous(limits = c(1, 1.8)) +
  scale_colour_manual(values = unname(pal[c("laranja", "azul")])) +
  labs(x = "Frequência média (1 = nunca; 5 = muitas vezes por semana)", y = NULL, colour = NULL,
       title = "Tipos de bullying sofridos e praticados, Portugal",
       caption = cap("Fonte: Matos e Gonçalves (2009), Quadro 1 (HBSC 2002; 6.131 estudantes).")) + tema
salva(g09, "g09_portugal_tipos", 8.5, 5)

g10 <- f("correlacao") %>%
  separate(categoria, c("variavel", "tipo"), "\\|") %>%
  mutate(tipo = factor(tipo, rev(tipos)), variavel = factor(variavel, c("Percepção de insegurança na escola", "Porte de armas", "Satisfação com a vida"))) %>%
  ggplot(aes(valor, tipo, colour = grupo)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(position = position_dodge(.5), size = 3) +
  facet_wrap(~variavel, labeller = label_wrap_gen(22)) +
  scale_colour_manual(values = unname(pal[c("laranja", "azul")])) +
  labs(x = "Correlação (r de Pearson)", y = NULL, colour = NULL, title = "Bullying, insegurança, armas e satisfação com a vida, Portugal",
       caption = cap("Fonte: Matos e Gonçalves (2009), Quadro 2. O primeiro bloco do quadro aparece rotulado 'Provocador' no artigo; pelos itens, refere-se ao bullying sofrido.")) + tema
salva(g10, "g10_portugal_correlacoes", 10, 5)

# 11. Porto Alegre
g11 <- f("convivencia_escola") %>%
  mutate(categoria = factor(categoria, c("Ótima ou boa", "Regular", "Ruim", "Péssima"))) %>%
  ggplot(aes(categoria, valor, fill = categoria)) +
  geom_col(width = .65, show.legend = FALSE) +
  geom_text(colour = cores$fg, aes(label = paste0(fmt(valor), " (", n, ")")), vjust = -.4, size = 4) +
  scale_y_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, .12))) +
  scale_fill_manual(values = unname(pal[c("verde", "laranja", "vermelho", "vermelho")])) +
  labs(x = NULL, y = "% dos 192 estudantes", title = "Como os estudantes avaliam a convivência na escola",
       caption = cap("Fonte: Grossi e Santos (2009), p. 250-254 (4 escolas públicas de Porto Alegre, 2007). Entre parênteses: número de estudantes. O artigo informa as contagens; os percentuais de regular, ruim e péssima foram calculados sobre 192.")) + tema
salva(g11, "g11_poa_convivencia", 7.5, 4.8)

g12 <- bind_rows(f("sentimentos_vitimas"), f("sentimentos_testemunhas")) %>%
  mutate(grupo = factor(grupo, c("Vítimas", "Colegas que presenciam")), categoria = str_wrap(categoria, 26)) %>%
  ggplot(aes(valor, reorder(categoria, valor))) +
  geom_col(aes(fill = grupo), width = .7, show.legend = FALSE) +
  geom_text(colour = cores$fg, aes(label = fmt(valor)), hjust = -.15, size = 4) +
  facet_wrap(~grupo, scales = "free_y", ncol = 1) +
  scale_x_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, .15))) +
  scale_fill_manual(values = unname(pal[c("roxo", "cinza")])) +
  labs(x = NULL, y = NULL, title = "O que sentem vítimas e testemunhas",
       caption = cap("Fonte: Grossi e Santos (2009), p. 250-254. Respostas múltiplas.")) + tema
salva(g12, "g12_poa_sentimentos", 8, 6)

g13 <- f("autor_bullying") %>%
  mutate(categoria = fct_reorder(categoria, valor)) %>%
  ggplot(aes(valor, categoria)) +
  geom_col(fill = pal["laranja"], width = .6) +
  geom_text(colour = cores$fg, aes(label = fmt(valor)), hjust = -.15, size = 4.2) +
  scale_x_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, .15))) +
  labs(x = NULL, y = NULL, title = "Quem pratica o bullying, segundo os estudantes",
       caption = cap("Fonte: Grossi e Santos (2009), p. 250-254. Respostas não excludentes.")) + tema
salva(g13, "g13_poa_autores", 7.5, 4)

g14 <- f("denuncia_escola") %>%
  mutate(rotulo = case_when(str_detect(observacao, "^mais de") ~ paste0("mais de ", fmt(valor)), str_detect(observacao, "^cerca") ~ paste0("cerca de ", fmt(valor)),
                            str_detect(observacao, "^apenas") ~ paste0("apenas ", fmt(valor)), TRUE ~ fmt(valor)),
         categoria = fct_reorder(str_wrap(categoria, 30), valor)) %>%
  ggplot(aes(valor, categoria)) +
  geom_col(fill = pal["azul"], width = .65) +
  geom_text(colour = cores$fg, aes(label = rotulo), hjust = -.1, size = 4) +
  scale_x_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, .3))) +
  labs(x = NULL, y = NULL, title = "Silêncio, conhecimento dos professores e reações",
       caption = cap("Fonte: Grossi e Santos (2009), p. 250-254. Alguns valores são aproximados no artigo ('mais de', 'cerca de'). As bases variam: todos os estudantes, envolvidos, vítimas ou casos.")) + tema
salva(g14, "g14_poa_denuncia", 9, 5.5)
cat("14 gráficos salvos em", pasta, "\n")
