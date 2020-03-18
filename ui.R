##
#  UI
##
library(shiny)
library(shinydashboard)
library(shinyjs)
library(V8)
library(googlesheets4)


programdat <- "programs"

##
# Functions
##
source(paste(programdat,"functions.R", sep="/"))

ui <- dashboardPage(title="Recreare", skin="purple", 
                    
                    dashboardHeader(title="Questionário Recreare", titleWidth=side_width), 
                    
                    dashboardSidebar(width = side_width,
                                     sidebarMenu(
                                             id="tabs",
                                             menuItem("Instruções", tabName = "about", icon = icon("info")),
                                             menuItem("Informações pessoais", tabName = "personal", icon = icon("user")),
                                             menuItem("Saúde geral", tabName = "health", icon = icon("stethoscope")),
                                             menuItem("Satisfação com o trabalho", tabName = "work", icon = icon("briefcase")),
                                             menuItem("Saúde mental", tabName = "mental", icon = icon("brain")),
                                             menuItem("Validação das respostas", tabName = "validation", icon = icon("check")),
                                             useShinyjs(), 
                                             actionButton("reset_forms", "Reiniciar", icon = icon("trash-alt"))
                                     )
                    ),
                    
                    dashboardBody(
                            id="forms",
                            useShinyjs(), 
                            position = "fixed-top",
                            tags$script(HTML("$('body').addClass('fixed');")),
                            extendShinyjs(text = "shinyjs.gotoTop = function() {window.scrollTo(0, 0);}"),
                            tabItems(
                                    tabItem("about",
                                            tags$h4(
                                                    tags$p("Este formulário online foi desenvolvido pela equipe Recreare, e estruturado para coletar informações gerais sobre a sua saúde e sobre como o seu trabalho pode influenciar a sua saúde."),
                                                    tags$p(""),
                                                    tags$p("Por favor preencha as questões indicadas nas abas ao lado e ao final clique em Submeter."),
                                                    tags$p(""),
                                                    tags$p("Se estiver usando um dispositivo móvel e as abas não aparecerem, clique no ícone de menu para selecionar as abas.")
                                            ),
                                            
                                            fluidRow(column(2, actionButton('t1next', "Próximo", icon("arrow-circle-right"))))
                                            
                                    ),
                                    
                                    tabItem("personal",
                                            h4("Preencha abaixo com suas informações pessoais"),
                                            
                                            textInput("nome", "Nome completo"),
                                            
                                            textInput("cep", "CEP de onde mora", placeholder = "12345-000"),
                                            
                                            textInput("empresa", "Empresa em que trabalha"),
                                            
                                            textInput("celular", "Telefone celular", placeholder = "11987654321"),
                                            
                                            textInput("email", "Endereço de e-mail", placeholder = "email@empresa.com"),
                                            
                                            radioButtons("sexo",
                                                         label= "Sexo",
                                                         choices=list("Feminino" = 1,
                                                                      "Masculino" = 2,
                                                                      "Prefiro não responder" = 2)),
                                            
                                            dateInput("dob", "Data de nascimento", value = NULL, min = NULL, max = Sys.Date(),
                                                      format = "dd-mm-yyyy", language = "pt-BR"),
                                            
                                            radioButtons("etnia",
                                                         label= "Etnia auto-declarada",
                                                         choices=list("Asiático" = 1,
                                                                      "Branco" = 2,
                                                                      "Negro" = 3,
                                                                      "Pardo" = 4,
                                                                      "Outra" = 5)),
                                            
                                            fluidRow(
                                                    column(2, actionButton('t2prev', "Anterior", icon("arrow-circle-left"))),
                                                    column(2, offset=1, actionButton('t2next', "Próximo", icon("arrow-circle-right")))
                                            )
                                            
                                    ),
                                    
                                    tabItem("health",
                                            h4("Preencha abaixo com informações relacionadas à sua saúde geral"),
                                            h5("Tente responder da maneira mais fidedigna possível, representando como sua saúde se apresenta recentemente (no último mês)."),
                                            
                                            selectizeInput(
                                                    'doencas', 'Você já foi diagnosticado com alguma das doenças abaixo?', choices = c("Diabetes"=1,
                                                                                                                                       "Hipertensão"=2,
                                                                                                                                       "Asma"=3,
                                                                                                                                       "Câncer"=4,
                                                                                                                                       "Depressão"=5,
                                                                                                                                       "Doenças do coração"=6,
                                                                                                                                       "Doenças de Alzheimer"=7,
                                                                                                                                       "Doença de Parkinson"=8,
                                                                                                                                       "Doença Pulmonar Obstrutiva Crônica"=9,
                                                                                                                                       "Apneia do Sono"=10,
                                                                                                                                       "Insônia"=11),
                                                    multiple = TRUE
                                                    
                                            ),
                                            
                                            selectizeInput('doencas_fam',
                                                           'Algum parente de primeiro grau já foi diagnosticado com alguma das doenças abaixo?',
                                                           choices = c("Diabetes"=1,                                                                                                                                           "Hipertensão"=2,
                                                                       "Asma"=3,
                                                                       "Câncer"=4,
                                                                       "Depressão"=5,
                                                                       "Doenças do coração"=6,
                                                                       "Doenças de Alzheimer"=7,
                                                                       "Doença de Parkinson"=8,
                                                                       "Doença Pulmonar Obstrutiva Crônica"=9,
                                                                       "Apneia do Sono"=10,
                                                                       "Insônia"=11),
                                                           multiple = TRUE
                                                           
                                                           
                                                           ),
                                            
                                            radioButtons('dores', "Você apresenta dores crônicas (que existem há bastante tempo e é difícil saber quando começaram)?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            
                                            sliderInput('dores_nivel', "Em caso positivo, em uma escala de 0 a 100, qual é o nível de dor crônica que você apresenta (0: sem dor nenhuma; 100: dor completamente insuportável)?",
                                                        min = 0, max = 100, value = 0),
                                            
                                            selectizeInput(
                                                    'dores_local', 'Quais regiões do seu corpo que doem (selecione todas que se apliquem)?',
                                                    choices = c("Cabeça"=1,
                                                                "Pescoço"=2,
                                                                "Tórax"=3,
                                                                "Coluna cervical"=4,
                                                                "Coluna torácica"=5,
                                                                "Coluna lombar"=6,
                                                                "Glúteos"=7,
                                                                "Ombros"=8,
                                                                "Braços"=9,
                                                                "Pernas"=10,
                                                                "Abdômen"=11,
                                                                "Mãos"=12,
                                                                "Pés"=13),
                                                    multiple = TRUE),
                                            
                                            selectizeInput(
                                                    'dores_local_mais', 'Entre todas as regiões, qual é a que incomoda mais?',
                                                    choices = c("Nenhuma"=NA,
                                                                "Cabeça"=1,
                                                                "Pescoço"=2,
                                                                "Tórax"=3,
                                                                "Coluna cervical"=4,
                                                                "Coluna torácica"=5,
                                                                "Coluna lombar"=6,
                                                                "Glúteos"=7,
                                                                "Ombros"=8,
                                                                "Braços"=9,
                                                                "Pernas"=10,
                                                                "Abdômen"=11,
                                                                "Mãos"=12,
                                                                "Pés"=13),
                                                    multiple = FALSE),
                                            
                                            dateInput("data_ex_clinico", "Aproximadamente, quando foi a última vez que fez um check-up clínico?", format = "mm-yyyy", language = "pt-BR"),
                                            
                                            sliderInput('satisf_saude', "Em uma escala de 0 a 100, qual é a sua satisfação geral em relação ao seu estado de saúde? (0: completamente insatisfeito; 100: completamente satisfeito",
                                                        min = 0, max = 100, value = 0),
                                            
                                            radioButtons('meds', "Você toma algum medicamento?", choices = c("Não"=0, "Sim"=1)),
                                            numericInput('meds_quantos', "Em caso positivo, quantos medicamentos diferentes você toma por dia?", value = 0, min = 0),
                                            
                                            radioButtons('ativ_fis', "Você pratica alguma atividade física por pelo menos 30 minutos contínuos? Quantas vezes por semana?",
                                                         choices = c("Não pratico"=0,
                                                                     "1x por semana ou menos"=1,
                                                                     "2x por semana"=2,
                                                                     "3x por semana"=3,
                                                                     "4x por semana"=4,
                                                                     "5x por semana"=5,
                                                                     "6x por semana"=6,
                                                                     "7x por semana"=7)),
                                            
                                            radioButtons('fuma', "Você fuma derivados do tabaco?",
                                                         choices = c("Não"=0,
                                                                     "Ex-fumante"=1,
                                                                     "Fumante"=2)),
                                            numericInput('fuma_quantos', "Em caso positivo, por quantos anos fuma ou fumou?", value = 0, min = 0),
                                            
                                            radioButtons('bebe', "Você consome bebidas alcoólicas?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            
                                            numericInput('bebe_quantos', "Em caso positivo, quantas bebidas (latas de cerveja, taças de vinho ou copos de destilados) você consome por semana?", value = 0, min = 0),
                                            
                                            fluidRow(
                                                    column(2, actionButton('t3prev', "Anterior", icon("arrow-circle-left"))),
                                                    column(2, offset=1, actionButton('t3next', "Próximo", icon("arrow-circle-right")))
                                            )
                                    ),
                                    
                                    tabItem("work",
                                            h4("Responda às perguntas abaixo em relação ao seu trabalho."),
                                            h5("Tente ser o mais fiel possível. Estas respostas não serão divulgadas para seu empregador de maneira individual, apenas em conjunto com os dados dos outros funcionários de maneira anônima."),
                                            
                                            sliderInput('satisf_work', "Em uma escala de 0 a 100, qual é o seu nível de satisfação com o seu trabalho?",
                                                        min = 0, max = 100, value = 0),
                                            
                                            radioButtons('problema_trabalho', "Você acha que os seus problemas de saúde são ocasionados pelo seu trabalho?",
                                                         choices = c("Discordo plenamente"=1,
                                                                     "Discordo parcialmente"=2,
                                                                     "Concordo parcialmente"=3,
                                                                     "Concordo plenamente"=4)),
                                            
                                            radioButtons('work_1', "O meu trabalho me proporciona ótimas condições para crescimento pessoal e profissional.",
                                                         choices = c("Discordo plenamente"=1,
                                                                     "Discordo parcialmente"=2,
                                                                     "Concordo parcialmente"=3,
                                                                     "Concordo plenamente"=4)),
                                          
                                            radioButtons('work_2', "Eu me sinto motivado para ir trabalhar.",
                                                         choices = c("Discordo plenamente"=1,
                                                                     "Discordo parcialmente"=2,
                                                                     "Concordo parcialmente"=3,
                                                                     "Concordo plenamente"=4)),
                                            
                                            
                                            radioButtons('afast_trabalho', "Você já precisou pedir afastamento do trabalho por conta de seu problema de saúde?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            
                                            sliderInput('satisf_colegas', "Em uma escala de 0 a 100, qual é o seu nível de satisfação com os seus colegas de trabalho?",
                                                        min = 0, max = 100, value = 0),
                                            
                                            sliderInput('satisf_salario', "Em uma escala de 0 a 100, qual é o seu nível de satisfação com os seu salário?",
                                                        min = 0, max = 100, value = 0),
                                            
                                            sliderInput('satisf_chefe', "Em uma escala de 0 a 100, qual é o seu nível de satisfação com o seu chefe ou superior direto?",
                                                        min = 0, max = 100, value = 0),
                                            fluidRow(
                                                    column(2, actionButton('t4prev', "Anterior", icon("arrow-circle-left"))),
                                                    column(2, offset=1, actionButton('t4next', "Próximo", icon("arrow-circle-right")))
                                            )
                                    ),
                                    
                                    tabItem("mental",
                                            h4("As perguntas a seguir são relacionadas à sua saúde mental. Por favor siga as instruções para cada item individualmente."),
                                            
                                            h4("Nas últimas 4 semanas, o quanto você se sentiu incomodado pelos seguintes problemas:"),
                                            radioButtons('phq_1a', "Dor de estômago",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1b', "Dores nas costas",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1c', "Dores dos membros ou juntas",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1d', "Dores mestruais ou problemas com a menstruação",
                                                         choices = c("Não aplicável"=1,
                                                                     "Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1e', "Dores ou problemas durante atividade sexual",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1f', "Dores de cabeça",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1g', "Dores no peito",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1h', "Tontura",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1i', "Desmaios",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1j', "Coração acelerado (taquicardia)",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1k', "Falta de ar",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1l', "Constitpação ou diarreia",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            radioButtons('phq_1m', "Náuseas, gases ou indigestão",
                                                         choices = c("Nada"=1,
                                                                     "Um pouco"=2,
                                                                     "Bastante"=3)),
                                            
                                            h4("Nas últimas 2 semanas, com que frequência você se sentiu incomodado pelos seguintes problemas:"),
                                            radioButtons('phq_2a', "Falta de interesse ou prazer em fazer as coisas",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3,
                                                                     "Quase todos os dias"=4)),
                                            radioButtons('phq_2b', "Sentindo-se para baixo, deprimido ou sem esperança",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3,
                                                                     "Quase todos os dias"=4)),
                                            radioButtons('phq_2c', "Com problemas no seu sono",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3,
                                                                     "Quase todos os dias"=4)),
                                            radioButtons('phq_2d', "Sentindo-se cansado ou com pouca energia",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3,
                                                                     "Quase todos os dias"=4)),
                                            radioButtons('phq_2e', "Sem apetite ou comendo demais",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3,
                                                                     "Quase todos os dias"=4)),
                                            radioButtons('phq_2f', "Sentindo-se mal consigo mesmo ou por ter chateado alguém",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3,
                                                                     "Quase todos os dias"=4)),
                                            radioButtons('phq_2g', "Falta de concentração em tarefas simples como ler ou assitir televisão",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3,
                                                                     "Quase todos os dias"=4)),
                                            radioButtons('phq_2h', "Movendo-se ou falando muito devagar ou sentindo-se muito agitado ao ponto de ter sido notado por outras  pessoas",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3,
                                                                     "Quase todos os dias"=4)),
                                            radioButtons('phq_2i', "Ter pensamentos em que seria melhor que estivesse morto, ou se machucando de alguma maneira",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3,
                                                                     "Quase todos os dias"=4)),
                                            
                                            h4("Perguntas sobre ansiedade:"),
                                            radioButtons('phq_3a', "Nas últimas 4 semanas, você teve algum ataque de ansiedade, sentindo medo ou pânico de repente?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            
                                            radioButtons('phq_3b', "Em caso positivo, isso já tinha acontecido antes?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_3c', "Estes ataques aparecem de repente, em situações em que você não esperava se sentir nervoso ou desconfortável?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_3d', "Estes ataques aborrecem você, ou você se preocupa em ter outro ataque deste no futuro?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            
                                            h4("Em relação ao seu último ataque de ansiedade:"),
                                            radioButtons('phq_4a', "Você teve falta de ar?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_4b', "Você teve taquicardia?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_4c', "Você sentiu dores ou pressão no peito?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_4d', "Você suou?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_4e', "Você sentiu que estava engasgando?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_4f', "Você sentiu calafrios ou ondas de calor?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_4g', "Você sentiu náusea, azia ou dor de barriga?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_4h', "Você se sentiu tonto, instável ou desmaiou?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_4i', "Você sentiu dormência ou formigamento em partes do seu corpo?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_4j', "Você estava trêmulo?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_4k', "Você estava com medo que estivesse morrendo?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            
                                            h4("Nas últimas 4 semanas, com que frequência você se sentiu incomodado pelos seguintes problemas:"),
                                            radioButtons('phq_5a', "Sentindo-se nervoso, ansioso, no limite ou se preocupando bastante com várias coisas diferentes",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3)),
                                            radioButtons('phq_5b', "Sentindo-se tão inqueto que não conseguiu ficar parado",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3)),
                                            radioButtons('phq_5c', "Sentindo-se cansado muito facilmente",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3)),
                                            radioButtons('phq_5d', "Tensão ou dores musculares",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3)),
                                            radioButtons('phq_5e', "Problemas ao pegar no sono ou manter o sono",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3)),
                                            radioButtons('phq_5f', "Problemas se concentrando em coisas como ler um livro ou assistir TV",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3)),
                                            radioButtons('phq_5g', "Sentindo-se irritável ou incomodado facilmente",
                                                         choices = c("Nenhum dia"=1,
                                                                     "Alguns dias"=2,
                                                                     "Mais da metade dos dias"=3)),
                                            
                                            h4("Perguntas sobre comportamentos alimentares:"),
                                            radioButtons('phq_6a', "Você frequentemente se sente que não consegue controlar o que ou o quanto você come?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_6b', "Você frequentemente come, dentro de um período de 2 horas, uma quantidade excessiva de comida quando comparado com outras pessoas?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_6c', "Este comportamento aconteceu em média pelo menos 2 vezes por semana nos últimos 3 meses?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            
                                            h4("Nos últimos 3 meses, você realizou com frequência algum dos comportamentos abaixo com o objetivo de evitar ganhar peso?"),
                                            radioButtons('phq_7a', "Estimulou vômito?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_7b', "Tomou mais que o dobro da dose recomendada de laxativos?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_7c', "Fez jejum, não comendo nada por pelo menos 24 horas?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_7d', "Praticou exercícios por mais de uma hora especificamente para evitar ganhar peso após um episódio de alimentação excessiva?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            
                                            
                                            radioButtons('phq_8', "Caso tenha respondido SIM para qualquer pergunta acima sobre evitar ganhar peso, estes comportamentos aconteceram em média pelo menos 2 vezes por semana?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            
                                            radioButtons('phq_9', "Você consome bebidas alcoólicas?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                                         
                                            h4("Algum dos comportamentos a seguir aconteceu mais de uma vez nos últimos 6 meses?"),
                                            radioButtons('phq_10a', "Consumiu bebidas alcoólicas mesmo após um médico recomendar que você pare de beber por conta de seu problema de saúde?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_10b', "Consumiu bebidas alcoólicas, ficou bêbado ou esteve de ressaca enquanto trabalhava, estudava, cuidava de crianças ou realizava outras responsabilidades?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_10c', "Faltou ou chegou atrasado ao trabalho, escola/universidade ou outras atividades pois estava bebendo ou de ressaca?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_10d', "Teve problemas de convivência com outras pessoas pois estava bebendo?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            radioButtons('phq_10e', "Dirigiu um carro depois de tomar várias bebidas alcoólicas?",
                                                         choices = c("Não"=0,
                                                                     "Sim"=1)),
                                            
                                            radioButtons('phq_11', "Caso tenha respondido SIM para qualquer um dos problemas relacionados à saúde mental acima, o qual complicado estes problemas foram para você em relação ao seu trabalho, se relacionar com outras pessoas ou cuidar de coisas sobre a sua vida?",
                                                         choices = c("Não foi complicado"=1,
                                                                     "Um pouco complicado"=2,
                                                                     "Muito complicado"=3,
                                                                     "Extremamente complicado"=4)),
                                            fluidRow(
                                                    column(2, actionButton('t5prev', "Anterior", icon("arrow-circle-left"))),
                                                    column(2, offset=1, actionButton('t5next', "Próximo", icon("arrow-circle-right")))
                                            )
                                            
                                            
                                            
                                    ),
                                    
                                    tabItem("validation",
                                            h4("Por favor verifique se todas as respostas foram incluídas corretamente. Caso algum erro apareça abaixo, favor retornar às abas correspondentes e corrigir."),
                                            
                                            fluidRow(
                                                    column(2, actionButton('t6prev', "Anterior", icon("arrow-circle-left")))
                                            ),
                                            
                                            actionButton("submit", "Submeter", icon = icon("share-square")),
                                            
                                            div(id = "form"),
                                            shinyjs::hidden(
                                                    div(
                                                            id = "thankyou_msg",
                                                            h3("Obrigado! A sua resposta foi enviada com sucesso!"),
                                                            actionButton("submit_another", "Reiniciar questionário", icon("redo"))
                                                    )
                                            )  
                                    )
                                    
                                    
                            )
                            
                            
                    ) 
                    
)
