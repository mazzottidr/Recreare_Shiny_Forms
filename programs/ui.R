##
#  UI
##

ui <- dashboardPage(title="Recreare", skin="purple",
                    
                    dashboardHeader(title="Questionário Recreare", titleWidth=side_width), 
                    
                    dashboardSidebar(width = side_width,
                                     sidebarMenu(
                                             menuItem("Instruções", tabName = "about", icon = icon("info")),
                                             menuItem("Informações pessoais", tabName = "personal", icon = icon("user")),
                                             menuItem("Saúde geral", tabName = "health", icon = icon("stethoscope")),
                                             menuItem("Satisfação com o trabalho", tabName = "work", icon = icon("briefcase")),
                                             menuItem("Saúde mental", tabName = "mental", icon = icon("brain")),
                                             menuItem("Validação das respostas", tabName = "validation", icon = icon("check")),
                                             actionButton("do", "Submeter", icon = icon("share-square"))
                                     )
                    ),
                    
                    dashboardBody(
                            
                            tabItems(
                                    tabItem("about",
                                            tags$h4(
                                                    tags$p("Este formulário online foi desenvolvido pela equipe Recreare, e estruturado para coletar informações gerais sobre a sua saúde e sobre como o trabalho que pode influenciar a sua saúde."),
                                                    tags$p(""),
                                                    tags$p("Por favor preencha as questões indicadas nas abas ao lado e ao final clique em Submeter."),
                                                    tags$p(""),
                                                    tags$p("Se estiver usando um dispositivo móvel e o as abas não aparecerem, clique o no ícone de menu para selecionar as abas.")
                                            )
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
                                                                      "Prefiro não responder" = 2),
                                                         selected=""),
                                            
                                            dateInput("dob", "Data de nascimento", value = NULL, min = NULL, max = Sys.Date(),
                                                      format = "dd-mm-yyyy", language = "pt-BR"),
                                            
                                            radioButtons("etnia",
                                                         label= "Etnia auto-declarada",
                                                         choices=list("Asiático" = 1,
                                                                      "Branco" = 2,
                                                                      "Negro" = 3,
                                                                      "Pardo" = 4,
                                                                      "Outra" = 5),
                                                         selected=""),
                                            
                                            h5("Ao finalizar, clicar na próxima aba ao lado, ou pelo ícone do menu.")
                                            
                                    ),
                                    
                                    tabItem("health",
                                            h4("Preencha abaixo com informações relacionadas à sua saúde geral"),
                                            
                                            selectizeInput(
                                                    'doencas', 'Você já foi diagnosticado com alguma das doenças abaixo?', choices = c("Diabetes",
                                                                                                                                       "Hipertensão",
                                                                                                                                       "Asma",
                                                                                                                                       "Câncer",
                                                                                                                                       "Depressão",
                                                                                                                                       "Doenças do coração",
                                                                                                                                       "Doenças de Alzheimer",
                                                                                                                                       "Doença de Parkinson",
                                                                                                                                       "Doença Pulmonar Obstrutiva Crônica",
                                                                                                                                       "Apneia do Sono",
                                                                                                                                       "Insônia"),
                                                    multiple = TRUE,
                                                    
                                            ),
                                            
                                            h4("Inserir texto com instruções sobre como preencher o questionário")
                                    ),
                                    
                                    tabItem("work",
                                            h4("Inserir texto de apresentação do questionário"),
                                            h4(" "),
                                            h4("Inserir texto com instruções sobre como preencher o questionário")
                                    ),
                                    
                                    tabItem("mental",
                                            h4("Inserir texto de apresentação do questionário"),
                                            h4(" "),
                                            h4("Inserir texto com instruções sobre como preencher o questionário")
                                    ),
                                    
                                    tabItem("validation",
                                            h4("Por favor verifique se todas as respostas foram incluídas corretamente. Caso algum erro apareça abaixo, favor retornar às abas correspondentes e corrijir.")
                                    )
                                    
                                    
                            )
                            
                            
                    ) 
                    
)
