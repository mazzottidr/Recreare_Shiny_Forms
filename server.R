library(shiny)
library(shinydashboard)
library(shinyjs)
library(V8)
library(rdrop2)
library(dplyr)


# Define programs directory
programdat <- "programs"

# Load functions
source(paste(programdat,"functions.R", sep="/"))


# Main server start
server <- function(input, output, session) {
        
        ####### Saving responses
        
        fieldsAll <- c(
                # personal
                "nome", "cep", "empresa", "celular", "email", "sexo", "dob", "etnia",
                
                # health
                "doencas", "doencas_fam", "dores", "dores_nivel", "dores_local", "dores_local_mais", "data_ex_clinico", "satisf_saude", "meds", "fuma", "fuma_quantos", "bebe", "bebe_quantos",
                
                # work
                "satisf_work", "problema_trabalho", "work_1", "work_2", "afast_trabalho", "satisf_colegas", "satisf_salario", "satisf_chefe",
                
                # mental
                "phq_1a","phq_1b","phq_1c","phq_1d","phq_1e","phq_1f","phq_1g","phq_1h","phq_1i","phq_1j","phq_1k","phq_1l","phq_1m",
                "phq_2a","phq_2b","phq_2c","phq_2d","phq_2e","phq_2f","phq_2g","phq_2h","phq_2i",
                "phq_3a","phq_3b","phq_3c","phq_3d",
                "phq_4a","phq_4b","phq_4c","phq_4d","phq_4e","phq_4f","phq_4g","phq_4h","phq_4i","phq_4j","phq_4k",
                "phq_5a","phq_5b","phq_5c","phq_5d","phq_5e","phq_5f","phq_5g",
                "phq_6a","phq_6b","phq_6c",
                "phq_7a","phq_7b","phq_7c","phq_7d",
                "phq_8",
                "phq_9",
                "phq_10a","phq_10b","phq_10c","phq_10d","phq_10e",
                "phq_11")
        
        fields_noMultiple <- fieldsAll[!fieldsAll %in% c("doencas", "doencas_fam", "dores_local")]
        
        
        # Doencas dictionary
        doencas_coding <- c("diab", "hipert", "asma", "cancer", "depr", "dcoracao", "dalz", "dpark", "dpoc", "apnea", "insonia")
        doencas_fam_coding <- paste0(doencas_coding, "_fam")
        doencas_df <- data.frame(d_id=as.character(1:length(doencas_coding)), doencas_coding, doencas_fam_coding, stringsAsFactors = F)
        
        # Dores dictionary
        dores_coding <- c("cabeca", "pescoco", "torax", "c_cervical", "c_toracica", "c_lombar", "gluteos", "ombros", "bracos", "pernas", "abdomen", "maos", "pes")
        dores_df <- data.frame(d_id=as.character(1:length(dores_coding)), dores_coding, stringsAsFactors = F)
        
        # Define Final Order of fields
        fields_Final <- c(
                # personal
                "nome", "cep", "empresa", "celular", "email", "sexo", "dob", "etnia",
                
                # health
                doencas_coding, doencas_fam_coding, "dores", "dores_nivel", dores_coding, "dores_local_mais", "data_ex_clinico", "satisf_saude", "meds", "fuma", "fuma_quantos", "bebe", "bebe_quantos",
                
                # work
                "satisf_work", "problema_trabalho", "work_1", "work_2", "afast_trabalho", "satisf_colegas", "satisf_salario", "satisf_chefe",
                
                # mental
                "phq_1a","phq_1b","phq_1c","phq_1d","phq_1e","phq_1f","phq_1g","phq_1h","phq_1i","phq_1j","phq_1k","phq_1l","phq_1m",
                "phq_2a","phq_2b","phq_2c","phq_2d","phq_2e","phq_2f","phq_2g","phq_2h","phq_2i",
                "phq_3a","phq_3b","phq_3c","phq_3d",
                "phq_4a","phq_4b","phq_4c","phq_4d","phq_4e","phq_4f","phq_4g","phq_4h","phq_4i","phq_4j","phq_4k",
                "phq_5a","phq_5b","phq_5c","phq_5d","phq_5e","phq_5f","phq_5g",
                "phq_6a","phq_6b","phq_6c",
                "phq_7a","phq_7b","phq_7c","phq_7d",
                "phq_8",
                "phq_9",
                "phq_10a","phq_10b","phq_10c","phq_10d","phq_10e",
                "phq_11")
        
        
        formData <- reactive({
                
                proc <- function(x) {
                        
                        if (x=="dob") {
                                
                                r <- as.character(input[[x]])
                                
                        } else if (x=="data_ex_clinico") {
                                
                                r <- as.character(input[[x]])
                                
                        } else {
                                
                                r <- input[[x]] 
                        }
                        
                        return(r)
                        
                }
                
                data <- sapply(fields_noMultiple, proc)
                #data[sapply(data,is.null)] <- NA
                data <- data.frame(t(data), stringsAsFactors = F)


                # Fix doencas
                data[c(doencas_coding, doencas_fam_coding)] <- NA

                data[doencas_df$doencas_coding[doencas_df$d_id %in% input[["doencas"]]]] <- 1
                data[doencas_df$doencas_coding[!doencas_df$d_id %in% input[["doencas"]]]] <- 0

                data[doencas_df$doencas_fam_coding[doencas_df$d_id %in% input[["doencas_fam"]]]] <- 1
                data[doencas_df$doencas_fam_coding[!doencas_df$d_id %in% input[["doencas_fam"]]]] <- 0

                # Fix dores
                data[dores_coding] <- NA

                data[dores_df$dores_coding[dores_df$d_id %in% input[["dores_local"]]]] <- 1
                data[dores_df$dores_coding[!dores_df$d_id %in% input[["dores_local"]]]] <- 0

                data <- dplyr::select(data, all_of(fields_Final))

                return(data)
                
        })
        
        # Save data
        saveData <- function(data) {
                
                timestamp <- epochTime()
                
                filePath <- file.path(tempdir(), paste0(timestamp, ".RData"))
                
                data$timestamp <- timestamp
                
                #write.table(x = data, file = filePath,
                #            row.names = FALSE, quote = TRUE, col.names = T, sep = "\t", fileEncoding = "utf-8", append = F)
                
                saveRDS(data, filePath)
                save_dropbox(filePath, token)
                
        }
        
        # Save data, reset form and show thank you
        observeEvent(input$submit, {
                saveData(formData())
                shinyjs::reset("forms")
                shinyjs::show("thankyou_msg")
        })
        
        # Restart form
        observeEvent(input$submit_another, {
                shinyjs::show("forms")
                shinyjs::hide("thankyou_msg")
                shinyjs::reset("forms")
                js$gotoTop()
                newtab <- switch(input$tabs,"validation" = "about")
                updateTabItems(session, "tabs", newtab)
        })
        
        # Reset form
        observeEvent(input$reset_forms, {
                shinyjs::show("forms")
                shinyjs::reset("forms")
                js$gotoTop()
                if (input$tabs != "about") { 
                        
                        updateTabItems(session, inputId = "tabs", selected = "about") 
                }
                
        })
        
        # Next and previous buttons
        
        observeEvent(input$tabs, {
                js$gotoTop()
        })
        
        observeEvent(input$t1next, {
                js$gotoTop()
                newtab <- switch(input$tabs,"about" = "personal")
                updateTabItems(session, "tabs", newtab)
        })
        
        observeEvent(input$t2prev, {
                js$gotoTop()
                newtab <- switch(input$tabs,"personal" = "about")
                updateTabItems(session, "tabs", newtab)
        })
        
        observeEvent(input$t2next, {
                js$gotoTop()
                newtab <- switch(input$tabs,"personal" = "health")
                updateTabItems(session, "tabs", newtab)
        })
        
        observeEvent(input$t3prev, {
                js$gotoTop()
                newtab <- switch(input$tabs,"health" = "personal")
                updateTabItems(session, "tabs", newtab)
        })
        
        observeEvent(input$t3next, {
                js$gotoTop()
                newtab <- switch(input$tabs,"health" = "work")
                updateTabItems(session, "tabs", newtab)
        })
        
        observeEvent(input$t4prev, {
                js$gotoTop()
                newtab <- switch(input$tabs,"work" = "health")
                updateTabItems(session, "tabs", newtab)
        })
        
        observeEvent(input$t4next, {
                js$gotoTop()
                newtab <- switch(input$tabs,"work" = "mental")
                updateTabItems(session, "tabs", newtab)
        })
        
        observeEvent(input$t5prev, {
                js$gotoTop()
                newtab <- switch(input$tabs,"mental" = "work")
                updateTabItems(session, "tabs", newtab)
        })
        
        observeEvent(input$t5next, {
                js$gotoTop()
                newtab <- switch(input$tabs,"mental" = "validation")
                updateTabItems(session, "tabs", newtab)
        })
        
        observeEvent(input$t6prev, {
                js$gotoTop()
                newtab <- switch(input$tabs,"validation" = "mental")
                updateTabItems(session, "tabs", newtab)
        })
 
} ## server end
