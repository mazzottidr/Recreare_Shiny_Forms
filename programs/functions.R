library(rdrop2)

# Layout
side_width <- 275

# Create respostas folder
dir.create(file.path("respostas"), showWarnings = FALSE)


# Other functions
epochTime <- function() {
        as.integer(Sys.time())
}

# Token
token <- readRDS("token.rds")
drop_acc(dtoken = token) 
save_dropbox <- function(file, token){
        #token <- readRDS("token.rds") # Requires this tobe created in advance
        #drop_acc(dtoken = token) #load token
        
        # save final result to dropbox
        drop_upload(file, path = "PenDrive/Recreare/Recreare_Shiny_Forms/respostas/", dtoken = token) #filename is just the date and time
        
        invisible()
}