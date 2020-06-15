# Title Extracting HTTP Headers
# Example Shiny app - extracting full client-side http headers and using them in server-side. 
#    Use case example:  Authorization/authentication with tokens which are provided in the http header. 
#                       This way one can use these tokens to connect to backend services
#    Author: Daniel Bodden
#    Date: 15-06-2020

library(shiny)
library(shinyjs)
library(jsonlite)
library(DT)

# Define UI for application 
ui <- function(req){
    
    fluidPage(
        
        shinyjs::useShinyjs(),  # Include shinyjs
        
        #Javascript 
        shinyjs::extendShinyjs(text = paste0('shinyjs.getHeader = function(params) {
                          var header =  JSON.stringify(', jsonlite::serializeJSON( rapply(as.list(req), as.list, how = "list") )  ,');
                          Shiny.onInputChange("jsheader", header);
                      }'), functions = c("shinyjs.getHeader")),
        
    # Application title
    titlePanel("Getting the full client http header"),
    
    #show keys
    uiOutput('uilistheaderkeys'),
    
    #show value
    DT::DTOutput('tablewithvalues')
)
}

# Define server logic 
server <- function(input, output, session) {
    
    # get namespace id for rendering or changing UI elements in server logic
    ns <- session$ns
    
    #execute
    js$getHeader()
    
    #reactive Values
    rv <- reactiveValues(
        httpkeys = list(),
        dfheaders = data.frame()
    )
    
    #####
    #OBSERVER extract header to session$userData 
    observe({
        if(!is.null(input$jsheader)){
            session$userData$httpheader <- jsonlite::unserializeJSON(input$jsheader)
        }
    })
    #####
    
    #####
    #FUNCTION request value from header with lookup of key
    getHeaderValue <- function(lookupkey=NULL){
        
        #Early Exit
        if(is.null(session$userData$httpheader)){
            return(NULL)
        }
        
        #Early Exit
        if(is.null(lookupkey)){
            return(NULL)
        }
        
        #lookup key on root level
        if(toupper(lookupkey) %in% ls( session$userData$httpheader) ){
            return(as.character(unlist(session$userData$httpheader[as.character(lookupkey)])))
        }
        #search in nested header
        else if(toupper(lookupkey) %in% ls( session$userData$httpheader$HEADERS) ){
            return(as.character(unlist(session$userData$httpheader$header[as.character(lookupkey)])))
        }else{
            #not found
            return("NO VALUE")
        }
    }
    #####
    
    #####
    #FUNCTION Show root keys
    getHeaderKeys <- function(){
        
        #Early Exit
        if(is.null(session$userData$httpheader)){
            return(NULL)
        }
        
        #get k
        return(c(ls(session$userData$httpheader), ls(session$userData$httpheader$HEADERS)))
    }
    #####
    
    #####
    # Observe - Create dropdown list
    observe({
                #Early Exit
        if(is.null(input$jsheader)){
            return(NULL)
        }
        
        #Retrieve http header keys
        rv$httpkeys <- getHeaderKeys()

        output$uilistheaderkeys = renderUI({
            selectizeInput(inputId = 'headerkeys', label = 'Header keys', choices = rv$httpkeys,  multiple = TRUE)
        })

    })
    #####
    
    #####
    #Show lookup value by user input
    observeEvent(input$headerkeys, {
        
        #found values
        foundvalues  <- lapply( input$headerkeys, getHeaderValue)
        
        #update reactive data.frame
        rv$dfheaders <- do.call(rbind, Map(data.frame, key=input$headerkeys, value=foundvalues))
        
    })
    #####
    
    #####
    #Render DT With http key(s) and value(s)
    output$tablewithvalues <- DT::renderDT({
        DT::datatable(rv$dfheaders,
                      rownames= FALSE   )
     })
    #####
    
}

# Run the application 
shinyApp(ui = ui, server = server)
