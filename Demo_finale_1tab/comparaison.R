#####################################
#                                   #
# liste des packages nécessaires    #
#                                   #
#####################################
library(shiny)
library(reactable)
library(deSolve)
library(tidyverse)
library(GGally)
library(fda)
library(dplyr)
library(data.table)
library(waiter)
library(openxlsx)
library(viridis)
library(shinydashboard)
###############
#Module de comparaison des modèles
comparaisonServer <- function(id,noms_modeles,listes_etiquettes_param,param,MC,brut){
  
  moduleServer(id,function(input, output,session){
    ns <- session$ns
    
    #tableau avec cases à cocher
    output[["table_param"]] <-renderReactable({reactable(data.table(param()),
                                                         selection = "multiple", 
                                                         onClick = "select",
                                                         groupBy = "modele"
    )})
    #récuperer les rangs sélectionnés : getReactableState()
    selected <- reactive(getReactableState("table_param", "selected"))
    #étape d'après: récupérer les noms des simulations que l'utilisateur a sélectionnées
    noms_simul<-reactive({unlist(lapply(selected(),function(i){param()[i,1]}))})
    
   
    
    donnees_comparees<-reactive({
      donnees_comparees<-data.frame(MC())
      if (nrow(brut())!=0){
      variables <- names(subset(data.frame(brut()),select=-c(time,nom,modele,nom_complet)))


      comparaison_i<-data.frame(MC())

      for (i in variables){
        ##code quand on a le checkbox logtransfo
    #      if (input[[paste0("transfo", i)]]){
    # 
    #       donnees_transfo<-data.frame(brut())[,c("time",paste0(i),"nom","modele","nom_complet")]
    #       donnees_transfo[[paste0(i)]]<-log10(as.numeric(donnees_transfo[[paste0(i)]]))
    #       donnees_transfo <-as.data.table(donnees_transfo)
    # 
    #       moy <- data.frame(MC())[,c("time",paste0(i),"nom","modele","nom_complet")]
    #        colnames(moy)<-c("time",paste0(i),"nom","modele","nom_complet")
    #        moy<-as.data.table(moy)
    #        moy[[paste0(i)]]<-log10(as.numeric(moy[[paste0(i)]]))
    #       inf <-donnees_transfo[,lapply(.SD,quantile,probs=.025,na.rm=TRUE),by=.(time,nom,modele,nom_complet)]
    # colnames(inf)<-c("time","nom","modele","nom_complet",paste0(i,"inf"))
    # sup<-donnees_transfo[,lapply(.SD,quantile,probs=.975,na.rm=TRUE),by=.(time,nom,modele,nom_complet)]
    # colnames(sup)<-c("time","nom","modele","nom_complet",paste0(i,"sup"))
    # comparaison_i<- data.table::merge.data.table(moy,sup,by=c("time","nom","modele","nom_complet"))
    # comparaison_i <- data.table::merge.data.table(comparaison_i,inf,by=c("time","nom","modele","nom_complet"))
    # donnees_comparees <- as.data.table(dplyr::select(MC(),-c(paste0(i),paste0(i,"inf"),paste0(i,"sup"))))
    # donnees_comparees <- data.table::merge.data.table(donnees_comparees,comparaison_i,by=c("time","nom","modele","nom_complet"))

           
        #}
        
        if (input[[paste0("transfo2", i)]]==2){

          donnees_transfo<-data.frame(brut())[,c("time",paste0(i),"nom","modele","nom_complet")]
          #ligne nouvelle
          donnees_transfo[[paste0(i)]]<ifelse (donnees_transfo[[paste0(i)]]<=1,1,donnees_transfo[[paste0(i)]])
          donnees_transfo[[paste0(i)]]<-log10(as.numeric(donnees_transfo[[paste0(i)]]))
          #ligne nouvelle
          #donnees_transfo[[paste0(i)]]<ifelse (donnees_transfo[[paste0(i)]]<=1,1,donnees_transfo[[paste0(i)]])
          donnees_transfo <-as.data.table(donnees_transfo)
          
          moy <- data.frame(MC())[,c("time",paste0(i),"nom","modele","nom_complet")]
          colnames(moy)<-c("time",paste0(i),"nom","modele","nom_complet")
          moy<-as.data.table(moy)
          #ligne nouvelle
          moy[[paste0(i)]]<- ifelse (moy[[paste0(i)]]<=1,1,moy[[paste0(i)]])
          moy[[paste0(i)]]<-log10(as.numeric(moy[[paste0(i)]]))
          #ligne nouvelle
          # moy[[paste0(i)]]<- ifelse (moy[[paste0(i)]]<=1,1,moy[[paste0(i)]])
          inf <-donnees_transfo[,lapply(.SD,quantile,probs=.025,na.rm=TRUE),by=.(time,nom,modele,nom_complet)]
          colnames(inf)<-c("time","nom","modele","nom_complet",paste0(i,"inf"))
          sup<-donnees_transfo[,lapply(.SD,quantile,probs=.975,na.rm=TRUE),by=.(time,nom,modele,nom_complet)]
          colnames(sup)<-c("time","nom","modele","nom_complet",paste0(i,"sup"))
          comparaison_i<- data.table::merge.data.table(moy,sup,by=c("time","nom","modele","nom_complet"))
          comparaison_i <- data.table::merge.data.table(comparaison_i,inf,by=c("time","nom","modele","nom_complet"))
          donnees_comparees <- as.data.table(dplyr::select(donnees_comparees,-c(paste0(i),paste0(i,"inf"),paste0(i,"sup"))))
          donnees_comparees <- data.table::merge.data.table(donnees_comparees,comparaison_i,by=c("time","nom","modele","nom_complet"))
          
        }
        if (input[[paste0("transfo2", i)]]==3){

          donnees_transfo<-data.frame(brut())[,c("time",paste0(i),"nom","modele","nom_complet")]
          #ligne nouvelle
          donnees_transfo[[paste0(i)]] <-ifelse(donnees_transfo[[paste0(i)]]<0,0,donnees_transfo[[paste0(i)]])
          donnees_transfo[[paste0(i)]]<-(as.numeric(donnees_transfo[[paste0(i)]]))^(1/4)
          donnees_transfo <-as.data.table(donnees_transfo)
          
          moy <- data.frame(MC())[,c("time",paste0(i),"nom","modele","nom_complet")]
          colnames(moy)<-c("time",paste0(i),"nom","modele","nom_complet")
          moy<-as.data.table(moy)
          #ligne nouvelle
          moy[[paste0(i)]]<- ifelse(moy[[paste0(i)]]<0,0,moy[[paste0(i)]])
          moy[[paste0(i)]]<-(as.numeric(moy[[paste0(i)]]))^(1/4)
          inf <-donnees_transfo[,lapply(.SD,quantile,probs=.025,na.rm=TRUE),by=.(time,nom,modele,nom_complet)]
          colnames(inf)<-c("time","nom","modele","nom_complet",paste0(i,"inf"))
          sup<-donnees_transfo[,lapply(.SD,quantile,probs=.975,na.rm=TRUE),by=.(time,nom,modele,nom_complet)]
          colnames(sup)<-c("time","nom","modele","nom_complet",paste0(i,"sup"))
          comparaison_i<- data.table::merge.data.table(moy,sup,by=c("time","nom","modele","nom_complet"))
          comparaison_i <- data.table::merge.data.table(comparaison_i,inf,by=c("time","nom","modele","nom_complet"))
          donnees_comparees <- as.data.table(dplyr::select(MC(),-c(paste0(i),paste0(i,"inf"),paste0(i,"sup"))))
          donnees_comparees <- data.table::merge.data.table(donnees_comparees,comparaison_i,by=c("time","nom","modele","nom_complet"))
          
        }
      }

      donnees_comparees
      }
    })
    

    
    
    #graphe des comparaisons
    # on crée le bon nombre de graphes
    output$graphescompare <- renderUI({
      div(class = "dynamicSI",
          if(nrow(brut())!=0){
            variables <- names(subset(data.frame(brut()),select=-c(time,nom,modele,nom_complet)))
            
            plot_output_list <- lapply(1:length(variables), function(i) {
              
              ns <- session$ns
              box(title=paste("graphe de ",variables[i],sep=" "),status="info",width=6,
                  plotOutput(ns(paste("plotcompare", variables[i], sep=""))),
                  #checkboxInput(inputId=ns(paste0("transfo",variables[i])), label="log-transform", value = FALSE),
                  withMathJax(radioButtons(inputId=ns(paste0("transfo2",variables[i])),label="",choiceNames=list("données non transformées","log-transformation","\\(\\sqrt[4]X\\)"),choiceValues=seq(from=1, to=3)))
                  )
            })
          }
          else {plot_output_list<-list()}
      )
      # conversion en Taglist
      do.call(tagList, plot_output_list)
    })
    
    
    #la dedans on un un probleme de if argument of length 0 quand on met partie = données comparées
    observe({
      if(nrow(brut())!=0){
        variables <- names(subset(data.frame(brut()),select=-c(time,nom,modele,nom_complet)))
        
        lapply(1:length(variables), function(i) {
          
          plotname <- paste("plotcompare", variables[i], sep="")
          
          output[[plotname]] <- renderPlot({
            
            partie<-subset(donnees_comparees(),nom_complet %in% noms_simul())
            
            ggplot(partie)+
              geom_line(aes(x=time,y=.data[[paste0(variables[i])]],color=nom_complet),size=0.4)+
              geom_ribbon(aes(ymin=.data[[paste0(variables[i],"inf")]],ymax=.data[[paste0(variables[i],"sup")]], x= time, fill=nom_complet), alpha=0.3)+
              theme_classic()+
              scale_color_viridis(discrete = TRUE, option = "D")+
              scale_fill_viridis(discrete = TRUE)+
              theme(legend.position="top",legend.title=element_blank())
            #ggtitle(paste0("graphe de ",variables[i]))
          
              
          })
        })
      }
    })
    

    
    
    
  })
}

comparaisonUI <-function(id){
  ns <-NS(id)
  tagList(
    tags$style(HTML("
    .skin-blue .main-header .navbar {
      background-color: #367fa9;
    }
    ")),
    tags$style(HTML("


.box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#367fa9
                    }

.box.box-solid.box-primary{
border-bottom-color:#666666;
border-left-color:#666666;
border-right-color:#666666;
border-top-color:#666666;
}

                                    ")),
    tags$style(HTML("


.box.box-solid.box-info>.box-header {
  color:#fff;
  background:#8AA9DB 
                    }

.box.box-solid.box-info{
border-bottom-color:#666666;
border-left-color:#666666;
border-right-color:#666666;
border-top-color:#666666;
}

                                    ")),
    
    tabsetPanel(
      tabPanel("Tableaux des enregistrements",
               
               reactableOutput(ns("table_param")),
               
      ),
      tabPanel("Graphes des modèles",
               reactableOutput(ns("testmoyenne")),
               
               fluidRow(uiOutput(ns("graphescompare")))
      ),
      
    )
    
  )
}
