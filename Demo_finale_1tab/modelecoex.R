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
##############################################################
#                                                            #
#    liste des fonctions et objets crées en permanence       #
#                                                            #
##############################################################
#création de la fonction qui regarde si une valeur est dans une colonne de data frame 
identique<- function(i,rang,tableau){
  return(which(tableau[,i+1] ==rang[i]))
}

#Fonction qui résoud 1 ode sur un intervalle d'injection
resoud_ode_intervalle <- function (i,modele,param,intervalle,xstart,liste_etiquette){
  #première partie, on assigne le vecteur omega_j qui est ici parms
  
  parms <- vector(length=length(liste_etiquette))
  creation_parms <- function (j,param,parms){
    parms[j] <- exp(log(param[2*j-1])+rnorm(1,0,param[2*j]))
  }
  parms <- unlist(lapply(1:length(liste_etiquette),creation_parms,param,parms))
  
  simul<- ode(
    func=modele,
    y=xstart,
    times=intervalle,
    parms=parms
  ) %>%
    as.data.frame()
}
#fonction qui résoud l'ode sur tout le temps de simulation
#arguments :
#i pour faire un lapply quand on voudra faire monte Carlo

#modele: fonction qui définit champ de vecteur
#time : fin de simulation
#liste_param : liste de toutes les valeurs de paramètres pour chaque injection avec variabilité, de longueur=nbinj*2*nombre_param
#liste_injection : liste des jours de toutes les injections_supp (la première étant à j0), et l'écart-type des variabilités
#xstart: valeurs des compartiments à j0
#liste__etiquette: étiquette des paramètres du modèle
resoud_global <- function (i,modele, time,liste_param,liste_injection,xstart,liste_etiquette){
  #première partie de simulation
  #on va trabsformer le vecteur liste_injection pour lui ajouter les variabilités éventuelles 
  #on aura donc un vecteur [tinj2;tinj3....fin_simul]
  temps_injection <- vector(length=length(liste_injection)/2)
  creation_calendrier <- function (j,liste_injection,temps_injection){
    temps_injection[j]<-floor(liste_injection[2*j-1]+rnorm(1,0,liste_injection[2*j]))
  }
  if (length(liste_injection)!=0){
    temps_injection <- unlist(lapply(1:(length(liste_injection)/2),creation_calendrier,liste_injection,temps_injection))
  }
  temps_injection<-c(temps_injection, time)
  
  #on fait la simulation de la première injectio,
  simulation_globale <- resoud_ode_intervalle(i=1,modele=modele,param=liste_param[1:(length(liste_etiquette)*2)],intervalle=seq(from=0, to=(temps_injection[1])),xstart=xstart,liste_etiquette=liste_etiquette)
  #s'il y a des injections supp
  if (length(temps_injection)-1!=0){
    #on fait une boucle dessus pour obtenir les simulations sur toutes les intervalles
    for (j in 1:(length(temps_injection)-1)){
      #la on simule sur un temps 0 à longueur d'intervalle
      simulation_intervalle <-resoud_ode_intervalle(i=1,modele=modele,param=liste_param[(2*j*length(liste_etiquette)+1):(2*j*length(liste_etiquette)+2*length(liste_etiquette))],intervalle=seq(from=0,to=(temps_injection[j+1]-temps_injection[j])),xstart=unlist(tail(simulation_globale[,2:ncol(simulation_globale)],n=1)),liste_etiquette=liste_etiquette)
      #on remet la bonne valeur de temps 
      simulation_intervalle$time<-simulation_intervalle$time+temps_injection[j]
      #on ajoute cet intervalle dans la simulation globale
      simulation_globale <- rbind(simulation_globale,simulation_intervalle)
    }
  }
  return(simulation_globale)
}


#Créer une fonction qui teste var et resoud et fait le MC si var!=0 
#arguments :
#modele: fonction qui définit champ de vecteur
#var=somme des écart-types, utilisé pour tester s'il y ba de la variabilité
#time : fin de simulation
#liste_param : liste de toutes les valeurs de paramètres pour chaque injection avec variabilité, de longueur=nbinj*2*nombre_param
#liste_injection : liste des jours de toutes les injections_supp (la première étant à j0), et l'écart-type des variabilités
#xstart: valeurs des compartiments à j0
#liste__etiquette: étiquette des paramètres du modèle
resoud_ode_et_MC <- function (modele,var, time,liste_param,liste_injection,xstart,liste_etiquette){
  #test si besoin de Monte Carlo
  if (var==0){
    #on appelle 1 seule fois la fonction qui résoud l'ode sur l'ensemble du temps de simulation
    simulations_globales <- resoud_global(i=1,modele=modele, time=time,liste_param=liste_param,liste_injection=liste_injection,xstart=xstart,liste_etiquette=liste_etiquette)
    simulation_globale <- simulations_globales
    #vu qu'on n'a pas de Monte-Carlo, on va assigner les mêmes valeurs pour le ruban d'intervalle de confiance
    #on va récupérer tous les noms de variables de ce modèle sauf times qui est en premier
    noms<- names(simulation_globale[2:ncol(simulation_globale)])
    
    for (nom in noms){
      simulation_globale[paste0(nom,"inf")]<-simulation_globale[paste0(nom)]
      simulation_globale[paste0(nom,"sup")]<-simulation_globale[paste0(nom)]
    }
  }
  #si on a de la variabilité, ne pas oublier que c'est possible de faire varier le jour d'injection
  else{
    
    #on va faire 100 ce qu'on vient de faire au dessus
    simulations_globales<-lapply(seq_len(100),resoud_global,modele=modele, time=time,liste_param=liste_param,liste_injection=liste_injection,xstart=xstart,liste_etiquette=liste_etiquette)
    #ne plus calculer moy mais mettre à la place résoud_global en changeant liste_param pour que tt varia=0
    #enlever aussi les varia des tinj
    
    
    #on les met tous ensemble, 1 pour faire directement le Monte Carlo, l'autre pour garder toutes les simulation individuelles
    simulation_globale <- data.table::rbindlist(simulations_globales)
    simulations_globales <- data.table::rbindlist(simulations_globales)
    #calcul des moyennes et quantiles
    simulation_globale<-as.data.table(simulation_globale)
    #valeur moyenne : résolution sans varia
    #on doit mettre des 0 pour chaue varia de parametres et chaque varia de date
    liste_param_moy<-liste_param
    for (j in 1:(length(liste_param)/2)){
      liste_param_moy[2*j]<-0
    }
    liste_injection_moy <- liste_injection
    for (j in 1:(length(liste_injection)/2)){
      liste_injection_moy[2*j]<-0
    }
    moy <- resoud_global(i=1,modele=modele, time=time,liste_param=liste_param_moy,liste_injection=liste_injection_moy,xstart=xstart,liste_etiquette=liste_etiquette)
    #moy<-simulation_globale[,lapply(.SD,mean),by=time]
    inf <-simulation_globale[,lapply(.SD,quantile,probs=.025),by=time]
    sup<-simulation_globale[,lapply(.SD,quantile,probs=.975),by=time]
    colnames(inf)<-c("time",paste0(colnames(simulation_globale[,2:ncol(simulation_globale)]),"inf"))
    colnames (sup)<-c("time",paste0(colnames(simulation_globale[,2:ncol(simulation_globale)]),"sup"))
    simulation_globale<-data.table::merge.data.table(moy,sup,by="time")
    simulation_globale<-data.table::merge.data.table(simulation_globale,inf,by="time")
    
  }
  return(list(as.data.frame(simulations_globales),as.data.frame(simulation_globale)))
}

#################################
#                               #
#           module              #
#                               #
################################
#Module qui s'occupe d'un modèle et retourne toutes les variables pour la comparaison.
modelecoexServer <- function(id,nom_modele,champ_vecteur,liste_etiquette,liste_valeur,nombre_estime,xstart,liste_etiquettes_tableau,modif_param,transfo){
  moduleServer(id,function(input, output,session){
    ns <- session$ns
    ######################################
    #   là on crée tous les widgets 
    
    output$valeurparametres <-renderUI({
      listeinj<-as.list(1:input$nbinj)
      div(class = "dynamicSI",
          lapply(1:length(liste_etiquette),function(j){
            lapply(listeinj, function(i) {
              #index des parametres modifié par le type de vaccin(index dans le vecteur des etiquettes du tableau)
              index <- lapply(1:nrow(modif_param),function(k){
                match(modif_param[k,1],liste_etiquettes_tableau)
              })
              #on calcule du coup leur indice dans le vecteur des parametres (index1=3 signifie que le 3 elemt de la liste des parametre, depend du vaccin)
              index1 <- lapply(index,function(x){(x+1)/2})
              fluidRow(column(3,
                              withMathJax(numericInput(ns(paste("par",j, i,sep="_")), 
                                                       label = paste0(liste_etiquette[j], i),
                                                       value=if(nombre_estime==0){0}
                                                       else if (i<= nombre_estime){if (j %in% index1){if (as.numeric(input[[paste0("vaccin",i)]])<=length(names(modif_param)[-1])){as.numeric(modif_param[match(j,index1),as.numeric(input[[paste0("vaccin",i)]])+1])}else{0}} else{liste_valeur[i+nombre_estime*(j-1)]}}
                                                       else if (i>nombre_estime){
                                                         if (j %in% index1){
                                                           #si on est sur un parametre qui depend du type de vaccin
                                                           if (as.numeric(input[[paste0("vaccin",i)]])<=length(names(modif_param)[-1])){
                                                             #si l'utilisateur ne choisit pas 'autre' pour lequel aucune valeur n'est entrée
                                                             as.numeric(modif_param[match(j,index1),as.numeric(input[[paste0("vaccin",i)]])+1])
                                                             #on lui met la valeur associée au vaccin
                                                           }
                                                           else{
                                                             #si l'utilisateur choisit catégorie "autre" on met 0
                                                             0}} 
                                                         else{
                                                           #si on est sur un parametre ne dépendant pas du vaccin on met juste la valeur de paramètre=valeur du param pour la dernière inj connue
                                                           liste_valeur[nombre_estime+nombre_estime*(j-1)]}}
                                                       ,
                                                       min=0)
                              )),
                       column(3,
                              withMathJax(numericInput(ns(paste("varia","par",j, i,sep="_")), 
                                                       label = paste0("\\(\\sigma\\)(",liste_etiquette[j], i,")"),
                                                       value=0,
                                                       min=0)
                              )),
              )
            })
          })
      )
    })
    #création des widgets pour le calendrier vaccinal :
    # uiOutput("tinj"),
    output$tinj <- renderUI({
      nbinj <- as.integer(input$nbinj)
      if (nbinj>=2){
        lapply(2:nbinj, function(i) {
          
          numericInput(ns(paste0("tinj", i)), label = paste0("jour de l'injection", i), value = switch(i,0,56,365),min=0)
        })
      }
    })
    # uiOutput("aleatinj"),
    output$aleatinj <- renderUI({
      nbinj <- as.integer(input$nbinj)
      if (nbinj>=2){
        lapply(2:nbinj, function(i) {
          
          numericInput(ns(paste0("aleatinj", i)), label = paste0("\\(\\sigma\\) (jour injection)", i), value = 0,min=0)
        })
      }
    })
    output$vaccin <- renderUI({
      nbinj <- as.integer(input$nbinj)
      lapply(1:nbinj, function(i) {
        type <- c(names(modif_param)[-1],"autre")
        column(3,
               withMathJax(radioButtons(ns(paste0("vaccin", i)), label = paste0("type de vaccin", i), choiceNames=type,choiceValues=seq(from=1, to=length(type)))
               ))
      })
    })
    
    
    
    # Insert the right number of plot output objects into the web page
    output$plots <- renderUI({
      
      plot_output_list <- lapply(1:length(names(xstart)), function(i) {
        ns <- session$ns
        box(title=paste("graphe de ",names(xstart)[i],sep=" "),status="info",width=6,
            plotOutput(ns(paste("plot", names(xstart)[i], sep=""))),
        )
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    }) 
    
    observe({
      lapply(1:length(names(xstart)), function(i) {
        
        plotname <- paste("plot", names(xstart)[i], sep="")
        
        output[[plotname]] <- renderPlot({
          ggplot(simulation_MC())+
            geom_line(aes(x=time,y=.data[[paste0(names(xstart)[i])]]),col='blue',size=0.4)+
            geom_ribbon(aes(ymin=.data[[paste0(names(xstart)[i],"inf")]],ymax=.data[[paste0(names(xstart)[i],"sup")]], x= time), fill="skyblue3",alpha=0.3)+
            theme_classic()+
            scale_x_continuous(expand = c(0, 0), limits = c(0,NA)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
            #labs(x='time post vaccination(days)',y="Memory B-cells concentration (ASCS/millions)")+
            theme(legend.position = "none") +
            ggtitle(paste0("graphe de ",names(xstart)[i]))
        })
        
      })
    })
    
  
    ##########################
    #   construction simul   #
    
    
    #la on crée les objets pour les simulations et vecteur paramètres, il faudra changer pour prendre en compte les modèles avec d'autres compartiments:
    #etape 1 créer des objets quand rien n'est simulé (pour pouvoir afficher des tableaux et graphiques vides)
    #parametres_simul : tableau des paramètres des simulations gardées en mémoire
    parametres_simul <- data.frame(matrix(nrow=0,ncol=3))
    colnames(parametres_simul)<-c("modele","nom","nom_complet")
    #données_MC data.frame de tous les MC des différentes simulations enregistrées
    
    donnees_MC <- data.frame(matrix(nrow=0,ncol=14))
    
    simulation_brute <- data.frame()
    donnees_brutes <- data.frame()
   
    
    
    
    #Maintenant on les rend réactifs pour qu'ils se modifient avec les interactions de l'utilisateur
    vecteur_param<- reactiveVal(c())
    parametres_simul<- reactiveVal(parametres_simul)
    donnees_MC <- reactiveVal(donnees_MC)
    simulation_brute <-reactiveVal(simulation_brute)
    donnees_brutes <- reactiveVal (donnees_brutes)
    
    
    
    #Construction de la simulation
    simulation_MC<-eventReactive(
      input$action,
      { waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())
      nbinj<-as.numeric(input$nbinj)
      
      
      #on fait la somme des varia pour le mettre en argument de notre fonction qui fait le Monte Carlo
      #et on construit le vecteur des parametres
      #et on construit le calendrier des injections supplémentaires
      
      #on construit aussi ici la ligne de tableau correspondant à la simulation demandée
      #faudra voir si c'est possible de se débarasser des boucles ?
      
      #argument de la fonction avec les tinj et varia
      calendrier <- c()
      #juste les tinj
      ordre <-c()
      var <-0
      param_fonction <- c()
      
      l <- length(liste_etiquette)
      #la liste de toutes les valeurs de paramètre en argument de la fonction
      for (i in 1:nbinj){
        inji <- c()
        for (j in 1:l){
          var <-var+input[[paste("varia","par",j, i,sep="_")]]
          param_fonction <- c(param_fonction,input[[paste("par",j, i,sep="_")]],input[[paste("varia","par",j, i,sep="_")]])
        }
      }
      ligne1<-c(input$time,nbinj,param_fonction[1:(2*l)])
      if (nbinj>=2){
        inji<-c()
        for (i in 2:nbinj){
          inji<-param_fonction[(2*(i-1)*l+1):(2*i*l)]
          ligne1<- c (ligne1,input[[paste0("tinj",i)]],input[[paste0("aleatinj",i)]],inji)
          var <-var+input[[paste0("aleatinj", i)]]
          ordre <- c(ordre,input[[paste0("tinj",i)]])
          calendrier <- c(calendrier,input[[paste0("tinj",i)]],input[[paste0("aleatinj",i)]])
          
        }
      }
      vecteur_param(ligne1)
      #on vérifie le calendrier vaccinal si on n'est pas dans le bon ordre, on corrige avant de passer à la suite:
      if(any(diff(ordre)<=0)){
        #si le calendrier d'injections n'est pas chronologique, on le rend chronologique
        ordre <- ordre[order(ordre)]
        #la on regarde si 2 injections sont prévues le même jour, on fait un while
        while (anyDuplicated(ordre)>0){
          #on récupère les valeurs en double
          double<-ordre[duplicated(ordre)]
          #on récupère les rangs où apparait la première valeur dupliquée
          rangs<-which(ordre %in% double[1])
          #et on rajoute +1 à sa deuxième apparition
          nouveau<-ordre
          nouveau[rangs[2]]<-nouveau[rangs[2]]+1
          ordre <- nouveau
          #et on recommence si besoin
        }
        showNotification("Le calendrier vaccinal a été modifié car la chronologie présentait une anomalie")
        #on modifie calendrier pour prendre les valeurs de ordre comme jours d'injections.
        for (i in 1:length(ordre)){
          calendrier[1+2*(i-1)]<-ordre[i]
        }
        
      }
      #on vérifie maintenant que la simulation demandée n'est pas déjà enregistrée
      #pour cela on compare chaque ligne du tableau au vecteur "ligne"(sans le 1er élément: le nom)
      #premier test : on voit s'il y a des paramètres d'enregistrés et si le nombre de paramètres est au moins égal à celui de la nouvelle simulation
      if (nrow(parametres_simul())!=0 & length(vecteur_param())<=length(parametres_simul())-1){
        #on ne compare que pour le même nombre d'injections
        comparaison <-subset(parametres_simul(),nbinj==nbinj)
        #on garde le nombre de colonnes qui correspond au nombre d'inj ( enleve les colonnes vides)
        comparaison <- comparaison[,1:(2*(length(liste_etiquette)+1)*nbinj)]
        #on trouve les numéros des rangs identiques
        rang<-Reduce(intersect,lapply(1:min(ncol(comparaison)-1,ncol(vecteur_param())),identique,vecteur_param(),comparaison))
        if(length(rang)!=0){
          showNotification("Cette simulation est déja enregistrée")
          simulation_MC<- subset(donnees_MC(),nom==comparaison$nom[min(rang)])
          simulation_brute(subset(donnees_brutes(),nom==comparaison$nom[min(rang)]))
          
        }
        #else de si on n'a pas trouvé d'enregistrement identique :
        else{
          #on fait la simulation
          liste_simul <-resoud_ode_et_MC(modele=champ_vecteur,var=var, time=input$time,liste_param=param_fonction,liste_injection=calendrier,xstart=xstart,liste_etiquette=liste_etiquette)
          simulation_MC <- as.data.frame(liste_simul[2])
          simulation_brute(as.data.table(liste_simul[1]))
          
        }
      }
      #else de s'il n'y avait pas d'enregistrement avant ou si ils concernaient des simulations avec moins de doses :
      else{
        #on fait la simulation
        liste_simul <-resoud_ode_et_MC(modele=champ_vecteur,var=var, time=input$time,liste_param=param_fonction,liste_injection=calendrier,xstart=xstart,liste_etiquette=liste_etiquette)
        simulation_MC <- as.data.frame(liste_simul[2])
        simulation_brute(as.data.table(liste_simul[1]))
        
      }
      
      simulation_MC
      })
    
    
    
    
    observeEvent(input$keep,{
      nom<-ifelse(input$nomsimul=="",paste0("simulation ",input$keep),input$nomsimul)
      nom_complet <- paste(nom_modele,nom,sep=" ")
      # id <- paste(nom,modele,sep=" ")
      nbinj<-vecteur_param()[2]
      if (nrow(parametres_simul())==0){
        p <- c (nom_complet,nom_modele, nom,vecteur_param())
        vecteur_param(p)
        t <- rbind (parametres_simul(),vecteur_param())
        parametres_simul(t)
        MC<-as.data.frame(simulation_MC())
        brut <- as.data.frame(simulation_brute())
        MC$nom<-nom
        brut$nom<-nom
        MC$modele<-nom_modele
        brut$modele <- nom_modele
        MC$nom_complet<- nom_complet
        brut$nom_complet<- nom_complet
        j<-rbind(donnees_brutes(),brut)
        r <- rbind(donnees_MC(),MC)
        donnees_brutes(j)
        donnees_MC(r)
      }
      
      else {
        #test pour voir si le nom existe déjà :
        if(nom %in% parametres_simul()[,1]){
          showNotification("Vous avez déja utlisé ce nom")
        }
        else{
          #ajoute des NA au nouvel enregistrement si nb injection inférieur pour avoir les même tailles de lignes
          if(length(vecteur_param())<length(parametres_simul())-3){
            n <- (length(parametres_simul())-3) - length(vecteur_param())
            a <- rep(NA,n)
            par<-c(vecteur_param(),a)
            vecteur_param(par)
          }
          
          if (length(vecteur_param())==length(parametres_simul())-3){
            p <- c (nom_complet,nom_modele, nom,vecteur_param())
            vecteur_param(p)
            t <- rbind (parametres_simul(),vecteur_param())
            parametres_simul(t)
            
          }
          if (length(vecteur_param())>length(parametres_simul())){
            n <- length(vecteur_param())-(length(parametres_simul())-3)
            t=parametres_simul()
            ajout<-data.frame(matrix(nrow=1,ncol=n))
            t <- cbind (t,ajout)
            p <- c (nom_complet,nom_modele, nom,vecteur_param())
            vecteur_param(p)
            t <- rbind (parametres_simul(),vecteur_param())
            parametres_simul(t)
            
          }
          
          
          
          
          MC<-as.data.frame(simulation_MC())
          brut <- as.data.frame(simulation_brute())
          MC$nom<-nom
          brut$nom<-nom
          MC$modele<- nom_modele
          brut$modele<- nom_modele
          MC$nom_complet<- nom_complet
          brut$nom_complet<- nom_complet
          j<-rbind(donnees_brutes(),brut)
          r <- rbind(donnees_MC(),MC)
          donnees_brutes(j)
          donnees_MC(r)
          
        }
      }
      #on gère les colnames de tout le monde :
      l<-length(liste_etiquettes_tableau)
      n<-length(parametres_simul())
      
      #calcul nombre d'inj
      k <- (n-1)/(l+2)
      
      noms_param<-c("nom_complet","modele","nom","temps","nbinj",paste0(liste_etiquettes_tableau,1))
      noms_variable <-names(xstart)
      if (k>=2){
        for (i in 2:k){
          ajout<-c(paste0("tinj",i),paste0("aleatinj",i),paste0(liste_etiquettes_tableau,i))
          noms_param<-c(noms_param,ajout)
        }}
      a=parametres_simul()
      b=donnees_MC()
      colnames(a)<-noms_param
      
      parametres_simul(a)
      donnees_MC(b)
    })
    
    #tableau avec cases à cocher
    output[["table_param"]] <-renderReactable({reactable(subset(parametres_simul(),select=-c(nom_complet,modele)),
                                                         selection = "multiple", 
                                                         onClick = "select"
    )})
    
    
    #suppression des enregistrements :
    observeEvent(input$clear,{
      p<-parametres_simul()[0,]
      parametres_simul(p)
      t<-donnees_MC()[0,]
      donnees_MC(t)
      
      r <- donnees_brutes()[0,]
      donnees_brutes(donnees_brutes)
    })
    
    
    
    #récuperer les rangs sélectionnés : getReactableState()
    selected <- reactive(getReactableState("table_param", "selected"))
    #étape d'après: récupérer les noms des simulations que l'utilisateur a sélectionnées
    
    noms_simul<-reactive({lapply(selected(),function(i){parametres_simul()[i,3]})})
    
    
    donnees_comparees<-reactive({
      donnees_comparees<-data.frame(donnees_MC())
      variables <-names(xstart)
      for (i in variables){
        if (input[[paste0("transfo", i)]]){
          donnees_transfo<-as.data.table(donnees_brutes()[,c("time",paste0(i),"nom","modele","nom_complet")])
          donnees_transfo[[paste0(i)]]<-log10(donnees_transfo[[paste0(i)]])
          #moy<-donnees_transfo[,lapply(.SD,mean),by=.(time,nom,modele,nom_complet)]
          moy <- as.data.table(donnees_MC()[,c("time",paste0(i),"nom","modele","nom_complet")])
          moy[[paste0(i)]]<-log10(moy[[paste0(i)]])
          #colnames(moy)<-c("time","nom","modele","nom_complet",paste0(i))
          inf <-donnees_transfo[,lapply(.SD,quantile,probs=.025,na.rm=TRUE),by=.(time,nom,modele,nom_complet)]
          colnames(inf)<-c("time","nom","modele","nom_complet",paste0(i,"inf"))
          sup<-donnees_transfo[,lapply(.SD,quantile,probs=.975,na.rm=TRUE),by=.(time,nom,modele,nom_complet)]
          colnames(sup)<-c("time","nom","modele","nom_complet",paste0(i,"sup"))
          comparaison_i<- data.table::merge.data.table(moy,sup,by=c("time","nom","modele","nom_complet"))
          comparaison_i <- data.table::merge.data.table(comparaison_i,inf,by=c("time","nom","modele","nom_complet"))
          donnees_comparees <- as.data.table(dplyr::select(donnees_MC(),-c(paste0(i),paste0(i,"inf"),paste0(i,"sup"))))
          donnees_comparees <- data.table::merge.data.table(donnees_comparees,comparaison_i,by=c("time","nom","modele","nom_complet"))
          
        }
        
      }
      
      donnees_comparees
    })
    
    
    
    
    # Insert the right number of plot output objects into the web page
    output$plotscompare <- renderUI({
      div(class = "dynamicSI",
          plot_output_list <- lapply(1:length(names(xstart)), function(i) {
            ns <- session$ns
            box(title=paste("graphe de ",names(xstart)[i],sep=" "),status="info",width=6,
                plotOutput(ns(paste("plotcompare", names(xstart)[i], sep=""))),
                checkboxInput(inputId=ns(paste0("transfo",names(xstart)[i])), label="log-transform", value = FALSE))
          })
      )
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    }) 
    
    observe({
      if (nrow(donnees_MC())!=0){
      lapply(1:length(names(xstart)), function(i) {
        
        plotname <- paste("plotcompare", names(xstart)[i], sep="")
        
        output[[plotname]] <- renderPlot({
          
            partie<-subset(donnees_comparees(),nom %in% noms_simul())
            ggplot(partie)+
              geom_line(aes(x=time,y=.data[[paste0(names(xstart)[i])]],color=nom),size=0.4)+
              geom_ribbon(aes(ymin=.data[[paste0(names(xstart)[i],"inf")]],ymax=.data[[paste0(names(xstart)[i],"sup")]], x= time, fill=nom), alpha=0.3)+
              theme_classic()+
              scale_x_continuous(expand = c(0, 0), limits = c(0,NA)) +
              scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
              scale_color_viridis(discrete = TRUE, option = "D")+
              scale_fill_viridis(discrete = TRUE)+
              #labs(x='time post vaccination(days)',y="Memory B-cells concentration (ASCS/millions)")+
              theme(legend.position = "none") +
              theme(legend.position="top")+
              ggtitle(paste0("graphe de ",names(xstart)[i]))
           
            
        
        })
      })
    }
      })
    
    
    output$dl <- downloadHandler(
      
      filename = function() {
        "simulations.xlsx"
      },
      content = function(filename){
        
        df_list <- list(parametres_simul(), donnees_MC(),donnees_brutes())
        write.xlsx(x = df_list , file = filename, rowNames = FALSE)
      }
    )
    
    
    
    #objets qu'on veut retourner pour la comparaison
    
    return(
      list(
        param = parametres_simul,
        brut = donnees_brutes,
        MC= donnees_MC,
        moy=donnees_MC
      )
    )  
    
    
    
  })
}


#module UI
modelecoexUI <-function(id,titre,image,hauteur,largeur,nombre_estime){
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
      tabPanel("Présentation du modèle",
               tags$img(src=image,height=hauteur,width=largeur),
               
      ),
      tabPanel("Entrée des paramètres",
               tags$style(type="text/css", "#inline3 label{ display: table-cell; text-align: center; vertical-align: middle; } 
                 #inline3 .form-group { display: table-row;}"),
               box(title=h3("Entrée rapide du schéma vaccinal", align = "center"),width = 12,collapsible = FALSE,solidHeader= TRUE,status='primary',
                   fluidRow(helpText("Vous pouvez entrer ici le schéma vaccinal avec les valeurs par défaut pour les",align="center"),
                            helpText (paste(nombre_estime), "premières doses",sep=" "),align="center"),
                   fluidRow(column(width=12, offset=4,div(style="height: 40px; width: 200px",tags$div(id = "inline3",numericInput(ns("nbinj"), label = "nombre d'injections", value = paste0(nombre_estime),min =1,step=1,width="10%"))))), 
                   tags$hr(),                           
                   fluidRow(column(width=5,offset=1,uiOutput(ns("tinj"))),
                            column(width=5,offset=0,uiOutput(ns("aleatinj"))),
                   ),
                   fluidRow(uiOutput(ns("vaccin"))),
                   fluidRow(column(width=12, align="center", sliderInput(ns("time"), label = "Temps simulation", min = 0, 
                                                                         max = 3000, value =  1000))),
                   fluidRow(column(width=12, align="center", actionButton(ns("action"), label = "Go !",style='padding:4px; font-size:80%')),)),
               
               box(title=h3("Entrée détaillée des paramètres", align = "center"),width = 12,collapsible = TRUE,solidHeader= TRUE,
                   uiOutput(ns("valeurparametres")),
                   
               ),
      ),
      tabPanel("Simulation obtenue", 
               waiter::use_waiter(),
               
               textInput(ns("nomsimul"), label="nom de cette simulation", value ="", width = "20%", placeholder = NULL),
               actionButton(ns("keep"), label = "Garder pour comparer",width="20%",style='padding:4px; font-size:80%'),
               
               uiOutput(ns("plots")),
               
      ),
      tabPanel("Comparaison des simulations",
               
               
               fluidRow(box(title="Tableau des paramètres des simulations enregistrées",width=12,reactableOutput(ns("table_param")),
                            actionButton(ns("clear"), label = "effacer",style='padding:4px; font-size:80%'))),
               
               fluidRow(uiOutput(ns("plotscompare"))),
               
               downloadButton(ns("dl"),"Export in Excel",width="50%",style='padding:4px; font-size:80%')
      ),
    )
  )
  
}

