source("modelecoex.R")
source("comparaison.R")

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
#################################################################################
#                                                                               #
# liste des objets et fonctions que devra coder celui qui implémente le modèle  #
#                                                                               #
#################################################################################

#######################
#  Modele original

liste_etiquette<-c("\\(\\rho\\)","\\(\\delta_A\\)","\\(\\mu_S\\)","\\(\\mu_L\\)","\\(\\delta_M\\)","\\(\\delta_S\\)","\\(\\delta_L\\)","\\(\\theta_S\\)","\\(\\theta_L\\)","\\(\\delta_{Ab}\\)")
liste_etiquettes_tableau <- c("ρ","σ(ρ)","δA","σ(δA)","μS","σ(μS)","μL","σ(μL)","δM","σ(δM)","δS","σ(δS)","δL","σ(δL)","ϴS","σ(ϴS)","ϴL","σ(ϴL)","δAb","σ(δAb)")
xstart <- c(M=0,S=0,L=0,Ab=0)
#on veut aussi prendre en compte les valeurs par défaut
#je veux savoir pour combien d'injection ont des paramètres estimés
nombre_estime<-2
#ici je fais une liste de taille nombre_estimé*nombre_paramètres avec toutes les valeurs, pas besoin de mettre les noms dans la liste.
#j'ai mis les noms pour vérifier mes valeurs entrées
liste_values <- c(rho1=3.5,rho2=20,deltaA1=0.064,0.064,2.5,0.21,0.011,0.4,log(2)/(365.25*63.3),log(2)/(365.25*63.3),1,log(2)/11.6,log(2)/(365.25*9.5),log(2)/(365.25*9.5),20,20,30,30,log(2)/23.9,log(2)/23.9)

#ATTENTION !
#la liste des etiquettes en latex, en utf8 et la liste des valeurs doit garder le même ordre (rho puis delta etc...) et correspondre à celui du champ de vecteur !


#prise en compte des vaccins et modifications des paramètres:
#tableau avec 1 colonne par vaccin et une ligne par paramètre modifié 
modif_param <- tibble::tribble(
  ~param, ~Ad26, ~MVA,
  "δA", 0.064, 0.21,
  "δS", 0.34, 0.23,
  "μS", 2.5, 0.4,
  "μL", 0.011, 0.0035,
)

#attention, il faut que les param soient ceux donnés dans la liste_etiquette_tableau, l'ordre n'a pas d'importance.

#Fonction champ de vecteur pour [t_i;t_i+1 ]
modele.original <- function (t,x,parms){
  M <- x[1]
  S <- x[2]
  L <- x[3]
  Ab <- x[4]
  dMdt <- parms[1] * exp(-parms[2] * t)-(parms[3] + parms[4])*exp(-parms[2] * t)*M-parms[5]*M
  dSdt <- parms[3]*exp(-parms[2]*t)*M-parms[6]*S
  dLdt <- parms[4]*exp(-parms[2]*t)*M-parms[7]*L
  dAbdt <- parms[8]*S+parms[9]*L-parms[10]*Ab
  dXdt <- c(dMdt,dSdt,dLdt,dAbdt)
  list(dXdt)
}

##########################
# Modele Mdelay 

liste_etiquette2<-c("\\(\\rho\\)","\\(\\delta_A\\)","\\(\\gamma\\)","\\(\\mu_S\\)","\\(\\mu_L\\)","\\(\\delta_M\\)","\\(\\delta_S\\)","\\(\\delta_L\\)","\\(\\theta_S\\)","\\(\\theta_L\\)","\\(\\delta_{Ab}\\)")
liste_etiquettes_tableau2 <- c("ρ","σ(ρ)","δA","σ(δA)","ϒ","σ(ϒ)","μS","σ(μS)","μL","σ(μL)","δM","σ(δM)","δS","σ(δS)","δL","σ(δL)","ϴS","σ(ϴS)","ϴL","σ(ϴL)","δAb","σ(δAb)")
xstart2 <- c(Mdelay=0,M=0,S=0,L=0,Ab=0)
#bien mettre les titres des compartiments

#valeurs par défaut
nombre_estime2<-3
liste_values2 <- c(rho1=4.4,36.6,167.3,deltaA1=0.064,0.064,0.064,gamma1=1,1,1,muS1=0.13,1.28,0.25,muL1=14*10^(-4),14*10^(-4),14*10^(-4),deltaM1=1/(60*365.25),1/(60*365.25),1/(60*365.25),deltaS1=0.34,0.34,0.34,deltaL1=1/(8.5*365.25),1/(8.5*365.25),1/(8.5*365.25),thetaS1=13.5,13.5,13.5,thetaL1=13.5,13.5,13.5,deltaAb1=0.029,0.029,0.029)

#valeurs modifiées selon le vaccin
modif_param2 <- tibble::tribble(
  ~param, ~Ad26, ~MVA,
  "δA", 0.064, 0.21,
  "δS", 0.34, 0.23,
  
)

#Fonction champ de vecteur pour [t_i;t_i+1 ]
modele.Mdelay <- function (t,x,parms){
  Mdelay <- x[1]
  M <- x[2]
  S <- x[3]
  L <- x[4]
  Ab <- x[5]
  
  dMdelay <- parms[1]*exp(-parms[2]*t)-parms[3]*Mdelay
  dM <- parms[3]*Mdelay-(parms[4]+parms[5])*exp(-parms[2]*t)*M-parms[6]*M
  dS <- parms[4]*exp(-parms[2]*t)*M-parms[7]*S
  dL <- parms[5]*exp(-parms[2]*t)*M-parms[8]*L
  dAb <- parms[9]*S+parms[10]*L-parms[11]*Ab
  dXdt <- c(dMdelay,dM,dS,dL,dAb)
  list(dXdt)
}

#Pour l'appli :
titre_appli <- "Modèles de réponse immunitaire Ebola"
# #variables dont on veut voir les graphes quand on compare tous nos modèles
# variables_comparees <- c("M","S","L","Ab")



#############
#           #
#    UI     #
#           #
#############


ui <- withMathJax(tagList(
  #présentation générale de l'appli, toujours identique :
  dashboardPage(
    dashboardHeader(titleWidth = "800px",
                    title = span(tags$img(src='inserm.jpg', height="40", width="100"),titre_appli ,tags$img(src='Logo-Inria.png', height="60", width="100") )),
    
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      tags$head(tags$style(HTML('
         .skin-blue .left-side, .skin-blue .wrapper {
                        background-color: #ecf0f5;
                        }
         '))),
      #on place tous les modules dans ce tabsetPAnel
      tabsetPanel(
        #A chaque nouveau modèle, on écrit :
        #image correspond à l'image d'illustration du modèle présente dans le dossier www
        #on peut régler leur taille avec hauteur et largeur
        tabPanel("Modèle Balleli",
                 modelecoexUI("test",image="Modèle1.png",hauteur="500px",largeur="1000px",titre="modèle original",nombre_estime=2)
        ),
        
        #Nouveau modèle
        tabPanel("Modèle Mdelay",
                 modelecoexUI("test2",image="Modèle.png",hauteur="500px",largeur="1000px",titre="modèle avec latence",nombre_estime=3)
        ),
        #Pour finir on crée l'onglet de comparaison :
        tabPanel("Comparaison des modèles",
                 comparaisonUI("comp")
                ),
        
        
      #fin du tabsetPanel dans lequel on a placé les modèles et le comparatif :  
      )
    )
  )
))



###################
#                 #
#     Server      #
#                 #
###################
server <- function(input, output) {
  #toujours là, ne pas modifier:
  param<-reactiveVal()
  MC<-reactiveVal()
  brut<-reactiveVal()
  #noms<-reactiveVal()
  
  #Appel du module qui implémente le modèle et on le stocke dans recup pour créer les  arguments pour le module qui les compare
  recup1<-modelecoexServer("test",nom_modele="original",champ_vecteur=modele.original,liste_valeur=liste_values,nombre_estime=2,liste_etiquette=liste_etiquette,xstart=xstart,liste_etiquettes_tableau=liste_etiquettes_tableau,modif_param=modif_param)
  recup2<-modelecoexServer("test2",nom_modele="avec Mdelay",champ_vecteur=modele.Mdelay,liste_valeur=liste_values2,nombre_estime=3,liste_etiquette=liste_etiquette2,xstart=xstart2,liste_etiquettes_tableau=liste_etiquettes_tableau2,modif_param=modif_param2)
  
  
  #fonction qui met toutes les données ensembles, ajouter en argument à chaque nouveau modèle implémenté:
  observe({
    MC_commun <-rbindlist(list(recup1$MC(), recup2$MC()), fill = TRUE)
    MC(MC_commun)
    param_commun <-rbindlist(list(recup1$param(), recup2$param()), fill = TRUE)
    param(param_commun)
    brut_commun <-rbindlist(list(recup1$brut(), recup2$brut()), fill = TRUE)
    brut(brut_commun)
    # noms_communs<-append(noms1$noms(),noms2$noms())
    # noms(noms_communs)
  })
  comparaisonServer("comp",noms_modeles=c("original","avec Mdelay"),param=param,MC=MC,brut=brut)
#mettre les mêmes noms de modèle que donnés aux modules qui ont implémenté les modèles.
}



#########################
#                       #
#  Run the application  # 
#                       #
#########################
shinyApp(ui = ui, server = server)

