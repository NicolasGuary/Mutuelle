#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("shinythemes")
#install.packages('rsconnect')
#install.packages("tidyr")
#install.packages("FactoMineR")
#install.packages("lme4")
library(shinythemes)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(tidyr)
library(FactoMineR)

shinyApp(
  ui = 
    navbarPage(
      theme = shinytheme("cosmo"),
      "Mutuelle des motards",
      tabPanel("Accueil",includeHTML("www/index.html")),
      tabPanel("Accidents",
               h1("Evaluation des cas d'accident"),
               fluidRow(column(12,align="center", h4("Dans 14% des accidents, le motard etait fatigué."))),
             
               fluidPage(
                 fluidRow(
                   column(6,align="center",
                          h1("Etude de notre cas"),
                          fluidRow(column(12,mainPanel(plotOutput("piecauseacc"))))
                   ),
                   column (6,align="center",
                           h1("Cas général"),
                           fluidRow(column(12,mainPanel(plotOutput("piecauseaccgeneral"))))
                           
                   )),
                 
                 fluidRow(column (12 ,align="center" ,h4("Ces graphes circulaires représentent les causes qui ont mené à l'accident.",tags$br(),
                                                         " On observe que dans plus de 3 cas sur 4, la cause est principalement humaine. En effet dans notre cas, la cause principale selon le motard est l'usager qu'il a percuté, à presque 50%.",tags$br()," Cette information est intéressante puisque nous savons que selon le code de la route, dans le cas de cet accident celui qui percute l'usager de devant est responsable. Néanmoins, lorsque l'on compare cette proportion à celle du cas général, on se rend compte qu'elle est en dessous de la moyenne. Cela signifie que l'autre usager est moins souvent responsable dans notre cas d'accident.",tags$br(),
                                                         "Pour ce qui est de la responsabilité du motard, elle est proche d'un tiers, ce qui comme nous l'avons remarqué précémment est faible. Néanmoins, cette proportion est plus forte que dans le cas général ce qui semble logique.",tags$br()
                                                         ,"Enfin, les causes météorologiques, animales ou de la route sont à peu près identiques et faible dans chacun des cas. "))),
                 
                 fluidRow(
                   column(6,
                          fluidRow(column(12,mainPanel(plotOutput("pietyperouteacci"))))
                   ),
                   column(6,
                          fluidRow(column(12,mainPanel(plotOutput("pietyperouteaccig"))))
                   )
                 ),
                 
                 fluidRow(column (12 ,align="center" ,h4("Ces graphes circulaires représentent les différents types de routes où survient notre accident par rapport au cas général.",tags$br(),
                                                         "On remarque que majoritairement, cet accident a lieu près d'une fois sur deux en ville. Viens ensuite la route, puis l'autoroute.",tags$br(),
                                                         "En comparaison au cas général, on note que cet accident survient bien plus fréquemment sur l'autoroute."))),
                 
                 fluidRow(
                   column(6,
                          fluidRow(column(12,mainPanel(plotOutput("humeurpie"))))
                   ),
                   column(6,
                          fluidRow(column(12,mainPanel(plotOutput("humeurpieg"))))
                   )
                 ),
                 fluidRow(
                   column(12,align="center",radioButtons("h", "Humeur a evaluer", c("Neutre"="HUMEURNEUTRE","Gai"="HUMEURGAI","Mécontent"="HUMEURMECONTENT","Triste"="HUMEURTRISTE"), inline=T))),
                 fluidRow(column (12 ,align="center" ,h4("Ces graphes circulaires représentent les humeurs ressenties par l'usager avant l'accident.",tags$br(),
                                                         "On remarque que l'humeur la plus solicitée est l'humeur neutre. Ensuite vient l'humeur gaie, et enfin mécontente et triste qui toutes deux ont été très peu choisies.",tags$br(),
                                                         "En comparaison au cas général, on remarque que l'humeur gaie est moins présente dans notre cas.",tags$br(),tags$br())))
                 
                 )
               
               
               
      ),
      tabPanel("Incidents",  
               h1("Evaluation des cas d'incident"),
               fluidRow(column(12,align="center", h4("Dans 5% des incidents, le motard etait fatigué."))),
               
               fluidPage(
                 fluidRow(
                   column(6,align="center",
                          h1("Etude de notre cas"),
                          fluidRow(column(12,mainPanel(plotOutput("piecauseinc"))))
                   ),
                   column (6,align="center",
                           h1("Cas général"),
                           fluidRow(column(12,mainPanel(plotOutput("piecauseincgeneral"))))
                           
                   )),
                 
                 fluidRow(column (12 ,align="center" ,h4("Ces graphes circulaires représentent les causes qui ont mené à l'incident",tags$br(),
                                                         " On observe que dans plus de 3 cas sur 4, la cause est, selon le motard, l'usager qu'il a percuté.",tags$br(),
                                                         "Pour ce qui est de la responsabilité du motard, elle est faible.",tags$br()
                                                         ,"Enfin, les causes météorologiques, animales ou de la route sont à peu près identiques et dérisoires dans chacun des cas."))),
                 
                 fluidRow(
                   column(6,
                          fluidRow(column(12,mainPanel(plotOutput("pietyperouteinci"))))
                   ),
                   column(6,
                          fluidRow(column(12,mainPanel(plotOutput("pietyperouteincig"))))
                   )
                 ),
                 
                 fluidRow(column (12 ,align="center" ,h4("Ces graphes circulaires représentent les différents types de routes où survient notre incident par rapport au cas général.",tags$br(),
                                                         "On remarque que majoritairement, cet incident a lieu en villesuivi de près par la route, le périphérique, puis l'autoroute.",tags$br(),
                                                         "Les données de notre incident spécifique sont très semblables à celles du cas général."))),
                 
                 fluidRow(
                   column(6,
                          fluidRow(column(12,mainPanel(plotOutput("humeurInciPie"))))
                   ),
                   column(6,
                          fluidRow(column(12,mainPanel(plotOutput("humeurInciPieg"))))
                   )
                 ),
                 
                 fluidRow(
                   column(12, align="center",radioButtons("hum", "Humeur a evaluer", c("Neutre"="HUMEURNEUTRE","Gai"="HUMEURGAI","Content"="HUMEURMECONTENT","Triste"="HUMEURTRISTE"), inline=T))),
                 fluidRow(column (12 ,align="center" ,h4("Ces graphes circulaires représentent les humeurs ressenties par l'usager avant l'incident",tags$br(),
                                                         "On remarque que l'humeur la plus solicitée est l'humeur neutre. Ensuite vient l'humeur gaie, et enfin mécontente et triste qui toutes deux ont été très peu choisies.",tags$br(),tags$br())))
               )
               

      ),
      tabPanel("Comparaison", 
               h2("Mise en opposition des cas d'incident et d'accident afin d'en cerner les causes"),
               br(),
               fluidRow(column(12, align="center", h4("Notre cas d'étude est dans 75,3% des cas un incident, et dans 24,7% des cas un accident.",tags$br(),
                                     "Sur la totalité des données récoltées, 52,6% des cas sont un accident. Notre cas d'étude a donc une forte tendance à être un incident par rapport à la moyenne.",tags$br(),tags$br(),"On réduirait le risque d'accident de 12% en roulant par temps sec (18,28% pour accident contre 6,27% pour incident)",tags$br(), "Les motards ont deux fois plus d'accident que d'incidents quand ils sont distraits (31% contre 16,5%)")),
               br(),
               fluidRow(
                 column(12,h3("1 - Etude de l'allure et de la vitesse dans la reponsabilité de l'accident/incident")),
                 column(12, align="center",radioButtons("compvariable", "Variable à étudier:",  c("ALLURE" = "ALLURE", "VITESSE" = "VITESSE"),inline=T))),
                 column(12,plotOutput("vitesse"))),
                 column (12 ,align="center" ,h4("Ces graphes représentent l'allure et la vitesse du motard en fonction du type d'évènement (accident ou incident).",tags$br(),
                                                "L'allure nous montre qu'aller trop vite par rapport à une vitesse appropriée augmente les chances de transformer l'incident en accident.",tags$br(),
                                                "Le graphe de vitesse est lui aussi intéressant. On remarque particulièrement dans le cas des incidents deux pics aux alentours des 45-50km/h et 90km/h. Les vitesses qui correspondent aux accidents sont elles plus diffuses.")),
      
               
               br(),
               fluidRow(
                 column(12,h3("2 - Etude de l'impact des autres usagers dans la reponsabilité de l'accident/incident")),
                 column(12,plotOutput("aggrav"))),
                column (12 ,align="center" ,h4("Ce graphe représente l'influence du comportement de l'autre usager dans l'accident ou l'incident.",tags$br(),
                                               "On remarque que d'après les motards, l'influence de l'autre usager est bien plus présente dans le cadre d'un accident que d'un incident.", tags$br(),tags$br())),
      
               fluidRow(
                 column(12,h3("3 - Etude de l'impact du traffic dans la reponsabilité de l'accident/incident")),
                 column(12,plotOutput("trafic")),
                 column(12 ,align="center" ,h4("Ce graphe représente l'influence du trafic sur l'accident ou l'incident.",tags$br(),
                                                 "On remarque très facilement que plus le trafic est fluide, plus l'accident a tendance à se produire.", tags$br(),tags$br()))
      
                 )
      ),
      
      tabPanel("Prévention",
               
               h4("Cette section montre la proportion d'incident et accidents en fonction de l'equipement du motard"),
               h5(verbatimTextOutput("autrereacmean")),
               sidebarPanel(
                 radioButtons("prev", "Variable a afficher :",  c("BIEN EQUIPE ?" = "1", "ABS ?" = "2", "CBS ?"="3"))),
               mainPanel(plotOutput("equipe")),
               fluidRow(column(12, align="center", h4("Ces graphes représentent la part des accidents et incidents selon la qualité de l'équipement.",tags$br(),
                                                      "Pour ce qui est de la qualité de l'équipement, on remarque que globalement, plus le motard est mal équipé plus l'incident à tendance à se transformer à accident.",tags$br(),
                                                      "Pour ce qui est de la présence d'ABS et CBS, en cas d'absence ils ont tendance à augmenter le taux d'accidents.",tags$br()))),
               fluidRow(
                 column(12,h4("Durée de possession du permis avec nombre d'accident/incident")),
                 column(12,plotOutput("duree")),
                 column(12, align="center", h4("Ce graphe représente la part des accidents et incidents selon la qualité de l'équipement.",tags$br(),
                                                        "Pour ce qui est de la qualité de l'équipement, on remarque que globalement, plus le motard est mal équipé plus l'incident à tendance à se transformer à accident.",tags$br(),
                                                        "Pour ce qui est de la présence d'ABS et CBS, en cas d'absence ils ont tendance à augmenter le taux d'accidents.",tags$br()))),
               
               fluidRow(column(12,h3("Test du Chi2"))),
               fluidRow(
                 column(2,
                        radioButtons("choixAnova", "Variable a étudier:",  c("Vitesse" = "VITESSE", "Bien équipé ?" = "BIEN_EQUIPE", "Allure"="ALLURE","Role infrastructure"="ROLEINFRA"))),
                 column(8,
                        verbatimTextOutput("anova"))),
               fluidRow(column(12,h5("Le test du Chi2 évalue le lien entre deux variables selon 2 hypothèses H0 et H1. H0 soumet l'hypothèse que les deux variables sont dépendantes. H1 au contraire, que les deux variables sont indépendantes. Si la valeur p-value <= 0,05 on rejette H0 au profit de H1. Dans le cas contraire, on valide H0.")))
      ) 
    ),
  
  server = function(input, output) {
    moto <- read.csv("motoString.csv",sep=";")
    motoc <- read.csv("motoStringComplet.csv",sep=";")
    #TEXTE EXEMPLE
    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
      head(cars, 4)
    })
    
    #IRIS GRAPHE TP
    output$resultat<-renderPlot({
      irisFiltre<-iris
      if(!is.null(input$variable)){
        irisFiltre<-iris%>%filter(Species %in% input$variable)
      }
      methodFiltre <-input$smoothing
      # plot construction
      ggplot(data = irisFiltre, mapping = aes(x=Sepal.Width, y=Sepal.Length)) + geom_point(mapping = aes(color = Species, shape=Species)) + facet_grid(~Species, scales="fixed") + theme_light() + labs(title="Sepals according to Species or iris") + stat_smooth(method=methodFiltre) -> p
      plot(p)
    })
    
    #BIEN EQUPE
    output$equipe<-renderPlot({
      donnee.equipe<-subset(moto, !is.na(BIEN_EQUIPE))
      donnee.abs<-subset(moto, !is.na(EQ2RMABS))
      donnee.cbs<-subset(moto, !is.na(EQ2RMCBS))
      ifelse(input$prev ==1,  
             g<-ggplot(donnee.equipe, aes(factor(BIEN_EQUIPE),fill = ACC.INC)) +
               geom_bar(stat="count", position="fill") + 
               labs(title = "Proportion d'accidents en fonction de l'équipement") +
               xlab(label = "Qualité de l'équipement") +
               ylab(label = "Pourcentage")+
               scale_x_discrete(labels=c("1"="Très mal équipé","2"="Mal équipé","3"="Bien équipé","4"="Très bien équipé"))+theme(legend.title = element_blank()),
             ifelse(input$prev ==2,
                    g<-ggplot(donnee.abs, aes(factor(EQ2RMABS),fill = ACC.INC)) +
                      geom_bar(stat="count", position="fill") + 
                      labs(title = "Proportion d'accidents en fonction de la disponibilite de l'ABS") +
                      xlab(label = "ABS Disponible") +
                      ylab(label = "Pourcentage") +
                      scale_x_discrete(labels=c("0"="OUI","1"="NON","2"="JSP"))+theme(legend.title = element_blank()),
                    g<-ggplot(donnee.cbs, aes(factor(EQ2RMCBS),fill = ACC.INC)) +
                      geom_bar(stat="count", position="fill") + 
                      labs(title = "Proportion d'accidents en fonction de la disponibilite de l'ABS") +
                      xlab(label = "CBS Disponible") +
                      ylab(label = "Pourcentage")+
                      scale_x_discrete(labels=c("0"="OUI","1"="NON","2"="JSP"))+theme(legend.title = element_blank()))
      )
      plot(g)
    })
    
    #VITESSE
    output$vitesse<-renderPlot({
      ifelse(input$compvariable=="VITESSE", 
             g<- ggplot(data=moto, mapping = aes(x=VITESSE, color=ACC.INC, fill=ACC.INC)) + geom_density(alpha=0.5) + theme_light() + labs(title="Incidents ou Accidents selon la vitesse")+theme(legend.title = element_blank()),
             g<-ggplot(data=subset(moto, ALLURE!=5), mapping = aes(x=ALLURE, color=ACC.INC, fill=ACC.INC)) + geom_bar(stat="count", position="fill")+ theme_light() + labs(title="Incidents ou Accidents selon l'allure")+theme(legend.title = element_blank()) +  scale_x_discrete(limits= c("trop vite","trop lentement","vitesse appropriée","a l'arret"))
      )
      plot(g)
      
    })
    
    #PIECHART CAUSES
    # RETIRER LES VALEURS NON RENSEIGNEES DE LA CAUSE
    donnee.piechart<-subset(moto, !is.na(CAUSE_ACC1))
    
    # DONNEES ACCIDENTS
    donnee.piechart.acc <- subset(donnee.piechart, ACC.INC=="accident")
    
    output$piecauseacc<-renderPlot({
      df.cause.acc <- data.frame(
        Cause = c("Moi-même", "Autre usager", "Un animal", "Route", "Météo"),
        value = c(mean(donnee.piechart.acc$CAUSE_ACC1), mean(donnee.piechart.acc$CAUSE_ACC2), mean(donnee.piechart.acc$CAUSE_ACC3), mean(donnee.piechart.acc$CAUSE_ACC4), mean(donnee.piechart.acc$CAUSE_ACC5))
      )
      
      bp<- ggplot(df.cause.acc, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Causes de l'accident")
      pie <- bp + coord_polar("y", start=0)
      plot(pie)
    })
    
    output$piecauseinc<-renderPlot({
      # DONNEES INCIDENTS
      donnee.piechart.inc <- subset(donnee.piechart, ACC.INC=="incident")
      
      df.cause.inc <- data.frame(
        Cause = c("Moi-même", "Autre usager", "Un animal", "Route", "Météo"),
        value = c(mean(donnee.piechart.inc$CAUSE_ACC1), mean(donnee.piechart.inc$CAUSE_ACC2), mean(donnee.piechart.inc$CAUSE_ACC3), mean(donnee.piechart.inc$CAUSE_ACC4), mean(donnee.piechart.inc$CAUSE_ACC5))
      )
      
      bp<-ggplot(df.cause.inc, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Causes de l'incident")
      pie <- bp + coord_polar("y", start=0)
      pie
    })
    
    #HUMEURS ACCIDENTS
    output$humeurpie<-renderPlot({
      
      # RETIRER LES VALEURS NON RENSEIGNEES DE L'A CAUSE'HUMEUR
      donnee.filtered<-subset(moto, !is.na(get(input$h)))
      
      # RETIRER LES VALEURS NE SAIS PAS
      donnee.filtered.bis<-subset(donnee.filtered, get(input$h)!=5)
      
      # ACCIDENTS
      donnee.filtered.bis.acc<-subset(donnee.filtered.bis, ACC.INC=="accident")
      
      df.acc <- data.frame(
        Cause = c("Oui (contexte)", "Oui (travail)", "Oui (privé)", "Non"),
        value = c(count(subset(donnee.filtered.bis.acc,get(input$h)==1))$n, count(subset(donnee.filtered.bis.acc,get(input$h)==2))$n, count(subset(donnee.filtered.bis.acc,get(input$h)==3))$n, count(subset(donnee.filtered.bis.acc,get(input$h)==4))$n)
      )
      
      bp<- ggplot(df.acc, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Humeur durant l'accident")+ 
        scale_fill_manual("legend", values = c("#e57373", "#d7ffd9", "#a5d6a7", "#75a478"))
      pie <- bp + coord_polar("y", start=0)
      plot(pie)
    })
    
    #HUMEURS INCIDENT
    output$humeurInciPie<-renderPlot({
      
      # RETIRER LES VALEURS NON RENSEIGNEES DE L'A CAUSE'HUMEUR
      donnee.filtered<-subset(moto, !is.na(get(input$hum)))
      
      # RETIRER LES VALEURS NE SAIS PAS
      donnee.filtered.bis<-subset(donnee.filtered, get(input$hum)!=5)
      
      # ACCIDENTS
      donnee.filtered.bis.acc<-subset(donnee.filtered.bis, ACC.INC=="incident")
      
      df.acc <- data.frame(
        Cause = c("Oui (contexte)", "Oui (travail)", "Oui (privé)", "Non"),
        value = c(count(subset(donnee.filtered.bis.acc,get(input$hum)==1))$n, count(subset(donnee.filtered.bis.acc,get(input$hum)==2))$n, count(subset(donnee.filtered.bis.acc,get(input$hum)==3))$n, count(subset(donnee.filtered.bis.acc,get(input$hum)==4))$n)
      )
      
      bp<- ggplot(df.acc, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Humeur durant l'incident")+ 
        scale_fill_manual("legend", values = c("#e57373", "#d7ffd9", "#a5d6a7", "#75a478"))
      pieInci <- bp + coord_polar("y", start=0)
      plot(pieInci)
    })
    
    #AGGRAVTION
    output$aggrav<-renderPlot({
      # RETIRER LES VALEURS NON RENSEIGNEES
      donnee.filtered<-subset(moto, AGGRAVATION!="")
      # RETIRER LES VALEURS NE SAIS PAS
      donnee.filtered.bis<-subset(donnee.filtered, AGGRAVATION!="NSP")
      df.aggravation.inc <- data.frame(
        Aggravation = c("Oui", "Non", "Oui", "Non"),
        value = c(
          count(subset(donnee.filtered, AGGRAVATION==1, ACC.INC==1))$n,
          count(subset(donnee.filtered, AGGRAVATION==2, ACC.INC==1))$n,
          count(subset(donnee.filtered, AGGRAVATION==1, ACC.INC==2))$n,
          count(subset(donnee.filtered, AGGRAVATION==2, ACC.INC==2))$n
        )
      )
      ggplot(donnee.filtered.bis, aes(factor(AGGRAVATION), fill = ACC.INC)) +
        geom_bar(stat="count", position = "fill") + 
        labs(title = "Influence du comportement l'autre usager dans l'accident") +
        ylab(label = "Pourcentage") +
        xlab(label = "Influence de l'autre usager") +
        scale_x_discrete(labels=c("1" = "Oui", "2" = "Non")) +theme(legend.title = element_blank())
    })
    
    #ANOVA
    output$anova<-renderPrint({
      chisq.test(table(get(x=input$choixAnova,pos=moto),moto$ACC.INC))
    })
    
    
    #GRAPHE TRAFIC
    output$trafic<-renderPlot({
      #Tableau des acident/incident en fct du trafic
      tabtrafic<-table(moto$ACC.INC,moto$TRAFIC)
      #on recupere les colonnes qui nous interessent
      tabtrafic<-tabtrafic[,c(2,3,4)]
      #On normalise les donnÃ©es
      tabtrafic[1,c(1,2,3)]<-tabtrafic[1,c(1,2,3)]/79
      tabtrafic[2,c(1,2,3)]<-tabtrafic[2,c(1,2,3)]/317
      #Transformation du tableau en dtframe pour ggplot
      dftabtrafic<-as.data.frame.table(tabtrafic)
      #A REPRENDRE Graphique de l'incidence du trafic dans le cas des accident et ou incidents
      ggplot(data=dftabtrafic, aes(x=dftabtrafic$Var2,y=dftabtrafic$Freq,fill=dftabtrafic$Var1)) + geom_bar(stat="identity",  position=position_dodge()) + theme_light() + 
        labs(title = "Influence du trafic sur les situations d'accident ou d'incident") +
        ylab(label = "Pourcentage") + theme(legend.title = element_blank()) +
        xlab(label = "Etat du trafic") + scale_x_discrete(labels=c("1" = "Très fluide", "2" = "Fluide", "3"="Dense","4"="A l'arrêt"))
    })
    
    
    #PIE TYPE ROUTE
 
    #ACCIDENT
    # RETIRER LES VALEURS NON RENSEIGNEES DE BIEN EQUIPE
    output$pietyperouteacci<-renderPlot({
      donnee.filtered<-subset(moto, (TYPE_ROUTE==1)|(TYPE_ROUTE==2)|(TYPE_ROUTE==3)|(TYPE_ROUTE==4)|(TYPE_ROUTE==5)|(TYPE_ROUTE==6))
      
      #ACCIDENT
      donnee.filtered.acc<-subset(donnee.filtered, ACC.INC=="accident")
      
      df.acc <- data.frame(
        Cause = c("Ville", "Route", "Autoroute", "Périphérique", "Chemin"),
        value = c(count(subset(donnee.filtered.acc,TYPE_ROUTE==1))$n, count(subset(donnee.filtered.acc,TYPE_ROUTE==2))$n, count(subset(donnee.filtered.acc,TYPE_ROUTE==3))$n, count(subset(donnee.filtered.acc,TYPE_ROUTE==4))$n, count(subset(donnee.filtered.acc,TYPE_ROUTE==6))$n)
      )
      bp<- ggplot(df.acc, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Type de route")
      pie <- bp + coord_polar("y", start=0)
      plot(pie)
    })
    
    #INCIDENT
    output$pietyperouteinci<-renderPlot({
      donnee.filtered<-subset(moto, (TYPE_ROUTE==1)|(TYPE_ROUTE==2)|(TYPE_ROUTE==3)|(TYPE_ROUTE==4)|(TYPE_ROUTE==5)|(TYPE_ROUTE==6))
      donnee.filtered.inc<-subset(donnee.filtered, ACC.INC=="incident")
      df.inc <- data.frame(
        Cause = c("Ville", "Route", "Autoroute", "Périphérique", "Chemin"),
        value = c(count(subset(donnee.filtered.inc,TYPE_ROUTE==1))$n, count(subset(donnee.filtered.inc,TYPE_ROUTE==2))$n, count(subset(donnee.filtered.inc,TYPE_ROUTE==3))$n, count(subset(donnee.filtered.inc,TYPE_ROUTE==4))$n, count(subset(donnee.filtered.inc,TYPE_ROUTE==6))$n)
      )
      
      bp<- ggplot(df.inc, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Type de route")
      pie <- bp + coord_polar("y", start=0)
      plot(pie)
    })
    
    #MOYENNE AUTRE REACTION
    output$autrereacmean<-renderText({
      donne<-subset(moto,(!is.na(AUTRE_REACTION)|AUTRE_REACTION !="NA"))
      b<-mean(donne$AUTRE_REACTION)*100
      a<-round(b,2)
      paste("Il y a ",a,"% des motards qui pensent qu'ils auraient pu réagir différemment",sep= " ")
    })
    
    #DUREE PERMIS
    output$duree<-renderPlot({
      # RETIRER LES VALEURS NON RENSEIGNEES DUREE
      donnee.filtered<-subset(moto, !is.na(DUREE))
      ggplot(donnee.filtered, aes(factor(DUREE), fill = factor(ACC.INC))) + geom_bar(position = "fill") + theme(legend.title = element_blank())
    })
    
    
    
    
    ######## CAS GENERAL ########################################################################################################################################################
    
    #PIECHART CAUSES
    # RETIRER LES VALEURS NON RENSEIGNEES DE LA CAUSE
    donnee.piechartg<-subset(motoc, !is.na(CAUSE_ACC1))
    
    # DONNEES ACCIDENTS
    donnee.piechart.accg <- subset(donnee.piechartg, ACC.INC=="accident")
    
    output$piecauseaccgeneral<-renderPlot({
      df.cause.accg <- data.frame(
        Cause = c("Moi-même", "Autre usager", "Un animal", "Route", "Météo"),
        value = c(mean(donnee.piechart.accg$CAUSE_ACC1), mean(donnee.piechart.accg$CAUSE_ACC2), mean(donnee.piechart.accg$CAUSE_ACC3), mean(donnee.piechart.accg$CAUSE_ACC4), mean(donnee.piechart.accg$CAUSE_ACC5))
      )
      
      bp<- ggplot(df.cause.accg, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Causes de l'accident")
      pie <- bp + coord_polar("y", start=0)
      plot(pie)
    })
    
    output$piecauseincgeneral<-renderPlot({
      # DONNEES INCIDENTS
      donnee.piechart.incg <- subset(donnee.piechartg, ACC.INC=="incident")
      
      df.cause.incg <- data.frame(
        Cause = c("Moi-même", "Autre usager", "Un animal", "Route", "Météo"),
        value = c(mean(donnee.piechart.incg$CAUSE_ACC1), mean(donnee.piechart.incg$CAUSE_ACC2), mean(donnee.piechart.incg$CAUSE_ACC3), mean(donnee.piechart.incg$CAUSE_ACC4), mean(donnee.piechart.incg$CAUSE_ACC5))
      )
      
      bp<-ggplot(df.cause.incg, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Causes de l'incident")
      pie <- bp + coord_polar("y", start=0)
      pie
    })
    
    #PIE TYPE ROUTE
    
    #ACCIDENT
    # RETIRER LES VALEURS NON RENSEIGNEES DE BIEN EQUIPE
    output$pietyperouteaccig<-renderPlot({
      donnee.filteredg<-subset(motoc, (TYPE_ROUTE==1)|(TYPE_ROUTE==2)|(TYPE_ROUTE==3)|(TYPE_ROUTE==4)|(TYPE_ROUTE==5)|(TYPE_ROUTE==6))
      
      #ACCIDENT
      donnee.filtered.accg<-subset(donnee.filteredg, ACC.INC=="accident")
      
      df.accg <- data.frame(
        Cause = c("Ville", "Route", "Autoroute", "Périphérique", "Chemin"),
        value = c(count(subset(donnee.filtered.accg,TYPE_ROUTE==1))$n, count(subset(donnee.filtered.accg,TYPE_ROUTE==2))$n, count(subset(donnee.filtered.accg,TYPE_ROUTE==3))$n, count(subset(donnee.filtered.accg,TYPE_ROUTE==4))$n, count(subset(donnee.filtered.accg,TYPE_ROUTE==6))$n)
      )
      bp<- ggplot(df.accg, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Type de route")
      pie <- bp + coord_polar("y", start=0)
      plot(pie)
    })
    
    #INCIDENT
    output$pietyperouteincig<-renderPlot({
      donnee.filteredg<-subset(motoc, (TYPE_ROUTE==1)|(TYPE_ROUTE==2)|(TYPE_ROUTE==3)|(TYPE_ROUTE==4)|(TYPE_ROUTE==5)|(TYPE_ROUTE==6))
      donnee.filtered.incg<-subset(donnee.filteredg, ACC.INC=="incident")
      df.incg <- data.frame(
        Cause = c("Ville", "Route", "Autoroute", "Périphérique", "Chemin"),
        value = c(count(subset(donnee.filtered.incg,TYPE_ROUTE==1))$n, count(subset(donnee.filtered.incg,TYPE_ROUTE==2))$n, count(subset(donnee.filtered.incg,TYPE_ROUTE==3))$n, count(subset(donnee.filtered.incg,TYPE_ROUTE==4))$n, count(subset(donnee.filtered.incg,TYPE_ROUTE==6))$n)
      )
      
      bp<- ggplot(df.incg, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Type de route")
      pie <- bp + coord_polar("y", start=0)
      plot(pie)
    })
    
    #HUMEURS ACCIDENTS
    output$humeurpieg<-renderPlot({
      
      # RETIRER LES VALEURS NON RENSEIGNEES DE L'A CAUSE'HUMEUR
      donnee.filtered<-subset(motoc, !is.na(get(input$h)))
      
      # RETIRER LES VALEURS NE SAIS PAS
      donnee.filtered.bis<-subset(donnee.filtered, get(input$h)!=5)
      
      # ACCIDENTS
      donnee.filtered.bis.acc<-subset(donnee.filtered.bis, ACC.INC=="accident")
      
      df.acc <- data.frame(
        Cause = c("Oui (contexte)", "Oui (travail)", "Oui (privé)", "Non"),
        value = c(count(subset(donnee.filtered.bis.acc,get(input$h)==1))$n, count(subset(donnee.filtered.bis.acc,get(input$h)==2))$n, count(subset(donnee.filtered.bis.acc,get(input$h)==3))$n, count(subset(donnee.filtered.bis.acc,get(input$h)==4))$n)
      )
      
      bp<- ggplot(df.acc, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Humeur durant l'accident")+ 
        scale_fill_manual("legend", values = c("#e57373", "#d7ffd9", "#a5d6a7", "#75a478"))
      pie <- bp + coord_polar("y", start=0)
      plot(pie)
    })
    
    #HUMEURS INCIDENT
    output$humeurInciPieg<-renderPlot({
      
      # RETIRER LES VALEURS NON RENSEIGNEES DE L'A CAUSE'HUMEUR
      donnee.filteredg<-subset(motoc, !is.na(get(input$hum)))
      
      # RETIRER LES VALEURS NE SAIS PAS
      donnee.filtered.bisg<-subset(donnee.filteredg, get(input$hum)!=5)
      
      # ACCIDENTS
      donnee.filtered.bis.accg<-subset(donnee.filtered.bisg, ACC.INC=="incident")
      
      df.accg <- data.frame(
        Cause = c("Oui (contexte)", "Oui (travail)", "Oui (privé)", "Non"),
        value = c(count(subset(donnee.filtered.bis.accg,get(input$hum)==1))$n, count(subset(donnee.filtered.bis.accg,get(input$hum)==2))$n, count(subset(donnee.filtered.bis.accg,get(input$hum)==3))$n, count(subset(donnee.filtered.bis.accg,get(input$hum)==4))$n)
      )
      
      bp<- ggplot(df.accg, aes(x="", y=value, fill=Cause))+
        geom_bar(width = 1, stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) +
        labs(title = "Humeur durant l'incident")+ 
        scale_fill_manual("legend", values = c("#e57373", "#d7ffd9", "#a5d6a7", "#75a478"))
      pieIncig <- bp + coord_polar("y", start=0)
      plot(pieIncig)
    })
    
    
  }
)