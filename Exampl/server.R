# Text of the books downloaded from:


function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })

  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })

  
  output$plot2 <- renderPlot({
    v <- terms()
    df<-data.frame(term=names(v),freq=v)
    ggplot(df, aes(x=term,y=freq))+geom_bar(stat="identity")+xlab("Terms")+ylab("Count")+coord_flip()+
      labs(title="Frequency Distribution of Words/Terms in Plane Crash Data", x="Words/Terms", y="Count")
  })
  korpus <- getDocumentsMatrix()
  
  # Analiza toksonomiczna - brak transformacji
  output$plottf <- renderPlot({
    dtm<-DocumentTermMatrix(korpus)
    #dtm<-DocumentTermMatrix(korpus, control = list(bounds = input$freq))
    d<-dist(dtm,method="euclidean") #Tworzenie macierzy odległości 
    fit1<-hclust(d=d, method = "ward.D") 
    plot(fit1)
  })
  
  TF<- function(){
    dtm<-DocumentTermMatrix(korpus)
    #dtm<-DocumentTermMatrix(korpus, control = list(bounds = input$freq))
    d<-dist(dtm,method="euclidean") #Tworzenie macierzy odległości 
    fit1<-hclust(d=d, method = "ward.D") 
    return(fit1)
  }
  
  #Analiza tf-idf(ważone częstości logarytmiczne)
  output$plotidf <- renderPlot({
    dtm<-DocumentTermMatrix(korpus, control = list(weighting=weightTfIdf,bounds = list(global = c(input$max)))) #częstotliwość i ilość 
    d<-dist(dtm,method="euclidean") #Tworzenie macierzy odległości 
    fit<-hclust(d=d, method = "ward.D") # 
    plot(fit)
  })
  
  #Analiza LSA(ukrytych składowych sematycznych)
  output$plotLSA <- renderPlot({
    dtm<-DocumentTermMatrix(docsCorpus, control = list(weighting=weightTfIdf,bounds = list(global = c(2,6))))  
    txt_mat <- as.textmatrix(t(as.matrix(dtm)))
    #txt_mat <- dist(t(as.matrix(dtm)))
    lsa_model <- lsa(txt_mat)
    lsa_model$tk
    lsa_model$dk
    lsa_model&sk
    s<-matrix(rep(0,16),4,4) #tworzenie macierzy o elementach zerowych
    diag(s)<-lsa_model$sk
    d<-dist(lsa_model$dk%*%s)
    fit5<-hclust(d=d, method="ward.D")
    plot(fit5)
    #t_imp<-diag(lsa_model$tk%*%s%*%t(s)%*%t(lsa_model$tk))
    #words<-names(tail(sort(t_imp),25))
    #dane<-rbind(lsa_model$dk%*%s,lsa_model$tk[words,]%*%s)
    #d<-dist(dane)
    #fit9<-hclust(d=d,method="ward.D")
    #plot(fit9)
  })
  
  LSA<- function(){
    dtm<-DocumentTermMatrix(korpus, control = list(weighting=weightTfIdf,bounds = list(global = c(input$max))))  
    txt_mat <- as.textmatrix(t(as.matrix(dtm)))
    lsa_model <- lsa(txt_mat)
    lsa_model$tk
    lsa_model$dk
    lsa_model&sk
    s<-matrix(rep(0,16),4,4) #tworzenie macierzy o elementach zerowych
    diag(s)<-lsa_model$sk
    d<-dist(lsa_model$dk%*%s)
    fit5<-hclust(d=d, method="ward.D")
    return(fit5)
  }
  
  # Porównanie analizy toksonomicznej i LSA 
  output$plottfLSA <- renderPlot({
    fit1<-TF()
    fit5<-LSA()
    dend1<-as.dendrogram(fit1)
    dend2<-as.dendrogram(fit5)
    Bk_plot(dend1,dend2,add_E=FALSE,rejection_line_asymptotic=FALSE,main="Fowlkes-Mallows Index", ylab="Fowlkes-Mallows Index")
  })
  
  #Analiza LDA
  output$plotLDA <- renderPlot({
    dtm<-DocumentTermMatrix(korpus) #częstotliwość i ilość 
    #v <- terms()
    n.words <- ncol(dtm)
    n_group <- 6
    Ida.model6 <-lda(dtm,k=n_group, method = "Gibbs", control = list(burnin = 2000, thin = 100, iter = 3000))
    perp6 <- perplexity(Ida.model6, dtm)
    res6 <- posterior(Ida.model6)
    par(mai = c(1,2,1,1))
    tl = head(sort(res6$terms[1,],description=TRUE),20)
    barplot(rev(tl),horiz=TRUE,Ias=1,main="Plik1",xlab = "Prawdopodob.")
  })

}
