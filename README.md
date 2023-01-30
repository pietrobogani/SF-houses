# SF-houses

THINGS TO BE DONE:

PARTE PRELIMINARE

1) FATTO Rendere coerenti i nhood tra i dataset: rent_clean, Eviction_notices_clean, Buyout_Agreements_clean   SARA

2) FATTO Aggiungere i nhood al dataset new_construction_clean e al dataset parcels_final SARA

3) FATTO Aggiungere dimensione (square feet)/popolazione dei nhood     SARA

4) FATTO Aggiungere il centro dei nhood, prendo da dataset SFneighboorhods di Sara  PIETRO

5) Capire la regressione funzionale SARA PIETRO TOMASO

6) Decidere come interpolare per le parcels, chiedendo al professore. 
   Prima possibilità: come da paper, distanza pesata sui nhood entro 2,5 km
   Seconda possibilità: smoothing con kernel2D (Gaussiano?) e poi valuto sulle parcels (usando questo possiamo usare l'idea di Cappozzo, cioè di raccontare
                        come stiamo "validando" il paper tramite un approccio diverso

----- FINE PARTE PRELIMINARE

PARTE ESPLORATIVA

4) Partizionare i nhood e fare functional tests sui smoothed rent (oppure su d/dt(smoothed rent)) TOM 
   Criteri di partizione: - FATTO Tante costruzioni/(square feet) e poche costruzioni/(square feet)                   PIETRO
                          - FATTO Divido in nhood con tante evictions/(square feet) e poche evictions/(square feet))  PIETRO
                          - Divido in nhood con tante evictions/(#parcels) e poche evictions/(#parcels))              PIETRO
                          - Tante costruzioni/(#parcels) e poche costruzioni/(#parcels)                               PIETRO
                          - Costieri e centrali 
                          
                         
                         
 5) Partizionare i nhood e fare functional tests sulle smoother evictions (oppure su d/dt(smoothed evictions)) TOM 
    Criteri di partizione: - Tante costruzioni/(square feet) e poche costruzioni/(square feet)     o popolazione PIETRO
                           - Divido in nhood con rent alti e rent bassi  PIETRO
                           - Tante costruzioni/(#parcels) e poche costruzioni/(#parcels)                               PIETRO
                           - Costieri e centrali                                              
                          
                          
 6) FATTO Depth Measures and Outliers   TOM
 
 7) FATTO Per ogni anno t in cui abbiamo numerose osservazioni, fare un ANOVA test, i gruppi sono i quartieri, la variabile il prezzo al mq (oppure two-way ANOVA, time
    and nhood come gruppi); PIETRO
    
 8) Per ogni anno t in cui abbiamo numerose osservazioni, fare un ANOVA test, i gruppi sono i quartieri, la variabile il numero di evictions (oppure two-way ANOVA,
    time and nhood come gruppi); PIETRO

 
 10) Basic version of GAM (rent (granularità giornaliera) ~ nhood + #costruzioni nel nhood + anno + distanza del nhood da attrazioni) .
     Cose necessarie: - Distanza luoghi di interesse dai centri di nhood (Caltrain Station - Distance to financial district)
                      - Constructions per nhood and per year PIETRO
 
 ----- FINE ANALISI ESPLORATIVA (Prima Presentazione)


1) FATTO TOM Smoothing con kernel2D (Gaussiano?) e poi valuto sulle parcels (usando questo possiamo usare l'idea di Cappozzo, cioè di raccontare
   come stiamo "validando" il paper tramite un approccio diverso). 
   N.B. Utilizziamo i rent geolocalizzati come ulteriori "vincoli" nell'interpolazione
   Utilizzo di "npreg". Provare a calcolare l'errore che viene fatto tramite smoothing su solo centroidi e medie calcolato su rent geolocalzzati.
   Cancella osservazioni fuori da SF  
1.1) FATTO Assegno pesi diversi a osservazioni e medie e calcolo con npreg (usare codice codice kern_smooth2d_code.r). PIETRO (se hai tempo)
1.2)FATTO Mappa 2D con colore sfumato: mettere i nhood e non parcels (codice kern_smooth2d_code.r) SARA
2) FATTO Rifare Smoothing delle Evictions, questa volta normalizzando i valori o dividendo per l'area oppure per il #parcel. L'idea sarebbe ottenere un trend simile a 
   quello osservato sugli smoothed rent. Poi ripetiamo i Functional tests.
   Rifare normalizzazione con RESUNITS in nhood e vedere cosa esce TOM
3) BALZA Smoothing su new_constructions normalizzando per RESUNITS per fare punto 4) 
4) Ripetere i Functional Tests con partizioni diverse: - Functional Clustering sulle evictions e poi test sui rent PIETRO 
5) TOM GAM scrauso a granularità nhood: - Aggiungere nhood come parte non parametrica  -> NO SENSE! O distanze da punti di interesse O nhood!
                                        - Cambiare "year" con "giorno"
                                        - Provare a usare gam() invece di lm()
                                        - FATTO Imparare ad interpretare il summary dei GAM, in particolare i p-value. Cioè come capire se una covariata è significativa o meno TOM
                                        - Usare cubic e non natural splines
6) FATTO Calcolo new_constructions vicine ad ogni parcel: 1° idea: calcolo per ogni parcel un cerchio con tutti i vari raggi e cerco una funzione che calcola se un punto 
                                                             (i.e. una new_construction) sta o meno dentro quel cerchio.   SARA                                                       
7) Fatto Calcolare distanza da Financial District e Caltrain Station da ogni parcel, da aggiungere al file "parcel" oppure in un file nuovo. La funzione "distanza" si trova
   nel file "codeforgammodel" SARA
8) FATTO Google Bus Stations (https://www.google.com/maps/d/viewer?msa=0&mid=1LWUFje0UZth-Z9pheZ_9J6RBkQk&ll=37.756617583210826%2C-122.42224206347657&z=13) SARA
8.1) FATTO Aggiungere distanza minima da bus stop per ogni parcel SARA
9) PIETRO Riprodurre a granularità nhood il GAM usando come target variable evictions(nhood,monthly) 
   Modificare la normalizzazione
   Provare a troncare le osservazioni? Provare a non considerare in stile robust?  
10) Implementare Robust Statistics/Conformal Prediction PIETRO
11) Valutare mappe per la presentazione finale 
12) TOM Implementare GAM sui prezzi a granularità parcels  
13) Scrivere due righe di commento per i modelli per presentare veloce a cappozzo e raccogliere domande da fare al Cappozz
14) TUTTI Raccogliere in una cartella i vari plot/summary di modelli 


DA CHIEDERE:
- "riscalare/normalizzare" i dati prima dei modelli ha senso?
- dipendenza/indipendeza osservazioni nei gam ?
- report?
- quantità di modelli?
- smoothing kernel con aggiunta di osservazioni localizzate ha senso?
- far vedere qualche plot per interpretazione 








------- PRESENTAZIONE FINALE.
1) Scrivere nella prima slide di quanti cfu è il corso.
2) Lista link aritcoli o blibliografia: 

WHERE TO DOWNLOAD DATASETS:

1) https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-07-05
2) https://data.sfgov.org/Housing-and-Buildings/Eviction-Notices/5cei-gny5
3) https://data.sfgov.org/Housing-and-Buildings/Buyout-Agreements/wmam-7g8d
4) https://sfdbi.org/building-permits-issued
5) https://data.sfgov.org/Housing-and-Buildings/Land-Use/us3s-fp9q (Parcel file, to be exported in non-geographical form)


RESEARCH QUESTIONS:


0) Introduzione al problema: - Mostrare come a SF sia peggio che in altre città, ad esempio guardando il costo di un affitto al mq e paragonandolo ad altre città
                               (a parità di salario)

1) IDEA: RENT CRESCENTI => GENTRIFICAZIONE CRESCENTE
   Osservare la crescita dei prezzi dei rent nei vari quartieri al passare del tempo, fare così ipotesi su
   displacement/gentrificazione da poi verificare (si spera) alla fine dello studio.
   Possiamo: - Per ogni istante di tempo che abbiamo fare un ANOVA test, i gruppi sono i quartieri, la variabile il prezzo al mq (oppure two-way ANOVA, time and nhood 
               come gruppi);
             - Attraverso la funzione avg_rent_price(t,q):= prezzo medio di affitto nell'anno t nel quartiere q, verificare in quali quartieri il prezzo è cresciuto
               nel tempo
                                              
2) IDEA: EVICTIONS/BUYOUTS CRESCENTI => GENTRIFICAZIONE CRESCENTE
   Osservare il numero di eviction nei quartieri al passare del tempo, fare così ipotesi su displacement/gentrificazione da poi verificare (si spera) alla fine dello 
   studio.
   Possiamo: - Per ogni istante di tempo che abbiamo fare un ANOVA test, i gruppi sono i quartieri, la variabile il numero di evictions (oppure two-way ANOVA, time and
               nhood come gruppi);
             - Attraverso la funzione eviction_number(t,q):= numero di evictions nell'anno t nel quartiere q, verificare in quali quartieri il numero di evictions è
             cresciuto nel tempo
             

3) Regression model: 
   - Output : rent price al tempo t della parcel p nel neighborhood c.
   - Covariates : - Quantità di case nuove costruite "attorno" (perchè non nel nhood direttamente? Si potrebbe semplificare...)
                    Questo si può fare con weighted local regression, spiegato nel corso
                  - Fixed effect sul neighborhood (https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html)
                  - Fixed effect sull'anno
                  - Distanza dal "centro" (o studiamo luoghi importanti)
   - Spiegazione: Dal dataset rent abbiamo circa 10k annunci con nhood associato e anno di riferimento. Si procede poi con:
                   -- calcolare il prezzo medio per ogni anno dei nhood 
                   -- per ogni parcel, calcolare la distanza da essa a ogni nhood (confronto i centroidi) e
                      fissare un raggio (a tentativi) per calcolare le intersezioni di ciascuna parcel con i nhood
                   -- calcolare il prezzo annuale di ciascuna parcel come media pesata (sulla distanza) dei prezzi medi dei nhood nel suo raggio
                   -- per ogni parcel e ogni anno, calcolare la distanza da essa a ogni new_construction (in che range di tempo? 4 anni?) e
                      fissare un raggio (a tentativi) per calcolare #near_new_constr vicino alla parcel
                      Quindi ad esempio si avrà per ogni parcel #near_new_constr2010 = 10 considerando il range 2007-2010 per le costruzioni
                   -- per ogni parcel, trovare il quartiere di appartenenza ... come?!
                   -- train del modello di regressione con price_parcel ~ #near_new_constr_year_inrange4 + nhood + year + nhood:year

      E' corretto?? Se così fosse, i prezzi annuali delle parcel sarebbero tutti appiattiti sulle medie dei nhood e sfrumerebbero solamente
        sui confini con gli altri nhood ... A quel punto direi che è quasi impossibile trovare un effetto delle new_constr nelle vicinanze
        (dato che tutti i prezzi delle parcel sarebbero molto simili  
                 
4) Previsione sul numero di sfratti : n_sfratti_monthly ~ avg_monthly_price_nhood + new_constructions_nhood  ... bisogna inventarsi qualcosa di meglio...
     L'idea sarebbe che il numero di sfratti è un altro indicatore di displacement. E' vero che anche usando questo indicatore le new costruction hanno un effetto 
     positivo? ( i.e. più nuove case => meno sfratti (=> meno gentrificazione)

5) Spatial Statistics
6) Survival Analysis su quanto tempo uno sta nella casa prima dell'eviction


