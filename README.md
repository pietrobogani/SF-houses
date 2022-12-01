# SF-houses

THINGS TO BE DONE:

PARTE PRELIMINARE

1) Rendere coerenti i nhood tra i dataset: rent_clean, Eviction_notices_clean, Buyout_Agreements_clean   SARA

2) Aggiungere i nhood al dataset new_construction_clean e al dataset parcels_final SARA

3) Aggiungere dimensione (square feet) dei nhood     SOLO SE NON SONO RIUSCITO AD AGGIUNGERE I NHOOD A PARCELS_FINAL SARA

4) Aggiungere il centro dei nhood PIETRO

5) Capire la regressione funzionale SARA PIETRO TOMASO

6) Decidere come interpolare per le parcels, chiedendo al professore. 
   Prima possibilità: come da paper, distanza pesata sui nhood entro 2,5 km
   Seconda possibilità: smoothing con kernel2D (Gaussiano?) e poi valuto sulle parcels (usando questo possiamo usare l'idea di Cappozzo, cioè di raccontare
                        come stiamo "validando" il paper tramite un approccio diverso

----- FINE PARTE PRELIMINARE

PARTE ESPLORATIVA

4) Partizionare i nhood e fare functional tests sui smoothed rent (oppure su d/dt(smoothed rent)) TOM 
   Criteri di partizione: - Costieri e centrali 
                         - Tante costruzioni/(square feet) e poche costruzioni/(square feet)     OPPURE al posto di (square feet) metto #numero di parcels nel nhood se
                           siamo riusciti ad aggiungere i nhood a parcels_final 
                         - Partizione indotta da 8) (i.e. divido in nhood con tante evictions/(square feet) e poche evictions/(square feet)) 
                           OPPURE al posto di (square feet) metto #numero di parcels nel nhood se siamo riusciti ad aggiungere i nhood a parcels_final
                         - Punti importanti/di interesse e resto
                         
                         
 5) Partizionare i nhood e fare functional tests sulle smoother evictions (oppure su d/dt(smoothed evictions)) TOM 
    Criteri di partizione: - Costieri e centrali 
                          - Tante costruzioni/(square feet) e poche costruzioni/(square feet)     OPPURE al posto di (square feet) metto #numero di parcels nel nhood
                            se siamo riusciti ad aggiungere i nhood a parcels_final 
                          - Partizione indotta da 7) ( i.e. divido in nhood con rent alti e rent bassi )                                             
                          - Punti importanti/di interesse e resto
                          
 6) Ragionare su Depth Measures and Outliers   SARA
 
 7) Per ogni anno t in cui abbiamo numerose osservazioni, fare un ANOVA test, i gruppi sono i quartieri, la variabile il prezzo al mq (oppure two-way ANOVA, time and
    nhood come gruppi); PIETRO
    
 8) Per ogni anno t in cui abbiamo numerose osservazioni, fare un ANOVA test, i gruppi sono i quartieri, la variabile il numero di evictions (oppure two-way ANOVA,
    time and nhood come gruppi); PIETRO

 9) Mappa (Panoply) SARA
 
 ----- FINE ANALISI ESPLORATIVA



- Inventarsi un modo per fare il secondo modello (come usare nhood/coorinate per gli sfratti e nuove costruzioni?)
- Eviction Notices motivation, guardarle e selezionare le importanti
- Studiare l'idea di fare una regressione funzionale
- Ripetere sulle Evictions l'interpolation e lo smoothing 



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


