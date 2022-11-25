# SF-houses

THINGS TO BE DONE:

- New_construction_clean : usare tool per inserire le coordinate a partitre dagli indirizzi (geoapify tipo)
- Rendere coerenti i nhood tra i dataset : rent_clean, Eviction_notices_clean, Buyout_Agreements_clean
- Fare analisi esplorativa "funzionale" su rent monthly/year + usare altre tecniche di analisi esplorativa
- Fare slide (titolo dell'altra volta, domande "differenze tra quartieri, impatto costruzioni sugli affitti e sfratti  
- Inventarsi un modo per fare il secondo modello (come usare nhood/coorinate per gli sfratti e nuove costruzioni?)
- Fare il bordello di operazioni per fare il modello di regressione al punto 3  
- Eviction Notices motivation, guardarle e selezionare le importanti



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
             
4) Si calcola la funzione new_construction(t,q):= numero di costruzioni nell'anno t nel quartiere q, si fa cluster dividendo così in due gruppi i nhood, poi si può
   fare un functional test sulle funzioni d/dt(avg_rent_price(t,q)) con l'obiettivo di verificarie se il primo cluster (assumiamo sia quello con "tante" costruzioni)
   è associato a funzioni negative (i.e. derivata negativa, prezzi calanti)
   
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


