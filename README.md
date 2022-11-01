# SF-houses

THINGS TO BE DONE:

1) Controllare se i vari datasets usino gli stessi quartieri, decidere eventualmente quali usare, informarsi su questo aspetto
2) Creare un file unico contenente le new_constructions, aggregando i dati mensili che si trovano su https://sfdbi.org/building-permits-issued
   Da decidere ancora la finestra temporale utile al progetto
3) Preparare datasets e idee per parlarne con un prof.


WHERE TO DOWNLOAD DATASETS:

1) https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-07-05
2) https://data.sfgov.org/Housing-and-Buildings/Eviction-Notices/5cei-gny5
3) https://data.sfgov.org/Housing-and-Buildings/Buyout-Agreements/wmam-7g8d
4) https://sfdbi.org/building-permits-issued


RESEARCH QUESTIONS:

1) Osservare la crescita dei prezzi nei vari quartieri ,
   fare ipotesi su displacement/gentrificazione e poi verificarle alla fine (come?).
   Possiamo: - Per ogni istante di tempo che abbiamo fare un ANOVA test, i gruppi sono i quartieri, la variabile il prezzo al mq;
                                                   - magari come funzione avg_monthly_price in nhood vs time -> studiare differenza tra quartieri
2) I quartieri dove viene costruito di più sono quelli dove ci si aspetta i prezzi saliranno maggiormente (i.e. dove c'è 
   displacement/gentrification). Si potrebbe anche in questo modo ottenere una lista di quartieri più soggetti a displacemente e verificare
   alla fine (come?) ).
   Tom: io controllerei prima se "costruire di più" implica "aumento prezzi". 
        Magari si può: - fare funzioni new_monthly_construction_nhood vs time
                       - fare funzioni avg_monthly_price_nhood vs time
                       - fare funzioni evictions_monthly_nhood vs time 
                       - vedere se valori alti di new_construction_nhood corrispondono ad aumenti di prezzi negli anni successivi
                       - come usare l'informazione sugli sfratti?? Più prezzi alti, più sfratti mi aspetto??  
3) Regression model: 
   - Output : rent price al tempo t della parcel p nel neighborhood c.
   - Covariates : - Quantità di case nuove costruite "attorno" (perchè non nel nhood direttamente? Si potrebbe semplificare...)
                  - Fixed effect sul neighborhood (https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html)
                  - Fixed effect sull'anno
                  - Distanza dal "centro" (o studiamo luoghi importanti)
 
 Previsione sul numero di sfratti : n_sfratti_monthly ~ avg_monthly_price_nhood + new_constructions_nhood  ... bisogna inventarsi qualcosa di meglio...

4) Spatial Statistics
5) Survival Analysis su quanto tempo uno sta nella casa


