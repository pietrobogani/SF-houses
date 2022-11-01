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


0) Evidenziare il problema: - funzione avg_monthly_price vs time : confrontare tra diverse città (con crescita di avg_income simile)? Servono i dati ... =(
                            - guardare come cambiano avg_monthly_price vs avg_monthly_income per dire che la gente povera se ne va(?). Servono i dati =(

1) IDEA: RENT CRESCENTI => GENTRIFICAZIONE CRESCENTE
   Osservare la crescita dei prezzi dei rent nei vari quartieri al passare del tempo, fare così ipotesi su
   Displacement/gentrificazione da poi verificare (si spera) alla fine dello studio.
   Possiamo: - Per ogni istante di tempo che abbiamo fare un ANOVA test, i gruppi sono i quartieri, la variabile il prezzo al mq;
             - Attraverso una funzione avg_monthly_price in nhood vs time, verificare in quali quartieri il prezzo è cresciuto nel tempo
             - Forse è possibile racchiudere in un colpo solo i due punti sopra in una ANOVA two-way con i quartieri come 1st way e gli anni come 2nd way
                                 
2) IDEA: NUOVE COSTRUZIONI IN UNA CERTA ZONA => GENTRIFICAZIONE IN ATTO/PREVISTA NELLA STESSA ZONA
   I quartieri dove viene costruito di più sono quelli dove ci si aspetta i prezzi saliranno maggiormente (i.e. dove c'è 
   displacement/gentrification). Si potrebbe anche in questo modo ottenere una lista di quartieri più soggetti a displacemente e poi verificare (si spera)
   alla fine dello studio.
   Tom: io controllerei prima se "costruire di più" implica "aumento prezzi". 
        Magari si può: - fare funzioni new_monthly_construction_nhood vs time
                       - fare funzioni avg_monthly_price_nhood vs time
                       - fare funzioni evictions_monthly_nhood vs time 
                       - vedere se valori alti di new_construction_nhood corrispondono ad aumenti di prezzi negli anni successivi
                       - come usare l'informazione sugli sfratti?? Più prezzi alti, più sfratti mi aspetto??  
3) IDEA: EVICTIONS/BUYOUTS CRESCENTI => GENTRIFICAZIONE CRESCENTE
   Controllo semplicemente in quali quartieri ho il maggior aumento di evictions/buyouts
3) Regression model: 
   - Output : rent price al tempo t della parcel p nel neighborhood c.
   - Covariates : - Quantità di case nuove costruite "attorno" (perchè non nel nhood direttamente? Si potrebbe semplificare...)
                  - Fixed effect sul neighborhood (https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html)
                  - Fixed effect sull'anno
                  - Distanza dal "centro" (o studiamo luoghi importanti)
 
 Previsione sul numero di sfratti : n_sfratti_monthly ~ avg_monthly_price_nhood + new_constructions_nhood  ... bisogna inventarsi qualcosa di meglio...

4) Spatial Statistics
5) Survival Analysis su quanto tempo uno sta nella casa


