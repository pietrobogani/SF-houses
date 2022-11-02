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


0) Introduzione al problema: - Mostrare come a SF sia peggio che in altre città, ad esempio guardando il costo di un affitto al mq e paragonandolo ad altre città
                               (a parità di salario)

1) IDEA: RENT CRESCENTI => GENTRIFICAZIONE CRESCENTE
   Osservare la crescita dei prezzi dei rent nei vari quartieri al passare del tempo, fare così ipotesi su
   displacement/gentrificazione da poi verificare (si spera) alla fine dello studio.
   Possiamo: - Per ogni istante di tempo che abbiamo fare un ANOVA test, i gruppi sono i quartieri, la variabile il prezzo al mq;
             - Attraverso una funzione avg_monthly_price in nhood vs time, verificare in quali quartieri il prezzo è cresciuto nel tempo
             - Forse è possibile racchiudere in un colpo solo i due punti sopra in una ANOVA two-way con i quartieri come 1st way e gli anni come 2nd way
                                 
2) IDEA: EVICTIONS/BUYOUTS CRESCENTI => GENTRIFICAZIONE CRESCENTE
   Controllo semplicemente in quali quartieri ho il maggior aumento di evictions/buyouts. Potrebbe essere utile calcolare una funzione:
     f(q,t) := numero di evictions nel quartiere q nell'anno t
3) Regression model: 
   - Output : rent price al tempo t della parcel p nel neighborhood c.
   - Covariates : - Quantità di case nuove costruite "attorno" (perchè non nel nhood direttamente? Si potrebbe semplificare...)
                  - Fixed effect sul neighborhood (https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html)
                  - Fixed effect sull'anno
                  - Distanza dal "centro" (o studiamo luoghi importanti)
 
 Previsione sul numero di sfratti : n_sfratti_monthly ~ avg_monthly_price_nhood + new_constructions_nhood  ... bisogna inventarsi qualcosa di meglio...

4) Spatial Statistics
5) Survival Analysis su quanto tempo uno sta nella casa prima dell'eviction


