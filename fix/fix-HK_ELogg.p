DEFINE VARIABLE cTabellNavn AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.

ASSIGN cTabellNavn = "hgrdag,timedag,varedag,akt_rapp,dags_rap," +
                     "kas_rap,Kort_Spes,KassererBilag,KassererKontanter," +
                     "kassererOppgj,KassererValuta,BokforingsBilag,StLinje".
/* exportrutinen är i stort sett förberedd för att ta hand om */
/* konto och kassererDag men för tillfället skall de inte överföras */
FOR EACH ELogg WHERE ELogg.EksterntSystem = "HK":
    DELETE ELogg.
END.
DO iCount = 1 TO NUM-ENTRIES(cTabellNavn):
    CREATE Elogg.
    ASSIGN ELogg.EksterntSystem = "HK"
           ELogg.TabellNavn     = ENTRY(iCount,cTabellNavn)
           ELogg.Verdier        = "ALLE"
           ELogg.EndringsType   = 1
           ELogg.Behandlet      = FALSE.
END.

