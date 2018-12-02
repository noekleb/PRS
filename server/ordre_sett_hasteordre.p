/* Sett ordre og tilhørende bestillinger som hasteordre, dvs at leveringsdato 
   settes som dagens dato.
   Parametere: 
               ROWID(ordre)
                    
   Opprettet: 27.04.07 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.
  

FIND Ordre WHERE ROWID(Ordre) = TO-ROWID(icParam)
     EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL Ordre THEN DO:
  FOR EACH BestHode OF Ordre
      EXCLUSIVE-LOCK:
    BestHode.LevDato = TODAY.
  END.
  ASSIGN Ordre.Hasteordre    = YES
         Ordre.LeveringsDato = TODAY.
END.
ELSE ocReturn = "Kundeordre ikke tilgjengelig for oppdatering".

obOk = ocReturn = "".

