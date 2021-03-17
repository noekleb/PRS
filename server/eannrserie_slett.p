/* Kopier artikkel 
   Parametere: Aksjon (new|edit|copy),artikkelnr,prisprofil
               temp-tabell med overstyrte verdier for ny artikkel
   
   Opprettet: 17.11.04 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fEANSerieId   AS INT    NO-UNDO.
DEF VAR piLoop        AS INT    NO-UNDO.
DEF VAR cKode         AS CHAR   NO-UNDO.

ASSIGN 
    fEANSerieId = int(ENTRY(1,icParam)).

FIND EANNrSerie NO-LOCK WHERE
    EANNrSerie.EANSerieId = fEANSerieId NO-ERROR.
IF NOT AVAILABLE EANNrSerie THEN
DO:
    obOk = FALSE.
    ocReturn = '** Finner ikke EANNrSerie ' + STRING(fEANSerieId) + '.'.
    RETURN.
END.

FOR EACH EANNrListe OF EANNrSerie:
    DELETE EANNrListe.
END.
obOK = TRUE.
