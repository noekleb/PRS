/* Henter filnavn på fil til eksternt EDB system. 
   Parametere:  Navn på eksternt EDB system.
   Kommentar : 
   
   Opprettet: 8 nov 05 av Tom Nøkleby       
   
   TESTRUTINE SOM KAN KJØRES FRA EDITOR:
       /* TEST */
       {initjukebox.i}
       {incl/devmode.i}
       {incl/custdevmode.i}
       MESSAGE program-name(1) SKIP
         "TEST:" DYNAMIC-FUNCTION("runProc","get_ekstedbsys_filnavn.p","VISMA2",?)
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.

DEF VAR cFilNavn    AS CHAR NO-UNDO.
DEF VAR cFilKatalog AS CHAR NO-UNDO.
DEF VAR iSeqNr      AS INT  NO-UNDO.
DEF VAR cSeqNr      AS CHAR NO-UNDO.
DEF VAR iMaksSeq    AS INT  NO-UNDO.
DEF VAR cEDBSystem  AS CHAR NO-UNDO.

/* Stopper ugyldige verdier. */
IF icParam = "?" THEN
DO:
    ocRETURN = "Ukjent verdi i parameter (get_ekstedbsys_filnavn.p).".
    RETURN.
END.
ASSIGN
    cEDBSystem = icParam
    .

FIND EkstEDBSystem NO-LOCK WHERE
    EkstEDBSystem.EDBSystem = cEDBSystem NO-ERROR.
IF NOT AVAILABLE ekstEDBSystem THEN DO:
    ocRETURN = "Ukjent EDB system " + cEDBSystem + " sendt til (get_ekstedbsys_filnavn.p).".
    RETURN.
END.

/* Til filnavn er unikt */
UNIKT:
DO WHILE TRUE TRANSACTION:
    ASSIGN
        iMaksSeq    = ekstEDBSystem.MaksSeq
        iSeqNr      = ekstEDBSystem.SeqNr + 1
        iSeqNr      = IF iSeqNr > iMaksSeq
                       THEN 1
                       ELSE iSeqNr
        cSeqNr      = SUBSTRING(STRING(iSeqNr,"99999999"),9 - LENGTH(STRING(iMaksSeq)),8)
        cFilKatalog = ekstEDBSystem.FilKatalog
        cFilNavn    = ekstEDBSystem.FilPrefix + 
                      (IF ekstEDBSystem.SeqvAktiv 
                         THEN cSeqNr
                         ELSE "") + 
                     (IF ekstEDBSystem.FilEkstent <> "" 
                         THEN "."
                         ELSE "") + 
                     ekstEDBSystem.FilEkstent
        obOk = TRUE
        NO-ERROR.
    FIND CURRENT ekstEDBSystem EXCLUSIVE-LOCK.
    ASSIGN
        ekstEDBSystem.SeqNr = iSeqNr
        .
    FIND CURRENT ekstEDBSystem EXCLUSIVE-LOCK.

    /* Sekvensnummer inngår ikke i filnavn. */
    IF ekstEDBSystem.SeqvAktiv = FALSE THEN
        LEAVE UNIKT.
    /* Sekvensnr inngår. Da skal filnavn være unikt. */
    IF SEARCH(cFilKatalog + "\" + cFilNavn) = ? THEN
        LEAVE UNIKT.
END. /* UNIKT */

/* TEST */
/* OUTPUT TO VALUE(cFilKatalog + "\" + cFilNavn). */
/* EXPORT "Gurre" TODAY STRING(TIME,"HH:MM:SS").  */
/* OUTPUT CLOSE.                                  */

IF obOk THEN
    ocReturn = cFilKatalog + "|" + cFilNavn + "|" + ekstEDBSystem.FilPrefix.
ELSE
    ocReturn = "Klarte ikke å lage filnavn.".

