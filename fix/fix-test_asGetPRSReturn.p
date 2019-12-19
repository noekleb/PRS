DEFINE VAR lcShipping  AS LONGCHAR NO-UNDO.
DEFINE VAR obOk     AS LOG      NO-UNDO.
DEFINE VAR ocReturn AS CHAR     NO-UNDO. 
DEF VAR lKORdre_Id AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR cTabellNavn AS CHAR NO-UNDO.

ASSIGN 
    lKOrdre_Id = 1190000012
    .
FIND KORdrEHode EXCLUSIVE-LOCK WHERE 
    KOrdrEHode.KOrdre_Id = lKOrdre_Id NO-ERROR.

IF AVAILABLE KOrdreHode THEN
WEBBUTIKK:
DO:
    ASSIGN 
        KOrdrEHode.ShipmentSendt = ?
        .

    cTabellNavn = IF KordreHode.EkstOrdreNr MATCHES '*RETUR*'
                          THEN "RETURKOrdreHode"
                          ELSE "KOrdreHode".
    
   /* Rydder litt. */
   FOR EACH ELogg WHERE 
       ELogg.TabellNavn = cTabellNavn:
       DELETE ELogg.
   END.

   /* Skaper Elogg på test ordren. */
    FIND ELogg EXCLUSIVE-LOCK WHERE 
         ELogg.TabellNavn     = cTabellNavn AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(KOrdreHode.KOrdre_Id) NO-ERROR NO-WAIT.
    IF LOCKED ELogg THEN 
        LEAVE WEBBUTIKK.
    ELSE IF NOT AVAIL Elogg THEN 
    DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = cTabellNavn
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(KOrdreHode.KOrdre_Id)
               .
        RELEASE ELogg.
    END.
    ELSE DO:
        ASSIGN ELogg.EndringsType = 1 
               ELogg.Behandlet    = FALSE.
        RELEASE ELogg.
    END. 
    RUN asGetPRSReturn.p (OUTPUT lcShipping,
                          OUTPUT obOk,
                          OUTPUT ocReturn).

    MESSAGE STRING(lcShipping)
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END. /* WEBBUTIKK */
ELSE 
    MESSAGE 'kjent kundeordre' lKOrdre_Id
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
