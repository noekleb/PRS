/* To invoke: 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"CustomUpdateValProc","=update_validation.p").
   Procedure name must be prefixed by = or +
   =:   Dynamic validation is suppressed
   +:   The validation proc is run in addition to dynamic validation
   Dynamic (automatic) validation is done before custom val (ref doc/html/fieldMap.html for rules).
   
   If there's no fieldmap (viewer) set the attribute on the browse object
 
 Endret: 22.04.13 av Brynjar
       - Dersom kunde er endret så kan det hende at mva skal regnes om dersom kunde ikke er mva-registrert
         Dette styres ved at context settes til "recalc_linjer" som sjekkes i postUpdateProc (kordre_post_update.p)
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

DEF BUFFER bKunde FOR kunde.

DEF VAR fKundenr AS DEC NO-UNDO.

FIND KOrdreHode WHERE ROWID(KOrdreHode) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL KOrdreHode THEN DO:
  fKundeNr = DEC(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"KundeNr")).  
  FIND FIRST Kunde NO-LOCK
       WHERE Kunde.KundeNr = fKundenr
       NO-ERROR.
  IF NOT AVAIL Kunde THEN
    ocReturn = "Ugyldig kundenr".
  ELSE IF KOrdreHode.KundeNr NE fKundenr THEN DO:
    FIND FIRST bKunde NO-LOCK
         WHERE bKunde.KundeNr = KOrdreHode.KundeNr
         NO-ERROR.
    IF AVAIL bKunde AND bKunde.MvaFri NE Kunde.MvaFri THEN
      DYNAMIC-FUNCTION("setContext" IN SOURCE-PROCEDURE,"recalc_linjer").
  END.
END.


