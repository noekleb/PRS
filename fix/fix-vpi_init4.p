DEF VAR ocReturn AS CHAR NO-UNDO.
DEF VAR obOk AS LOG NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE ihBuffer AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE tmpVPIArtBas NO-UNDO LIKE VPIArtBas
  INDEX VPIArtBas EkstVPILevNr VareNr BehStatus.

DISPLAY 'Størrelsestype i ArtBAs'.
PAUSE 0.
TEST:
FOR EACH ArtBas WHERE ArtBas.EDato = TODAY:
  FIND FIRST VPIArtBas NO-LOCK WHERE
    VPIArtBas.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
  IF AVAILABLE VPIArtBas THEN 
  DO:
    iAnt = iAnt + 1.  
    CREATE tmpVPIArtBas.
    BUFFER-COPY VPIArtBas TO tmpVPIArtBas NO-ERROR.
    IF ERROR-STATUS:ERROR AND AVAILABLE tmpVPIArtBAs 
      THEN DELETE tmpVPIArtBas.
  END.
END. /* TEST */


/*
DISPLAY 'Størrelsestype i VPIArtBAs'.
PAUSE 0.
FOR EACH VPIArtBas WHERE VPIArtBas.EDato = TODAY:

    iAnt = iAnt + 1.  
    CREATE tmpVPIArtBas.
    BUFFER-COPY VPIArtBas TO tmpVPIArtBas NO-ERROR.
    IF ERROR-STATUS:ERROR AND AVAILABLE tmpVPIArtBAs 
      THEN DELETE tmpVPIArtBas.
END.
*/

ihBuffer = BUFFER tmpVPIArtBas:HANDLE.
IF CAN-FIND(FIRST tmpVPIArtBas) THEN 
DO:
  RUN vpiartbas_send_til_butikk.p ('1' + ';|YES|NO|YES',ihBuffer,'',OUTPUT ocReturn, OUTPUT obOk).
END.

MESSAGE iAnt 'Artikler oppdatert og sendt til butikk.'
VIEW-AS ALERT-BOX.  