DEFINE OUTPUT PARAMETER dRabSjekkId LIKE MedRabSjekk.RabSjekkId NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER              NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE MedRabSjekk.RabSjekkId NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE MedRabSjekk.RabSjekkId NO-UNDO.
   
{syspara.i 1 1 30 cHkNumSerier}
   
/* TN 11/6-09 Create trigger logger artikkelnr i ArtikkelNr serie.
   Ved opprettelse av artikler på HK, benyttes en annen artikkelnrserie.
   Det er derfor ikke nødvendig å legge inn opprettelse av
   poster i ArtikkelNrSerie her.
*/   
   
NUMMERLOOP:
DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
  HKBLOKK:
  DO:     
    ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
           dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
    FIND LAST MedRabSjekk WHERE MedRabSjekk.RabSjekkId >= dFraNr AND
                             MedRabSjekk.RabSjekkId <= dTilNr 
                           USE-INDEX RabSjekk NO-LOCK NO-ERROR.

    /* Det ligger ingen artikler i noen av tabellene. */
    IF NOT AVAILABLE MedRabSjekk THEN 
    DO:
        ASSIGN dRabSjekkId = dFraNr.
        LEAVE HKBLOKK.
    END.
    /* Det ligger artikler bare i ArtBas */
    IF AVAILABLE MedRabSjekk THEN 
    DO:
        ASSIGN dRabSjekkId = MedRabSjekk.RabSjekkId + 1.
        LEAVE HKBLOKK.
    END.
  END. /* HKBLOKK */
  
  /* Tildelt nummer innenfor nummerserie. */
  IF dRabSjekkId <= dTilNr THEN 
    LEAVE NUMMERLOOP.
  ELSE dRabSjekkId = 0.
    
  /* Er vi på siste nummerserie og den er full. Gis melding */
  IF iCount = NUM-ENTRIES(cHkNumSerier) THEN
  DO:
      MESSAGE "RabattsjekkId nummerserie er full. Kontakt systemansvarlig." dRabSjekkId SKIP
          "HK nummeriserier:" cHkNumSerier
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      dRabSjekkId = 0.
      LEAVE NUMMERLOOP.
  END.
END. /* NUMMERLOOP */


