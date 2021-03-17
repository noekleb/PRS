DEFINE VARIABLE bMapping AS LOG NO-UNDO.
DEFINE VARIABLE bEntries AS LOG NO-UNDO.
DEFINE VARIABLE pcListe AS CHARACTER NO-UNDO.

/* ************************  Function Prototypes ********************** */
FUNCTION getMapping RETURNS LOGICAL 
  ( INPUT piButNr AS INTEGER, OUTPUT pcReturn AS CHARACTER ) FORWARD.


CURRENT-WINDOW:WIDTH = 350.

FOR EACH FakturaHode NO-LOCK WHERE 
    FakturaHode.FakturertDato >= 03/01/2020 AND 
    FakturaHode.FakturaNr > 0 AND 
    FakturaHode.Opphav = 20
    BY FakturaHode.FakturertDato
    BY FakturaHode.FakturaNr:
    
    FIND Kunde OF FakturaHode NO-LOCK NO-ERROR.
/*    IF NOT CAN-DO('1,20',STRING(Kunde.butikkNr)) THEN*/
    IF NOT CAN-DO('20',STRING(Kunde.butikkNr)) THEN
    DO:
        NEXT.        
    END.

    /* Mapper om data */
    MAPPING:
    DO:
      pcListe = ''.
      IF NOT getMapping( INPUT FakturaHode.ButikkNr, OUTPUT pcListe ) THEN 
      DO:
        bMapping = FALSE.
      END.
      ELSE 
        bMapping = TRUE.
        
      IF NUM-ENTRIES(pcListe) <> 5 THEN
      DO: 
        bEntries = FALSE.
      END.
      ELSE 
        bEntries = TRUE.
    END. /* MAPPING */  
    
    DISPLAY
        FakturaHode.FakturertDato
        FakturaHode.FakturaNr
        FakturaHode.Opphav
        FakturaHode.butikkNr
        FakturaHode.KundeNr
        Kunde.ButikkNr WHEN AVAILABLE Kunde
        FakturaHode.EDato
        STRING(FakturaHode.ETid,"HH:MM:SS") FORMAT "x(12)"
        '|'
        FakturaHode.EksportertDato
        FakturaHode.EksportertAv
        bMapping
        bEntries
    WITH WIDTH 350.
END.






/* ************************  Function Implementations ***************** */
FUNCTION getMapping RETURNS LOGICAL 
  ( INPUT piButNr AS INTEGER, OUTPUT pcReturn AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/  

    DEFINE VARIABLE bResult      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pcEDB-System AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcTabell     AS CHARACTER NO-UNDO.

    DEFINE BUFFER bImpKonv FOR ImpKonv.
    
    ASSIGN 
      pcEDB-System = 'Gant Global'
      pcTabell     = 'Regnskapsavd'
      .
      
    MAPPINGBLOKK:
    DO FOR bImpKonv:
      FIND FIRST bImpKonv NO-LOCK WHERE 
        bImpKonv.EDB-System = pcEDB-System AND 
        bImpKonv.Tabell     = pcTabell AND 
        bImpKonv.InterntId = TRIM(STRING(piButNr)) NO-ERROR.
      IF AVAILABLE bImpKonv THEN
      DO: 
        ASSIGN 
          pcReturn = bImpKonv.EksterntId
          bResult = TRUE
          .
      END.
    END. /* MAPPINGBLOKK */

    RETURN bResult.
    
END FUNCTION.
