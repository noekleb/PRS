
/*------------------------------------------------------------------------
    File        : fix-kundeordre_RETUR_bak_ordrenr.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Sun Jan 27 10:42:28 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cTekst AS CHARACTER FORMAT "x(30)" NO-UNDO.

ON WRITE OF kordrelinje OVERRIDE DO: END.
ON WRITE OF KOrdreHode  OVERRIDE DO: END.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
CURRENT-WINDOW:WIDTH = 350.

FOR EACH KORdreHode EXCLUSIVE-LOCK WHERE
    KOrdreHode.EkstOrdreNr BEGINS 'RETUR':
        
    
    
    IF NUM-ENTRIES(KOrdreHode.EkstOrdreNr,' ') > 1 THEN
    DO:
        ASSIGN 
            cTekst = ENTRY(2,KOrdreHode.EkstOrdreNr,' ') + 
                     ' ' + 
                     ENTRY(1,KOrdreHode.EkstOrdreNr,' ').

        DISPLAY
            KOrdreHode.EkstOrdreNr FORMAT "x(30)"
            KOrdreHode.EDato 
            cTekst
        WITH WIDTH 350.
    END.

END.
