
/*------------------------------------------------------------------------
    File        : artbas_Lagerkoder.p
    Purpose     : Legger til manglende lagerkoder i listen.

    Syntax      :

    Description : Beregner totaler for ubehandlede kundeordre.

    Author(s)   : tomn
    Created     : Sun Sep 29 11:18:08 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLagerkoder AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
  cLagerkoder = ENTRY(1,icParam,'|')
  .

FOR EACH ArtBas NO-LOCK WHERE 
  ArtBas.LagerKoder > '' AND 
  CAN-FIND(FIRST ArtLag WHERE 
           ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND 
           ArtLAg.Butik = 16):
 
  DO iLoop = 1 TO NUM-ENTRIES(ArtBas.LagerKoder):
    cTekst = TRIM(ENTRY(iLoop,ArtBas.LagerKoder)).
    
    IF LOOKUP(cTekst,cLagerkoder) = 0 THEN 
      cLagerkoder = cLagerkoder + 
                    (IF cLagerKoder = '' THEN '' ELSE ',') + 
                    cTekst + ',' + cTekst.
    
  END.
    
END.

ASSIGN 
  obOk = TRUE
  ocReturn = cLagerkoder
  .