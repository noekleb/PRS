
/*------------------------------------------------------------------------
    File        : eksporterPUBArtbas_run.p 
    Purpose     : 

    Syntax      :
 
    Description : Eksport av prisinformasjon hos PUB/Jarmeus.

    Author(s)   : tomn
    Created     : Fredag jan 01 2010
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE VAR cLanButiker     AS CHARACTER  NO-UNDO.
DEFINE VAR cFtpButiker     AS CHARACTER  NO-UNDO.
DEFINE VAR cVareFiler      AS CHARACTER  NO-UNDO.
DEFINE VAR cMixFiler       AS CHARACTER  NO-UNDO.
DEFINE VAR iAntVarer       AS INTEGER    NO-UNDO.
DEFINE VAR iAntPakker      AS INTEGER    NO-UNDO.

DEFINE VARIABLE obOk      AS LOG       NO-UNDO.
DEFINE VARIABLE ocMelding AS CHAR      NO-UNDO.
DEFINE VARIABLE cButNr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTekst    AS CHARACTER NO-UNDO.

ASSIGN
  cLanButiker = '1'
  cFtpButiker = ''.

/* Henter oppkoblingsinfo fra oppstartsicon. */
IF SESSION:PARAMETER <> "" THEN 
DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
    IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "BUTLST" AND 
        NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN 
    DO:
        ASSIGN 
            cLanButiker = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
    END.
END.

RUN eksportPUBArtBas.p (INPUT cLanButiker,INPUT cFtpButiker,OUTPUT cVareFiler,OUTPUT cMixFiler,OUTPUT iAntVarer,OUTPUT iAntPakker).

