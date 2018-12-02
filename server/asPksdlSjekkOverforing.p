/*------------------------------------------------------------------------
    File        : asPksdlSjekkOverforing.p
    Purpose     : Sjekker fra kassen om det kan angres på en pakkseddel.

    Syntax      : asPksdlSjekkOverforing.p (lPkSdlNr, iButNr, OUTPUT cOKButLst, output cOKButNavn, output lOk, OUTPUT cReturn).

    Description : Sjekker om angitt pakkseddel kan angres. Hvis den kan det, hentes liste over butikker den kan oveføres til.

    Author(s)   : Tom Nøkleby
    Created     : Wed May 10 10:55:04 CEST 2017
    Notes       :  
  ----------------------------------------------------------------------*/


DEFINE INPUT  PARAMETER lPkSdlId    AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER iButNr      AS INTEGER   NO-UNDO.
 
DEFINE OUTPUT PARAMETER cOKButLst   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cOKButNavn  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER iOrgOvbut   AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER lOK         AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER cReturn     AS CHARACTER NO-UNDO.

DEFINE VARIABLE cIkkePlussMinus AS CHARACTER NO-UNDO.

/* Ved angring av overføring, er det ikke aktuelt å overføre til +/- butikkene. */
{syspara.i 11 7 8 cIkkePlussMinus}

/* Sjekker om pakkseddel er gyldig og ikke mottatt, samt at det er butikken selv som er opphav til den. */
RUN PkSdlValiderAngre.p (lPkSdlId, iButNr, OUTPUT iOrgOvbut, OUTPUT lOk, OUTPUT cReturn).
IF lOk = FALSE THEN
    RETURN cReturn. 

/* Henter liste med butikker som denne butikken kan overføre til. */
RUN getOvButLst.p (iButNr, cIkkePlussMinus, OUTPUT cOKButLst, OUTPUT cOKButNavn, OUTPUT lOK, OUTPUT cReturn).
RETURN cReturn.    

