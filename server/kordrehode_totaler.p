
/*------------------------------------------------------------------------
    File        : kordrehode_totaler.p
    Purpose     : Henter totaler som så kan vises frem i skjermbildet.

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

DEF VAR lTotalt AS DEC FORMAT "->>,>>>,>>>,>>9.99" NO-UNDO.
DEF VAR iAntall AS INT FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE iNetbut AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{syspara.i 150 1 2 iNetBut INT}

ASSIGN 
  iAntall = 0
  ltotalt = 0
  .
FOR EACH KOrdreHod NO-LOCK WHERE 
  KOrdreHode.ButikkNr = iNetbut AND 
  KOrdreHode.LevStatus >= '30' AND 
  KOrdreHode.LevStatus  < '50' AND 
  KOrdrEHode.Opphav = 10:
  ASSIGN
      iAntall = iAntall + 1
      ltotalt = ltotalt + KOrdreHode.Totalt
      .
END.
FOR EACH KOrdreHod NO-LOCK WHERE 
  KOrdreHode.ButikkNr = iNetbut AND 
  KOrdreHode.LevStatus = '55' AND 
  KOrdrEHode.Opphav = 10:
  ASSIGN
      iAntall = iAntall + 1
      ltotalt = ltotalt + KOrdreHode.Totalt
      .
END.

ASSIGN 
  obOk = TRUE
  ocReturn = STRING(iAntall) + '|' + STRING(lTotalt)
  .