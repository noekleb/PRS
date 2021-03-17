
/*------------------------------------------------------------------------
    File        : asSendLinkViaSMSTilDinSide.p
    Purpose     : Enkelt kall som skal benyttes fra POS får å verve kunde  

    Syntax      :

    Description : Rutinen tar imot et mobilnr, henter URL'en til Minside, og sender denne på SMS til kunden.

    Author(s)   : Tom Nøkleby
    Created     : Wed Nov 18 17:07:44 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER cMobilNr AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER bOk AS LOG NO-UNDO.

DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR cURLMinside AS CHAR NO-UNDO.
DEF VAR cMsgs    AS CHAR NO-UNDO.
DEF VAR cEmne    AS CHAR NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{syspara.i 14 303 6 cURLMinside}

IF cMobilNr = '' THEN 
  DO:
    bOk = FALSE.
    RETURN.
  END.

ASSIGN 
    cEmne    = 'GANT Exclusive'
    cMsgs    = 'Legg inn dine data via GANT Exclusive registrerings side' + CHR(10) + 
               cURLMinside + CHR(10) + 
               STRING(NOW,"99/99/9999 HH:MM:SS")
/*    cMobilNr = '+4748004066' /* Are */ */
/*    cMobilNr = '+4741223320' /* Ken1 */*/
/*    cMobilNr = '+4741365436' /* Tom */ */
    .

RUN sendSMSTilMobil.p (cEmne + '|' +
               cMsgs + '|' +
               cMobilNr,
               ?,
               '',
               OUTPUT cReturn,
               OUTPUT bOk).
