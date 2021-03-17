
/*------------------------------------------------------------------------
    File        : sjekkDataMottak.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Thu Feb 27 12:46:54 CET 2020
    Notes       : Starter opp, sjekker og sender melding hvs det er mer 
                  enn 5 min siden siste kjøring. 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSendLst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX          AS INTEGER   NO-UNDO.
DEFINE VARIABLE iLoop       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxTid     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDager      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTid        AS INTEGER   NO-UNDO.
DEFINE VARIABLE dtSistKjort AS DATETIME  NO-UNDO.
DEFINE VARIABLE iDiff       AS INTEGER   NO-UNDO.
DEFINE VARIABLE ceMailLst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest       AS LOG       NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

{syspara.i 50 50 51 ceMailLst}

ASSIGN
    bTest   = TRUE 
    cLogg   = 'sjekkDataMottak' + REPLACE(STRING(TODAY),'/','')
    iMaxTid = 60 * 5 * 2 /* 10 min */
    .

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).

SJEKKBLOKK:
DO:
  ASSIGN 
    dtSistKjort = ?
    iDager      = 0
    iTid        = 0
    .
    
  DO ON ERROR UNDO, LEAVE:
    {syspara.i 200 1 3 dtSistKjort DATETIME}
  END.
  IF dtSistKjort = ? THEN 
    LEAVE SJEKKBLOKK.

  {syspara.i 200 1 3 cTekst}
    
  ASSIGN 
    iDager = TODAY - DATE(dtSistKjort)
    cTekst = ENTRY(2,cTekst,' ')
    NO-ERROR.
  IF NUM-ENTRIES(cTekst,':') = 3 THEN 
  DO iLoop = 1 TO 3:
    ASSIGN 
      iTid = INT(ENTRY(1,cTekst,':')) * 60 * 60
      iTid = iTid + INT(ENTRY(2,cTekst,':')) * 60
      iTid = iTid + INT(ENTRY(3,cTekst,':'))
      iDiff = TIME - iTid
      .
  END.

  IF bTest THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  iDager : ' + STRING(iDager) 
        ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  iTid   : ' + STRING(iTid) 
        ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  iMaxTid: ' + STRING(iMaxTid) 
        ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  iDiff  : ' + STRING(iDiff) 
        ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Varsel : ' + (IF (iDiff > iMaxTid) OR iDager > 0 THEN 'Ja' ELSE 'Nei') 
        ).
  END.
  
  IF iDager > 0 THEN 
    RUN sendVarselDataMottak (1).
  ELSE IF (iDiff) > iMaxTid THEN  
    RUN sendVarselDataMottak (2).

  LEAVE SJEKKBLOKK.
  
END. /* SJEKKBLOKK */  

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).

QUIT.

/* **********************  Internal Procedures  *********************** */

PROCEDURE sendVarselDataMottak:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER piType AS INTEGER NO-UNDO.

    IF ceMailLst = '' THEN 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Det er ikke satt opp mottager for eMail varsel DataMottak (Syspara 50 50 51).' 
          ).
      RETURN.
    END.
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Mailvarsel sendt: ' + STRING(NOW,"99/99/9999 HH:MM:SS") 
        ).

    rSendEMail:parToADDRESS       = ceMailLst.
    rSendEMail:parMailType        = 'PAKKSEDDEL'.
    rSendEMail:parSUBJECT         = "Sjekk av DataMottak fra butikkene " + 
                                    ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + ".".
    rSendEMail:parMessage-Charset = 'iso-8859-1'. /* Blank eller 'UTF-8' når det går fra fil. */
    rSendEMail:parFILE            = ''.  

    IF piType = 1 THEN
    DO: 
      cTekst = "Det er " + STRING(iDager)  + " dager siden sisten innlesning av bongdata er kjørt. ".
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  ' + cTekst 
          ).
      rSendEMail:parMESSAGE =  cTekst.
    END.
    ELSE IF pitype = 2 THEN 
      rSendEMail:parMESSAGE =  "Det er mer enn " + STRING(ROUND(iDiff / 60,0))  + " minutter siden sisten innlesning av bongdata er kjørt. ".

    rSendEMail:send( ).

END PROCEDURE.


