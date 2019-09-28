
/*------------------------------------------------------------------------
    File        : logglagerendringGoogleMerchant.p
    Purpose     : 

    Syntax      :

    Description : Lager lagerendringer som gjøres på web artikler.  

    Author(s)   : tomn
    Created     : Fri Sep 27 14:59:14 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER iButNr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER lArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
DEFINE INPUT PARAMETER iStrKode AS INT NO-UNDO.

DEFINE VARIABLE iAktiv AS INTEGER NO-UNDO.
DEFINE VARIABLE cKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{syspara.i 50 65 1 iAktiv INT}

/* Google Merchant Center integrasjon er ikke aktiv. */
IF iAktiv = 0 THEN 
  RETURN.
  
{syspar2.i 50 65 2 cKatalog}
{syspara.i 50 65 2 cFilNavn}
{syspara.i 50 65 4 cButLst}

/* Parametre er ikke satt opp. */
IF (cKatalog = '' OR 
    cFilNavn = '' OR 
    cButlst  = '') THEN 
  RETURN.
ELSE DO:
  ASSIGN 
    cKatalog = TRIM(cKatalog,'\')
    cKatalog = TRIM(cKatalog,'/')
    .
  /* Oppretter kataloger hvis de mangler. */
  cTekst = ''.
  DO iLoop = 1 TO NUM-ENTRIES(cKatalog):
    cTekst = cTekst +
             (IF cTekst <> '' THEN '\' ELSE '') +  
             ENTRY(iLoop,cKatalog).
    OS-CREATE-DIR VALUE(cTekst).
  END.
  ASSIGN 
    cKatalog = cKatalog + '\'
    .
END.

IF CAN-DO(cbutLst,STRING(iButNr)) THEN 
DO:    
  FOR EACH Strekkode NO-LOCK WHERE 
    Strekkode.ArtikkelNr = lArtikkelNr AND 
    Strekkode.StrKode    = iStrKode:
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas AND ArtBas.WebButikkArtikkel THEN 
    DO:
      OUTPUT STREAM ut TO VALUE(cKatalog + cFilNavn) APPEND.
      EXPORT STREAM ut DELIMITER ';' 
        StrekKode.Kode
        StrekKode.StrKode
        NOW
        .
      OUTPUT STREAM Ut CLOSE.
    END.  
  END.
END.  
  