&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksporter_lager_kobling.p
    Purpose     : Eksport av koblingstabell mellom EAN og artikkel.

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    
&ELSE
    
&ENDIF

DEFINE OUTPUT PARAMETER ocRetur     AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

DEF VAR iAntLinjer          AS INT     NO-UNDO.
DEF VAR iAlle               AS INT     NO-UNDO.
DEF VAR bStream             AS LOG     NO-UNDO.
DEFINE VARIABLE lDec        AS DECIMAL NO-UNDO.
DEFINE VARIABLE pdLoopDato  AS DATE    NO-UNDO.
DEFINE VARIABLE lTid        AS INTEGER NO-UNDO.
DEFINE VARIABLE cLinje      AS CHARACTER NO-UNDO.

/* Filhåndtering */
DEF VAR cFilNavn   AS CHAR FORMAT "x(40)"     NO-UNDO.
DEFINE VARIABLE ctmpFilNavn AS CHARACTER NO-UNDO.
DEF VAR cKatalog   AS CHAR                    NO-UNDO.
DEF VAR cPrefix    AS CHAR                    NO-UNDO.
DEFINE VARIABLE cKopi    AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE bStreamApen  AS LOG NO-UNDO.

DEFINE STREAM Inn.
DEF STREAM Ut.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 1 1 51 cKatalog}
IF cKatalog = '' THEN
  cKatalog = 'c:\home\lindbak\sendes'.

ASSIGN lTid = TIME.

/* Legger ut data til fil. */
RUN Eksporter.

ocRetur = "OK," + String(iAntEksport) + cTekst.

lTid = TIME - lTid.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Eksporter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Eksporter Procedure 
PROCEDURE Eksporter :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE iButikkNr       AS INT       NO-UNDO.
DEFINE VARIABLE cFilNavn        AS CHAR      NO-UNDO.
DEFINE VARIABLE iAnt            AS INT       NO-UNDO.

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cFilNavn    = cKatalog + '\' + 'POSEAN' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpPOSEAN' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

LAGERLOOP:
FOR EACH Butiker NO-LOCK WHERE
  Butiker.ApningsDato     <> ? AND
  Butiker.NedlagtDato     = ?, 
  EACH Lager WHERE Lager.Butik = Butiker.Butik
  BREAK BY Butiker.Butik: 

  IF FIRST-of(Butiker.Butik) THEN 
  DO:
      OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + STRING(Butiker.Butik)).
  
      EXPORT STREAM Ut DELIMITER ';'
      'SE ArtikkelNr;'
      'StrKode;' 
      'VismaNr (ArtNr+StrKode);'
      'EAN;'
      'ERPNr;'
      'ModellNr;'
      'Varenavn;'
      'Farge;'
      'Størrelse'
      SKIP.
  END.
  
  FIND FIRST ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = Lager.ArtikkelNr NO-ERROR.

  IF AVAILABLE ArtBas THEN 
  EAN_KOBLING:
  FOR EACH Strekkode NO-LOCK WHERE
    Strekkode.ArtikkelNr = ArtBas.ArtikkelNr: 
    
    lDec = DEC(Strekkode.Kode) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      NEXT EAN_KOBLING.
    IF TRIM(Strekkode.Kode) = '' THEN 
      NEXT EAN_KOBLING.
    FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
  
    PUT STREAM Ut UNFORMATTED
        STRING(Strekkode.ArtikkelNr) ';'
        TRIM(STRING(Strekkode.StrKode,">>999")) ';'
        STRING(Strekkode.ArtikkelNr) + TRIM(STRING(Strekkode.StrKode,">>999")) ';'
        Strekkode.Kode ';'
        Strekkode.ERPNr  ';'    
        REPLACE(ArtBas.LevKod,';',',') ';'
        REPLACE(ArtBas.Beskr,';',',') ';'
        REPLACE(ArtBas.LevFargKod,';',',') ';'
        TRIM((IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ''))
        SKIP. 
         
  END. /* EAN_KOBLING */ 
  IF LAST-OF(Butiker.Butik) THEN
  DO: 
    OUTPUT STREAM Ut CLOSE.
    /* Gir filen dens riktige navn og tar bort den temporære filen. */
    OS-COPY value(ctmpFilNavn + STRING(Butiker.Butik)) value(cFilNavn + STRING(Butiker.Butik)).
    IF SEARCH(cFilNavn + STRING(Butiker.Butik)) <> ? THEN
        OS-DELETE VALUE(ctmpFilNavn + STRING(Butiker.Butik)).
  END.
END. /* LAGERLOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

