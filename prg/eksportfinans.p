&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksporsalg.p
    Purpose     : Eksport av salg fra butikk til hk.
                  Eksporterer alle salg pr. dag pr. størrelse siste 10 dager. 

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

DEFINE INPUT  PARAMETER dinpFraDato AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER dinpTilDato AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER ocRetur     AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

DEF VAR iAntLinjer          AS INT     NO-UNDO.
DEF VAR iAlle               AS INT     NO-UNDO.
DEF VAR bStream             AS LOG     NO-UNDO.
DEFINE VARIABLE lDec        AS DECIMAL NO-UNDO.
DEFINE VARIABLE bManuell    AS LOG     NO-UNDO.
DEFINE VARIABLE dFraDato    AS DATE    NO-UNDO.
DEFINE VARIABLE dTilDato    AS DATE    NO-UNDO.
DEFINE VARIABLE pdLoopDato  AS DATE    NO-UNDO.
DEFINE VARIABLE lTid        AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntallKnd  AS INTEGER NO-UNDO.     
DEFINE VARIABLE lRabattKr   AS DECIMAL NO-UNDO.  
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

{manedsrapport_tmptabell.i &NEW = "NEW" &SHARED = "SHARED"}
DEF BUFFER tmptotManedsrap FOR tmpManedsrap.

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


{syspara.i 1 1 61 cKatalog}
IF cKatalog = '' THEN 
DO:
  {syspara.i 1 1 51 cKatalog}
  IF cKatalog = '' THEN
    cKatalog = 'c:\home\lindbak\sendes'. 
END.

ASSIGN lTid = TIME.

IF dinpFraDato = ? OR dinpTilDato = ? THEN 
DO: 
  RUN SettFraTilDato.
  RUN bibl_logg.p ('eksporter_finans', 'eksportfinans.p: AUTO WinCheduler Periode: ' + string(dinpFraDato) + ' - ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
END.
ELSE DO:
  ASSIGN 
    dFraDato = dinpFraDato
    dTilDato = dinpTilDato
    bManuell = TRUE. /* Flagger at eksporten kjøres manuelt og at det ikke skal sendes eMail. */
  RUN bibl_logg.p ('eksporter_finans', 'eksportfinans.p: MANUELL Periode: ' + string(dinpFraDato) + ' - ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
END.

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
DEFINE VARIABLE iButikkNr       AS INT  NO-UNDO.
DEFINE VARIABLE cFilNavn        AS CHAR NO-UNDO.
DEFINE VARIABLE iAnt            AS INT  NO-UNDO.
DEFINE VARIABLE cSistSolgtDato  AS CHARACTER NO-UNDO.

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cKopi       = cKatalog + "\purpleo"
    cFilNavn    = cKatalog + '\' + 'POSD' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpPOSD' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

BUTIKKLOOP:
FOR EACH Butiker NO-LOCK WHERE
  Butiker.harButikksystem = TRUE AND
  Butiker.ApningsDato     <> ? AND
  Butiker.NedlagtDato     = ?: 

/* Bygger temp-tabell */
RUN manedsrapport_bygg_tmptabell.p (STRING(Butiker.Butik) ,dFraDato, dTilDato).
IF RETURN-VALUE = "AVBRYT" THEN
  NEXT BUTIKKLOOP.
  
OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + STRING(Butiker.Butik)).

RUN bibl_logg.p ('eksporter_finans', 'eksportfinans.p Butikk: ' + string(Butiker.Butik)+ ' Fil: ' + string(cFilNavn) + STRING(Butiker.Butik)).

PUT STREAM Ut UNFORMATTED 
    'Butik;'
    'Dato;'
    'AntallKnd;'
    'InnVerdiKr;'
    'MvaKr;'
    'UtverdiKr;'
    'KreditsalgKr;'
    'KortSalgKr;'
    'RabattKr'
    SKIP.

UTLEGG_FINANS:
FOR EACH tmpManedsrap WHERE
    tmpManedsrap.ButikkNr > 0
    BREAK BY tmpManedsrap.ButikkNr
          BY tmpManedsrap.Dato:

    PUT STREAM Ut UNFORMATTED
      tmpManedsrap.ButikkNr ';'    
      tmpManedsrap.Dato ';'        
      tmpManedsrap.AntallKunder ';'     
      (tmpManedsrap.OmsEksMvaGrp1 + 
       tmpManedsrap.OmsEksMvaGrp2 +  
       tmpManedsrap.OmsEksMvaGrpDiv) ';' /*  InnVerdiKr */    
      (tmpManedsrap.MvaKrGrp1 + 
       tmpManedsrap.MvaKrGrp2 +  
       tmpManedsrap.MvaKrGrpDiv) ';' /* MvaKr */      
      (tmpManedsrap.OmsetningMvaGrp1 + 
       tmpManedsrap.OmsetningMvaGrp2 +  
       tmpManedsrap.OmsetningMvaGrpDiv) ';' /*  UtverdiKr */
      tmpManedsrap.Kreditsalg ';' 
      (tmpManedsrap.BankKort +               
       tmpManedsrap.Visa  +             
       tmpManedsrap.Eurocard +              
       tmpManedsrap.Amex +             
       tmpManedsrap.Diners +                
       tmpManedsrap.SenterGavekort +         
       tmpManedsrap.DiverseKort) ';' /* Kortsalg */ 
      tmpManedsrap.RabattKr   
      SKIP.

    DELETE tmpManedsrap.
    iAnt = IAnt + 1.
    
END. /* UTLEGG_FINANS */
    
OUTPUT STREAM Ut CLOSE.

    /* Gir filen dens riktige navn og tar bort den temporære filen. */
    OS-COPY value(ctmpFilNavn + STRING(Butiker.Butik)) value(cFilNavn + STRING(Butiker.Butik)).
    IF SEARCH(cFilNavn + STRING(Butiker.Butik)) <> ? THEN
        OS-DELETE VALUE(ctmpFilNavn + STRING(Butiker.Butik)).

    /* Sikrer at backup katalog finnes. */
    OS-CREATE-DIR value(cKopi).

    /*
    /* Legger en kopi av filen i egen katalog. */
    OS-COPY value(cFilNavn + STRING(Butiker.Butik)) 
            value(cKopi).
    */
    /* Appender en kopi av filen inn i en felles fil. */
    /* TN 26/1-11 Ref. mail fra Kåre. De trenger ikke finansdata.
    cTekst = ENTRY(NUM-ENTRIES(cFilNavn,'\'),cFilNavn,'\').
    IF NUM-ENTRIES(cTekst,'.') > 0 THEN
    DO: 
      ENTRY(2,cTekst,'.') = 'txt'.
      IF SEARCH(cKopi + '\' + cTekst) = ? THEN 
        OS-COMMAND SILENT VALUE('TYPE '+ cFilNavn + STRING(Butiker.Butik) + ' >> ' + cKopi + '\' + cTekst).
      ELSE DO:
        INPUT STREAM Inn FROM VALUE(cFilNavn + STRING(Butiker.Butik)) NO-ECHO.
        OUTPUT STREAM Ut TO VALUE(cKopi + '\' + cTekst) APPEND NO-ECHO.
        
        REPEAT:
          IMPORT STREAM Inn UNFORMATTED cLinje.
          IF cLinje BEGINS '"Butik;' THEN NEXT.
          ELSE PUT STREAM Ut UNFORMATTED cLinje SKIP.
        END.
        
        OUTPUT STREAM Ut CLOSE.
        INPUT STREAM Inn CLOSE.
      END.
    END.
    */
END. /* BUTIKKLOOP */
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SettFraTilDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettFraTilDato Procedure 
PROCEDURE SettFraTilDato :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE pdDato AS DATE NO-UNDO.

{syspara.i 210 202 1 pcTekst}
ASSIGN
  pdDato = DATE(pcTekst) NO-ERROR.
IF ERROR-STATUS:ERROR OR pcTekst = '' 
THEN ASSIGN pdDato = TODAY.
ELSE ASSIGN pdDato = pdDato + 1.

ASSIGN
  dFraDato    = pdDato - 10
  dTilDato    = (IF pdDato > TODAY THEN pdDato ELSE TODAY)
  dinpFraDato = dFraDato
  dinpTilDato = dTilDato
  .

DO TRANSACTION:
  {setsyspara.i 210 101 1 STRING(dTilDato)}
  IF AVAILABLE SysPara THEN 
    RELEASE SysPara.
END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

