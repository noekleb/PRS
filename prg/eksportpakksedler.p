&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksportpakksedler.p
    Purpose     : Eksport av pakkseddler fra butikk til hk.
                  Eksporterer alle pakkseddler.

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     :
    Notes       : varenummer, strekkode, pris, antall
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    
&ELSE
    
&ENDIF

DEFINE        VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

DEFINE VARIABLE ocRetur     AS CHARACTER  NO-UNDO.
DEF VAR iAntLinjer          AS INT     NO-UNDO.
DEF VAR iAlle               AS INT     NO-UNDO.
DEF VAR bStream             AS LOG     NO-UNDO.
DEFINE VARIABLE lDec        AS DECIMAL NO-UNDO.
DEFINE VARIABLE bManuell    AS LOG     NO-UNDO.
DEFINE VARIABLE dFraDato    AS DATE    NO-UNDO.
DEFINE VARIABLE dTilDato    AS DATE    NO-UNDO.
DEFINE VARIABLE pdLoopDato  AS DATE    NO-UNDO.
DEFINE VARIABLE lTid        AS INTEGER NO-UNDO.
DEFINE VARIABLE cLinje      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCL         AS INTEGER NO-UNDO.
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


{syspara.i 1 1 61 cKatalog}
IF cKatalog = '' THEN 
DO:
  {syspara.i 1 1 51 cKatalog}
  IF cKatalog = '' THEN
    cKatalog = 'c:\home\lindbak\sendes'. 
END.

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
DEFINE VARIABLE cSistSolgtDato  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVVareKost      AS CHARACTER NO-UNDO.

DEFINE BUFFER bufPkSdlHode FOR PkSdlHode.

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cFilNavn    = cKatalog + '\' + 'POSPK' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpPOSPK' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + STRING(iButikkNr)).

RUN bibl_logg.p ('eksporter_varetelling', 'eksportvaretelling.p Butikk: ' + string(iButikkNr)+ ' Fil: ' + string(cFilNavn) + STRING(iButikkNr)).

EXPORT STREAM Ut
    'Butik;'
    'SendtDato;'
    'PksdlStatus;'
    'VismaNr (ArtNr+StrKode);'
    'EAN;'
    'Varekost;'
    'Pris;'
    'Antall;'
    'Verdi;'
    'Pakkseddelnummer;'
    'Leverandør;'
    'MvaKode'
    SKIP.

PAKKSEDDEL:
FOR EACH PkSdlHode NO-LOCK WHERE 
  PkSdlHode.SendtDato >= 01/01/2012 AND 
  PkSdlHode.MeldingFraLev = '',
  EACH PkSdlLinje OF PkSdlHode NO-LOCK,
  EACH PkSdlPris OF PkSdlHode WHERE 
    PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-LOCK,
  FIRST ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr =  PkSdlLinje.ArtikkelNr,
  FIRST VarGr NO-LOCK WHERE VarGr.Vg = ArtBas.Vg:
      
  PUT STREAM Ut UNFORMATTED
    PkSdlLinje.ButikkNr ';'
    PkSdlHode.SendtDato ';'
    PkSdlHode.PkSdlStatus ';'
    STRING(PkSdlLinje.ArtikkelNr) + TRIM(STRING(PkSdlLinje.StrKode,">999")) ';'
    PkSdlLinje.Kode ';'
    PkSdlPris.NyVarekost ';'
    PksdlPris.NyPris ';'
    (IF PkSdlHode.PkSdlStatus = 10 THEN PkSdlLinje.Antall ELSE PkSdlLinje.AntLevert) ';'
    (IF PkSdlHode.PkSdlStatus = 10 THEN PkSdlLinje.Antall * PkSdlPris.NyVarekost ELSE PkSdlLinje.AntLevert * PkSdlPris.NyVarekost) ';'
    PkSdlHode.PkSdlNr ';'
    ArtBas.LevNr ';'
    VarGr.MomsKod
  SKIP.
  iAnt = IAnt + 1.
  
  DO TRANSACTION:
    FIND bufPkSdlHode EXCLUSIVE-LOCK WHERE RECID(bufPkSdlHode) = RECID(PkSdlHode) NO-ERROR.
    IF AVAILABLE bufPkSdlHode THEN 
    DO:
      ASSIGN bufPkSdlHode.MeldingFraLev = 'Sendt HK ' + STRING(TODAY).
      RELEASE bufPkSdlHode.
    END.
  END.        
END. /* PAKKSEDDEL */        
    
OUTPUT STREAM Ut CLOSE.

/* Gir filen dens riktige navn og tar bort den temporære filen. */
OS-COPY value(ctmpFilNavn + STRING(iButikkNr)) value(cFilNavn + STRING(iButikkNr)).
IF SEARCH(cFilNavn + STRING(iButikkNr)) <> ? THEN
    OS-DELETE VALUE(ctmpFilNavn + STRING(iButikkNr)).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
