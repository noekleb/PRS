&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksportlager_purpleo.p
    Purpose     : Eksport av lager fra butikk til hk.
                  Eksporterer alle lagerposter

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
DEFINE VARIABLE bManuell    AS LOG     NO-UNDO.
DEFINE VARIABLE pdLoopDato  AS DATE    NO-UNDO.
DEFINE VARIABLE lTid        AS INTEGER NO-UNDO.
DEFINE VARIABLE cLinje      AS CHARACTER NO-UNDO.

/* Filhåndtering */
DEF VAR cFilNavn   AS CHAR FORMAT "x(40)"     NO-UNDO.
DEFINE VARIABLE ctmpFilNavn AS CHARACTER NO-UNDO.
DEF VAR cKatalog   AS CHAR                    NO-UNDO.
DEF VAR cPrefix    AS CHAR                    NO-UNDO.
DEFINE VARIABLE cKopi       AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocReturn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bStreamApen AS LOG       NO-UNDO.
DEFINE VARIABLE cLevNrLst   AS CHARACTER NO-UNDO.

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

{syspara.i 210 202 2 cLevNrLst}
IF cLevNrLst = '' THEN 
  cLevNrLst = '68,103,55,779,1,759,18,43'. 

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
DEFINE VARIABLE cSistSolgtDato  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVVareKost      AS CHARACTER NO-UNDO.

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cKatalog    = cKatalog + "\purpleo"
    cFilNavn    = cKatalog + '\' + 'XPOSL' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpXPOSL' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

/* Sikrer at backup katalog finnes. */
OS-CREATE-DIR value(cKatalog).

OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + 'txt').

RUN bibl_logg.p ('eksporter_lager', 'eksporterLager.p Butikk: ' + 'txt' + ' Fil: ' + string(cFilNavn) + 'txt').

EXPORT STREAM Ut DELIMITER ';'
    'Butik;'
    'VismaNr (ArtNr+StrKode);'
    'EAN;'
    'ERPNr;'
    'ModellNr;'
    'Varenavn;'
    'Farge;'
    'Størrelse;'
    'Varegruppenr;'
    'Leverandørnr;'
    'Leverandørnavn;'
    'Beholdning;'
    'Beholdningsdato;'
    'Sist solgtdato;'
    'VVarekost'
    'MinAnt'
    'MaksAnt'
    'BestillAnt'
    SKIP.

LEV_ARTIKKEL_LOOP:
FOR EACH LevBas NO-LOCK WHERE
  CAN-DO(cLevNrLst,STRING(LevBas.LevNr)),  
  EACH ArtBas NO-LOCK WHERE 
    ArtBas.LevNr = LevBas.LevNr,
  FIRST ArtPris OF ArtBas NO-LOCK: 

BUTIKKLOOP:
FOR EACH Butiker NO-LOCK WHERE
  Butiker.ApningsDato     <> ? AND
  Butiker.NedlagtDato     = ?: 
  
  /* Butikker det ikke skal legges ut lager for. */
  IF CAN-DO('203,219,260,293,355,801',STRING(Butiker.Butik)) THEN 
    NEXT BUTIKKLOOP.
  
UTLEGG_LAGER:
FOR EACH Lager NO-LOCK WHERE
    Lager.ArtikkelNR = ArtBas.ArtikkelNr AND 
    Lager.Butik = Butiker.Butik,
    EACH ArtLag NO-LOCK WHERE
         ArtLag.ArtikkelNr = Lager.ArtikkelNr AND
         ArtLag.Butik      = Lager.Butik:

    FIND FIRST StrKonv NO-LOCK WHERE
        StrKonv.StrKode = ArtLag.StrKode NO-ERROR.

    /* Henter min/maks og bestillt antall */
    FIND ArtBestPkt NO-LOCK WHERE 
         ArtBestPkt.ArtikkelNr = ArtBas.ArtikkelNr AND 
         ArtBestPkt.ButikkNr   = Lager.Butik AND 
         ArtBestPkt.StrKode    = ArtLag.StrKode NO-ERROR.

    ASSIGN
      cSistSolgtDato  = ''.
      
    LOOPEN:
    FOR EACH TransLogg NO-LOCK WHERE
      TransLogg.ArtikkelNr = Lager.ArtikkelNr AND
      Translogg.Storl      = ArtLag.Storl AND
      TransLogg.Dato       <= TODAY AND
      TransLogg.Tid        >= 0 AND
      TransLogg.Butik      = Lager.Butik AND
      TransLogg.TTId       = 1
      BY Translogg.ArtikkelNr
      BY TransLogg.Storl
      BY TransLogg.Dato DESCENDING:
  
      cSistSolgtDato = STRING(TransLogg.Dato).
      LEAVE LOOPEN.
    END. /* LOOPEN */
    
    /* Setter vektet varekost. */
    IF Lager.VVareKost = ? OR Lager.VVareKost <= 0 THEN 
      cVVarekost = STRING(ArtPris.VareKost[1]).
    ELSE cVVareKost = STRING(Lager.VVareKost).
    IF cVVareKost = ? OR dec(cVVareKost) <= 0 THEN 
      cVVareKost = '0'.
    
    FOR EACH Strekkode NO-LOCK WHERE
        Strekkode.ArtikkelNr = Lager.ArtikkelNr AND
        Strekkode.StrKode    = ArtLag.StrKode:

      PUT STREAM Ut UNFORMATTED
        ArtLag.Butik ';'
        STRING(ArtBas.ArtikkelNr) + TRIM(STRING(ArtLag.StrKode,">>999")) ';'
        Strekkode.Kode ';'
        Strekkode.ERPNr  ';'    
        REPLACE(ArtBas.LevKod,';',',') ';'
        REPLACE(ArtBas.Beskr,';',',') ';'
        REPLACE(ArtBas.LevFargKod,';',',') ';'
        TRIM((IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')) ';' 
        ArtBas.Vg ';'
        ArtBas.LevNr ';'
        (IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE '') ';'
        ArtLag.LagAnt ';' 
        TODAY ';'  
        cSistSolgtDato ';'  
        cVVarekost ';'  
        (IF AVAILABLE ArtBestPkt THEN STRING(ArtBestPkt.MinAnt) ELSE '') ';'
        (IF AVAILABLE ArtBestPkt THEN STRING(ArtBestPkt.MaksAnt) ELSE '') ';'
        (IF AVAILABLE ArtBestPkt THEN STRING(ArtBestPkt.BestAnt) ELSE '')
        SKIP.
    END.

    iAnt = IAnt + 1.
    
END. /* UTLEGG_LAGER  */
END. /* BUTIKKLOOP    */
END. /* LEV_ARTIKKEL_LOOP */
        
OUTPUT STREAM Ut CLOSE.

    /* Gir filen dens riktige navn og tar bort den temporære filen. */
    OS-COPY value(ctmpFilNavn + 'txt') value(cFilNavn + 'txt').
    IF SEARCH(cFilNavn + 'txt') <> ? THEN
        OS-DELETE VALUE(ctmpFilNavn + 'txt').
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

