&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksportvaretelling.p
    Purpose     : Eksport av varetelling fra butikk til hk.
                  Eksporterer alle varetellingsposter hvor EAN koden er angitt. Dvs. normal bare lokasjonslistene.
                  døgn.

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

DEFINE BUFFER bufTelleHode FOR TelleHode.

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cFilNavn    = cKatalog + '\' + 'POSVT' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpPOSVT' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

BUTIKKLOOP:
FOR EACH Butiker NO-LOCK WHERE
  Butiker.ApningsDato     <> ? AND
  Butiker.NedlagtDato     = ? AND  
  CAN-FIND(FIRST TelleHode WHERE
           TelleHode.ButikkListe = STRING(Butiker.Butik)):
  
OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + STRING(Butiker.Butik)).

RUN bibl_logg.p ('eksporter_varetelling', 'eksportvaretelling.p Butikk: ' + string(Butiker.Butik)+ ' Fil: ' + string(cFilNavn) + STRING(Butiker.Butik)).

IF CAN-FIND(
  FIRST TelleHode NO-LOCK WHERE
    TelleHode.TelleNr > 0 AND
    TelleHode.ButikkListe = STRING(Butiker.Butik) AND 
    TelleHode.LokasjonsId = '' AND 
    TelleHode.Oppdatert >= 01/01/2010
  ) THEN 
EXPORT STREAM Ut
    'Butik;'
    'TelleNr;'
    'TelleDato;'
    'VismaNr (ArtNr+StrKode);'
    'EAN;'
    'ERPNr;'
    'ModellNr;'
    'Varenavn;'
    'Farge;'
    'Størrelse;'
    'Varegruppenr;'
    'Oppr.antall;'
    'Oppr.verdi;'
    'Talt antall;'
    'Talt verdi;'
    'VVarekost'
    SKIP.

VARETELLING:
FOR EACH TelleHode NO-LOCK WHERE
  TelleHode.TelleNr > 0 AND
  TelleHode.ButikkListe = STRING(Butiker.Butik) AND 
  TelleHode.LokasjonsId = '' AND 
  TelleHode.Oppdatert >= 01/01/2010
  BREAK BY TelleHode.TelleNr
        BY TelleHode.TelleNr:
  TELLELINJE:
  FOR EACH TelleLinje OF TelleHode NO-LOCK WHERE 
    TelleLinje.Kode > '':
      IF (TelleLinje.AntallPar = 0 AND 
          TelleLinje.AntallTalt = 0) THEN 
          NEXT TELLELINJE.
      FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN NEXT TELLELINJE.
      FIND Strekkode NO-LOCK WHERE STrekkode.Kode = TelleLinje.Kode NO-ERROR.
      IF NOT AVAILABLE Strekkode THEN NEXT TELLELINJE.
      
      PUT STREAM Ut UNFORMATTED
        TelleLinje.Butik ';'
        TelleLinje.TelleNr ';'
        (IF TelleHode.StartDato <> ? THEN STRING(TelleHode.StartDato) ELSE '') ';'
        STRING(ArtBas.ArtikkelNr) + TRIM(STRING(Strekkode.StrKode,'>999')) ';'
        Strekkode.Kode ';'
        Strekkode.ERPNr ';'
        ArtBas.LevKod ';'
        REPLACE(ArtBas.Beskr,';',',') ';'
        REPLACE(ArtBas.LevFargKod,';',',') ';'
        REPLACE(TelleLinje.Storl,';',',') ';'
        ArtBas.Vg ';'
        TelleLinje.AntallPar ';'
        TelleLinje.OpprVerdi ';'
        TelleLinje.AntallTalt ';'
        TelleLinje.OpptVerdi ';'
        TelleLinje.VVareKost
      SKIP.

      iAnt = IAnt + 1.
      
  END. /* TELLELINJE */  
  
  DO TRANSACTION:
    FIND bufTelleHode EXCLUSIVE-LOCK WHERE RECID(bufTelleHode) = RECID(TelleHode) NO-ERROR.
    IF AVAILABLE bufTelleHode THEN 
    DO:
      ASSIGN bufTelleHode.LokasjonsId = 'EKSPORTERT'.
      RELEASE bufTelleHode.
    END.
  END.        
END. /* VARETELLING */        
    
OUTPUT STREAM Ut CLOSE.

    /* Gir filen dens riktige navn og tar bort den temporære filen. */
    OS-COPY value(ctmpFilNavn + STRING(Butiker.Butik)) value(cFilNavn + STRING(Butiker.Butik)).
    IF SEARCH(cFilNavn + STRING(Butiker.Butik)) <> ? THEN
        OS-DELETE VALUE(ctmpFilNavn + STRING(Butiker.Butik)).
END. /* BUTIKKLOOP */
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
