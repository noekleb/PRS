&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksporterkunder.p
    Purpose     : Eksport av kunder.

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
DEFINE VARIABLE lSaldo      AS DECIMAL NO-UNDO.
DEFINE VARIABLE dForsteDato AS DATE NO-UNDO.
DEFINE VARIABLE dSisteDato  AS DATE NO-UNDO.

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

DEFINE BUFFER bufKunde FOR Kunde.

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
DEFINE VARIABLE cSistSolgtDato  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVVareKost      AS CHARACTER NO-UNDO.

DEFINE BUFFER bufTelleHode FOR TelleHode.

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cFilNavn    = cKatalog + '\' + 'POSKU' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpPOSKU' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + STRING(iButikkNr)).

RUN bibl_logg.p ('eksporter_kunder', 'eksporterkunder.p' + ' Fil: ' + string(cFilNavn) + STRING(iButikkNr)).

PUT STREAM Ut UNFORMATTED
    'Kunde.KundeNr;'                                
    'Kunde.Navn;'                                   
    'Kunde.ePostAdresse;'                           
    'Kunde.GruppeId;'                               
    'Kunde.Adresse1;'                               
    'Kunde.Adresse2;'                               
    'Kunde.PostNr;'                                 
    'Kunde.Land;'                                   
    'Kunde.TeleFon;'                                
    'Kunde.TeleFaks;'                               
    'Kunde.MobilTlf;'                               
    'Kunde.KontNavn;'                               
    'Kunde.KontE-Post;'                             
    'Kunde.KontTelefon;'                            
    'Kunde.KontMobilTlf;'                           
    'Kunde.Stilling;'                               
    'Kunde.LevAdresse1;'                            
    'Kunde.LevAdresse2;'                            
    'Kunde.LevPostNr;'                              
    'Kunde.LevLand;'                                
    'Kunde.MaksKredit;'                             
    'Kunde.KreditSperret;'                          
    'Kunde.Opphørt;'                                
    'Kunde.ButikkNr;'                               
    'Kunde.OrgNr;'                                  
    'Kunde.TotalRabatt%;'                           
    'Kunde.BankKonto;'                              
    'Kunde.PostGiro;'                               
    'Kunde.BetBet;'                                 
    'Kunde.Etablert;'                               
    'Kunde.Samlefaktura;'                           
    'Kunde.Kjon;'                                   
    'Kunde.FodtDato;'                               
    'Kunde.Alder;'                                  
    'Kunde.FaktAdresse1;'                           
    'Kunde.FaktAdresse2;'                           
    'Kunde.FaktPostNr;'                             
    'Kunde.FaktLand;'                               
    'Kunde.FaktTekst;'                              
    'Kunde.DeresRef;'                               
    'Kunde.Privat;'                                 
    'Kunde.WebKunde;'                               
    'Kunde.Kilde;'                                  
    'Kunde.TilgKilde;'
    'Kunde.EksterntKundeNr;'
    'Kunde.Tittel;'
    'Kunde.URLFirma;'
    'Kunde.Region;'
    'KundeKort.KortNr;'
    'KundeKort.AktivertDato;'
    'KundeKort.UtgarDato;'
    'Kundekort.Sperret;'
    'KundeKort.Innehaver;'
    /*
    'Saldo;'
    'ForsteKjøp;'
    'SisteKjøp;' 
    */
    'Kunde.KundeSaldo;'
    'Kunde.SisteKjop;'
    'Kunde.SisteKjop'     
    SKIP.

KUNDELOOP:
FOR EACH Kunde NO-LOCK,
  EACH KundeKort OF Kunde NO-LOCK:

  IF Kunde.ByNavn = 'EKSPORTERT' THEN
    NEXT KUNDELOOP.
  
  ASSIGN 
      lSaldo = 0
      dForsteDato = ?
      dSisteDato  = ?
      .
  SALDOLOOP:
  FOR EACH KundeSaldo NO-LOCK WHERE
    KundeSaldo.KundeNr = Kunde.KundeNr:
    ASSIGN 
        lSaldo = lSaldo + KundeSaldo.Saldo
        .
    IF KundeSaldo.ForsteDato <> ? THEN
    DO: 
      IF dForsteDato = ? 
        THEN dForsteDato = KundeSaldo.ForsteDato.
      ELSE 
        IF dForsteDato > KundeSaldo.forsteDato
          THEN dForsteDato = KundeSaldo.forsteDato. 
    END.
    IF KundeSaldo.DatoSiste <> ? THEN
    DO: 
      IF dSisteDato = ? 
        THEN dSisteDato = KundeSaldo.DatoSiste.
      ELSE 
        IF dSisteDato > KundeSaldo.DatoSiste
          THEN dSisteDato = KundeSaldo.DatoSiste. 
    END.
  END. /* SALDOLOOP */
     
  PUT STREAM Ut UNFORMATTED
    Kunde.KundeNr ';'
    REPLACE(Kunde.Navn,';',',') ';'
    Kunde.ePostAdresse ';'
    Kunde.GruppeId ';'
    REPLACE(Kunde.Adresse1,';',',') ';'
    REPLACE(Kunde.Adresse2,';',',') ';'
    Kunde.PostNr ';'
    Kunde.Land ';'
    Kunde.TeleFon ';'
    Kunde.TeleFaks ';'
    Kunde.MobilTlf ';'
    REPLACE(Kunde.KontNavn,';',',') ';'
    Kunde.KontE-Post ';'
    Kunde.KontTelefon ';'
    Kunde.KontMobilTlf ';'
    REPLACE(Kunde.Stilling,';',',') ';'
    REPLACE(Kunde.LevAdresse1,';',',') ';'
    REPLACE(Kunde.LevAdresse2,';',',') ';'
    Kunde.LevPostNr ';'
    Kunde.LevLand ';'
    Kunde.MaksKredit ';'
    Kunde.KreditSperret ';'
    Kunde.Opphort ';'
    Kunde.ButikkNr ';'
    Kunde.OrgNr ';'
    Kunde.TotalRabatt% ';'
    Kunde.BankKonto ';'
    Kunde.PostGiro ';'
    Kunde.BetBet ';'
    Kunde.Etablert ';'
    Kunde.Samlefaktura ';'
    Kunde.Kjon ';'
    Kunde.FodtDato ';'
    Kunde.Alder ';'
    REPLACE(Kunde.FaktAdresse1,';',',') ';'
    REPLACE(Kunde.FaktAdresse2,';',',') ';'
    Kunde.FaktPostNr ';'
    Kunde.FaktLand ';'
    Kunde.FaktTekst ';'
    Kunde.DeresRef ';'
    Kunde.Privat ';'
    Kunde.WebKunde ';'
    Kunde.Kilde ';'
    Kunde.TilgKilde ';'
    Kunde.EksterntKundeNr ';'
    Kunde.Tittel ';'
    Kunde.URLFirma ';'
    Kunde.Region ';'
    KundeKort.KortNr ';'
    KundeKort.AktivertDato ';'
    KundeKort.UtgarDato ';'
    Kundekort.Sperret ';'
    KundeKort.Innehaver  ';'
    /*
    lSaldo ';'
    dForsteDato ';'
    dSisteDato ';' 
    */
    Kunde.KundeSaldo ';'
    Kunde.SisteKjop ';'
    Kunde.SisteKjop     
  SKIP.

  iAnt = IAnt + 1.
  DO TRANSACTION:
    FIND bufKunde EXCLUSIVE-LOCK WHERE RECID(bufKunde) = RECID(Kunde) NO-ERROR.
    IF AVAILABLE bufKunde THEN 
    DO:
      ASSIGN bufKunde.ByNavn = 'EKSPORTERT'.
      RELEASE bufKunde.
    END.
  END.        
    
END. /* KUNDELOOP */

OUTPUT STREAM Ut CLOSE.

    /* Gir filen dens riktige navn og tar bort den temporære filen. */
    OS-COPY value(ctmpFilNavn + STRING(iButikkNr)) value(cFilNavn + STRING(iButikkNr)).
    IF SEARCH(cFilNavn + STRING(iButikkNr)) <> ? THEN
        OS-DELETE VALUE(ctmpFilNavn + STRING(iButikkNr)).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
