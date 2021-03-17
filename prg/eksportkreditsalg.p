&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksportkreditsalg.p
    Purpose     : Eksport av kreditsalg fra butikk til hk.
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

DEFINE OUTPUT PARAMETER ocRetur     AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

DEF VAR iAntLinjer          AS INT     NO-UNDO.
DEF VAR iAlle               AS INT     NO-UNDO.
DEF VAR bStream             AS LOG     NO-UNDO.
DEFINE VARIABLE lDec        AS DECIMAL NO-UNDO.
DEFINE VARIABLE bManuell    AS LOG     NO-UNDO.
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

DEFINE TEMP-TABLE tmpFaktura
    FIELD ButikkNr AS INTEGER FORMAT ">>>>>9"
    FIELD Fakturanr AS DECIMAL FORMAT ">>>>>>>>>>>>9"
    FIELD Bilagstype AS CHARACTER FORMAT "x(20)"
    FIELD FakturertDato AS DATE FORMAT "99/99/99"
    FIELD ForfallsDato AS DATE FORMAT "99/99/99"
    FIELD KID AS DECIMAL FORMAT ">>>>>>>>>>>>>>>>>>>>>>>>9"
    FIELD KundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9"
    FIELD EksterntKundeNr AS CHARACTER FORMAT "x(20)"
    FIELD Navn AS CHARACTER FORMAT "x(40)"
    FIELD Adresse1 AS CHARACTER FORMAT "x(30)"
    FIELD Adresse2 AS CHARACTER FORMAT "x(30)"
    FIELD PostNr AS CHARACTER FORMAT "x(20)"
    FIELD Poststed AS CHARACTER FORMAT "x(30)"
    FIELD Land AS CHARACTER FORMAT "x(30)"
    FIELD ePostAdresse AS CHARACTER FORMAT "x(40)"
    FIELD MaksKredit AS DECIMAL FORMAT "->,>>>,>>9.99"
    FIELD SamleFaktura AS LOG
    FIELD KreditSperret AS LOG 
    FIELD OrgNr AS CHARACTER FORMAT "x(15)"
    FIELD AvgPlSalg AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD AvgFriSalg AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD MvaKr AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD Totalt AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD AntDager AS INT FORMAT "->>,>>>,>>9.99"
    INDEX Butikk ButikkNr FakturaNr.

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
         HEIGHT             = 14.24
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

ASSIGN 
    bManuell = TRUE. /* Flagger at eksporten kjøres manuelt og at det ikke skal sendes eMail. */
RUN bibl_logg.p ('eksporter_kreditsalg', 'eksportkreditsalg.p: Dato: ' + STRING(TODAY) + ' ' + string(TIME,"HH:MM:SS")).

/* Legger ut data til fil. */
RUN Eksporter.

ocRetur = "OK," + STRING(iAntEksport) + cTekst.

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
DEFINE VARIABLE cEAN            AS CHAR NO-UNDO.

DEFINE BUFFER bFakturaHode FOR FakturaHode.

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cFilNavn    = cKatalog + '\' + 'POSK' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpPOSK' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.


BUTIKKLOOP:
FOR EACH Butiker NO-LOCK WHERE
  Butiker.harButikksystem = TRUE AND
  Butiker.ApningsDato     <> ? AND
  Butiker.NedlagtDato     = ?: 

/* Tømmer tmpfile */
FOR EACH tmpFaktura:
    DELETE tmpFaktura.
END.    
  
RUN bibl_logg.p ('eksporter_kreditsalg', 'eksportsalg.p Butikk: ' + string(Butiker.Butik)+ ' Fil: ' + string(cFilNavn) + STRING(Butiker.Butik)).

OPPRETT_FAKTURA:
FOR EACH FakturaHode NO-LOCK WHERE 
  FakturaHode.EksportertDato = ? AND 
  FakturaHode.FakturertDato  >= 01/01/2012 
  BREAK BY FakturaHode.ButikkNr
        BY FakturaHode.BilagsType
        BY FakturaHode.FakturaNr:
            
  IF FakturaHode.FakturaNr = ? THEN 
    NEXT OPPRETT_FAKTURA.        
    
  DO FOR bFakturaHode TRANSACTION:
      FIND bFakturaHode EXCLUSIVE-LOCK WHERE
          RECID(bFakturaHode) = RECID(FakturaHode) NO-WAIT NO-ERROR.
      IF AVAILABLE bFakturaHode THEN 
      DO:
          ASSIGN 
              bFakturaHode.EksportertDato = TODAY
              iAnt                        = IAnt + 1.
          
          FIND Kunde OF FakturaHode NO-LOCK NO-ERROR.
          FIND Bilagstype OF FakturaHode NO-LOCK NO-ERROR.
          
          CREATE tmpFaktura.
          ASSIGN 
            tmpFaktura.ButikkNr        = FakturaHode.ButikkNr
            tmpFaktura.Fakturanr       = FakturaHode.FakturaNr
            tmpFaktura.BilagsType      = (IF AVAILABLE Bilagstype THEN Bilagstype.BTTekst ELSE '')
            tmpFaktura.FakturertDato   = FakturaHode.FakturertDato
            tmpFaktura.ForfallsDato    = FakturaHode.Forfallsdato
            tmpFaktura.KID             = FakturaHode.KID
            tmpFaktura.KundeNr         = FakturaHode.KundeNr
            tmpFaktura.SamleFaktura    = FakturaHode.SamleFaktura
            tmpFaktura.AvgPlSalg       = FakturaHode.AvgPlSalg
            tmpFaktura.AvgFriSalg      = FakturaHode.AvgFriSalg
            tmpFaktura.MvaKr           = FakturaHode.MvaKr
            tmpFaktura.Totalt          = FakturaHode.Totalt
            tmpFaktura.AntDager        = FakturaHode.AntDager            
            tmpFaktura.EksterntKundeNr = (IF AVAILABLE Kunde THEN Kunde.EksterntKundeNr ELSE '')
            tmpFaktura.Navn            = FakturaHode.Navn
            tmpFaktura.Adresse1        = FakturaHode.Adresse1
            tmpFaktura.Adresse2        = FakturaHode.Adresse2
            tmpFaktura.PostNr          = FakturaHode.PostNr
            tmpFaktura.Poststed        = FakturaHode.PostSted            
            tmpFaktura.Land            = FakturaHode.Land
            tmpFaktura.ePostAdresse    = (IF AVAILABLE Kunde THEN Kunde.ePostAdresse ELSE '')
            tmpFaktura.MaksKredit      = (IF AVAILABLE Kunde THEN Kunde.MaksKredit ELSE 0)
            tmpFaktura.KreditSperret   = (IF AVAILABLE Kunde THEN Kunde.KreditSperret ELSE FALSE)
            tmpFaktura.OrgNr           = (IF AVAILABLE Kunde THEN Kunde.OrgNr ELSE '')
          .
      END.
  END. /* TRANSACTION */     
END. /* OPPRETT_FAKTURA */    
    
IF CAN-FIND(FIRST tmpFaktura) THEN
EKSPORT: 
DO:
    OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + STRING(Butiker.Butik)).
    PUT STREAM Ut UNFORMATTED 
        'ButikkNr;'
        'Fakturanr;'
        'BilagsType;'
        'FakturertDato;'
        'ForfallsDato;'
        'KID;'
        'KundeNr;'
        'EksterntKundeNr;'
        'Navn;'
        'Adresse1;'
        'Adresse2;'
        'PostNr;'
        'Poststed;'
        'Land;'
        'ePostAdresse;'
        'MaksKredit;'
        'SamleFaktura;'
        'KreditSperret;' 
        'OrgNr;'
        'AvgPlSalg;'
        'AvgFriSalg;'
        'MvaKr;'
        'Totalt;'
        'AntDager'
        SKIP.

    FOR EACH tmpFaktura 
        BREAK BY tmpFaktura.ButikkNr
              BY tmpFaktura.FakturaNr:
                        
        PUT STREAM Ut UNFORMATTED
          tmpFaktura.ButikkNr ';'
          tmpFaktura.Fakturanr ';'
          tmpFaktura.BilagsType ';'
          tmpFaktura.FakturertDato ';'
          tmpFaktura.ForfallsDato ';'
          tmpFaktura.KID ';'
          tmpFaktura.KundeNr ';'
          tmpFaktura.EksterntKundeNr ';'
          tmpFaktura.Navn ';'
          tmpFaktura.Adresse1 ';'
          tmpFaktura.Adresse2 ';'
          tmpFaktura.PostNr ';'
          tmpFaktura.Poststed ';'
          tmpFaktura.Land ';'
          tmpFaktura.ePostAdresse ';'
          tmpFaktura.MaksKredit ';'
          tmpFaktura.SamleFaktura ';'
          tmpFaktura.KreditSperret ';' 
          tmpFaktura.OrgNr ';'
          tmpFaktura.AvgPlSalg ';'
          tmpFaktura.AvgFriSalg ';'
          tmpFaktura.MvaKr ';'
          tmpFaktura.Totalt ';'
          tmpFaktura.AntDager
          SKIP.
    END.
    OUTPUT STREAM Ut CLOSE.

    /* Gir filen dens riktige navn og tar bort den temporære filen. */
    OS-COPY value(ctmpFilNavn + STRING(Butiker.Butik)) value(cFilNavn + STRING(Butiker.Butik)).
    IF SEARCH(cFilNavn + STRING(Butiker.Butik)) <> ? THEN
        OS-DELETE VALUE(ctmpFilNavn + STRING(Butiker.Butik)).
END. /* EKSPORT */
END. /* BUTIKKLOOP */
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
