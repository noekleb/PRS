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

DEFINE TEMP-TABLE tmpSalgPrStr
    FIELD ButikkNr AS INTEGER    
    FIELD ArtikkelNr AS DECIMAL 
    FIELD StrKode AS INTEGER     
    FIELD Dato AS DATE        
    FIELD Antall AS DECIMAL     
    FIELD InnVerdiKr AS DECIMAL  
    FIELD MvaKr AS DECIMAL      
    FIELD UtverdiKr AS DECIMAL
    FIELD LevNr AS INTEGER  
    INDEX SalgIdx ButikkNr ArtikkelNr StrKode Dato.
 
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

IF dinpFraDato = ? OR dinpTilDato = ? THEN 
DO: 
  RUN SettFraTilDato.
  RUN bibl_logg.p ('eksporter_salg', 'eksportsalg.p: AUTO WinCheduler Periode: ' + string(dinpFraDato) + ' - ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
END.
ELSE DO:
  ASSIGN 
    dFraDato = dinpFraDato
    dTilDato = dinpTilDato
    bManuell = TRUE. /* Flagger at eksporten kjøres manuelt og at det ikke skal sendes eMail. */
  RUN bibl_logg.p ('eksporter_salg', 'eksportsalg.p: MANUELL Periode: ' + string(dinpFraDato) + ' - ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
END.

/* Legger ut data til fil. */
RUN Eksporter.

lTid = TIME - lTid.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */
&IF DEFINED(EXCLUDE-Eksporter) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Eksporter Procedure
PROCEDURE Eksporter:
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
    cKatalog    = TRIM(cKatalog,'\')
    cKopi       = cKatalog + "\purpleo"
    cFilNavn    = cKatalog + '\' + 'XPOSS' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpXPOSS' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

BUTIKKLOOP:
FOR EACH Butiker NO-LOCK WHERE
  Butiker.harButikksystem = TRUE AND
  Butiker.ApningsDato     <> ? AND
  Butiker.NedlagtDato     = ?: 
  
OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + STRING(Butiker.Butik)).

RUN bibl_logg.p ('eksporter_salg', 'eksportsalg.p Butikk: ' + string(Butiker.Butik)+ ' Fil: ' + string(cFilNavn) + STRING(Butiker.Butik)).

PUT STREAM Ut UNFORMATTED 
    'Butik;'
    'VismaNr (ArtNr+StrKode);'
    'EAN;'
    'ERPNr;'
    'Dato;'
    'Antall;'
    'InnVerdiKr;'
    'MvaKr;'
    'UtverdiKr;'
    'Levnr;'
    'Vg;'
    'Oms.eks mva;'
    'Varetekst'
    SKIP.

UTLEGG_LAGER:
FOR EACH Lager NO-LOCK WHERE
    Lager.Butik = Butiker.Butik,
    FIRST ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = Lager.ArtikkelNr,
    FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = Lager.ArtikkelNr,
    EACH ArtLag NO-LOCK WHERE
         ArtLag.ArtikkelNr = Lager.ArtikkelNr AND
         ArtLag.Butik      = Lager.Butik:

    /* Tømmer tmpTabell. */
    FOR EACH tmpSalgPrStr:
      DELETE tmpSalgPrStr.
    END.
    
    FIND FIRST StrKonv NO-LOCK WHERE
        StrKonv.StrKode = ArtLag.StrKode NO-ERROR.
    IF AVAILABLE StrKonv THEN 
    LOOPEN:
    FOR EACH TransLogg NO-LOCK WHERE
      TransLogg.ArtikkelNr = Lager.ArtikkelNr AND
      Translogg.Storl      = ArtLag.Storl AND
      TransLogg.Dato       >= dFraDato AND
      TransLogg.Dato       <= dTilDato AND
      TransLogg.Tid        >= 0 AND
      TransLogg.Butik      = Lager.Butik AND
      CAN-DO('1,10',STRING(TransLogg.TTId))
      BREAK BY Translogg.ArtikkelNr
            BY TransLogg.Storl
            BY TransLogg.Dato DESCENDING:
      IF FIRST-OF(TransLogg.Dato) THEN 
        DO:
          CREATE tmpSalgPrStr.
          ASSIGN
            tmpSalgPrStr.ButikkNr   = Lager.Butik
            tmpSalgPrStr.ArtikkelNr = Lager.ArtikkelNr
            tmpSalgPrStr.StrKode    = StrKonv.StrKode
            tmpSalgPrStr.Dato       = TransLogg.Dato
            tmpSalgPrStr.LevNr      = ArtBas.LevNr
            .
        END.
      ASSIGN
          tmpSalgPrStr.Antall     = tmpSalgPrStr.Antall + TransLogg.Antall
          tmpSalgPrStr.InnVerdiKr = tmpSalgPrStr.InnVerdiKr + (Translogg.VVareKost * TransLogg.Antall) /*(TransLogg.Pris * TransLogg.Antall) - (TransLogg.RabKr * TransLogg.Antall) - (TransLogg.Mva * TransLogg.Antall)*/
          tmpSalgPrStr.MvaKr      = tmpSalgPrStr.MvaKr + (TransLogg.Mva * TransLogg.Antall)
          tmpSalgPrStr.UtverdiKr  = tmpSalgPrStr.UtVerdiKr + (TransLogg.Pris * TransLogg.Antall) - (TransLogg.RabKr * TransLogg.Antall)
          .   
    END. /* LOOPEN */
    
    FOR EACH Strekkode NO-LOCK WHERE
        Strekkode.ArtikkelNr = Lager.ArtikkelNr AND
        Strekkode.StrKode    = ArtLag.StrKode:
      FOR EACH tmpSalgPrStr WHERE
        tmpSalgPrStr.ButikkNr   = Lager.Butik AND 
        tmpSalgPrStr.ArtikkelNr = Lager.ArtikkelNr AND 
        tmpSalgPrStr.StrKode    = StrKonv.StrKode
        BY tmpSalgPrStr.ButikkNr   
        BY tmpSalgPrStr.ArtikkelNr  
        BY tmpSalgPrStr.StrKode    
        BY tmpSalgPrStr.Dato:

        PUT STREAM Ut UNFORMATTED
          /* A */ ArtLag.Butik ';'
          /* B */ STRING(ArtBas.ArtikkelNr,">>>>>>9999999") + TRIM(STRING(ArtLag.StrKode,">>999")) ';'
          /* C */ Strekkode.Kode ';'
          /* D */ Strekkode.ERPNr  ';'    
          /* E */ tmpSalgPrStr.Dato ';'
          /* F */ tmpSalgPrStr.Antall ';' 
          /* G */ ROUND(tmpSalgPrStr.InnVerdiKr,2) ';'  
          /* H */ ROUND(tmpSalgPrStr.MvaKr,2) ';'
          /* I */ ROUND(tmpSalgPrStr.UtVerdiKr,2) ';'
          /* J */ tmpSalgPrStr.LevNr ';'
          /* K */ ArtBas.Vg ';'
          /* L */ ROUND((tmpSalgPrStr.UtVerdiKr - tmpSalgPrStr.MvaKr),2) ';'
          /* M */ ArtBas.Beskr  
          SKIP.
        /*DELETE tmpSalgPrStr.*/
      END.
    END.

    iAnt = IAnt + 1.
    
END. /* UTLEGG_LAGER */
    
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
    
END. /* BUTIKKLOOP */
	
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-SettFraTilDato) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettFraTilDato Procedure
PROCEDURE SettFraTilDato:

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
