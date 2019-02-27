&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : webWebMaker.w 
    Purpose     : Eksport av initieringsfil til webside for initiering
                  av medlemmer.

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 23 jul 07
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE OUTPUT PARAMETER ocRetur AS CHARACTER  NO-UNDO.
DEFINE VAR oc2Retur AS CHARACTER  NO-UNDO.

/* NB NB NB Denne skal skrives om til å håndtere medlemsdata til Web */

DEF VAR cTmpFilNavn AS CHAR NO-UNDO.
DEF VAR cFilNavn    AS CHAR NO-UNDO.
DEF VAR cGetFilnavn AS CHAR NO-UNDO.
DEF VAR iAntEksport AS INTEGER    NO-UNDO.
DEF VAR iCl      AS INT  NO-UNDO.
DEF VAR iKode       AS INT INITIAL 1 NO-UNDO.
DEF VAR piLoop    AS INT NO-UNDO.
DEF VAR iTotAntEksport AS INT NO-UNDO.

DEFINE VARIABLE hParent        AS HANDLE NO-UNDO.
DEFINE VARIABLE cEkstEDBSystem AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg.
DEFINE BUFFER   bTT_Elogg FOR TT_Elogg.

DEF STREAM Ut.

{medweb.i}

hParent = SOURCE-PROCEDURE.
IF VALID-HANDLE(hParent) THEN 
  RUN getEkstEDBSystem IN hParent (OUTPUT cEkstEDBSystem)NO-ERROR.
IF cEkstEDBSystem = '' OR cEkstEDBSystem = ? THEN 
  cEkstEDBSystem = "WEBINIT".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fixChkEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fixChkEAN Procedure 
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

{incl\devmode.i}
{incl\custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 5 1 1 iCL INT}

FIND FIRST EkstEDBSystem WHERE 
    EkstEDBSystem.DataType = "MEDW" AND 
    EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF NOT AVAIL EkstEDBSystem THEN DO:
    ocRetur = "ERROR - Ingen medlemseksport-rutine aktiv".
    RETURN.
END.

MOTTAGER:
DO:
    IF DYNAMIC-FUNCTION("runProc","get_ekstedbsys_filnavn.p",EkstEDBSystem.EDBSystem,?) THEN 
        ASSIGN cGetFilnavn = DYNAMIC-FUNCTION("getTransactionMessage").
    IF NOT NUM-ENTRIES(cGetFilnavn,"|") = 3 THEN DO:
        ocRetur = "ERROR-" + cGetFilnavn.
        RETURN.
    END.
    ELSE DO:
        ASSIGN cTmpFilNavn = RIGHT-TRIM(ENTRY(1,cGetFilnavn,"|"),"\") + "\" + "TMP" + ENTRY(2,cGetFilnavn,"|")
               cFilNavn    = ENTRY(2,cGetFilnavn,"|").
    END.

    /* Tømmer buffer før ny sortering bygges opp. */
    FOR EACH tt_Medlem:
        DELETE tt_Medlem.
    END.
    FOR EACH TT_ELogg:
        DELETE TT_Elogg.
    END.

    /* Leser alle loggede ordre og logger berørte artikler. */
    RUN KopierElogg.

    /* Nå legger vi ut ordrene. */
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = iCl NO-ERROR.
    RUN ByggTmpTabell.
    RUN EksporterMedlem.
    ASSIGN
        iTotAntEksport = iTotantEksport + iantEksport
        iAntEksport    = 0
        .
END. /* MOTTAGER */

ocRetur = "OK," + String(iTotAntEksport).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTmpTabell) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabell Procedure 
PROCEDURE ByggTmpTabell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.
DEF VAR cNotat1      AS CHAR NO-UNDO.
DEF VAR cNotat2      AS CHAR NO-UNDO.
DEF VAR cNotat3      AS CHAR NO-UNDO.
DEF VAR cEAN         AS CHAR NO-UNDO.
DEF VAR cEANprefix   AS CHAR NO-UNDO.

ASSIGN
    cEANprefix = "29"
    .

MEDLEM:
FOR EACH TT_Elogg: 
    FIND Medlem NO-LOCK WHERE
        Medlem.MedlemsNr = DEC(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Medlem THEN
    DO:
        IF Medlem.KundeNr > 0 THEN
            FIND Kunde OF Medlem NO-LOCK NO-ERROR.
        ASSIGN
        cNotat1 = Medlem.Fornavn
        cNotat2 = Medlem.Etternavn
        cNotat3 = IF AVAILABLE Kunde
                   THEN Kunde.Navn
                   ELSE ""
        .
    END.

    ASSIGN
        cNotat1  = REPLACE (cNotat1,CHR(13),"|")
        cNotat1  = REPLACE (cNotat1,CHR(10),"|")
        cNotat2  = REPLACE (cNotat2,CHR(13),"|")
        cNotat2  = REPLACE (cNotat2,CHR(10),"|")
        cNotat3  = REPLACE (cNotat3,CHR(13),"|")
        cNotat3  = REPLACE (cNotat3,CHR(10),"|")
        .

    FIND FIRST Medlemskort OF Medlem NO-LOCK NO-ERROR.

    IF AVAILABLE Medlemskort THEN
    DO:
        ASSIGN cEAN = DYNAMIC-FUNCTION('fixChkEAN':U,
                 cEANprefix + STRING(DEC(Medlemskort.KortNr),"9999999999")).
        CREATE tt_Medlem.
        ASSIGN 
            iAntEksport          = iAntEksport + 1
            tt_Medlem.iRecType  = IF iRecType = 0 THEN 1 ELSE iRecType 
            tt_Medlem.ButikkNr  = Medlem.Butik

            tt_Medlem.Medlemsnr = Medlem.MedlemsNr
            tt_Medlem.EAN       = dec(cEAN)

            tt_Medlem.ForNavn   = cNotat1
            tt_Medlem.Etternavn = cNotat2
            tt_Medlem.KundeNr   = Medlem.KundeNr
            tt_Medlem.KundeNavn = cNotat3
            tt_Medlem.WebBrukerId = Medlem.WebBrukerId
            tt_Medlem.WebPassord  = Medlem.WebPassord
            tt_medlem.Aktiv       = IF Medlem.ForNavn BEGINS "Ukjent" 
                                    THEN 0
                                    ELSE 1
            tt_Medlem.EksterntMedlemsNr  = Medlem.EksterntMedlemsNr
            .
    END.

END. /* MEDLEM */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterMedlem Procedure 
PROCEDURE EksporterMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
DEF TEMP-TABLE tt_eksport
    FIELD iRecType  AS INT
    FIELD ButikkNr  AS INT
    FIELD Medlemsnr AS DEC  FORMAT "zzzzz99999999"
    FIELD EAN       AS DEC  FORMAT "9999999999999"
    FIELD ForNavn   AS CHAR FORMAT "x(30)"
    FIELD Etternavn AS CHAR FORMAT "x(30)"
    FIELD Kundenr   AS DEC  FORMAT "zzzzzzz999999"
    FIELD Kundenavn AS CHAR FORMAT "x(30)" 
    

------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

EKSPORTER:
FOR EACH tt_Medlem
    BREAK BY tt_Medlem.MedlemsNr:

    IF bStreamAapen = FALSE THEN
    DO:
        OUTPUT STREAM Ut TO VALUE(cTmpFilNavn) NO-ECHO.
        bStreamAapen = TRUE.
        RUN eksportHeader.
        iAntPoster = iAntPoster + 1.
    END.

    EXPORT STREAM Ut DELIMITER ";"
        tt_Medlem
        .
    iAntPoster = iAntPoster + 1.

    IF LAST(tt_medlem.MedlemsNr) THEN
    DO:
        /* Legger ut kontrollpost. */
        iAntPoster = iAntPoster + 1.
        EXPORT STREAM Ut DELIMITER ";"
            9
            IAntPoster
            .
    END.
END. /* EKSPORTER */


IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.
OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-eksportHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eksportHeader Procedure 
PROCEDURE eksportHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

EXPORT STREAM Ut DELIMITER ";"
  "iRectype"
  "ButikkNr" 
  "EksterntMedlemsNr"
  "Medlemsnr"
  "EAN"
  "ForNavn"  
  "Etternavn"
  "Kundenr"  
  "Kundenavn"
  "cFDato"
  "Telefon"
  "Mobil"
  "Adresse1"
  "Adresse2"
  "PostNr"
  "Poststed"
  "eMail"
  "cKjonn"
  "WebBrukerId"
  "WebPassord"
  "Aktiv"
  .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bElogg   FOR Elogg.
    DEFINE BUFFER erpELogg FOR Elogg.

    DEFINE VARIABLE iTst AS INTEGER    NO-UNDO.
    DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn     = "Medlem"   AND
                             ELogg.EksterntSystem = cEkstEDBSystem AND 
                             Elogg.EndringsType   = 1       NO-LOCK:
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF NOT AVAIL bElogg THEN
                NEXT.
            ASSIGN iTst = INT(bELogg.Verdier) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE bElogg.
                NEXT.
            END.
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fixChkEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fixChkEAN Procedure 
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Räknar ut checksiffra för ean EAN-kod - parameter utan chksiffra
              i.e 12 lång
      Notes:  
  ------------------------------------------------------------------------------*/
      
  cKode = cKode + '0'.
  RUN bibl_chkean.p(INPUT-OUTPUT cKode).
  RETURN cKode.

      /*
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
      DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
      END.
      RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).
      */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

