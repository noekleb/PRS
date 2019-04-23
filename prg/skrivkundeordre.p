&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description : ï¿½ 

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR cParaString AS CHARACTER INIT "1120000001|FULL" NO-UNDO.
    DEFINE VAR lDirekte    AS LOGICAL                      NO-UNDO.
    DEFINE VAR cPrinter    AS CHARACTER                    NO-UNDO.
    DEFINE VAR iAntEks     AS INTEGER INIT 1               NO-UNDO.
    DEFINE VAR cMailAdress AS CHARACTER                    NO-UNDO.
    DEFINE VAR cStatusTxt  AS CHARACTER                    NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER cParaString AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER lDirekte    AS LOGICAL    NO-UNDO.
    DEFINE INPUT PARAMETER cPrinter    AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER iAntEks     AS INTEGER    NO-UNDO.
    DEFINE INPUT PARAMETER cMailAdress AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER cStatusTxt  AS CHARACTER  NO-UNDO.
&ENDIF

DEFINE VARIABLE bSkrivMedlem AS LOG NO-UNDO.
DEFINE VARIABLE cFirma       AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE hHodeTH      AS HANDLE                   NO-UNDO.
DEFINE VARIABLE hLinjeTH     AS HANDLE                   NO-UNDO.
DEFINE VARIABLE hTTHodeBuff  AS HANDLE                   NO-UNDO.
DEFINE VARIABLE hTTLinjeBuff AS HANDLE                   NO-UNDO.
DEFINE VARIABLE cCmd AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcRappFil AS CHARACTER NO-UNDO.

DEFINE VARIABLE iFormatKod   AS INTEGER                  NO-UNDO. /* formatteras bort */
DEFINE VARIABLE lFullRapport AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE cSprak       AS CHARACTER                NO-UNDO.
DEFINE VARIABLE iLeftMarg    AS INTEGER                  NO-UNDO.
DEFINE VARIABLE iBottomMarg  AS INTEGER                  NO-UNDO.
DEFINE VARIABLE dColPos      AS DECIMAL EXTENT 13        NO-UNDO.
DEFINE VARIABLE dColPos2     AS DECIMAL EXTENT 13        NO-UNDO.
DEFINE VARIABLE lCode39      AS LOGICAL INIT FALSE       NO-UNDO.
DEFINE VARIABLE cLogo        AS CHARACTER                NO-UNDO.
DEFINE VARIABLE BongText     AS CHARACTER                NO-UNDO.
DEFINE VARIABLE lStrKod      AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE cStrKod      AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cFraktNr     AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cKvittoTekst AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cPlace       AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cDate        AS CHARACTER                NO-UNDO.
DEFINE VARIABLE iAntTkn      AS INTEGER                  NO-UNDO.
DEFINE VARIABLE cTekst       AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cLayout      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUtleverPGM  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cKopior AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAntal AS INTEGER     NO-UNDO.
DEFINE TEMP-TABLE TT_RapportRader NO-UNDO
    FIELD iPageNum AS INTEGER  /* Sidnr */
    FIELD iColPage AS INTEGER  /* Hantering av 'fï¿½r mï¿½nga cols' */
    FIELD iRadNum  AS INTEGER
    FIELD cRadData AS CHARACTER
    INDEX RadNum iPageNum iColPage iRadNum.

DEFINE TEMP-TABLE TT_image NO-UNDO
    FIELD image_name    AS CHARACTER
INDEX obj_name AS PRIMARY
      image_name.

    { pdf_inc.i "THIS-PROCEDURE"}.

  &ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd Procedure 
  FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER )  FORWARD.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fixChkEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fixChkEAN Procedure 
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRapPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRapPrinter Procedure 
FUNCTION getRapPrinter RETURNS CHARACTER
  ( INPUT ipcPrinter AS CHARACTER )  FORWARD.

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

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
IF lDirekte AND NOT CAN-DO(SESSION:GET-PRINTERS(),cPrinter) THEN
DO:
    RETURN.
END.    

{syspar2.i 1 1 8 cCmd}
IF cCmd = '' THEN 
    cCmd = "cmd\FoxitReader.exe /t".

IF NUM-ENTRIES(cParaString,"|") = 1 THEN
    ASSIGN cParaString = cParaString + "|".
IF ENTRY(2,cParaString,"|") = "FULL" THEN
    ASSIGN lFullRapport = TRUE
           ENTRY(2,cParaString,"|") = "".
FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
   cSprak = TRIM(Bruker.Lng).

cKvittoTekst = "Spara detta kvitto vid eventuellt byte eller retur.".


{syspara.i 19 9 11 cTekst}
IF CAN-DO('1,Ja,J,Yes,True',cTekst)
  THEN bSkrivMedlem = TRUE.
ELSE bSkrivMedlem = FALSE. 
{syspara.i 150 10 1 cFraktNr}
{syspara.i 19 9 10 BongText}
{syspar2.i 19 9 10 cKvittoTekst}
{syspara.i 5 4 31 cLogo}
{syspar2.i 5 4 31 cPlace}
{syspara.i 19 13 1 cLayout}
{syspar2.i 19 13 1 cUtleverPGM}
{syspara.i 19 13 3 cKopior}
IF cLogo = "" THEN
    ASSIGN cLogo = "icon\johsvart.jpg".
{syspara.i 5 4 32 cStrKod}
 IF cStrKod = "" THEN
    ASSIGN cStrKod = "0".
 IF cStrkod = "1" THEN
    ASSIGN lStrKod = TRUE.
 ELSE
    ASSIGN lSTrKod = FALSE.
/* RETURN. */
    iAntal = INT(cKopior) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        iAntal = 1.
    iAntEks = MAX(iAntal,iAnteks).

RUN GetTempFileName IN wLibHandle ("skrivkundeordre", "pdf", OUTPUT pcRappFil).
IF cLayout = "1" AND cUtleverPGM <> "" /* AND ENTRY(2,cParaString,"|") = "utlev" */ THEN DO:
    RUN VALUE(cUtleverPGM) (ENTRY(1,cParaString,"|"), pcRappFil).
/*     RUN Gant_Skrivutlever.p (ENTRY(1,cParaString,"|"), pcRappFil). */
END.
ELSE DO:
    RUN PopulateTT.
    RUN PDFSkrivRapport.
END.

IF lDirekte = FALSE THEN
DO:
  IF SEARCH(pcRappFil) <> ? THEN
    RUN browse2pdf\viewxmldialog.w (pcRappFil,"Polygon Retail Solutions").
END.
ELSE 
DO:
  IF SEARCH(pcRappFil) <> ? THEN DO iAntal = 1 TO iAntEks:
/*      OS-COMMAND SILENT VALUE(cCmd + ' ' + pcRappFil + ' "' + cPrinter + '"').*/
      OS-COMMAND SILENT VALUE(SEARCH(ENTRY(1,cCmd,' ')) + ' /t ' + pcRappFil + ' "' + cPrinter + '"').
      
  END.
    RUN bibl_logg.p ('skrivKundeOrdre', 'skrivkundeordre.p: ' + 
        ' Fil: ' + pcRappFil).
END.

RETURN pcRappFil.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PDFPageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPageHeader Procedure 
PROCEDURE PDFPageHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iSidnr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER cStatus      AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lKopi        AS LOGICAL    NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER dY     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE  cRubrik     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cKopiStr    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAdresse1    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAdresse2    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cPostNr      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cPostSted    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cLand        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cKOidEAN     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iWidth       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iHeight      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cPostNr2     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iY           AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cBildNamn    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iImageHeight AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iImageWidth  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iButik       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLogoCol     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLogoRow     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLogoStr1    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLogoStr2    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cWrk         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE dY2          AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE ii           AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cEpost       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTelefon     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTlfTMP      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLevKontakt  AS CHARACTER   NO-UNDO.
    ASSIGN iLogoCol = INT(ENTRY(1,cPlace,",")) 
           iLogoRow = INT(ENTRY(2,cPlace,",")) 
           iLogoStr1 = INT(ENTRY(3,cPlace,",")) 
           iLogoStr2 = INT(ENTRY(4,cPlace,",")) NO-ERROR.
    IF iLogoCol = 0 THEN
      ASSIGN iLogoCol = 570.
    IF iLogoRow = 0 THEN
      ASSIGN iLogoRow = 100.
    IF iLogoStr1 = 0 THEN
      ASSIGN iLogoStr1 = 230.
    IF iLogoStr2 = 0 THEN
      ASSIGN iLogoStr2 = 210.

    ASSIGN iWidth = pdf_PageWidth ("Spdf") - 70.
    ASSIGN iHeight = pdf_PageHeight ("Spdf") - 60.

    RUN pdf_rect2 ("Spdf",iLeftMarg,iBottomMarg,iWidth,iHeight,0.1).

    IF CAN-DO("SE,SVE",cSprak) THEN
      ASSIGN cKopiStr = IF lKopi THEN "K O P I A" ELSE "" 
             cRubrik = "O R D E R / K V I T T O".
    ELSE
      ASSIGN cKopiStr = IF lKopi THEN "K O P I" ELSE "" 
             cRubrik  = "O R D R E / B O N G".

    cAdresse1    = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE).
    cAdresse2    = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE).
    cPostNr      = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE).
    cPostSted = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostSted"):BUFFER-VALUE).
    cLand        = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE).

    ASSIGN iButik = INT(hTTHodeBuff:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).
    FIND FIRST Butiker WHERE Butiker.Butik = iButik NO-LOCK NO-ERROR.

    IF cAdresse1 = "" THEN DO:
        cAdresse1  = TRIM(hTTHodeBuff:BUFFER-FIELD("Adresse1"):BUFFER-VALUE).
        cAdresse2  = TRIM(hTTHodeBuff:BUFFER-FIELD("Adresse2"):BUFFER-VALUE).
        cPostNr    = TRIM(hTTHodeBuff:BUFFER-FIELD("PostNr"):BUFFER-VALUE).
        cPostSted  = TRIM(hTTHodeBuff:BUFFER-FIELD("PostSted"):BUFFER-VALUE).
        cLand      = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE).
    END.
    ASSIGN cKOidEAN = STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE).

    RUN pdf_set_font ("Spdf", "Helvetica-Bold",14).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE),iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",cKopiStr,iLeftMarg + 180,dY).
    RUN pdf_text_xy_dec ("Spdf",cRubrik,iLeftMarg + 300,dY).
    dY = dY - 20.
    RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
    IF CAN-DO("SE,SVE",cSprak) THEN
      RUN pdf_text_xy_dec ("Spdf","Adress",iLeftMarg + 5,dY).
    ELSE
      RUN pdf_text_xy_dec ("Spdf","Adresse",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
    RUN pdf_text_xy_dec ("Spdf",Butiker.BuAdr,iLeftMarg + 75,dY).
/*    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse1"):BUFFER-VALUE),iLeftMarg + 75,dY).*/

    dY = dY - 13.
    ASSIGN cPostNr2 = STRING(hTTHodeBuff:BUFFER-FIELD("FirmaPostNr"):BUFFER-VALUE).
    RUN pdf_text_xy_dec ("Spdf","Postnr",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
    RUN pdf_text_xy_dec ("Spdf",Butiker.BuPonr + " " + Butiker.BuPadr,iLeftMarg + 75,dY).
/*    RUN pdf_text_xy_dec ("Spdf",cPostNr2 + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPoststed"):BUFFER-VALUE),iLeftMarg + 75,dY).*/

    dY = dY - 13.
    RUN pdf_text_xy_dec ("Spdf","Telefon",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
/*    IF hTTHodeBuff:BUFFER-FIELD("FirmaTelefon"):BUFFER-VALUE <> "" THEN
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefon"):BUFFER-VALUE),iLeftMarg + 75,dY).
    ELSE*/
    RUN pdf_text_xy_dec ("Spdf",Butiker.BuTel,iLeftMarg + 75,dY).
/*    RUN pdf_text_xy_dec ("Spdf","Bank",iLeftMarg + 220,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 270,dY).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE),iLeftMarg + 275,dY).*/
    IF CAN-DO("SE,SVE",cSprak) THEN
      RUN pdf_text_xy_dec ("Spdf","Sida",iLeftMarg + 220,dY).
    ELSE
      RUN pdf_text_xy_dec ("Spdf","Side",iLeftMarg + 220,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 275,dY).
    RUN pdf_text_xy_dec ("Spdf",STRING(iSidNr),iLeftMarg + 280,dY).

    dY = dY - 13.
/*    IF CAN-DO("SE,SVE",cSprak) THEN
      RUN pdf_text_xy_dec ("Spdf","Telefax",iLeftMarg + 5,dY).
    ELSE
      RUN pdf_text_xy_dec ("Spdf","Telefaks",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefaks"):BUFFER-VALUE),iLeftMarg + 75,dY).*/
    RUN pdf_text_xy_dec ("Spdf","E-post",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
/*    IF hTTHodeBuff:BUFFER-FIELD("FirmaEPost"):BUFFER-VALUE <> "" THEN
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaEPost"):BUFFER-VALUE),iLeftMarg + 75,dY).
    ELSE*/
    RUN pdf_text_xy_dec ("Spdf",Butiker.ePostAdresse,iLeftMarg + 75,dY).
/*    RUN pdf_text_xy_dec ("Spdf","Postgiro",iLeftMarg + 220,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 270,dY).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE),iLeftMarg + 275,dY).*/
    cLevKontakt = Butiker.levkontakt.
    IF cLevkontakt <> "" THEN DO:
/*         IF CAN-DO("SE,SVE",cSprak) THEN                                */
/*             RUN pdf_text_xy_dec ("Spdf","Ordernr",iLeftMarg + 220,dY). */
/*         ELSE                                                           */
/*             RUN pdf_text_xy_dec ("Spdf","Ordrenr",iLeftMarg + 220,dY). */
        RUN pdf_text_xy_dec ("Spdf","Kontakt",iLeftMarg + 220,dY).
        RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 275,dY).
        RUN pdf_text_xy_dec ("Spdf",cLevkontakt,iLeftMarg + 280,dY).
/*         RUN pdf_text_xy_dec ("Spdf",(IF hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE) ELSE ""),iLeftMarg + 280,dY). */
    END.
    dY = dY - 13.
    RUN pdf_text_xy_dec ("Spdf","Org.nr",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
/*    IF hTTHodeBuff:BUFFER-FIELD("FirmaOrganisasjonsNr"):BUFFER-VALUE <> "" THEN
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaOrganisasjonsNr"):BUFFER-VALUE),iLeftMarg + 75,dY).
    ELSE*/
    RUN pdf_text_xy_dec ("Spdf",Butiker.OrganisasjonsNr,iLeftMarg + 75,dY).

    IF bSkrivMedlem THEN DO:
      IF CAN-DO("SE,SVE",cSprak) THEN
        RUN pdf_text_xy_dec ("Spdf","Medlemsnr",iLeftMarg + 220,dY).
      ELSE
        RUN pdf_text_xy_dec ("Spdf","Medlemsnr",iLeftMarg + 220,dY).
      RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 275,dY).
      FIND FIRST Medlem NO-LOCK WHERE 
        Medlem.KundeNr = DEC(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE) NO-ERROR.
      IF AVAILABLE Medlem THEN FIND FIRST Medlemskort OF Medlem NO-LOCK NO-ERROR.
      IF AVAILABLE Medlem THEN 
          RUN pdf_text_xy_dec ("Spdf",STRING(Medlem.MedlemsNr),iLeftMarg + 280,dY).
      /*
      IF AVAILABLE MedlemsKort THEN 
          RUN pdf_text_xy_dec ("Spdf",STRING(MedlemsKort.KortNr),iLeftMarg + 280,dY).
      */
    END.
    ELSE DO:
      IF CAN-DO("SE,SVE",cSprak) THEN
        RUN pdf_text_xy_dec ("Spdf","Kundenr",iLeftMarg + 220,dY).
      ELSE
        RUN pdf_text_xy_dec ("Spdf","Kundenr",iLeftMarg + 220,dY).
      RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 275,dY).
      RUN pdf_text_xy_dec ("Spdf",STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE),iLeftMarg + 280,dY).
    END.
    dY = dY - 13.
    RUN pdf_text_xy_dec ("Spdf","WebAdress",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
    RUN pdf_text_xy_dec ("Spdf",Butiker.VaarRef,iLeftMarg + 75,dY).
    IF hTTHodeBuff:BUFFER-FIELD("EkstOrdreNr"):BUFFER-VALUE <> "" THEN
    DO:
      /*RUN pdf_text_xy_dec ("Spdf","WebNr",iLeftMarg + 220,dY).*/
      RUN pdf_text_xy_dec ("Spdf","Ordrenr",iLeftMarg + 220,dY).
      RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 275,dY).
      RUN pdf_text_xy_dec ("Spdf",STRING(hTTHodeBuff:BUFFER-FIELD("EkstOrdreNr"):BUFFER-VALUE),iLeftMarg + 280,dY).
    END.

/*    RUN pdf_text_xy_dec ("Spdf","URL",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaURLAdresse"):BUFFER-VALUE),iLeftMarg + 75,dY).*/

    dY = dY - 13.
/*    IF CAN-DO("SE,SVE",cSprak) THEN
      RUN pdf_text_xy_dec ("Spdf","LevAdress",iLeftMarg + 5,dY).
    ELSE
      RUN pdf_text_xy_dec ("Spdf","LevAdresse",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("LevAdresse1"):BUFFER-VALUE),iLeftMarg + 75,dY).*/

    RUN pdf_set_font ("Spdf", "Helvetica-Bold",14).
    dY = dY - 26.
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE),iLeftMarg + 5,dY).

    dY = dY - 13.
    RUN pdf_text_xy_dec ("Spdf",cAdresse1,iLeftMarg + 5,dY).
    dY = dY - 13.
    IF cAdresse2 <> "" THEN
        RUN pdf_text_xy_dec ("Spdf",cAdresse2,iLeftMarg + 5,dY).
/*       RUN pdf_text_xy_dec ("Spdf",cAdresse2,iLeftMarg + 200,dY). */

    dY = dY - 13.
    RUN pdf_text_xy_dec ("Spdf",cPostNr + " " + cPoststed,iLeftMarg + 5,dY).

    dY = dY - 13.
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE),iLeftMarg + 5,dY).

    RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
    dY = dY - 13.

    RUN pdf_rect2 ("Spdf",iLeftMarg + 400,dY - 40,350,100,0.1). /* Left,Topp,Width,Height */

    IF lFullRapport THEN 
    DO:
/*       ASSIGN cWrk = "Kund har godkï¿½nt att fï¿½ 250kr i rabatt pga av mï¿½rk flï¿½ck pï¿½ hï¿½gerskon. Detta ï¿½r en test av lï¿½nga texter. Jag vet inte hur mï¿½nga rader man fï¿½r plats med. Kanske bara ytterligare en rad.".*/
       ASSIGN cWrk = TRIM(hTTHodeBuff:BUFFER-FIELD("KundeMerknad"):BUFFER-VALUE).
       RUN pdf_set_font ("Spdf", "Helvetica",10).
       ASSIGN dY2 = 398.
       REPEAT:
         iAntTkn = pdf_GetNumFittingChars ("Spdf",cWrk,450,760).    /* 740*/
         IF iAntTkn >= LENGTH(cWrk) THEN
         DO:
           RUN pdf_text_xy_dec ("Spdf",cWrk,450,dY2).
           LEAVE.
         END.
         ELSE
         DO: 
           ASSIGN ii = iAntTkn.
           DO iAntTkn = ii TO 1 BY -1:
           IF SUBSTRING(cWrk,iAntTkn,1) = " " THEN
             LEAVE.
           END.
           RUN pdf_text_xy_dec ("Spdf",SUBSTRING(cWrk,1,iAntTkn),450,dY2).
           IF dY2 = 318 THEN
             LEAVE.
           ASSIGN cWrk = SUBSTRING(cWrk,iAntTkn + 1,LENGTH(cWrk)).
           ASSIGN dY2 = dY2 - 20.
/*           RUN pdf_text_xy_dec ("Spdf",SUBSTRING(cWrk,iAntTkn + 1,LENGTH(cWrk)),450,375).*/
         END.
       END.
    END.
    RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).


/*    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("LevPostNr"):BUFFER-VALUE) + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("LevPostSted"):BUFFER-VALUE),iLeftMarg + 75,dY).*/

    dY = dY - 13.
    cEpost = TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("ePostAdresse"):BUFFER-VALUE)).
    IF cEpost = "" THEN
        cEpost = TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("FirmaEPost"):BUFFER-VALUE)).
    IF cEpost <> "" THEN DO:
        RUN pdf_text_xy_dec ("Spdf","Email",iLeftMarg + 5,dY).
        RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
        RUN pdf_text_xy_dec ("Spdf",cEpost,iLeftMarg + 75,dY).
    END.
    dY = dY - 13.
    DO: /* telefon */
        cTelefon = TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Telefon"):BUFFER-VALUE)).
        cTlfTMP  = TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("KontTelefon"):BUFFER-VALUE)).
        IF cTlfTMP <> "" THEN
            cTelefon = cTelefon + (IF cTelefon = "" THEN "" ELSE " / ") + cTlfTMP.
/*         cTlfTMP  = TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("FirmaTelefon"):BUFFER-VALUE)). */
/*         IF cTlfTMP <> "" THEN                                                      */
/*             cTelefon = cTelefon + (IF cTelefon = "" THEN "" ELSE " / ") + cTlfTMP. */
        IF cTelefon <> "" THEN DO:
         RUN pdf_text_xy_dec ("Spdf","Telefon",iLeftMarg + 5,dY).
         RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
         RUN pdf_text_xy_dec ("Spdf",cTelefon,iLeftMarg + 75,dY).
        END.
    END.

    dY = dY - 13.
    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      ASSIGN cDate = STRING(YEAR(hTTHodeBuff:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE),"9999") + "-" 
                   + STRING(MONTH(hTTHodeBuff:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE),"99") + "-" 
                   + STRING(DAY(hTTHodeBuff:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE),"99").
    ELSE
      ASSIGN cDate = STRING(hTTHodeBuff:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE).

    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
        RUN pdf_text_xy_dec ("Spdf","Reg.Datum",iLeftMarg + 5,dY).
    ELSE
        RUN pdf_text_xy_dec ("Spdf","Reg.Dato",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
    RUN pdf_text_xy_dec ("Spdf",cDate,iLeftMarg + 75,dY).

/*    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("LevLand"):BUFFER-VALUE),iLeftMarg + 75,dY).*/

    dY = dY - 13.
    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      ASSIGN cDate = STRING(YEAR(hTTHodeBuff:BUFFER-FIELD("Utsendelsesdato"):BUFFER-VALUE),"9999") + "-" 
                   + STRING(MONTH(hTTHodeBuff:BUFFER-FIELD("Utsendelsesdato"):BUFFER-VALUE),"99") + "-" 
                   + STRING(DAY(hTTHodeBuff:BUFFER-FIELD("Utsendelsesdato"):BUFFER-VALUE),"99").
    ELSE
      ASSIGN cDate = STRING(hTTHodeBuff:BUFFER-FIELD("Utsendelsesdato"):BUFFER-VALUE).

    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
        RUN pdf_text_xy_dec ("Spdf","Lev.Datum",iLeftMarg + 5,dY).
    ELSE
        RUN pdf_text_xy_dec ("Spdf","Lev.Dato",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
    RUN pdf_text_xy_dec ("Spdf",cDate,iLeftMarg + 75,dY).
/*    RUN pdf_text_xy_dec ("Spdf","Vï¿½r ref",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("VaarRef"):BUFFER-VALUE),iLeftMarg + 75,dY).*/

/*     dY = dY - 13. */
/*    IF CAN-DO("SE,SVE",cSprak) THEN
      RUN pdf_text_xy_dec ("Spdf","Er ref",iLeftMarg + 5,dY).
    ELSE
      RUN pdf_text_xy_dec ("Spdf","Deres ref",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("DeresRef"):BUFFER-VALUE),iLeftMarg + 75,dY).*/

    dY = dY - 13.
/*    IF CAN-DO("SE,SVE",cSprak) THEN
      RUN pdf_text_xy_dec ("Spdf","Referens",iLeftMarg + 5,dY).
    ELSE
      RUN pdf_text_xy_dec ("Spdf","Referanse",iLeftMarg + 5,dY).
    RUN pdf_text_xy_dec ("Spdf",":",iLeftMarg + 70,dY).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("Referanse"):BUFFER-VALUE),iLeftMarg + 75,dY).*/

    dY = dY - 13.


    RUN pdf_stroke_fill ("Spdf",.85,.85,.85).  
    RUN pdf_rect ("Spdf", iLeftMarg + 3, dY - 10, iLeftMarg + 720, 15,0.1).      /* Left,Bottom,Length,Height*/
    RUN pdf_stroke_fill ("Spdf",1.0,1.0,1.0).

    IF lStrKod = TRUE THEN
    DO:
      IF lCode39 = FALSE THEN
      DO:
/*        RUN pdf_load_font IN h_PDFinc ("Spdf","Code39","PDFinclude\samples\support\code39.ttf","PDFinclude\samples\support\code39.afm","").*/
        FILE-INFO:FILENAME = 'pdfinclude\samples\support\code39.ttf'.
        RUN pdf_load_font IN h_PDFinc ("Spdf","Code39",FILE-INFO:FULL-PATHNAME,REPLACE(FILE-INFO:FULL-PATHNAME,'.ttf','.afm'),"").
        ASSIGN lCode39 = TRUE.
      END.

      RUN  pdf_set_font ("Spdf","Code39",20.0).
      RUN pdf_set_parameter("Spdf","ScaleY","2.5").


      RUN  pdf_text_xy ("Spdf",cKOidEAN,630, 450).
      RUN pdf_set_parameter("Spdf","ScaleY","1").
    END.

    RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).

    ASSIGN cLogo = REPLACE(cLogo,".bmp",".jpg").

    IF cLogo <> "" AND SEARCH(cLogo) <> ? THEN
    DO:
      ASSIGN cBildNamn = "Logo".
      IF NOT CAN-FIND(FIRST TT_image
        WHERE TT_image.image_name = cBildNamn NO-LOCK) 
      THEN DO:
        RUN pdf_load_image ("Spdf",cBildNamn,cLogo).
        CREATE TT_image.
        ASSIGN TT_image.IMAGE_name = cBildNamn.
      END.
      iImageHeight = pdf_imageDim ("Spdf",cBildNamn,"HEIGHT").
      iImageWidth = pdf_imageDim ("Spdf",cBildNamn,"WIDTH").

      IF iImageWidth >= iImageHeight THEN
        ASSIGN iImageHeight = iLogoStr1 * (iImageHeight / iImageWidth)
               iImageWidth = iLogoStr1.
      ELSE
        ASSIGN iImageWidth = iLogoStr2 * (iImageWidth / iImageHeight)
               iImageHeight = iLogoStr2.

      RUN pdf_place_image ("Spdf",cBildNamn,iLogoCol,iLogoRow,iImageWidth,iImageHeight).       /* 570,100 */

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFSetPositioner) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSetPositioner Procedure 
PROCEDURE PDFSetPositioner :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN dColPos[1] = 45
       dColPos[2] = 120
       dColPos[3] = 260
       dColPos[4] = 400
       dColPos[5] = 450
       dColPos[6] = 475
       dColPos[7] = 520
       dColPos[8] = 600
       dColPos[9] = 650
       dColPos[10] = 700
       dColPos[11] = 780.
/*       dColPos[9] = 520
       dColPos[10] = 580 */
ASSIGN dColPos2[1] = 45
       dColPos2[2] = 120
       dColPos2[3] = 260
       dColPos2[4] = 400
       dColPos2[5] = 465
       dColPos2[6] = 500
       dColPos2[7] = 571
       dColPos2[8] = 637
       dColPos2[9] = 688
       dColPos2[10] = 738
       dColPos2[11] = 800.
/*       dColPos2[9] = 558
       dColPos2[10] = 580 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFSkrivRapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSkrivRapport Procedure 
PROCEDURE PDFSkrivRapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE iSidNr         AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iRadNr         AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cDetaljRad     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cDetaljRad1    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cDetaljRad2    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSumRad        AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE qH             AS HANDLE     NO-UNDO.
   DEFINE VARIABLE qL             AS HANDLE     NO-UNDO.
   DEFINE VARIABLE iAntLinjer     AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iAntNotatRader AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iCount2        AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cFakturaType   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cFakturaNr     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iBilagsType    AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cRefTxt        AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iKontrollRad   AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cLevKod        AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE dAvgFri        AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dAvgPlikt      AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dNetto         AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dAvrund        AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dRabatt        AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dMvakr         AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dFraktBel      AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE dForskudd      AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dRabatt%       AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dLinjeRabatt%  AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dLinjeRabattKr AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dLinjeBruttoKr AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dBruttoSum     AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dTotSum        AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dY             AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE cBeskr         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE cwrk           AS CHARACTER   NO-UNDO.

   DEFINE VARIABLE cLevstatus AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iExtraRad AS INTEGER    NO-UNDO. /* Om vi har iFormatKod = 2 skall vi lï¿½gga till vid summarad */
   DEFINE VARIABLE iSumRad AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cHlbl AS CHARACTER EXTENT 13 NO-UNDO.
   DEFINE VARIABLE cSlbl AS CHARACTER EXTENT 9 NO-UNDO.
   DEFINE VARIABLE lDec    AS DECIMAL    NO-UNDO.

   /* Hantering av rader fï¿½r olika layouter */
   /* 1 = Internationell, 2 = Postgiro */
   iFormatKod = 2.
   ASSIGN iKontrollrad = IF iFormatKod = 1 THEN 62 ELSE IF iFormatKod = 2 THEN 42 ELSE 62  /* 41 -> 42 */
          iExtraRad    = IF iFormatKod = 2 THEN 3 ELSE 0
          iSumRad = 42. 
   IF CAN-DO("SE,SVE",cSprak) THEN
     ASSIGN cHlbl[1] = "Lev.art.nr"    
            cHlbl[2] = "     "  /* "Beskr"*/
            cHlbl[3] = "Färg"    
            cHlbl[4] = "Leverans"    
            cHlbl[5] = "Str"       
            cHlbl[6] = "Antal"     
            cHlbl[7] = "Enhetspris"
            cHlbl[8] = "Nto.pris" /*"Bruttopris"*/
            cHlbl[9] = "Rabatt%"   
            cHlbl[10] = "Moms%"     
            cHlbl[11] = "Sum"
            cSlbl[1] = "Frakt" /*"Avgfri"*/
            cSlbl[2] = "Avgfri"
            cSlbl[3] = "Varukostnad" /*"Avgpl"*/
            cSlbl[4] = "Netto"
            cSlbl[5] = "Rabatt"
            cSlbl[6] = "Tot.rabatt%"
            cSlbl[7] = "Moms"
            cSlbl[8] = "Totalt"  /*"Förskott"*/
            cSlbl[9] = "Att betala".
   ELSE
     ASSIGN cHlbl[1] = "Lev.art.nr"    
            cHlbl[2] = "     "  /* "Beskr"*/
            cHlbl[3] = "Farve"    
            cHlbl[4] = "Levert"    
            cHlbl[5] = "Str"       
            cHlbl[6] = "Antall"     
            cHlbl[7] = "Enhetspris"
            cHlbl[8] = "Nto.pris" /*"Bruttopris"*/
            cHlbl[9] = "Rabatt%"   
            cHlbl[10] = " Mva%"     
            cHlbl[11] = "Sum"
            cSlbl[1] = "Frakt"  /*"Avgfri"*/
            cSlbl[2] = "Avgfri"
            cSlbl[3] = "Avgpl"
            cSlbl[4] = "Netto"
            cSlbl[5] = "Rabatt"
            cSlbl[6] = "Tot.rabatt%"
            cSlbl[7] = " Mva"
            cSlbl[8] = "Forskudd"
            cSlbl[9] = "  Å betale".

   CREATE QUERY qH.
   CREATE QUERY qL.
   qL:SET-BUFFERS(hTTLinjeBuff).
   qH:SET-BUFFERS(hTTHodeBuff).
   qH:QUERY-PREPARE("FOR EACH " + hTTHodeBuff:NAME).
   qH:QUERY-OPEN().
   
   RUN pdf_new ("Spdf",pcRappFil).
   RUN pdf_set_BottomMargin ("Spdf", 40).
   RUN pdf_set_PaperType ("Spdf","A4").
   RUN pdf_set_Orientation IN h_PDFinc ("Spdf","Landscape").
   RUN pdf_set_VerticalSpace ("Spdf",13).
   RUN pdf_set_LeftMargin ("Spdf",40).
   ASSIGN iLeftMarg = 40.
   ASSIGN iBottomMarg = 20.

   qH:GET-FIRST().
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE KOrdre_Id = " +
                   STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE + 
                        " BY KOrdreLinje.Leveringsdato BY KOrdreLinje.KOrdreLinjeNr")).

       DO iCount = 1 TO iAntEks:
           RUN PDFSetPositioner.
           ASSIGN dAvgFri = 0
                  dAvgPlikt = 0
                  dNetto = 0
                  dRabatt = 0
                  dBruttosum = 0
                  dMvakr = 0
                  dForskudd = 0
                  dFraktBel = 0
                  dTotSum = 0.

           ASSIGN iSidNr = 1.
           ASSIGN dY = pdf_PageHeight ("Spdf") - 55.
           RUN pdf_new_page ("Spdf").
           RUN PDFPageHeader (iSidNr,cStatusTxt,iCount > 1,INPUT-OUTPUT dY).
           DO iCount2 = 1 TO 11:
             RUN pdf_text_xy_dec ("Spdf",cHlbl[iCount2],dColPos[iCount2],dY - 5).
           END.
           dY = dY - 13.
           ASSIGN iRadNr     = 20. 
           IF iCount = 1 THEN
               qL:QUERY-OPEN().
           qL:GET-FIRST().
           ASSIGN iAntLinjer = 0.
           REPEAT WHILE NOT qL:QUERY-OFF-END:
             ASSIGN iAntLinjer = iAntLinjer + 1.
             IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE) <> "" THEN
                ASSIGN iAntLinjer = iAntLinjer + 1.
             IF TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE) <> "" THEN
                    iAntLinjer = iAntLinjer + 1.
             IF TRIM(hTTLinjeBuff:BUFFER-FIELD("RefNr"):BUFFER-VALUE) <> "0" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("RefTekst"):BUFFER-VALUE) <> "" THEN
                   iAntLinjer = iAntLinjer + 1.

             qL:GET-NEXT().
           END.
           qL:GET-FIRST().
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN cLevKod = ''.
               ASSIGN lDec = DECIMAL(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE) NO-ERROR.
               IF NOT ERROR-STATUS:ERROR THEN
               DO:
                 IF lDec > 0 THEN 
                   FIND ArtBas NO-LOCK WHERE 
                     ArtBas.ArtikkelNr = lDec NO-ERROR.
                 IF AVAILABLE ArtBas 
                   THEN cLevKod = ArtBas.LevKod. 
                 ASSIGN 
                    iRadNr     = iRadNr + 1
                    iAntLinjer = iAntLinjer - 1
                    dAvgFri    = dAvgFri   + IF hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE = 0 THEN
                                          ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2) ELSE 0.
                 IF hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE = cFraktNr THEN
                    ASSIGN dFraktBel = dFraktBel + ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2) - ROUND(hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE,2).
                 ELSE
                    ASSIGN dAvgPlikt  = dAvgPlikt + IF hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE <> 0 THEN
                                          ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2) - ROUND(hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE,2) ELSE 0.
                 ASSIGN dNetto     = dNetto    + ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2) - ROUND(hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE,2)
                    dAvrund    = dAvrund   + 0
                    dRabatt    = dRabatt   + (hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE * ROUND(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE,2)) - ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2)
                    dMvakr     = dMvakr    + ROUND(hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE,2)
                    dForskudd  = dForskudd + hTTLinjeBuff:BUFFER-FIELD("Depositum"):BUFFER-VALUE
                    dBruttoSum = dBruttoSum + hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE * hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE
                    dTotSum    = dTotSum   + ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2)
                    dLinjeBruttoKr = hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE * hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE.
                    dLinjeRabattKr = (dLinjeBruttoKr) - ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2).
               END.
               ELSE 
                 ASSIGN
                     iRadNr        = iRadNr + 1
                     dForskudd     = dForskudd + (ROUND(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,2) * -1)
                     cLevKod       = STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE)
                     dLinjeRabatt% = ROUND(dLinjeRabattKr / dLinjeBruttoKr * 100,1) NO-ERROR.
               dLinjeRabatt% = IF dLinjeRabatt% = ? THEN 0 ELSE dLinjeRabatt%.
               dY = dY - 13.
               RUN pdf_set_font ("Spdf", "Helvetica-Bold",8).
               iAntTkn = pdf_GetNumFittingChars ("Spdf",cLevKod,45,115).
               
               IF iAntTkn > LENGTH(cLevKod) THEN
                 RUN pdf_text_xy_dec ("Spdf",cLevkod,dColPos2[1],dY).
               ELSE
                 RUN pdf_text_xy_dec ("Spdf",SUBSTRING(cLevkod,1,iAntTkn),dColPos2[1],dY).
               ASSIGN cBeskr = STRING(hTTLinjeBuff:BUFFER-FIELD("Varetekst"):BUFFER-VALUE).
               iAntTkn = pdf_GetNumFittingChars ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("Varetekst"):BUFFER-VALUE),120,255).
               IF iAntTkn >= LENGTH(cBeskr) THEN
                 RUN pdf_text_xy_dec ("Spdf",cBeskr,dColPos2[2],dY).
               ELSE
                 RUN pdf_text_xy_dec ("Spdf",SUBSTRING(cBeskr,1,iAntTkn),dColPos2[2],dY).
               RUN pdf_set_font ("Spdf", "Helvetica",8).
               ASSIGN cBeskr = hTTLinjeBuff:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE.
               iAntTkn = pdf_GetNumFittingChars ("Spdf",cBeskr,260,395).
               IF iAntTkn >= LENGTH(cBeskr) THEN
                 RUN pdf_text_xy_dec ("Spdf",cBeskr,dColPos2[3],dY).
               ELSE
                 RUN pdf_text_xy_dec ("Spdf",SUBSTRING(cBeskr,1,iAntTkn),dColPos2[3],dY).
               IF hTTLinjeBuff:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE <> ? THEN
                 RUN pdf_text_xy_dec ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE),dColPos2[4],dY).
               ASSIGN cwrk = STRING(hTTLinjeBuff:BUFFER-FIELD("Storl"):BUFFER-VALUE).
               RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos2[5] - bredd(cwrk),dY).
               ASSIGN cwrk = STRING(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE).
               RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos2[6] - bredd(cwrk),dY).
               ASSIGN cwrk = STRING(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE,"->,>>>,>>9.99").
               RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos2[7] - bredd(cwrk),dY).
               ASSIGN cwrk = STRING(hTTLinjeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE,"->,>>>,>>9.99").
               RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos2[8] - bredd(cwrk),dY).
               ASSIGN cwrk = STRING(dLinjeRabatt%,"->>>,>>9.99").
               RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos2[9] - bredd(cwrk),dY).
               ASSIGN cwrk = STRING(hTTLinjeBuff:BUFFER-FIELD("Mva%"):BUFFER-VALUE,"->9.99").
               RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos2[10] - bredd(cwrk),dY).
               ASSIGN cwrk = STRING(hTTLinjeBuff:BUFFER-FIELD("NettoLinjeSum"):BUFFER-VALUE,"->,>>>,>>9.99").
               RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos2[11] - bredd(cwrk),dY).

               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE) <> "" THEN DO:
                 ASSIGN cRefTxt = IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE) <> "" 
                                     THEN TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE) ELSE "".
                 ASSIGN iRadNr     = iRadNr + 1
                        iAntLinjer = iAntLinjer - 1.
                 dY = dY - 13.
                 iAntTkn = pdf_GetNumFittingChars ("Spdf",cRefTxt,120,255).
                 IF iAntTkn >= LENGTH(cRefTxt) THEN
                   RUN pdf_text_xy_dec ("Spdf",cRefTxt,dColPos2[2],dY).
                 ELSE
                   RUN pdf_text_xy_dec ("Spdf",SUBSTRING(cRefTxt,1,iAntTkn),dColPos2[2],dY).
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE) <> "" THEN DO:
                 ASSIGN cRefTxt = IF TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE) <> "" 
                                     THEN TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE) ELSE "".
                 ASSIGN iRadNr     = iRadNr + 1
                        iAntLinjer = iAntLinjer - 1.
                 dY = dY - 13.
                 iAntTkn = pdf_GetNumFittingChars ("Spdf",cRefTxt,120,255).
                 IF iAntTkn >= LENGTH(cRefTxt) THEN
                   RUN pdf_text_xy_dec ("Spdf",cRefTxt,dColPos2[2],dY).
                 ELSE
                   RUN pdf_text_xy_dec ("Spdf",SUBSTRING(cRefTxt,1,iAntTkn),dColPos2[2],dY).
               END.                                    
               /* !! */
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("RefNr"):BUFFER-VALUE) <> "0" OR 
                  TRIM(hTTLinjeBuff:BUFFER-FIELD("RefTekst"):BUFFER-VALUE) <> "" THEN 
               DO:
                 ASSIGN cRefTxt = IF TRIM(hTTLinjeBuff:BUFFER-FIELD("RefNr"):BUFFER-VALUE) <> "0" 
                              THEN TRIM(hTTLinjeBuff:BUFFER-FIELD("RefNr"):BUFFER-VALUE) 
                              ELSE ""
                        cRefTxt = cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("RefTekst"):BUFFER-VALUE).
                 ASSIGN iRadNr     = iRadNr + 1
                        iAntLinjer = iAntLinjer - 1.
                 dY = dY - 13.
                 iAntTkn = pdf_GetNumFittingChars ("Spdf",cRefTxt,120,255).
                 IF iAntTkn >= LENGTH(cRefTxt) THEN
                   RUN pdf_text_xy_dec ("Spdf",cRefTxt,dColPos2[2],dY).
                 ELSE
                   RUN pdf_text_xy_dec ("Spdf",SUBSTRING(cRefTxt,1,iAntTkn),dColPos2[2],dY).
               END.

              /* Skall vi gï¿½ra sidbryt? */
               IF (dY - 13) < (iBottomMarg + 30) AND iAntLinjer > 2 THEN DO:
                 ASSIGN iSidNr = iSidNr + 1
                        iRadNr = 20. /* 22 */
                 ASSIGN dY = pdf_PageHeight ("Spdf") - 55.
                 RUN pdf_new_page ("Spdf").
                 RUN PDFPageHeader (iSidNr,cStatusTxt,iCount > 1,INPUT-OUTPUT dY).
                 DO iCount2 = 1 TO 11:
                   RUN pdf_text_xy_dec ("Spdf",cHlbl[iCount2],dColPos[iCount2],dY - 5).
                 END.
                 dY = dY - 13.
               END.
               qL:GET-NEXT().
             END.
             IF (dY - 13) < (iBottomMarg + 30) THEN DO:
               ASSIGN iSidNr = iSidNr + 1
                      iRadNr = 20. /* 22 */
               ASSIGN dY = pdf_PageHeight ("Spdf") - 55.
               RUN pdf_new_page ("Spdf").
               RUN PDFPageHeader (iSidNr,cStatusTxt,iCount > 1,INPUT-OUTPUT dY).
               DO iCount2 = 1 TO 11:
                 RUN pdf_text_xy_dec ("Spdf",cHlbl[iCount2],dColPos[iCount2],dY - 5).
               END.
               dY = dY - 13.
             END.
             IF (dY - 13) < (iBottomMarg + 30) THEN DO:
                 ASSIGN iSidNr = iSidNr + 1
                        iRadNr = 20. /* 22 */
                 ASSIGN dY = pdf_PageHeight ("Spdf") - 55.
                 RUN pdf_new_page ("Spdf").
                 RUN PDFPageHeader (iSidNr,cStatusTxt,iCount > 1,INPUT-OUTPUT dY).
                 DO iCount2 = 1 TO 11:
                   RUN pdf_text_xy_dec ("Spdf",cHlbl[iCount2],dColPos[iCount2],dY - 5).
                 END.
                 dY = dY - 13.
             END.
             IF BongText <> "" THEN
             DO:
               RUN pdf_set_font ("Spdf", "Helvetica-Bold",15).
               RUN pdf_text_xy_dec ("Spdf",BongText,iLeftMarg + 300,iBottomMarg + 55).
             END.
             RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
             RUN pdf_text_xy_dec ("Spdf",cKvittoTekst,iLeftMarg + 150,iBottomMarg + 40).
             RUN pdf_set_font ("Spdf", "Helvetica",8).
             dY = iBottomMarg + 30.
             RUN pdf_stroke_fill ("Spdf",.85,.85,.85). 
             RUN pdf_rect ("Spdf", iLeftMarg + 5, dY - 8, iLeftMarg + 720, 15,0.1).      /* Left,Bottom,Length,Height*/
             RUN pdf_stroke_fill ("Spdf",1.0,1.0,1.0).
             ASSIGN dColPos[1] = 70
                    dColPos[2] = 120
                    dColPos[3] = 220
                    dColPos[4] = 300
                    dColPos[5] = 350
                    dColPos[6] = 400
                    dColPos[7] = 500
                    dColPos[8] = 550
                    dColPos[9] = 755.
             RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
             DO iCount2 = 1 TO 8:
               RUN pdf_text_xy_dec ("Spdf",cSlbl[iCount2],dColPos[iCount2],dY - 5).
             END.
             IF (dTotSum - dForskudd) <> 0 THEN
               RUN pdf_text_xy_dec ("Spdf",cSlbl[9],dColPos[9],dY - 5).
             RUN pdf_set_font ("Spdf", "Helvetica",10).
             dY = dY - 20.
             ASSIGN cwrk = STRING(dFraktBel,"->,>>>,>>9.99").
             RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos[1] - 20,dY).
             ASSIGN cwrk = STRING(dAvgFri,"->,>>>,>>9.99").
             RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos[2] - 20,dY).
             ASSIGN cwrk = STRING(dAvgPlikt,"->,>>>,>>9.99").
             RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos[3],dY).
             ASSIGN cwrk = STRING(dNetto,"->,>>>,>>9.99").
             RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos[4] - 25,dY).
             ASSIGN cwrk = STRING(dRabatt,"->,>>>,>>9.99").
             RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos[5] - 15,dY).
             ASSIGN cwrk = STRING(ROUND(dRabatt / dBruttosum * 100,2),"->>9.99").
             RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos[6] + 25,dY).
             ASSIGN cwrk = STRING(dMvakr,"->,>>>,>>9.99").
             RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos[7] - 25,dY).
             ASSIGN cwrk = STRING(dForskudd,"->,>>>,>>9.99").
             RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos[8] - 20,dY).
             IF (dTotSum - dForskudd) <> 0 THEN
             DO:
               ASSIGN cwrk = STRING(dTotSum - dForskudd,"->,>>>,>>9.99").
               RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
               RUN pdf_text_xy_dec ("Spdf",cwrk,dColPos[9] - 5,dY).
             END.
             RUN pdf_set_font ("Spdf", "Helvetica",10).
         END.
         qH:GET-NEXT().
         IF NOT qH:QUERY-OFF-END THEN
             PAGE.
     END.
     qH:QUERY-CLOSE().
     DELETE OBJECT qH.
     qL:QUERY-CLOSE().
     DELETE OBJECT qL.
     RUN pdf_close ("Spdf").
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PopulateTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateTT Procedure 
PROCEDURE PopulateTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    hHodeTH  = DYNAMIC-FUNCTION("getTempTable","get_kordrehode.p",cParaString,?).
    hLinjeTH = DYNAMIC-FUNCTION("getTempTable","get_kordrelinje.p",cParaString,?).
    hTTHodeBuff  = hHodeTH:DEFAULT-BUFFER-HANDLE.
    hTTLinjeBuff = hLinjeTH:DEFAULT-BUFFER-HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivHeader Procedure 
PROCEDURE SkrivHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iSidnr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER cStatus      AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lKopi        AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cRubrik      AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cKopiStr     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAdresse1    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAdresse2    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cPostNr      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cPostSted AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cLand AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cKOidEAN AS CHARACTER  NO-UNDO.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */
    ASSIGN cKopiStr = IF lKopi THEN "<C40>K O P I" ELSE ""
           cRubrik  = "O R D R E".

    cAdresse1    = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE).
    cAdresse2    = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE).
    cPostNr      = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE).
    cPostSted = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostSted"):BUFFER-VALUE).
    cLand        = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE).
    
    IF cAdresse1 = "" THEN DO:
        cAdresse1  = TRIM(hTTHodeBuff:BUFFER-FIELD("Adresse1"):BUFFER-VALUE).
        cAdresse2  = TRIM(hTTHodeBuff:BUFFER-FIELD("Adresse2"):BUFFER-VALUE).
        cPostNr    = TRIM(hTTHodeBuff:BUFFER-FIELD("PostNr"):BUFFER-VALUE).
        cPostSted  = TRIM(hTTHodeBuff:BUFFER-FIELD("PostSted"):BUFFER-VALUE).
        cLand      = TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE).
    END.
    ASSIGN cKOidEAN = STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE).
/*            cKOidEAN = FixChkEAN(FILL("",12 - LENGTH(cKOidEAN)) + cKOidEAN). */
    PUT UNFORMATTED
/*         "<R1><C60>1<R2><C60>2<R3><C60>3<R4><C60>4<R5><C60>5" */
        "<AT=5,> "   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
/*       "<R+.8,><C57><P12><B><RIGHT=C+10>" cRubrik  "<P10></B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */ */
        "<R+.8,><C" (STRING(60 - ROUND(LENGTH(cRubrik) / 2,0))) "><P12><B>" cRubrik  "<C" STRING(111 - LENGTH(cStatus)) ">" cStatus "<P10></B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
        /*  "<R+.8> */ "<P10><B><C6>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE) cKopiStr
      "<R+.7><C6><P7>" "Adresse" "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse1"):BUFFER-VALUE) + 
                                           (IF TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse2"):BUFFER-VALUE) <> '' 
                                              THEN ', ' + TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse2"):BUFFER-VALUE)
                                              ELSE '')
      "<R+.7><C6><P7>" "Postnr" "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPoststed"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "Telefon"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefon"):BUFFER-VALUE)  "<C26>Bank <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "Telefaks" "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefaks"):BUFFER-VALUE) "<C26>Postgiro <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "E-post:"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaEPost"):BUFFER-VALUE)
      "<R+.7><C6><P7>"   "URL:"     "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaURLAdresse"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "Org.nr"   "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaOrganisasjonsNr"):BUFFER-VALUE)
    /* Kundeadress */
      "<AT=40,><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE)
      "<R+1><C8><P10>" cAdresse1
      (IF cAdresse2 <> "" THEN "<R+1><C8><P10>" + cAdresse2 ELSE "")
      "<R+1><C8><P10>" cPostNr " " cPoststed
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE)
      "<R+1><C6><P7>LevAdresse : <C12.5>" TRIM(hTTHodeBuff:BUFFER-FIELD("LevAdresse1"):BUFFER-VALUE)
      (IF TRIM(hTTHodeBuff:BUFFER-FIELD("LevAdresse2"):BUFFER-VALUE) <> "" THEN ", " + TRIM(hTTHodeBuff:BUFFER-FIELD("LevAdresse2"):BUFFER-VALUE) ELSE "")
      "<R+.7><C12.5><P7>" TRIM(hTTHodeBuff:BUFFER-FIELD("LevPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("LevPostSted"):BUFFER-VALUE)
      "<R+.7><C12.5><P7>" TRIM(hTTHodeBuff:BUFFER-FIELD("LevLand"):BUFFER-VALUE)
    /* Referenser */
      "<AT=70,><C6><P7>Vår ref<C12>: "     TRIM(hTTHodeBuff:BUFFER-FIELD("VaarRef"):BUFFER-VALUE) 
      "<R+.7><C6><P7>Deres ref<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("DeresRef"):BUFFER-VALUE)
      "<R+.7><C6><P7>Referanse<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("Referanse"):BUFFER-VALUE)
    "<AT=100><C.1> " SKIP
    /* Faktura header info */
    "<R4><C50><P7>" "Side"               "<C60><P7>: "  STRING(iSidNr) SKIP
    "<R4.7><C50><P7>" "Ordrenr"          "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE) ELSE "") SKIP
    "<R5.4><C50><P7>" "Kundenr"          "<C60><P7>: "  STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE) SKIP
        /*
        "<R6.1><C50><P7>" "Prosjekt"         "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE) ELSE "") SKIP
        "<R6.8><C50><P7>" "Leveringsform"   "<C60><P7>: "  /* hTTHodeBuff:BUFFER-FIELD("LevFormMetode"):BUFFER-VALUE  */ SKIP
        "<R7.5><C50><P7>" "Lev.betingelser" "<C60><P7>: "  /* hTTHodeBuff:BUFFER-FIELD("LevFormBeskrivelse"):BUFFER-VALUE  */ SKIP
        "<R8.2><C50><P7>" "Valuta"          "<C60><P7>: "  hTTHodeBuff:BUFFER-FIELD("ValKod"):BUFFER-VALUE SKIP
    "<R8.9><C50><P7>" "Bet.betingelser"   "<C60><P7>: "  /* hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE */ SKIP
    "<R9.6><C50><P7>" "Forfallsdato"    "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE)) SKIP
         */.
     
/*     "<R10.9><C50><P7>" "KID"             "<C60><P7>: "  /* IF hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE > 0 THEN hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE ELSE "" */ SKIP */
/*     "<R11.6><C50><P7>" "Fakturadato"     "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE)) SKIP */
/*     "<R12.3><C50><P7>" "Bet.betingelser"   "<C60><P7>: "  /* hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE */ SKIP                                                                                */
/*     "<R13><C50><P7>" "Forfallsdato"    "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE)) SKIP. */
    PUT UNFORMATTED "<USE#1>" IF lFullRapport THEN hTTHodeBuff:BUFFER-FIELD("VerkstedMerknad"):BUFFER-VALUE ELSE ""  ".</USE>".
                                                                                                                        .
PUT UNFORMATTED
   "<AT=" STRING(16) "," STRING(250) "><#2><AT=+12,+38>"
     "<BARCODE#2,TYPE=39,CHECKSUM=none,VALUE=" cKOidEAN ">"
       .
    PUT UNFORMATTED "<R11><C50><FROM><R19><C111><RECT>" SKIP.
    PUT UNFORMATTED "<R2><C5><FROM><R2><C113><LINE>" SKIP
                    "<R46><C5><FROM><R46><C113><LINE>" SKIP
                    "<R2><C5><FROM><R46><C5><LINE>" SKIP
                    "<R2><C113><FROM><R46><C113><LINE>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivHeaderOld) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivHeaderOld Procedure 
PROCEDURE SkrivHeaderOld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iSidnr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER cFaktNr AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cFakturaType AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lKopi        AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cKopiStr     AS CHARACTER  NO-UNDO.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */
    ASSIGN
/*          cFaktNr = " " + STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) */
/*            cFaktNr = IF cFaktNr = ? THEN "" ELSE cFaktNr                            */
           cKopiStr = IF lKopi THEN "<C40>K O P I" ELSE "".
    PUT UNFORMATTED
      "<R1><C57><P10><B><RIGHT=C+20>" cFakturatype " " cFaktNr "</B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<R1.8><P10><B><C6>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE) cKopiStr
      "<R2.5><C6><P7>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse1"):BUFFER-VALUE) SKIP
      "<R3.2><C6><P7>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPoststed"):BUFFER-VALUE) SKIP
      "<R3.9><C6><P7>" "Telefon"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefon"):BUFFER-VALUE)  "<C26>Bank <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE) SKIP
      "<R4.6><C6><P7>" "Telefaks" "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefaks"):BUFFER-VALUE) "<C26>Postgiro <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE) SKIP
      "<R5.3><C6><P7>" "E-post:"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaEPost"):BUFFER-VALUE) SKIP
      "<R6><C6><P7>"   "URL:"     "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaURLAdresse"):BUFFER-VALUE) SKIP
      "<R6.7><C6><P7>" "Org.nr"   "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaOrganisasjonsNr"):BUFFER-VALUE) SKIP
    /* Kundeadress */
      "<R10><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE)
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE)
      (IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN "<R+1><C8><P10>" + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) ELSE "")
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE)
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE)
    /* Referenser */
      "<R17><C6><P7>Vår ref<C12>: "     TRIM(hTTHodeBuff:BUFFER-FIELD("VaarRef"):BUFFER-VALUE)   SKIP
      "<R17.7><C6><P7>Deres ref<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("DeresRef"):BUFFER-VALUE)  SKIP
      "<R18.4><C6><P7>Referanse<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("Referanse"):BUFFER-VALUE) SKIP
    "<R23><C.1>___" SKIP
    /* Faktura header info */
    "<R6><C50><P7>" "Side"               "<C60><P7>: "  STRING(iSidNr) SKIP
    "<R6.7><C50><P7>" "Ordrenr"          "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE) ELSE "") SKIP
    "<R7.4><C50><P7>" "Kundenr"          "<C60><P7>: "  STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE) SKIP
    "<R8.1><C50><P7>" "Prosjekt"         "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE) ELSE "") SKIP
    "<R8.8><C50><P7>" "Leveringsform"   "<C60><P7>: "  hTTHodeBuff:BUFFER-FIELD("LevFormMetode"):BUFFER-VALUE SKIP
    "<R9.5><C50><P7>" "Lev.betingelser" "<C60><P7>: "  hTTHodeBuff:BUFFER-FIELD("LevFormBeskrivelse"):BUFFER-VALUE SKIP
    "<R10.2><C50><P7>" "Valuta"          "<C60><P7>: "  hTTHodeBuff:BUFFER-FIELD("ValKod"):BUFFER-VALUE SKIP
    "<R10.9><C50><P7>" "KID"             "<C60><P7>: "  IF hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE > 0 THEN hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE ELSE "" SKIP
    "<R11.6><C50><P7>" "Fakturadato"     "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE)) SKIP
    "<R12.3><C50><P7>" "Bet.betingelser"   "<C60><P7>: "  hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE SKIP
    "<R13><C50><P7>" "Forfallsdato"    "<C60><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE)) SKIP.
    PUT UNFORMATTED "<USE#1>" hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE "</USE>".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivPostGiro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivPostGiro Procedure 
PROCEDURE SkrivPostGiro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     DEFINE INPUT  PARAMETER iSidnr       AS INTEGER    NO-UNDO. */
/*     DEFINE INPUT  PARAMETER cFaktNr AS CHARACTER  NO-UNDO.      */
/*     DEFINE INPUT  PARAMETER cFakturaType AS CHARACTER  NO-UNDO. */
/*     DEFINE INPUT  PARAMETER lKopi        AS LOGICAL    NO-UNDO. */
    DEFINE        VARIABLE  cKopiStr       AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dTotalt        AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  cBankPG        AS CHARACTER  NO-UNDO.
    ASSIGN dTotalt = DECI(hTTHodeBuff:BUFFER-FIELD("Totalt"):STRING-VALUE)
           cBankPG = TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE).
    IF cBankPG = "" THEN
        ASSIGN cBankPG = TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE).
    PUT UNFORMATTED
/*       "<R44.4><P8><C8><P10><B>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE) */
/*        "<AT=180,15> " */
/*        "<R44.4><P8><C8><P10><B>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE) */
        "<AT=188,22><P8><C8><P10><B>" TRIM(cBankPG)
        "<AT=,90><RIGHT=C+8>" STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99")
/*        "<R44.4><P8><C8><P10><B>" TRIM(cBankPG)                                                    */
/*        "<C35><RIGHT=C+8>" STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99") */
       "<AT=200,175>" TRIM(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE)
/*         "<R47.4><C68>" TRIM(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE) */
       "<AT=207,22>" "Kundenr"    "<C17>: "  STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE)
       "<R+1><AT=,22>"   "Faktura"    "<C17>: "  STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE)
       "<R+1><AT=,22>"   "Fakturadato" "<C17>: " STRING(hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE)
       "<AT=236,22>" TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE)
       "<R+1><AT=,22>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE)
       (IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN "<R+1><AT=,22><P10>" + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) ELSE " ")
       "<R+1><AT=,22>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE)
       "<R+1><AT=,22>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE)
       "<AT=236,120>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE) 
       "<R+1><AT=,120>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse1"):BUFFER-VALUE)
       "<R+2><AT=,120>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPoststed"):BUFFER-VALUE)
      "<AT=273,82><RIGHT=C+8>" STRING(TRUNC(dTotalt,0),"->,>>>,>>9")
/*       "<AT=275,22><C32.3><RIGHT=C+8>" STRING(TRUNC(dTotalt,0),"->,>>>,>>9") */
      "<AT=,108>" STRING(100 * (dTotalt - TRUNC(dTotalt,0)),"99")
/*         "<C+3.5>" STRING(100 * (dTotalt - TRUNC(dTotalt,0)),"99") */
      "<AT=,133>" cBankPG "</B>".
/*       "<C50>" cBankPG "</B>". */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

    RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION. /* bredd */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fixChkEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fixChkEAN Procedure 
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Räknar ut checksiffra för ean EAN-kod - parameter utan chksiffra
              i.e 12 läng
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

&IF DEFINED(EXCLUDE-getRapPrinter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRapPrinter Procedure 
FUNCTION getRapPrinter RETURNS CHARACTER
  ( INPUT ipcPrinter AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN IF ipcPrinter <> "" THEN ipcPrinter ELSE
      IF DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER") <> "" THEN
          DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER") ELSE SESSION:PRINTER-NAME.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

