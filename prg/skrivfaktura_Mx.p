&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR cParaString AS CHARACTER INIT "1050000001|" NO-UNDO.
    DEFINE VAR lDirekte    AS LOGICAL    NO-UNDO.
    DEFINE VAR cPrinter    AS CHARACTER  NO-UNDO.
    DEFINE VAR iAntEks     AS INTEGER    NO-UNDO.
    DEFINE VAR cMailAdress AS CHARACTER  NO-UNDO.
    DEFINE VAR iFormatKod  AS INTEGER    NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER cParaString AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER lDirekte    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER cPrinter    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iAntEks     AS INTEGER    NO-UNDO.
    DEFINE INPUT PARAMETER cMailAdress AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER iFormatKod  AS INTEGER    NO-UNDO.
&ENDIF


DEFINE VARIABLE cFirma AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE hHodeTH      AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLinjeTH     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTTHodeBuff  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTTLinjeBuff AS HANDLE     NO-UNDO.
DEFINE VARIABLE iPageHeight AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageWidth  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLeftCol    AS INTEGER     NO-UNDO.
DEFINE VARIABLE qH             AS HANDLE     NO-UNDO.
DEFINE VARIABLE qL             AS HANDLE     NO-UNDO.
DEFINE VARIABLE iColLbl AS INTEGER    EXTENT 7  NO-UNDO. /* sätts i SkrivRapportPDF */
DEF VAR cSkrivKIDnr AS CHAR NO-UNDO.

DEFINE TEMP-TABLE TT_RapportRader NO-UNDO
    FIELD iPageNum AS INTEGER  /* Sidnr */
    FIELD iColPage AS INTEGER  /* Hantering av 'för många cols' */
    FIELD iRadNum  AS INTEGER
    FIELD cRadData AS CHARACTER
    INDEX RadNum iPageNum iColPage iRadNum.
    
DEFINE TEMP-TABLE TT_mva NO-UNDO
    FIELD mva%      AS DECI
    FIELD mvakr     AS DECI
    FIELD linjesum AS DECI
    INDEX mva% IS PRIMARY UNIQUE mva%.

{runlib.i}

{ pdf_inc.i "THIS-PROCEDURE"}

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
    RETURN.
{syspara.i 19 100 5 cSkrivKIDnr}
RUN PopulateTT.
IF iFormatKod = 3 THEN
    RUN SkrivRapportPDF. /* svensk layout */
ELSE
    RUN SkrivRapport.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFooter Procedure 
PROCEDURE PageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCol   AS INTEGER    EXTENT 8  NO-UNDO.
  DEFINE VARIABLE cLabel AS CHARACTER  EXTENT 8  NO-UNDO.
  DEFINE VARIABLE dY     AS DECIMAL              NO-UNDO.
  DEFINE VARIABLE iY AS INTEGER     NO-UNDO.
  ASSIGN iCol[1] = iLeftCol
         iCol[2] = 190
         iCol[3] = 305
         iCol[4] = 425
         iCol[5] = 50
         iCol[6] = 190
         iCol[7] = 305
         iCol[8] = 425.
  ASSIGN cLabel[1] = "Adress"
         cLabel[2] = "Telefon"
         cLabel[3] = "PlusGiro"
         cLabel[4] = "Organisationsnr"
         cLabel[5] = ""
         cLabel[6] = "Fax"
         cLabel[7] = "Bankgiro"
         cLabel[8] = "".
  /* Notat */
  dY = 120.
/*   PUT UNFORMATTED "<USE#1>" hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE "</USE>". */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
/*   RUN pdf_text_xy_dec ("Spdf","Notat",iCol[1],dY). */
/*   RUN pdf_set_TextY("Spdf",120).                                                                                                                                                                                           */
/*   RUN pdf_Wrap_Text ("Spdf","Vid betalning efter förfallodatum debiteras dröjsmålsränta med  2% per månad. Ägarförbehåll: levererade varor förblir säljarens egendom till full betalning skett.",15,130,"left",OUTPUT iY). */
/* /*   RUN pdf_Wrap_Text ("Spdf",hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE,15,75,"left",OUTPUT iY). */                                                                                                                 */
/*   PUT UNFORMATTED "<USE#1>" hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE "</USE>". */
dY = 75.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_text_xy_dec ("Spdf",cLabel[1],iCol[1],dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[2],iCol[2],dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[3],iCol[3],dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[4],iCol[4],dY).
  dY = 50.
/*   RUN pdf_text_xy_dec ("Spdf",cLabel[5],iCol[5]),dY). */
  RUN pdf_text_xy_dec ("Spdf",cLabel[6],iCol[6],dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[7],iCol[7],dY).
/*   RUN pdf_text_xy_dec ("Spdf",cLabel[8],iCol[8]),dY). */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
  dY = 65.
  RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse1"):BUFFER-VALUE),iCol[1],dY).
  RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefon"):BUFFER-VALUE),iCol[2],dY).
  RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE),iCol[3],dY).
  RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaOrganisasjonsNr"):BUFFER-VALUE),iCol[4],dY).
  dY = 40.
  RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostNr"):BUFFER-VALUE) + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPoststed"):BUFFER-VALUE),iCol[5],dY).
  RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefaks"):BUFFER-VALUE),iCol[6],dY).
  RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE),iCol[7],dY).
  RUN pdf_text_xy_dec ("Spdf","Innehar F-skattebevis",iCol[8],dY).

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
    hHodeTH  = DYNAMIC-FUNCTION("getTempTable","get_fakturahode.p",cParaString,?).
    hLinjeTH = DYNAMIC-FUNCTION("getTempTable","get_fakturalinje.p",cParaString,?).
    hTTHodeBuff  = hHodeTH:DEFAULT-BUFFER-HANDLE.
    hTTLinjeBuff = hLinjeTH:DEFAULT-BUFFER-HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RitaRamar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RitaRamar Procedure 
PROCEDURE RitaRamar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO:
               /* Färgen  */
/*         RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,1.0,1.0). */
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",.92,.92,.92).
        RUN pdf_rect IN h_PDFinc ("Spdf", 300, iPageHeight - 45, 255, 33,0.1).
        RUN pdf_rect IN h_PDFinc ("Spdf", iLeftcol - 10, 555, 515, 20,0.1).
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,1.0,1.0).
        RUN pdf_rect IN h_PDFinc ("Spdf", 300, iPageHeight - 85, 155, 33,0.1). /* faktnr */
        RUN pdf_rect IN h_PDFinc ("Spdf", 460, iPageHeight - 85, 95, 33,0.1). /* datumram */
        RUN pdf_rect IN h_PDFinc ("Spdf", iLeftcol - 10, 145, 515, 400,0.1).
        RUN pdf_rect IN h_PDFinc ("Spdf", iLeftcol - 10, 85, 515, 50,0.1).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivAttBetala) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivAttBetala Procedure 
PROCEDURE SkrivAttBetala :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCol   AS INTEGER    EXTENT 5  NO-UNDO.
  DEFINE VARIABLE cLabel AS CHARACTER  EXTENT 5  NO-UNDO.
  DEFINE VARIABLE dY     AS DECIMAL              NO-UNDO.
  ASSIGN iCol[1] = 130
         iCol[2] = 295
/*          iCol[3] = 340 */
         iCol[4] = 400
         iCol[5] = 550.
  ASSIGN cLabel[1] = "Netto"
         cLabel[2] = "Exkl moms"
/*          cLabel[3] = "Moms %"  Moms% meningslös vid flera momssatser  */
         cLabel[4] = "Moms kr"
         cLabel[5] = "ATT BETALA".
  dY = 180.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_text_xy_dec ("Spdf",cLabel[1],iCol[1] - bredd(cLabel[1]),dY).
  RUN pdf_text_xy_dec ("Spdf",cLabel[2],iCol[2] - bredd(cLabel[2]),dY).
/*   RUN pdf_text_xy_dec ("Spdf",cLabel[3],iCol[3] - bredd(cLabel[3]),dY). */
  RUN pdf_text_xy_dec ("Spdf",cLabel[4],iCol[4] - bredd(cLabel[4]),dY).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cLabel[5],iCol[5] - bredd(cLabel[5]),dY).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivColLabels) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivColLabels Procedure 
PROCEDURE SkrivColLabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iPage AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cLabel  AS CHARACTER  EXTENT 7  NO-UNDO.
/*   ASSIGN iColLbl[1] = iLeftCol */
/*          iColLbl[2] = 130      */
/*          iColLbl[3] = 317      */
/*          iColLbl[4] = 320      */
/*          iColLbl[5] = 430      */
/*          iColLbl[6] = 474      */
/*          iColLbl[7] = 545.     */
  ASSIGN cLabel[1] = "Artnr"
         cLabel[2] = "Benämning"
         cLabel[3] = "Lev ant"
         cLabel[4] = "Enh"
         cLabel[5] = "á-pris"
         cLabel[6] = "Rabatt"
         cLabel[7] = "Summa".
  iPage = pdf_Page ("Spdf").
  dY = iPageHeight - 280.
 
 RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
 RUN pdf_text_xy_dec ("Spdf",cLabel[1],iColLbl[1],dY).
 RUN pdf_text_xy_dec ("Spdf",cLabel[2],iColLbl[2],dY).
 RUN pdf_text_xy_dec ("Spdf",cLabel[3],iColLbl[3] - bredd(cLabel[3]),dY).
 RUN pdf_text_xy_dec ("Spdf",cLabel[4],iColLbl[4],dY).
 RUN pdf_text_xy_dec ("Spdf",cLabel[5],iColLbl[5] - bredd("a-pris"),dY).
 RUN pdf_text_xy_dec ("Spdf",cLabel[6],iColLbl[6] - bredd(cLabel[6]),dY).
 RUN pdf_text_xy_dec ("Spdf",cLabel[7],iColLbl[7] - bredd(cLabel[7]),dY).


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
        "<AT=5,> "   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<R+.8,><C57><P10><B><RIGHT=C+20>" cFakturatype " " cFaktNr "</B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
    /*  "<R+.8> */ "<P10><B><C6>" TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE) cKopiStr
      "<R+.7><C6><P7>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaAdresse1"):BUFFER-VALUE)
      "<R+.7><C6><P7>"     TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPoststed"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "Telefon"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefon"):BUFFER-VALUE) /* "<C26>Bank <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE)     */
      "<R+.7><C6><P7>" "Telefaks" "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaTelefaks"):BUFFER-VALUE) /* "<C26>Postgiro <C32>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE) */
      "<R+.7><C6><P7>" "E-post:"  "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaEPost"):BUFFER-VALUE)
      "<R+.7><C6><P7>"   "URL:"     "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaURLAdresse"):BUFFER-VALUE)
      "<R+.7><C6><P7>" "Org.nr"   "<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaOrganisasjonsNr"):BUFFER-VALUE)
    /* Kundeadress */
      "<AT=40,><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE)
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE)
      (IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN "<R+1><C8><P10>" + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) ELSE "")
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE)
      "<R+1><C8><P10>" TRIM(hTTHodeBuff:BUFFER-FIELD("FaktLand"):BUFFER-VALUE)
    /* Referenser */
      "<AT=70,><C6><P7>Vår ref<C12>: "     TRIM(hTTHodeBuff:BUFFER-FIELD("VaarRef"):BUFFER-VALUE) 
      "<R+.7><C6><P7>Deres ref<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("DeresRef"):BUFFER-VALUE)
      "<R+.7><C6><P7>Referanse<C12>: " TRIM(hTTHodeBuff:BUFFER-FIELD("Referanse"):BUFFER-VALUE)
    "<AT=100><C.1>___" SKIP
    /* Faktura header info */
    "<R4><C50><P7>" "Side"               "<C62><P7>: "  STRING(iSidNr) SKIP
    "<R4.7><C50><P7>" "Ordrenr"          "<C62><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE) ELSE "") SKIP
    "<R5.4><C50><P7>" "Kundenr/Kort/Medlem"          "<C62><P7>: "  STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE) 
        
        (IF hTTHodeBuff:BUFFER-FIELD("Kundekort"):BUFFER-VALUE <> "" THEN " / " + hTTHodeBuff:BUFFER-FIELD("Kundekort"):BUFFER-VALUE ELSE " ")
        (IF hTTHodeBuff:BUFFER-FIELD("Medlemskort"):BUFFER-VALUE <> "" THEN  " / " + hTTHodeBuff:BUFFER-FIELD("Medlemskort"):BUFFER-VALUE ELSE "") SKIP
    "<R6.1><C50><P7>" "Prosjekt"         "<C62><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE > 0 THEN STRING(hTTHodeBuff:BUFFER-FIELD("KProsjektNr"):BUFFER-VALUE) ELSE "") SKIP
    "<R6.8><C50><P7>" "Leveringsform"   "<C62><P7>: "  hTTHodeBuff:BUFFER-FIELD("LevFormMetode"):BUFFER-VALUE SKIP
    "<R7.5><C50><P7>" "Lev.betingelser" "<C62><P7>: "  hTTHodeBuff:BUFFER-FIELD("LevFormBeskrivelse"):BUFFER-VALUE SKIP
    "<R8.2><C50><P7>" "Valuta"          "<C62><P7>: "  hTTHodeBuff:BUFFER-FIELD("ValKod"):BUFFER-VALUE SKIP
    "<R8.9><C50><P7>" (IF cSkrivKIDnr = '1' THEN "KID" ELSE "") "<C62><P7>: "  IF (hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE > 0 AND cSkrivKIDnr = '1') THEN hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE ELSE "" SKIP
    "<R9.6><C50><P7>" "Fakturadato"     "<C62><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE)) SKIP
    "<R10.3><C50><P7>" "Bet.betingelser" "<C62><P7>: "  hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE SKIP
    "<R11><C50><P7>" "Forfallsdato"     "<C62><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE)) SKIP
    "<R11.7><C50><P7>" "Bank"           "<C62><P7>: "  TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaBankKonto"):BUFFER-VALUE) SKIP
    "<R12.4><C50><P7>" "Postgiro"       "<C62><P7>: "  TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaPostgiro"):BUFFER-VALUE) .
    PUT UNFORMATTED "<USE#1>" hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE "</USE>".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivHeaderPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivHeaderPDF Procedure 
PROCEDURE SkrivHeaderPDF :
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
    DEFINE        VARIABLE  dDato    AS DATE        NO-UNDO.
    DEFINE        VARIABLE  cStr         AS CHARACTER   NO-UNDO.
    DEFINE        VARIABLE  iMittCol     AS INTEGER  INIT 305   NO-UNDO.
    DEFINE VARIABLE iY AS INTEGER     NO-UNDO.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */

    ASSIGN cKopiStr = IF lKopi THEN "  K O P I A" ELSE "".
    
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",22).
    RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",26).
    RUN pdf_set_TextY("Spdf",iPageHeight - 38).
    RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE),7,25,"left",OUTPUT iY).

/*     RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE),iLeftCol,iPageHeight - 38). */
    RUN pdf_text_xy_dec ("Spdf",cFakturaType + cKopiStr,iMittCol,iPageHeight - 38).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_text_xy_dec ("Spdf","Faktnr",iMittCol,iPageHeight - 64).
    RUN pdf_text_xy_dec ("Spdf","Kundnr",iMittCol + 80,iPageHeight - 64).
    RUN pdf_text_xy_dec ("Spdf","Fakturadatum",470,iPageHeight - 64).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
    RUN pdf_text_xy_dec ("Spdf",cFaktNr,iMittCol,iPageHeight - 77).
    cStr = TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE)).    
    RUN pdf_text_xy_dec ("Spdf",cStr,iMittCol + 80,iPageHeight - 77).
    dDato = hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE.
    IF dDato = ? THEN
        cStr = "".
    ELSE
        cStr = STRING(YEAR(dDato),"9999") + "-" + STRING(MONTH(dDato),"99") + "-" + STRING(DAY(dDato),"99").
    RUN pdf_text_xy_dec ("Spdf",cStr,470,iPageHeight - 77).
    /* levadress label */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_text_xy_dec ("Spdf","Leveransadress",iLeftCol,iPageHeight - 100).
    /* faktadress label */
    RUN pdf_text_xy_dec ("Spdf","Fakturaadress",iMittCol,iPageHeight - 100).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
/*     /* levadress */                                                                                     */
/*     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).                                                                                                                            */
/*     RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE),iMittCol,iPageHeight - 115).                                                                           */
/*     RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE),iMittCol,iPageHeight - 130).                                                                   */
/*     IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN                                                                                                              */
/*         RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE),iMittCol,iPageHeight - 145).                                                               */
/*     RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE),iMittCol,iPageHeight - 160). */
/*     FIND kunde WHERE kunde.kundenr = hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE NO-LOCK NO-ERROR. */
/*     IF AVAIL kunde THEN DO:                                                                             */
/*                                                                                                         */
/*     END.                                                                                                */
    /* faktadress */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE),iMittCol,iPageHeight - 115).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE),iMittCol,iPageHeight - 130).
    IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN
        RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE),iMittCol,iPageHeight - 145).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE),iMittCol,iPageHeight - 160).

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    /* kundref txt */
    RUN pdf_text_xy_dec ("Spdf","Er referens",iLeftCol,iPageHeight - 201).
    RUN pdf_text_xy_dec ("Spdf","Ert ordernr",iLeftCol,iPageHeight - 218).
    RUN pdf_text_xy_dec ("Spdf","Leveransvillkor",iLeftCol,iPageHeight - 235).
    RUN pdf_text_xy_dec ("Spdf","Leverenssätt",iLeftCol,iPageHeight - 252).
    /* vår ref txt */
    RUN pdf_text_xy_dec ("Spdf","Vår referens",iMittCol,iPageHeight - 201).
    RUN pdf_text_xy_dec ("Spdf","Betalningsvillkor",iMittCol,iPageHeight - 218).
    RUN pdf_text_xy_dec ("Spdf","Förfallodatum",iMittCol,iPageHeight - 235).
    RUN pdf_text_xy_dec ("Spdf","Dröjsmålsränta",iMittCol,iPageHeight - 252).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("DeresRef"):BUFFER-VALUE),iLeftCol + 100,iPageHeight - 201).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("VaarRef"):BUFFER-VALUE),iMittCol + 100,iPageHeight - 201).
/*     RUN pdf_text_xy_dec ("Spdf","Ert ordernr",iLeftCol,iPageHeight - 218). */
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE),iMittCol + 100,iPageHeight - 218).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("LevFormBeskrivelse"):BUFFER-VALUE),iLeftCol + 100,iPageHeight - 235).
    dDato = hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE.
    IF dDato = ? THEN
        cStr = "".
    ELSE
        cStr = STRING(YEAR(dDato),"9999") + "-" + STRING(MONTH(dDato),"99") + "-" + STRING(DAY(dDato),"99").
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    RUN pdf_text_xy_dec ("Spdf",cStr,iMittCol + 100,iPageHeight - 235).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

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

      "<AT=,22>" (IF cSkrivKIDnr = '1' THEN STRING(hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE) ELSE '')
      "<AT=,108>" STRING(100 * (dTotalt - TRUNC(dTotalt,0)),"99")
/*         "<C+3.5>" STRING(100 * (dTotalt - TRUNC(dTotalt,0)),"99") */
      "<AT=,133>" cBankPG "</B>".
/*       "<C50>" cBankPG "</B>". */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivRapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapport Procedure 
PROCEDURE SkrivRapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE pcRappFil      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iSidNr         AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iRadNr         AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cDetaljRad     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSumRad        AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE qH             AS HANDLE     NO-UNDO.
   DEFINE VARIABLE qL             AS HANDLE     NO-UNDO.
   DEFINE VARIABLE iAntLinjer     AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iAntNotatRader AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cFakturaType   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cFakturaNr     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iBilagsType    AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cRefTxt        AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iKontrollRad   AS INTEGER    NO-UNDO.
   DEFINE VARIABLE dBruttoPris    AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dRabKr         AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dAntal         AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE iExtraRad AS INTEGER    NO-UNDO. /* Om vi har iFormatKod = 2 skall vi lägga till vid summarad */
   DEFINE VARIABLE cHlbl1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl2 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl3 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl4 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl5 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl6 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl7 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl8 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl9 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlblX1 AS CHARACTER  NO-UNDO. /* substitute tar bara 9 parametrar och vi har 110, vi gör en replace mha denna */
   DEFINE VARIABLE cNettoPrisX1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl2 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl3 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl4 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl5 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl6 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl7 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl8 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl9 AS CHARACTER  NO-UNDO.

   cDetaljRad = "><P8><C6><RIGHT=C+6>&1<C13>&2<C30>&3<C34><RIGHT=C+4>&4<C38><RIGHT=C+4>&5<C46><RIGHT=C+4>&6<C53><RIGHT=C+4>&10<C60><RIGHT=C+4>&7<C65><RIGHT=C+4>&8<C70><RIGHT=C+7>&9".
   cSumRad    = "<R@1.7><P8><C7><RIGHT=C+7>&1<C15><RIGHT=C+7>&2<C23><RIGHT=C+7>&3<C31><RIGHT=C+7>&4<C39><RIGHT=C+7>&5<C45><RIGHT=C+7>&6<C52><RIGHT=C+7>&7<C60><RIGHT=C+7>&8<C70><RIGHT=C+7>&9".

/*    iFormatkod = 2. */
   /* Hantering av rader för olika layouter */
   /* 1 = Internationell, 2 = Postgiro */
   ASSIGN iKontrollrad = IF iFormatKod = 1 THEN 61 ELSE IF iFormatKod = 2 THEN 34 ELSE 61  /* 34 */
          iExtraRad    = IF iFormatKod = 2 THEN 3 ELSE 0. 

   RUN GetTempFileName IN wLibHandle ("fakt", "xpr", OUTPUT pcRappFil).

   ASSIGN cHlbl1 = "Art.nr"
          cHlbl2 = "Beskr"
          cHlbl3 = "Levert"
          cHlbl4 = "Str"
          cHlbl5 = "Antall"
          cHlbl6 = "Enhetspris"
          cHlbl7 = "Rabatt%"
          cHlbl8 = "Mva%"
          cHlbl9 = "Sum"
          cHlblX1 = "Netto".
   ASSIGN cSlbl1 = "Avgfri"
          cSlbl2 = "Avgpl"
          cSlbl3 = "Netto"
          cSlbl4 = "Avrunding"
          cSlbl5 = "Rabatt"
          cSlbl6 = "Rabatt%"
          cSlbl7 = "Mva"
          cSlbl8 = "Forskudd"
          cSlbl9 = "Å betale".

   CREATE QUERY qH.
   CREATE QUERY qL.
   qL:SET-BUFFERS(hTTLinjeBuff).
   qH:SET-BUFFERS(hTTHodeBuff).
   qH:QUERY-PREPARE("FOR EACH " + hTTHodeBuff:NAME).
   qH:QUERY-OPEN().
/*    ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "rapp.xpr". */
   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   IF NOT lDirekte THEN DO:
       PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
       PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '>'.
       PUT CONTROL '<PREVIEW=ZoomToWidth>'.
   END.
   ELSE DO:
       ASSIGN cMailAdress = "".
       PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '>'.
   END.
   PUT UNFORMATTED "<ALIGN=BASE><FArial><UNITS=MM>" SKIP.
/*    PUT UNFORMATTED "<TOP=10mm><R1>" SKIP. */
   PUT UNFORMATTED "<R14><C40><#1><R18><C75><FRAME#1>" SKIP. /* <15>..<19>*/
/*    PUT UNFORMATTED "<R16><C40><#1><R21><C75><FRAME#1>" SKIP. */
   qH:GET-FIRST().
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE Faktura_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)).

       ASSIGN iBilagstype = hTTHodeBuff:BUFFER-FIELD("BilagsType"):BUFFER-VALUE.
       CASE iBilagstype:
           WHEN 1 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                              cFakturaType = STRING(cFakturaNr <> "","Faktura/Proformafaktura"). 
           WHEN 2 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                              cFakturaType = STRING(cFakturaNr <> "","Kreditnota"). 
           WHEN 5 THEN ASSIGN cFakturaNr   = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                              cFakturaType = "Utbetaling". 
           WHEN 10 THEN ASSIGN cFakturaNr   = ""
                               cFakturaType = "Betalingspåminnelse". 
           OTHERWISE ASSIGN cFakturaNr   = ""
                            cFakturaType = "". 
       END CASE.
       
       DO iCount = 1 TO iAntEks:
           iSidNr = 1.
           RUN SkrivHeader (iSidNr,cFakturaNr,cFakturaType,iCount > 1).
/* PUT UNFORMATTED "<AT=5,160>+5160<AT=10,160>+10160<AT=15,160>+15160<AT=20,160>+20160" SKIP. */
           ASSIGN iRadNr     = 20. /* 22 */
           PUT UNFORMATTED                                     /* 168,255,168 */
           SUBSTITUTE("<R&1><C6><FROM><R&2><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.
/*            PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,"Prodnr","Beskr","Levert","Str","Antall","Enhetspris","Rabatt","Mva","Sum") "</B>" SKIP. */
           PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(REPLACE(cDetaljRad,"&10",cHlblX1),cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) "</B>" SKIP.
           IF iCount = 1 THEN
               qL:QUERY-OPEN().
           qL:GET-FIRST().
           ASSIGN iAntLinjer = 0.
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN iAntLinjer = iAntLinjer + 1.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN
                   ASSIGN iAntLinjer = iAntLinjer + 1.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN
                   ASSIGN iAntLinjer = iAntLinjer + 1.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE) <> "" THEN
                   ASSIGN iAntLinjer = iAntLinjer + 1.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE) <> "" THEN
                   ASSIGN iAntLinjer = iAntLinjer + 1.
               qL:GET-NEXT().
           END.
           qL:GET-FIRST().
           REPEAT WHILE NOT qL:QUERY-OFF-END:

               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer - 1.
               ASSIGN dBruttoPris = ABS(hTTLinjeBuff:BUFFER-FIELD("Linjesum"):BUFFER-VALUE)
                      dRabKr      = ABS(hTTLinjeBuff:BUFFER-FIELD("TotalRabattKr"):BUFFER-VALUE)
                      dAntal      = ABS(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE)
                      dBruttoPris = (dRabKr + dBruttoPris) / dAntal
/*                       dBruttoPris = ((dAntal * dRabKr) + dBruttoPris) / dAntal */
                      /* TN 6/4-07 Endret slik at Pris feltet benyttes hvis det er utfyllt. Pris eks mva og eks rabatter. */
                      dBruttoPris = IF hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE > 0
                                      THEN hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE
                                      ELSE dBruttoPris
                      dBruttoPris = IF dBruttoPris = ? THEN 0 ELSE dBruttoPris.

         /*            PUT UNFORMATTED SUBSTITUTE(cDetaljRad,STRING(iRadNr), */
               cNettoPrisX1 = STRING(hTTLinjeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE,"->,>>>,>>9.99").
               PUT UNFORMATTED "<R+.9" SUBSTITUTE(REPLACE(cDetaljRad,"&10",cNettoPrisX1),
                   IF STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE) = "" THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE),
                   "<B>" + STRING(hTTLinjeBuff:BUFFER-FIELD("Varetekst"):BUFFER-VALUE) + "</B>",
                   IF hTTLinjeBuff:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE <> ? THEN STRING(hTTLinjeBuff:BUFFER-FIELD("Leveringsdato"):BUFFER-VALUE) ELSE " ",
                   STRING(hTTLinjeBuff:BUFFER-FIELD("Storl"):BUFFER-VALUE),
                   STRING(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE),
                   STRING(dBruttoPris,"->,>>>,>>9.99"),
/*                    STRING(hTTLinjeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE,"->,>>>,>>9.99"), */
                   STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE,"->>9.99"),
                   STRING(hTTLinjeBuff:BUFFER-FIELD("Mva%"):BUFFER-VALUE,"->9.99"),
                   STRING(hTTLinjeBuff:BUFFER-FIELD("Linjesum"):BUFFER-VALUE,"->,>>>,>>9.99")).
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   PUT UNFORMATTED "<I><R+.9" SUBSTITUTE(REPLACE(cDetaljRad,"&10","") + "</B>"," ",TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE))) "</I>".
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE)
                          cRefTxt = cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   PUT UNFORMATTED "<I><R+.9" SUBSTITUTE(REPLACE(cDetaljRad,"&10","") + "</B>"," ",cRefTxt) "</I>".
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   PUT UNFORMATTED "<I><R+.9" SUBSTITUTE(REPLACE(cDetaljRad,"&10","") + "</B>"," ",cRefTxt) "</I>".
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   PUT UNFORMATTED "<I><R+.9" SUBSTITUTE(REPLACE(cDetaljRad,"&10","") + "</B>"," ",cRefTxt) "</I>".
               END.
                   
               /* Skall vi göra sidbryt? */
               IF iRadNr > iKontrollrad AND iAntLinjer > 2  THEN DO:
                   IF iCount = 1 AND iBilagsType = 1 AND iFormatKod = 2 AND iSidNr = 1 THEN DO:
                       RUN SkrivPostGiro.
                   END.
                   PAGE.
                   ASSIGN iSidNr = iSidNr + 1
                          iRadNr = 21. /* 22 */
                   RUN SkrivHeader (iSidNr,cFakturaNr,cFakturaType,iCount > 1).
                   PUT UNFORMATTED                                     /* 168,255,168 */
                   SUBSTITUTE("<R&1><P8><C6><FROM><R&2><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.
/*                    PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,"Prodnr","Beskr","Levert","Str","Antall","Enhetspris","Rabatt","Mva","Sum") "</B>" SKIP. */
                   PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(REPLACE(cDetaljRad,"&10",cHlblX1),cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) "</B>" SKIP.
               END.
               qL:GET-NEXT().
           END.
           PUT UNFORMATTED SUBSTITUTE("<R&1.7><P8><C6><FROM><R&2.7><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iKontrollrad + iExtraRad + 1),STRING(iKontrollrad + iExtraRad + 2)) SKIP.
/*            PUT UNFORMATTED "<B>" SUBSTITUTE(cSumRad,STRING(65),"Avgfri","Avgpl","Netto","Avrunding","Rabatt","Rabatt%","Mva","Sum inkl. mva") "</B>" SKIP. */
           PUT UNFORMATTED "<B>" REPLACE(SUBSTITUTE(cSumRad,cSlbl1,cSlbl2,cSlbl3,cSlbl4,cSlbl5,cSlbl6,cSlbl7,cSlbl8,cSlbl9),"@1",STRING(iKontrollrad + iExtraRad + 1)) "</B>" SKIP.
           PUT UNFORMATTED REPLACE(SUBSTITUTE(cSumRad,
                                      STRING(hTTHodeBuff:BUFFER-FIELD("AvgFriSalg"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("AvgPlSalg"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("TotalRabattKr"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("TotalRabatt%"):BUFFER-VALUE,"->>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE,"->,>>>,>>9.99"),
                                      STRING(hTTHodeBuff:BUFFER-FIELD("Forskuddsbetalt"):BUFFER-VALUE,"->,>>>,>>9.99"),"<B>" +
                                      STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99")),"@1",STRING(iKontrollrad + iExtraRad + 2)) + "</B>" SKIP.
/*            PUT UNFORMATTED REPLACE(SUBSTITUTE(cSumRad,                                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99"), /* avrund */                                  */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("TotalRabatt%"):BUFFER-VALUE,"->>9.99"),       */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99"),                                               */
/*                                       STRING(1000000,"->,>>>,>>9.99")),"@1",STRING(iKontrollrad + 3)) + "</B>" SKIP. */
           
           /* Här skrives postgiroblankett ut */
           IF iCount = 1 AND iBilagsType = 1 AND iFormatKod = 2 AND iSidNr = 1 THEN
               RUN SkrivPostGiro.
           IF iCount < iAntEks THEN
               PAGE.
       END.
       qH:GET-NEXT().
       IF NOT qH:QUERY-OFF-END THEN
           PAGE.
   END.
   qH:QUERY-CLOSE().
   DELETE OBJECT qH.
   qL:QUERY-CLOSE().
   DELETE OBJECT qL.
/*        PUT UNFORMATTED                                                                                                    */
/* "<R24><C6><P6><RIGHT=C+4>9801234<C13>Stiga bortennisracket<C43><C52><RIGHT=C+5>0<C59><RIGHT=C+10>0<C71><RIGHT=C+6>" SKIP  */
/* "<R25><C6><RIGHT=C+4>9801234<C13>Stiga bordtennisballer<C43><C52><RIGHT=C+5>0<C59><RIGHT=C+10>0<C71><RIGHT=C+6>"     SKIP */
/* "<R26><C6><RIGHT=C+4>9801234<C13>Craft shorts<C43><C52><RIGHT=C+5>0<C59><RIGHT=C+10>0<C71>"                SKIP.          */
   IF TRIM(cMailAdress) <> "" THEN DO:
       PUT UNFORMATTED "<MAILTO=TO:" + cMailAdress + ">". 
   END.
   OUTPUT CLOSE.
   RUN VisXprint.p (pcRappFil).
   OS-DELETE VALUE(pcRappFil).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivRapportPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapportPDF Procedure 
PROCEDURE SkrivRapportPDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dFaktura_id AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE cFilNavn    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE iBilagsType    AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cFakturaType   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cFakturaNr     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iSidNr AS INTEGER     NO-UNDO.
   DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.
   DEFINE VARIABLE iRadNr AS INTEGER     NO-UNDO.
   DEFINE VARIABLE cDetaljRad AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE dBruttoPris    AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dRabKr         AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dAntal         AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE iAntLinjer     AS INTEGER     NO-UNDO.
   DEFINE VARIABLE cRefTxt        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE iKontrollRad   AS INTEGER     NO-UNDO.
   DEFINE VARIABLE cTxt           AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE dY             AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE dYorg          AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE iY             AS INTEGER     NO-UNDO.
   DEFINE VARIABLE iAntMva        AS INTEGER     NO-UNDO.
   DEFINE VARIABLE cSpecLbl       AS CHARACTER EXTENT 4  NO-UNDO.
   DEFINE VARIABLE dSpecCol       AS DECI   EXTENT 4  NO-UNDO.
   DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
   
   /* används i SkrivColLabels och här */
   ASSIGN iLeftCol    = 50
          iColLbl[1] = iLeftCol
          iColLbl[2] = 130
          iColLbl[3] = 317
          iColLbl[4] = 320
          iColLbl[5] = 415
          iColLbl[6] = 474
          iColLbl[7] = 545.

   ASSIGN cSpecLbl[1] = "Exkl moms"
          cSpecLbl[2] = "Mva%"
          cSpecLbl[3] = "Moms kr"
          cSpecLbl[4] = "Öresavr".
   ASSIGN dSpecCol[1] = 150
          dSpecCol[2] = 210
          dSpecCol[3] = 270
          dSpecCol[4] = 330.

   CREATE QUERY qH.
   CREATE QUERY qL.
   qL:SET-BUFFERS(hTTLinjeBuff).
   qH:SET-BUFFERS(hTTHodeBuff).
   qH:QUERY-PREPARE("FOR EACH " + hTTHodeBuff:NAME).
   qH:QUERY-OPEN().
   qH:GET-FIRST().
   
   dY = 550.
   dFaktura_id = hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE.
   cFilNavn = SESSION:TEMP-DIR + "Faktura" + "_" + STRING(TIME) + ".pdf".
   RUN pdf_new ("Spdf",cFilNavn).
   RUN pdf_set_BottomMargin ("Spdf", 20).
   RUN pdf_set_PaperType ("Spdf","A4").
   iPageHeight = pdf_PageHeight ("Spdf").
   iPageWidth  = pdf_PageWidth ("Spdf").
   RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
   RUN pdf_set_Orientation ("Spdf","portrait").
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE Faktura_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)).

       ASSIGN iBilagstype = hTTHodeBuff:BUFFER-FIELD("BilagsType"):BUFFER-VALUE.
       CASE iBilagstype:
           WHEN 1 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                              cFakturaType = STRING(cFakturaNr <> "","Faktura/Proformafaktura"). 
           WHEN 2 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                              cFakturaType = STRING(cFakturaNr <> "","Kreditnota"). 
           WHEN 5 THEN ASSIGN cFakturaNr   = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                              cFakturaType = "Utbetaling". 
           WHEN 10 THEN ASSIGN cFakturaNr   = ""
                               cFakturaType = "Betalingspåminnelse". 
           OTHERWISE ASSIGN cFakturaNr   = ""
                            cFakturaType = "". 
       END CASE.
       
/*        DO iCount = 1 TO iAntEks: */
       DO:
           iCount = 1.
           iSidNr = 1.
           RUN pdf_new_page ("Spdf").
           RUN RitaRamar. /* pt kvar att göra */
           RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
           RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
           /* Här skriver vi ut notat. Först sätter vi Y */
           RUN pdf_set_TextY("Spdf",120).
/*            RUN pdf_Wrap_Text ("Spdf","Vid betalning efter förfallodatum debiteras dröjsmålsränta med  2% per månad. Ägarförbehåll: levererade varor förblir säljarens egendom till full betalning skett.",15,125,"left",OUTPUT iY). */
           RUN pdf_Wrap_Text ("Spdf",hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE,15,125,"left",OUTPUT iY).
           RUN PageFooter.
           ASSIGN iRadNr     = 21. /* 22 */
           RUN SkrivColLabels.
           
           /* Tømmer momsloggen */
           FOR EACH TT_Mva:
             DELETE TT_Mva.
           END.
           
           IF iCount = 1 THEN
               qL:QUERY-OPEN().
           qL:GET-FIRST().
           ASSIGN iAntLinjer = 0.
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN iAntLinjer = iAntLinjer + 1.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN
                   ASSIGN iAntLinjer = iAntLinjer + 1.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN
                   ASSIGN iAntLinjer = iAntLinjer + 1.
               FIND TT_Mva WHERE TT_Mva.mva% = dec(hTTLinjeBuff:BUFFER-FIELD("MVA%"):BUFFER-VALUE) NO-ERROR.
               IF NOT AVAIL TT_Mva THEN DO:
                   iAntMva = iAntMva + 1.
                   CREATE tt_Mva.
                   ASSIGN tt_Mva.mva% = dec(hTTLinjeBuff:BUFFER-FIELD("Mva%"):BUFFER-VALUE).
               END.
               ASSIGN TT_Mva.mvakr   = TT_Mva.mvakr + dec(hTTLinjeBuff:BUFFER-FIELD("Mvakr"):BUFFER-VALUE)
                      TT_Mva.linjesum = TT_Mva.linjesum + dec(hTTLinjeBuff:BUFFER-FIELD("Linjesum"):BUFFER-VALUE).
               qL:GET-NEXT().
           END.
           
           qL:GET-FIRST().
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer - 1.
               ASSIGN dBruttoPris = ABS(dec(hTTLinjeBuff:BUFFER-FIELD("Linjesum"):BUFFER-VALUE))
/*                       dRabKr      = ABS(hTTLinjeBuff:BUFFER-FIELD("TotalRabattKr"):BUFFER-VALUE) */
                      dAntal      = ABS(dec(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE))
/*                       dBruttoPris = (dRabKr + dBruttoPris) / dAntal */
/*                       dBruttoPris = ((dAntal * dRabKr) + dBruttoPris) / dAntal */
                      /* TN 6/4-07 Endret slik at Pris feltet benyttes hvis det er utfyllt. Pris eks mva og eks rabatter. */
                      dBruttoPris = IF dec(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE) > 0
                                      THEN dec(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE)
                                      ELSE dBruttoPris
                      dBruttoPris = IF dBruttoPris = ? THEN 0 ELSE dBruttoPris.
               dY = dY - 14.
               IF dY < 185 THEN DO:
                   iSidNr = iSidNr + 1.
                   RUN pdf_new_page ("Spdf").
                   RUN RitaRamar. /* pt kvar att göra */
                   RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
                   /* Här skriver vi ut notat. Först sätter vi Y */
                   RUN pdf_set_TextY("Spdf",120).
        /*            RUN pdf_Wrap_Text ("Spdf","Vid betalning efter förfallodatum debiteras dröjsmålsränta med  2% per månad. Ägarförbehåll: levererade varor förblir säljarens egendom till full betalning skett.",15,125,"left",OUTPUT iY). */
                   RUN pdf_Wrap_Text ("Spdf",hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE,15,125,"left",OUTPUT iY).
                   RUN PageFooter.
                   RUN SkrivColLabels.
                   dY = 550 - 14.
               END.
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
               RUN pdf_text_xy_dec ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE) + " - " + STRING(dY),iColLbl[1],dY).
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
               RUN pdf_text_xy_dec ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("Varetekst"):BUFFER-VALUE),iColLbl[2],dY).
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
               dAntal      = hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE.
               RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dAntal,"->>,>>9")),iColLbl[3] - bredd(TRIM(STRING(dAntal,"->>,>>9"))),dY).
/*                RUN pdf_text_xy_dec ("Spdf",cLabel[4],iColLbl[4],dY). */
               RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dBruttoPris,"->>,>>9.99")),iColLbl[5] - bredd(TRIM(STRING(dBruttoPris,"->>,>>9.99"))),dY).
               dRabKr = dAntal * (hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE - hTTLinjeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE).
               RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dRabKr,"->>,>>9.99")),iColLbl[6] - bredd(TRIM(STRING(dRabKr,"->>,>>9.99"))),dY).
               cTxt = TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("nettolinjesum"):BUFFER-VALUE,"->>,>>9.99")).
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).

               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   cRefTxt = TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE)).
                   dY = dY - 15.
                   RUN pdf_text_xy_dec ("Spdf",cRefTxt,iColLbl[2],dY).
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE)
                          cRefTxt = cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   dY = dY - 15.
                   RUN pdf_text_xy_dec ("Spdf",TRIM(cRefTxt),iColLbl[2],dY).
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   dY = dY - 15.
                   RUN pdf_text_xy_dec ("Spdf",TRIM(cRefTxt),iColLbl[2],dY).
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE) <> "" THEN DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("Varespesifikasjon"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   dY = dY - 15.
                   RUN pdf_text_xy_dec ("Spdf",TRIM(cRefTxt),iColLbl[2],dY).
               END.
               qL:GET-NEXT().
           END. /* fakturalinjeloop */
           /* skriv mva */
           dYorg = dY.
           dY = 153 + (iAntMva * 12).
           dY = dY + 4 + 12.
           IF dYorg < dY THEN DO:
               /* beklagar, men vi måste sidbryta */
               RUN pdf_new_page ("Spdf").
               RUN RitaRamar. /* pt kvar att göra */
               RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
               /* Här skriver vi ut notat. Först sätter vi Y */
               RUN pdf_set_TextY("Spdf",120).
    /*            RUN pdf_Wrap_Text ("Spdf","Vid betalning efter förfallodatum debiteras dröjsmålsränta med  2% per månad. Ägarförbehåll: levererade varor förblir säljarens egendom till full betalning skett.",15,125,"left",OUTPUT iY). */
               RUN pdf_Wrap_Text ("Spdf",hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE,15,125,"left",OUTPUT iY).
               RUN PageFooter.
               RUN SkrivColLabels.
           END.
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[1],dSpecCol[1] - bredd(cSpecLbl[1]),dY).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[2],dSpecCol[2] - bredd(cSpecLbl[2]),dY).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[3],dSpecCol[3] - bredd("Oresavr"),dY).
           IF hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE THEN
               RUN pdf_text_xy_dec ("Spdf",cSpecLbl[4],dSpecCol[4] - bredd(cSpecLbl[4]),dY).
           dY = dY - 12.
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
           
           FOR EACH tt_mva BREAK BY tt_mva.mva% :
               cTxt = STRING(tt_mva.linjesum - tt_mva.mvakr,"->,>>>,>>9.99").
               RUN pdf_text_xy_dec ("Spdf",cTxt,dSpecCol[1] - bredd(cTxt),dY).
               cTxt = STRING(tt_mva.mva%,"->,>>>,>>9.99").
               RUN pdf_text_xy_dec ("Spdf",cTxt,dSpecCol[2] - bredd(cTxt),dY).
               cTxt = STRING(tt_mva.mvakr,"->,>>>,>>9.99").
               RUN pdf_text_xy_dec ("Spdf",cTxt,dSpecCol[3] - bredd(cTxt),dY).
/*                cTxt = STRING(tt_mva.linjesum,"->,>>>,>>9.99").         */
/*                RUN pdf_text_xy_dec ("Spdf",cTxt,350 - bredd(cTxt),dY). */
               IF LAST-OF(tt_mva.mva%) AND hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE <> 0 THEN DO:
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE,"->,>>>,>>9.99"), */
                   cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE,"->,>>>,>>9.99").
                   RUN pdf_text_xy_dec ("Spdf",cTxt,dSpecCol[4] - bredd(cTxt),dY).
               END.
               dY = dY - 12.
           END.
/*            DEFINE TEMP-TABLE TT_mva NO-UNDO       */
/*                FIELD mva%      AS DECI            */
/*                FIELD mvakr     AS DECI            */
/*                FIELD exklmvakr AS DECI            */
/*                INDEX mva% IS PRIMARY UNIQUE mva%. */


           /* Skriv Att betala */
           dY =  180.
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
           cTxt = "ATT BETALA".
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).
           dY = dY - 15.
           cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99").
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).


/*            PUT UNFORMATTED SUBSTITUTE("<R&1.7><P8><C6><FROM><R&2.7><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iKontrollrad + iExtraRad + 1),STRING(iKontrollrad + iExtraRad + 2)) SKIP. */
/* /*            PUT UNFORMATTED "<B>" SUBSTITUTE(cSumRad,STRING(65),"Avgfri","Avgpl","Netto","Avrunding","Rabatt","Rabatt%","Mva","Sum inkl. mva") "</B>" SKIP. */                                */
/*            PUT UNFORMATTED "<B>" REPLACE(SUBSTITUTE(cSumRad,cSlbl1,cSlbl2,cSlbl3,cSlbl4,cSlbl5,cSlbl6,cSlbl7,cSlbl8,cSlbl9),"@1",STRING(iKontrollrad + iExtraRad + 1)) "</B>" SKIP.             */
/*            PUT UNFORMATTED REPLACE(SUBSTITUTE(cSumRad,                                                                                                                                          */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("AvgFriSalg"):BUFFER-VALUE,"->,>>>,>>9.99"),                                                                              */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("AvgPlSalg"):BUFFER-VALUE,"->,>>>,>>9.99"),                                                                               */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE,"->,>>>,>>9.99"),                                                                               */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE,"->,>>>,>>9.99"),                                                                             */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("TotalRabattKr"):BUFFER-VALUE,"->,>>>,>>9.99"),                                                                           */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("TotalRabatt%"):BUFFER-VALUE,"->>9.99"),                                                                                  */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE,"->,>>>,>>9.99"),                                                                                   */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("Forskuddsbetalt"):BUFFER-VALUE,"->,>>>,>>9.99"),"<B>" +                                                                  */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99")),"@1",STRING(iKontrollrad + iExtraRad + 2)) + "</B>" SKIP.                        */
           /* Här skrives postgiroblankett ut */
           IF iCount = 1 AND iBilagsType = 1 AND iFormatKod = 2 AND iSidNr = 1 THEN
               RUN SkrivPostGiro.
/*            IF iCount < iAntEks THEN */
/*                PAGE.                */
       END.
       qH:GET-NEXT().
/*        IF NOT qH:QUERY-OFF-END THEN */
/*            PAGE.                    */
   END.
   RUN pdf_close ("Spdf").
   IF lDirekte = FALSE THEN
       RUN browse2pdf\viewxmldialog.w (cFilNavn,"FAKTURA").
   ELSE DO ii = 1 TO iAntEks:
       OS-COMMAND SILENT VALUE(".\cmd\PrintPdf " + cFilnavn + ' "' + cPrinter + '"').
   END.

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

