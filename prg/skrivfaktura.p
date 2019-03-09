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
    DEFINE VAR cParaString AS CHARACTER INIT "1130000001|" NO-UNDO.
2/*    DEFINE VAR cParaString AS CHARACTER INIT "1070000412,1070000413,1070000414|" NO-UNDO.*/
/*    DEFINE VAR cParaString AS CHARACTER INIT "1090000260|" NO-UNDO.*/
    DEFINE VAR lDirekte    AS LOGICAL        NO-UNDO.
    DEFINE VAR cPrinter    AS CHARACTER      NO-UNDO.
    DEFINE VAR iAntEks     AS INTEGER        NO-UNDO.
    DEFINE VAR cMailAdress AS CHARACTER      NO-UNDO.
    DEFINE VAR iFormatKod  AS INTEGER INIT 3 NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER cParaString AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER lDirekte    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER cPrinter    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iAntEks     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER cMailAdress AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iFormatKod  AS INTEGER   NO-UNDO.
&ENDIF
                
DEFINE VARIABLE cFirma       AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE VARIABLE hHodeTH      AS HANDLE                   NO-UNDO.
DEFINE VARIABLE hLinjeTH     AS HANDLE                   NO-UNDO.
DEFINE VARIABLE hTTHodeBuff  AS HANDLE                   NO-UNDO.
DEFINE VARIABLE hTTLinjeBuff AS HANDLE                   NO-UNDO.
DEFINE VARIABLE iPageHeight  AS INTEGER                  NO-UNDO.
DEFINE VARIABLE iPageWidth   AS INTEGER                  NO-UNDO.
DEFINE VARIABLE iLeftCol     AS INTEGER                  NO-UNDO.
DEFINE VARIABLE qH           AS HANDLE                   NO-UNDO.
DEFINE VARIABLE qL           AS HANDLE                   NO-UNDO.
DEFINE VARIABLE iColLbl      AS INTEGER EXTENT 7         NO-UNDO. /* sætts i SkrivRapportPDF */
DEFINE VARIABLE cSkrivKIDnr  AS CHARACTER                NO-UNDO.
DEFINE VARIABLE iUtskrTyp    AS INTEGER                  NO-UNDO.
DEFINE VARIABLE cSprak       AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cLayout      AS CHARACTER                NO-UNDO.
DEFINE VARIABLE lNorLayout   AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE cFakturaTekst AS CHARACTER               NO-UNDO.
DEFINE VARIABLE cUtskrift    AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cCmd         AS CHARACTER                NO-UNDO.
DEFINE VARIABLE lMedMva      AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE lCode39      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE hJbApi       AS HANDLE NO-UNDO.
DEFINE VARIABLE bBareFil     AS LOG NO-UNDO.
DEFINE VARIABLE ocReturn     AS CHARACTER NO-UNDO.

DEFINE VARIABLE cEmail AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE TT_RapportRader NO-UNDO
    FIELD iPageNum AS INTEGER  /* Sidnr */
    FIELD iColPage AS INTEGER  /* Hantering av 'før mï¿½nga cols' */
    FIELD iRadNum  AS INTEGER
    FIELD cRadData AS CHARACTER
    INDEX RadNum iPageNum iColPage iRadNum.
    
DEFINE TEMP-TABLE TT_mva NO-UNDO
    FIELD mva%     AS DECIMAL
    FIELD mvakr    AS DECIMAL
    FIELD linjesum AS DECIMAL
    INDEX mva% IS PRIMARY UNIQUE mva%.

DEFINE TEMP-TABLE TT_Kvitto NO-UNDO
    FIELD B_Id      AS DECIMAL
    FIELD cKvittoNr AS CHARACTER  
    FIELD dRabatt   AS DECIMAL  
    FIELD dBelopp   AS DECIMAL
    FIELD cRefTxt   AS CHARACTER
    INDEX KvittoNr B_Id.

{ pdf_inc.i "THIS-PROCEDURE"}

/*{initjukebox.i} Kan ikke gjøres her. Kjør jukebox programmene direkte. Gjeller fill tt. */

  &ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd Procedure 
  FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER )  FORWARD.

  &ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRapPrinter Procedure 
  FUNCTION getRapPrinter RETURNS CHARACTER
    ( INPUT ipcPrinter AS CHARACTER )  FORWARD.

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
         HEIGHT             = 21.33
         WIDTH              = 107.8.
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
{sww.i}
{syspara.i 19 100 5 cSkrivKIDnr}
{syspara.i 19 12 50 iUtskrTyp INT}
{syspara.i 19 12 52 cLayout}
{syspara.i 19 12 53 cFakturaTekst}
{syspara.i 19 12 54 lMedMva LOGICAL}
IF cFakturaTekst = '' THEN 
  cFakturaTekst = "Faktura/Proformafaktura".
{syspara.i 1 1 8 cUtskrift}
IF cUtskrift = '' THEN 
  cUtskrift = ".\utskrift".
OS-CREATE-DIR VALUE(RIGHT-TRIM(cUtskrift,'\')).    
  cUtskrift = RIGHT-TRIM(cUtskrift,'\') + '\'.
{syspar2.i 1 1 8 cCmd}
IF cCmd = '' THEN 
  cCmd = ".\cmd\FoxitReader.exe /t".
    
IF cLayout = "" OR
   cLayout <> "1" THEN
   ASSIGN lNorLayout = FALSE.
ELSE
   ASSIGN lNorLayout = TRUE.

FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
   cSprak = TRIM(Bruker.Lng).
RUN PopulateTT.
{swn.i}

/* Flagger at bare fil skal genereres, og at den ikke skal skrives ut. */
IF ENTRY(1,cMailAdress,'|') = 'BAREFIL' THEN
    ASSIGN  
        bBareFil = TRUE
        cMailAdress = REPLACE(cMailAdress,'BAREFIL|','')
        .
ELSE 
    bBareFil = FALSE.
    
IF iUtskrTyp = 0 OR iUtskrTyp = 1 THEN
  RUN SkrivRapportPDF. /* svensk layout Brukes ogsï¿½ hos Gant*/
ELSE IF iUtskrTyp = 2 THEN
  RUN SkrivRapportPDF2.
ELSE IF iUtskrTyp = 3 THEN
  RUN SkrivRapportPDF3.
ELSE IF iUtskrTyp = 4 THEN
    RUN SkrivRapportPDF4. /* Mx-variant */

/* Rydder opp fï¿½r avsluttning. */
EMPTY TEMP-TABLE TT_RapportRader.
EMPTY TEMP-TABLE TT_mva.
EMPTY TEMP-TABLE TT_Kvitto.
EMPTY TEMP-TABLE TT_pdf_ext.
EMPTY TEMP-TABLE TT_Font.
EMPTY TEMP-TABLE TT_Info.
EMPTY TEMP-TABLE TT_Object.
EMPTY TEMP-TABLE TT_Resource.
EMPTY TEMP-TABLE TT_pdf_xml.
EMPTY TEMP-TABLE TT_Widget.

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
  DEFINE VARIABLE iY     AS INTEGER              NO-UNDO.
  ASSIGN iCol[1] = iLeftCol
         iCol[2] = 190
         iCol[3] = 305
         iCol[4] = 425
         iCol[5] = 50
         iCol[6] = 190
         iCol[7] = 305
         iCol[8] = 425.

  IF CAN-DO("SE,SVE",TRIM(cSprak)) THEN
    ASSIGN cLabel[1] = "Adress"
           cLabel[2] = "Telefon"
           cLabel[3] = "PlusGiro"
           cLabel[4] = "Organisationsnr"
           cLabel[5] = ""
           cLabel[6] = "Fax"
           cLabel[7] = "Bankgiro"
           cLabel[8] = "Godkänd för F-skatt".
  ELSE
    ASSIGN cLabel[1] = "Adresse"
           cLabel[2] = "Telefon"
           cLabel[3] = "Giro"
           cLabel[4] = "Organisasjonsnr"
           cLabel[5] = "Sum inkl. mva"
           cLabel[6] = "Faks"
           cLabel[7] = "Bankgiro"
           cLabel[8] = "Foretaksregisteret".
  /* Notat */
/*  dY = 120.*/
/*   PUT UNFORMATTED "<USE#1>" hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE "</USE>". */
/*  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).*/
/*   RUN pdf_text_xy_dec ("Spdf","Notat",iCol[1],dY). */
/*   RUN pdf_set_TextY("Spdf",120). */
/*   RUN pdf_Wrap_Text ("Spdf","Vid betalning efter fï¿½rfallodatum debiteras drï¿½jsmï¿½lsrï¿½nta med  2% per mï¿½nad. ï¿½garfï¿½rbehï¿½ll: levererade varor fï¿½rblir sï¿½ljarens egendom till full betalning skett.",15,125,"left",OUTPUT iY). */
/*   RUN pdf_Wrap_Text ("Spdf",hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE,15,125,"left",OUTPUT iY).*/
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
  RUN pdf_text_xy_dec ("Spdf",cLabel[8],iCol[8],dY).

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
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
    DEF OUTPUT PARAM TABLE-HANDLE ohTempTable.
    DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.
------------------------------------------------------------------------------*/
    
    
/*    hHodeTH  = DYNAMIC-FUNCTION("getTempTable","get_fakturahode.p",cParaString,?). */
/*    hLinjeTH = DYNAMIC-FUNCTION("getTempTable","get_fakturalinje.p",cParaString,?).*/
    
    RUN get_fakturahode.p ("validsession", cParaString, OUTPUT TABLE-HANDLE hHodeTH, OUTPUT ocReturn).
    RUN get_fakturalinje.p ("validsession", cParaString, OUTPUT TABLE-HANDLE hLinjeTH, OUTPUT ocReturn).
    
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
               /* Fï¿½rgen  */
/*         RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,1.0,1.0). */
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",.92,.92,.92).
        RUN pdf_rect IN h_PDFinc ("Spdf", 300, iPageHeight - 45, 255, 33,0.1).               /* Left,Bottum,Width,Hight,Thickness */
        RUN pdf_rect IN h_PDFinc ("Spdf", iLeftcol - 10, 555, 515, 20,0.1).                  /* Left,Bottum,Width,Hight,Thickness */
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,1.0,1.0).
        RUN pdf_rect IN h_PDFinc ("Spdf", 300, iPageHeight - 85, 155, 33,0.1). /* faktnr */  /* Left,Bottum,Width,Hight,Thickness */
        RUN pdf_rect IN h_PDFinc ("Spdf", 460, iPageHeight - 85, 95, 33,0.1). /* datumram */ /* Left,Bottum,Width,Hight,Thickness */
        RUN pdf_rect IN h_PDFinc ("Spdf", iLeftcol - 10, 145, 515, 400,0.1).                 /* Left,Bottum,Width,Hight,Thickness */
        RUN pdf_rect IN h_PDFinc ("Spdf", iLeftcol - 10, 85, 515, 50,0.1).                   /* Left,Bottum,Width,Hight,Thickness */
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
  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
  ASSIGN cLabel[1] = "Netto"
         cLabel[2] = "Exkl moms"
/*          cLabel[3] = "Moms %"  Moms% meningslï¿½s vid flera momssatser  */
         cLabel[4] = "Moms kr"
         cLabel[5] = "ATT BETALA".
  ELSE 
  ASSIGN cLabel[1] = "Netto"
         cLabel[2] = "Eks. mva"
/*          cLabel[3] = "Moms %"  Moms% meningslï¿½s vid flera momssatser  */
         cLabel[4] = "Mva kr"
         cLabel[5] = "Sum inkl. mva".
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

&IF DEFINED(EXCLUDE-SkrivBarcode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivBarcode Procedure 
PROCEDURE SkrivBarcode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cFakturanr AS CHARACTER   NO-UNDO.

    IF lCode39 = FALSE THEN DO:
        RUN pdf_load_font IN h_PDFinc ("Spdf","Code39",SEARCH("pdfinclude\samples\support\code39.ttf"),SEARCH("pdfinclude\samples\support\code39.afm"),""). 
        ASSIGN lCode39 = TRUE.
    END.
    
    RUN  pdf_set_font ("Spdf","Code39",20.0).
RUN pdf_set_TextX("Spdf",560).
RUN pdf_set_TextY("Spdf",830).
    RUN pdf_text_rotate ("Spdf", 90).
/*     RUN pdf_set_parameter("Spdf","ScaleY","1.5"). */
    RUN pdf_text ("Spdf","*" + cFakturanr + "*").
/*     RUN  pdf_text_xy ("Spdf",cFakturanr). */
/*     RUN  pdf_text_xy ("Spdf",cFakturanr,450, 800). */
/*     RUN pdf_set_parameter("Spdf","ScaleY","1"). */

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
  DEFINE VARIABLE cLabel  AS CHARACTER  EXTENT 8  NO-UNDO.
/*   ASSIGN iColLbl[1] = iLeftCol */
/*          iColLbl[2] = 130      */
/*          iColLbl[3] = 317      */
/*          iColLbl[4] = 320      */
/*          iColLbl[5] = 430      */
/*          iColLbl[6] = 474      */
/*          iColLbl[7] = 545.     */

IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
  ASSIGN cLabel[1] = "Artnr"
         cLabel[2] = "Benämning"
         cLabel[3] = "Lev ant"
         cLabel[4] = "Enh"
         cLabel[5] = "Ä-pris ex moms"
         cLabel[6] = "Rabatt"
         cLabel[7] = "Sum ink moms"
         cLabel[8] = "Ä-pris ink moms".
ELSE 
  ASSIGN cLabel[1] = "Artnr"
         cLabel[2] = "Beskrivelse"
         cLabel[3] = "Lev ant"
         cLabel[4] = "Enh"
         cLabel[5] = "Enh.pr eks.mva"
         cLabel[6] = "Rabatt-%"
         cLabel[7] = "Sum ink. mva"
         cLabel[8] = "Enh.pr ink.mva".
  iPage = pdf_Page ("Spdf").
  dY = iPageHeight - 280.
 
 IF iUtskrTyp = 0 OR iUtskrTyp = 1 THEN
 DO:
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
   RUN pdf_text_xy_dec ("Spdf",cLabel[1],iColLbl[1],dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[2],iColLbl[2],dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[3],iColLbl[3] - bredd(cLabel[3]),dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[4],iColLbl[4],dY).
   IF lMedMva = TRUE THEN
       RUN pdf_text_xy_dec ("Spdf",cLabel[8],iColLbl[5] - bredd(cLabel[8]),dY).
   ELSE
     RUN pdf_text_xy_dec ("Spdf",cLabel[5],iColLbl[5] - bredd(cLabel[5]),dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[6],iColLbl[6] - bredd(cLabel[6]),dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[7],iColLbl[7] - bredd(cLabel[7]),dY).
 END.
 ELSE IF iUtskrTyp = 2 THEN
 DO:
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
   IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
     RUN pdf_text_xy_dec ("Spdf","KvittoNr/OrderNr",iColLbl[1],dY).
   ELSE
     RUN pdf_text_xy_dec ("Spdf","BongNr/OrdreNr",iColLbl[1],dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[6],iColLbl[6] - bredd(cLabel[6]),dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[7],iColLbl[7] - bredd(cLabel[7]),dY).
 END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivColLabels4) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivColLabels4 Procedure 
PROCEDURE SkrivColLabels4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iColLbl AS INTEGER EXTENT 8    NO-UNDO.
  DEFINE VARIABLE iPage AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dY AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cLabel  AS CHARACTER  EXTENT 8  NO-UNDO.
/*   ASSIGN iColLbl[1] = iLeftCol */
/*          iColLbl[2] = 130      */
/*          iColLbl[3] = 317      */
/*          iColLbl[4] = 320      */
/*          iColLbl[5] = 430      */
/*          iColLbl[6] = 474      */
/*          iColLbl[7] = 545.     */


IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
  ASSIGN cLabel[1] = "Artnr"
         cLabel[2] = "Benämning"
         cLabel[3] = "Lev ant"
         cLabel[4] = "Normalpris"
         cLabel[5] = "Ä-pris"
         cLabel[6] = "Ex moms"
         cLabel[7] = "Rab-%"
         cLabel[8] = "Sum ex moms".
ELSE 
  ASSIGN cLabel[1] = "Artnr"
         cLabel[2] = "Beskrivelse"
         cLabel[3] = "Lev ant"
         cLabel[4] = "Veiledende"
         cLabel[5] = "Enh.pr"
         cLabel[6] = "Eks mva"
         cLabel[7] = "Rab-%"
         cLabel[8] = "Sum eks. mva".
  iPage = pdf_Page ("Spdf").
  dY = iPageHeight - 280.
 
 IF iUtskrTyp = 0 OR iUtskrTyp = 1 OR iUtskrTyp = 4 THEN
 DO:
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
   RUN pdf_text_xy_dec ("Spdf",cLabel[1],iColLbl[1],dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[2],iColLbl[2],dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[3],iColLbl[3] - bredd(cLabel[3]),dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[4],iColLbl[4] - bredd(cLabel[4]),dY).
/*    IF lMedMva = TRUE THEN                                                       */
/*        RUN pdf_text_xy_dec ("Spdf",cLabel[8],iColLbl[5] - bredd(cLabel[8]),dY). */
/*    ELSE                                                                         */
   RUN pdf_text_xy_dec ("Spdf",cLabel[5],iColLbl[5] - bredd(cLabel[5]),dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[6],iColLbl[6] - bredd(cLabel[6]),dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[7],iColLbl[7] - bredd(cLabel[7]),dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[8],iColLbl[8] - bredd(cLabel[8]),dY).
 END.
 ELSE IF iUtskrTyp = 2 THEN
 DO:
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
   IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
     RUN pdf_text_xy_dec ("Spdf","KvittoNr/OrderNr",iColLbl[1],dY).
   ELSE
     RUN pdf_text_xy_dec ("Spdf","BongNr/OrdreNr",iColLbl[1],dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[6],iColLbl[6] - bredd(cLabel[6]),dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[7],iColLbl[7] - bredd(cLabel[7]),dY).
   RUN pdf_text_xy_dec ("Spdf",cLabel[8],iColLbl[8] - bredd(cLabel[8]),dY).
 END.


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
    DEFINE INPUT  PARAMETER iSidnr       AS INTEGER          NO-UNDO.
    DEFINE INPUT  PARAMETER cFaktNr      AS CHARACTER        NO-UNDO.
    DEFINE INPUT  PARAMETER cFakturaType AS CHARACTER        NO-UNDO.
    DEFINE INPUT  PARAMETER lKopi        AS LOGICAL          NO-UNDO.
    DEFINE        VARIABLE  cKopiStr     AS CHARACTER        NO-UNDO.
    DEFINE        VARIABLE  dDato        AS DATE             NO-UNDO.
    DEFINE        VARIABLE  cStr         AS CHARACTER        NO-UNDO.
    DEFINE        VARIABLE  iMittCol     AS INTEGER INIT 305 NO-UNDO.
    DEFINE        VARIABLE  iY           AS INTEGER          NO-UNDO.
    DEFINE        VARIABLE  cRub1        AS CHARACTER EXTENT 13  NO-UNDO.
    DEFINE        VARIABLE  lBetBet      AS LOGICAL          NO-UNDO.
    DEFINE        VARIABLE  cSidText     AS CHARACTER        NO-UNDO.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */
    ASSIGN cFaktNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                     TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE)) ELSE "".
                  /*   TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE)). */ /* ghg */

  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
  DO:
    ASSIGN cKopiStr = IF lKopi THEN " KOPIA" ELSE "".
    ASSIGN cRub1[1] = "Faktnr"
           cRub1[2] = "Kundnr"
           cRub1[3] = "Fakturadatum"
           cRub1[4] = "Leveransadress"
           cRub1[5] = "Fakturaadress"
           cRub1[6] = "Er referens"
           cRub1[7] = "Vår referens"
           cRub1[8] = "Ert ordernr"
           cRub1[9] = "Betalningsvillkor"
           cRub1[10] = "Leveransvillkor"
           cRub1[11] = "Förfallodatum"
           cRub1[12] = "Leveranssätt"
           cRub1[13] = "Dröjsmålsränta".
  END.
  ELSE DO:
    ASSIGN cKopiStr = IF lKopi THEN " KOPI" ELSE "".
    ASSIGN cRub1[1] = "Faktnr"
           cRub1[2] = "Kundenr"
           cRub1[3] = "Fakturadato"
           cRub1[4] = "Leveringsadresse"
           cRub1[5] = "Fakturaadresse"
           cRub1[6] = "Deres ref."
           cRub1[7] = "Vår ref."
           cRub1[8] = "Deres ordrenr"
           cRub1[9] = "Betalingsvilkår"
           cRub1[10] = "Leveringvilkår"
           cRub1[11] = "Forfallsdato"
           cRub1[12] = "Leveringsmåe"
           cRub1[13] = "Forsinkelsesrente".
  END.
    IF hTTHodeBuff:BUFFER-FIELD("BetBet"):BUFFER-VALUE > 0 THEN
       ASSIGN lBetBet = TRUE.
    ELSE
       ASSIGN lBetBet = FALSE.
    IF NUM-ENTRIES(cParaString) > 1 THEN
      ASSIGN cSidText = TRIM("Sida: " + STRING(iSidNr)).
    ELSE
      ASSIGN cSidText = TRIM("Sida: " + STRING(pdf_Page("Spdf")) + " (" + pdf_TotalPages("Spdf") + ")").
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    RUN pdf_text_xy_dec ("Spdf",cSidText,iMittCol + 200,iPageHeight - 10).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",22).
    RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",26).
    RUN pdf_set_TextY("Spdf",iPageHeight - 38).
    RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE),7,25,"left",OUTPUT iY).

/*     RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE),iLeftCol,iPageHeight - 38). */
    RUN pdf_text_xy_dec ("Spdf",cFakturaType + cKopiStr,iMittCol,iPageHeight - 38).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_text_xy_dec ("Spdf",cRub1[1],iMittCol,iPageHeight - 64).
    RUN pdf_text_xy_dec ("Spdf",cRub1[2],iMittCol + 80,iPageHeight - 64).
    RUN pdf_text_xy_dec ("Spdf",cRub1[3],470,iPageHeight - 64).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
    RUN pdf_text_xy_dec ("Spdf",cFaktNr,iMittCol,iPageHeight - 77).
    cStr = TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE)).    
    RUN pdf_text_xy_dec ("Spdf",cStr,iMittCol + 80,iPageHeight - 77).
    dDato = hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE.     
/*    dDato = hTTHodeBuff:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE.     */
    IF dDato = ? THEN
    DO:
       ASSIGN dDato = TODAY.
       IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
         ASSIGN cStr = STRING(YEAR(dDato),"9999") + "-" + STRING(MONTH(dDato),"99") + "-" + STRING(DAY(dDato),"99").
       ELSE
         ASSIGN cStr = STRING(dDato).
    END.
    ELSE
      IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
        ASSIGN cStr = STRING(YEAR(dDato),"9999") + "-" + STRING(MONTH(dDato),"99") + "-" + STRING(DAY(dDato),"99").
      ELSE
        ASSIGN cStr = STRING(dDato).
    RUN pdf_text_xy_dec ("Spdf",cStr,470,iPageHeight - 77).
    /* levadress label */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    IF lNorLayout = FALSE THEN
    DO:
      RUN pdf_text_xy_dec ("Spdf",cRub1[4],iLeftCol,iPageHeight - 100).
      /* faktadress label */
      RUN pdf_text_xy_dec ("Spdf",cRub1[5],iMittCol,iPageHeight - 100).
    END.
    ELSE
    DO:
      RUN pdf_text_xy_dec ("Spdf",cRub1[5],iLeftCol,iPageHeight - 100).
      /* faktadress label */
      RUN pdf_text_xy_dec ("Spdf",cRub1[4],iMittCol,iPageHeight - 100).
    END.
    IF cSkrivKIDnr = '1' THEN 
    DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
      RUN pdf_text_xy_dec ("Spdf","KID:",iMittCol + 120,iPageHeight - 100).
      IF (hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE > 0 AND cSkrivKIDnr = '1') THEN
        RUN pdf_text_xy_dec ("Spdf",STRING(hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE),iMittCol + 145,iPageHeight - 100).
    END.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
    /* faktadress */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    IF lNorLayout = FALSE THEN
    DO:
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE),iMittCol,iPageHeight - 115).
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE),iMittCol,iPageHeight - 130).
      IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN
        RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE),iMittCol,iPageHeight - 145).
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE),iMittCol,iPageHeight - 160).
    END.
    ELSE
    DO:
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE),iLeftCol,iPageHeight - 115).
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE),iLeftCol,iPageHeight - 130).
      IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN
        RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE),iLeftCol,iPageHeight - 145).
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE),iLeftCol,iPageHeight - 160).
    END.

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    /* kundref txt */
    RUN pdf_text_xy_dec ("Spdf",cRub1[6],iLeftCol,iPageHeight - 201).
    RUN pdf_text_xy_dec ("Spdf",cRub1[8],iLeftCol,iPageHeight - 218).
    RUN pdf_text_xy_dec ("Spdf",cRub1[10],iLeftCol,iPageHeight - 235).
    RUN pdf_text_xy_dec ("Spdf",cRub1[12],iLeftCol,iPageHeight - 252).
    /* vår ref txt */
    RUN pdf_text_xy_dec ("Spdf",cRub1[7],iMittCol,iPageHeight - 201).
    IF lBetBet = TRUE THEN
    DO:
      RUN pdf_text_xy_dec ("Spdf",cRub1[9],iMittCol,iPageHeight - 218).
      RUN pdf_text_xy_dec ("Spdf",cRub1[11],iMittCol,iPageHeight - 235).
    END.
    RUN pdf_text_xy_dec ("Spdf",cRub1[13],iMittCol,iPageHeight - 252).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("DeresRef"):BUFFER-VALUE),iLeftCol + 100,iPageHeight - 201).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("VaarRef"):BUFFER-VALUE),iMittCol + 100,iPageHeight - 201).
/*     RUN pdf_text_xy_dec ("Spdf","Ert ordernr",iLeftCol,iPageHeight - 218). */
    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      ASSIGN cStr = REPLACE(hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE,"Dager","Dagar").
    ELSE
      ASSIGN cStr = hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE.
    IF lBetBet = TRUE THEN
      RUN pdf_text_xy_dec ("Spdf",TRIM(cStr),iMittCol + 100,iPageHeight - 218).
    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      ASSIGN cStr = REPLACE(hTTHodeBuff:BUFFER-FIELD("LevFormBeskrivelse"):BUFFER-VALUE,"Butikksalg","Butiksförsäljning").
    ELSE
      ASSIGN cStr = hTTHodeBuff:BUFFER-FIELD("LevFormBeskrivelse"):BUFFER-VALUE.

    RUN pdf_text_xy_dec ("Spdf",TRIM(cStr),iLeftCol + 100,iPageHeight - 235).

    dDato = hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE.
    IF dDato = ? THEN
    DO:                                       
       FIND FIRST Betalingsbetingelser WHERE Betalingsbetingelser.BetBet = INT(hTTHodeBuff:BUFFER-FIELD("BetBet"):BUFFER-VALUE) NO-LOCK NO-ERROR.
       ASSIGN dDato = TODAY + Betalingsbetingelser.AntKredittDager.
       IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
         ASSIGN cStr = STRING(YEAR(dDato),"9999") + "-" + STRING(MONTH(dDato),"99") + "-" + STRING(DAY(dDato),"99").
       ELSE
         ASSIGN cStr = STRING(dDato).
    END.
    ELSE
      IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
        ASSIGN cStr = STRING(YEAR(dDato),"9999") + "-" + STRING(MONTH(dDato),"99") + "-" + STRING(DAY(dDato),"99").
      ELSE
        ASSIGN cStr = STRING(dDato).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    IF lBetBet = TRUE THEN
      RUN pdf_text_xy_dec ("Spdf",cStr,iMittCol + 100,iPageHeight - 235).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivHeaderPDF_4) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivHeaderPDF_4 Procedure 
PROCEDURE SkrivHeaderPDF_4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iSidnr       AS INTEGER          NO-UNDO.
    DEFINE INPUT  PARAMETER cFaktNr      AS CHARACTER        NO-UNDO.
    DEFINE INPUT  PARAMETER cFakturaType AS CHARACTER        NO-UNDO.
    DEFINE INPUT  PARAMETER lKopi        AS LOGICAL          NO-UNDO.
    DEFINE        VARIABLE  cKopiStr     AS CHARACTER        NO-UNDO.
    DEFINE        VARIABLE  dDato        AS DATE             NO-UNDO.
    DEFINE        VARIABLE  cStr         AS CHARACTER        NO-UNDO.
    DEFINE        VARIABLE  iMittCol     AS INTEGER INIT 305 NO-UNDO.
    DEFINE        VARIABLE  iY           AS INTEGER          NO-UNDO.
    DEFINE        VARIABLE  cRub1        AS CHARACTER EXTENT 13  NO-UNDO.
    DEFINE        VARIABLE  lBetBet      AS LOGICAL          NO-UNDO.
    DEFINE        VARIABLE  cSidText     AS CHARACTER        NO-UNDO.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */
    ASSIGN cFaktNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                     TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE)) ELSE "".
                  /*   TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE)). */ /* ghg */

  IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
  DO:
    ASSIGN cKopiStr = IF lKopi THEN " KOPIA" ELSE "".
    ASSIGN cRub1[1] = "Faktnr"
           cRub1[2] = "Kundnr"
           cRub1[3] = "Fakturadatum"
           cRub1[4] = "Leveransadress"
           cRub1[5] = "Fakturaadress"
           cRub1[6] = "Er referens"
           cRub1[7] = "Vår referens"
           cRub1[8] = "Ert ordernr"
           cRub1[9] = "Betalningsvillkor"
           cRub1[10] = "Leveransvillkor"
           cRub1[11] = "Förfallodatum"
           cRub1[12] = "Leveranssått"
           cRub1[13] = "Dröjsmålsränta".
  END.
  ELSE DO:
    ASSIGN cKopiStr = IF lKopi THEN " KOPI" ELSE "".
    ASSIGN cRub1[1] = "Faktnr"
           cRub1[2] = "Kundenr"
           cRub1[3] = "Fakturadato"
           cRub1[4] = "Leveringsadresse"
           cRub1[5] = "Fakturaadresse"
           cRub1[6] = "Deres ref."
           cRub1[7] = "Vår ref."
           cRub1[8] = "Deres ordrenr"
           cRub1[9] = "Betalingsvilkår"
           cRub1[10] = "Leveringvilkår"
           cRub1[11] = "Forfallsdato"
           cRub1[12] = "Leveringsmete"
           cRub1[13] = "Forsinkelsesrente".
  END.
    IF hTTHodeBuff:BUFFER-FIELD("BetBet"):BUFFER-VALUE > 0 THEN
       ASSIGN lBetBet = TRUE.
    ELSE
       ASSIGN lBetBet = FALSE.

    IF NUM-ENTRIES(cParaString) > 1 THEN
      ASSIGN cSidText = TRIM("Sida: " + STRING(iSidNr)).
    ELSE
      ASSIGN cSidText = TRIM("Sida: " + STRING(pdf_Page("Spdf")) + " (" + pdf_TotalPages("Spdf") + ")").
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    RUN pdf_text_xy_dec ("Spdf",cSidText,iMittCol + 200,iPageHeight - 10).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",22).
    RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",26).
    RUN pdf_set_TextY("Spdf",iPageHeight - 38).
    RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE),7,25,"left",OUTPUT iY).

/*     RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaNavn"):BUFFER-VALUE),iLeftCol,iPageHeight - 38). */
    RUN pdf_text_xy_dec ("Spdf",cFakturaType + cKopiStr,iMittCol,iPageHeight - 38).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_text_xy_dec ("Spdf",cRub1[1],iMittCol,iPageHeight - 64).
    RUN pdf_text_xy_dec ("Spdf",cRub1[2],iMittCol + 80,iPageHeight - 64).
    RUN pdf_text_xy_dec ("Spdf",cRub1[3],470,iPageHeight - 64).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
    RUN pdf_text_xy_dec ("Spdf",cFaktNr,iMittCol,iPageHeight - 77).
    cStr = TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("KundeNr"):BUFFER-VALUE)).    
    RUN pdf_text_xy_dec ("Spdf",cStr,iMittCol + 80,iPageHeight - 77).
    dDato = hTTHodeBuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE.     
/*    dDato = hTTHodeBuff:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE.     */
    IF dDato = ? THEN
    DO:
       ASSIGN dDato = TODAY.
       IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
         ASSIGN cStr = STRING(YEAR(dDato),"9999") + "-" + STRING(MONTH(dDato),"99") + "-" + STRING(DAY(dDato),"99").
       ELSE
         ASSIGN cStr = STRING(dDato).
    END.
    ELSE
      IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
        ASSIGN cStr = STRING(YEAR(dDato),"9999") + "-" + STRING(MONTH(dDato),"99") + "-" + STRING(DAY(dDato),"99").
      ELSE
        ASSIGN cStr = STRING(dDato).
    RUN pdf_text_xy_dec ("Spdf",cStr,470,iPageHeight - 77).
    /* levadress label */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    IF lNorLayout = FALSE THEN
    DO:
      RUN pdf_text_xy_dec ("Spdf",cRub1[4],iLeftCol,iPageHeight - 100).
      /* faktadress label */
      RUN pdf_text_xy_dec ("Spdf",cRub1[5],iMittCol,iPageHeight - 100).
    END.
    ELSE
    DO:
      RUN pdf_text_xy_dec ("Spdf",cRub1[5],iLeftCol,iPageHeight - 100).
      /* faktadress label */
      RUN pdf_text_xy_dec ("Spdf",cRub1[4],iMittCol,iPageHeight - 100).
    END.
    IF cSkrivKIDnr = '1' THEN 
    DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
      RUN pdf_text_xy_dec ("Spdf","KID:",iMittCol + 120,iPageHeight - 100).
      IF (hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE > 0 AND cSkrivKIDnr = '1') THEN
        RUN pdf_text_xy_dec ("Spdf",STRING(hTTHodeBuff:BUFFER-FIELD("KID"):BUFFER-VALUE),iMittCol + 145,iPageHeight - 100).
    END.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
    /* faktadress */
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    IF lNorLayout = FALSE THEN
    DO:
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE),iMittCol,iPageHeight - 115).
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE),iMittCol,iPageHeight - 130).
      IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN
        RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE),iMittCol,iPageHeight - 145).
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE),iMittCol,iPageHeight - 160).
    END.
    ELSE
    DO:
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("Navn"):BUFFER-VALUE),iLeftCol,iPageHeight - 115).
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse1"):BUFFER-VALUE),iLeftCol,iPageHeight - 130).
      IF TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE) <> "" THEN
        RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktAdresse2"):BUFFER-VALUE),iLeftCol,iPageHeight - 145).
      RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPostNr"):BUFFER-VALUE) + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("FaktPoststed"):BUFFER-VALUE),iLeftCol,iPageHeight - 160).
    END.

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    /* kundref txt */
    RUN pdf_text_xy_dec ("Spdf",cRub1[6],iLeftCol,iPageHeight - 200).
/*     RUN pdf_text_xy_dec ("Spdf",cRub1[8],iLeftCol,iPageHeight - 213). */
    RUN pdf_text_xy_dec ("Spdf",cRub1[10],iLeftCol,iPageHeight - 226).
    RUN pdf_text_xy_dec ("Spdf",cRub1[12],iLeftCol,iPageHeight - 239).
    /* vï¿½r ref txt */
    RUN pdf_text_xy_dec ("Spdf",cRub1[7],iMittCol,iPageHeight - 200).
    IF lBetBet = TRUE THEN
    DO:
      RUN pdf_text_xy_dec ("Spdf",cRub1[9],iMittCol,iPageHeight - 226).
      RUN pdf_text_xy_dec ("Spdf",cRub1[11],iMittCol,iPageHeight - 239).
    END.
    RUN pdf_text_xy_dec ("Spdf",cRub1[13],iMittCol,iPageHeight - 252).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("DeresRef"):BUFFER-VALUE),iLeftCol + 100,iPageHeight - 200).
    RUN pdf_text_xy_dec ("Spdf",TRIM(cEmail),iLeftCol + 100,iPageHeight - 213).
    RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("VaarRef"):BUFFER-VALUE),iMittCol + 100,iPageHeight - 200).
/*     RUN pdf_text_xy_dec ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FirmaEPost"):BUFFER-VALUE),iMittCol + 100,iPageHeight - 213). */
/*     RUN pdf_text_xy_dec ("Spdf","Ert ordernr",iLeftCol,iPageHeight - 218). */
    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      ASSIGN cStr = REPLACE(hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE,"Dager","Dagar").
    ELSE
      ASSIGN cStr = hTTHodeBuff:BUFFER-FIELD("BetTekst"):BUFFER-VALUE.
    IF lBetBet = TRUE THEN
      RUN pdf_text_xy_dec ("Spdf",TRIM(cStr),iMittCol + 100,iPageHeight - 226).
    IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
      ASSIGN cStr = REPLACE(hTTHodeBuff:BUFFER-FIELD("LevFormBeskrivelse"):BUFFER-VALUE,"Butikksalg","Butiksförsäljning").
    ELSE
      ASSIGN cStr = hTTHodeBuff:BUFFER-FIELD("LevFormBeskrivelse"):BUFFER-VALUE.

    RUN pdf_text_xy_dec ("Spdf",TRIM(cStr),iLeftCol + 100,iPageHeight - 239).

    dDato = hTTHodeBuff:BUFFER-FIELD("ForfallsDato"):BUFFER-VALUE.
    IF dDato = ? THEN
    DO:                                       
       FIND FIRST Betalingsbetingelser WHERE Betalingsbetingelser.BetBet = INT(hTTHodeBuff:BUFFER-FIELD("BetBet"):BUFFER-VALUE) NO-LOCK NO-ERROR.
       ASSIGN dDato = TODAY + Betalingsbetingelser.AntKredittDager.
       IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
         ASSIGN cStr = STRING(YEAR(dDato),"9999") + "-" + STRING(MONTH(dDato),"99") + "-" + STRING(DAY(dDato),"99").
       ELSE
         ASSIGN cStr = STRING(dDato).
    END.
    ELSE
      IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
        ASSIGN cStr = STRING(YEAR(dDato),"9999") + "-" + STRING(MONTH(dDato),"99") + "-" + STRING(DAY(dDato),"99").
      ELSE
        ASSIGN cStr = STRING(dDato).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    IF lBetBet = TRUE THEN
      RUN pdf_text_xy_dec ("Spdf",cStr,iMittCol + 100,iPageHeight - 239).
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

&IF DEFINED(EXCLUDE-SkrivRapportPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapportPDF Procedure 
PROCEDURE SkrivRapportPDF :
/*--
  Purpose:
  Notes:
--*/
   DEF VAR dFaktura_id  AS DECIMAL            NO-UNDO.
   DEF VAR cFilNavn     AS CHARACTER          NO-UNDO.
   DEF VAR cButNr       AS CHARACTER          NO-UNDO.
   DEF VAR iBilagsType  AS INTEGER            NO-UNDO.
   DEF VAR cFakturaType AS CHARACTER          NO-UNDO.
   DEF VAR cFakturaNr   AS CHARACTER          NO-UNDO.
   DEF VAR iSidNr       AS INTEGER            NO-UNDO.
   DEF VAR iCount       AS INTEGER            NO-UNDO.
   DEF VAR iRadNr       AS INTEGER            NO-UNDO.
   DEF VAR cDetaljRad   AS CHARACTER          NO-UNDO.
   DEF VAR dBruttoPris  AS DECIMAL            NO-UNDO.
   DEF VAR dRabKr       AS DECIMAL            NO-UNDO.
   DEF VAR dAntal       AS DECIMAL            NO-UNDO.
   DEF VAR iAntLinjer   AS INTEGER            NO-UNDO.
   DEF VAR cRefTxt      AS CHARACTER          NO-UNDO.
   DEF VAR iKontrollRad AS INTEGER            NO-UNDO.
   DEF VAR cTxt         AS CHARACTER          NO-UNDO.
   DEF VAR dY           AS DECIMAL            NO-UNDO.
   DEF VAR dYorg        AS DECIMAL            NO-UNDO.
   DEF VAR iY           AS INTEGER            NO-UNDO.
   DEF VAR iAntMva      AS INTEGER            NO-UNDO.
   DEF VAR cSpecLbl     AS CHARACTER EXTENT 4 NO-UNDO.
   DEF VAR dSpecCol     AS DECIMAL   EXTENT 4 NO-UNDO.
   DEF VAR ii           AS INTEGER            NO-UNDO.
   DEF VAR dB_Id        AS DECIMAL            NO-UNDO.
   DEF VAR iMinus       AS INTEGER            NO-UNDO.
   DEF VAR lSamleFakt   AS LOGICAL            NO-UNDO.
   DEF VAR lFirstaRad   AS LOGICAL            NO-UNDO.
   DEF VAR dWrk         AS DECIMAL            NO-UNDO.
   DEF VAR iOpphav      AS INTEGER     NO-UNDO.
   DEF VAR cEkstRefTekst AS CHARACTER NO-UNDO.
   /* anvï¿½nds i SkrivColLabels och hï¿½r */
   ASSIGN iLeftCol   = 50
          iColLbl[1] = iLeftCol
          iColLbl[2] = 130
          iColLbl[3] = 317
          iColLbl[4] = 320
          iColLbl[5] = 415
          iColLbl[6] = 474
          iColLbl[7] = 545.

 IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
   ASSIGN cSpecLbl[1] = "Exkl moms"
     cSpecLbl[2] = "Moms%"             /* "Mva%" */
     cSpecLbl[3] = "Moms kr".
/*      cSpecLbl[4] = "ï¿½resavr". */
 ELSE
   ASSIGN cSpecLbl[1] = "Totalsum eks. mva"
     cSpecLbl[2] = "Mva%"             /* "Mva%" */
     cSpecLbl[3] = "Mva kr".
/*      cSpecLbl[4] = "ï¿½resavr". */
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
   
   dFaktura_id = hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE.
   cFakturaNr  = STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE).
   cButNr      = STRING(hTTHodeBuff:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).
   IF cFakturaNr = ? THEN
       cFilNavn = cUtskrift + "Faktura" + "_" + STRING(dFaktura_id) + '-' + REPLACE(STRING(TODAY),'/','') + '-' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".pdf".
   ELSE
   cFilNavn = cUtskrift + 
              "But" + '_' + cButNr + '_' +
              "Faktura" + "_" + 
              STRING(cFakturaNr) + '-' + 
              REPLACE(STRING(TODAY),'/','') + '-' + 
              STRING(ETIME) + 
              ".pdf".
   
   RUN pdf_new ("Spdf",cFilNavn).
   RUN pdf_set_BottomMargin ("Spdf", 20).
   RUN pdf_set_PaperType ("Spdf","A4").
   RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
   RUN pdf_set_Orientation ("Spdf","portrait").
   iPageHeight = pdf_PageHeight ("Spdf").
   iPageWidth  = pdf_PageWidth ("Spdf").
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE Faktura_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)).
       ASSIGN iBilagstype = hTTHodeBuff:BUFFER-FIELD("BilagsType"):BUFFER-VALUE.
       CASE iBilagstype:
         WHEN 1 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                            cFakturaType = STRING(cFakturaNr <> "",cFakturaTekst).
         WHEN 2 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                            cFakturaType = STRING(cFakturaNr <> "","Kreditnota/ProformaKredit"). 
         WHEN 5 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE 
                                 IF hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE > 0 THEN
                                     TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE)) ELSE ""  /* ghg */
                            cFakturaType = "Utbetalning". 
         WHEN 10 THEN ASSIGN cFakturaNr   = ""
                             cFakturaType = "Betalningspåminnelse". 
         OTHERWISE ASSIGN cFakturaNr   = ""
                          cFakturaType = "". 
       END CASE.
       /* ref til pakkseddel når ikke referansetekster vises i layout. */
       FIND FIRST FakturaLinje NO-LOCK WHERE FakturaLinje.Faktura_Id = hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE NO-ERROR.
       IF AVAILABLE FakturaLinje THEN 
         cEkstRefTekst = FakturaLinje.EkstRefTekst.
       ELSE 
         cEkstRefTekst = ''.
       IF hTTHodeBuff:BUFFER-FIELD("SamleFaktura"):BUFFER-VALUE  = TRUE THEN
         ASSIGN lSamleFakt = TRUE.
       ELSE
         ASSIGN lSamleFakt = FALSE.
       dY = 550.
       DO:
         iCount = 1.
         iSidNr = 1.
         RUN pdf_new_page ("Spdf").
         RUN RitaRamar. /* pt kvar att gï¿½ra */
         RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
         RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
         /* Hï¿½r skriver vi ut notat. Fï¿½rst sï¿½tter vi Y */
         RUN pdf_set_TextY("Spdf",120).
         IF TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE) <> '' THEN 
             RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
         ELSE 
             RUN pdf_Wrap_Text ("Spdf",cEkstRefTekst,15,125,"left",OUTPUT iY).
         RUN PageFooter.

         IF cFakturanr <> "" THEN DO:
            RUN SkrivBarcode (cFakturanr).
         END.
         ASSIGN iRadNr     = 21. /* 22 */
         RUN SkrivColLabels.
         /* Tï¿½mmer momsloggen */
         FOR EACH TT_Mva:
           DELETE TT_Mva.
         END.
         
         IF iCount = 1 THEN
             qL:QUERY-OPEN().
         qL:GET-FIRST().
         ASSIGN iAntLinjer = 0
                iAntMva = 0.
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
         
         ASSIGN lFirstaRad = TRUE.
         qL:GET-FIRST().
         REPEAT WHILE NOT qL:QUERY-OFF-END:
           ASSIGN iRadNr     = iRadNr + 1
                  iAntLinjer = iAntLinjer - 1.
           ASSIGN dBruttoPris = ABS(dec(hTTLinjeBuff:BUFFER-FIELD("Linjesum"):BUFFER-VALUE))
                  dAntal      = ABS(dec(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE))
                  dBruttoPris = IF dec(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE) > 0
                                  THEN dec(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE)
                                  ELSE dBruttoPris
                  dBruttoPris = IF dBruttoPris = ? THEN 0 ELSE dBruttoPris.
                  IF dBruttoPris <> 0 AND lMedMva = TRUE THEN
                     dBruttoPris = dBruttoPris + (dec(hTTLinjeBuff:BUFFER-FIELD("Mvakr"):BUFFER-VALUE) / dAntal).
           dY = dY - 14.
           IF dY < 190 THEN DO:    /* 185*/
             iSidNr = iSidNr + 1.
             RUN pdf_new_page ("Spdf").
             RUN RitaRamar. /* pt kvar att gï¿½ra */
             RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
             RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
             RUN pdf_set_TextY("Spdf",120).
             RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
             RUN PageFooter.
             RUN SkrivColLabels.
             dY = 550 - 14.
           END.
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
           RUN pdf_text_xy_dec ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE) /* + " - " + STRING(dY) */ ,iColLbl[1],dY).
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
           RUN pdf_text_xy_dec ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("Varetekst"):BUFFER-VALUE),iColLbl[2],dY).
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
           dAntal      = hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE.
           RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dAntal,"->>,>>9")),iColLbl[3] - bredd(TRIM(STRING(dAntal,"->>,>>9"))),dY).

           RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dBruttoPris,"->>,>>9.99")),iColLbl[5] - bredd(TRIM(STRING(dBruttoPris,"->>,>>9.99"))),dY).

           iOpphav = hTTLinjeBuff:BUFFER-FIELD("Opphav"):BUFFER-VALUE.
           IF iOpphav = 20 THEN  /* Fakturaavgift */
               dRabKr = 0.
           ELSE
               dRabKr = dAntal * (hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE - hTTLinjeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE).
           IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
             RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dRabKr,"->>,>>9.99")),iColLbl[6] - bredd(TRIM(STRING(dRabKr,"->>,>>9.99"))),dY).
           ELSE
             RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE,"->>,>>9.99")),iColLbl[6] - bredd(TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE,"->>,>>9.99"))),dY).
           ASSIGN dWrk = DEC(hTTLinjeBuff:BUFFER-FIELD("nettolinjesum"):BUFFER-VALUE) + dec(hTTLinjeBuff:BUFFER-FIELD("Mvakr"):BUFFER-VALUE).
           cTxt = TRIM(STRING(dWrk,"->>,>>9.99")).
/*           cTxt = TRIM(STRING(DEC(hTTLinjeBuff:BUFFER-FIELD("nettolinjesum":BUFFER-VALUE + dec(hTTLinjeBuff:BUFFER-FIELD("Mvakr"):BUFFER-VALUE,"->>,>>9.99")).*/
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).


           IF lSamleFakt = TRUE THEN
           DO:
             IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN 
             DO:
               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer - 1.
               cRefTxt = TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE)).
               ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Bong:","")).
               ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Kvitto:","")).
               IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                 ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"But:","Butik:"))
                        cRefTxt = TRIM(REPLACE(cRefTxt,"Kas:","Kassa:"))
                        cRefTxt = TRIM(REPLACE(cRefTxt,"Dato:","Datum:")).
               dY = dY - 15.
               IF dY < 185 THEN
               DO:
                 iSidNr = iSidNr + 1.
                 RUN pdf_new_page ("Spdf").
                 RUN RitaRamar. /* pt kvar att gï¿½ra */
                 RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
                 RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
                 RUN pdf_set_TextY("Spdf",120).
                 RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
                 RUN PageFooter.
                 RUN SkrivColLabels.
                 dY = 550 - 15.
               END.
               ASSIGN dB_Id = hTTLinjeBuff:BUFFER-FIELD("B_Id"):BUFFER-VALUE.
               FIND FIRST BongHode WHERE BongHode.b_id = dB_Id NO-LOCK NO-ERROR.
               IF AVAILABLE Bonghode THEN
                 ASSIGN cRefTxt = cRefTxt + " KortNr: " + IF BongHode.KundeKort <> "" THEN BongHode.KundeKort ELSE BongHode.MedlemsKort.
               RUN pdf_text_xy_dec ("Spdf",cRefTxt,iColLbl[2],dY).
             END.
             IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN 
             DO:
               ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE)
                      cRefTxt = cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE).
               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer - 1.
               dY = dY - 15.
               RUN pdf_text_xy_dec ("Spdf",TRIM(cRefTxt),iColLbl[2],dY).
             END.
           END.
           ELSE IF lFirstaRad = TRUE THEN
           DO:
             ASSIGN lFirstaRad = FALSE.
             IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN 
             DO:
               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer - 1.
               cRefTxt = TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE)).
               ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Bong:","")).
               ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Kvitto:","")).
               IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                 ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"But:","Butik:"))
                        cRefTxt = TRIM(REPLACE(cRefTxt,"Kas:","Kassa:"))
                        cRefTxt = TRIM(REPLACE(cRefTxt,"Dato:","Datum:")).
               RUN pdf_text_xy_dec ("Spdf",cRefTxt,iLeftCol,iPageHeight - 175).
             END.
             IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN 
             DO:
               ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE).
                      IF cRefTxt <> TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) THEN
                          cRefTxt = cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE).
               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer - 1.
               RUN pdf_text_xy_dec ("Spdf",TRIM(cRefTxt),iLeftCol,iPageHeight - 190).
             END.
           END.
           IF TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE) <> "" THEN 
           DO:
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
           /* beklagar, men vi mï¿½ste sidbryta */
           RUN pdf_new_page ("Spdf").
           RUN RitaRamar. /* pt kvar att gï¿½ra */
           RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
           RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
           /* Hï¿½r skriver vi ut notat. Fï¿½rst sï¿½tter vi Y */
           RUN pdf_set_TextY("Spdf",120).
           RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
           RUN PageFooter.
           RUN SkrivColLabels.
         END.
         RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
         RUN pdf_text_xy_dec ("Spdf",cSpecLbl[1],dSpecCol[1] - bredd(cSpecLbl[1]),dY).
         RUN pdf_text_xy_dec ("Spdf",cSpecLbl[2],dSpecCol[2] - bredd(cSpecLbl[2]),dY).
         RUN pdf_text_xy_dec ("Spdf",cSpecLbl[3],dSpecCol[3] - bredd(cSpecLbl[3]),dY).  /* 20121029/ghg */
/*          IF hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE THEN                      */
/*              RUN pdf_text_xy_dec ("Spdf",cSpecLbl[4],dSpecCol[4] - bredd(cSpecLbl[4]),dY). */
         dY = dY - 12.
         RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
         
         FOR EACH tt_mva BREAK BY tt_mva.mva% :
           cTxt = STRING(tt_mva.linjesum - tt_mva.mvakr,"->,>>>,>>9.99").
           RUN pdf_text_xy_dec ("Spdf",cTxt,dSpecCol[1] - bredd(cTxt),dY).
           cTxt = STRING(tt_mva.mva%,"->,>>>,>>9.99").
           RUN pdf_text_xy_dec ("Spdf",cTxt,dSpecCol[2] - bredd(cTxt),dY).
           cTxt = STRING(tt_mva.mvakr,"->,>>>,>>9.99").
           RUN pdf_text_xy_dec ("Spdf",cTxt,dSpecCol[3] - bredd(cTxt),dY).
/*            IF LAST-OF(tt_mva.mva%) AND hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE <> 0 THEN DO: */
/*                cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE,"->,>>>,>>9.99").       */
/*                RUN pdf_text_xy_dec ("Spdf",cTxt,dSpecCol[4] - bredd(cTxt),dY).                            */
/*            END.                                                                                           */
           dY = dY - 12.
         END.
         /* Skriv Att betala */
         dY =  180.
         RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
         IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
           ASSIGN cTxt = "ATT BETALA"
                  iMinus = 0.
         ELSE
           ASSIGN cTxt = "TOTALT Å BETALE"
                  iMinus = 8.
         RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - iMinus - bredd(cTxt),dY).
         dY = dY - 15.
         cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE - hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE,"->,>>>,>>9.99").
         RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).
       END.
       qH:GET-NEXT().
   END.
   RUN pdf_close ("Spdf").
   
   /* Kallende rutine skal bare ha filen, utskrift skal ikke gjï¿½res. */
   IF bBareFil THEN DO:
       FILE-INFO:FILENAME = cFilNavn.
       PUBLISH 'fakturaFilNavn' (FILE-INFO:FULL-PATHNAME). 
     END.
   /* Faktura skal skrives direkte ut pï¿½ skjerm. */
   ELSE IF lDirekte = FALSE THEN
       RUN browse2pdf\viewxmldialog.w (cFilNavn,"FAKTURA").
   /* Faktura utskrift */
   ELSE DO ii = 1 TO iAntEks:
       OS-COMMAND SILENT VALUE(cCmd + ' ' + cFilnavn + ' "' + cPrinter + '"').
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivRapportPDF2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapportPDF2 Procedure 
PROCEDURE SkrivRapportPDF2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dFaktura_id  AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE cFilNavn     AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE iBilagsType  AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cFakturaType AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE cFakturaNr   AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE iSidNr       AS INTEGER            NO-UNDO.
   DEFINE VARIABLE iCount       AS INTEGER            NO-UNDO.
   DEFINE VARIABLE iCount2      AS INTEGER            NO-UNDO.
   DEFINE VARIABLE iRadNr       AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cDetaljRad   AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE dBruttoPris  AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE dRabKr       AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE dAntal       AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE iAntLinjer   AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cRefTxt      AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE iKontrollRad AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cTxt         AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE dY           AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE dYorg        AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE iY           AS INTEGER            NO-UNDO.
   DEFINE VARIABLE iAntMva      AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cSpecLbl     AS CHARACTER EXTENT 4 NO-UNDO.
   DEFINE VARIABLE dSpecCol     AS DECIMAL   EXTENT 4 NO-UNDO.
   DEFINE VARIABLE ii           AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cWrk         AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE cWrk2        AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE iWrk         AS INTEGER            NO-UNDO.
   DEFINE VARIABLE dB_Id        AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE iMinus       AS INTEGER            NO-UNDO.
   DEFINE VARIABLE dWrk         AS DECIMAL            NO-UNDO.
   
   /* anvï¿½nds i SkrivColLabels och hï¿½r */
   ASSIGN iLeftCol   = 50
          iColLbl[1] = iLeftCol
          iColLbl[2] = 130
          iColLbl[3] = 317
          iColLbl[4] = 320
          iColLbl[5] = 415
          iColLbl[6] = 474
          iColLbl[7] = 545.
 IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
   ASSIGN cSpecLbl[1] = "Exkl moms"
          cSpecLbl[2] = "Moms%"             /* "Mva%" */
          cSpecLbl[3] = "Moms kr"
          cSpecLbl[4] = "Öresavr".
 ELSE
   ASSIGN cSpecLbl[1] = "Exkl mva"
          cSpecLbl[2] = "Mva%"             /* "Mva%" */
          cSpecLbl[3] = "Mva kr"
          cSpecLbl[4] = "Øresavr".
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
   
   dFaktura_id = hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE.
   /*cFilNavn = SESSION:TEMP-DIR + "Faktura" + "_" + STRING(TIME) + ".pdf".*/
   cFilNavn = cUtskrift + "Faktura" + "_" + STRING(dFaktura_id) + '-' + REPLACE(STRING(TODAY),'/','') + '-' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".pdf".
   RUN pdf_new ("Spdf",cFilNavn).
   RUN pdf_set_BottomMargin ("Spdf", 20).
   RUN pdf_set_PaperType ("Spdf","A4").
   RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
   RUN pdf_set_Orientation ("Spdf","portrait").
   iPageHeight = pdf_PageHeight ("Spdf").
   iPageWidth  = pdf_PageWidth ("Spdf").
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE Faktura_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)).

       ASSIGN iBilagstype = hTTHodeBuff:BUFFER-FIELD("BilagsType"):BUFFER-VALUE.
       CASE iBilagstype:
           WHEN 1 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                                         /*  IF hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE > 0 THEN
                                              TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE)) ELSE "" */ /* ghg */
                            cFakturaType = STRING(cFakturaNr <> "","Faktura/Proformafaktura"). 
           WHEN 2 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                                         /*  IF hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE > 0 THEN
                                              TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE)) ELSE "" */ /* ghg */
                            cFakturaType = STRING(cFakturaNr <> "","Kreditnota/ProformaKredit"). 
           WHEN 5 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE 
                                           IF hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE > 0 THEN
                                              TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE)) ELSE ""  /* ghg */     
                              cFakturaType = "Utbetalning". 
           WHEN 10 THEN ASSIGN cFakturaNr   = ""
                               cFakturaType = "Betalningspåminnelse". 
           OTHERWISE ASSIGN cFakturaNr   = ""
                            cFakturaType = "". 
       END CASE.

       dY = 550.
       
       DO:
           iCount = 1.
           iSidNr = 1.
           RUN pdf_new_page ("Spdf").
           RUN RitaRamar. /* pt kvar att gï¿½ra */
           RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
           RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
           /* Hï¿½r skriver vi ut notat. Fï¿½rst sï¿½tter vi Y */
           RUN pdf_set_TextY("Spdf",120).
/*            RUN pdf_Wrap_Text ("Spdf","Vid betalning efter fï¿½rfallodatum debiteras drï¿½jsmï¿½lsrï¿½nta med  2% per mï¿½nad. ï¿½garfï¿½rbehï¿½ll: levererade varor fï¿½rblir sï¿½ljarens egendom till full betalning skett.",15,125,"left",OUTPUT iY). */
           RUN pdf_Wrap_Text ("Spdf",hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE,15,125,"left",OUTPUT iY).
           RUN PageFooter.
           ASSIGN iRadNr     = 21. /* 22 */
           RUN SkrivColLabels.
           
           /* Tï¿½mmer momsloggen */
           FOR EACH TT_Mva:
             DELETE TT_Mva.
           END.

           FOR EACH TT_Kvitto:
             DELETE TT_Kvitto.
           END.
           
           IF iCount = 1 THEN
               qL:QUERY-OPEN().
           qL:GET-FIRST().
           ASSIGN iAntLinjer = 0
                  iAntMva = 0.
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
               IF dBruttoPris <> 0 AND lMedMva = TRUE THEN
                  dBruttoPris = dBruttoPris + (dec(hTTLinjeBuff:BUFFER-FIELD("Mvakr"):BUFFER-VALUE) / dAntal).
/*               dY = dY - 14.*/
               IF dY < 185 THEN DO:
                   iSidNr = iSidNr + 1.
                   RUN pdf_new_page ("Spdf").
                   RUN RitaRamar. /* pt kvar att gï¿½ra */
                   RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
                   /* Hï¿½r skriver vi ut notat. Fï¿½rst sï¿½tter vi Y */
                   dy = 120.
                   RUN pdf_set_TextY("Spdf",120).
                   RUN pdf_Wrap_Text ("Spdf",hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE,15,125,"left",OUTPUT iY).
                   RUN PageFooter.
                   RUN SkrivColLabels.
                   dY = 550 - 14.
               END.
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
/*               RUN pdf_text_xy_dec ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE) + " - " + STRING(dY),iColLbl[1],dY).
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
               RUN pdf_text_xy_dec ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("Varetekst"):BUFFER-VALUE),iColLbl[2],dY).
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
               dAntal      = hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE.
               RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dAntal,"->>,>>9")),iColLbl[3] - bredd(TRIM(STRING(dAntal,"->>,>>9"))),dY).
/*                RUN pdf_text_xy_dec ("Spdf",cLabel[4],iColLbl[4],dY). */
               RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dBruttoPris,"->>,>>9.99")),iColLbl[5] - bredd(TRIM(STRING(dBruttoPris,"->>,>>9.99"))),dY).*/
               dRabKr = dAntal * (hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE - hTTLinjeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE).
/*               RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dRabKr,"->>,>>9.99")),iColLbl[6] - bredd(TRIM(STRING(dRabKr,"->>,>>9.99"))),dY).
               cTxt = TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("nettolinjesum"):BUFFER-VALUE,"->>,>>9.99")).
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).*/

               ASSIGN dB_Id = hTTLinjeBuff:BUFFER-FIELD("B_Id"):BUFFER-VALUE.
               FIND FIRST TT_Kvitto WHERE TT_Kvitto.B_Id = dB_Id NO-ERROR.
               IF AVAILABLE TT_Kvitto THEN
               DO:
                  ASSIGN TT_Kvitto.dRabatt = TT_Kvitto.dRabatt + dRabKr.
                  ASSIGN TT_Kvitto.dBelopp = TT_Kvitto.dBelopp + hTTLinjeBuff:BUFFER-FIELD("nettolinjesum"):BUFFER-VALUE.
               END.
               ELSE
               DO:
                 IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN 
                 DO:
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   ASSIGN cRefTxt = TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE)).
                   ASSIGN cWrk = TRIM(REPLACE(cRefTxt,"Bong:","")).
                   ASSIGN cWrk = TRIM(REPLACE(cWrk,"Kvitto:","")).
                   DO iCount2 = LENGTH(cWrk) TO 1 BY -1:
                       ASSIGN cWrk2 = SUBSTRING(cWrk,iCount2,1).
                       IF cWrk2 = "/" THEN
                         LEAVE.
                   END.
                   ASSIGN iCount2 = iCount2 - 1.
                   ASSIGN cWrk = SUBSTRING(cWrk,1,iCount2).
                   IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                     ASSIGN cWrk = TRIM(REPLACE(cWrk,"But:","Butik:"))
                            cWrk = TRIM(REPLACE(cWrk,"Kas:","Kassa:"))
                            cWrk = TRIM(REPLACE(cWrk,"Dato:","Datum:")).
                 END.
                 ELSE
                   ASSIGN cWrk = STRING(dB_Id).
               CREATE TT_Kvitto.
               ASSIGN TT_Kvitto.B_Id = dB_Id
                      TT_Kvitto.cKvittoNr = cWrk
                      TT_Kvitto.dRabatt = dRabKr
                      TT_Kvitto.dBelopp = hTTLinjeBuff:BUFFER-FIELD("nettolinjesum"):BUFFER-VALUE.
                      IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN 
                         ASSIGN TT_Kvitto.cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE)
                                TT_Kvitto.cRefTxt = TT_Kvitto.cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE).
                      ELSE
                         ASSIGN TT_Kvitto.cRefTxt = "".
               END.
               qL:GET-NEXT().
           END. /* fakturalinjeloop */


           FOR EACH TT_Kvitto:
             ASSIGN dY = dY - 20.
             IF dY < 185 THEN 
             DO:
               iSidNr = iSidNr + 1.
               RUN pdf_new_page ("Spdf").
               RUN RitaRamar. /* pt kvar att gï¿½ra */
               RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
               dy = 120.
               RUN pdf_set_TextY("Spdf",120).
               RUN pdf_Wrap_Text ("Spdf",hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE,15,125,"left",OUTPUT iY).
               RUN PageFooter.
               RUN SkrivColLabels.
               dY = 550 - 14.
             END.
             RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
             RUN pdf_text_xy_dec ("Spdf",TT_Kvitto.cKvittoNr,iColLbl[1],dY).
             RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(TT_Kvitto.dRabatt,"->>,>>9.99")),iColLbl[6] - bredd(TRIM(STRING(TT_Kvitto.dRabatt,"->>,>>9.99"))),dY).
             cTxt = TRIM(STRING(TT_Kvitto.dBelopp,"->>,>>9.99")).
             RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).
             IF TT_Kvitto.cRefTxt <> "" THEN
             DO:
               dY = dY - 15.
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
               RUN pdf_text_xy_dec ("Spdf",TT_Kvitto.cRefTxt,iColLbl[2],dY).
             END.
           END.
           /* skriv mva */
           dYorg = dY.
           dY = 153 + (iAntMva * 12).
           dY = dY + 4 + 12.
           IF dYorg < dY THEN DO:
               /* beklagar, men vi mï¿½ste sidbryta */
               RUN pdf_new_page ("Spdf").
               RUN RitaRamar. /* pt kvar att gï¿½ra */
               RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
               /* Hï¿½r skriver vi ut notat. Fï¿½rst sï¿½tter vi Y */
               RUN pdf_set_TextY("Spdf",120).
    /*            RUN pdf_Wrap_Text ("Spdf","Vid betalning efter fï¿½rfallodatum debiteras drï¿½jsmï¿½lsrï¿½nta med  2% per mï¿½nad. ï¿½garfï¿½rbehï¿½ll: levererade varor fï¿½rblir sï¿½ljarens egendom till full betalning skett.",15,125,"left",OUTPUT iY). */
               RUN pdf_Wrap_Text ("Spdf",hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE,15,125,"left",OUTPUT iY).
               RUN PageFooter.
               RUN SkrivColLabels.
           END.

           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[1],dSpecCol[1] - bredd(cSpecLbl[1]),dY).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[2],dSpecCol[2] - bredd(cSpecLbl[2]),dY).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[3],dSpecCol[3] - bredd(cSpecLbl[3]),dY).  /* 20121029/ghg */
/*           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[3],dSpecCol[3] - bredd("Oresavr"),dY).*/
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
           IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
             ASSIGN cTxt = "ATT BETALA"
               iMinus = 0.
           ELSE
             ASSIGN cTxt = "TOTALT Å BETALE"
               iMinus = 8.
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - iMinus - bredd(cTxt),dY).
           dY = dY - 15.
           cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99").
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).

       END.
       qH:GET-NEXT().
   END.
   RUN pdf_close ("Spdf").
   IF lDirekte = FALSE THEN
       RUN browse2pdf\viewxmldialog.w (cFilNavn,"FAKTURA").
   ELSE DO ii = 1 TO iAntEks:
       OS-COMMAND SILENT VALUE(cCmd + ' ' + cFilnavn + ' "' + cPrinter + '"').
       /*OS-COMMAND SILENT VALUE(".\cmd\PrintPdf.cmd " + cFilnavn + ' "' + cPrinter + '"').*/
   END.

END PROCEDURE. /* SkrivRapportPDF2 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivRapportPDF3) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapportPDF3 Procedure 
PROCEDURE SkrivRapportPDF3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dFaktura_id  AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE cFilNavn     AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE iBilagsType  AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cFakturaType AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE cFakturaNr   AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE iSidNr       AS INTEGER            NO-UNDO.
   DEFINE VARIABLE iCount       AS INTEGER            NO-UNDO.
   DEFINE VARIABLE iRadNr       AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cDetaljRad   AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE dBruttoPris  AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE dRabKr       AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE dAntal       AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE iAntLinjer   AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cRefTxt      AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE iKontrollRad AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cTxt         AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE dY           AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE dYorg        AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE iY           AS INTEGER            NO-UNDO.
   DEFINE VARIABLE iAntMva      AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cSpecLbl     AS CHARACTER EXTENT 4 NO-UNDO.
   DEFINE VARIABLE dSpecCol     AS DECIMAL   EXTENT 4 NO-UNDO.
   DEFINE VARIABLE ii           AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cVaruText    AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE iMinus       AS INTEGER            NO-UNDO.

   {syspara.i 19 12 51 cVaruText}
   
   /* anvï¿½nds i SkrivColLabels och hï¿½r */
   ASSIGN iLeftCol   = 50
          iColLbl[1] = iLeftCol
          iColLbl[2] = 130
          iColLbl[3] = 317
          iColLbl[4] = 320
          iColLbl[5] = 415
          iColLbl[6] = 474
          iColLbl[7] = 545.

 IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
   ASSIGN cSpecLbl[1] = "Exkl moms"
          cSpecLbl[2] = "Moms%"             /* "Mva%" */
          cSpecLbl[3] = "Moms kr"
          cSpecLbl[4] = "Öresavr".
 ELSE
   ASSIGN cSpecLbl[1] = "Exkl mva"
          cSpecLbl[2] = "Mva%"             /* "Mva%" */
          cSpecLbl[3] = "Mva kr"
          cSpecLbl[4] = "Øresavr".
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
   
   dFaktura_id = hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE.
   /*cFilNavn = SESSION:TEMP-DIR + "Faktura" + "_" + STRING(TIME) + ".pdf".*/
   cFilNavn = cUtskrift + "Faktura" + "_" + STRING(dFaktura_id) + '-' + REPLACE(STRING(TODAY),'/','') + '-' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".pdf".
   RUN pdf_new ("Spdf",cFilNavn).
   RUN pdf_set_BottomMargin ("Spdf", 20).
   RUN pdf_set_PaperType ("Spdf","A4").
   RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
   RUN pdf_set_Orientation ("Spdf","portrait").
   iPageHeight = pdf_PageHeight ("Spdf").
   iPageWidth  = pdf_PageWidth ("Spdf").
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE Faktura_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)).

       ASSIGN iBilagstype = hTTHodeBuff:BUFFER-FIELD("BilagsType"):BUFFER-VALUE.
       CASE iBilagstype:
           WHEN 1 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                                          /* IF hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE > 0 THEN
                                              TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE)) ELSE "" */ /* ghg */
                            cFakturaType = STRING(cFakturaNr <> "","Faktura/Proformafaktura"). 
           WHEN 2 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                                          /* IF hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE > 0 THEN
                                              TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE)) ELSE "" */ /* ghg */
                            cFakturaType = STRING(cFakturaNr <> "","Kreditnota/ProformaKredit"). 
           WHEN 5 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE
                                           IF hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE > 0 THEN
                                              TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE)) ELSE ""  /* ghg */
                              cFakturaType = "Utbetalning". 
           WHEN 10 THEN ASSIGN cFakturaNr   = ""
                               cFakturaType = "Betalningspåminnelse". 
           OTHERWISE ASSIGN cFakturaNr   = ""
                            cFakturaType = "". 
       END CASE.
       
       dY = 550.

/*        DO iCount = 1 TO iAntEks: */
       DO:
           iCount = 1.
           iSidNr = 1.
           RUN pdf_new_page ("Spdf").
           RUN RitaRamar. /* pt kvar att gï¿½ra */
           RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
           RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
           /* Hï¿½r skriver vi ut notat. Fï¿½rst sï¿½tter vi Y */
           RUN pdf_set_TextY("Spdf",120).
/*            RUN pdf_Wrap_Text ("Spdf","Vid betalning efter fï¿½rfallodatum debiteras drï¿½jsmï¿½lsrï¿½nta med  2% per mï¿½nad. ï¿½garfï¿½rbehï¿½ll: levererade varor fï¿½rblir sï¿½ljarens egendom till full betalning skett.",15,125,"left",OUTPUT iY). */
           RUN pdf_Wrap_Text ("Spdf",hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE,15,125,"left",OUTPUT iY).
           RUN PageFooter.
           ASSIGN iRadNr     = 21. /* 22 */
           RUN SkrivColLabels.
           
           /* Tï¿½mmer momsloggen */
           FOR EACH TT_Mva:
             DELETE TT_Mva.
           END.
           
           IF iCount = 1 THEN
               qL:QUERY-OPEN().
           qL:GET-FIRST().
           ASSIGN iAntLinjer = 0
                  iAntMva = 0.
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

           dY = dY - 40.

           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",20).
           IF cVaruText = "" THEN
             RUN pdf_text_xy_dec ("Spdf","Varor enligt följesedlar.",iColLbl[2],dY).
           ELSE
             RUN pdf_text_xy_dec ("Spdf",cVaruText,iColLbl[2],dY).
           
           /* Skriv moms */
           dY = 153 + (iAntMva * 12).
           dY = dY + 4 + 12.

           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[1],dSpecCol[1] - bredd(cSpecLbl[1]),dY).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[2],dSpecCol[2] - bredd(cSpecLbl[2]),dY).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[3],dSpecCol[3] - bredd(cSpecLbl[3]),dY).  /* 20121029/ghg */
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
               IF LAST-OF(tt_mva.mva%) AND hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE <> 0 THEN DO:
                   cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE,"->,>>>,>>9.99").
                   RUN pdf_text_xy_dec ("Spdf",cTxt,dSpecCol[4] - bredd(cTxt),dY).
               END.
               dY = dY - 12.
           END.

           /* Skriv Att betala */
           dY =  180.
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
           IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
             ASSIGN cTxt = "ATT BETALA"
                    iMinus = 0.
           ELSE
             ASSIGN cTxt = "TOTALT Å BETALE"
                    iMinus = 8.
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - iMinus - bredd(cTxt),dY).
           dY = dY - 15.
           cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99").
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).


       END.
       qH:GET-NEXT().
   END.
   RUN pdf_close ("Spdf").
   IF lDirekte = FALSE THEN
       RUN browse2pdf\viewxmldialog.w (cFilNavn,"FAKTURA").
   ELSE DO ii = 1 TO iAntEks:
       OS-COMMAND SILENT VALUE(cCmd + ' ' + cFilnavn + ' "' + cPrinter + '"').       
       /*OS-COMMAND SILENT VALUE(".\cmd\PrintPdf.cmd " + cFilnavn + ' "' + cPrinter + '"').*/
   END.

END PROCEDURE. /* SkrivRapportPDF3 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivRapportPDF4) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapportPDF4 Procedure 
PROCEDURE SkrivRapportPDF4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR dFaktura_id  AS DECIMAL   NO-UNDO.
   DEF VAR cFilNavn     AS CHARACTER NO-UNDO.
   DEF VAR iBilagsType  AS INTEGER   NO-UNDO.
   DEF VAR cFakturaType AS CHARACTER NO-UNDO.
   DEF VAR cFakturaNr   AS CHARACTER NO-UNDO.
   DEF VAR iSidNr       AS INTEGER   NO-UNDO.
   DEF VAR iCount       AS INTEGER   NO-UNDO.
   DEF VAR iRadNr       AS INTEGER   NO-UNDO.
   DEF VAR cDetaljRad   AS CHARACTER NO-UNDO.
   DEF VAR dBruttoPris  AS DECIMAL   NO-UNDO.
   DEF VAR dBruttoExMva  AS DECIMAL   NO-UNDO.
   DEF VAR dRabKr       AS DECIMAL   NO-UNDO.
   DEF VAR dAntal       AS DECIMAL   NO-UNDO.
   DEF VAR iAntLinjer   AS INTEGER   NO-UNDO.
   DEF VAR cRefTxt      AS CHARACTER NO-UNDO.
   DEF VAR iKontrollRad AS INTEGER   NO-UNDO.
   DEF VAR cTxt         AS CHARACTER NO-UNDO.
   DEF VAR dY           AS DECIMAL   NO-UNDO.
   DEF VAR dYorg        AS DECIMAL   NO-UNDO.
   DEF VAR iY           AS INTEGER   NO-UNDO.
   DEF VAR iAntMva      AS INTEGER   NO-UNDO.
   DEF VAR cSpecLbl     AS CHARACTER EXTENT 4 NO-UNDO.
   DEF VAR dSpecCol     AS DECIMAL   EXTENT 4 NO-UNDO.
   DEF VAR ii           AS INTEGER   NO-UNDO.
   DEF VAR dB_Id        AS DECIMAL   NO-UNDO.
   DEF VAR iMinus       AS INTEGER   NO-UNDO.
   DEF VAR lSamleFakt   AS LOGICAL   NO-UNDO.
   DEF VAR lFirstaRad   AS LOGICAL   NO-UNDO.
   DEF VAR dWrk         AS DECIMAL   NO-UNDO.
   DEF VAR dMvaKoeff    AS DECIMAL   NO-UNDO.
   DEF VAR dNormalpris  AS DECIMAL   NO-UNDO.
   DEF VAR iColLbl      AS INTEGER EXTENT 8  NO-UNDO.
   /* anvï¿½nds i SkrivColLabels och hï¿½r */
   ASSIGN iLeftCol   = 50
          iColLbl[1] = iLeftCol
          iColLbl[2] = 130  /* 130 */
          iColLbl[3] = 300  /* 317 */
          iColLbl[4] = 350  /* 320 */
          iColLbl[5] = 400  /* 415 */
          iColLbl[6] = 450  /* 474 */
          iColLbl[7] = 483. /* 545 */
          iColLbl[8] = 545. /* 545 */

 IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
   ASSIGN cSpecLbl[1] = "Exkl moms"
          cSpecLbl[2] = "Moms%"
          cSpecLbl[3] = "Moms kr"
          cSpecLbl[4] = "Öresavr".
 ELSE
   ASSIGN cSpecLbl[1] = "Totalsum eks. mva"
          cSpecLbl[2] = "Mva%" 
          cSpecLbl[3] = "Mva kr"
          cSpecLbl[4] = "Øresavr".
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
   dFaktura_id = hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE.
   cEmail = "".
   FIND fakturahode WHERE fakturahode.faktura_id = dFaktura_id NO-LOCK NO-ERROR.
   IF AVAIL fakturahode THEN DO:
       FIND kunde WHERE kunde.kundenr = fakturahode.kundenr NO-LOCK NO-ERROR.
       IF AVAIL kunde THEN DO:
           cEmail = Kunde.KontE-Post.
/*            IF cEmail = "" THEN              */
/*                cEmail = Kunde.ePostAdresse. */
       END.
   END.
   cFilNavn = cUtskrift + "Faktura" + "_" + STRING(dFaktura_id) + '-' + REPLACE(STRING(TODAY),'/','') + '-' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".pdf".
   RUN pdf_new ("Spdf",cFilNavn).
   RUN pdf_set_BottomMargin ("Spdf", 20).
   RUN pdf_set_PaperType ("Spdf","A4").
   RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
   RUN pdf_set_Orientation ("Spdf","portrait").
   iPageHeight = pdf_PageHeight ("Spdf").
   iPageWidth  = pdf_PageWidth ("Spdf").
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE Faktura_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)).

       ASSIGN iBilagstype = hTTHodeBuff:BUFFER-FIELD("BilagsType"):BUFFER-VALUE.
       CASE iBilagstype:
           WHEN 1 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                            cFakturaType = STRING(cFakturaNr <> "",cFakturaTekst).
                             
           WHEN 2 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                            cFakturaType = STRING(cFakturaNr <> "","Kreditnota/ProformaKredit"). 
           WHEN 5 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE 
                                    IF hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE > 0 THEN
                                              TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE)) ELSE ""  /* ghg */
                              cFakturaType = "Utbetalning". 
           WHEN 10 THEN ASSIGN cFakturaNr   = ""
                               cFakturaType = "Betalningspåminnelse". 
           OTHERWISE ASSIGN cFakturaNr   = ""
                            cFakturaType = "". 
       END CASE.

       IF hTTHodeBuff:BUFFER-FIELD("SamleFaktura"):BUFFER-VALUE  = TRUE THEN
         ASSIGN lSamleFakt = TRUE.
       ELSE
         ASSIGN lSamleFakt = FALSE.

       dY = 550.
       DO:
           iCount = 1.
           iSidNr = 1.
           RUN pdf_new_page ("Spdf").
           RUN RitaRamar. /* pt kvar att gï¿½ra */
           RUN SkrivHeaderPDF_4(iSidnr,cFakturaNr,cFakturaType,FALSE).
           RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
           /* Hï¿½r skriver vi ut notat. Fï¿½rst sï¿½tter vi Y */
           RUN pdf_set_TextY("Spdf",120).
           RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
           RUN PageFooter.
           ASSIGN iRadNr     = 21. /* 22 */
           RUN SkrivColLabels4 (iColLbl).
           /* Tï¿½mmer momsloggen */
           FOR EACH TT_Mva:
             DELETE TT_Mva.
           END.
           
           IF iCount = 1 THEN
               qL:QUERY-OPEN().
           qL:GET-FIRST().
           ASSIGN iAntLinjer = 0
                  iAntMva = 0.
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
           
           ASSIGN lFirstaRad = TRUE.
           qL:GET-FIRST().
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer - 1.
                      dMvaKoeff  = (dec(hTTLinjeBuff:BUFFER-FIELD("Mva%"):BUFFER-VALUE) / 100).
               ASSIGN dBruttoPris = ABS(dec(hTTLinjeBuff:BUFFER-FIELD("Linjesum"):BUFFER-VALUE))
                      dAntal      = ABS(dec(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE))
                      dBruttoPris = IF dec(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE) > 0
                                      THEN dec(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE)
                                      ELSE dBruttoPris
                      dBruttoPris = IF dBruttoPris = ? THEN 0 ELSE dBruttoPris.
               IF dBruttoPris <> 0 THEN DO:
                  dBruttoExMva = dBruttoPris.
                  dBruttoPris = dBruttoPris + ROUND(dMvaKoeff * dBruttoPris,2).
               END.
               dY = dY - 14.
               IF dY < 190 THEN DO:    /* 185*/
                   iSidNr = iSidNr + 1.
                   RUN pdf_new_page ("Spdf").
                   RUN RitaRamar. /* pt kvar att gï¿½ra */
                   RUN SkrivHeaderPDF_4(iSidnr,cFakturaNr,cFakturaType,FALSE).
                   RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
                   RUN pdf_set_TextY("Spdf",120).
                   RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
                   RUN PageFooter.
                   RUN SkrivColLabels4 (iColLbl).
                   dY = 550 - 14.
               END.
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
               RUN pdf_text_xy_dec ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE) + " - " + STRING(dY),iColLbl[1],dY).
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
               RUN pdf_text_xy_dec ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("Varetekst"):BUFFER-VALUE),iColLbl[2],dY).
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
               dAntal      = hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE.
               RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dAntal,"->>,>>9")),iColLbl[3] - bredd(TRIM(STRING(dAntal,"->>,>>9"))),dY).
               
               dNormalpris      = hTTLinjeBuff:BUFFER-FIELD("Normalpris"):BUFFER-VALUE.
               
               IF dNormalpris > 0 THEN
                   RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dNormalpris,"->>,>>9.99")),iColLbl[4] - bredd(TRIM(STRING(dNormalpris,"->>,>>9.99"))),dY).

               RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dBruttoPris,"->>,>>9.99")),iColLbl[5] - bredd(TRIM(STRING(dBruttoPris,"->>,>>9.99"))),dY).
               RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dBruttoExmva,"->>,>>9.99")),iColLbl[6] - bredd(TRIM(STRING(dBruttoExmva,"->>,>>9.99"))),dY).

               dRabKr = dAntal * (hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE - hTTLinjeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE).
               IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                 RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dRabKr,"->>,>>9.99")),iColLbl[7] - bredd(TRIM(STRING(dRabKr,"->>,>>9.99"))),dY).
               ELSE
                 RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE,"->>,>>9.99")),iColLbl[7] - bredd(TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE,"->>,>>9.99"))),dY).
               ASSIGN dWrk = DEC(hTTLinjeBuff:BUFFER-FIELD("nettolinjesum"):BUFFER-VALUE) + 0.
               cTxt = TRIM(STRING(dWrk,"->>,>>9.99")).
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[8] - bredd(cTxt),dY).
               IF lSamleFakt = TRUE THEN
               DO:
                 IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN 
                 DO:
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   cRefTxt = TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE)).
                   ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Bong:","")).
                   ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Kvitto:","")).
                   IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                     ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"But:","Butik:"))
                            cRefTxt = TRIM(REPLACE(cRefTxt,"Kas:","Kassa:"))
                            cRefTxt = TRIM(REPLACE(cRefTxt,"Dato:","Datum:")).
                   dY = dY - 15.
                   IF dY < 185 THEN
                   DO:
                     iSidNr = iSidNr + 1.
                     RUN pdf_new_page ("Spdf").
                     RUN RitaRamar. /* pt kvar att gï¿½ra */
                     RUN SkrivHeaderPDF_4(iSidnr,cFakturaNr,cFakturaType,FALSE).
                     RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
                     RUN pdf_set_TextY("Spdf",120).
                     RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
                     RUN PageFooter.
                     RUN SkrivColLabels4 (iColLbl).
                     dY = 550 - 15.
                   END.
                   ASSIGN dB_Id = hTTLinjeBuff:BUFFER-FIELD("B_Id"):BUFFER-VALUE.
                   FIND FIRST BongHode WHERE BongHode.b_id = dB_Id NO-LOCK NO-ERROR.
                   IF AVAILABLE Bonghode THEN
                     ASSIGN cRefTxt = cRefTxt + " KortNr: " + IF BongHode.KundeKort <> "" THEN BongHode.KundeKort ELSE BongHode.MedlemsKort.
                   RUN pdf_text_xy_dec ("Spdf",cRefTxt,iColLbl[2],dY).
                 END.
                 IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN 
                 DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE)
                          cRefTxt = cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   dY = dY - 15.
                   RUN pdf_text_xy_dec ("Spdf",TRIM(cRefTxt),iColLbl[2],dY).
                 END.
               END.
               ELSE IF lFirstaRad = TRUE THEN
               DO:
                 ASSIGN lFirstaRad = FALSE.
                 IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN 
                 DO:
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   cRefTxt = TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE)).
                   ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Bong:","")).
                   ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Kvitto:","")).
                   IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                     ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"But:","Butik:"))
                            cRefTxt = TRIM(REPLACE(cRefTxt,"Kas:","Kassa:"))
                            cRefTxt = TRIM(REPLACE(cRefTxt,"Dato:","Datum:")).
                   RUN pdf_text_xy_dec ("Spdf",cRefTxt,iLeftCol,iPageHeight - 175).
                 END.
                 IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN 
                 DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE)
                          cRefTxt = cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   RUN pdf_text_xy_dec ("Spdf",TRIM(cRefTxt),iLeftCol,iPageHeight - 190).
                 END.
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE) <> "" THEN 
               DO:
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
             /* beklagar, men vi mï¿½ste sidbryta */
             RUN pdf_new_page ("Spdf").
             RUN RitaRamar. /* pt kvar att gï¿½ra */
             RUN SkrivHeaderPDF_4(iSidnr,cFakturaNr,cFakturaType,FALSE).
             RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
             /* Hï¿½r skriver vi ut notat. Fï¿½rst sï¿½tter vi Y */
             RUN pdf_set_TextY("Spdf",120).
             RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
             RUN PageFooter.
             RUN SkrivColLabels4 (iColLbl).
           END.
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[1],dSpecCol[1] - bredd(cSpecLbl[1]),dY).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[2],dSpecCol[2] - bredd(cSpecLbl[2]),dY).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[3],dSpecCol[3] - bredd(cSpecLbl[3]),dY).  /* 20121029/ghg */
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
               IF LAST-OF(tt_mva.mva%) AND hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE <> 0 THEN DO:
                   cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE,"->,>>>,>>9.99").
                   RUN pdf_text_xy_dec ("Spdf",cTxt,dSpecCol[4] - bredd(cTxt),dY).
               END.
               dY = dY - 12.
           END.
           /* Skriv Att betala */
           dY =  180.
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
           IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
             ASSIGN cTxt = "ATT BETALA"
                    iMinus = 0.
           ELSE
             ASSIGN cTxt = "TOTALT ï¿½ BETALE"
                    iMinus = 8.
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[8] - iMinus - bredd(cTxt),dY).
           dY = dY - 15.
           cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99").
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[8] - bredd(cTxt),dY).
       END.
       qH:GET-NEXT().
   END.
   RUN pdf_close ("Spdf").
   
   IF lDirekte = FALSE THEN
       RUN browse2pdf\viewxmldialog.w (cFilNavn,"FAKTURA").
   ELSE DO ii = 1 TO iAntEks:
       OS-COMMAND SILENT VALUE(cCmd + ' ' + cFilnavn + ' "' + cPrinter + '"').
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivRapportPDF4ORG) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapportPDF4ORG Procedure 
PROCEDURE SkrivRapportPDF4ORG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dFaktura_id  AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE cFilNavn     AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE iBilagsType  AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cFakturaType AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE cFakturaNr   AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE iSidNr       AS INTEGER            NO-UNDO.
   DEFINE VARIABLE iCount       AS INTEGER            NO-UNDO.
   DEFINE VARIABLE iRadNr       AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cDetaljRad   AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE dBruttoPris  AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE dRabKr       AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE dAntal       AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE iAntLinjer   AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cRefTxt      AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE iKontrollRad AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cTxt         AS CHARACTER          NO-UNDO.
   DEFINE VARIABLE dY           AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE dYorg        AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE iY           AS INTEGER            NO-UNDO.
   DEFINE VARIABLE iAntMva      AS INTEGER            NO-UNDO.
   DEFINE VARIABLE cSpecLbl     AS CHARACTER EXTENT 4 NO-UNDO.
   DEFINE VARIABLE dSpecCol     AS DECIMAL   EXTENT 4 NO-UNDO.
   DEFINE VARIABLE ii           AS INTEGER            NO-UNDO.
   DEFINE VARIABLE dB_Id        AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE iMinus       AS INTEGER            NO-UNDO.
   DEFINE VARIABLE lSamleFakt   AS LOGICAL            NO-UNDO.
   DEFINE VARIABLE lFirstaRad   AS LOGICAL            NO-UNDO.
   DEFINE VARIABLE dWrk         AS DECIMAL            NO-UNDO.
   DEFINE VARIABLE dMvaKoeff    AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE dNormalpris  AS DECIMAL     NO-UNDO.
   /* anvï¿½nds i SkrivColLabels och hï¿½r */
   ASSIGN iLeftCol   = 50
          iColLbl[1] = iLeftCol
          iColLbl[2] = 130  /* 130 */
          iColLbl[3] = 300  /* 317 */
          iColLbl[4] = 360  /* 320 */
          iColLbl[5] = 430  /* 415 */
          iColLbl[6] = 480  /* 474 */
          iColLbl[7] = 545. /* 545 */

 IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
   ASSIGN cSpecLbl[1] = "Exkl moms"
          cSpecLbl[2] = "Moms%"             /* "Mva%" */
          cSpecLbl[3] = "Moms kr"
          cSpecLbl[4] = "Öresavr".
 ELSE
   ASSIGN cSpecLbl[1] = "Totalsum eks. mva"
          cSpecLbl[2] = "Mva%"             /* "Mva%" */
          cSpecLbl[3] = "Mva kr"
          cSpecLbl[4] = "Øresavr".
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
   
   dFaktura_id = hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE.
   /*cFilNavn = SESSION:TEMP-DIR + "Faktura" + "_" + STRING(dFaktura_id) + ".pdf".*/
   cFilNavn = cUtskrift + "Faktura" + "_" + STRING(dFaktura_id) + '-' + REPLACE(STRING(TODAY),'/','') + '-' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".pdf".
   
   RUN pdf_new ("Spdf",cFilNavn).
   RUN pdf_set_BottomMargin ("Spdf", 20).
   RUN pdf_set_PaperType ("Spdf","A4").
   RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
   RUN pdf_set_Orientation ("Spdf","portrait").
   iPageHeight = pdf_PageHeight ("Spdf").
   iPageWidth  = pdf_PageWidth ("Spdf").
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE Faktura_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)).

       ASSIGN iBilagstype = hTTHodeBuff:BUFFER-FIELD("BilagsType"):BUFFER-VALUE.
       CASE iBilagstype:
           WHEN 1 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                            cFakturaType = STRING(cFakturaNr <> "",cFakturaTekst).
                             
           WHEN 2 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE ""
                            cFakturaType = STRING(cFakturaNr <> "","Kreditnota/ProformaKredit"). 
           WHEN 5 THEN ASSIGN cFakturaNr = IF hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE) ELSE 
                                           IF hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE > 0 THEN
                                              TRIM(STRING(hTTHodeBuff:BUFFER-FIELD("Faktura_Id"):BUFFER-VALUE)) ELSE ""  /* ghg */
                              cFakturaType = "Utbetalning". 
           WHEN 10 THEN ASSIGN cFakturaNr   = ""
                               cFakturaType = "Betalningspåminnelse". 
           OTHERWISE ASSIGN cFakturaNr   = ""
                            cFakturaType = "". 
       END CASE.

       IF hTTHodeBuff:BUFFER-FIELD("SamleFaktura"):BUFFER-VALUE  = TRUE THEN
         ASSIGN lSamleFakt = TRUE.
       ELSE
         ASSIGN lSamleFakt = FALSE.

       dY = 550.
       
/*        DO iCount = 1 TO iAntEks: */
       DO:
           iCount = 1.
           iSidNr = 1.
           RUN pdf_new_page ("Spdf").
           RUN RitaRamar. /* pt kvar att gï¿½ra */
           RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
           RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
           /* Hï¿½r skriver vi ut notat. Fï¿½rst sï¿½tter vi Y */
           RUN pdf_set_TextY("Spdf",120).
           RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
           RUN PageFooter.
           ASSIGN iRadNr     = 21. /* 22 */
           RUN SkrivColLabels4.
           
           /* Tï¿½mmer momsloggen */
           FOR EACH TT_Mva:
             DELETE TT_Mva.
           END.
           
           IF iCount = 1 THEN
               qL:QUERY-OPEN().
           qL:GET-FIRST().
           ASSIGN iAntLinjer = 0
                  iAntMva = 0.
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
           
           ASSIGN lFirstaRad = TRUE.
           qL:GET-FIRST().
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer - 1.
                      dMvaKoeff  = (dec(hTTLinjeBuff:BUFFER-FIELD("Mva%"):BUFFER-VALUE) / 100).
               ASSIGN dBruttoPris = ABS(dec(hTTLinjeBuff:BUFFER-FIELD("Linjesum"):BUFFER-VALUE))
                      dAntal      = ABS(dec(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE))
                      dBruttoPris = IF dec(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE) > 0
                                      THEN dec(hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE)
                                      ELSE dBruttoPris
                      dBruttoPris = IF dBruttoPris = ? THEN 0 ELSE dBruttoPris.
               IF dBruttoPris <> 0 THEN
                  dBruttoPris = dBruttoPris + ROUND(dMvaKoeff * dBruttoPris,2).
               dY = dY - 14.
               IF dY < 190 THEN DO:    /* 185*/
                   iSidNr = iSidNr + 1.
                   RUN pdf_new_page ("Spdf").
                   RUN RitaRamar. /* pt kvar att gï¿½ra */
                   RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
                   RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
                   RUN pdf_set_TextY("Spdf",120).
                   RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
                   RUN PageFooter.
                   RUN SkrivColLabels4.
                   dY = 550 - 14.
               END.
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
               RUN pdf_text_xy_dec ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("VareNr"):BUFFER-VALUE) + " - " + STRING(dY),iColLbl[1],dY).
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
               RUN pdf_text_xy_dec ("Spdf",STRING(hTTLinjeBuff:BUFFER-FIELD("Varetekst"):BUFFER-VALUE),iColLbl[2],dY).
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
               dAntal      = hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE.
               RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dAntal,"->>,>>9")),iColLbl[3] - bredd(TRIM(STRING(dAntal,"->>,>>9"))),dY).
               
               dNormalpris      = hTTLinjeBuff:BUFFER-FIELD("Normalpris"):BUFFER-VALUE.
               IF dNormalpris > 0 THEN
                   RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dNormalpris,"->>,>>9.99")),iColLbl[4] - bredd(TRIM(STRING(dNormalpris,"->>,>>9.99"))),dY).

               RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dBruttoPris,"->>,>>9.99")),iColLbl[5] - bredd(TRIM(STRING(dBruttoPris,"->>,>>9.99"))),dY).

               dRabKr = dAntal * (hTTLinjeBuff:BUFFER-FIELD("Pris"):BUFFER-VALUE - hTTLinjeBuff:BUFFER-FIELD("NettoPris"):BUFFER-VALUE).
               IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                 RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(dRabKr,"->>,>>9.99")),iColLbl[6] - bredd(TRIM(STRING(dRabKr,"->>,>>9.99"))),dY).
               ELSE
                 RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE,"->>,>>9.99")),iColLbl[6] - bredd(TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeRab%"):BUFFER-VALUE,"->>,>>9.99"))),dY).
               ASSIGN dWrk = DEC(hTTLinjeBuff:BUFFER-FIELD("nettolinjesum"):BUFFER-VALUE) + 0.
/*  */
/*                    dec(hTTLinjeBuff:BUFFER-FIELD("Mvakr"):BUFFER-VALUE). */
               cTxt = TRIM(STRING(dWrk,"->>,>>9.99")).
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).
               IF lSamleFakt = TRUE THEN
               DO:
                 IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN 
                 DO:
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   cRefTxt = TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE)).
                   ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Bong:","")).
                   ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Kvitto:","")).
                   IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                     ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"But:","Butik:"))
                            cRefTxt = TRIM(REPLACE(cRefTxt,"Kas:","Kassa:"))
                            cRefTxt = TRIM(REPLACE(cRefTxt,"Dato:","Datum:")).
                   dY = dY - 15.
                   IF dY < 185 THEN
                   DO:
                     iSidNr = iSidNr + 1.
                     RUN pdf_new_page ("Spdf").
                     RUN RitaRamar. /* pt kvar att gï¿½ra */
                     RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
                     RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
                     RUN pdf_set_TextY("Spdf",120).
                     RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
                     RUN PageFooter.
                     RUN SkrivColLabels4.
                     dY = 550 - 15.
                   END.
                   ASSIGN dB_Id = hTTLinjeBuff:BUFFER-FIELD("B_Id"):BUFFER-VALUE.
                   FIND FIRST BongHode WHERE BongHode.b_id = dB_Id NO-LOCK NO-ERROR.
                   IF AVAILABLE Bonghode THEN
                     ASSIGN cRefTxt = cRefTxt + " KortNr: " + IF BongHode.KundeKort <> "" THEN BongHode.KundeKort ELSE BongHode.MedlemsKort.
                   RUN pdf_text_xy_dec ("Spdf",cRefTxt,iColLbl[2],dY).
                 END.
                 IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN 
                 DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE)
                          cRefTxt = cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   dY = dY - 15.
                   RUN pdf_text_xy_dec ("Spdf",TRIM(cRefTxt),iColLbl[2],dY).
                 END.
               END.
               ELSE IF lFirstaRad = TRUE THEN
               DO:
                 ASSIGN lFirstaRad = FALSE.
                 IF TRIM(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE) <> "" THEN 
                 DO:
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   cRefTxt = TRIM(STRING(hTTLinjeBuff:BUFFER-FIELD("Notat"):BUFFER-VALUE)).
                   ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Bong:","")).
                   ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"Kvitto:","")).
                   IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
                     ASSIGN cRefTxt = TRIM(REPLACE(cRefTxt,"But:","Butik:"))
                            cRefTxt = TRIM(REPLACE(cRefTxt,"Kas:","Kassa:"))
                            cRefTxt = TRIM(REPLACE(cRefTxt,"Dato:","Datum:")).
                   RUN pdf_text_xy_dec ("Spdf",cRefTxt,iLeftCol,iPageHeight - 175).
                 END.
                 IF TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE) <> "" OR TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE) <> "" THEN 
                 DO:
                   ASSIGN cRefTxt = TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefId"):BUFFER-VALUE)
                          cRefTxt = cRefTxt + (IF cReftxt <> "" THEN " " ELSE "") + TRIM(hTTLinjeBuff:BUFFER-FIELD("EkstRefTekst"):BUFFER-VALUE).
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer - 1.
                   RUN pdf_text_xy_dec ("Spdf",TRIM(cRefTxt),iLeftCol,iPageHeight - 190).
                 END.
               END.
               IF TRIM(hTTLinjeBuff:BUFFER-FIELD("ArbeidsBeskr"):BUFFER-VALUE) <> "" THEN 
               DO:
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
             /* beklagar, men vi mï¿½ste sidbryta */
             RUN pdf_new_page ("Spdf").
             RUN RitaRamar. /* pt kvar att gï¿½ra */
             RUN SkrivHeaderPDF(iSidnr,cFakturaNr,cFakturaType,FALSE).
             RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
             /* Hï¿½r skriver vi ut notat. Fï¿½rst sï¿½tter vi Y */
             RUN pdf_set_TextY("Spdf",120).
             RUN pdf_Wrap_Text ("Spdf",TRIM(hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE),15,125,"left",OUTPUT iY).
             RUN PageFooter.
             RUN SkrivColLabels4.
           END.
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[1],dSpecCol[1] - bredd(cSpecLbl[1]),dY).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[2],dSpecCol[2] - bredd(cSpecLbl[2]),dY).
           RUN pdf_text_xy_dec ("Spdf",cSpecLbl[3],dSpecCol[3] - bredd(cSpecLbl[3]),dY).  /* 20121029/ghg */
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
               IF LAST-OF(tt_mva.mva%) AND hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE <> 0 THEN DO:
                   cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("AvrundingKr"):BUFFER-VALUE,"->,>>>,>>9.99").
                   RUN pdf_text_xy_dec ("Spdf",cTxt,dSpecCol[4] - bredd(cTxt),dY).
               END.
               dY = dY - 12.
           END.
           /* Skriv Att betala */
           dY =  180.
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
           IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
             ASSIGN cTxt = "ATT BETALA"
                    iMinus = 0.
           ELSE
             ASSIGN cTxt = "TOTALT ï¿½ BETALE"
                    iMinus = 8.
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - iMinus - bredd(cTxt),dY).
           dY = dY - 15.
           cTxt = STRING(hTTHodeBuff:BUFFER-FIELD("Totalt"):BUFFER-VALUE,"->,>>>,>>9.99").
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).
       END.
       qH:GET-NEXT().
   END.
   RUN pdf_close ("Spdf").
   
   IF lDirekte = FALSE THEN
       RUN browse2pdf\viewxmldialog.w (cFilNavn,"FAKTURA").
   ELSE DO ii = 1 TO iAntEks:
       OS-COMMAND SILENT VALUE(cCmd + ' ' + cFilnavn + ' "' + cPrinter + '"').
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
  
/*  RETURN IF ipcPrinter <> "" THEN ipcPrinter ELSE                                                                        */
/*      IF DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER") <> "" THEN                                                */
/*          DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER") ELSE SESSION:PRINTER-NAME.   /* Function return value. */*/

  RETURN IF ipcPrinter <> "" THEN 
            ipcPrinter 
         ELSE 
            SESSION:PRINTER-NAME.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

