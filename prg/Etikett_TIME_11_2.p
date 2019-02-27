&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

/* DEF INPUT  PARAM icParam     AS CHAR  NO-UNDO.  */
/* DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO. */
/* DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.  */
/* DEF OUTPUT PARAM ocReturn    AS CHAR  NO-UNDO.  */
/* DEF OUTPUT PARAM obOk        AS LOG NO-UNDO.    */

DEF VAR icParam     AS CHAR  NO-UNDO. 
DEF VAR ihBuffer    AS HANDLE NO-UNDO.
DEF VAR icSessionId AS CHAR  NO-UNDO.
DEF VAR ocReturn    AS CHAR  NO-UNDO.
DEF VAR obOk        AS LOG NO-UNDO. 



DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR fReklamasjonsNr AS INT    NO-UNDO.
DEF VAR iStatus         AS INT    NO-UNDO.

DEFINE VARIABLE cFilNavn       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLeftMargin    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRMarginPos    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageHeight    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageWidth     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCL            AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFirmanavn     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAdress        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPostadress AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE tt_Artbas NO-UNDO LIKE Artbas.

{etikettlogg.i}
{ pdf_inc.i "THIS-PROCEDURE"}
/* { pdf_inc.i "NOT SUPER"} */
/* DEFINE SHARED TEMP-TABLE TT_Etikett NO-UNDO LIKE Etikett */
/* FIELD Kode LIKE StrekKode.Kode                           */
/* FIELD ArtikkelNr LIKE StrekKode.ArtikkelNr               */
/* FIELD individnr LIKE Individ.IndividNr                   */
/* FIELD Pris2 as decimal.                                  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-Dobson2OF5) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Dobson2OF5 Procedure 
FUNCTION Dobson2OF5 RETURNS CHARACTER
  ( INPUT ipNr AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EAN13BC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EAN13BC Procedure 
FUNCTION EAN13BC RETURNS CHARACTER
  ( INPUT icStrekKode AS CHAR )  FORWARD.

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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

    
    RUN RapportPDF.
    ASSIGN 
      ocReturn = cFilnavn
      obOk     = TRUE
    .
IF VALID-HANDLE(h_PDFinc) THEN DO:
        DELETE PROCEDURE h_PDFinc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-new_page) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new_page Procedure 
PROCEDURE new_page :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pdf_new_page ("Spdf").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportPDF Procedure 
PROCEDURE RapportPDF PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
Font      Storlek Sidlayout Första TO_Vänsterjusterat Sista_TO Sista_AT
Helvetika      10 Landscape      6                    121      285
               11                6                    110      259
               12                5                    100      237
               10 Portrait       6                     82      192
               11                6                     75      174
               12                5                     68      160
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii            AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cLoadedFont   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iLeftCol      AS INTEGER INIT 5    NO-UNDO.
  DEFINE VARIABLE iRadY         AS DECIMAL EXTENT 22
    INIT [800,800,730,730,660,660,590,590,520,520,450,450,380,380,310,310,240,240,170,170,100,100]  NO-UNDO.
  DEFINE VARIABLE iCol2Plus     AS INTEGER INIT 130  NO-UNDO.
  DEFINE VARIABLE lPlus         AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cDato         AS CHARACTER INIT "2009-04-13"  NO-UNDO.
  DEFINE VARIABLE iPlus         AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iPlustmp      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iNr           AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iYidx         AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iJmfOren      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iJmfKr        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iPrisOren     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iPrisKr       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dJamforPris   AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cJmfenhet     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cSalgsenhet   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dJmfKrWidth   AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dJmfOrenWidth AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dKrWidth      AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE yY            AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE iRadNr        AS INTEGER  INIT 1 NO-UNDO.
  DEFINE VARIABLE cLinkTxt      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dEan          AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cEan          AS CHARACTER   NO-UNDO.
  DEFINE BUFFER linkArtbas FOR artbas.
  cFilNavn = SESSION:TEMP-DIR + "Etikett_11_2" + "_" + STRING(TIME) + ".pdf".

  cDato = STRING(YEAR(TODAY),"9999") + "-" + STRING(MONTH(TODAY),"99") + "-" + STRING(DAY(TODAY),"99").

    /* skapa ett utlägg pr butik */
  RUN pdf_new ("Spdf",cFilNavn).
  /* BARCODE-FONT */
RUN pdf_load_font ("Spdf","Dobson2OF5","pdfinclude\Dobson2OF5.TTF","pdfinclude\Dobson2OF5.AFM","").
/* Load Bar Code Font */
RUN pdf_load_font IN h_PDFinc ("Spdf","Code39",".\PDFinclude\samples\support\code39.ttf",".\PDFinclude\samples\support\code39.afm",""). 
RUN pdf_load_font IN h_PDFinc ("Spdf","EAN13HH",".\PDFinclude\EAN-13HH.ttf",".\PDFinclude\EAN-13HH.afm",""). 
  /* för att pagefooter skall komma automatiskt */
  RUN pdf_set_BottomMargin ("Spdf", 60).
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
    /*   RUN LoadFonts. */
  RUN pdf_set_Orientation ("Spdf","portrait").
/*  RUN pdf_set_Orientation ("Spdf","landscape").*/
  iPageHeight = pdf_PageHeight ("Spdf").
  iPageWidth  = pdf_PageWidth ("Spdf").
/*  MESSAGE "iPageHeight " iPageHeight
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  MESSAGE "iPageWidth " iPageWidth
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
  
  RUN pdf_set_VerticalSpace ("Spdf",8).
  RUN new_page.

  ASSIGN iLeftMargin = pdf_LeftMargin ("Spdf") 
         iRMarginPos =  pdf_PageWidth ("Spdf") - iLeftMargin.

FOR EACH Etikettlogg:
    FIND strekkode WHERE strekkode.kode = Etikettlogg.storl NO-LOCK NO-ERROR.
    IF NOT AVAIL strekkode THEN
    DO:
        MESSAGE "ingenträff streckkod" Etikettlogg.storl
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        NEXT.
    END.
    FIND artbas WHERE artbas.artikkelnr = strekkode.artikkelnr NO-LOCK NO-ERROR.
    IF NOT AVAIL artbas THEN
        NEXT.
    FIND vargr OF artbas NO-LOCK NO-ERROR.
    cLinkTxt = "".
    IF ArtBas.LinkVareNr <> 0 THEN DO:
        FIND linkArtbas WHERE linkArtbas.artikkelnr = ArtBas.LinkVareNr NO-LOCK NO-ERROR.
        IF AVAIL linkArtbas AND TRIM(linkArtbas.Etikettekst1) <> "" THEN
            cLinkTxt = "Exkl. " + linkArtbas.Etikettekst1.
    END.
    dJamforPris = ROUND(Etikettlogg.Pris / ArtBas.Mengde,2).
    IF dJamforPris = ? THEN
        dJamforPris = 0.
    ASSIGN iJmfKr      = TRUNC(dJamforPris,0)
           iJmfOren    = (dJamforPris - iJmfKr) * 100
           iPrisKr     = TRUNC(Etikettlogg.pris,0)
           iPrisOren   = (Etikettlogg.pris - iPrisKr) * 100
           cJmfenhet   = IF TRIM(ArtBas.JamforEnhet) <> "" THEN "Per " + ArtBas.JamforEnhet ELSE "".
           cSalgsenhet = IF TRIM(ArtBas.SalgsEnhet) <> "" THEN "Per " + ArtBas.SalgsEnhet ELSE "".
    DO ii = 1 TO Etikettlogg.ant:
        iNr = iNr + 1.
        IF iNr = 23 THEN DO:
            iNr = 1.
            iRadNr = 1.
            RUN new_page.
        END.
/*         iPlus = IF iNr MOD 2 = 0 THEN 122 ELSE 0. */
        yY = iRadY[iNr].
/*        RUN pdf_set_TextY("Spdf",iRadY[iNr]).*/
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",15).
        IF iNr MOD 2 = 1 THEN
          iPlus = 0.
        ELSE
          iPlus = 300.
        RUN pdf_text_xy_dec ("Spdf",SUBSTRING(ArtBas.Etikettekst1,1,25),7 + iPlus,yY).
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
        RUN pdf_text_xy_dec ("Spdf","JÄMFÖRPRIS",135 + iPlus,yY).
        RUN pdf_text_xy_dec ("Spdf","PRIS",250 + iPlus,yY).

        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",12).
        RUN pdf_text_xy_dec ("Spdf",SUBSTRING(ArtBas.Etikettekst2,1,20),7 + iPlus,yY - 15).
        FIND Produsent WHERE Produsent.ProdNr = ArtBas.ProdNr NO-LOCK NO-ERROR.
        IF AVAIL Produsent THEN
          RUN pdf_text_xy_dec ("Spdf",Produsent.Beskrivelse,50 + iPlus,yY - 15).
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",7).
        RUN pdf_line IN h_PDFinc  ("Spdf", 7 + iPlus ,yY - 20, 130 + iPlus , yY - 20, 0.5).

        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
        IF AVAIL Strekkode THEN
            RUN pdf_text_xy_dec ("Spdf",Strekkode.Bestillingsnummer,7 + iPlus,yY - 35).
        
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",7).
/*        RUN pdf_text_xy_dec ("Spdf",IF AVAIL strekkode THEN strekkode.bestillingsnummer ELSE " ",7 + iPlus,yY - 22).*/
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",7).
        RUN pdf_set_parameter("Spdf","ScaleY",".60").
/*        IF strekkode.bestillingsnummer <> "" THEN DO:
            RUN pdf_set_font IN h_PDFinc ("Spdf","Dobson2OF5",38.0).
            IF iNr MOD 3 = 1 THEN
                RUN pdf_text_xy_dec ("Spdf",Dobson2OF5(Strekkode.kode),7,yY - 43).
            ELSE
                RUN pdf_text_xy_dec ("Spdf",Dobson2OF5(Strekkode.kode),7 + iPlus,yY - 43).
        END. */
        IF strekkode.kode <> "" THEN 
        DO:
            ASSIGN cEan = strekkode.kode.
/*            ASSIGN cEan = STRING(dEan). */
            IF LENGTH(cEan) = 8 OR LENGTH(cEan) = 12 OR LENGTH(cEan) = 13 THEN
            DO:
              RUN pdf_set_font IN h_PDFinc ("Spdf","EAN13HH",30.0).
              IF iNr MOD 2 = 1 THEN
                  RUN pdf_text_xy_dec ("Spdf",EAN13BC(cEan),60,yY - 35).
              ELSE
                  RUN pdf_text_xy_dec ("Spdf",EAN13BC(cEan),60 + iPlus,yY - 35).
            END.
        END. 
        RUN pdf_set_parameter("Spdf","ScaleY","1").



        /* Färgen  */
        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,1.0,1.0).
        /* Jmfpris */
        IF dJamforpris > 0 THEN DO:
            RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",30).
            IF iNr MOD 2 = 1 THEN
                RUN pdf_text_xy_dec ("Spdf","." + STRING(iJmfOren,"99"),170,yY - 25).
            ELSE 
                RUN pdf_text_xy_dec ("Spdf","." + STRING(iJmfOren,"99"),470,yY - 25).
    
            dJmfKrWidth = pdf_text_widthdec ("Spdf",TRIM(STRING(iJmfKr))).
            IF iNr MOD 2 = 1 THEN
              RUN pdf_text_xy_dec ("Spdf",STRING(iJmfKr),170 - dJmfKrWidth,yY - 25).
            ELSE 
              RUN pdf_text_xy_dec ("Spdf",STRING(iJmfKr),470 - dJmfKrWidth,yY - 25).

            IF cJmfenhet <> "" THEN DO:
                RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",7).
                IF iNr MOD 2 = 1 THEN
                  RUN pdf_text_xy_dec ("Spdf",cJmfenhet,155,yY - 35).
                ELSE 
                  RUN pdf_text_xy_dec ("Spdf",cJmfenhet,455,yY - 35).
            END.
        END.

        /* Pris */
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",30).
        IF iNr MOD 2 = 1 THEN
            RUN pdf_text_xy_dec ("Spdf","." + STRING(iPrisOren,"99"),255,yY - 25).
        ELSE 
            RUN pdf_text_xy_dec ("Spdf","." + STRING(iPrisOren,"99"),555,yY - 25).
        dKrWidth = pdf_text_widthdec ("Spdf",TRIM(STRING(iPrisKr))).
        IF iNr MOD 2 = 1 THEN
          RUN pdf_text_xy_dec ("Spdf",STRING(iPrisKr),255 - dKrWidth,yY - 25).
        ELSE 
          RUN pdf_text_xy_dec ("Spdf",STRING(iPrisKr),554 - dKrWidth,yY - 25).
        
        IF clinkTxt <> "" THEN DO:
            RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",5).
            RUN pdf_text_xy_dec ("Spdf",clinkTxt,275 + iPlus - pdf_text_widthdec ("Spdf",TRIM(STRING(clinkTxt))),yY - 53).
        END.
    END.
END.

 RUN pdf_close ("Spdf").
/*  RUN SendEmail IN THIS-PROCEDURE. */
 RUN browse2pdf\viewxmldialog.w (cFilNavn,"Etiketter").
  /* Sender filen til visning og utskrift. */
/*   RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-Dobson2OF5) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Dobson2OF5 Procedure 
FUNCTION Dobson2OF5 RETURNS CHARACTER
  ( INPUT ipNr AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE i2      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cStart  AS CHARACTER INIT "NnNn"  NO-UNDO.
  DEFINE VARIABLE cEnd    AS CHARACTER INIT "WnN"  NO-UNDO.
  DEFINE VARIABLE cMapS   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMaptmp AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE c2      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cLeft   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRight  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMap    AS CHARACTER EXTENT 10 INIT
      ["NNWWN","WNNNW","NWNNW","WWNNN","NNWNW","WNWNN","NWWNN","NNNWW","WNNWN","NWNWN"]  NO-UNDO.
  ii = LENGTH(ipNr).
  IF ii MOD 2 = 1 THEN
      ipNR = "0" + ipNr.
  DO ii = 1 TO LENGTH(ipNr) - 1 BY 2:
      cMapTmp = "".
      c2 = SUBSTR(ipNr,ii,2).
      cLeft  = cMap[INT(SUBSTR(c2,1,1)) + 1].
      cRight = LC(cMap[INT(SUBSTR(c2,2,1)) + 1]).
      DO i2 = 1 TO 5:
          cMapTmp = cMapTmp + SUBSTR(cLeft,i2,1) + SUBSTR(cRight,i2,1).
      END.
      cMapS = cMapS + cMapTmp.
  END.
  RETURN cStart + cMapS + cEnd.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EAN13BC) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EAN13BC Procedure 
FUNCTION EAN13BC RETURNS CHARACTER
  ( INPUT icStrekKode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cBarCode AS CHARACTER EXTENT 14 NO-UNDO.
DEFINE VARIABLE cBCString AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
IF LENGTH(icStrekKode) = 12 THEN 
   icStrekKode = FILL("0",13 - LENGTH(icStrekKode)) + icStrekKode.
/*IF LENGTH(icStrekKode) < 13 THEN
  icStrekKode = FILL("0",13 - LENGTH(icStrekKode)) + icStrekKode.
ELSE IF LENGTH(icStrekKode) > 13 THEN
  RETURN "".*/

ASSIGN cBarCode[1] = CHR(ASC(SUBSTR(icStrekKode,1,1)) - 15) 
       cBarCode[2] = CHR(ASC(SUBSTR(icStrekKode,2,1)) + 48)
       cBarCode[8] = CHR(124).

ASSIGN cBarcode[3] = SUBSTR(icStrekKode,3,1)
       cBarcode[4] = SUBSTR(icStrekKode,4,1)
       cBarcode[5] = SUBSTR(icStrekKode,5,1)
       cBarcode[6] = SUBSTR(icStrekKode,6,1)
       cBarcode[7] = SUBSTR(icStrekKode,7,1).

CASE SUBSTR(icStrekKode,1,1):
    WHEN "1" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "2" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "3" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
    WHEN "4" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "5" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "6" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16).
    WHEN "7" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "8" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
    WHEN "9" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
END CASE.
IF LENGTH(icStrekKode) = 13 THEN 
DO:
  ASSIGN cBarcode[9] = CHR(ASC(SUBSTR(icStrekKode,8,1)) + 32) 
         cBarcode[10] = CHR(ASC(SUBSTR(icStrekKode,9,1)) + 32) 
         cBarcode[11] = CHR(ASC(SUBSTR(icStrekKode,10,1)) + 32) 
         cBarcode[12] = CHR(ASC(SUBSTR(icStrekKode,11,1)) + 32) 
         cBarcode[13] = CHR(ASC(SUBSTR(icStrekKode,12,1)) + 32).
       

  cBarcode[14] = CHR(ASC(SUBSTR(icStrekKode,13,1)) + 64).
  DO iCount = 1 TO 14:
      cBCString = cBCString + cBarCode[iCount].
  END.
END.
ELSE
DO:
  cBarcode[9] = CHR(ASC(SUBSTR(icStrekKode,8,1)) + 64).
  DO iCount = 1 TO 9:
    cBCString = cBCString + cBarCode[iCount].
  END.
END.

IF cBCstring = ? THEN cBCstring = "".

RETURN cBCstring. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

