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
DEFINE INPUT  PARAMETER ipButik  LIKE Butiker.Butik    NO-UNDO.
DEFINE INPUT  PARAMETER ipDato   AS DATE               NO-UNDO.
DEFINE INPUT  PARAMETER lBatch   AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER lDirekte AS LOGICAL    NO-UNDO.

DEFINE VARIABLE cButBatchPrinter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPolygon AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKasseNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFirma   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSprak AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPrinter AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cTittel AS CHARACTER   NO-UNDO.

DEFINE FRAME PageHeader
   HEADER
      "<ALIGN=BASE><FArial><R3><B><C5><P14>Bongrapport kassenr: " cKasseNr " " ipDato "<C65>Sidenr:" PAGE-NUMBER FORMAT ">>9"
      "</B><P10>" SKIP
      "<R4><C5><FROM><R4><C78><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 100.

  DEFINE VARIABLE dColPosRub AS DECIMAL   EXTENT 7  NO-UNDO.
  DEFINE VARIABLE cLabels  AS CHARACTER EXTENT 7 NO-UNDO.
  DEFINE VARIABLE dColPosData AS DECIMAL   EXTENT 7  NO-UNDO.
  DEFINE VARIABLE lRightPos AS LOGICAL  EXTENT 7  NO-UNDO.
  DEFINE VARIABLE dY       AS DECIMAL     NO-UNDO.



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

&IF DEFINED(EXCLUDE-setDY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDY Procedure 
FUNCTION setDY RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-strikeout) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD strikeout Procedure 
FUNCTION strikeout RETURNS LOGICAL
  ( INPUT iLeftstart AS DECIMAL, INPUT iYpos AS DECIMAL )  FORWARD.

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

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
{syspara.i 1 1 101 cPolygon}

cTittel = "Bongrapport".

IF lBatch = TRUE THEN DO:
    FIND Butiker WHERE Butiker.Butik = ipButik NO-LOCK NO-ERROR.
    IF AVAIL Butiker AND TRIM(Butiker.RAPPrinter) <> "" AND CAN-DO(SESSION:GET-PRINTERS(),Butiker.RAPPrinter) THEN
        ASSIGN cButBatchPrinter = TRIM(Butiker.RAPPrinter)
               lDirekte         = TRUE.
    ELSE
        RETURN.
END.
ELSE
    ASSIGN cPrinter = DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER").
IF CAN-FIND(FIRST Bonghode WHERE bonghode.butikknr = ipButik AND 
                                 bonghode.dato     = ipDato  AND
                                 Bonghode.belop    <> 0) THEN DO:
    FIND Butiker WHERE Butiker.Butik = ipButik NO-LOCK NO-ERROR.
    RUN PDFRapport.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFooter Procedure 
PROCEDURE PageFooter :
/*------------------------------------------------------------------------------
  Purpose:  Procedure to Print Page Footer -- on all pages.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cSidTxt AS CHARACTER   NO-UNDO.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_BottomMargin ("Spdf"), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_BottomMargin ("Spdf"), 0.5).

  RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(TODAY)) + " " + STRING(TIME,"HH:MM:SS"),pdf_LeftMargin ("Spdf"),pdf_BottomMargin ("Spdf") - 14).
  cSidTxt = TRIM("Sida: " + STRING(pdf_page("Spdf")) + (IF SESSION:REMOTE = FALSE THEN  " (" + pdf_TotalPages("Spdf") + ")" ELSE "") ).
  RUN pdf_text_xy_dec ("Spdf",cSidTxt,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 50,pdf_BottomMargin ("Spdf") - 14).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageHeader Procedure 
PROCEDURE PageHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iPos AS INTEGER     NO-UNDO.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",14).
  RUN pdf_text_xy_dec ("Spdf",cTittel + "     " + STRING(ipDato),pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 45).

  cTxt = cFirma + (IF CAN-DO("SE,SVE",cSprak) THEN " KASSA: " ELSE " KASSE: ") + cKassenr.
  iPos = dColPosData[7] - bredd(cTxt).
  RUN pdf_text_xy_dec ("Spdf",cTxt,iPos,pdf_PageHeight("Spdf") - 45).

/*   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).                                                                                                        */
/*   RUN pdf_text_xy_dec ("Spdf",cFirma + (IF CAN-DO("SE,SVE",cSprak) THEN " KASSA: " ELSE " KASSE: ") + cKassenr,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 61). */
  
/*   RUN pdf_text_xy_dec ("Spdf",STRING(ipDato),pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 72). */
  
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight ("Spdf") - 61, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight ("Spdf") - 61, 0.5).
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).

  DO ii = 1 TO EXTENT(cLabels):
      RUN pdf_text_xy_dec ("Spdf",cLabels[ii],dColPosRub[ii],pdf_PageHeight("Spdf") - 75).
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  dY = pdf_PageHeight ("Spdf") - 88.

/*   RUN xx. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFrapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFrapport Procedure 
PROCEDURE PDFrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcFilNavn    AS CHAR   NO-UNDO.


  DEF VAR pcLabels    AS CHAR    NO-UNDO.
  DEFINE VARIABLE wOK  AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iRad     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAntKasser AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iOldKasseNr AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iPos AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cOrgPrinter AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE i#pages AS INTEGER     NO-UNDO.
/*   DEFINE VARIABLE cLabel2    AS CHARACTER EXTENT 10  NO-UNDO. */

  ASSIGN
      cFirma      = Butiker.butnamn.

  IF CAN-DO("SE,SVE",cSprak) THEN DO:
      ASSIGN cLabels[1] = "Kvittonr"
             cLabels[2] = "Transtyp"
             cLabels[3] = "EAN"
             cLabels[4] = "Beskrivning"
             cLabels[5] = "Antal"
             cLabels[6] = "Linjsesum"
             cLabels[7] = "Rabatt"
             .
  END.
  ELSE DO:
    ASSIGN cLabels[1] = "Bongnr"
           cLabels[2] = "Transtype"
           cLabels[3] = "EAN"
           cLabels[4] = "Beskrivelse"
           cLabels[5] = "Antall"
           cLabels[6] = "Linjsesum"
           cLabels[7] = "Rabatt"
           .
  END.
  ASSIGN dColPosRub[1] = 40
         dColPosRub[2] = 80
         dColPosRub[3] = 200
         dColPosRub[4] = 290
         dColPosRub[5] = 422
         dColPosRub[6] = 470
         dColPosRub[7] = 530.
  ASSIGN dColPosData[1] = 70
         dColPosData[2] = 80
         dColPosData[3] = 200
         dColPosData[4] = 285
         dColPosData[5] = 445
         dColPosData[6] = 505
         dColPosData[7] = 555.
  ASSIGN lRightPos[1] = TRUE
         lRightPos[2] = FALSE
         lRightPos[3] = FALSE
         lRightPos[4] = FALSE
         lRightPos[5] = TRUE
         lRightPos[6] = TRUE
         lRightPos[7] = TRUE.

 DO :

 STATUS DEFAULT "Skriver ut, venligst vent...".

  /* Henter tempfilnavn */
/*  RUN GetTmpFileName IN hLibHandle ("utk","xpr", OUTPUT pcRappFil). */
   /* Åpner stream til skriverfil. */
  IF lBatch = TRUE THEN
      ASSIGN cPrinter = cButBatchPrinter.
  ELSE
      ASSIGN cPrinter = DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER").

  ASSIGN pcFilNavn = SESSION:TEMP-DIR + "Bongrapport" + "_" + STRING(TIME) + ".pdf".

  RUN pdf_new ("Spdf",pcFilNavn).
  pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PageHeader").
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter").
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 40).
  RUN pdf_set_BottomMargin ("Spdf", 60).
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
  RUN pdf_set_Orientation ("Spdf","portrait").


  FOR EACH Kasse NO-LOCK WHERE Kasse.ButikkNr = ipButik AND
                               Kasse.GruppeNr = 1 AND CAN-FIND(FIRST BongHode WHERE Bonghode.Butik = ipButik AND
                                           BongHode.GruppeNr = 1 AND
                                           BongHode.kassenr = kasse.kassenr AND
                                           BongHode.Dato = ipDato) BREAK BY Kasse.KasseNr:
      IF kasse.kassenr <> iOldkassenr THEN DO:
        /*   RUN ButikRubrik. */
          cTittel     = IF CAN-DO("SE,SVE",cSprak)THEN "Kvittorapport" ELSE "Bongrapport".
        iOldKasseNr = kasse.kassenr.
        ASSIGN cKasseNr = STRING(Kasse.Kassenr).
          RUN pdf_new_page ("Spdf").
/*           RUN PageHeader. */
/*           dY = pdf_PageHeight ("Spdf") - 110. */
      END.
      FOR EACH BongHode NO-LOCK WHERE Bonghode.Butik = ipButik         AND
                              BongHode.GruppeNr = 1            AND
                              BongHode.kassenr = kasse.kassenr AND
                              BongHode.Dato = ipDato BREAK BY BongHode.BongNr:
          FOR EACH Bonglinje NO-LOCK WHERE Bonglinje.Butik    = Bonghode.Butik    AND
                                           Bonglinje.GruppeNr = BongHode.GruppeNr AND
                                           Bonglinje.KasseNr  = BongHode.kassenr  AND
                                           BongLinje.dato     = BongHode.Dato     AND
                                           BongLinje.BongNr   = BongHode.Bongnr BREAK BY BongLinje.BongNr:

              FIND TransType WHERE TransType.TTId = Bonglinje.TTId NO-LOCK NO-ERROR.
              IF FIRST-OF(BongLinje.BongNr) THEN DO:
                  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
                  iPos = dColPosData[1] - (IF lRightPos[1] = TRUE THEN bredd(STRING(Bonghode.BongNr)) ELSE 0).
                  RUN pdf_text_xy_dec ("Spdf",STRING(Bonghode.BongNr),iPos,dY).
                  iPos = dColPosData[2].
                  RUN pdf_text_xy_dec ("Spdf",STRING(Tid,"HH:MM") /* + " " + STRING(Bonglinje.KasseNr) */,iPos,dY).
                  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
                  setDY().
/*                   ASSIGN dY = dY - pdf_VerticalSpace ("Spdf"). */
              END.
              IF CAN-DO("0,95,147",STRING(BongLinje.TTId)) THEN
                  NEXT.

              IF BongLinje.TTId < 13 THEN
                  FIND ArtBas NO-LOCK WHERE
                  ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr) NO-ERROR.
              ELSE
                  IF AVAILABLE ArtBas THEN
                      RELEASE ArtBas.
/*
                  iPos = dColPosData[1] - (IF lRightPos[1] = TRUE THEN bredd(STRING(Bonghode.BongNr)) ELSE 0).
                  RUN pdf_text_xy_dec ("Spdf",STRING(Bonghode.BongNr),iPos,dY).
  */
              IF BongHode.Makulert = 2 OR BongLinje.Makulert THEN DO:
                  cTxt = "MAK".
                  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
                  iPos = dColPosData[1] - (IF lRightPos[1] = TRUE THEN bredd(cTxt) ELSE 0).
                  RUN pdf_text_xy_dec ("Spdf",cTxt,iPos,dY).
                  strikeout(dColPosRub[2],dY + 2).
                  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
              END.
              cTxt = STRING(BongLinje.TTId) + " " + (IF AVAIL TransType THEN TransType.Beskrivelse ELSE "") 
                                            + (IF Bonglinje.TTId = 146 THEN " " + BongLinje.Strekkode + " " + Bonglinje.Bongtekst ELSE "").
              RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosData[2],dY).
              cTxt = (IF BongLinje.TTId < 13 THEN BongLinje.Strekkode ELSE " ").
              RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosData[3],dY).
              cTxt = (IF BongLinje.TTId < 13  THEN BongLinje.BongTekst ELSE " ").
              RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosData[4],dY).

              cTxt = (IF BongLinje.TTId < 13 AND BongLinje.antall <> 0 THEN TRIM(STRING(BongLinje.Antall,"->>9.999")) ELSE " ").
              iPos = dColPosData[5] - (IF lRightPos[5] = TRUE THEN bredd(cTxt) ELSE 0).
              RUN pdf_text_xy_dec ("Spdf",cTxt,iPos,dY).

              cTxt = (IF CAN-DO("92,93,97,145,150",STRING(BongLinje.TTId)) OR BongLinje.Linjesum = 0 THEN " " ELSE STRING(BongLinje.Linjesum,"->>>,>>9.99")).
              iPos = dColPosData[6] - (IF lRightPos[6] = TRUE THEN bredd(cTxt) ELSE 0).
              RUN pdf_text_xy_dec ("Spdf",cTxt,iPos,dY).

              cTxt = (IF CAN-DO("92,93,97,145,150",STRING(BongLinje.TTId)) OR BongLinje.Linjerab = 0 THEN " " ELSE TRIM(STRING(Linjerab,"->>,>>9.99"))).
              iPos = dColPosData[7] - (IF lRightPos[7] = TRUE THEN bredd(cTxt) ELSE 0).

              RUN pdf_text_xy_dec ("Spdf",cTxt,iPos,dY).
              IF AVAIL artbas AND (TRIM(artbas.Levkod) <> "" OR TRIM(Artbas.LevFargKod) <> "") THEN DO:
                  setDY().
/*                   ASSIGN dY = dY - pdf_VerticalSpace ("Spdf"). */
                  IF TRIM(ArtBas.LevKod) <> "" THEN DO:
                      cTxt = TRIM(ArtBas.LevKod).
                      RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosData[3],dY).
                  END.
                  IF TRIM(ArtBas.LevFargKod) <> "" THEN DO:
                      RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosData[4],dY).
                  END.

              END.
              setDY().
/*               IF (BongLinje.TTid < 13 AND AVAILABLE ArtBas) THEN PUT UNFORMATTED SKIP.                                      */
/*               ASSIGN iRad = iRad + 1.                                                                                       */
/*               IF (NOT LAST-OF(Bonglinje.BongNr) AND iRad >= 60) /* OR (LAST-OF(Bonglinje.BongNr) AND iRad > 60) */ THEN DO: */
/*                   PUT UNFORMATTED                                                                                           */
/*                   "<R65><C5><FROM><R65><C78><LINE>" SKIP                                                                    */
/*                   "<R66><C5>" cFirma "<C30>" cPolygon                                                                       */
/*                   "<TRANSPARENT=false><C55><#4><R69><C79><IMAGE#4=icon\skotex_ligg.bmp>" SKIP.                              */
/*                                                                                                                             */
/*                   PAGE.                                                                                                     */
/*                   VIEW FRAME PageHeader.                                                                                    */
/*                   PUT UNFORMATTED pcLabels SKIP "<R6>".                                                                     */
/*                   ASSIGN iRad = 5.                                                                                          */
/*               END.                                                                                                          */
              IF LAST-OF(Bonglinje.BongNr) AND Bonghode.Belop <> 0 THEN DO:
/*                   ASSIGN dY = dY - pdf_VerticalSpace ("Spdf"). */
                  IF BongHode.Makulert = 2 OR BongLinje.Makulert THEN DO:
                      cTxt = "MAK".
                      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
                      iPos = dColPosData[1] - (IF lRightPos[1] = TRUE THEN bredd(cTxt) ELSE 0).
                      RUN pdf_text_xy_dec ("Spdf",cTxt,iPos,dY).
                      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
                  END.
                  cTxt = IF CAN-DO("SE,SVE",cSprak) THEN "Kvittototal"  ELSE "Bongtotal".
                  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
                  RUN pdf_text_xy_dec ("Spdf",cTxt,dColPosData[2],dY).
                  cTxt = TRIM(STRING(BongHode.Belop,"->>>,>>9.99")).
                  iPos = dColPosData[6] - (IF lRightPos[6] = TRUE THEN bredd(cTxt) ELSE 0).
                  RUN pdf_text_xy_dec ("Spdf",cTxt,iPos,dY).
                  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
                  setDY().
              END.
          END.
      END.
  END.
  i#pages = pdf_page ("Spdf").
  
  RUN pdf_close ("Spdf").
/*   IF cEmail <> "" THEN DO:                                */
/*       RUN sendEmail IN THIS-PROCEDURE (cEmail,pcFilnavn). */
/*   END.                                                    */
/*   ELSE                                                    */
  IF lBatch = FALSE AND lDirekte = FALSE THEN
      RUN browse2pdf\viewxmldialog.w (pcFilNavn,"Rapport").
  ELSE IF lBatch = TRUE AND cPrinter <> "" THEN DO:
      IF SEARCH("cmd\PrintPdf.cmd") <> ? THEN DO:
          OS-COMMAND SILENT VALUE(SEARCH("cmd\PrintPdf.cmd") + " " + pcFilnavn + " " + '"' + cPrinter + '"').
      END.
  END.
  ELSE IF lDirekte = TRUE THEN DO:
      cOrgPrinter = SESSION:PRINTER-NAME.
      IF cPrinter = "" THEN DO: /* I mainblocket gör vi return om vi har en 'blank' printer och lBatch = true */
          SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.
          IF NOT wOK THEN DO:
              RETURN.
          END.
          ASSIGN cPrinter = SESSION:PRINTER-NAME.
          SESSION:PRINTER-NAME = cOrgPrinter.
      END.
      IF SEARCH("cmd\PrintPdf.cmd") <> ? THEN DO:
          OS-COMMAND SILENT VALUE(SEARCH("cmd\PrintPdf.cmd") + " " + pcFilnavn + " " + '"' + cPrinter + '"').
      END.
  END.
  /* Sender filen til visning og utskrift. */
/*  RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
/*   RUN VisXprint.p (pcRappFil). */

  STATUS DEFAULT " ".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rapport Procedure 
PROCEDURE Rapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcRappFil    AS CHAR   NO-UNDO.

  DEF VAR pcOverskr    AS CHAR    NO-UNDO.
  DEFINE VARIABLE wOK  AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cPrinter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFirma   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRad     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAntKasser AS INTEGER    NO-UNDO.
  ASSIGN
      cFirma      = Butiker.butnamn
      pcOverskr   = "<C13><B><U>Transtype<C29>EAN<C40>Beskrivelse<C57><RIGHT=C+5>Antall<C64><RIGHT=C+6>Linjesum<C71><RIGHT=C+6>Rabatt</U></B>".

 DO :

 STATUS DEFAULT "Skriver ut, venligst vent...".

  /* Henter tempfilnavn */
/*  RUN GetTmpFileName IN hLibHandle ("utk","xpr", OUTPUT pcRappFil). */
  ASSIGN pcRappFil = SESSION:TEMP-DIR + "Bongrap.xpr".
   /* Åpner stream til skriverfil. */
  IF lBatch = TRUE THEN
      ASSIGN cPrinter = cButBatchPrinter.
  ELSE
      ASSIGN cPrinter = DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER").
  IF cPrinter = "" THEN DO: /* I mainblocket gör vi return om vi har en 'blank' printer och lBatch = true */
      SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.
      IF NOT wOK THEN
          RETURN NO-APPLY.
      ASSIGN cPrinter = SESSION:PRINTER-NAME.
  END.
  OUTPUT TO value(pcRappFil) PAGED page-size 72.
/*   PUT UNFORMATTED "<OLANDSCAPE>". */
  IF NOT lDirekte THEN
      PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
  PUT CONTROL     "<PRINTER" cPrinter ">".
  Put CONTROL "<TRANSPARENT=true><UNITS=MM><LINECOLOR=BLACK><FGCOLOR=BLACK>".
  IF NOT lDirekte THEN
      put control '<PREVIEW=ZoomToWidth>'.

/*   PUT UNFORMATTED   "<ALIGN=BASE><FArial><P10>" SKIP. */
/*   PUT UNFORMATTED "<P12><C20><B>Bongrapport " cFirma "<C+4>" STRING(ipDato) "</B><P10><R+1>" SKIP. */
/*   VIEW FRAME PageHeader.                 */
/*   PUT UNFORMATTED pcOverskr SKIP "<R6>". */

  ASSIGN iRad = 5.
  FOR EACH Kasse NO-LOCK WHERE Kasse.ButikkNr = ipButik AND
                               Kasse.GruppeNr = 1 AND CAN-FIND(FIRST BongHode WHERE Bonghode.Butik = ipButik AND
                                           BongHode.GruppeNr = 1 AND
                                           BongHode.kassenr = kasse.kassenr AND
                                           BongHode.Dato = ipDato) BREAK BY Kasse.KasseNr:
      ASSIGN cKasseNr = STRING(Kasse.Kassenr)
             iAntKasser = iAntKasser + 1.
      IF iAntKasser > 1 THEN DO:
          PUT UNFORMATTED
          "<R65><C5><FROM><R65><C78><LINE>" SKIP
          "<R66><C5>" cFirma "<C30>" cPolygon
          "<TRANSPARENT=false><C55><#4><R69><C79><IMAGE#4=icon\skotex_ligg.bmp>" SKIP.
          PAGE.
      END.
          VIEW FRAME PageHeader.
          PUT UNFORMATTED  "<R5>" pcOverskr SKIP "<R6>".
          ASSIGN iRad = 5.
      FOR EACH BongHode NO-LOCK WHERE Bonghode.Butik = ipButik         AND
                              BongHode.GruppeNr = 1            AND
                              BongHode.kassenr = kasse.kassenr AND
                              BongHode.Dato = ipDato BREAK BY BongHode.BongNr:
          FOR EACH Bonglinje NO-LOCK WHERE Bonglinje.Butik    = Bonghode.Butik    AND
                                           Bonglinje.GruppeNr = BongHode.GruppeNr AND
                                           Bonglinje.KasseNr  = BongHode.kassenr  AND
                                           BongLinje.dato     = BongHode.Dato     AND
                                           BongLinje.BongNr   = BongHode.Bongnr BREAK BY BongLinje.BongNr:

              FIND TransType WHERE TransType.TTId = Bonglinje.TTId NO-LOCK NO-ERROR.
              IF FIRST-OF(BongLinje.BongNr) THEN DO:
                  PUT UNFORMATTED "<C8><B>" STRING(Bonghode.BongNr)
                                  "<C13>" STRING(Tid,"HH:MM")
                                  "<C17>" STRING(Bonglinje.KasseNr) "</B>" /* (IF BongHode.Makulert = 2 THEN "<K>" ELSE "") */ SKIP.
                  ASSIGN iRad = iRad + 1.
              END.
              IF CAN-DO("0,95,147",STRING(BongLinje.TTId)) THEN
                  NEXT.

              IF BongLinje.TTId < 13 THEN
                  FIND ArtBas NO-LOCK WHERE
                  ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr) NO-ERROR.
              ELSE
                  IF AVAILABLE ArtBas THEN
                      RELEASE ArtBas.

              PUT UNFORMATTED "<C13>"  (IF BongHode.Makulert = 2 OR BongLinje.Makulert THEN "<K>" ELSE "")
                                        STRING(BongLinje.TTId) " " (IF AVAIL TransType THEN TransType.Beskrivelse ELSE "") 
                                       (IF Bonglinje.TTId = 146 THEN " " + BongLinje.Strekkode + " " + Bonglinje.Bongtekst ELSE "")
                              "<C29>" (IF BongLinje.TTId < 13 THEN BongLinje.Strekkode ELSE " ")
    /*                                    IF BongLinje.TTId = 146 THEN BongLinje.Strekkode + " " + BongLinje.BongTekst ELSE " ") */
                              "<C40>" (IF BongLinje.TTId < 13  THEN BongLinje.BongTekst ELSE " ")
                              "<C57><RIGHT=C+5>" (IF BongLinje.TTId < 13 AND BongLinje.antall <> 0 THEN STRING(BongLinje.Antall,"->>9.999") ELSE " ")
                              "<C64><RIGHT=C+6>" (IF CAN-DO("92,93,97,145,150",STRING(BongLinje.TTId)) OR BongLinje.Linjesum = 0 THEN " " ELSE STRING(BongLinje.Linjesum,"->>>,>>9.99"))
                              "<C71><RIGHT=C+6>" (IF CAN-DO("92,93,97,145,150",STRING(BongLinje.TTId)) OR BongLinje.Linjerab = 0 THEN "" ELSE STRING(BongLinje.Linjerab,"->>,>>9.99"))
    /*                                          (IF CAN-DO("92,93,97,145,150",STRING(BongLinje.TTId)) OR BongLinje.Kunderab = 0 THEN "" ELSE  "(" + STRING(BongLinje.Kunderab,"->>,>>9.99") + ")") */
                                       (IF BongHode.Makulert = 2 OR BongLinje.Makulert THEN "</K>" ELSE "") 
                              SKIP
                              "<C29>" (IF BongLinje.TTId < 13 AND AVAILABLE ArtBas THEN ArtBas.LevKod ELSE " ")
                              "<C40>" (IF BongLinje.TTId < 13 AND AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE " ")                              
                              .
              IF (BongLinje.TTid < 13 AND AVAILABLE ArtBas) THEN PUT UNFORMATTED SKIP.
              ASSIGN iRad = iRad + 1.
              IF (NOT LAST-OF(Bonglinje.BongNr) AND iRad >= 60) /* OR (LAST-OF(Bonglinje.BongNr) AND iRad > 60) */ THEN DO:
                  PUT UNFORMATTED
                  "<R65><C5><FROM><R65><C78><LINE>" SKIP
                  "<R66><C5>" cFirma "<C30>" cPolygon
                  "<TRANSPARENT=false><C55><#4><R69><C79><IMAGE#4=icon\skotex_ligg.bmp>" SKIP.

                  PAGE.
                  VIEW FRAME PageHeader.
                  PUT UNFORMATTED pcOverskr SKIP "<R6>".
                  ASSIGN iRad = 5.
              END.
              IF LAST-OF(Bonglinje.BongNr) AND Bonghode.Belop <> 0 THEN DO:
                  PUT UNFORMATTED (IF BongHode.Makulert = 2 OR Bonglinje.Makulert THEN "<K>" ELSE "") "<C13><B>"  "Bongtotal"
                                  "<C64><RIGHT=C+6>"  STRING(BongHode.Belop,"->>>,>>>,>>9.99") "</B>" (IF BongHode.Makulert = 2 OR Bonglinje.Makulert THEN "</K>" ELSE "") SKIP.
                  ASSIGN iRad = iRad + 1.
              END.
          END.
      END.
  END.
  PUT UNFORMATTED
  "<R65><C5><FROM><R65><C78><LINE>" SKIP
  "<R66><C5>" cFirma "<C30>" cPolygon
  "<TRANSPARENT=false><C55><#4><R69><C79><IMAGE#4=icon\skotex_ligg.bmp>" SKIP.

  /* Lukker stream */
/*   OUTPUT TO TERMINAL. */
  OUTPUT CLOSE.
  /* Klargjør rapportfilnavnet */
  ASSIGN
      FILE-INFO:File-NAME = pcRappFil
      .
    
  /* Sender filen til visning og utskrift. */
/*  RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
  RUN VisXprint.p (pcRappFil).    

  STATUS DEFAULT " ".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportOrg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportOrg Procedure 
PROCEDURE RapportOrg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcRappFil    AS CHAR   NO-UNDO.

  DEF VAR pcOverskr    AS CHAR    NO-UNDO.
  DEFINE VARIABLE wOK  AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cPrinter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFirma   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iRad     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAntKasser AS INTEGER    NO-UNDO.
  ASSIGN
      cFirma      = Butiker.butnamn
      pcOverskr   = "<C13><B><U>Transtype<C29>EAN<C40>Beskrivelse<C57><RIGHT=C+5>Antall<C64><RIGHT=C+6>Linjesum<C71><RIGHT=C+6>Rabatt</U></B>".

 DO :

 STATUS DEFAULT "Skriver ut, venligst vent...".

  /* Henter tempfilnavn */
/*  RUN GetTmpFileName IN hLibHandle ("utk","xpr", OUTPUT pcRappFil). */
  ASSIGN pcRappFil = SESSION:TEMP-DIR + "Bongrap.xpr".
   /* Åpner stream til skriverfil. */
  IF lBatch = TRUE THEN
      ASSIGN cPrinter = cButBatchPrinter.
  ELSE
      ASSIGN cPrinter = DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER").
  IF cPrinter = "" THEN DO: /* I mainblocket gör vi return om vi har en 'blank' printer och lBatch = true */
      SYSTEM-DIALOG PRINTER-SETUP UPDATE wOK.
      IF NOT wOK THEN
          RETURN NO-APPLY.
      ASSIGN cPrinter = SESSION:PRINTER-NAME.
  END.
  OUTPUT TO value(pcRappFil) PAGED page-size 72.
/*   PUT UNFORMATTED "<OLANDSCAPE>". */
  IF NOT lDirekte THEN
      PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
  PUT CONTROL     "<PRINTER" cPrinter ">".
  Put CONTROL "<TRANSPARENT=true><UNITS=MM><LINECOLOR=BLACK><FGCOLOR=BLACK>".
  IF NOT lDirekte THEN
      put control '<PREVIEW=ZoomToWidth>'.

/*   PUT UNFORMATTED   "<ALIGN=BASE><FArial><P10>" SKIP. */
/*   PUT UNFORMATTED "<P12><C20><B>Bongrapport " cFirma "<C+4>" STRING(ipDato) "</B><P10><R+1>" SKIP. */
/*   VIEW FRAME PageHeader.                 */
/*   PUT UNFORMATTED pcOverskr SKIP "<R6>". */

  ASSIGN iRad = 5.
  FOR EACH Kasse NO-LOCK WHERE Kasse.ButikkNr = ipButik AND
                               Kasse.GruppeNr = 1 AND CAN-FIND(FIRST BongHode WHERE Bonghode.Butik = ipButik AND
                                           BongHode.GruppeNr = 1 AND
                                           BongHode.kassenr = kasse.kassenr AND
                                           BongHode.Dato = ipDato) BREAK BY Kasse.KasseNr:
      ASSIGN cKasseNr = STRING(Kasse.Kassenr)
             iAntKasser = iAntKasser + 1.
      IF iAntKasser > 1 THEN DO:
          PUT UNFORMATTED
          "<R65><C5><FROM><R65><C78><LINE>" SKIP
          "<R66><C5>" cFirma "<C30>" cPolygon
          "<TRANSPARENT=false><C55><#4><R69><C79><IMAGE#4=icon\skotex_ligg.bmp>" SKIP.
          PAGE.
      END.
          VIEW FRAME PageHeader.
          PUT UNFORMATTED  "<R5>" pcOverskr SKIP "<R6>".
          ASSIGN iRad = 5.
      FOR EACH BongHode NO-LOCK WHERE Bonghode.Butik = ipButik         AND
                              BongHode.GruppeNr = 1            AND
                              BongHode.kassenr = kasse.kassenr AND
                              BongHode.Dato = ipDato BREAK BY BongHode.BongNr:
          FOR EACH Bonglinje NO-LOCK WHERE Bonglinje.Butik    = Bonghode.Butik    AND
                                           Bonglinje.GruppeNr = BongHode.GruppeNr AND
                                           Bonglinje.KasseNr  = BongHode.kassenr  AND
                                           BongLinje.dato     = BongHode.Dato     AND
                                           BongLinje.BongNr   = BongHode.Bongnr BREAK BY BongLinje.BongNr:
              IF CAN-DO("95,147",STRING(BongLinje.TTId)) THEN
                  NEXT.
              FIND TransType WHERE TransType.TTId = Bonglinje.TTId NO-LOCK NO-ERROR.
              IF FIRST-OF(BongLinje.BongNr) THEN DO:
                  PUT UNFORMATTED "<C8><B>" STRING(Bonglinje.BongNr)
                                  "<C13>" STRING(Tid,"HH:MM")
                                  "<C17>" STRING(Bonglinje.KasseNr) "</B>" (IF BongHode.Makulert = 2 THEN "<K>" ELSE "") SKIP.
                  ASSIGN iRad = iRad + 1.
              END.
              PUT UNFORMATTED "<C13>"  (IF BongHode.Makulert <> 2 AND BongLinje.Makulert THEN "<K>" ELSE "")
                                        STRING(BongLinje.TTId) " " (IF AVAIL TransType THEN TransType.Beskrivelse ELSE "") 
                                       (IF Bonglinje.TTId = 146 THEN " " + BongLinje.Strekkode + " " + Bonglinje.Bongtekst ELSE "")
                              "<C29>" (IF BongLinje.TTId < 13 THEN BongLinje.Strekkode ELSE " ")
    /*                                    IF BongLinje.TTId = 146 THEN BongLinje.Strekkode + " " + BongLinje.BongTekst ELSE " ") */
                              "<C40>" (IF BongLinje.TTId < 13  THEN BongLinje.BongTekst ELSE " ")
                              "<C57><RIGHT=C+5>" (IF BongLinje.TTId < 13 AND BongLinje.antall <> 0 THEN STRING(BongLinje.Antall,"->>9.999") ELSE " ")
                              "<C64><RIGHT=C+6>" (IF CAN-DO("92,93,97,145,150",STRING(BongLinje.TTId)) OR BongLinje.Linjesum = 0 THEN " " ELSE STRING(BongLinje.Linjesum,"->>>,>>9.99"))
                              "<C71><RIGHT=C+6>" (IF CAN-DO("92,93,97,145,150",STRING(BongLinje.TTId)) OR BongLinje.Linjerab = 0 THEN "" ELSE STRING(BongLinje.Linjerab,"->>,>>9.99"))
    /*                                          (IF CAN-DO("92,93,97,145,150",STRING(BongLinje.TTId)) OR BongLinje.Kunderab = 0 THEN "" ELSE  "(" + STRING(BongLinje.Kunderab,"->>,>>9.99") + ")") */
                                       (IF BongHode.Makulert <> 2 AND BongLinje.Makulert THEN "</K>" ELSE "") SKIP.
              ASSIGN iRad = iRad + 1.
              IF (NOT LAST-OF(Bonglinje.BongNr) AND iRad >= 60) /* OR (LAST-OF(Bonglinje.BongNr) AND iRad > 60) */ THEN DO:
                  PUT UNFORMATTED
                  "<R65><C5><FROM><R65><C78><LINE>" SKIP
                  "<R66><C5>" cFirma "<C30>" cPolygon
                  "<TRANSPARENT=false><C55><#4><R69><C79><IMAGE#4=icon\skotex_ligg.bmp>" SKIP.

                  PAGE.
                  VIEW FRAME PageHeader.
                  PUT UNFORMATTED pcOverskr SKIP "<R6>".
                  ASSIGN iRad = 5.
              END.
              IF LAST-OF(Bonglinje.BongNr) AND Bonghode.Belop <> 0 THEN DO:
                  PUT UNFORMATTED "<C13><B>"  "Bongtotal"
                                  "<C64><RIGHT=C+6>"  STRING(BongHode.Belop,"->>>,>>>,>>9.99") "</B>" (IF BongHode.Makulert = 2 THEN "</K>" ELSE "") SKIP.
                  ASSIGN iRad = iRad + 1.
              END.
          END.
      END.
  END.
  PUT UNFORMATTED
  "<R65><C5><FROM><R65><C78><LINE>" SKIP
  "<R66><C5>" cFirma "<C30>" cPolygon
  "<TRANSPARENT=false><C55><#4><R69><C79><IMAGE#4=icon\skotex_ligg.bmp>" SKIP.

  /* Lukker stream */
/*   OUTPUT TO TERMINAL. */
  OUTPUT CLOSE.
  /* Klargjør rapportfilnavnet */
  ASSIGN
      FILE-INFO:File-NAME = pcRappFil
      .
    
  /* Sender filen til visning og utskrift. */
/*  RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
  RUN VisXprint.p (pcRappFil).    

  STATUS DEFAULT " ".
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

&IF DEFINED(EXCLUDE-setDY) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDY Procedure 
FUNCTION setDY RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF dY - pdf_VerticalSpace ("Spdf") > pdf_BottomMargin ("Spdf") + 3 THEN
        ASSIGN dY = dY - 12.
/*         ASSIGN dY = dY - pdf_VerticalSpace ("Spdf"). */
    ELSE
        RUN pdf_new_page ("Spdf").

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-strikeout) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION strikeout Procedure 
FUNCTION strikeout RETURNS LOGICAL
  ( INPUT iLeftstart AS DECIMAL, INPUT iYpos AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RUN pdf_line IN h_PDFinc  ("Spdf", iLeftstart, iYpos, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , iYpos, 0.5).

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

