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
/* DEFINE VAR ipBuntNr LIKE ovBunt.BuntNr  NO-UNDO. */

DEFINE INPUT  PARAMETER ipBuntNr LIKE ovBunt.BuntNr  NO-UNDO.

DEFINE VARIABLE cButikNavnFra AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE cButikNavnTil AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE cRubrik       AS CHARACTER FORMAT "X(16)" NO-UNDO.
DEFINE VARIABLE cBeskr        AS CHARACTER FORMAT "X(25)" NO-UNDO.

/* DEFINE VARIABLE iColVg        AS INTEGER INIT 6  NO-UNDO. */
/* DEFINE VARIABLE iColLopNr     AS INTEGER INIT 12 NO-UNDO. */
/* DEFINE VARIABLE iColBeskr     AS INTEGER INIT 18 NO-UNDO. */
/* DEFINE VARIABLE iColAntall    AS INTEGER INIT 42 NO-UNDO. */
/* DEFINE VARIABLE iColStorl     AS INTEGER INIT 48 NO-UNDO. */
/* DEFINE VARIABLE iColTilStorl  AS INTEGER INIT 53 NO-UNDO. */
/* DEFINE VARIABLE iColVkost     AS INTEGER INIT 58 NO-UNDO. */
/* DEFINE VARIABLE iColSum       AS INTEGER INIT 67 NO-UNDO. */
/* DEFINE VARIABLE iColMerknad   AS INTEGER INIT 77 NO-UNDO. */


DEFINE VARIABLE iColVg        AS INTEGER INIT 30 NO-UNDO.
DEFINE VARIABLE iRposVg       AS INTEGER INIT 60 NO-UNDO.
DEFINE VARIABLE iColLopNr     AS INTEGER INIT 80 NO-UNDO.
DEFINE VARIABLE iRposLop      AS INTEGER INIT 105  NO-UNDO.
DEFINE VARIABLE iColBeskr     AS INTEGER INIT 130 NO-UNDO.
DEFINE VARIABLE iColAntall    AS INTEGER INIT 310 NO-UNDO.
DEFINE VARIABLE iRposAnt      AS INTEGER INIT 330 NO-UNDO.
DEFINE VARIABLE iColStorl     AS INTEGER INIT 360 NO-UNDO.
DEFINE VARIABLE iRposStr      AS INTEGER INIT 370 NO-UNDO.
DEFINE VARIABLE iColTilStorl  AS INTEGER INIT 390 NO-UNDO.
DEFINE VARIABLE iRposRilStr   AS INTEGER INIT 410    NO-UNDO.
DEFINE VARIABLE iColVkost     AS INTEGER INIT 440 NO-UNDO.
DEFINE VARIABLE iRposVkost    AS INTEGER INIT 480    NO-UNDO.
DEFINE VARIABLE iColSum       AS INTEGER INIT 520 NO-UNDO.
DEFINE VARIABLE iRposSum      AS INTEGER INIT 550 NO-UNDO.
DEFINE VARIABLE iColMerknad   AS INTEGER INIT 570 NO-UNDO.
DEFINE VARIABLE iStrekrad     AS INTEGER  FORMAT ">9"  NO-UNDO.
DEFINE VARIABLE iRad          AS INTEGER    NO-UNDO.
DEFINE VARIABLE cSideRubrikker  AS CHARACTER  INIT "PLUKKLISTE,MOTTAKSLISTE,FØLGESEDDEL,STØRRELSESENDRING,AVDELINGSREGNSKAP" NO-UNDO.
DEFINE VARIABLE cMerknadLbl     AS CHARACTER INIT "Kommentar:" NO-UNDO.
DEFINE VARIABLE cMerknad        AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cButikkInfo     AS CHARACTER  INIT "Fra butikk:,Til butikk:" NO-UNDO.
DEFINE VARIABLE cListeRubrikker AS CHARACTER  INIT "Vg,Løpnr,Varetekst,Antall,Str,Til str,Varekost,Sum,Kommentar" NO-UNDO.
DEFINE VARIABLE cSumLinje       AS CHARACTER  INIT "Sum:" NO-UNDO.
DEFINE VARIABLE cKvittering     AS CHARACTER  INIT "Mottatt:,Butikk:" NO-UNDO.
DEFINE VARIABLE cBeskrRub       AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE cOppdaterttxt   AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE cBuntNr         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iSidnr          AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iFirstRow AS INTEGER  INIT 115   NO-UNDO.
  DEFINE VARIABLE iY        AS INTEGER     NO-UNDO.


DEFINE VARIABLE cAvdelingTxt    AS CHARACTER FORMAT "X(13)" EXTENT 10 INIT ["Avd1","Avd2","Avd3","Avd4","Avd5","Avd6","Avd7","Avd8","Avd9","Sum"] NO-UNDO.
DEFINE FRAME PageHeader
   HEADER
      "<OLANDSCAPE><ALIGN=BASE><FArial><R3><B><C6><P14>" TODAY "<C+3>Buntnr:<C+1>" cBuntNr "<C+10><P24>" cRubrik
      "<P12></B><C110><P10>" PAGE-NUMBER FORMAT ">>" SKIP
      "<R5><C6><B>" ENTRY(1,CButikkInfo) FORMAT "X(15)" "<C15>" cButikNavnFra "<C50><RIGHT=C+20>" cOppdaterttxt "</B>"  SKIP
      "<R6><C6><B>" ENTRY(2,CButikkInfo) FORMAT "X(15)" "<C15>" cButikNavnTil "</B>" SKIP
      "<R7><C6><B>" cMerknadLbl FORMAT "X(15)" "<C15>" cMerknad "</B>" SKIP
      "<R8><C6><FROM><R8><C112><LINE>" SKIP
      "<R" STRING(iStrekrad) "><C" STRING(iColVg,">9")       ">" ENTRY(1,cListeRubrikker)
                              "<C" STRING(iColLopNr,">9")    ">" ENTRY(2,cListeRubrikker)
                              "<C" STRING(iColBeskr,">9")    ">" cBeskrRub /* ENTRY(3,cListeRubrikker) */
                              "<C" STRING(iColAntall,"->9")   ">" ENTRY(4,cListeRubrikker)
                              "<C" STRING(iColStorl,">9")    ">" ENTRY(5,cListeRubrikker)
                              "<C" STRING(iColTilStorl,">9") ">" ENTRY(6,cListeRubrikker)
                              "<C" STRING(iColVkost,"->9")    "><RIGHT=C+8>" ENTRY(7,cListeRubrikker)
                              "<C" STRING(iColSum,"->9")      "><RIGHT=C+9>" ENTRY(8,cListeRubrikker)
                              "<C" STRING(iColMerknad,">9")  ">" ENTRY(9,cListeRubrikker) FORMAT "X(12)"
      "<R9><C6><FROM><R9><C112><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

DEFINE FRAME Kvittering
   HEADER
      "<ALIGN=BASE><FArial>"
      "<R" STRING(iRad) "><C20><B>" ENTRY(1,cKvittering) "</B>" SKIP
      "<R" STRING(iRad + 1) "><C20><FROM><R" STRING(iRad + 1) "><C45><LINE>" SKIP

      "<R" STRING(iRad + 1) "><C20><B>" ENTRY(2,cKvittering) "<C26>" cButikNavnTil "</B>" SKIP
      WITH STREAM-IO WIDTH 255.

DEFINE FRAME SumHeader
   HEADER
      "<OLANDSCAPE><ALIGN=BASE><FArial><R3><B><C6><P14>" TODAY "<C+3>Buntnr:<C+1>" cBuntNr "<C+10><P24>" cRubrik
      "<P12></B><C110><P10>" PAGE-NUMBER FORMAT ">>" SKIP
      "<B><C50><RIGHT=C+20>" cOppdaterttxt "</B>" SKIP
      "<R7><C6><B>" cMerknadLbl FORMAT "X(15)" "<C15>" cMerknad "</B>" SKIP
      "<R8><C6><FROM><R8><C112><LINE>" SKIP
      "<R" STRING(iStrekrad) "><C6>" "Fra/Til"
                              "<C14>" cAvdelingTxt[1]
                              "<C24>" cAvdelingTxt[2]
                              "<C34>" cAvdelingTxt[3]
                              "<C44>" cAvdelingTxt[4]
                              "<C54>" cAvdelingTxt[5]
                              "<C64>" cAvdelingTxt[6]
                              "<C74>" cAvdelingTxt[7]
                              "<C84>" cAvdelingTxt[8]
                              "<C94>" cAvdelingTxt[9]
                              "<C104>" cAvdelingTxt[10] SKIP
      "<R9><C6><FROM><R9><C112><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

DEFINE TEMP-TABLE ttOvAvdSum
    FIELD ButikkNrFra LIKE OvBuffer.ButikkNrFra
    FIELD ButikkNrTil LIKE OvBuffer.ButikkNrTil
    FIELD AvdelingSum AS DECI DECIMALS 2 FORMAT "->>>>,>>9.99" EXTENT 10
    INDEX FraTil ButikkNrFra DESC ButikkNrTil DESC.

DEFINE BUFFER bufovBuffer FOR ovBuffer.

{ pdf_inc.i "THIS-PROCEDURE"}
/* {xPrint.i} */
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
    ( INPUT cText AS CHARACTER ) FORWARD.

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
/* {sww.i} */

FIND ovBunt WHERE ovBunt.BuntNr = ipBuntNr NO-LOCK NO-ERROR.
IF NOT AVAIL ovBunt THEN DO:
    MESSAGE "Överföring saknas"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
ELSE IF NOT CAN-FIND(FIRST ovBuffer OF ovBunt) THEN DO:
    MESSAGE "Inga artiklar finns registrerade på denna överföring."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
/* RUN InitLabels för språkhantering */ 
ASSIGN cBuntNr = STRING(ipBuntNr).
RUN InitLabels.

RUN PDFSkrivRapport.
RETURN RETURN-VALUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InitLabels) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLabels Procedure 
PROCEDURE InitLabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.
  FOR EACH SysPara WHERE SysPara.SysHId = 6 AND SysPara.SysGr = 1101 NO-LOCK:
      CASE SysPara.ParaNr:
          WHEN 1 THEN DO:
              IF SysPara.Parameter1 <> "" THEN DO:
                  DO iCount = 1 TO NUM-ENTRIES(SysPara.Parameter1):
                      ASSIGN ENTRY(iCount,cSideRubrikker) = ENTRY(iCount,SysPara.Parameter1).
                  END.
/*                   IF NUM-ENTRIES(SysPara.Parameter1) = 3 THEN     */
/*                       ASSIGN cSideRubrikker = SysPara.Parameter1. */
              END.
          END.
          WHEN 2 THEN DO:
              IF NUM-ENTRIES(SysPara.Parameter1) = 2 THEN
                  ASSIGN cButikkInfo = SysPara.Parameter1.
          END.
          WHEN 3 THEN DO:
              IF NUM-ENTRIES(SysPara.Parameter1) = 9 THEN
                  ASSIGN cListeRubrikker = SysPara.Parameter1.
          END.
          WHEN 4 THEN DO:
              IF NUM-ENTRIES(SysPara.Parameter1) = 1 THEN
                  ASSIGN cSumLinje = SysPara.Parameter1.
          END.
          WHEN 5 THEN DO:
              IF NUM-ENTRIES(SysPara.Parameter1) = 2 THEN
                  ASSIGN cKvittering = SysPara.Parameter1.
          END.
      END CASE.
      ASSIGN cBeskrRub = ENTRY(3,cListeRubrikker).
      DO iCount = 1 TO 9:
          FIND Avdeling WHERE Avdeling.Avdelingnr = iCount NO-LOCK NO-ERROR.
          IF AVAIL Avdeling THEN
             ASSIGN cAvdelingTxt[iCount] = TRIM(CAPS(SUBSTRING(Avdeling.AvdelingNavn, 1, 1) ) +
                                           LC(SUBSTRING(Avdeling.AvdelingNavn, 2,12) )).
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPageHeader Procedure 
PROCEDURE PDFPageHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
   iSidNr = iSidNr + 1.
   cTxt = STRING(TODAY) + "   Buntnr: " + cBuntNr. 
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",14).
   RUN pdf_text_xy_dec ("Spdf",cTxt, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 35).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",24).
   RUN pdf_text_xy_dec ("Spdf",cRubrik, pdf_LeftMargin ("Spdf") + 280,pdf_PageHeight("Spdf") - 35).
   IF cOppdaterttxt <> "" THEN DO:
       RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",18).
       RUN pdf_text_xy_dec ("Spdf",cOppdaterttxt, pdf_LeftMargin ("Spdf") + 280,pdf_PageHeight("Spdf") - 60).
   END.
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",14).
   RUN pdf_text_xy_dec ("Spdf",STRING(iSidNr), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") - bredd(STRING(iSidNr)) ,pdf_PageHeight("Spdf") - 35).


   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(1,CButikkInfo), pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 55).
   RUN pdf_text_xy_dec ("Spdf",cButikNavnFra, pdf_LeftMargin ("Spdf") + 80,pdf_PageHeight("Spdf") - 55).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(2,CButikkInfo), pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 65).
   RUN pdf_text_xy_dec ("Spdf",cButikNavnTil, pdf_LeftMargin ("Spdf") + 80,pdf_PageHeight("Spdf") - 65).
   RUN pdf_text_xy_dec ("Spdf",cMerknadLbl, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 75).
   RUN pdf_text_xy_dec ("Spdf", cMerknad, pdf_LeftMargin ("Spdf") + 80,pdf_PageHeight("Spdf") - 75).

RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
/*    RUN pdf_text_xy_dec ("Spdf",cTxt, pdf_LeftMargin ("Spdf") + 30,pdf_PageHeight("Spdf") - 35). */
   RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight ("Spdf") - 85, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight ("Spdf") - 85, 0.5).

   RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cListeRubrikker), iColVg      ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cListeRubrikker), iColLopNr   ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(3,cListeRubrikker), iColBeskr   ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(4,cListeRubrikker), iColAntall  ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(5,cListeRubrikker), iColStorl   ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(6,cListeRubrikker), iColTilStorl,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(7,cListeRubrikker), iColVkost   ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(8,cListeRubrikker), iColSum     ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(9,cListeRubrikker), iColMerknad ,pdf_PageHeight("Spdf") - 96).
   
   RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight ("Spdf") - 101, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight ("Spdf") - 101, 0.5).
   
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
  DEF VAR pcRappFil           AS CHAR               NO-UNDO.
  DEFINE VARIABLE dVarekost   LIKE Lager.VVarekost  NO-UNDO.
  DEFINE VARIABLE dSum        LIKE Lager.VVarekost  NO-UNDO.
  DEFINE VARIABLE dTotSum     LIKE Lager.VVarekost  NO-UNDO.
  DEFINE VARIABLE iProfilNr   LIKE Butiker.Profilnr NO-UNDO.
  DEFINE VARIABLE iAvdelingNr AS INTEGER            NO-UNDO.
  DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.

/*   DEFINE VARIABLE iFirstRow AS INTEGER  INIT 115   NO-UNDO. */
/*   DEFINE VARIABLE iY        AS INTEGER     NO-UNDO.         */
  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "ovBunt_" + STRING(ipBuntNr) + ".pdf"
         cOppdaterttxt = IF OvBunt.DatoOppdatert = ? THEN "IKKE OPPDATERT" ELSE "".

  /*   IF VALID-HANDLE(wLibHandle) THEN                                          */
  /*      RUN GetTempFileName IN wLibHandle ("ovBunt", "xpr", OUTPUT pcRappFil). */
  /* Åpner stream til skriverfil. */
  RUN pdf_new ("Spdf",pcRappFil).
/*   pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageHeader"). */
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 30).
  RUN pdf_set_BottomMargin ("Spdf", 40).
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf", 13).
  RUN pdf_set_Orientation ("Spdf", "landscape").

  FOR EACH ovBuffer OF ovBunt NO-LOCK WHERE ovBuffer.ButikkNrFra <> ovBuffer.ButikkNrTil BREAK /* BY ovBuffer.BuntNr */
                                    BY ovBuffer.ButikkNrFra 
                                    BY ovBuffer.ButikkNrTil:
      dTotSum = 0.
      iSidNr = 0.
    IF FIRST-OF(ovBuffer.ButikkNrTil) THEN DO:
        ASSIGN cRubrik = ENTRY(1,cSideRubrikker).
        FIND Butiker WHERE Butiker.Butik = ovBuffer.ButikkNrFra NO-LOCK NO-ERROR.
        ASSIGN cButikNavnFra = IF AVAIL Butiker THEN Butiker.ButNamn ELSE "?????????"
               iProfilNr = IF AVAIL Butiker THEN Butiker.Profilnr ELSE 1.
        FIND Butiker WHERE Butiker.Butik = ovBuffer.ButikkNrTil NO-LOCK NO-ERROR.
        ASSIGN cButikNavnTil = IF AVAIL Butiker THEN Butiker.ButNamn ELSE "?????????".
        FIND ttOvAvdSum WHERE ttOvAvdSum.ButikkNrFra = OvBuffer.ButikkNrFra AND
                              ttOvAvdSum.ButikkNrTil = OvBuffer.ButikkNrTil NO-ERROR.
        IF NOT AVAIL ttOvAvdSum THEN DO:
            CREATE ttOvAvdSum.
            ASSIGN ttOvAvdSum.ButikkNrFra = OvBuffer.ButikkNrFra
                   ttOvAvdSum.ButikkNrTil = OvBuffer.ButikkNrTil.
        END.

        RUN pdf_new_page ("Spdf").
        RUN PDFPageHeader.
        iY = iFirstRow.
        
        RUN PDF_one2three(1).
        
        iY = iFirstRow.
        RUN pdf_new_page ("Spdf").
        ASSIGN iRad      = 10
               iStrekrad = iRad - 2.
        ASSIGN cRubrik = ENTRY(2,cSideRubrikker).
        RUN PDFPageHeader.
        
        RUN PDF_one2three(2).

        iY = iFirstRow.
        RUN pdf_new_page ("Spdf").
        ASSIGN iRad      = 10
               iStrekrad = iRad - 2.
        ASSIGN cRubrik = ENTRY(3,cSideRubrikker).
        RUN PDFPageHeader.
        
        RUN PDF_one2three(3).
    END.
  END.
  IF CAN-FIND(FIRST ovBuffer OF ovBunt WHERE ovBuffer.ButikkNrFra = ovBuffer.ButikkNrTil) THEN DO:
      ASSIGN cRubrik = ENTRY(4,cSideRubrikker).
      iSidNr = 0.
        iY = iFirstRow.
        RUN pdf_new_page ("Spdf").
        RUN PDFPageHeader.
      FOR EACH ovBuffer OF ovBunt NO-LOCK WHERE ovBuffer.ButikkNrFra = ovBuffer.ButikkNrTil BREAK
                                  BY ovBuffer.ButikkNrFra:
          IF FIRST-OF(ovBuffer.ButikkNrFra) THEN DO:
              FIND Butiker WHERE Butiker.Butik = ovBuffer.ButikkNrFra NO-LOCK NO-ERROR.
              ASSIGN cButikNavnFra = IF AVAIL Butiker THEN Butiker.ButNamn ELSE "?????????"
                     cButikNavnTil = cButikNavnFra.
              FOR EACH bufovBuffer OF ovBunt NO-LOCK WHERE bufovBuffer.ButikkNrFra = ovBuffer.ButikkNrFra AND
                                                   bufovBuffer.ButikkNrTil = ovBuffer.ButikkNrTil BY
                                                   bufovBuffer.Vg BY bufovBuffer.LopNr BY bufovBuffer.Storl 
                                                   BY bufovBuffer.TilStorl.
                  FIND ArtBas WHERE Artbas.Vg = bufovBuffer.Vg AND ArtBas.LopNr = bufovBuffer.LopNr NO-LOCK NO-ERROR.
                  IF AVAILABLE ArtBas THEN FIND farg OF ArtBas NO-LOCK NO-ERROR.
                  ASSIGN cBeskr = IF AVAIL ArtBas THEN ArtBas.Beskr ELSE "".
                   ACCUMULATE bufovBuffer.Antall (TOTAL).

               RUN pdf_text_xy_dec ("Spdf",STRING(bufovBuffer.Vg,"zzzzz9"), iRposVg - bredd(STRING(bufovBuffer.Vg,"zzzzz9")) ,pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",STRING(bufovBuffer.LopNr,"zzzzz9") , iRposLop - bredd(STRING(bufovBuffer.LopNr,"zzzzz9"))   ,pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",cBeskr, iColBeskr   ,pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",STRING(bufovBuffer.Antall,"-zz9"), iRposAnt - bredd(STRING(bufovBuffer.Antall,"-zz9"))  ,pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",bufovBuffer.Storl, iRposStr - bredd(bufovBuffer.Storl)  ,pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",bufovBuffer.TilStorl, iRposRilStr - bredd(bufovBuffer.TilStorl),pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",(IF AVAILABLE FArg THEN Farg.FarBeskr + " " ELSE "") + bufovBuffer.Merknad, iColMerknad ,pdf_PageHeight("Spdf") - iY).
                iY = iY + 15.
                IF pdf_PageHeight("Spdf") - iY - 30 <= 0 THEN DO:
                    iY = iFirstRow.
                 RUN pdf_new_page ("Spdf").
                 RUN PDFPageHeader.
                END.
              END.
              RUN pdf_line IN h_PDFinc  ("Spdf", iColAntall - bredd(cSumLinje),pdf_PageHeight("Spdf") - iY, iRposAnt , pdf_PageHeight("Spdf") - iY, 0.5).
              iY = iY + 15.
              RUN pdf_text_xy_dec ("Spdf",cSumLinje, iColAntall - bredd(cSumLinje) ,pdf_PageHeight("Spdf") - iY).
              RUN pdf_text_xy_dec ("Spdf",STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9"), iRposAnt - bredd(STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9"))  ,pdf_PageHeight("Spdf") - iY).
          END.
      END.
  END.
  /*
  IF CAN-FIND(FIRST ttOvAvdSum) THEN DO:
      OUTPUT TO VALUE(pcRappFil) APPEND PAGED PAGE-SIZE VALUE(48). /* iPageSize */
      ASSIGN cRubrik = ENTRY(5,cSideRubrikker).
      PAGE.
      VIEW FRAME SumHeader.

      FOR EACH ttOvAvdSum BY ttOvAvdSum.ButikkNrFra BY ttOvAvdSum.ButikkNrTil.
          DO iCount = 1 TO 9:
              ASSIGN ttOvAvdSum.AvdelingSum[10] = ttOvAvdSum.AvdelingSum[10] + 
                                                  ttOvAvdSum.AvdelingSum[iCount].
          END.
          PUT UNFORMATTED "<B><R+1>" 
            "<C6>"  ttOvAvdSum.ButikkNrFra "/"
             "<C9>" ttOvAvdSum.ButikkNrTil
             "<C13><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[1] FORMAT "->>>>,>>9.99"
             "<C23><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[2] FORMAT "->>>>,>>9.99"
             "<C33><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[3] FORMAT "->>>>,>>9.99"
             "<C43><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[4] FORMAT "->>>>,>>9.99"
             "<C53><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[5] FORMAT "->>>>,>>9.99"
             "<C63><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[6] FORMAT "->>>>,>>9.99"
             "<C73><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[7] FORMAT "->>>>,>>9.99"
             "<C83><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[8] FORMAT "->>>>,>>9.99"
             "<C93><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[9] FORMAT "->>>>,>>9.99"
             "<C103><RIGHT=C+9>" ttOvAvdSum.AvdelingSum[10] FORMAT "->>>>>,>>9.99" SKIP.

       END.
       OUTPUT CLOSE.
  END.
  */
  RUN pdf_close ("Spdf").

  RUN browse2pdf\viewxmldialog.w (pcRappFil,"VAR HERPolygon Retail Solutions").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-PDF_one2three) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDF_one2three Procedure 
PROCEDURE PDF_one2three :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iTyp AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dVarekost   LIKE Lager.VVarekost  NO-UNDO.
  DEFINE VARIABLE dSum        LIKE Lager.VVarekost  NO-UNDO.
  DEFINE VARIABLE dTotSum     LIKE Lager.VVarekost  NO-UNDO.
  DEFINE VARIABLE iProfilNr   LIKE Butiker.Profilnr NO-UNDO.
  DEFINE VARIABLE iAvdelingNr AS INTEGER            NO-UNDO.
  DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
        FOR EACH bufovBuffer OF ovBunt NO-LOCK WHERE bufovBuffer.ButikkNrFra = ovBuffer.ButikkNrFra AND
                                             bufovBuffer.ButikkNrTil = ovBuffer.ButikkNrTil BY
                                             bufovBuffer.Vg BY bufovBuffer.LopNr BY bufovBuffer.Storl.
            FIND ArtBas WHERE Artbas.Vg = bufovBuffer.Vg AND ArtBas.LopNr = bufovBuffer.LopNr NO-LOCK NO-ERROR.
            IF AVAIL ArtBas THEN DO:
                FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.
                ASSIGN iAvdelingNr = IF AVAIL HuvGr AND HuvGr.AvdelingNr > 0 AND HuvGr.AvdelingNr < 9 THEN HuvGr.AvdelingNr ELSE 9.
               FIND Farg OF ArtBas NO-LOCK NO-ERROR.
            END.

            ASSIGN dVarekost = bufovBuffer.Varekost
                   dSum      = bufovBuffer.Antall * dVarekost
                   dTotSum   = dTotSum + dSum
                   cBeskr    = IF AVAIL ArtBas THEN SUBSTR(ArtBas.Beskr,1,30) ELSE "".
               ACCUMULATE bufovBuffer.Antall (TOTAL).
               
               RUN pdf_text_xy_dec ("Spdf",STRING(bufovBuffer.Vg,"zzzzz9"), iRposVg - bredd(STRING(bufovBuffer.Vg,"zzzzz9")) ,pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",STRING(bufovBuffer.LopNr,"zzzzz9") , iRposLop - bredd(STRING(bufovBuffer.LopNr,"zzzzz9"))   ,pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",cBeskr, iColBeskr   ,pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",STRING(bufovBuffer.Antall,"-zz9"), iRposAnt - bredd(STRING(bufovBuffer.Antall,"-zz9"))  ,pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",bufovBuffer.Storl, iRposStr - bredd(bufovBuffer.Storl)  ,pdf_PageHeight("Spdf") - iY).
               IF TRIM(bufovBuffer.Storl) <> TRIM(bufovBuffer.Storl) THEN
                   RUN pdf_text_xy_dec ("Spdf",bufovBuffer.TilStorl, iRposRilStr - bredd(bufovBuffer.TilStorl),pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",STRING(dVarekost,">>,>>9.99"), iRposVkost - bredd(STRING(dVarekost,">>,>>9.99"))   ,pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",STRING(dSum,"->>>,>>9.99"), iRposSum - bredd(STRING(dSum,"->>>,>>9.99"))     ,pdf_PageHeight("Spdf") - iY).
               RUN pdf_text_xy_dec ("Spdf",(IF AVAILABLE FArg THEN Farg.FarBeskr + " " ELSE "") + bufovBuffer.Merknad, iColMerknad ,pdf_PageHeight("Spdf") - iY).

                    ASSIGN iRad = iRad + 1.
             ASSIGN ttOvAvdSum.AvdelingSum[iAvdelingNr] = ttOvAvdSum.AvdelingSum[iAvdelingNr] + dSum.
               iY = iY + 15.
               IF pdf_PageHeight("Spdf") - iY - 30 <= 0 THEN DO:
                   iY = iFirstRow.
                RUN pdf_new_page ("Spdf").
                ASSIGN iRad      = 10
                       iStrekrad = iRad - 2.
                RUN PDFPageHeader.
               END.
        END.
        IF iTyp <> 3 THEN DO:
            RUN pdf_line IN h_PDFinc  ("Spdf", iColAntall - bredd(cSumLinje),pdf_PageHeight("Spdf") - iY, iRposSum , pdf_PageHeight("Spdf") - iY, 0.5).
            iY = iY + 15.
            RUN pdf_text_xy_dec ("Spdf",cSumLinje, iColAntall - bredd(cSumLinje) ,pdf_PageHeight("Spdf") - iY).
            RUN pdf_text_xy_dec ("Spdf",STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9"), iRposAnt - bredd(STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9"))  ,pdf_PageHeight("Spdf") - iY).
            RUN pdf_text_xy_dec ("Spdf",STRING(dTotSum,"->>>,>>9.99"), iRposSum - bredd(STRING(dTotSum,"->>>,>>9.99"))     ,pdf_PageHeight("Spdf") - iY).
        END.
        ELSE DO:
            RUN pdf_line IN h_PDFinc  ("Spdf", iColAntall - bredd(cSumLinje),pdf_PageHeight("Spdf") - iY, iRposAnt , pdf_PageHeight("Spdf") - iY, 0.5).
            iY = iY + 15.
            RUN pdf_text_xy_dec ("Spdf",cSumLinje, iColAntall - bredd(cSumLinje) ,pdf_PageHeight("Spdf") - iY).
            RUN pdf_text_xy_dec ("Spdf",STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9"), iRposAnt - bredd(STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9"))  ,pdf_PageHeight("Spdf") - iY).
            iY = iY + 20.

            IF pdf_PageHeight("Spdf") - iY - 30 <= 0 THEN DO:
                 iY = iFirstRow + 10.
               RUN pdf_new_page ("Spdf").
                ASSIGN iRad      = 10
                       iStrekrad = iRad - 2.
                RUN PDFPageHeader.
            END.
            RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",14).
            RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cKvittering), 100 ,pdf_PageHeight("Spdf") - iY).
            RUN pdf_line IN h_PDFinc  ("Spdf", 100,pdf_PageHeight("Spdf") - iY - 2, 350 , pdf_PageHeight("Spdf") - iY - 2, 0.5).
            RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cKvittering) + " " + cButikNavnTil, 100 ,pdf_PageHeight("Spdf") - iY - 15).
        END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER ):
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

    RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

