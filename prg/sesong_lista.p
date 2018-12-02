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
/*          This .p file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cPer            AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE cPer2           AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE cDatum          AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE cKundenavn      AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE cPolygon        AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE dLagerv         AS DECIMAL  FORMAT "-zzz,zzz,zz9.99" NO-UNDO.
DEFINE VARIABLE dFsgv           AS DECIMAL  FORMAT "-zzz,zzz,zz9.99" NO-UNDO.
DEFINE VARIABLE dY              AS DECIMAL                           NO-UNDO.
DEFINE VARIABLE iCount          AS INTEGER                           NO-UNDO.
DEFINE VARIABLE cUL             AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE iLen            AS INTEGER                           NO-UNDO.
DEFINE VARIABLE cTmpData        AS CHARACTER EXTENT 15               NO-UNDO.
DEFINE VARIABLE cTxt            AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE cStrl           AS CHARACTER                         NO-UNDO. 
DEFINE VARIABLE cStrl2          AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE pcRappFil       AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE cVisEntry       AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE cWrk            AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE cToday          AS CHARACTER FORMAT "X(10)"          NO-UNDO.
DEFINE VARIABLE cSidTxt         AS CHARACTER                         NO-UNDO.
DEFINE VARIABLE iInd            AS INTEGER                           NO-UNDO.
DEFINE VARIABLE iVgind          AS INTEGER EXTENT 3                  NO-UNDO.
DEFINE VARIABLE cMailEnablat AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDF AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE TT_Sasong1
    FIELD StDatum     LIKE SaSong.StartDato
    FIELD Sasong      LIKE SaSong.Sasong
    FIELD SasongBeskr LIKE SaSong.SasBeskr
    FIELD Antal AS INTEGER
    FIELD Fsgv  AS DECIMAL FORMAT "-zzz,zzz,zz9.99"
    FIELD Lagerv AS DECIMAL FORMAT "-zzz,zzz,zz9.99"
    INDEX ix Stdatum DESC.
DEFINE TEMP-TABLE TT_Sasong2
    FIELD StDatum     LIKE SaSong.StartDato
    FIELD Sasong      LIKE SaSong.Sasong
    FIELD SasongBeskr LIKE SaSong.SasBeskr
    FIELD Antal AS INTEGER
    FIELD Fsgv  AS DECIMAL FORMAT "-zzz,zzz,zz9.99"
    FIELD Lagerv AS DECIMAL FORMAT "-zzz,zzz,zz9.99"
    INDEX ix Stdatum DESC.
DEFINE TEMP-TABLE TT_Sasong3
    FIELD StDatum     LIKE SaSong.StartDato
    FIELD Sasong      LIKE SaSong.Sasong
    FIELD SasongBeskr LIKE SaSong.SasBeskr
    FIELD Antal AS INTEGER
    FIELD Fsgv  AS DECIMAL FORMAT "-zzz,zzz,zz9.99"
    FIELD Lagerv AS DECIMAL FORMAT "-zzz,zzz,zz9.99"
    INDEX ix Stdatum DESC.

{ pdf_inc.i "THIS-PROCEDURE"}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHARACTER )  FORWARD.

/*{runlib.i}*/

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
{syspara.i 1 1 100 cKundenavn}
{syspara.i 1 1 101 cPolygon}
{syspara.i 210 258 1 cWrk}
{syspar2.i 50 50 25 cMailEnablat}
cWrk = "".
IF cWrk = "" THEN
    ASSIGN iVgInd[1] = 50
           iVgInd[2] = 98
           iVgInd[3] = 99.
ELSE
  DO iInd = 1 TO 3:
     ASSIGN iVgInd[iInd] = INTEGER(ENTRY(iInd,cWrk,",")).
  END.

/* cDF = SESSION:DATE-FORMAT.   */
/* SESSION:DATE-FORMAT = "ymd". */
ASSIGN cToday = STRING(YEAR(TODAY),"9999") + "-"
              + STRING(MONTH(TODAY),"99") + "-"
              + STRING(DAY(TODAY),"99").
RUN GetStartDatum.

RUN LaddaTabell1.

RUN LaddaTabell2.

RUN LaddaTabell3.
/* SESSION:DATE-FORMAT = cDF. */
/*FOR EACH TT_Sasong NO-LOCK:
    DISPLAY TT_Sasong.SasongBeskr TT_Sasong.Antal TT_Sasong.Lagerv.
END.*/
RUN PDFSasongLista.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GetStartDatum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetStartDatum Procedure 
PROCEDURE GetStartDatum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH SaSong EXCLUSIVE-LOCK:
/*    IF STRING(SaSong.StartDato) <> "" THEN
        NEXT.*/
    IF SaSong.Sasong = 0 THEN 
    DO:
      ASSIGN cDatum = "20010101".
      ASSIGN SaSong.StartDato = DATE(cDatum).
      NEXT.
    END.
    ASSIGN cPer = SUBSTRING(SaSong.SasBeskr,1,1).
    ASSIGN cPer2 = SUBSTRING(SaSong.SasBeskr,2,2).
    IF cPer = "V" THEN
      ASSIGN cDatum = "20" + cPer2 + "0101".
    ELSE
      ASSIGN cDatum = "20" + cPer2 + "0701".
/*    DISPLAY SaSong.Sasong SaSong.SasBeskr SaSong.StartDato cPer cPer2.*/
    ASSIGN SaSong.StartDato = DATE(cDatum).
END.
RELEASE SaSong.

FOR EACH SaSong NO-LOCK:
/*    IF SaSong.Sasong = 0 THEN
        NEXT.*/
    CREATE TT_Sasong1.
    ASSIGN TT_Sasong1.StDatum = SaSong.StartDato
           TT_Sasong1.Sasong     = SaSong.Sasong
           TT_Sasong1.SasongBeskr = SasBeskr.
    CREATE TT_Sasong2.
    ASSIGN TT_Sasong2.StDatum = SaSong.StartDato
           TT_Sasong2.Sasong     = SaSong.Sasong
           TT_Sasong2.SasongBeskr = SasBeskr.
    CREATE TT_Sasong3.
    ASSIGN TT_Sasong3.StDatum = SaSong.StartDato
           TT_Sasong3.Sasong     = SaSong.Sasong
           TT_Sasong3.SasongBeskr = SasBeskr.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LaddaTabell1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LaddaTabell1 Procedure 
PROCEDURE LaddaTabell1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH TT_Sasong1 EXCLUSIVE-LOCK:
  FOR EACH ArtBas WHERE ArtBas.SaSong = TT_Sasong1.SaSong NO-LOCK:
      IF ArtBas.Vg = iVgInd[1] OR ArtBas.Vg = iVgInd[2] OR ArtBas.Vg = iVgInd[3] THEN
        NEXT.
      FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
      FOR EACH Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND Lager.lagant <> 0 NO-LOCK:
        ASSIGN TT_Sasong1.Antal  = TT_Sasong1.Antal + Lager.LagAnt.
        ASSIGN dFsgv             = 0.
        IF AVAIL artpris THEN
            ASSIGN dFsgv = Lager.LagAnt * (IF NOT artpris.tilbud THEN artpris.pris[1] ELSE artpris.pris[2]).
        ASSIGN TT_Sasong1.Fsgv = TT_Sasong1.Fsgv + dFsgv.
        ASSIGN dLagerv = Lager.LagAnt * Lager.VVarekost.
        ASSIGN TT_Sasong1.Lagerv = TT_Sasong1.Lagerv + dLagerv.
      END.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LaddaTabell2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LaddaTabell2 Procedure 
PROCEDURE LaddaTabell2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH TT_Sasong2 EXCLUSIVE-LOCK:
  FOR EACH ArtBas WHERE ArtBas.SaSong = TT_Sasong2.SaSong NO-LOCK:
      IF ArtBas.Vg <> iVgInd[2] THEN
        NEXT.
      FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
      FOR EACH Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND Lager.lagant <> 0 NO-LOCK:
        ASSIGN TT_Sasong2.Antal  = TT_Sasong2.Antal + Lager.LagAnt.
        ASSIGN dFsgv             = 0.
        IF AVAIL artpris THEN
            ASSIGN dFsgv = Lager.LagAnt * (IF NOT artpris.tilbud THEN artpris.pris[1] ELSE artpris.pris[2]).
        ASSIGN TT_Sasong2.Fsgv = TT_Sasong2.Fsgv + dFsgv.
        ASSIGN dLagerv = Lager.LagAnt * Lager.VVarekost.
        ASSIGN TT_Sasong2.Lagerv = TT_Sasong2.Lagerv + dLagerv.
      END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LaddaTabell3) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LaddaTabell3 Procedure 
PROCEDURE LaddaTabell3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH TT_Sasong3 EXCLUSIVE-LOCK:
  FOR EACH ArtBas WHERE ArtBas.SaSong = TT_Sasong3.SaSong NO-LOCK:
      IF ArtBas.Vg <> iVgInd[3] THEN
        NEXT.
      FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
      FOR EACH Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND Lager.lagant <> 0 NO-LOCK:
        ASSIGN TT_Sasong3.Antal  = TT_Sasong3.Antal + Lager.LagAnt.
        ASSIGN dFsgv             = 0.
        IF AVAIL artpris THEN
            ASSIGN dFsgv = Lager.LagAnt * (IF NOT artpris.tilbud THEN artpris.pris[1] ELSE artpris.pris[2]).
        ASSIGN TT_Sasong3.Fsgv = TT_Sasong3.Fsgv + dFsgv.
        ASSIGN dLagerv = Lager.LagAnt * Lager.VVarekost.
        ASSIGN TT_Sasong3.Lagerv = TT_Sasong3.Lagerv + dLagerv.
      END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFNysida) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFNysida Procedure 
PROCEDURE PDFNysida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER doRad AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iLM AS INTEGER NO-UNDO.
DEFINE VARIABLE        iLM2 AS INTEGER     NO-UNDO.
  RUN pdf_new_page ("Spdf").
  ASSIGN doRad = pdf_PageHeight ("Spdf") - 35.
  RUN pdf_stroke_fill ("Spdf",.85,.85,.85).  
  RUN pdf_rect ("Spdf", iLM - 5, doRad - 3, iLM + 510, 15,0.5).      /* Left,Bottom,Length,Height*/
  RUN pdf_stroke_fill ("Spdf",1.0,1.0,1.0).
  RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_xy_dec ("Spdf",cToday,iLM,doRad).
  RUN pdf_text_xy_dec ("Spdf","SäsongsLista",iLM + 200,doRad).
  ASSIGN iLM2 = (iLM + 530) - bredd(cKundenavn).
  RUN pdf_text_xy_dec ("Spdf",cKundenavn,iLM2,doRad).
  ASSIGN doRad = doRad - 20.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPageFooter Procedure 
PROCEDURE PDFPageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pdf_set_font ("Spdf", "Helvetica",8).
/*  RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).*/
  RUN pdf_line ("Spdf", pdf_LeftMargin ("Spdf"), pdf_BottomMargin ("Spdf"), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_BottomMargin ("Spdf"), 0.5).

  RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).  
  RUN pdf_text_xy_dec ("Spdf",cKundeNavn,pdf_LeftMargin ("Spdf"),pdf_BottomMargin ("Spdf") - 14).
  RUN pdf_text_xy_dec ("Spdf",cPolygon,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 350,pdf_BottomMargin ("Spdf") - 14).
  cSidTxt = TRIM("Sida: " + STRING(pdf_page("Spdf")) + " (" + pdf_TotalPages("Spdf") + ")").
/*  ASSIGN iTestSnr = pdf_page("Spdf").*/
  RUN pdf_text_xy_dec ("Spdf",cSidTxt,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 50,pdf_BottomMargin ("Spdf") - 14).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFSasongLista) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSasongLista Procedure 
PROCEDURE PDFSasongLista :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iLM          AS INTEGER    NO-UNDO.
  DEFINE VARIABLE doRad        AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iAntal AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dSumFsg AS DECIMAL  FORMAT "-zzz,zzz,zz9.99"   NO-UNDO.
  DEFINE VARIABLE dSumValue AS DECIMAL  FORMAT "-zzz,zzz,zz9.99"   NO-UNDO.

  ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "SasongsLista_" + cToday + ".pdf".

  RUN pdf_new ("Spdf",pcRappFil).
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 30).
  RUN pdf_set_BottomMargin ("Spdf", 40).
  RUN pdf_set_VerticalSpace ("Spdf", 13).
/*  RUN pdf_set_Orientation ("Spdf", "landscape").*/
  RUN pdf_set_Orientation ("Spdf","Portrait").
  ASSIGN iLM = pdf_LeftMargin ("Spdf").

  RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
/*  ASSIGN doRad = dOrad - 15.*/
  RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_xy_dec ("Spdf","Säsong",iLM + 20,doRad).
  RUN pdf_text_xy_dec ("Spdf","Antal",iLM + 150 - bredd("Antal"),doRad).
  RUN pdf_text_xy_dec ("Spdf","Alla Vgr utom " + STRING(iVgInd[1]) + "," + STRING(iVgInd[2]) + "," + STRING(iVgInd[3]),iLM + 170,doRad).
  RUN pdf_text_xy_dec ("Spdf","Pris",iLM + 330 - bredd("Pris"),doRad).
  RUN pdf_text_xy_dec ("Spdf","Fsgvärde",iLM + 430 - bredd("Fsgvärde"),doRad).
  RUN pdf_text_xy_dec ("Spdf","Lagervärde",iLM + 530 - bredd("Lagervärde"),doRad).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  ASSIGN doRad = doRad - 5.
  RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , doRad, 0.5).
  RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).

  iAntal    = 0.
  dSumFsg   = 0.
  dSumValue = 0.
  FOR EACH TT_Sasong1 WHERE TT_Sasong1.Antal <> 0 NO-LOCK:
    ASSIGN doRad = doRad - 13.
    IF doRad < 70 THEN
    DO:
      RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
      RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
      RUN pdf_text_xy_dec ("Spdf","Säsong",iLM + 20,doRad).
      RUN pdf_text_xy_dec ("Spdf","Antal",iLM + 150 - bredd("Antal"),doRad).
      RUN pdf_text_xy_dec ("Spdf","Alla Vgr utom " + STRING(iVgInd[1]) + "," + STRING(iVgInd[2]) + "," + STRING(iVgInd[3]),iLM + 170,doRad).
      RUN pdf_text_xy_dec ("Spdf","Pris",iLM + 330 - bredd("Pris"),doRad).
      RUN pdf_text_xy_dec ("Spdf","Fsgvärde",iLM + 430 - bredd("Fsgvärde"),doRad).
      RUN pdf_text_xy_dec ("Spdf","Lagervärde",iLM + 530 - bredd("Lagervärde"),doRad).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
      ASSIGN doRad = doRad - 5.
      RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , doRad, 0.5).
      RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
      ASSIGN doRad = doRad - 13.
    END.
    ASSIGN iAntal    = iAntal    + TT_Sasong1.Antal
           dSumFsg   = dSumFsg   + TT_Sasong1.Fsgv
           dSumValue = dSumValue + TT_Sasong1.Lagerv.
    RUN pdf_text_xy_dec ("Spdf",TT_Sasong1.SasongBeskr + " / " + STRING(TT_Sasong1.StDatum),iLM + 20,doRad).
    ASSIGN cWrk = TRIM(STRING(TT_Sasong1.Antal)).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 150 - bredd(cWrk),doRad).
    
    ASSIGN cWrk = TRIM(STRING(TT_Sasong1.Fsgv / TT_Sasong1.Antal,"-zz,zz9.99")).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 330 - bredd(cWrk),doRad).
    
    ASSIGN cWrk = TRIM(STRING(TT_Sasong1.Fsgv,"-zzz,zzz,zz9.99")).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 430 - bredd(cWrk),doRad).
    
    ASSIGN cWrk = TRIM(STRING(TT_Sasong1.Fsgv,"-zzz,zzz,zz9.99")).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 430 - bredd(cWrk),doRad).
    ASSIGN cWrk = TRIM(STRING(TT_Sasong1.Lagerv,"-zzz,zzz,zz9.99")).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 530 - bredd(cWrk),doRad).
  END.
  IF iAntal <> 0 THEN DO:
      ASSIGN doRad = dOrad - 15.
      RUN pdf_text_xy_dec ("Spdf","SUMMA",iLM + 20,doRad).
      ASSIGN cWrk = TRIM(STRING(iAntal)).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 150 - bredd(cWrk),doRad).
      ASSIGN cWrk = TRIM(STRING(dSumFsg,"-zzz,zzz,zz9.99")).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 430 - bredd(cWrk),doRad).
      ASSIGN cWrk = TRIM(STRING(dSumValue,"-zzz,zzz,zz9.99")).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 530 - bredd(cWrk),doRad).
      ASSIGN doRad = dOrad - 10.
  END.
  ASSIGN doRad = dOrad - 13.
  IF doRad < 70 THEN
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
  RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_xy_dec ("Spdf","Säsong",iLM + 20,doRad).
  RUN pdf_text_xy_dec ("Spdf","Antal",iLM + 150 - bredd("Antal"),doRad).
  RUN pdf_text_xy_dec ("Spdf","Vgr: " + STRING(iVgInd[2]),iLM + 170,doRad).
  RUN pdf_text_xy_dec ("Spdf","Pris",iLM + 330 - bredd("Pris"),doRad).
  RUN pdf_text_xy_dec ("Spdf","Fsgvärde",iLM + 430 - bredd("Fsgvärde"),doRad).
  RUN pdf_text_xy_dec ("Spdf","Lagervärde",iLM + 530 - bredd("Lagervärde"),doRad).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  ASSIGN doRad = doRad - 5.
  RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , doRad, 0.5).
  RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).

  iAntal    = 0.
  dSumFsg   = 0.
  dSumValue = 0.
  FOR EACH TT_Sasong2 WHERE TT_Sasong2.Antal <> 0 NO-LOCK:
    ASSIGN doRad = dOrad - 13.
    ASSIGN iAntal    = iAntal    + TT_Sasong2.Antal
           dSumFsg   = dSumFsg   + TT_Sasong2.Fsgv
           dSumValue = dSumValue + TT_Sasong2.Lagerv.
    IF doRad < 70 THEN
    DO:
      RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
      RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
      RUN pdf_text_xy_dec ("Spdf","Säsong",iLM + 20,doRad).
      RUN pdf_text_xy_dec ("Spdf","Antal",iLM + 150 - bredd("Antal"),doRad).
      RUN pdf_text_xy_dec ("Spdf","Vgr: " + STRING(iVgInd[2]),iLM + 170,doRad).
      RUN pdf_text_xy_dec ("Spdf","Pris",iLM + 330 - bredd("Pris"),doRad).
      RUN pdf_text_xy_dec ("Spdf","Fsgvärde",iLM + 430 - bredd("Fsgvärde"),doRad).
      RUN pdf_text_xy_dec ("Spdf","Lagervärde",iLM + 530 - bredd("Lagervärde"),doRad).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
      ASSIGN doRad = doRad - 5.
      RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , doRad, 0.5).
      RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
      ASSIGN doRad = doRad - 13.
    END.
    RUN pdf_text_xy_dec ("Spdf",TT_Sasong2.SasongBeskr + " / " + STRING(TT_Sasong2.StDatum),iLM + 20,doRad).
    ASSIGN cWrk = TRIM(STRING(TT_Sasong2.Antal)).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 150 - bredd(cWrk),doRad).
    ASSIGN cWrk = TRIM(STRING(TT_Sasong2.Fsgv / TT_Sasong2.Antal,"-zz,zz9.99")).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 330 - bredd(cWrk),doRad).
    ASSIGN cWrk = TRIM(STRING(TT_Sasong2.Fsgv,"-zzz,zzz,zz9.99")).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 430 - bredd(cWrk),doRad).
    ASSIGN cWrk = TRIM(STRING(TT_Sasong2.Lagerv,"-zzz,zzz,zz9.99")).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 530 - bredd(cWrk),doRad).
  END.
  IF iAntal <> 0 THEN DO:
      ASSIGN doRad = dOrad - 15.
      RUN pdf_text_xy_dec ("Spdf","SUMMA",iLM + 20,doRad).
      ASSIGN cWrk = TRIM(STRING(iAntal)).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 150 - bredd(cWrk),doRad).
      ASSIGN cWrk = TRIM(STRING(dSumFsg,"-zzz,zzz,zz9.99")).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 430 - bredd(cWrk),doRad).
      ASSIGN cWrk = TRIM(STRING(dSumValue,"-zzz,zzz,zz9.99")).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 530 - bredd(cWrk),doRad).
      ASSIGN doRad = dOrad - 10.
  END.

  ASSIGN doRad = doRad - 13.
  IF doRad < 70 THEN
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
  RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_xy_dec ("Spdf","Säsong",iLM + 20,doRad).
  RUN pdf_text_xy_dec ("Spdf","Antal",iLM + 150 - bredd("Antal"),doRad).
  RUN pdf_text_xy_dec ("Spdf","Vgr: " + STRING(iVgInd[3]),iLM + 170,doRad).
  RUN pdf_text_xy_dec ("Spdf","Pris",iLM + 330 - bredd("Pris"),doRad).
  RUN pdf_text_xy_dec ("Spdf","Fsgvärde",iLM + 430 - bredd("Fsgvärde"),doRad).
  RUN pdf_text_xy_dec ("Spdf","Lagervärde",iLM + 530 - bredd("Lagervärde"),doRad).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  ASSIGN doRad = doRad - 5.
  RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , doRad, 0.5).
  RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).

  iAntal    = 0.
  dSumFsg   = 0.
  dSumValue = 0.
  FOR EACH TT_Sasong3 WHERE TT_Sasong3.Antal <> 0 NO-LOCK:
    ASSIGN doRad = doRad - 13.
    ASSIGN iAntal    = iAntal    + TT_Sasong3.Antal
           dSumFsg   = dSumFsg   + TT_Sasong3.Fsgv
           dSumValue = dSumValue + TT_Sasong3.Lagerv.
    IF doRad < 70 THEN
    DO:
      RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
      RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
      RUN pdf_text_xy_dec ("Spdf","Säsong",iLM + 20,doRad).
      RUN pdf_text_xy_dec ("Spdf","Antal",iLM + 150 - bredd("Antal"),doRad).
      RUN pdf_text_xy_dec ("Spdf","Vgr: " + STRING(iVgInd[3]),iLM + 170,doRad).
      RUN pdf_text_xy_dec ("Spdf","Pris",iLM + 330 - bredd("Pris"),doRad).
      RUN pdf_text_xy_dec ("Spdf","Fsgvärde",iLM + 430 - bredd("Fsgvärde"),doRad).
      RUN pdf_text_xy_dec ("Spdf","Lagervärde",iLM + 530 - bredd("Lagervärde"),doRad).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
      ASSIGN doRad = doRad - 5.
      RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , doRad, 0.5).
      RUN pdf_set_font ("Spdf", "Helvetica-Bold",10).
      ASSIGN doRad = doRad - 13.
    END.
    RUN pdf_text_xy_dec ("Spdf",TT_Sasong3.SasongBeskr + " / " + STRING(TT_Sasong3.StDatum),iLM + 20,doRad).
    ASSIGN cWrk = TRIM(STRING(TT_Sasong3.Antal)).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 150 - bredd(cWrk),doRad).
    ASSIGN cWrk = TRIM(STRING(TT_Sasong3.Fsgv / TT_Sasong3.Antal,"-zz,zz9.99")).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 330 - bredd(cWrk),doRad).
    ASSIGN cWrk = TRIM(STRING(TT_Sasong3.Fsgv,"-zzz,zzz,zz9.99")).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 430 - bredd(cWrk),doRad).
    ASSIGN cWrk = TRIM(STRING(TT_Sasong3.Lagerv,"-zzz,zzz,zz9.99")).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 530 - bredd(cWrk),doRad).
  END.
  IF iAntal <> 0 THEN DO:
      ASSIGN doRad = dOrad - 15.
      RUN pdf_text_xy_dec ("Spdf","SUMMA",iLM + 20,doRad).
      ASSIGN cWrk = TRIM(STRING(iAntal)).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 150 - bredd(cWrk),doRad).
      ASSIGN cWrk = TRIM(STRING(dSumFsg,"-zzz,zzz,zz9.99")).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 430 - bredd(cWrk),doRad).
      ASSIGN cWrk = TRIM(STRING(dSumValue,"-zzz,zzz,zz9.99")).
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 530 - bredd(cWrk),doRad).
      ASSIGN doRad = dOrad - 10.
  END.

  RUN pdf_close ("Spdf").

  IF SEARCH(pcRappFil) <> ? THEN
  DO:
    IF cMailEnablat = "1" THEN DO:
        RUN sendmail_tsl.p ("SASONGLISTA","Säsongslista",pcRappFil,"","","") NO-ERROR.
    END.
  END.
/*  RUN browse2pdf\viewxmldialog.w (pcRappFil,"Polygon Retail Solutions"). */

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

