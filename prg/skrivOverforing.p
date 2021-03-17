&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : skrivOverforing.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* DEFINE VAR ipBuntNr LIKE ovBunt.BuntNr  NO-UNDO. */

DEFINE INPUT PARAMETER ipBuntNr LIKE ovBunt.BuntNr  NO-UNDO.
DEFINE INPUT PARAMETER cBruker AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cSkriver AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER bUtskrift AS LOG NO-UNDO.

DEFINE VARIABLE cButikNavnFra AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE cButikNavnTil AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE cRubrik       AS CHARACTER FORMAT "X(16)" NO-UNDO.
DEFINE VARIABLE cBeskr        AS CHARACTER FORMAT "X(25)" NO-UNDO.
DEFINE VARIABLE cCmd          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst        AS CHARACTER NO-UNDO.

DEFINE VARIABLE iColVg        AS INTEGER INIT 30 NO-UNDO.
DEFINE VARIABLE iRposVg       AS INTEGER INIT 90 NO-UNDO.
DEFINE VARIABLE iColLopNr     AS INTEGER INIT 170 NO-UNDO.
DEFINE VARIABLE iRposLop      AS INTEGER INIT 215  NO-UNDO.
DEFINE VARIABLE iColBeskr     AS INTEGER INIT 230 NO-UNDO.
DEFINE VARIABLE iColStorl     AS INTEGER INIT 490 NO-UNDO.
DEFINE VARIABLE iRposStr      AS INTEGER INIT 500 NO-UNDO.
DEFINE VARIABLE iColAntall    AS INTEGER INIT 505 NO-UNDO.
DEFINE VARIABLE iRposAnt      AS INTEGER INIT 530 NO-UNDO.
DEFINE VARIABLE iColVkost     AS INTEGER INIT 540 NO-UNDO.
DEFINE VARIABLE iRposVkost    AS INTEGER INIT 580    NO-UNDO.
DEFINE VARIABLE iColSum       AS INTEGER INIT 620 NO-UNDO.
DEFINE VARIABLE iRposSum      AS INTEGER INIT 650 NO-UNDO.
DEFINE VARIABLE iColMerknad   AS INTEGER INIT 670 NO-UNDO.

DEFINE VARIABLE cSideRubrikker  AS CHARACTER  INIT "RESERVASJON - Plukkliste,RESERVASJON - Følgeseddel" NO-UNDO.
DEFINE VARIABLE cMerknadLbl     AS CHARACTER INIT "Merknad:" NO-UNDO.
DEFINE VARIABLE cMerknad        AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cButikkInfo     AS CHARACTER  INIT "Fra:,Til :" NO-UNDO.
DEFINE VARIABLE cListeRubrikker AS CHARACTER  INIT "Lev.Artikkelnr,Fargekode,Varetekst,Antall,Str,Til str,Varekost,Sum,Notat" NO-UNDO.
DEFINE VARIABLE cSumLinje       AS CHARACTER  INIT "Sum:" NO-UNDO.
DEFINE VARIABLE cKvittering     AS CHARACTER  INIT "Mottatt:,Butikk:" NO-UNDO.
DEFINE VARIABLE cOppdaterttxt   AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE cBuntNr         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iSidnr          AS INTEGER     NO-UNDO.

DEFINE VARIABLE lCode39    AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStrekrad  AS INTEGER FORMAT ">9" NO-UNDO.
DEFINE VARIABLE iRad       AS INTEGER NO-UNDO.
DEFINE VARIABLE iFirstRow  AS INTEGER INIT 115 NO-UNDO.
DEFINE VARIABLE iY         AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttOvAvdSum
    FIELD ButikkNrFra LIKE OvBuffer.ButikkNrFra
    FIELD ButikkNrTil LIKE OvBuffer.ButikkNrTil
    FIELD AvdelingSum AS DECI DECIMALS 2 FORMAT "->>>>,>>9.99" EXTENT 10
    INDEX FraTil ButikkNrFra DESC ButikkNrTil DESC.

DEFINE BUFFER bufovBuffer FOR ovBuffer.

{ pdf_inc.i "THIS-PROCEDURE"}
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
{syspara.i 1 1 7 cTekst}
{syspar2.i 1 1 8 cCmd}
IF cCmd = '' THEN 
  cCmd = "cmd\FoxitReader.exe /t".
ELSE 
  cCmd = RIGHT-TRIM(cTekst,'\') + '\' + cCmd.

FIND ovBunt WHERE ovBunt.BuntNr = ipBuntNr NO-LOCK NO-ERROR.
IF NOT AVAIL ovBunt THEN DO:
    MESSAGE "Ukjent reservasjonsordre."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.
ELSE IF NOT CAN-FIND(FIRST ovBuffer OF ovBunt) THEN DO:
    MESSAGE "Det ligger ingen artikler på denne reservasjonsordren."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN "AVBRYT".
END.

ASSIGN 
  cBuntNr = STRING(ipBuntNr)
  .

RUN PDFSkrivRapport.

RETURN RETURN-VALUE.

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
   DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
   iSidNr = iSidNr + 1.
   cTxt = STRING(TODAY) + "   Res.nr: " + cBuntNr. 
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",14).
   RUN pdf_text_xy_dec ("Spdf",cTxt, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 35).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",24).
   RUN pdf_text_xy_dec ("Spdf",cRubrik, pdf_LeftMargin ("Spdf") + 200,pdf_PageHeight("Spdf") - 35).
   
   IF cOppdaterttxt <> "" THEN 
   DO:
       RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",18).
       RUN pdf_text_xy_dec ("Spdf",cOppdaterttxt, pdf_LeftMargin ("Spdf") + 280,pdf_PageHeight("Spdf") - 60).
   END.
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",14).
   RUN pdf_text_xy_dec ("Spdf",STRING(iSidNr), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") - bredd(STRING(iSidNr)) - 20 ,pdf_PageHeight("Spdf") - 35).


   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(1,CButikkInfo), pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 55).
   RUN pdf_text_xy_dec ("Spdf",cButikNavnFra, pdf_LeftMargin ("Spdf") + 25,pdf_PageHeight("Spdf") - 55).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(2,CButikkInfo), pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 65).
   RUN pdf_text_xy_dec ("Spdf",cButikNavnTil, pdf_LeftMargin ("Spdf") + 25,pdf_PageHeight("Spdf") - 65).
   RUN pdf_text_xy_dec ("Spdf",cMerknadLbl, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 75).
   RUN pdf_text_xy_dec ("Spdf", cMerknad, pdf_LeftMargin ("Spdf") + 80,pdf_PageHeight("Spdf") - 75).

   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
/*    RUN pdf_text_xy_dec ("Spdf",cTxt, pdf_LeftMargin ("Spdf") + 30,pdf_PageHeight("Spdf") - 35). */
   RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight ("Spdf") - 85, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") - 20, pdf_PageHeight ("Spdf") - 85, 0.5).

   RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cListeRubrikker), iColVg      ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cListeRubrikker), iColLopNr   ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(3,cListeRubrikker), iColBeskr   ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(4,cListeRubrikker), iColAntall  ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(5,cListeRubrikker), iColStorl   ,pdf_PageHeight("Spdf") - 96).
/*   RUN pdf_text_xy_dec ("Spdf",ENTRY(6,cListeRubrikker), iColTilStorl,pdf_PageHeight("Spdf") - 96).*/
   RUN pdf_text_xy_dec ("Spdf",ENTRY(7,cListeRubrikker), iColVkost   ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(8,cListeRubrikker), iColSum     ,pdf_PageHeight("Spdf") - 96).
   RUN pdf_text_xy_dec ("Spdf",ENTRY(9,cListeRubrikker), iColMerknad ,pdf_PageHeight("Spdf") - 96).
   
   RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight ("Spdf") - 101, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") - 20, pdf_PageHeight ("Spdf") - 101, 0.5).
   
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
  DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cKatalog AS CHARACTER NO-UNDO.

  {syspara.i 1 1 8 cKatalog}
  IF cKatalog = '' THEN 
    cKatalog = "utskrift".
  OS-CREATE-DIR VALUE(RIGHT-TRIM(cKatalog,'\')).    
    cKatalog = RIGHT-TRIM(cKatalog,'\') + '\'.

  ASSIGN 
    /*pcRappFil = SESSION:TEMP-DIRECTORY + "ovBunt_" + STRING(ipBuntNr) + ".pdf"*/
    pcRappFil = cKatalog + "ovBunt_" + STRING(ipBuntNr) + '_' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','')  + ".pdf"
    cOppdaterttxt = IF OvBunt.DatoOppdatert = ? 
                      THEN "" /*"IKKE OPPDATERT"*/ 
                    ELSE ""
    cMerknad = OvBunt.Merknad
    .

  /* Åpner stream til skriverfil og setter opp utskriften. */
  RUN pdf_new ("Spdf",pcRappFil).
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 30).
  RUN pdf_set_BottomMargin ("Spdf", 40).
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf", 13).
  RUN pdf_set_Orientation ("Spdf", "landscape").

  FOR EACH ovBuffer OF ovBunt NO-LOCK 
    WHERE ovBuffer.ButikkNrFra <> ovBuffer.ButikkNrTil 
    BREAK BY ovBuffer.ButikkNrFra 
          BY ovBuffer.ButikkNrTil:
            
      dTotSum = 0.
      iSidNr = 0.
    IF FIRST-OF(ovBuffer.ButikkNrTil) THEN 
    DO:
        ASSIGN cRubrik = ENTRY(1,cSideRubrikker).
        FIND Butiker WHERE 
          Butiker.Butik = ovBuffer.ButikkNrFra NO-LOCK NO-ERROR.
        ASSIGN 
          cButikNavnFra = IF AVAIL Butiker THEN Butiker.ButNamn ELSE "?????????"
          iProfilNr = IF AVAIL Butiker THEN Butiker.Profilnr ELSE 1
          .
        FIND Butiker WHERE 
          Butiker.Butik = ovBuffer.ButikkNrTil NO-LOCK NO-ERROR.
        ASSIGN 
          cButikNavnTil = IF AVAIL Butiker 
                            THEN Butiker.ButNamn 
                          ELSE "?????????".
        FIND ttOvAvdSum WHERE 
          ttOvAvdSum.ButikkNrFra = OvBuffer.ButikkNrFra AND
          ttOvAvdSum.ButikkNrTil = OvBuffer.ButikkNrTil NO-ERROR.
        IF NOT AVAIL ttOvAvdSum THEN 
        DO:
            CREATE ttOvAvdSum.
            ASSIGN ttOvAvdSum.ButikkNrFra = OvBuffer.ButikkNrFra
                   ttOvAvdSum.ButikkNrTil = OvBuffer.ButikkNrTil.
        END.

        /* Side 1 - Plukkliste */
        RUN pdf_new_page ("Spdf").
        RUN PDFPageHeader.
        iY = iFirstRow.
        RUN PDF_one2three(1).
        
        /* Side 2 Følgeseddel. */
        iY = iFirstRow.
        RUN pdf_new_page ("Spdf").
        ASSIGN iRad      = 10
               iStrekrad = iRad - 2.
        ASSIGN cRubrik = ENTRY(2,cSideRubrikker).
        RUN PDFPageHeader.
        RUN SkrivBarcode (STRING(ipBuntNr)).
        RUN PDF_one2three(2).
    END.
  END.
  
  RUN pdf_close ("Spdf").
  IF bUtskrift THEN 
  DO:    
    FILE-INFO:FILENAME = pcRappFil.
    OS-COMMAND SILENT VALUE(cCmd + ' ' + FILE-INFO:FULL-PATHNAME + ' "' + cSkriver + '"').
    MESSAGE pcRappFil SKIP 
     cCmd SKIP 
     FILE-INFO:FULL-PATHNAME SKIP
     cSkriver SKIP
     cCmd + ' ' + FILE-INFO:FULL-PATHNAME + ' "' + cSkriver + '"'
    VIEW-AS ALERT-BOX.    
  END.
  ELSE DO:
    RUN browse2pdf\viewxmldialog.w (pcRappFil,"Polygon Retail Solutions").
  END.
  
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
  
  FOR EACH bufovBuffer OF ovBunt NO-LOCK WHERE 
    bufovBuffer.ButikkNrFra = ovBuffer.ButikkNrFra AND
    bufovBuffer.ButikkNrTil = ovBuffer.ButikkNrTil 
    BY bufovBuffer.Vg 
    BY bufovBuffer.LopNr 
    BY bufovBuffer.Storl:
      
    FIND ArtBas NO-LOCK WHERE 
      Artbas.Vg = bufovBuffer.Vg AND 
      ArtBas.LopNr = bufovBuffer.LopNr NO-ERROR.
      
    IF AVAIL ArtBas THEN 
    DO:
      FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.
      ASSIGN iAvdelingNr = IF AVAIL HuvGr AND HuvGr.AvdelingNr > 0 AND HuvGr.AvdelingNr < 9 THEN HuvGr.AvdelingNr ELSE 9.
    END.

    ASSIGN 
      dVarekost = bufovBuffer.Varekost
      dSum      = bufovBuffer.Antall * dVarekost
      dTotSum   = dTotSum + dSum
      cBeskr    = IF AVAIL ArtBas THEN SUBSTR(ArtBas.Beskr,1,40) ELSE "".
      ACCUMULATE bufovBuffer.Antall (TOTAL)
      .
       
    RUN pdf_text_xy_dec ("Spdf",ArtBas.LevKod, iRposVg - bredd(ArtBas.LevKod) ,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",STRING(bufovBuffer.LopNr,"zzzzz9") , iRposLop - bredd(STRING(bufovBuffer.LopNr,"zzzzz9"))   ,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",cBeskr, iColBeskr   ,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",STRING(bufovBuffer.Antall,"-zz9"), iRposAnt - bredd(STRING(bufovBuffer.Antall,"-zz9"))  ,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",bufovBuffer.Storl, iRposStr - bredd(bufovBuffer.Storl)  ,pdf_PageHeight("Spdf") - iY).
/*    IF TRIM(bufovBuffer.Storl) <> TRIM(bufovBuffer.Storl) THEN                                                                   */
/*        RUN pdf_text_xy_dec ("Spdf",bufovBuffer.TilStorl, iRposRilStr - bredd(bufovBuffer.TilStorl),pdf_PageHeight("Spdf") - iY).*/
    RUN pdf_text_xy_dec ("Spdf",STRING(dVarekost,">>,>>9.99"), iRposVkost - bredd(STRING(dVarekost,">>,>>9.99"))   ,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",STRING(dSum,"->>>,>>9.99"), iRposSum - bredd(STRING(dSum,"->>>,>>9.99"))     ,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",bufovBuffer.Merknad, iColMerknad ,pdf_PageHeight("Spdf") - iY).

         ASSIGN iRad = iRad + 1.
    ASSIGN ttOvAvdSum.AvdelingSum[iAvdelingNr] = ttOvAvdSum.AvdelingSum[iAvdelingNr] + dSum.
    iY = iY + 15.
    
    /* Sidebryt */
    IF pdf_PageHeight("Spdf") - iY - 30 <= 0 THEN 
    DO:
      iY = iFirstRow.
      RUN pdf_new_page ("Spdf").
      ASSIGN iRad      = 10
             iStrekrad = iRad - 2.
      RUN PDFPageHeader.
    END.
  END.
        
  IF iTyp = 1 THEN 
  DO:
      RUN pdf_line IN h_PDFinc  ("Spdf", iColAntall - bredd(cSumLinje),pdf_PageHeight("Spdf") - iY, iRposSum , pdf_PageHeight("Spdf") - iY, 0.5).
      iY = iY + 15.
      RUN pdf_text_xy_dec ("Spdf",cSumLinje, iColAntall - bredd(cSumLinje) ,pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9"), iRposAnt - bredd(STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9"))  ,pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",STRING(dTotSum,"->>>,>>9.99"), iRposSum - bredd(STRING(dTotSum,"->>>,>>9.99"))     ,pdf_PageHeight("Spdf") - iY).
  END.
  ELSE IF iTyp = 2 THEN  
  DO:
    RUN pdf_line IN h_PDFinc  ("Spdf", iColAntall - bredd(cSumLinje),pdf_PageHeight("Spdf") - iY, iRposSum , pdf_PageHeight("Spdf") - iY, 0.5).
    iY = iY + 15.
    RUN pdf_text_xy_dec ("Spdf",cSumLinje, iColAntall - bredd(cSumLinje) ,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9"), iRposAnt - bredd(STRING((ACCUM TOTAL bufovBuffer.Antall),"-zz9"))  ,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",STRING(dTotSum,"->>>,>>9.99"), iRposSum - bredd(STRING(dTotSum,"->>>,>>9.99"))     ,pdf_PageHeight("Spdf") - iY).
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

&IF DEFINED(EXCLUDE-SkrivBarcode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivBarcode Procedure
PROCEDURE SkrivBarcode:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cBarcode AS CHARACTER   NO-UNDO.

  IF lCode39 = FALSE THEN 
  DO:
      RUN pdf_load_font IN h_PDFinc ("Spdf","Code39",SEARCH("pdfinclude\samples\support\code39.ttf"),SEARCH("pdfinclude\samples\support\code39.afm"),""). 
      ASSIGN lCode39 = TRUE.
  END.
    
  RUN pdf_set_font ("Spdf","Code39",20.0).
  RUN pdf_set_TextX("Spdf",794).
  RUN pdf_set_TextY("Spdf",580).
  RUN pdf_text_rotate ("Spdf", 90).
  RUN pdf_text ("Spdf","*" + cBarcode + "*").

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

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

