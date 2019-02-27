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
/*     DEFINE VAR iAntEks     AS INTEGER    NO-UNDO. */
    DEFINE VAR cMailAdress AS CHARACTER  NO-UNDO.
/*     DEFINE VAR iFormatKod  AS INTEGER    NO-UNDO. */
&ELSE
    DEFINE INPUT PARAMETER cParaString  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER lDirekte    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER cPrinter    AS CHARACTER NO-UNDO.
/*     DEFINE INPUT PARAMETER iAntEks     AS INTEGER    NO-UNDO. */
    DEFINE INPUT PARAMETER cMailAdress AS CHARACTER  NO-UNDO.
/*     DEFINE INPUT PARAMETER iFormatKod  AS INTEGER    NO-UNDO. */
&ENDIF
    DEFINE VAR iAntEks     AS INTEGER    NO-UNDO.
    DEFINE VAR iFormatKod  AS INTEGER    NO-UNDO.

DEFINE VARIABLE cFirma AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VARIABLE hHodeTH      AS HANDLE     NO-UNDO.
DEFINE VARIABLE hLinjeTH     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTTHodeBuff  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTTLinjeBuff AS HANDLE     NO-UNDO.

DEFINE VARIABLE cRubrik AS CHARACTER  NO-UNDO.


DEFINE VARIABLE iPageHeight AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageWidth  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLeftCol    AS INTEGER     NO-UNDO.
DEFINE VARIABLE qH             AS HANDLE     NO-UNDO.
DEFINE VARIABLE qL             AS HANDLE     NO-UNDO.
DEFINE VARIABLE iColLbl AS INTEGER    EXTENT 9  NO-UNDO. /* sätts i SkrivRapportPDF */
DEFINE VARIABLE cColLbl AS CHARACTER    EXTENT 9  NO-UNDO. /* sätts i SkrivRapportPDF */
DEFINE VARIABLE dY AS INTEGER     NO-UNDO.

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
FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
RUN PopulateTT.

RUN SkrivRapportPDF.

/* RUN SkrivRapport. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PopulateTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateTT Procedure 
PROCEDURE PopulateTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    hHodeTH  = DYNAMIC-FUNCTION("getTempTable","get_bonghode.p",cParaString,?).
    hLinjeTH = DYNAMIC-FUNCTION("getTempTable","get_bonglinje.p",cParaString,?).
    hTTHodeBuff  = hHodeTH:DEFAULT-BUFFER-HANDLE.
    hTTLinjeBuff = hLinjeTH:DEFAULT-BUFFER-HANDLE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivColLabelsPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivColLabelsPDF Procedure 
PROCEDURE SkrivColLabelsPDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEFINE INPUT  PARAMETER iColLbl AS INTEGER EXTENT 8    NO-UNDO. */
  DEFINE VARIABLE iPage AS INTEGER     NO-UNDO.
/*   ASSIGN iColLbl[1] = iLeftCol */
/*          iColLbl[2] = 130      */
/*          iColLbl[3] = 317      */
/*          iColLbl[4] = 320      */
/*          iColLbl[5] = 430      */
/*          iColLbl[6] = 474      */
/*          iColLbl[7] = 545.     */


  iPage = pdf_Page ("Spdf").
 
 DO:
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[1],iColLbl[1],dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[2],iColLbl[2],dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[3],iColLbl[3],dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[4],iColLbl[4] - bredd(cColLbl[4]),dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[5],iColLbl[5] - bredd(cColLbl[5]),dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[6],iColLbl[6] - bredd(cColLbl[6]),dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[7],iColLbl[7] - bredd(cColLbl[7]),dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[8],iColLbl[8] - bredd(cColLbl[8]),dY).
   RUN pdf_text_xy_dec ("Spdf",cColLbl[9],iColLbl[9] - bredd(cColLbl[9]),dY).
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
/*     DEFINE INPUT  PARAMETER iSidnr       AS INTEGER    NO-UNDO. */
/*     DEFINE INPUT  PARAMETER cFaktNr AS CHARACTER  NO-UNDO.      */
    DEFINE INPUT  PARAMETER cBongTekst AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButNamn  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cButikkNr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dKundenr  AS DECIMAL     NO-UNDO.
    cButikkNr = STRING(hTTHodeBuff:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).
    cButNamn = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + cButikkNr,"ButNamn").    
    dKundenr = hTTHodeBuff:BUFFER-FIELD("Kundenr"):BUFFER-VALUE.
    IF dKundeNr > 0 THEN DO:
    END.
    PUT UNFORMATTED
      "<R4><P10><C30><B>" cBongTekst " " STRING(hTTHodeBuff:BUFFER-FIELD("BongNr"):BUFFER-VALUE) "</B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<R+1><P8><B><C6>"  "Dato"     "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("Dato"):BUFFER-VALUE) " " STRING(hTTHodeBuff:BUFFER-FIELD("Tid"):BUFFER-VALUE,"HH:MM:SS")
      "<R+1><B><C6>"      "Butikk"   "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) " " cButNamn
      "<R+1><C6>"         "Kasse"    "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("KasseNr"):BUFFER-VALUE)
      "<R+1><C6>"         "Kasserer" "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("KassererNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("KassererNavn"):BUFFER-VALUE)
      "<R+1><C6>"         "Selger"   "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("SelgerNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("SelgerNavn"):BUFFER-VALUE).
      IF dKundeNr > 0 THEN
          PUT UNFORMATTED
          "<R+1><C6>"         "Kunde"   "<C12>: " STRING(dKundeNr) " " TRIM(hTTHodeBuff:BUFFER-FIELD("KundeNavn"):BUFFER-VALUE).
      PUT UNFORMATTED
          "<R+2><P10><B><C6>" "Sum"    "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("Belop"):BUFFER-VALUE,"->,>>>,>>9.99") "<P8>" SKIP.
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
    ASSIGN cHlbl1 = "Artikkelnr"
          cHlbl2 = "Bongtekst"
          cHlbl3 = "Ean"
          cHlbl4 = "Str"
          cHlbl5 = "Antall"
          cHlbl6 = "Brutto pris"
          cHlbl7 = "Linjerab"
          cHlbl8 = "Subtotrab"
          cHlbl9 = "Linjesum".
          
          
    DEFINE INPUT  PARAMETER cBongTekst AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButNamn  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cButikkNr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dKundenr  AS DECIMAL     NO-UNDO.
    cButikkNr = STRING(hTTHodeBuff:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).
    cButNamn = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + cButikkNr,"ButNamn").    
    dKundenr = hTTHodeBuff:BUFFER-FIELD("Kundenr"):BUFFER-VALUE.
    IF dKundeNr > 0 THEN DO:
    END.
    PUT UNFORMATTED
      "<R4><P10><C30><B>" cBongTekst " " STRING(hTTHodeBuff:BUFFER-FIELD("BongNr"):BUFFER-VALUE) "</B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<R+1><P8><B><C6>"  "Dato"     "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("Dato"):BUFFER-VALUE) " " STRING(hTTHodeBuff:BUFFER-FIELD("Tid"):BUFFER-VALUE,"HH:MM:SS")
      "<R+1><B><C6>"      "Butikk"   "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) " " cButNamn
      "<R+1><C6>"         "Kasse"    "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("KasseNr"):BUFFER-VALUE)
      "<R+1><C6>"         "Kasserer" "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("KassererNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("KassererNavn"):BUFFER-VALUE)
      "<R+1><C6>"         "Selger"   "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("SelgerNr"):BUFFER-VALUE) " " TRIM(hTTHodeBuff:BUFFER-FIELD("SelgerNavn"):BUFFER-VALUE).
      IF dKundeNr > 0 THEN
          PUT UNFORMATTED
          "<R+1><C6>"         "Kunde"   "<C12>: " STRING(dKundeNr) " " TRIM(hTTHodeBuff:BUFFER-FIELD("KundeNavn"):BUFFER-VALUE).
      PUT UNFORMATTED
          "<R+2><P10><B><C6>" "Sum"    "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("Belop"):BUFFER-VALUE,"->,>>>,>>9.99") "<P8>" SKIP.          
          
------------------------------------------------------------------------------*/
    DEFINE        VARIABLE  cKopiStr     AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dDato    AS DATE        NO-UNDO.
    DEFINE        VARIABLE  cStr         AS CHARACTER   NO-UNDO.
    DEFINE        VARIABLE  iMittCol     AS INTEGER  INIT 305   NO-UNDO.
    DEFINE VARIABLE iPlusCol AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iY AS INTEGER     NO-UNDO.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */

    DEFINE VARIABLE cButNamn  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cButikkNr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dKundenr  AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE cHeaderLbl AS CHARACTER EXTENT 7  NO-UNDO.

    cRubrik = IF AVAIL bruker AND bruker.lng = "SE" THEN "KVITTOKOPIA" ELSE "BONGKOPI".

    cHeaderLbl = IF AVAIL bruker AND bruker.lng = "SE" THEN "Datum,Butik,Kassa,Kassör,Säljare,Kund,Summa" ELSE "Dato,Butikk,Kasse,Kasserer,Selger,Kunde,Sum".

    iLeftCol    = 50.
    iMittCol    = 200.
    iPlusCol = 45.
    iY = 64.
    cButikkNr = STRING(hTTHodeBuff:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).
    cButNamn = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + cButikkNr,"ButNamn").    
    dKundenr = hTTHodeBuff:BUFFER-FIELD("Kundenr"):BUFFER-VALUE.

    
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",22).
    RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",26).
    RUN pdf_set_TextY("Spdf",iPageHeight - 38).
    
    RUN pdf_text_xy_dec ("Spdf",cRubrik + " " + STRING(hTTHodeBuff:BUFFER-FIELD("BongNr"):BUFFER-VALUE),iMittCol,iPageHeight - 38).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",9).

    RUN pdf_text_xy_dec ("Spdf","Dato",iLeftCol,iPageHeight - iY).
    RUN pdf_text_xy_dec ("Spdf",STRING(hTTHodeBuff:BUFFER-FIELD("Dato"):BUFFER-VALUE) + " " + STRING(hTTHodeBuff:BUFFER-FIELD("Tid"):BUFFER-VALUE,"HH:MM:SS"),iLeftCol + iPlusCol,iPageHeight - iY).
    iY = iY + 13.

    RUN pdf_text_xy_dec ("Spdf","Butikk",iLeftCol,iPageHeight - iY).
    RUN pdf_text_xy_dec ("Spdf",STRING(hTTHodeBuff:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) + " " + cButNamn,iLeftCol + iPlusCol,iPageHeight - iY).
    iY = iY + 13.

    RUN pdf_text_xy_dec ("Spdf","Kasse",iLeftCol,iPageHeight - iY).
    RUN pdf_text_xy_dec ("Spdf",STRING(hTTHodeBuff:BUFFER-FIELD("KasseNr"):BUFFER-VALUE),iLeftCol + iPlusCol,iPageHeight - iY).
    iY = iY + 13.

    RUN pdf_text_xy_dec ("Spdf","Kasserer",iLeftCol,iPageHeight - iY).
    RUN pdf_text_xy_dec ("Spdf",STRING(hTTHodeBuff:BUFFER-FIELD("KassererNr"):BUFFER-VALUE) + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("KassererNavn"):BUFFER-VALUE),iLeftCol + iPlusCol,iPageHeight - iY).
    iY = iY + 13.

    RUN pdf_text_xy_dec ("Spdf","Selger",iLeftCol,iPageHeight - iY).
    RUN pdf_text_xy_dec ("Spdf",STRING(hTTHodeBuff:BUFFER-FIELD("SelgerNr"):BUFFER-VALUE) + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("SelgerNavn"):BUFFER-VALUE),iLeftCol + iPlusCol,iPageHeight - iY).
    iY = iY + 13.

    IF dKundeNr > 0 THEN DO:
        RUN pdf_text_xy_dec ("Spdf","Kunde",iLeftCol,iPageHeight - iY).
        RUN pdf_text_xy_dec ("Spdf", STRING(dKundeNr) + " " + TRIM(hTTHodeBuff:BUFFER-FIELD("KundeNavn"):BUFFER-VALUE),iLeftCol + iPlusCol,iPageHeight - iY).
    END.
    iY = iY + 13.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    RUN pdf_text_xy_dec ("Spdf","Sum",iLeftCol,iPageHeight - iY).
    RUN pdf_text_xy_dec ("Spdf", STRING(hTTHodeBuff:BUFFER-FIELD("Belop"):BUFFER-VALUE,"->,>>>,>>9.99"),iLeftCol + iPlusCol,iPageHeight - iY).

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
   DEFINE        VARIABLE pcRappFil      AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE iSidNr         AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE iRadNr         AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE cDetaljRad     AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE cSumRad        AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE qH             AS HANDLE     NO-UNDO.
   DEFINE        VARIABLE qL             AS HANDLE     NO-UNDO.
   DEFINE        VARIABLE iAntLinjer     AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE iAntNotatRader AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE iCount         AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE cFakturaType   AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE cFakturaNr     AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE cRefTxt        AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE iKontrollRad   AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE lGT12          AS LOGICAL    NO-UNDO.
   DEFINE        VARIABLE lBlankSkrevet  AS LOGICAL    NO-UNDO.
   DEFINE        VARIABLE dMva           AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE cHlbl1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl2 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl3 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl4 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl5 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl6 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl7 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl8 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl9 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl2 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl3 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl4 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl5 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl6 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl7 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl8 AS CHARACTER  NO-UNDO.
   cDetaljRad = "><C6><P8><RIGHT=C+6>&1<C13>&2<C30>&3<C38><RIGHT=C+4>&4<C43><RIGHT=C+6>&5<C52><RIGHT=C+4>&6<C58><RIGHT=C+4>&7<C65><RIGHT=C+4>&8<C70><RIGHT=C+7>&9".
   cSumRad    = "<R&1><C45>&2<P8><RIGHT=C+7>&3<C60>".


   /* Hantering av rader för olika layouter */
   /* 1 = Internationell, 2 = Postgiro */
   ASSIGN iKontrollrad = 65.

   RUN GetTempFileName in wLibHandle ("bkopi", "xpr", output pcRappFil).

   ASSIGN cSlbl1 = "Mva".
   ASSIGN cHlbl1 = "Artikkelnr"
          cHlbl2 = "Bongtekst"
          cHlbl3 = "Ean"
          cHlbl4 = "Str"
          cHlbl5 = "Antall"
          cHlbl6 = "Brutto pris"
          cHlbl7 = "Linjerab"
          cHlbl8 = "Subtotrab"
          cHlbl9 = "Linjesum".

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
   PUT UNFORMATTED "<ALIGN=BASE><FArial>" SKIP.
/*    PUT UNFORMATTED "<R16><C40><#1><R21><C75><FRAME#1>" SKIP. */
   qH:GET-FIRST().
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE B_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("B_id"):BUFFER-VALUE)).
       DO:
           iSidNr = 1.
           RUN SkrivHeader ("BONGKOPI").
         
           ASSIGN iRadNr   = 15.
           PUT UNFORMATTED                                     /* 168,255,168 */
           SUBSTITUTE("<R&1><P8><C6><FROM><R&2><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.
/*            PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,"Prodnr","Beskr","Levert","Str","Fakturert","Enhetspris","Rabatt","Mva","Sum") "</B>" SKIP. */
           PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) "</B>" SKIP.
           qL:QUERY-OPEN().
           qL:GET-FIRST().
           ASSIGN lBlankSkrevet = FALSE
                  dMva          = 0.
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               IF hTTLinjeBuff:BUFFER-FIELD("Makulert"):BUFFER-VALUE <> TRUE AND 
                   hTTLinjeBuff:BUFFER-FIELD("TTId"):BUFFER-VALUE <> 95 AND
                   hTTLinjeBuff:BUFFER-FIELD("TTId"):BUFFER-VALUE <> 147 THEN DO:
                   ASSIGN dMva = dMva + hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE.
                   ASSIGN iRadNr     = iRadNr + 1
                          iAntLinjer = iAntLinjer + 1
                          lBlankSkrevet = lGT12 = TRUE
                          lGT12      = hTTLinjeBuff:BUFFER-FIELD("TTId"):BUFFER-VALUE > 12.
             /*            PUT UNFORMATTED SUBSTITUTE(cDetaljRad,STRING(iRadNr), */
                   PUT UNFORMATTED (IF lGT12 AND NOT lBlankSkrevet THEN "<R+1.9" ELSE "<R+.9") SUBSTITUTE(cDetaljRad,
                       IF STRING(hTTLinjeBuff:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) = "" THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE),
                       "<B>" + SUBSTR(STRING(hTTLinjeBuff:BUFFER-FIELD("BongTekst"):BUFFER-VALUE),1,20) + "</B>",
                       STRING(hTTLinjeBuff:BUFFER-FIELD("Strekkode"):BUFFER-VALUE),
                       IF lGT12 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("Storrelse"):BUFFER-VALUE),
                       IF lGT12 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE,"->>9.999"),
                       IF lGT12 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("BongPris"):BUFFER-VALUE,"->,>>>,>>9.99"),
                       IF lGT12 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeRab"):BUFFER-VALUE,"->>>>9.99"),
                       IF lGT12 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("SubtotalRab"):BUFFER-VALUE,"->>>>9.99"),
                       (IF lGT12 THEN "<B>" ELSE "") + STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeSum"):BUFFER-VALUE - 
                              hTTLinjeBuff:BUFFER-FIELD("LinjeRab"):BUFFER-VALUE - 
                              hTTLinjeBuff:BUFFER-FIELD("SubtotalRab"):BUFFER-VALUE,"->,>>>,>>9.99")) + (IF lGT12 THEN "</B>" ELSE "").

                   /* Skall vi göra sidbryt? */
                   IF iRadNr > iKontrollrad AND iAntLinjer > 2  THEN DO:
                       PAGE.
                       ASSIGN iSidNr = iSidNr + 1
                              iRadNr = 15.
                       RUN SkrivHeader ("BONGKOPI").
                       PUT UNFORMATTED                                     /* 168,255,168 */
                       SUBSTITUTE("<R&1><C6><FROM><R&2><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.
    /*                    PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,"Prodnr","Beskr","Levert","Str","Fakturert","Enhetspris","Rabatt","Mva","Sum") "</B>" SKIP. */
                       PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) "</B>" SKIP.
                   END.
               END.
               qL:GET-NEXT().
           END.
           PUT UNFORMATTED "<R+.9" SUBSTITUTE(cDetaljRad,
               " ",
               "<B>" + cSlbl1 + "</B>",
               " ",
               " ",
               " ",
               " ",
               " ",
               " ",
               "<B>"  + STRING(dMva,"->,>>>,>>9.99")) + "</B>".
/*            PUT UNFORMATTED SUBSTITUTE("<R&1><C40><FROM><R&2><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadnr + 3),STRING(iRadNr + 4)) SKIP.       */
/*            PUT UNFORMATTED "<B>" SUBSTITUTE(cSumRad,STRING(65),"Avgfri","Avgpl","Netto","Avrunding","Rabatt","Rabatt%","Mva","Sum inkl. mva") "</B>" SKIP. */
/*            PUT UNFORMATTED "<B>" SUBSTITUTE(cSumRad,STRING(iRadNr + 3),cSlbl1,cSlbl2) "</B>" SKIP.                                                         */
/*            PUT UNFORMATTED "<B>" SUBSTITUTE(cSumRad,STRING(iRadNr + 4),                                                                                    */
/*                                       STRING(dMva,"->,>>>,>>9.99"),                                                                                        */
/*                                       STRING(hTTHodeBuff:BUFFER-FIELD("Belop"):BUFFER-VALUE,"->,>>>,>>9.99")) + "</B>" SKIP.                               */
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
   DEF VAR cFilNavn     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE dMva AS DECIMAL     NO-UNDO.
   DEFINE        VARIABLE qH             AS HANDLE     NO-UNDO.
   DEFINE        VARIABLE qL             AS HANDLE     NO-UNDO.
   DEFINE VARIABLE lMomsSkrivet AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE cTmpLabel AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE cMvaTxt AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE iTTId AS INTEGER     NO-UNDO.
          ASSIGN iLeftCol    = 40
                 iColLbl[1] = iLeftCol
                 iColLbl[2] = 90  /* 130 */
                 iColLbl[3] = 215  /* 317 */
                 iColLbl[4] = 300  /* 320 */
                 iColLbl[5] = 350  /* 415 */
                 iColLbl[6] = 405  /* 474 */
                 iColLbl[7] = 450. /* 545 */
                 iColLbl[8] = 500. /* 545 */
                 iColLbl[9] = 560. /* 545 */

     ASSIGN cMvaTxt = IF bruker.lng = "SE" THEN "Moms" ELSE "Mva".
     ASSIGN cTmpLabel = IF bruker.lng = "SE" THEN "Artikelnr,Beskrivning,Ean,Str,Antal,Brutto pris,Linjerab,Subtotrab,Linjesum" ELSE "Artikkelnr,Bongtekst,Ean,Str,Antall,Brutto pris,Linjerab,Subtotrab,Linjesum".
     ASSIGN cColLbl[1] = ENTRY(1,cTmpLabel)
            cColLbl[2] = ENTRY(2,cTmpLabel)
            cColLbl[3] = ENTRY(3,cTmpLabel)
            cColLbl[4] = ENTRY(4,cTmpLabel)
            cColLbl[5] = ENTRY(5,cTmpLabel)
            cColLbl[6] = ENTRY(6,cTmpLabel)
            cColLbl[7] = ENTRY(7,cTmpLabel)
            cColLbl[8] = ENTRY(8,cTmpLabel)
            cColLbl[9] = ENTRY(9,cTmpLabel).
    
    CREATE QUERY qH.
    CREATE QUERY qL.
    qL:SET-BUFFERS(hTTLinjeBuff).
    qH:SET-BUFFERS(hTTHodeBuff).
    qH:QUERY-PREPARE("FOR EACH " + hTTHodeBuff:NAME).
    qH:QUERY-OPEN().
    qH:GET-FIRST().
   cFilNavn = SESSION:TEMP-DIR + "Bong" + "_" + STRING(hTTHodeBuff:BUFFER-FIELD("b_id"):BUFFER-VALUE) + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".pdf".
   RUN pdf_new ("Spdf",cFilNavn).
   RUN pdf_set_BottomMargin ("Spdf", 20).
   RUN pdf_set_PaperType ("Spdf","A4").
/*    RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13). */
   RUN pdf_set_Orientation ("Spdf","portrait").
   iPageHeight = pdf_PageHeight ("Spdf").
   iPageWidth  = pdf_PageWidth ("Spdf").

   REPEAT WHILE NOT qH:QUERY-OFF-END:
       RUN pdf_new_page ("Spdf").
       RUN SkrivHeaderPDF.
       dY = iPageHeight - 180.
       RUN SkrivColLabelsPDF.
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE b_id = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("b_id"):BUFFER-VALUE)).
       qL:QUERY-OPEN().
       qL:GET-FIRST().

       REPEAT WHILE NOT qL:QUERY-OFF-END:
           IF hTTLinjeBuff:BUFFER-FIELD("Makulert"):BUFFER-VALUE <> TRUE AND
/*                hTTLinjeBuff:BUFFER-FIELD("TTId"):BUFFER-VALUE <> 95 AND */
               hTTLinjeBuff:BUFFER-FIELD("TTId"):BUFFER-VALUE <> 146 AND
               hTTLinjeBuff:BUFFER-FIELD("TTId"):BUFFER-VALUE <> 147 THEN DO:
               ASSIGN dMva = dMva + hTTLinjeBuff:BUFFER-FIELD("MvaKr"):BUFFER-VALUE.
               DEFINE VARIABLE iRadNr AS INTEGER     NO-UNDO.
               DEFINE VARIABLE iAntLinjer AS INTEGER     NO-UNDO.
               DEFINE VARIABLE lBlankSkrevet AS LOGICAL     NO-UNDO.
               DEFINE VARIABLE lGT12 AS LOGICAL     NO-UNDO.
               iTTId = hTTLinjeBuff:BUFFER-FIELD("TTId"):BUFFER-VALUE.
               IF iTTId = 95 AND lMomsSkrivet = FALSE THEN DO:
                   dY = dY - 13.
                   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
                   RUN pdf_text_xy_dec ("Spdf",cMvaTxt,iColLbl[2],dY).
                   cTxt = STRING(dMva,"->,>>>,>>9.99").
                   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
                   RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[9] - bredd(cTxt),dY).
                   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
                   lMomsSkrivet = TRUE.
                   dY = dY - 13.
               END.
               dY = dY - 13.
               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer + 1
                      lBlankSkrevet = lGT12 = TRUE
                      lGT12      = hTTLinjeBuff:BUFFER-FIELD("TTId"):BUFFER-VALUE > 12.
         /*            PUT UNFORMATTED SUBSTITUTE(cDetaljRad,STRING(iRadNr), */
               cTxt = " ".
               RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
               cTxt = STRING(hTTLinjeBuff:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[1],dY).
               cTxt = SUBSTR(STRING(hTTLinjeBuff:BUFFER-FIELD("BongTekst"):BUFFER-VALUE),1,20).
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[2],dY).
               cTxt = STRING(hTTLinjeBuff:BUFFER-FIELD("Strekkode"):BUFFER-VALUE).
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[3],dY).
               cTxt = IF lGT12 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("Storrelse"):BUFFER-VALUE).
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[4] - bredd(cTxt),dY).
               cTxt = IF lGT12 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE).
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[5] - bredd(cTxt),dY).
               cTxt = IF lGT12 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("BongPris"):BUFFER-VALUE,"->,>>>,>>9.99").
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[6] - bredd(cTxt),dY).
               cTxt = IF lGT12 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeRab"):BUFFER-VALUE,"->>>>9.99").
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[7] - bredd(cTxt),dY).
               cTxt = IF lGT12 THEN " " ELSE STRING(hTTLinjeBuff:BUFFER-FIELD("SubtotalRab"):BUFFER-VALUE,"->>>>9.99").
               RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[8] - bredd(cTxt),dY).
               IF iTTId <> 95 THEN DO:
                   IF lGT12 THEN
                       RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).

                   cTxt = STRING(hTTLinjeBuff:BUFFER-FIELD("LinjeSum"):BUFFER-VALUE - hTTLinjeBuff:BUFFER-FIELD("LinjeRab"):BUFFER-VALUE - hTTLinjeBuff:BUFFER-FIELD("SubtotalRab"):BUFFER-VALUE,"->,>>>,>>9.99").
                   RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[9] - bredd(cTxt),dY).
                   IF lGT12 THEN
                       RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
                   IF iTTId = 69 THEN DO:
LEAVE. /* Funkar inte riktigt med code128 */
                       cTxt = STRING(hTTLinjeBuff:BUFFER-FIELD("Strekkode"):BUFFER-VALUE).
                       IF LENGTH(cTxt) = 22 THEN DO:

                           RUN BarCode128CStringConvert.p ("C",cTxt,OUTPUT cTxt).

                           RUN pdf_load_font IN h_PDFinc ("Spdf","Code128",".\pdfinclude\code128.ttf",".\PDFinclude\code128.afm",""). 
                           dY = dY - 50.
                           RUN  pdf_set_font ("Spdf","Code128",36.0).
                           RUN pdf_set_parameter("Spdf","ScaleY","1.5").
                           RUN  pdf_text_xy ("Spdf",cTxt,iColLbl[3], dY).

/*                                                                                                                                    */
/*                            dY = dY - 45.                                                                                           */
/*                            RUN pdf_load_font IN h_PDFinc ("Spdf","Code39",".\pdfinclude\code39.ttf",".\PDFinclude\code39.afm",""). */
/*                            RUN  pdf_set_font ("Spdf","Code39",16.0).                                                               */
/*                            RUN  pdf_text_xy ("Spdf","*" + cTxt + "*",iColLbl[3], dY).                                              */
/*                                                                                                                                    */

                           RUN pdf_set_parameter("Spdf","ScaleY","1").
                           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
                       END.
                   END.
               END.

               /* Skall vi göra sidbryt? */
/*                IF iRadNr > iKontrollrad AND iAntLinjer > 2  THEN DO:                                                                                                              */
/*                    PAGE.                                                                                                                                                          */
/*                    ASSIGN iSidNr = iSidNr + 1                                                                                                                                     */
/*                           iRadNr = 15.                                                                                                                                            */
/*                    RUN SkrivHeader ("BONGKOPI").                                                                                                                                  */
/*                    PUT UNFORMATTED                                     /* 168,255,168 */                                                                                          */
/*                    SUBSTITUTE("<R&1><C6><FROM><R&2><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.                                           */
/* /*                    PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,"Prodnr","Beskr","Levert","Str","Fakturert","Enhetspris","Rabatt","Mva","Sum") "</B>" SKIP. */ */
/*                    PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) "</B>" SKIP.                      */
/*                END.                                                                                                                                                               */
           END.

           qL:GET-NEXT().

       END.
       IF NOT lMomsSkrivet THEN DO:
           dY = dY - 13.
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
           RUN pdf_text_xy_dec ("Spdf",cMvaTxt,iColLbl[2],dY).
           cTxt = STRING(dMva,"->,>>>,>>9.99").
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
           RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[9] - bredd(cTxt),dY).
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
       END.

       qH:GET-NEXT().
   END.
   RUN pdf_close ("Spdf").
   qH:QUERY-CLOSE().
   qL:QUERY-CLOSE().
   DELETE OBJECT qH.
   DELETE OBJECT qL.
   RUN browse2pdf\viewxmldialog.w (cFilNavn,cRubrik).
   OS-DELETE VALUE(cFilNavn).
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

