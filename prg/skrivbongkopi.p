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
RUN PopulateTT.

RUN SkrivRapport.

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

/* ************************  Function Implementations ***************** */

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

