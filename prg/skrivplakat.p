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


    DEFINE INPUT PARAMETER cVaremerke  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cBeskr      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cEtikett1   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cEtikett2   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER cVarefakta  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER dOrdinarie  AS DECIMAL    NO-UNDO.
    DEFINE INPUT PARAMETER dTilbud     AS DECIMAL    NO-UNDO.
    DEFINE INPUT PARAMETER lSkrivbilde AS LOGICAL    NO-UNDO.
    DEFINE INPUT PARAMETER cBildeFIl AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER cLogoFil AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER iAntEx AS INTEGER    NO-UNDO.
    DEFINE INPUT PARAMETER iRapptype   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dPris1 AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dPris2 AS DECIMAL    NO-UNDO.

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
/* IF lDirekte AND NOT CAN-DO(SESSION:GET-PRINTERS(),cPrinter) THEN */
/*     RETURN.                                                      */

IF lSkrivbilde THEN
    RUN SkrivRapport.
ELSE
    RUN SkrivRapportUbilde.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-oldrapp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oldrapp Procedure 
PROCEDURE oldrapp :
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
   DEFINE VARIABLE dBruttoPris    AS INTEGER    NO-UNDO.
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
   DEFINE VARIABLE cPrisStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cReplaceStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iCol AS INTEGER  NO-UNDO.
   cDetaljRad = "><P8><C6><RIGHT=C+6>&1<C13>&2<C30>&3<C34><RIGHT=C+4>&4<C38><RIGHT=C+4>&5<C46><RIGHT=C+4>&6<C53><RIGHT=C+4>&10<C60><RIGHT=C+4>&7<C65><RIGHT=C+4>&8<C70><RIGHT=C+7>&9".
   cSumRad    = "<R@1.7><P8><C7><RIGHT=C+7>&1<C15><RIGHT=C+7>&2<C23><RIGHT=C+7>&3<C31><RIGHT=C+7>&4<C39><RIGHT=C+7>&5<C45><RIGHT=C+7>&6<C52><RIGHT=C+7>&7<C60><RIGHT=C+7>&8<C70><RIGHT=C+7>&9".


   ASSIGN dPris1 = IF dTilbud > 0 THEN dTilbud ELSE dOrdinarie
          dPris2 = IF dTilbud > 0 THEN dOrdinarie ELSE 0.

/*    iFormatkod = 2. */
   /* Hantering av rader för olika layouter */
   /* 1 = Internationell, 2 = Postgiro */
   RUN GetTempFileName in wLibHandle ("fakt", "xpr", output pcRappFil).

   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   PUT UNFORMATTED "<COPIES=" STRING(iANtEx) ">" SKIP.
/*    IF NOT lDirekte THEN DO:                                                         */
/* /*        PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'. */     */
/*        PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '>'. */
       PUT CONTROL '<PREVIEW=ZoomToWidth>'.
/*    END.                                                                             */
/*    ELSE DO:                                                                         */
/*        PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '>'. */
/*    END.                                                                             */
   PUT UNFORMATTED "<ALIGN=BASE><FArial><UNITS=MM><B>" SKIP.
/*    PUT UNFORMATTED "<TOP=10mm><R1>" SKIP. */
   PUT UNFORMATTED  "<R2><C2><|4><FROM><R67><C2><LINE>" SKIP
                    "<R2><C2><FROM><R2><C78><LINE>" SKIP
                    "<R2><C78><FROM><R67><C78><LINE>" SKIP
                    "<R67><C2><FROM><R67><C78><LINE>" SKIP
           .
       PUT UNFORMATTED
          "<TRANSPARENT=false><R" STRING(3 + 9)  ",5><C40><#3><R" STRING(3 + 1) ",25><C3><IMAGE#3=" +
          cLogoFil + ">".

       PUT UNFORMATTED "<R14><P20><C1><CENTER=C80>" CAPS(cVaremerke) SKIP
                       "<R20><P32><C1><CENTER=C80>" cBeskr     SKIP
                       "<R22><P20><C1><CENTER=C80>" cEtikett1 SKIP
                       "<R24><P20><C1><CENTER=C80>" cEtikett2 SKIP.
       IF lSkrivbilde = TRUE THEN
       PUT UNFORMATTED
          "<TRANSPARENT=false><R" STRING(37)  ",5><C55><#3><R" STRING(27) ",25><C30><IMAGE#3=" +
          cBildeFIl + ">".
       
       
       PUT UNFORMATTED "<R40> " SKIP.
       DO icount = 1 TO NUM-ENTRIES(cVarefakta,CHR(10)):
           PUT UNFORMATTED 
           "<R+1><P14><C1><CENTER=C80>" CHR(183) " " TRIM(ENTRY(iCount,cVarefakta,CHR(10))) SKIP.
       END.
       IF ROUND(dPris1,0) = dPris1 THEN
           PUT UNFORMATTED "<R62><P72><C1><CENTER=C80> kr. " + TRIM(ENTRY(1,STRING(dPris1,">>>>>9.99"))) + ",-"  SKIP.
       ELSE DO:
           iCol = IF dPris1 < 10 THEN 50 ELSE IF dPris1 < 100 THEN 54 ELSE IF dPris1 < 1000 THEN 57 ELSE IF dPris1 < 10000 THEN
                                              60 ELSE 63.
           cReplaceStr = "<C" + STRING(iCol + 1) + "><P36>,".
           cPrisStr =  "<R62><P72><C1><RIGHT=C+" + STRING(iCol) + " > kr. " + TRIM(STRING(dPris1,">>>>>9.99")).
           cPrisStr = REPLACE(cPrisStr,",",cReplaceStr).
/*            cPrisStr = REPLACE(cPrisStr,",","<C69><P36>,"). */
           PUT UNFORMATTED cPrisStr SKIP.
       END.
       IF dPris2 > 0 AND ROUND(dPris2,0) = dPris2 THEN
           PUT UNFORMATTED "<R64><P24><C1><CENTER=C80>(Ord. " + TRIM(ENTRY(1,STRING(dPris2,">>>>>9.99"))) + ",-)"  SKIP.
       ELSE IF dPris2 > 0 THEN DO:
           iCol = IF dPris2 < 10 THEN 40 ELSE IF dPris2 < 100 THEN 42 ELSE IF dPris2 < 1000 THEN 44 ELSE IF dPris2 < 10000 THEN
                                              46 ELSE 48.
           cReplaceStr = "<C" + STRING(iCol + 1) + "><P12>,".
           cPrisStr =  "<R65><P24><C1><RIGHT=C+" + STRING(iCol) + " >(Ord. " + TRIM(STRING(dPris2,">>>>>9.99")) + "<P24>)".
           cPrisStr = REPLACE(cPrisStr,",",cReplaceStr).
/*            cPrisStr = "<R65><P24><C1><CENTER=C80>(Ord. " + TRIM(STRING(dPris2,">>>>>9.99")) + ")". */
/*            cPrisStr = REPLACE(cPrisStr,",","<P12>,"). */
           PUT UNFORMATTED cPrisStr SKIP.
       END.
            /*
                cVarefakta 
                dOrdinarie 
                dTilbud    
                lSkrivbilde
                iRapptype  
            */


/*    PUT UNFORMATTED "<R14><C40><#1><R18><C75><FRAME#1>" SKIP. /* <15>..<19>*/ */
/*    PUT UNFORMATTED "<R16><C40><#1><R21><C75><FRAME#1>" SKIP. */
/*            PUT UNFORMATTED                                     /* 168,255,168 */                                                                                            */
/*            SUBSTITUTE("<R&1><C6><FROM><R&2><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.                                             */
/*            PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(REPLACE(cDetaljRad,"&10",cHlblX1),cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) "</B>" SKIP. */
   OUTPUT CLOSE.
   RUN VisXprint.p (pcRappFil).
   OS-DELETE VALUE(pcRappFil).
   
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
   DEFINE VARIABLE dBruttoPris    AS INTEGER    NO-UNDO.
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
   DEFINE VARIABLE cPrisStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cReplaceStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iCol AS INTEGER  NO-UNDO.
   cDetaljRad = "><P8><C6><RIGHT=C+6>&1<C13>&2<C30>&3<C34><RIGHT=C+4>&4<C38><RIGHT=C+4>&5<C46><RIGHT=C+4>&6<C53><RIGHT=C+4>&10<C60><RIGHT=C+4>&7<C65><RIGHT=C+4>&8<C70><RIGHT=C+7>&9".
   cSumRad    = "<R@1.7><P8><C7><RIGHT=C+7>&1<C15><RIGHT=C+7>&2<C23><RIGHT=C+7>&3<C31><RIGHT=C+7>&4<C39><RIGHT=C+7>&5<C45><RIGHT=C+7>&6<C52><RIGHT=C+7>&7<C60><RIGHT=C+7>&8<C70><RIGHT=C+7>&9".


   ASSIGN dPris1 = IF dTilbud > 0 THEN dTilbud ELSE dOrdinarie
          dPris2 = IF dTilbud > 0 THEN dOrdinarie ELSE 0.

/*    iFormatkod = 2. */
   /* Hantering av rader för olika layouter */
   /* 1 = Internationell, 2 = Postgiro */
   RUN GetTempFileName in wLibHandle ("fakt", "xpr", output pcRappFil).

   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   PUT UNFORMATTED "<COPIES=" STRING(iANtEx) ">" SKIP.
/*    IF NOT lDirekte THEN DO:                                                         */
/* /*        PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'. */     */
/*        PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '>'. */
   IF iRapptype = 1 THEN
       PUT CONTROL '<PREVIEW=ZoomToWidth>'.
/*    END.                                                                             */
/*    ELSE DO:                                                                         */
/*        PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '>'. */
/*    END.                                                                             */
   PUT UNFORMATTED "<ALIGN=BASE><FArial><UNITS=MM><B>" SKIP.
/*    PUT UNFORMATTED "<TOP=10mm><R1>" SKIP. */
   PUT UNFORMATTED  "<R2><C2><|4><FROM><R67><C2><LINE>" SKIP
                    "<R2><C2><FROM><R2><C78><LINE>" SKIP
                    "<R2><C78><FROM><R67><C78><LINE>" SKIP
                    "<R67><C2><FROM><R67><C78><LINE>" SKIP
           .
   IF cLogofil <> "" AND SEARCH(cLogofil) <> ? THEN
       PUT UNFORMATTED
          "<TRANSPARENT=false><R" STRING(3 + 9)  ",5><C40><#3><R" STRING(3 + 1) ",25><C3><IMAGE#3=" +
          cLogoFil + ">".

       PUT UNFORMATTED "<R14><P20><C1><CENTER=C80>" CAPS(cVaremerke) SKIP
                       "<R20><P32><C1><CENTER=C80>" cBeskr     SKIP
                       "<R22><P20><C1><CENTER=C80>" cEtikett1 SKIP
                       "<R24><P20><C1><CENTER=C80>" cEtikett2 SKIP.
       IF lSkrivbilde = TRUE THEN
       PUT UNFORMATTED
          "<TRANSPARENT=false><R" STRING(37)  ",5><C55><#3><R" STRING(27) ",25><C30><IMAGE#3=" +
          cBildeFIl + ">".
       
       
       PUT UNFORMATTED "<R40> " SKIP.
       DO icount = 1 TO NUM-ENTRIES(cVarefakta,CHR(10)):
           PUT UNFORMATTED 
           "<R+1><P14><C1><CENTER=C80>" CHR(183) " " TRIM(ENTRY(iCount,cVarefakta,CHR(10))) SKIP.
       END.
/*        iCol = IF dPris1 < 10 THEN 31 ELSE IF dPris1 < 100 THEN 28 ELSE IF dPris1 < 1000 THEN 25 ELSE IF dPris1 < 10000 THEN */
/*                                           22 ELSE 19.           /* P72 */                                                   */
       iCol = IF dPris1 < 10 THEN 22 ELSE IF dPris1 < 100 THEN 22 ELSE IF dPris1 < 1000 THEN 17 ELSE IF dPris1 < 10000 THEN
                                          10 ELSE 4.           /* P72 */
       PUT UNFORMATTED "</B><R62><P36><C" + STRING(iCol) + ">kr.<P128> " + TRIM(ENTRY(1,STRING(dPris1,">>>>>9.99"))) +
                 (IF ROUND(dPris1,0) = dPris1 THEN ",-" ELSE "<P72>," + TRIM(ENTRY(2,STRING(dPris1,">>>>>9.99"))))  SKIP.
       IF dPris2 > 0 THEN DO:
/*            iCol = IF dPris2 < 10 THEN 34 ELSE IF dPris2 < 100 THEN 33 ELSE IF dPris2 < 1000 THEN 32 ELSE IF dPris2 < 10000 THEN */
/*                                               31 ELSE 30.                                                                       */
           iCol = IF dPris2 < 10 THEN 34 ELSE IF dPris2 < 100 THEN 33 ELSE IF dPris2 < 1000 THEN 32 ELSE IF dPris2 < 10000 THEN
                                              31 ELSE 30.
           PUT UNFORMATTED "<R64.5><P24><C" + STRING(iCol) + ">(Ord. " + TRIM(ENTRY(1,STRING(dPris2,">>>>>9.99"))) + 
               (IF dPris2 = ROUND(dPris2,0) THEN ",-)" ELSE "<P12>," + TRIM(ENTRY(2,STRING(dPris2,">>>>>9.99"))) + "<P24>)") SKIP.
       END.
   OUTPUT CLOSE.
   RUN VisXprint.p (pcRappFil).
   OS-DELETE VALUE(pcRappFil).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivRapportUbilde) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapportUbilde Procedure 
PROCEDURE SkrivRapportUbilde :
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
   DEFINE VARIABLE dBruttoPris    AS INTEGER    NO-UNDO.
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
   DEFINE VARIABLE cPrisStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cReplaceStr AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cBeskr2 AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE iCol AS INTEGER  NO-UNDO.
   DEFINE VARIABLE iLength AS INTEGER   NO-UNDO.
   DEFINE VARIABLE cFormat AS CHARACTER   NO-UNDO.
   cDetaljRad = "><P8><C6><RIGHT=C+6>&1<C13>&2<C30>&3<C34><RIGHT=C+4>&4<C38><RIGHT=C+4>&5<C46><RIGHT=C+4>&6<C53><RIGHT=C+4>&10<C60><RIGHT=C+4>&7<C65><RIGHT=C+4>&8<C70><RIGHT=C+7>&9".
   cSumRad    = "<R@1.7><P8><C7><RIGHT=C+7>&1<C15><RIGHT=C+7>&2<C23><RIGHT=C+7>&3<C31><RIGHT=C+7>&4<C39><RIGHT=C+7>&5<C45><RIGHT=C+7>&6<C52><RIGHT=C+7>&7<C60><RIGHT=C+7>&8<C70><RIGHT=C+7>&9".


   ASSIGN dPris1 = IF dTilbud > 0 THEN dTilbud ELSE dOrdinarie
          dPris2 = IF dTilbud > 0 THEN dOrdinarie ELSE 0.

/*    iFormatkod = 2. */
   /* Hantering av rader för olika layouter */
   /* 1 = Internationell, 2 = Postgiro */
   RUN GetTempFileName in wLibHandle ("fakt", "xpr", output pcRappFil).

   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   PUT UNFORMATTED "<COPIES=" STRING(iANtEx) ">" SKIP.
/*    IF NOT lDirekte THEN DO:                                                         */
/* /*        PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'. */     */
/*        PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '>'. */
   IF iRapptype = 1 THEN
       PUT CONTROL '<PREVIEW=ZoomToWidth>'.
/*    END.                                                                             */
/*    ELSE DO:                                                                         */
/*        PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '>'. */
/*    END.                                                                             */
   PUT UNFORMATTED "<ALIGN=BASE><FArial><UNITS=MM><B>" SKIP.
/*    PUT UNFORMATTED "<TOP=10mm><R1>" SKIP. */
   PUT UNFORMATTED  "<R2><C2><|4><FROM><R67><C2><LINE>" SKIP
                    "<R2><C2><FROM><R2><C78><LINE>" SKIP
                    "<R2><C78><FROM><R67><C78><LINE>" SKIP
                    "<R67><C2><FROM><R67><C78><LINE>" SKIP
           .
       PUT UNFORMATTED
          "<TRANSPARENT=false><R" STRING(3 + 9)  ",5><C40><#3><R" STRING(3 + 1) ",25><C3><IMAGE#3=" +
          cLogoFil + ">".
       cFormat = IF LENGTH(cBeskr) < 11 THEN
                    "<R22><P70><C1><CENTER=C80>" ELSE
                 IF LENGTH(cBeskr) < 13 THEN     
                    "<R22><P64><C1><CENTER=C80>" ELSE
                 IF LENGTH(cBeskr) < 15 THEN     
                    "<R22><P55><C1><CENTER=C80>" ELSE
                 IF LENGTH(cBeskr) < 17 THEN     
                    "<R22><P48><C1><CENTER=C80>" ELSE
                    "<R22><P40><C1><CENTER=C80>".
           PUT UNFORMATTED "<R16><P20><C1><CENTER=C80>" CAPS(cVaremerke) SKIP
                           cFormat cBeskr         SKIP
                           "<R26><P34><C1><CENTER=C80>" cEtikett1 SKIP
                           "<R29><P34><C1><CENTER=C80>" cEtikett2 SKIP.
           PUT UNFORMATTED "<R31> " SKIP.
/*        ELSE DO:                                                                      */
/*            iLength = LENGTH(ENTRY(1,cBeskr," ")).                                    */
/*            cBeskr2 = SUBSTR(cBeskr,iLength + 1).                                     */
/*            cBeskr  = ENTRY(1,cBeskr," ").                                            */
/*            PUT UNFORMATTED "<R15><P20><C1><CENTER=C80>" CAPS(cVaremerke) SKIP        */
/*                            "<R21><P64><C1><CENTER=C80>" Substr(cBeskr,1,15) SKIP     */
/*                            "<R27><P64><C1><CENTER=C80>" Substr(cBeskr2,1,15)    SKIP */
/*                            "<R31><P34><C1><CENTER=C80>" cEtikett1  SKIP              */
/*                            "<R34><P34><C1><CENTER=C80>" cEtikett2  SKIP.             */
/*            PUT UNFORMATTED "<R35> " SKIP.                                            */
/*        END.                                                                          */
/*        IF lSkrivbilde = TRUE THEN                                                                */
/*        PUT UNFORMATTED                                                                           */
/*           "<TRANSPARENT=false><R" STRING(37)  ",5><C55><#3><R" STRING(27) ",25><C30><IMAGE#3=" + */
/*           cBildeFIl + ">".                                                                       */
/*                                                                                                  */
       
       DO icount = 1 TO NUM-ENTRIES(cVarefakta,CHR(10)):
           PUT UNFORMATTED 
           "<R+1><P22><C1><CENTER=C80>" CHR(183) " " TRIM(ENTRY(iCount,cVarefakta,CHR(10))) SKIP.
       END.
/*        iCol = IF dPris1 < 10 THEN 31 ELSE IF dPris1 < 100 THEN 28 ELSE IF dPris1 < 1000 THEN 25 ELSE IF dPris1 < 10000 THEN */
/*                                           22 ELSE 19.           /* P72 */                                                   */
       iCol = IF dPris1 < 10 THEN 18 ELSE IF dPris1 < 100 THEN 14 ELSE IF dPris1 < 1000 THEN 10 ELSE IF dPris1 < 10000 THEN
                                          6 ELSE 2.           /* P72 */
       PUT UNFORMATTED "<B><R62><P36><C" + STRING(iCol) + ">kr.<P156> " + TRIM(ENTRY(1,STRING(dPris1,">>>>>9.99"))) +
                 (IF ROUND(dPris1,0) = dPris1 THEN "<P72>,-" ELSE "<P72>," + TRIM(ENTRY(2,STRING(dPris1,">>>>>9.99")))) + "</B>" SKIP.
       IF dPris2 > 0 THEN DO:
/*            iCol = IF dPris2 < 10 THEN 34 ELSE IF dPris2 < 100 THEN 33 ELSE IF dPris2 < 1000 THEN 32 ELSE IF dPris2 < 10000 THEN */
/*                                               31 ELSE 30.                                                                       */
           iCol = IF dPris2 < 10 THEN 34 ELSE IF dPris2 < 100 THEN 33 ELSE IF dPris2 < 1000 THEN 32 ELSE IF dPris2 < 10000 THEN
                                              31 ELSE 30.
           PUT UNFORMATTED "<R64.5><P24><C" + STRING(iCol) + ">(Ord. " + TRIM(ENTRY(1,STRING(dPris2,">>>>>9.99"))) + 
               (IF dPris2 = ROUND(dPris2,0) THEN ",-)" ELSE "<P12>," + TRIM(ENTRY(2,STRING(dPris2,">>>>>9.99"))) + "<P24>)") SKIP.
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

