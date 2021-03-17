&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description : ï¿½ Dette er karrakterene som settes inn isteden for øæå.

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
DEFINE VARIABLE cUtskrift    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCmd         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ocReturn     AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE TT_RapportRader NO-UNDO
    FIELD iPageNum AS INTEGER  /* Sidnr */
    FIELD iColPage AS INTEGER  /* Hantering av 'før manga cols' */
    FIELD iRadNum  AS INTEGER
    FIELD cRadData AS CHARACTER
    INDEX RadNum iPageNum iColPage iRadNum.

{runlib.i}
/*{initjukebox.i} Kan ikke brukes her. Kjør jukebox programmer direkte. */

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

IF lDirekte AND NOT CAN-DO(SESSION:GET-PRINTERS(),cPrinter) THEN
    RETURN.

{syspara.i 1 1 8 cUtskrift}
IF cUtskrift = '' THEN 
  cUtskrift = ".\utskrift".
OS-CREATE-DIR VALUE(RIGHT-TRIM(cUtskrift,'\')).    
  cUtskrift = RIGHT-TRIM(cUtskrift,'\') + '\'.

FIND PkSdlHode WHERE PkSdlHode.PkSdlId = INT(ENTRY(1,cParaString,"|")) NO-ERROR.
IF AVAILABLE PkSdlHode THEN 
    FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
IF NOT AVAILABLE PkSdlHode OR NOT AVAILABLE PkSdlLinje THEN 
    RETURN.
        
RUN PopulateTT.

RUN SkrivRapport.

/* rydder opp før avsluttning. */
EMPTY TEMP-TABLE TT_RapportRader.

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

/*    hHodeTH  = DYNAMIC-FUNCTION("getTempTable","get_PkSdlHode.p",cParaString,?). */
/*    hLinjeTH = DYNAMIC-FUNCTION("getTempTable","get_PkSdlLinje.p",cParaString,?).*/

    RUN get_PkSdlHode.p  ("validsession", cParaString, OUTPUT TABLE-HANDLE hHodeTH, OUTPUT ocReturn).
    RUN get_PkSdlLinje.p ("validsession", cParaString, OUTPUT TABLE-HANDLE hLinjeTH, OUTPUT ocReturn).

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
    DEFINE INPUT  PARAMETER iSidnr       AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER cFaktNr AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cPkSdlType AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lKopi        AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cKopiStr     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cBarCodeFaktNr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iLength AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cLevLbl AS CHARACTER INIT "Levmelding"  NO-UNDO. /* om melding går over flera rader och ingen levmelding så blir denna blank. */
    DEFINE VARIABLE cMelding AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLevMeld AS CHARACTER   NO-UNDO.
  
    iLength = LENGTH(cFaktNr).
    IF ROUND(iLength / 2,0) = iLength / 2 THEN
        cBarCodeFaktNr = cFaktNr.
    ELSE
        cBarCodeFaktNr = "0" + cFaktNr.
/*            hTTHodeBuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE */
    ASSIGN
/*          cFaktNr = " " + STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE) */
/*            cFaktNr = IF cFaktNr = ? THEN "" ELSE cFaktNr                            */
           cKopiStr = IF lKopi THEN "<C40>K O P I" ELSE "".
    cMelding = TRIM(hTTHodeBuff:BUFFER-FIELD("Merknad"):BUFFER-VALUE).
    cLevMeld = TRIM(hTTHodeBuff:BUFFER-FIELD("MeldingFraLev"):BUFFER-VALUE).
    IF cLevMeld = "" AND NUM-ENTRIES(cMelding,CHR(10)) > 1 THEN DO:
        cLevLbl = "".
        cLevMeld = ENTRY(2,cMelding,CHR(10)).
        cMelding = ENTRY(1,cMelding,CHR(10)). 
    END.
    PUT UNFORMATTED
        "<AT=5,> "   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<R+.8,><C57><P30><B><RIGHT=C+20>" cPkSdlType " " cFaktNr "</B>"   /* "<P12></B><C77><P10>" PAGE-NUMBER FORMAT ">>" SKIP */
      "<AT=16,160><#2><AT=+10,+38>" "<BARCODE#2,TYPE=2_5_interleaved,CHECKSUM=none,VALUE=" cBarCodeFaktNr ">"
      "<R6><C6><P7>" "PkSdlId      " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)       "<C26>Status<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE)
                                                                                                                 + " / " + STRING(hTTHodeBuff:BUFFER-FIELD("Status"):BUFFER-VALUE)
      "<R6.7><C6><P7>" "Pakkseddel   " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE)     "<C26>Endret<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("EDato"):BUFFER-VALUE,"99/99/99")
      "<R+.7><C6><P7>" "Opphav       " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlOpphav"):BUFFER-VALUE) "<C26>Tid<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("ETid"):BUFFER-VALUE,"HH:MM:SS")
      "<R+.7><C6><P7>" "CL           " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("CL"):BUFFER-VALUE)          "<C26>Bruker<C32>: " /* TRIM(hTTHodeBuff:BUFFER-FIELD("BrukerID"):BUFFER-VALUE) */
      "<R+.7><C6><P7>" "SendtDato    " "<C12>: " STRING(hTTHodeBuff:BUFFER-FIELD("SendtDato"):BUFFER-VALUE,"99/99/99") "<C26>Regdato<C32>: " STRING(hTTHodeBuff:BUFFER-FIELD("RegistrertDato"):BUFFER-VALUE,"99/99/99")
    /* Kundeadress */
    /* Faktura header info */
    "<R6><C47><P7>"    "Side      "     "<C53><P7>: "  STRING(iSidNr) SKIP
    "<R6.7><C47><P7>"  "Registrert"     "<C53><P7>: "  STRING(hTTHodeBuff:BUFFER-FIELD("RegistrertTid"):BUFFER-VALUE,"HH:MM:SS") SKIP
    "<R7.4><C47><P7>"  "Registrert av"  "<C53><P7>: "  SKIP
    "<R8.1><C47><P7>"  "Merknad"        "<C53><P7>: "  REPLACE(cMelding,CHR(10),' ') SKIP
    "<R8.8><C47><P7>"   cLevLbl         "<C53><P7>: "  REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(cLevMeld,CHR(10),' '),'OrdreType','OT'),'Sesongkode','Ses'),'LandedCost','LC'),'Fil:',''),'Katalog:','') SKIP
    "<R9.5><C47><P7>"  "EkstId"         "<C53><P7>: "  TRIM(hTTHodeBuff:BUFFER-FIELD("EkstId"):BUFFER-VALUE) SKIP
    "<R10.2><C47><P7>" "Sum frakt"      "<C53><P7>: "  (IF hTTHodeBuff:BUFFER-FIELD("SumFrakt"):BUFFER-VALUE <> 0 THEN
                                                           STRING(hTTHodeBuff:BUFFER-FIELD("SumFrakt"):BUFFER-VALUE,"->>,>>>,>>9.99") ELSE " ") SKIP
    "<R12><C6><P7>" "Leverandør" SKIP /* Test endring med æ ø å Æ Ø Å  */
    "<R13.5><C6><P10><B>" hTTHodeBuff:BUFFER-FIELD("Levnamn"):BUFFER-VALUE SKIP.

/*     PUT UNFORMATTED "<USE#1>" hTTHodeBuff:BUFFER-FIELD("FNotat"):BUFFER-VALUE "</USE>". */
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
   DEFINE VARIABLE cTmpDetaljRad  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cTmpRubrik  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSumRad        AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE qH             AS HANDLE     NO-UNDO.
   DEFINE VARIABLE qL             AS HANDLE     NO-UNDO.
   DEFINE VARIABLE iAntLinjer     AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
   DEFINE VARIABLE cPkSdlType   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cPkSdlNr     AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE iBilagsType    AS INTEGER    NO-UNDO.
   DEFINE VARIABLE dAntal    AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dRest     AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dLevert   AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dInnkjSum AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dSalgSum  AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dTmpInnkj AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE dTmpSalg  AS DECIMAL    NO-UNDO.
   DEFINE VARIABLE iKontrollRad   AS INTEGER    NO-UNDO.
   DEFINE VARIABLE iExtraRad AS INTEGER    NO-UNDO. /* Om vi har iFormatKod = 2 skall vi legga till vid summarad */
   DEFINE VARIABLE cHlbl1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl2 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl3 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl4 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl5 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl6 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl7 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl8 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlbl9 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cHlblX1 AS CHARACTER  NO-UNDO. /* substitute tar bara 9 parametrar och vi har 11, vi gør en replace mha denna */
   DEFINE VARIABLE cHlblX2 AS CHARACTER  NO-UNDO. /* substitute tar bara 9 parametrar och vi har 11, vi gør en replace mha denna */
   DEFINE VARIABLE cHlblX3 AS CHARACTER  NO-UNDO. /* substitute tar bara 9 parametrar och vi har 11, vi gør en replace mha denna */
   DEFINE VARIABLE cSlbl1 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl2 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl3 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl4 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl5 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl6 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl7 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl8 AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSlbl9 AS CHARACTER  NO-UNDO.
   
   cDetaljRad = "><P6><C4><RIGHT=C+6>&1<C11>&2<C16>&3<C30>&4<C40>&5<C44>&6<C49><RIGHT=C+4>&7<C54><RIGHT=C+4>&8<C59><RIGHT=C+4>&9<C63><RIGHT=C+4>&10<C68><RIGHT=C+4>&11<C73><RIGHT=C+4>&12".
   cSumRad    = "<R@1.7><P8><C7><RIGHT=C+7>&1<C15><RIGHT=C+7>&2<C23><RIGHT=C+7>&3<C31><RIGHT=C+7>&4<C39><RIGHT=C+7>&5<C45><RIGHT=C+7>&6<C52><RIGHT=C+7>&7<C60><RIGHT=C+7>&8<C70><RIGHT=C+7>&9".

   /*RUN GetTempFileName IN wLibHandle ("pksdl", "xpr", OUTPUT pcRappFil).*/
   pcRappFil = cUtskrift + 
               "But_" + 
               STRING(PkSdlLinje.ButikkNr) + '_' +  
               "PkSdl" + "_" + 
               STRING(PkSdlHode.PkSdlNr) + '_' +  
               REPLACE(STRING(TODAY),'/','') + '-' + 
               STRING(ETIME) + 
               /*REPLACE(STRING(TIME,"HH:MM:SS"),':','') +*/ 
               ".xpr".

/*    iFormatkod = 2. */
   /* Hantering av rader før olika layouter */
   /* 1 = Internationell, 2 = Postgiro */
   ASSIGN iKontrollrad = IF iFormatKod = 1 THEN 61 ELSE IF iFormatKod = 2 THEN 34 ELSE 61  /* 34 */
          iExtraRad    = IF iFormatKod = 2 THEN 3 ELSE 0. 

   ASSIGN cHlbl1  = "Art.nr"
          cHlbl2  = "Lev.art"
          cHlbl3  = "Artnavn"
          cHlbl4  = "Farge"
          cHlbl5  = "Str"
          cHlbl6  = "Enh"
          cHlbl7  = "Utpris"
          cHlbl8  = "Enh.pris"
          cHlbl9  = "Rab"
          cHlblX1 = "Antall".
          cHlblX2 = "Rest".
          cHlblX3 = "Levert".

   CREATE QUERY qH.
   CREATE QUERY qL.
   qL:SET-BUFFERS(hTTLinjeBuff).
   qH:SET-BUFFERS(hTTHodeBuff).
   qH:QUERY-PREPARE("FOR EACH " + hTTHodeBuff:NAME).
   qH:QUERY-OPEN().



   OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
   PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
   PUT CONTROL '<PRINTER' + DYNAMIC-FUNCTION('getRapPrinter':U,cPrinter) + '>'.
   IF lDirekte = FALSE THEN
       PUT CONTROL '<PREVIEW=ZoomToWidth>'.
   PUT UNFORMATTED "<ALIGN=BASE><FArial><UNITS=MM>" SKIP.
   PUT UNFORMATTED "<R14><C40><#1><R18><C75><FRAME#1>" SKIP. /* <15>..<19>*/
   
   qH:GET-FIRST().
   REPEAT WHILE NOT qH:QUERY-OFF-END:
       qL:QUERY-PREPARE("FOR EACH " + hTTLinjeBuff:NAME + " WHERE PkSdlId = " +
                        STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)).

       iBilagstype = 1.
       CASE iBilagstype:
           WHEN 1 THEN ASSIGN cPkSdlNr = IF hTTHodeBuff:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE > 0 THEN 
                                              STRING(hTTHodeBuff:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE) ELSE ""
                              cPkSdlType = STRING(cPkSdlNr <> "","Pakkseddel"). 
           OTHERWISE ASSIGN cPkSdlNr   = ""
                            cPkSdlType = "". 
       END CASE.
       
       cTmpRubrik = cDetaljRad.
       cTmpRubrik = REPLACE(cTmpRubrik,"&10",cHlblX1).
       cTmpRubrik = REPLACE(cTmpRubrik,"&11",cHlblX2).
       cTmpRubrik = REPLACE(cTmpRubrik,"&12",cHlblX3).
       
       DO iCount = 1 TO iAntEks:
           iSidNr = 1.
           RUN SkrivHeader (iSidNr,cPkSdlNr,cPkSdlType,iCount > 1).

           ASSIGN iRadNr     = 20. /* 22 */
           
           PUT UNFORMATTED                                     /* 168,255,168 */
           SUBSTITUTE("<R&1><C6><FROM><R&2><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.

           PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cTmpRubrik,cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) "</B>" SKIP.
           
           IF iCount = 1 THEN
               qL:QUERY-OPEN().
           qL:GET-FIRST().
           
           /* Teller opp antall linjer */
           ASSIGN iAntLinjer = 0.
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN iAntLinjer = iAntLinjer + 1.
               qL:GET-NEXT().
           END.
           
           qL:GET-FIRST().
           REPEAT WHILE NOT qL:QUERY-OFF-END:
               ASSIGN iRadNr     = iRadNr + 1
                      iAntLinjer = iAntLinjer - 1.

               ASSIGN dAntal  = dAntal  + hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE
                      dRest   = dRest   + hTTLinjeBuff:BUFFER-FIELD("AntRest"):BUFFER-VALUE
                      dLevert = dLevert + hTTLinjeBuff:BUFFER-FIELD("AntLevert"):BUFFER-VALUE.

               dTmpInnkj = hTTLinjeBuff:BUFFER-FIELD("AntLevert"):BUFFER-VALUE * hTTLinjeBuff:BUFFER-FIELD("NyVarekost"):BUFFER-VALUE.
               dTmpSalg  = hTTLinjeBuff:BUFFER-FIELD("AntLevert"):BUFFER-VALUE * hTTLinjeBuff:BUFFER-FIELD("NyPris"):BUFFER-VALUE.
               
               dInnkjSum = dInnkjSum + IF dTmpInnkj <> ? THEN dTmpInnkj ELSE 0.
               dSalgsum  = dSalgsum  + IF dTmpSalg <> ? THEN dTmpSalg ELSE 0.

               cTmpDetaljRad = cDetaljRad.
               cTmpDetaljRad = REPLACE(cTmpDetaljRad,"&10",STRING(hTTLinjeBuff:BUFFER-FIELD("Antall"):BUFFER-VALUE,"->,>>>,>>9.99")).
               cTmpDetaljRad = REPLACE(cTmpDetaljRad,"&11",STRING(hTTLinjeBuff:BUFFER-FIELD("AntRest"):BUFFER-VALUE,"->,>>>,>>9.99")).
               cTmpDetaljRad = REPLACE(cTmpDetaljRad,"&12",STRING(hTTLinjeBuff:BUFFER-FIELD("AntLevert"):BUFFER-VALUE,"->,>>>,>>9.99")).

               FIND StrKonv NO-LOCK WHERE
                 StrKonv.StrKode = INTEGER(STRING(hTTLinjeBuff:BUFFER-FIELD("StrKode"):BUFFER-VALUE)) NO-ERROR.                                                             

               PUT UNFORMATTED "<R+.9" SUBSTITUTE(cTmpDetaljRad,
                   STRING(hTTLinjeBuff:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE),
                   "<B>" + hTTLinjeBuff:BUFFER-FIELD("LevKod"):BUFFER-VALUE + "</B>",
                   SUBSTR(hTTLinjeBuff:BUFFER-FIELD("Beskr"):BUFFER-VALUE,1,30),
                   hTTLinjeBuff:BUFFER-FIELD("Farge"):BUFFER-VALUE,
                   (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ''), 
                   STRING(hTTLinjeBuff:BUFFER-FIELD("Salgsenhet"):BUFFER-VALUE),
                   STRING(hTTLinjeBuff:BUFFER-FIELD("NyPris"):BUFFER-VALUE,"->,>>>,>>9.99"),
                   STRING(hTTLinjeBuff:BUFFER-FIELD("NyInnkjopsPris"):BUFFER-VALUE,"->,>>>,>>9.99"),
                   STRING(hTTLinjeBuff:BUFFER-FIELD("NyRab1%"):BUFFER-VALUE,"->>9.99")).
                   
               /* Skall vi gøra sidbryt? */
               IF iRadNr > iKontrollrad AND iAntLinjer > 2  THEN DO:
                   PAGE.
                   ASSIGN iSidNr = iSidNr + 1
                          iRadNr = 21. /* 22 */
                   RUN SkrivHeader (iSidNr,cPkSdlNr,cPkSdlType,iCount > 1).
                   PUT UNFORMATTED                                     /* 168,255,168 */
                   SUBSTITUTE("<R&1><P6><C6><FROM><R&2><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iRadNr),STRING(iRadNr + 1)) SKIP.
/*                    PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cDetaljRad,"Prodnr","Beskr","Levert","Str","Antall","Enhetspris","Rabatt","Mva","Sum") "</B>" SKIP. */
                   PUT UNFORMATTED "<B><R" STRING(iRadNr) SUBSTITUTE(cTmpRubrik,cHlbl1,cHlbl2,cHlbl3,cHlbl4,cHlbl5,cHlbl6,cHlbl7,cHlbl8,cHlbl9) "</B>" SKIP.
               END.
               qL:GET-NEXT().
           END.
           /* Her skrives postgiroblankett ut */
           IF iCount < iAntEks THEN
               PAGE.
       END.
       
       qH:GET-NEXT().
       IF NOT qH:QUERY-OFF-END THEN
           PAGE.
   END.

   ASSIGN cSlbl1 = "Antall"
          cSlbl2 = "Rest"
          cSlbl3 = "Levert"
          cSlbl4 = " "
          cSlbl5 = "Netto levert kr"
          cSlbl6 = " "
          cSlbl7 = "Brutto salgsverdi kr"
          cSlbl8 = " "
          cSlbl9 = " ".
   PUT UNFORMATTED SUBSTITUTE("<R&1.7><P8><C6><FROM><R&2.7><C78><RECT><BGCOLOR=220,220,220><FILLRECT>",STRING(iKontrollrad + iExtraRad + 1),STRING(iKontrollrad + iExtraRad + 2)) SKIP.
   PUT UNFORMATTED "<B>" REPLACE(SUBSTITUTE(cSumRad,cSlbl1,cSlbl2,cSlbl3,cSlbl4,cSlbl5,cSlbl6,cSlbl7,cSlbl8,cSlbl9),"@1",STRING(iKontrollrad + iExtraRad + 1)) "</B>" SKIP.
   PUT UNFORMATTED REPLACE(SUBSTITUTE(cSumRad,
                              STRING(dAntal,"->,>>>,>>9.99"),
                              STRING(dRest,"->,>>>,>>9.99"),
                              STRING(dLevert,"->,>>>,>>9.99"),
                              " ",
                              STRING(dInnkjSum,"->,>>>,>>9.99"),
                              " ",
                              STRING(dSalgsum,"->,>>>,>>9.99"),
                              " " +
                              " "),"@1",STRING(iKontrollrad + iExtraRad + 2)) + "</B>" SKIP.


   qH:QUERY-CLOSE().
   DELETE OBJECT qH.
   qL:QUERY-CLOSE().
   DELETE OBJECT qL.
   IF TRIM(cMailAdress) <> "" THEN DO:
       PUT UNFORMATTED "<MAILTO=TO:" + cMailAdress + ">". 
   END.
   OUTPUT CLOSE.

   RUN VisXprint.p (pcRappFil).
   
   /* Sender pdf. filen på eMail hvis det er satt for det. TN 25/2-19 Gjøres i trigger w_pksdlhode.p */
   IF SEARCH(REPLACE(pcRappFil,"xpr","pdf")) <> ? THEN
   DO:
       PUBLISH 'SendPakkseddel' (REPLACE(pcRappFil,"xpr","pdf")).
   END.
   
/*   OS-DELETE VALUE(pcRappFil).   Avvent litt med å slette denne. */
   
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

