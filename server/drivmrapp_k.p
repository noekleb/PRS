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
DEFINE INPUT  PARAMETER cMailTo AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER dFraDato AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER iFraTid  AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER dTilDato AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER iTilTid  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iButLoop  AS INTEGER     NO-UNDO.

DEFINE VARIABLE lSendEmail  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lMailOK AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cMessage    AS CHARACTER  NO-UNDO.


DEFINE VARIABLE cMailhub     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDoAUTH   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAuthType AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUser     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPassword AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmailCC  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmailFrom AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cTmpFileTOT AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmpFile    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cExcelFilename    AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE tt_Raindance NO-UNDO
    FIELD hg      AS INTE
    FIELD RDkod   AS INTE
    INDEX hg IS PRIMARY UNIQUE hg.

DEFINE TEMP-TABLE TT_Drivrapp NO-UNDO
    FIELD butik AS INTE
    FIELD dato  AS DATE
    FIELD HG    AS INTE
    FIELD HGbeskr AS CHAR
    FIELD RDkod   AS INTE
    FIELD volym AS DECI
    FIELD belopp AS DECI
    FIELD korttyp AS INTE
    FIELD korttext AS CHAR
   INDEX idx IS PRIMARY UNIQUE butik dato HG korttyp.

DEFINE TEMP-TABLE TT_DrivrappTOT NO-UNDO
    FIELD butik AS INTE
    FIELD HG    AS INTE
    FIELD HGbeskr AS CHAR
    FIELD RDkod   AS INTE
    FIELD volym AS DECI
    FIELD belopp AS DECI
    FIELD korttyp AS INTE
    FIELD korttext AS CHAR
    INDEX idx IS PRIMARY UNIQUE butik HG korttyp.


DEF STREAM sExportFile.

{runlib.i}
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getKortnamn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKortnamn Procedure 
FUNCTION getKortnamn RETURNS CHARACTER
      ( INPUT iSubType AS INTEGER )  FORWARD.

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
         HEIGHT             = 14
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* MESSAGE                                */
/*     cButiker                           */
/*     dFraDato                           */
/*     string(iFraTid,"HH:MM:SS")         */
/*     dTilDato                           */
/*     string(iTilTid,"HH:MM:SS")         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*                                        */
/* RETURN.                                */
/* DO  iButloop = 1 TO NUM-ENTRIES(cButiker). */
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
RUN initSmtp.
RUN SkapattRaindance.
FOR EACH butiker NO-LOCK:
    IF butiker.nedlagtdato = ? OR butiker.nedlagtdato > dTilDato THEN DO:
        RUN Data2Flik2 (butiker.butik).
/*         ii = ii + 1. */
    END.
/*     IF ii = 5 THEN */
/*         LEAVE.     */
END.
IF CAN-FIND(FIRST tt_drivrapp) THEN DO:
    /* Summering för perioden */
    RUN Data2Flik1.

    /* Vi sätter alla filnamn här */
    cTmpFileTOT = SESSION:TEMP-DIR + "DrivmedelTOT.tmp".
    IF SEARCH(cTmpFileTOT) <> ? THEN
        OS-DELETE VALUE(cTmpFileTOT).
    cTmpFile = SESSION:TEMP-DIR + "Drivmedel.tmp".
    IF SEARCH(cTmpFile) <> ? THEN
        OS-DELETE VALUE(cTmpFile).
    cExcelFilename = SESSION:TEMP-DIR + "Drivmedelsrapport_1_" + REPLACE(STRING(TODAY),"/","") + ".xlsx".  
    IF SEARCH(cExcelFilename) <> ? THEN
          OS-DELETE VALUE(cExcelFilename).

    RUN Export2Sheet1.
    RUN Export2Sheet2.
    RUN ToExcel (cTmpFileTOT + "," + cTmpFile,"Totalt perioden","Per dag").

/* cMailTo = "ken1@polygonsoftware.no". */
lSendEmail = TRUE.
    IF lSendEmail AND SEARCH(cExcelFilename) <> ? THEN DO:
        RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cMailhub,
        /*EmailTo    */   cMailTo,
        /*EmailFrom  */   cEmailFrom,
        /*EmailCC    */   "",
        /*Attachments*/   ENTRY(NUM-ENTRIES(cExcelFilename,"\"),cExcelFilename,"\"),
        /*LocalFiles */   cExcelFilename,
        /*Subject    */   "Drivmedelsrapport 1",
        /*Body       */   "",
        /*MIMEHeader */   "CharSet=iso8859-1",
        /*BodyType   */   "",
        /*Importance */   0,
        /*L_DoAUTH   */   cDoAUTH,
        /*C_AuthType */   cAuthType,
        /*C_User     */   cUser,
        /*C_Password */   cPassword,
        /*oSuccessful*/  OUTPUT lMailOK,
        /*vMessage   */  OUTPUT cMessage) NO-ERROR.
/*         IF cFileName <> "" THEN         */
/*             OS-DELETE VALUE(cFileName). */
/*         IF lMailOK = FALSE THEN                    */
/*             MESSAGE cMessage                       */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    END.
END.

/*                                  */
/* OUTPUT TO "C:\tmp\drivrapp.txt". */
/* FOR EACH tt_drivrapp.            */
/*     EXPORT tt_drivrapp.          */
/* END.                             */
/* OUTPUT CLOSE.                    */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Data2Flik1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Data2Flik1 Procedure 
PROCEDURE Data2Flik1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH TT_Drivrapp:
    FIND TT_DrivrappTOT WHERE TT_DrivrappTOT.butik = TT_Drivrapp.butik AND 
                              TT_DrivrappTOT.HG    = TT_Drivrapp.HG    AND 
                              TT_DrivrappTOT.korttyp = TT_Drivrapp.korttyp NO-ERROR.
    IF NOT AVAIL TT_DrivrappTOT THEN DO:
        CREATE TT_DrivrappTOT.
        ASSIGN TT_DrivrappTOT.butik    = TT_Drivrapp.butik
               TT_DrivrappTOT.HG       = TT_Drivrapp.HG
               TT_DrivrappTOT.korttyp  = TT_Drivrapp.korttyp
               TT_DrivrappTOT.HGbeskr  = TT_Drivrapp.HGbeskr
               TT_DrivrappTOT.korttext = TT_Drivrapp.korttext
               TT_DrivrappTOT.RDkod    = TT_Drivrapp.RDkod.
    END.
    ASSIGN TT_DrivrappTOT.volym  = TT_DrivrappTOT.volym  + TT_Drivrapp.volym 
           TT_DrivrappTOT.belopp = TT_DrivrappTOT.belopp + TT_Drivrapp.belopp.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Data2Flik2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Data2Flik2 Procedure 
PROCEDURE Data2Flik2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iButik AS INTEGER     NO-UNDO.
DEFINE VARIABLE dDatum AS DATE        NO-UNDO.
DEFINE VARIABLE dSalgssumTmp AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iKoeff AS INTEGER     NO-UNDO.
DEFINE VARIABLE iKortTyp AS INTEGER     NO-UNDO.
DEFINE BUFFER kort_bonglinje FOR bonglinje.
FOR EACH kasse WHERE kasse.butikknr = iButik NO-LOCK:
    DO dDatum = dFraDato TO dTilDato:
        FOR EACH bonghode WHERE bonghode.butikknr = iButik AND                                                                    
                                bonghode.gruppe   = 1      AND
                                bonghode.kassenr  = kasse.kassenr AND
                                bonghode.dato     = dDatum NO-LOCK:                                                               
            IF Bonghode.makulert = 2 THEN                                                                                         
                NEXT.                                                                                                             
            IF bonghode.dato = dFraDato AND bonghode.tid < iFraTid THEN                                                           
                NEXT.                                                                                                             
            IF bonghode.dato = dTilDato AND bonghode.tid > iTilTid THEN                                                           
                NEXT.                                                                                                             
            FOR EACH Bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK BY bonglinje.TTId:
                IF CAN-DO("1,10",STRING(BongLinje.TTId)) AND BongLinje.Antall <> 0 AND Bonglinje.Makulert = FALSE THEN SALG: DO:
                    FIND Artbas WHERE ArtBas.ArtikkelNr = DECI(Bonglinje.Artikkelnr) NO-LOCK NO-ERROR.
                    FIND HuvGr WHERE HuvGr.Hg = Bonglinje.HovedGr NO-LOCK NO-ERROR.
                    IF AVAIL HuvGr AND HuvGr.Avdelingnr = 1 OR AVAIL HuvGr AND Bonglinje.varegr = 8398 THEN prKDData: DO:
                        ASSIGN iKoeff       = IF BongLinje.Antall > 0 THEN 1 ELSE -1
                               dSalgssumTmp = (BongLinje.LinjeSum - BongLinje.Linjerab - BongLinje.SubtotalRab) * iKoeff.
                        
                        FIND FIRST kort_bonglinje WHERE kort_bonglinje.b_id = bonghode.b_id AND kort_bonglinje.ttid = 52 NO-LOCK NO-ERROR.
                        IF NOT AVAIL kort_bonglinje THEN DO:
                            FIND FIRST kort_bonglinje WHERE kort_bonglinje.b_id = bonghode.b_id AND kort_bonglinje.ttid = 58 NO-LOCK NO-ERROR.
                        END.
                        ASSIGN iKortTyp = IF AVAIL kort_bonglinje THEN INT(kort_bonglinje.antall) ELSE 0.
                        FIND TT_Drivrapp WHERE tt_drivrapp.butik = bonglinje.butik AND
                                               tt_drivrapp.dato  = bonglinje.dato  AND
                                               tt_drivrapp.HG    = bonglinje.hovedgr AND 
                                               tt_drivrapp.korttyp = iKortTyp NO-ERROR.
                        IF NOT AVAIL tt_drivrapp THEN DO:
                            FIND tt_Raindance WHERE tt_Raindance.hg = bonglinje.hovedgr NO-ERROR.
                            CREATE tt_drivrapp.
                            ASSIGN tt_drivrapp.butik = bonglinje.butik
                                   tt_drivrapp.dato  = bonglinje.dato
                                   tt_drivrapp.HG    = bonglinje.hovedgr
                                   tt_drivrapp.korttyp = iKorttyp
                                   tt_drivrapp.korttext = DYNAMIC-FUNCTION('getKortnamn',iKorttyp)
                                   tt_drivrapp.HGbeskr = TRIM(huvgr.hgbeskr)
                                   tt_drivrapp.RDkod = IF AVAIL tt_Raindance THEN tt_Raindance.RDkod ELSE 999.
                        END.
                        ASSIGN tt_drivrapp.volym     = tt_drivrapp.volym + bonglinje.antall
                               tt_drivrapp.belopp    = tt_drivrapp.belopp + dSalgssumTmp.
                    END.
                END. /* SALG: */
                ELSE IF CAN-DO("52,58",STRING(BongLinje.TTId)) AND Bonglinje.Makulert = FALSE THEN SALG: DO:
                    FIND TT_Drivrapp WHERE tt_drivrapp.butik = bonglinje.butik AND
                                           tt_drivrapp.dato  = bonglinje.dato  AND
                                           tt_drivrapp.HG    = 999 AND 
                                           tt_drivrapp.korttyp = 0 NO-ERROR.
                    IF NOT AVAIL tt_drivrapp THEN DO:
                        FIND tt_Raindance WHERE tt_Raindance.hg = bonglinje.hovedgr NO-ERROR.
                        CREATE tt_drivrapp.
                        ASSIGN tt_drivrapp.butik = bonglinje.butik
                               tt_drivrapp.dato  = bonglinje.dato
                               tt_drivrapp.HG    = 999
                               tt_drivrapp.korttyp = 0
                               tt_drivrapp.HGbeskr = "Totalt kort"
                               tt_drivrapp.RDkod = IF AVAIL tt_Raindance THEN tt_Raindance.RDkod ELSE 999.
                    END.
                    ASSIGN tt_drivrapp.belopp    = tt_drivrapp.belopp + bonglinje.linjesum.
                END.
            END. /* Bonglinjer slut */
        END.
    END.
END.



/* 
DEFINE TEMP-TABLE TT_Drivrapp NO-UNDO
    FIELD butik AS INTE
    FIELD dato  AS DATE
    FIELD HG    AS INTE
    FIELD volym AS DECI
    FIELD belopp AS DECI
    INDEX idx IS PRIMARY UNIQUE butik dato typ. 
 
 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Export2Sheet1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Export2Sheet1 Procedure 
PROCEDURE Export2Sheet1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cOrgDateFormat AS CHARACTER   NO-UNDO.

  cOrgDateFormat = SESSION:DATE-FORMAT.
  SESSION:DATE-FORMAT = "ymd".
  OUTPUT STREAM sExportFile TO VALUE(cTmpFileTOT) NO-ECHO.
  EXPORT STREAM sExportFile DELIMITER ";"
      "DRIVMEDELSRAPPORT 1 totalt för perioden" SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
      /* A  */ "Station"          
      /* B  */ "Typ"                 
      /* C  */ "Beskr"        
      /* D  */ "RDkod"
      /* E  */ "Volym"               
      /* F  */ "Sum"  
      /* G  */ "KortNr"               
      /* H  */ "Korttyp"  SKIP.                                 
    EKSPORT:
    FOR EACH TT_DrivrappTOT:
        EXPORT STREAM sExportFile DELIMITER ";"
            /* A  */ TT_DrivrappTOT.butik  
                     TT_DrivrappTOT.hg     
                     TT_DrivrappTOT.hgbeskr
                     TT_DrivrappTOT.RDkod
                     TT_DrivrappTOT.volym  
                     TT_DrivrappTOT.belopp
                     TT_DrivrappTOT.korttyp
                     TT_DrivrappTOT.korttext SKIP   
            .
    END.
    OUTPUT STREAM sExportFile CLOSE.
  SESSION:DATE-FORMAT = cOrgDateFormat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Export2Sheet2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Export2Sheet2 Procedure 
PROCEDURE Export2Sheet2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cOrgDateFormat AS CHARACTER   NO-UNDO.

  cOrgDateFormat = SESSION:DATE-FORMAT.
  SESSION:DATE-FORMAT = "ymd".
  OUTPUT STREAM sExportFile TO VALUE(cTmpFile) NO-ECHO.
  
  /* Legger ut overskrifter. */
/*   STATUS DEFAULT "Eksporterer data...". */
  EXPORT STREAM sExportFile DELIMITER ";"
    "DRIVMEDELSRAPPORT 1 " + STRING(dFraDato) + " " + STRING(iFraTid,"HH:MM") + " - " + STRING(dTilDato) + " " + STRING(iTilTid,"HH:MM") SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    /* A  */ "Station"          
    /* B  */ "Datum"              
    /* C  */ "Typ"                 
    /* D  */ "Beskr"        
    /* E  */ "RDkod"
    /* F  */ "Volym"               
    /* G  */ "Sum"              
    /* H  */ "KortNr"               
    /* I  */ "Korttyp"  SKIP.                                 
  EKSPORT:
  FOR EACH TT_Drivrapp:
      EXPORT STREAM sExportFile DELIMITER ";"
          /* A  */ TT_Drivrapp.butik  
                   TT_Drivrapp.dato   
                   TT_Drivrapp.hg     
                   TT_Drivrapp.hgbeskr
                   TT_Drivrapp.RDkod
                   TT_Drivrapp.volym  
                   TT_Drivrapp.belopp
                   TT_Drivrapp.korttyp
                   TT_Drivrapp.korttext SKIP.
  END.
  OUTPUT STREAM sExportFile CLOSE.
  SESSION:DATE-FORMAT = cOrgDateFormat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initSmtp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initSmtp Procedure 
PROCEDURE initSmtp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {syspara.i 50 50 1 cMailhub }
    {syspara.i 50 50 2 cDoAUTH  }
    {syspara.i 50 50 3 cAuthType}
    {syspara.i 50 50 4 cUser    }
    {syspara.i 50 50 5 cPassword}
/*     {syspar2.i 50 50 20 cEmailCC} */
    {syspara.i 50 50 40 cEmailFrom}
    IF cDoAUTH = "0" THEN
        ASSIGN cDoAUTH   = "FALSE"
               cAuthType = ""
               cUser     = ""
               cPassword = "".
    ELSE
        cDoAUTH = "TRUE".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-loadFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadFile Procedure 
PROCEDURE loadFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAM icFileName  AS CHAR NO-UNDO.
    DEF INPUT PARAM icSheetName AS CHAR NO-UNDO.
    DEFINE INPUT  PARAMETER iFliknr AS INTEGER     NO-UNDO.

    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iSheets AS INTEGER     NO-UNDO.

    DEF VAR cColValue    AS CHAR NO-UNDO.

    DEF VAR cRange       AS CHAR NO-UNDO.
    DEF VAR cFirstLetter AS CHAR NO-UNDO.
    DEF VAR cLastLetter  AS CHAR NO-UNDO.
    /* 6 betyder att excel skall se i fält 10 för separator */
    chExcelApplication:Workbooks:OPEN(icFileName,2,FALSE,6,,,,,";",,,,,,).

    ASSIGN
        chExcelApplication:ActiveSheet:NAME = icSheetName
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
/*         MESSAGE 'Feil i navn, meld feil til support ' SKIP icSheetName SKIP LENGTH(icSheetName) SKIP icFileName */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                  */
/*       ASSIGN                                                                                                    */
/*         icSheetName = ENTRY(1,icSheetName,' ')                                                                  */
/*         chExcelApplication:ActiveSheet:NAME = icSheetName                                                       */
/*       .                                                                                                         */
    END.
    /*Remove the 3 startup sheets that is added default */
    DO ii = 1 TO chExcelApplication:Sheets:COUNT:
      IF chExcelApplication:Sheets(ii):NAME BEGINS 'Sheet' THEN chExcelApplication:Sheets(ii):DELETE.
    END.
    ASSIGN
      iSheets = chExcelApplication:Workbooks(chExcelApplication:Workbooks:COUNT - 1):Sheets:COUNT
      iSheets = IF iSheets LE 0 THEN 1 ELSE iSheets
      NO-ERROR.

    IF chExcelApplication:Workbooks:COUNT GT 1 THEN
      chExcelApplication:ActiveSheet:MOVE(,chExcelApplication:Workbooks(chExcelApplication:Workbooks:COUNT - 1):Sheets(iSheets)).

    /*Formatering...*/
/*       chExcelApplication:ActiveSheet:PageSetup:Orientation     = 1. */
      chExcelApplication:ActiveSheet:Rows("3:3"):SELECT().
      chExcelApplication:ActiveWindow:FreezePanes = TRUE.
      IF iFliknr = 1 THEN DO: /* TOTRAPPORT 6 cols */
          chExcelApplication:ActiveSheet:Range("A1:G1"):Font:Bold = TRUE.
          chExcelApplication:ActiveSheet:Range("A1:G1"):Font:Italic = TRUE.
          chExcelApplication:ActiveSheet:Range("A2:H2"):Font:Bold = TRUE. 
          chExcelApplication:ActiveSheet:Range("A2:H2"):Font:Italic = TRUE.
          chExcelApplication:ActiveSheet:Range("A:A"):NumberFormat = "###0".
          chExcelApplication:ActiveSheet:Range("E:F"):NumberFormat = "# ##0,00".
          chExcelApplication:ActiveSheet:Range("A1:G1"):Merge().
          chExcelApplication:ActiveSheet:Columns("A:H"):AutoFit().
      END.
      ELSE IF iFliknr = 2 THEN DO: /* 7 cols  2=datum,*/
          chExcelApplication:Range("A1:G1"):Font:Bold = TRUE.
          chExcelApplication:Range("A1:G1"):Font:Italic = TRUE.
          chExcelApplication:Range("A2:I2"):Font:Bold = TRUE. 
          chExcelApplication:Range("A2:I2"):Font:Italic = TRUE.
          chExcelApplication:Range("A:A"):NumberFormat = "###0".
          chExcelApplication:Range("B:B"):NumberFormat = "ÅÅ-MM-DD".
          chExcelApplication:Range("F:G"):NumberFormat = "# ##0,00".
          chExcelApplication:Range("A1:G1"):Merge().
          chExcelApplication:Columns("A:I"):AutoFit().
      END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapattRaindance) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapattRaindance Procedure 
PROCEDURE SkapattRaindance :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTst AS INTEGER     NO-UNDO.
    FOR EACH syspara WHERE SysPara.SysHId = 210 AND 
                           SysPara.SysGr  = 271 NO-LOCK.
        iTst = INT(SysPara.Parameter1) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR AND iTst > 0 THEN DO:
            CREATE tt_Raindance.
            ASSIGN tt_Raindance.hg = SysPara.Paranr
                   tt_Raindance.RDkod = iTst NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE tt_Raindance.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToExcel Procedure 
PROCEDURE ToExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFilelist AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER cFliknamn_1 AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER cFliknamn_2 AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cFiles AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cSheetName AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cSheetFileName AS CHARACTER   NO-UNDO.


  CREATE "Excel.Application" chExcelApplication.
  ASSIGN
    chExcelApplication:VISIBLE       = FALSE
    chExcelApplication:DisplayAlerts = NO
  .

  DO ii = 1 TO NUM-ENTRIES(cFilelist): /*pr butik*/

    /*Må sjekke at det ikke blir flere tegn enn 32 (begrensning i excel*/
      ASSIGN cSheetName = IF ii = 1 THEN "Totalt perioden" ELSE "Per dag"
             cSheetFileName = ENTRY(ii,cFilelist).

      RUN loadFile(cSheetFileName,cSheetName,ii).
  END.
  chExcelApplication:Sheets(cFliknamn_1):SELECT().
  chExcelApplication:ActiveWorkbook:SaveAs(cExcelFilename,51,,,,,,,,,,).
  chExcelApplication:QUIT.
  RELEASE OBJECT chExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getKortnamn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKortnamn Procedure 
FUNCTION getKortnamn RETURNS CHARACTER
      ( INPUT iSubType AS INTEGER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
       DEFINE VARIABLE cSubtypeName AS CHARACTER  NO-UNDO.
       DEFINE VARIABLE cSubTypeNr   AS CHARACTER  NO-UNDO.
       DEFINE VARIABLE cReturNamn   AS CHARACTER  NO-UNDO.
       ASSIGN cSubtypeNr   = "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25"
       cSubtypename = "PREEM,SIV,PREEM SÅIFA,LOGPAY,UNO-X NO,UNO-X DK,RETAIN24,VOLVO,NESTE,DKV,PREEM FTG,FUNDINS,BANKKORT,VAKANT,VAKANT,VAKANT,UTA,BONUSKORT,OKDK,E100,EDC,TP24,ARIS,Eurowag,".
/*        cSubtypename = "PREEM,SIV,PREEM SÅIFA,LOGPAY,UNO-X NO,UNO-X DK,RETAIN24,VOLVO,NESTE,DKV,PREEM FTG,VAKANT,BANKKORT,VAKANT,VAKANT,VAKANT,UTA,BONUSKORT,OKDK,E100,EDC,TP24,ARIS,Eurowag,". */
/*        "PREEM,VAKANT,PREEM SÅIFA,LOGPAY,UNO-X NO,UNO-X DK,VAKANT,VOLVO,NESTE,DKV,VAKANT,VAKANT,BANKKORT,VAKANT,VAKANT,VAKANT,UTA,BONUSKOR,OKDK,E100,EDC,TP24,ARIS,Eurowag,". */
       /* "PREEM,PREEM VISA,SÅIFA,TEPAR,HY/TEX NO,HY/TEX DK,SAAB/OPEL,VOLVO,NESTE,DKV,OK,UNO-X,BANKKORT,AMEX,DINERS,FINAX,UTA,BONUSKORT,CAMPING,,,,,,". */
       IF iSubType = 0 THEN
           ASSIGN cReturNamn = "ÖVRIGT".
       ELSE IF CAN-DO(cSubtypeNr,STRING(iSubType)) THEN
           ASSIGN cReturNamn = ENTRY(iSubType,cSubTypename).
       IF cReturNamn = "" THEN
           ASSIGN cReturNamn = "OKÄNT".
       RETURN cReturNamn.   /* Function return value. */

    /* 1   PREEM      PREEM      */
    /* 2   PREEM VISA VAKANT     */
    /* 3   SÅIFA      PREEM SÅIFA*/
    /* 4   TEPAR      LOGPAY     */
    /* 5   HY/TEX NO  UNO-X NO   */
    /* 6   HY/TEX DK  UNO-X DK   */
    /* 7   SAAB/OPEL  VAKANT     */
    /* 8   VOLVO      VOLVO      */
    /* 9   NESTE      NESTE      */
    /* 10  DKV        DKV        */
    /* 11  OK         VAKANT     */
    /* 12  UNO-X      VAKANT     */
    /* 13  BANKKORT   BANKKORT   */
    /* 14  AMEX       VAKANT     */
    /* 15  DINERS     VAKANT     */
    /* 16  FINAX      VAKANT     */
    /* 17  UTA        UTA        */
    /* 18  BONUSKORT  BONUSKORT  */
    /* 19  CAMPING    OKDK       */
    /* 20              E100      */
    /* 21              EDC       */
    /* 22             TP24       */
    /* 23              ARIS      */
    /* 24             Eurowag    */
    /* 25                        */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

