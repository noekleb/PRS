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
DEFINE INPUT  PARAMETER iRapportTyp AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER dFraDato AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER dTilDato AS DATE        NO-UNDO.
DEFINE VARIABLE iButLoop  AS INTEGER     NO-UNDO.

DEFINE VARIABLE lSendEmail  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lMailOK AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cMessage    AS CHARACTER  NO-UNDO.

/* DEFINE VARIABLE cFilename  AS CHARACTER   NO-UNDO. */
/* DEFINE VARIABLE cFilename2 AS CHARACTER   NO-UNDO. */
DEFINE VARIABLE cMailhub   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDoAUTH    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAuthType  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUser      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPassword  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmailCC   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmailFrom AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cTmpFile23    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmpFile23TOT AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmpFile32    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmpFile32TOT AS CHARACTER   NO-UNDO.


DEFINE VARIABLE cExcelFilename23    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cExcelFilename32    AS CHARACTER   NO-UNDO.


DEFINE TEMP-TABLE tt_Raindance NO-UNDO
    FIELD hg      AS INTE
    FIELD RDkod   AS INTE
    INDEX hg IS PRIMARY UNIQUE hg.

DEFINE TEMP-TABLE TT_Drivrapp23_1 NO-UNDO
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

DEFINE TEMP-TABLE TT_Drivrapp23_1TOT NO-UNDO
    FIELD butik AS INTE
    FIELD HG    AS INTE
    FIELD HGbeskr AS CHAR
    FIELD RDkod   AS INTE
    FIELD volym AS DECI
    FIELD belopp AS DECI
    FIELD korttyp AS INTE
    FIELD korttext AS CHAR
    INDEX idx IS PRIMARY UNIQUE butik HG korttyp.


DEFINE TEMP-TABLE TT_Drivrapp23_2 NO-UNDO
    FIELD butik AS INTE
    FIELD dato  AS DATE
    FIELD HG    AS INTE
    FIELD HGbeskr AS CHAR
    FIELD RDkod   AS INTE
    FIELD volym AS DECI
    FIELD belopp AS DECI
    FIELD korttyp AS INTE
    FIELD korttext AS CHAR
    FIELD bemannade AS LOG
    INDEX idx IS PRIMARY UNIQUE butik dato HG korttyp.

DEFINE TEMP-TABLE TT_Drivrapp23_2TOT NO-UNDO
    FIELD butik AS INTE
    FIELD HG    AS INTE
    FIELD HGbeskr AS CHAR
    FIELD RDkod   AS INTE
    FIELD volym AS DECI
    FIELD belopp AS DECI
    FIELD korttyp AS INTE
    FIELD korttext AS CHAR
    FIELD bemannade AS LOG
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
         HEIGHT             = 14.38
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
RUN initSmtp.
RUN SkapattRaindance.

/* FOR EACH butiker WHERE butiker.butik = 12080 NO-LOCK: */
/* DEFINE VARIABLE ii AS INTEGER     NO-UNDO. */
FOR EACH butiker NO-LOCK:
    IF butiker.nedlagtdato = ? OR butiker.nedlagtdato > dTilDato THEN
        RUN Data2Flik2_23 (butiker.butik). /* Här byggs data i tabell för rapporttyp 2 o 3_1. Dom är samma tabell.*/
    /* ii = ii + 1. */                     /* I tillägg skapas också data för rapport  3_2.                       */
/*     IF ii = 10 THEN */
/*         LEAVE.      */
END.
IF CAN-FIND(FIRST TT_Drivrapp23_1) THEN DO: /* Om vi har data iden så inns också data i TT_Drivrapp23_1 */
    
    /* Vi sätter alla filnamn här */
    cTmpFile23TOT = SESSION:TEMP-DIR + "Drivmedel23TOT.tmp".
    IF SEARCH(cTmpFile23TOT) <> ? THEN
        OS-DELETE VALUE(cTmpFile23TOT).
    cTmpFile23 = SESSION:TEMP-DIR + "Drivmedel23.tmp".
    IF SEARCH(cTmpFile23) <> ? THEN
        OS-DELETE VALUE(cTmpFile23).

    IF iRapporttyp = 2 THEN DO:
        cExcelFilename23 = SESSION:TEMP-DIR + "Drivmedelsrapport2_" + REPLACE(STRING(TODAY),"/","") + ".xlsx".  
    END.
    ELSE DO:
        cExcelFilename23 = SESSION:TEMP-DIR + "Drivmedelsrapport3_1" + REPLACE(STRING(TODAY),"/","") + ".xlsx".  
        cExcelFilename32 = SESSION:TEMP-DIR + "Drivmedelsrapport3_2" + REPLACE(STRING(TODAY),"/","") + ".xlsx".  
        IF SEARCH(cExcelFilename32) <> ? THEN
           OS-DELETE VALUE(cExcelFilename32).
        cTmpFile32TOT = SESSION:TEMP-DIR + "Drivmedel32TOT.tmp".
        IF SEARCH(cTmpFile32TOT) <> ? THEN
            OS-DELETE VALUE(cTmpFile32TOT).
        cTmpFile32 = SESSION:TEMP-DIR + "Drivmedel32.tmp".
        IF SEARCH(cTmpFile32) <> ? THEN
            OS-DELETE VALUE(cTmpFile32).
    END.
    IF SEARCH(cExcelFilename23) <> ? THEN
       OS-DELETE VALUE(cExcelFilename23).

    RUN Data2Flik1_23_1.
    /* för enkelheten skull delar vi upp */
    RUN Export2Sheet1_23(cTmpFile23TOT).
    RUN Export2Sheet2_23(cTmpFile23).
    RUN ToExcel_23(cTmpFile23TOT + "," + cTmpFile23,"Totalt perioden","Per dag",cExcelFilename23).
    IF iRapportTyp = 3 THEN DO:
        RUN Data2Flik1_3_2.
        RUN Export2Sheet1_32(cTmpFile32TOT).
        RUN Export2Sheet2_32(cTmpFile32).
        RUN ToExcel_32(cTmpFile32TOT + "," + cTmpFile32,"Totalt perioden","Per dag",cExcelFilename32).
    END.
END.

/* cMailTo = "ken1@polygonsoftware.no". */
lSendEmail = TRUE.

    IF lSendEmail AND SEARCH(cExcelFilename23) <> ? THEN DO:
        RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cMailhub,
        /*EmailTo    */   cMailTo,
        /*EmailFrom  */   cEmailFrom,
        /*EmailCC    */   "",
        /*Attachments*/   ENTRY(NUM-ENTRIES(cExcelFilename23,"\"),cExcelFilename23,"\"),
        /*LocalFiles */   cExcelFilename23,
        /*Subject    */   "Drivmedelsrapport" + STRING(iRapportTyp) + (IF iRapportTyp = 3 THEN "_1" ELSE ""),
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

    IF lSendEmail AND cExcelFilename32 <> "" AND SEARCH(cExcelFilename32) <> ? THEN DO:
        RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cMailhub,
        /*EmailTo    */   cMailTo,
        /*EmailFrom  */   cEmailFrom,
        /*EmailCC    */   "",
        /*Attachments*/   ENTRY(NUM-ENTRIES(cExcelFilename32,"\"),cExcelFilename32,"\"),
        /*LocalFiles */   cExcelFilename32,
        /*Subject    */   "Drivmedelsrapport " + STRING(iRapportTyp) + "_2",
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
/*         IF cFileName <> "" THEN          */
/*             OS-DELETE VALUE(cFileName2). */
/*         IF lMailOK = FALSE THEN                    */
/*             MESSAGE cMessage                       */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
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

&IF DEFINED(EXCLUDE-ButData23Org) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButData23Org Procedure 
PROCEDURE ButData23Org :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iButik AS INTEGER     NO-UNDO.
DEFINE VARIABLE dDatum AS DATE        NO-UNDO.
DEFINE VARIABLE dSalgssumTmp AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iKoeff AS INTEGER     NO-UNDO.
DEFINE BUFFER kort_bonglinje FOR bonglinje.
DO dDatum = dFraDato TO dTilDato:
    FOR EACH Bokforingsdag WHERE bokforingsdag.butikknr = butiker.butik AND
                                 bokforingsdag.dato     = dDatum        AND
                                 bokforingsdag.pfflagg  = 3 NO-LOCK.
        FOR EACH skift WHERE skift.bokforingsid = bokforingsdag.bokforingsid NO-LOCK:
            IF skift.terminert = FALSE THEN
                NEXT.
            IF skift.butikknr <> bokforingsdag.butikknr THEN /* troligtvis onödigt */
                NEXT.
            FOR EACH bonghode WHERE bonghode.skiftid = skift.skiftid NO-LOCK:                                                               
                IF Bonghode.makulert = 2 THEN                                                                                         
                    NEXT.                                                                                                             
                FOR EACH Bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK BY bonglinje.TTId:
                    IF CAN-DO("1,10",STRING(BongLinje.TTId)) AND BongLinje.Antall <> 0 AND Bonglinje.Makulert = FALSE THEN SALG: DO:
                        FIND Artbas WHERE ArtBas.ArtikkelNr = DECI(Bonglinje.Artikkelnr) NO-LOCK NO-ERROR.
                        FIND HuvGr WHERE HuvGr.Hg = Bonglinje.HovedGr NO-LOCK NO-ERROR.
                        IF AVAIL HuvGr AND HuvGr.Avdelingnr = 1 THEN prKDData: DO:
                            ASSIGN iKoeff       = IF BongLinje.Antall > 0 THEN 1 ELSE -1
                                   dSalgssumTmp = (BongLinje.LinjeSum - BongLinje.Linjerab - BongLinje.SubtotalRab) * iKoeff.
                            FIND TT_Drivrapp23_1 WHERE TT_Drivrapp23_1.butik = bonglinje.butik AND
                                                   TT_Drivrapp23_1.dato  = bonglinje.dato  AND
                                                   TT_Drivrapp23_1.HG    = bonglinje.hovedgr NO-ERROR.
                            IF NOT AVAIL TT_Drivrapp23_1 THEN DO:
                                FIND tt_Raindance WHERE tt_Raindance.hg = bonglinje.hovedgr NO-ERROR.
                                CREATE TT_Drivrapp23_1.
                                ASSIGN TT_Drivrapp23_1.butik = bonglinje.butik
                                       TT_Drivrapp23_1.dato  = bonglinje.dato
                                       TT_Drivrapp23_1.HG    = bonglinje.hovedgr
                                       TT_Drivrapp23_1.HGbeskr = TRIM(huvgr.hgbeskr)
                                       TT_Drivrapp23_1.RDkod = IF AVAIL tt_Raindance THEN tt_Raindance.RDkod ELSE 999.
                            END.
                            ASSIGN TT_Drivrapp23_1.volym     = TT_Drivrapp23_1.volym + bonglinje.antall
                                   TT_Drivrapp23_1.belopp    = TT_Drivrapp23_1.belopp + dSalgssumTmp.
                            IF iRapportTyp = 3 THEN DO:
                                FIND FIRST kort_bonglinje WHERE kort_bonglinje.b_id = bonghode.b_id AND kort_bonglinje.ttid = 52 NO-LOCK NO-ERROR.
                                IF NOT AVAIL kort_bonglinje THEN DO:
                                    FIND FIRST kort_bonglinje WHERE kort_bonglinje.b_id = bonghode.b_id AND kort_bonglinje.ttid = 58 NO-LOCK NO-ERROR.
                                END.
                                IF AVAIL kort_bonglinje THEN DO:
                                    FIND TT_Drivrapp23_2 WHERE TT_Drivrapp23_2.butik   = bonglinje.butik AND
                                                               TT_Drivrapp23_2.dato    = bonglinje.dato  AND
                                                               TT_Drivrapp23_2.HG      = bonglinje.hovedgr AND 
                                                               TT_Drivrapp23_2.korttyp = INT(kort_bonglinje.antall) NO-ERROR.
                                    IF NOT AVAIL TT_Drivrapp23_2 THEN DO:
                                        FIND tt_Raindance WHERE tt_Raindance.hg = bonglinje.hovedgr NO-ERROR.
                                        CREATE TT_Drivrapp23_2.
                                        ASSIGN TT_Drivrapp23_2.butik    = bonglinje.butik
                                               TT_Drivrapp23_2.dato     = bonglinje.dato
                                               TT_Drivrapp23_2.HG       = bonglinje.hovedgr
                                               TT_Drivrapp23_2.korttyp  = INT(kort_bonglinje.antall)
                                               TT_Drivrapp23_2.HGbeskr  = TRIM(huvgr.hgbeskr)
                                               TT_Drivrapp23_2.RDkod    = IF AVAIL tt_Raindance THEN tt_Raindance.RDkod ELSE 999
                                               TT_Drivrapp23_2.korttext = DYNAMIC-FUNCTION('getKortnamn',kort_bonglinje.antall).
                                    END.
                                    ASSIGN TT_Drivrapp23_2.volym     = TT_Drivrapp23_2.volym + bonglinje.antall
                                           TT_Drivrapp23_2.belopp    = TT_Drivrapp23_2.belopp + dSalgssumTmp.
                                END.
                            END.
                        END.
                    END. /* SALG: */
                END. /* Bonglinjer slut */
            END.
        END.
    END.
END.
FOR EACH kasse WHERE kasse.butikknr = iButik NO-LOCK:
    DO dDatum = dFraDato TO dTilDato:
    END.
END.



/* 
DEFINE TEMP-TABLE TT_Drivrapp23_1 NO-UNDO
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

&IF DEFINED(EXCLUDE-Data2Flik1_23_1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Data2Flik1_23_1 Procedure 
PROCEDURE Data2Flik1_23_1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH TT_Drivrapp23_1:
    FIND TT_Drivrapp23_1TOT WHERE TT_Drivrapp23_1TOT.butik = TT_Drivrapp23_1.butik AND 
                                  TT_Drivrapp23_1TOT.HG    = TT_Drivrapp23_1.HG AND
                                  TT_Drivrapp23_1TOT.korttyp = TT_Drivrapp23_1.korttyp NO-ERROR.
    IF NOT AVAIL TT_Drivrapp23_1TOT THEN DO:
        CREATE TT_Drivrapp23_1TOT.
        ASSIGN TT_Drivrapp23_1TOT.butik   = TT_Drivrapp23_1.butik
               TT_Drivrapp23_1TOT.HG      = TT_Drivrapp23_1.HG
               TT_Drivrapp23_1TOT.HGbeskr = TT_Drivrapp23_1.HGbeskr
               TT_Drivrapp23_1TOT.RDkod   = TT_Drivrapp23_1.RDkod
               TT_Drivrapp23_1TOT.korttyp = TT_Drivrapp23_1.korttyp
               TT_Drivrapp23_1TOT.korttext = TT_Drivrapp23_1.korttext.
    END.
    ASSIGN TT_Drivrapp23_1TOT.volym  = TT_Drivrapp23_1TOT.volym  + TT_Drivrapp23_1.volym 
           TT_Drivrapp23_1TOT.belopp = TT_Drivrapp23_1TOT.belopp + TT_Drivrapp23_1.belopp.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Data2Flik1_3_2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Data2Flik1_3_2 Procedure 
PROCEDURE Data2Flik1_3_2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH TT_Drivrapp23_2:
    FIND TT_Drivrapp23_2TOT WHERE TT_Drivrapp23_2TOT.butik   = TT_Drivrapp23_2.butik   AND 
                                  TT_Drivrapp23_2TOT.HG      = TT_Drivrapp23_2.HG      AND 
                                  TT_Drivrapp23_2TOT.korttyp = TT_Drivrapp23_2.korttyp NO-ERROR.
    IF NOT AVAIL TT_Drivrapp23_2TOT THEN DO:
        CREATE TT_Drivrapp23_2TOT.
        ASSIGN TT_Drivrapp23_2TOT.butik    = TT_Drivrapp23_2.butik
               TT_Drivrapp23_2TOT.HG       = TT_Drivrapp23_2.HG
               TT_Drivrapp23_2TOT.HGbeskr  = TT_Drivrapp23_2.HGbeskr
               TT_Drivrapp23_2TOT.RDkod    = TT_Drivrapp23_2.RDkod
               TT_Drivrapp23_2TOT.korttyp  = TT_Drivrapp23_2.korttyp
               TT_Drivrapp23_2TOT.korttext = TT_Drivrapp23_2.korttext.
    END.
    ASSIGN TT_Drivrapp23_2TOT.volym  = TT_Drivrapp23_2TOT.volym  + TT_Drivrapp23_2.volym 
           TT_Drivrapp23_2TOT.belopp = TT_Drivrapp23_2TOT.belopp + TT_Drivrapp23_2.belopp.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Data2Flik2_23) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Data2Flik2_23 Procedure 
PROCEDURE Data2Flik2_23 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iButik AS INTEGER     NO-UNDO.
DEFINE VARIABLE dDatum AS DATE        NO-UNDO.
DEFINE VARIABLE dSalgssumTmp AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iKoeff AS INTEGER     NO-UNDO.
DEFINE VARIABLE iKorttyp AS INTEGER     NO-UNDO.
DEFINE BUFFER kort_bonglinje FOR bonglinje.

/* Vi skall inte summera för automatstationer vid rapport 2 */
/* vi skall heller inte summera för rapport 3 del 1, men den måste vi ta längre ner */
IF iRapportTyp = 2 AND butiker.EODrapport = FALSE THEN
    RETURN.

DO dDatum = dFraDato TO dTilDato:
    FOR EACH Bokforingsdag WHERE bokforingsdag.butikknr = butiker.butik AND
                                 bokforingsdag.dato     = dDatum        AND
                                 bokforingsdag.pfflagg  = 3 NO-LOCK.
        FOR EACH skift WHERE skift.bokforingsid = bokforingsdag.bokforingsid NO-LOCK:
            IF skift.terminert = FALSE THEN
                NEXT.
            IF skift.butikknr <> bokforingsdag.butikknr THEN /* troligtvis onödigt */
                NEXT.
            FOR EACH bonghode WHERE bonghode.skiftid = skift.skiftid NO-LOCK:                                                               
                IF Bonghode.makulert = 2 THEN                                                                                         
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
                            IF butiker.EODrapport = TRUE THEN DO:
                                iKorttyp = IF AVAIL kort_bonglinje THEN INT(kort_bonglinje.antall) ELSE 0.
                                FIND TT_Drivrapp23_1 WHERE TT_Drivrapp23_1.butik = bonglinje.butik AND
                                                       TT_Drivrapp23_1.dato  = bokforingsdag.dato  AND
                                                       TT_Drivrapp23_1.HG    = bonglinje.hovedgr AND 
                                                       TT_Drivrapp23_1.korttyp = iKorttyp NO-ERROR.
                                IF NOT AVAIL TT_Drivrapp23_1 THEN DO:
                                    FIND tt_Raindance WHERE tt_Raindance.hg = bonglinje.hovedgr NO-ERROR.
                                    CREATE TT_Drivrapp23_1.
                                    ASSIGN TT_Drivrapp23_1.butik = bonglinje.butik
                                           TT_Drivrapp23_1.dato  = bokforingsdag.dato
                                           TT_Drivrapp23_1.HG    = bonglinje.hovedgr
                                           TT_Drivrapp23_1.HGbeskr = TRIM(huvgr.hgbeskr)
                                           TT_Drivrapp23_1.RDkod = IF AVAIL tt_Raindance THEN tt_Raindance.RDkod ELSE 999
                                           TT_Drivrapp23_1.korttyp = iKorttyp
                                           TT_Drivrapp23_1.korttext = DYNAMIC-FUNCTION('getKortnamn',iKorttyp).
                                END.
                                ASSIGN TT_Drivrapp23_1.volym     = TT_Drivrapp23_1.volym + bonglinje.antall
                                       TT_Drivrapp23_1.belopp    = TT_Drivrapp23_1.belopp + dSalgssumTmp.
                            END.
                            IF iRapportTyp = 3 THEN DO:
/*                                 FIND FIRST kort_bonglinje WHERE kort_bonglinje.b_id = bonghode.b_id AND kort_bonglinje.ttid = 52 NO-LOCK NO-ERROR.     */
/*                                 IF NOT AVAIL kort_bonglinje THEN DO:                                                                                   */
/*                                     FIND FIRST kort_bonglinje WHERE kort_bonglinje.b_id = bonghode.b_id AND kort_bonglinje.ttid = 58 NO-LOCK NO-ERROR. */
/*                                 END.                                                                                                                   */
                                IF AVAIL kort_bonglinje THEN DO:
                                    FIND TT_Drivrapp23_2 WHERE TT_Drivrapp23_2.butik   = bonglinje.butik AND
                                                               TT_Drivrapp23_2.dato    = bokforingsdag.dato  AND
                                                               TT_Drivrapp23_2.HG      = bonglinje.hovedgr AND 
                                                               TT_Drivrapp23_2.korttyp = INT(kort_bonglinje.antall) NO-ERROR.
                                    IF NOT AVAIL TT_Drivrapp23_2 THEN DO:
                                        FIND tt_Raindance WHERE tt_Raindance.hg = bonglinje.hovedgr NO-ERROR.
                                        CREATE TT_Drivrapp23_2.
                                        ASSIGN TT_Drivrapp23_2.butik    = bonglinje.butik
                                               TT_Drivrapp23_2.dato     = bokforingsdag.dato
                                               TT_Drivrapp23_2.HG       = bonglinje.hovedgr
                                               TT_Drivrapp23_2.korttyp  = INT(kort_bonglinje.antall)
                                               TT_Drivrapp23_2.HGbeskr  = TRIM(huvgr.hgbeskr)
                                               TT_Drivrapp23_2.RDkod    = IF AVAIL tt_Raindance THEN tt_Raindance.RDkod ELSE 999
                                               TT_Drivrapp23_2.korttext = DYNAMIC-FUNCTION('getKortnamn',kort_bonglinje.antall)
                                               TT_Drivrapp23_2.bemannade = Butiker.EODRapporter.
                                    END.
                                    ASSIGN TT_Drivrapp23_2.volym     = TT_Drivrapp23_2.volym + bonglinje.antall
                                           TT_Drivrapp23_2.belopp    = TT_Drivrapp23_2.belopp + dSalgssumTmp.
                                END.
                            END.
                        END.
                    END. /* SALG: */
                END. /* Bonglinjer slut */
            END.
        END.
    END.
END.
FOR EACH kasse WHERE kasse.butikknr = iButik NO-LOCK:
    DO dDatum = dFraDato TO dTilDato:
    END.
END.



/* 
DEFINE TEMP-TABLE TT_Drivrapp23_1 NO-UNDO
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

&IF DEFINED(EXCLUDE-Export2Sheet1_23) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Export2Sheet1_23 Procedure 
PROCEDURE Export2Sheet1_23 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFilename AS CHARACTER   NO-UNDO.
  OUTPUT STREAM sExportFile TO VALUE(cFilename) NO-ECHO.
  EXPORT STREAM sExportFile DELIMITER ";"
    "DRIVMEDELSRAPPORT " + string(iRapportTyp) + "_1 totalt i perioden" SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    /* A  */ "Station"          
    /* B  */ "Typ"                 
    /* C  */ "Beskr"        
    /* D  */ "RDkod"
    /* E  */ "Volym"               
    /* F  */ "Sum"              
    /* G  */ "Kortnr"              
    /* H  */ "Korttyp"              
    SKIP.                                 
  EKSPORT:
  FOR EACH TT_Drivrapp23_1TOT:
      EXPORT STREAM sExportFile DELIMITER ";"
          /* A  */ TT_Drivrapp23_1TOT.butik  
                   TT_Drivrapp23_1TOT.hg     
                   TT_Drivrapp23_1TOT.hgbeskr
                   TT_Drivrapp23_1TOT.RDkod
                   TT_Drivrapp23_1TOT.volym  
                   TT_Drivrapp23_1TOT.belopp 
                   TT_Drivrapp23_1TOT.korttyp
                   TT_Drivrapp23_1TOT.korttext
          SKIP.
  END.

  /* Lukker stream */
  OUTPUT STREAM sExportFile CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Export2Sheet1_32) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Export2Sheet1_32 Procedure 
PROCEDURE Export2Sheet1_32 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cFilename AS CHARACTER   NO-UNDO.
    OUTPUT STREAM sExportFile TO VALUE(cFilename) NO-ECHO.

    /* Legger ut overskrifter. */
  /*   STATUS DEFAULT "Eksporterer data...". */
    EXPORT STREAM sExportFile DELIMITER ";"
        "DRIVMEDELSRAPPORT 3_2 totalt i perioden" SKIP.                                 
    EXPORT STREAM sExportFile DELIMITER ";"
      /* A  */ "Station"          
      /* B  */ "Typ"                 
      /* C  */ "Beskr"        
      /* D  */ "RDkod"
      /* E  */ "Volym"               
      /* F  */ "Sum"              
      /* G  */ "Kortnr"              
      /* H  */ "Korttyp"              
      SKIP.                                 
    EKSPORT:
    FOR EACH TT_Drivrapp23_2TOT:
        EXPORT STREAM sExportFile DELIMITER ";"
            /* A  */ TT_Drivrapp23_2TOT.butik  
                     TT_Drivrapp23_2TOT.hg     
                     TT_Drivrapp23_2TOT.hgbeskr
                     TT_Drivrapp23_2TOT.RDkod
                     TT_Drivrapp23_2TOT.volym  
                     TT_Drivrapp23_2TOT.belopp 
                     TT_Drivrapp23_2TOT.korttyp
                     TT_Drivrapp23_2TOT.korttext SKIP.
    END.
    /* Lukker stream */
    OUTPUT STREAM sExportFile CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Export2Sheet2_23) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Export2Sheet2_23 Procedure 
PROCEDURE Export2Sheet2_23 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFilename AS CHARACTER   NO-UNDO.
  OUTPUT STREAM sExportFile TO VALUE(cFilename) NO-ECHO.
  EXPORT STREAM sExportFile DELIMITER ";"
    "DRIVMEDELSRAPPORT " + string(iRapportTyp) + "_1 " + STRING(dFraDato) +  " - " + STRING(dTilDato)
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    /* A  */ "Station"          
    /* B  */ "Datum"              
    /* C  */ "Typ"                 
    /* D  */ "Beskr"        
    /* E  */ "RDkod"
    /* F  */ "Volym"               
    /* G  */ "Sum"              
    /* H  */ "Kortnr"              
    /* I  */ "Korttyp"              
    SKIP.                                 
  EKSPORT:
  FOR EACH TT_Drivrapp23_1:
      EXPORT STREAM sExportFile DELIMITER ";"
          /* A  */ TT_Drivrapp23_1.butik  
                   TT_Drivrapp23_1.dato   
                   TT_Drivrapp23_1.hg     
                   TT_Drivrapp23_1.hgbeskr
                   TT_Drivrapp23_1.RDkod
                   TT_Drivrapp23_1.volym  
                   TT_Drivrapp23_1.belopp
                   TT_Drivrapp23_1.korttyp  
                   TT_Drivrapp23_1.korttext SKIP.
  END.

  /* Lukker stream */
  OUTPUT STREAM sExportFile CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Export2Sheet2_32) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Export2Sheet2_32 Procedure 
PROCEDURE Export2Sheet2_32 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cFilename AS CHARACTER   NO-UNDO.
    OUTPUT STREAM sExportFile TO VALUE(cFilename) NO-ECHO.

    /* Legger ut overskrifter. */
  /*   STATUS DEFAULT "Eksporterer data...". */
    EXPORT STREAM sExportFile DELIMITER ";"
        "DRIVMEDELSRAPPORT " + string(iRapportTyp) + "_2 " + STRING(dFraDato) +  " - " + STRING(dTilDato)
        SKIP.                                 
    EXPORT STREAM sExportFile DELIMITER ";"
      /* A  */ "Station"          
      /* B  */ "Datum"              
      /* C  */ "Typ"                 
      /* D  */ "Beskr"        
      /* E  */ "RDkod"
      /* F  */ "Volym"               
      /* G  */ "Sum"              
      /* H  */ "Kortnr"              
      /* I  */ "Korttyp"              
      SKIP.                                 
    EKSPORT:
    FOR EACH TT_Drivrapp23_2:
        EXPORT STREAM sExportFile DELIMITER ";"
            /* A  */ TT_Drivrapp23_2.butik  
                     TT_Drivrapp23_2.dato   
                     TT_Drivrapp23_2.hg     
                     TT_Drivrapp23_2.hgbeskr
                     TT_Drivrapp23_2.RDkod
                     TT_Drivrapp23_2.volym  
                     TT_Drivrapp23_2.belopp 
                     TT_Drivrapp23_2.korttyp
                     TT_Drivrapp23_2.korttext SKIP.
    END.
    /* Lukker stream */
    OUTPUT STREAM sExportFile CLOSE.

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

&IF DEFINED(EXCLUDE-RunExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RunExcel Procedure 
PROCEDURE RunExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER cFilename AS CHARACTER   NO-UNDO.
  DEFINE OUTPUT PARAMETER cFilename2 AS CHARACTER   NO-UNDO.
  DEF VAR cTmpFile23_1  AS CHAR NO-UNDO.
  DEF VAR cTmpFile23_2  AS CHAR NO-UNDO.
  DEF VAR wAntPoster AS INT  NO-UNDO.
DEFINE VARIABLE wExcEkstent AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE wkriterier AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE wKunde AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE wSkoTex AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cOrgDateFormat AS CHARACTER   NO-UNDO.
  cOrgDateFormat = SESSION:DATE-FORMAT.
  SESSION:DATE-FORMAT = "ymd".

/*   {sww.i} */
cTmpFile23_1 = SESSION:TEMP-DIR + "Rapport23_1.tmp".
cTmpFile23_2 = SESSION:TEMP-DIR + "Rapport23_2.tmp".
  IF SEARCH(cTmpFile23_1) <> ? THEN
      OS-DELETE VALUE(cTmpFile23_1).
  IF SEARCH(cTmpFile23_2) <> ? THEN
      OS-DELETE VALUE(cTmpFile23_2).
  /* Finner temporært filnavn. */
  if valid-handle(wLibHandle) then
      run GetTempFileName in wLibHandle ("Rapport23_1", "tmp", output cTmpFile23_1).
  if valid-handle(wLibHandle) then
      run GetTempFileName in wLibHandle ("Rapport23_2", "tmp", output cTmpFile23_2).
/*     run GetTempFileName in wLibHandle ("Drivmedel", wExcEkstent, output cTmpFile23_1). */
  IF iRapporttyp = 2 THEN
     cFilename = SESSION:TEMP-DIR + "Drivmedelsrapport2_" + REPLACE(STRING(TODAY),"/","") + ".xlsx".  
  ELSE DO:
      cFilename = SESSION:TEMP-DIR + "Drivmedelsrapport3_1" + REPLACE(STRING(TODAY),"/","") + ".xlsx".  
      cFilename2 = SESSION:TEMP-DIR + "Drivmedelsrapport3_2" + REPLACE(STRING(TODAY),"/","") + ".xlsx".  
  END.
IF SEARCH(cFilename) <> ? THEN
      OS-DELETE VALUE(cFileName).
IF SEARCH(cFilename2) <> ? THEN
      OS-DELETE VALUE(cFileName2).

  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(cTmpFile23_1) NO-ECHO.
  
  /* Legger ut overskrifter. */
/*   STATUS DEFAULT "Eksporterer data...". */
  EXPORT STREAM sExportFile DELIMITER ";"
    "DRIVMEDELSRAPPORT " + string(iRapportTyp) + "_1 " + STRING(dFraDato) +  " - " + STRING(dTilDato)
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    /* A  */ "Station"          
    /* B  */ "Dato"              
    /* C  */ "Typ"                 
    /* D  */ "Beskr"        
    /* E  */ "RDkod"
    /* F  */ "Volym"               
    /* G  */ "Sum"              
    SKIP.                                 
  EKSPORT:
  FOR EACH TT_Drivrapp23_1:
      EXPORT STREAM sExportFile DELIMITER ";"
          /* A  */ TT_Drivrapp23_1.butik  
                   TT_Drivrapp23_1.dato   
                   TT_Drivrapp23_1.hg     
                   TT_Drivrapp23_1.hgbeskr
                   TT_Drivrapp23_1.RDkod
                   TT_Drivrapp23_1.volym  
                   TT_Drivrapp23_1.belopp SKIP.
  END.

  /* Lukker stream */
  OUTPUT STREAM sExportFile CLOSE.
IF iRapporttyp = 3 THEN DO:
    /* Åpner stream för sida 2*/
    OUTPUT STREAM sExportFile TO VALUE(cTmpFile23_2) NO-ECHO.

    /* Legger ut overskrifter. */
  /*   STATUS DEFAULT "Eksporterer data...". */
    EXPORT STREAM sExportFile DELIMITER ";"
        "DRIVMEDELSRAPPORT " + string(iRapportTyp) + "_2 " + STRING(dFraDato) +  " - " + STRING(dTilDato)
        SKIP.                                 
    EXPORT STREAM sExportFile DELIMITER ";"
      /* A  */ "Station"          
      /* B  */ "Dato"              
      /* C  */ "Typ"                 
      /* D  */ "Beskr"        
      /* E  */ "RDkod"
      /* F  */ "Volym"               
      /* G  */ "Sum"              
      /* H  */ "Kortnr"              
      /* I  */ "Korttyp"              
      SKIP.                                 
    EKSPORT:
    FOR EACH TT_Drivrapp23_2:
        EXPORT STREAM sExportFile DELIMITER ";"
            /* A  */ TT_Drivrapp23_2.butik  
                     TT_Drivrapp23_2.dato   
                     TT_Drivrapp23_2.hg     
                     TT_Drivrapp23_2.hgbeskr
                     TT_Drivrapp23_2.RDkod
                     TT_Drivrapp23_2.volym  
                     TT_Drivrapp23_2.belopp 
                     TT_Drivrapp23_2.korttyp
                     TT_Drivrapp23_2.korttext SKIP.
    END.
    /* Lukker stream */
    OUTPUT STREAM sExportFile CLOSE.
END.

/*   STATUS DEFAULT "Importerer data i Excel...". */
  CREATE "Excel.Application" chExcelApplication.  
  chExcelApplication:Visible = FALSE.                                     
  chWorkbooks = chExcelApplication:Workbooks:OpenText(cTmpFile23_1,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
 
/*   STATUS DEFAULT "Setter aktivt ark...". */
  chWorkSheets = chExcelApplication:Sheets:Item(1).
 
/*   STATUS DEFAULT "Setter overskrift...". */
  chWorkSheets:Range("A1:G1"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:G1"):Font:Italic = TRUE.
  chWorkSheets:Range("A2:G2"):Font:Bold = TRUE.
  chWorkSheets:Range("A2:G2"):Font:Italic = TRUE.

  chWorkSheets:Range("A:A"):NumberFormat = "###0".
/*   IF SESSION:DATE-FORMAT = "ymd" THEN */
      chWorkSheets:Range("B:B"):NumberFormat = "ÅÅ-MM-DD".
/*   ELSE                                                     */
/*       chWorkSheets:Range("B:B"):NumberFormat = "DD-MM-ÅÅ". */
  chWorkSheets:Range("F:G"):NumberFormat = "# ##0,00".
  
/*   STATUS DEFAULT "Setter overskrift...". */
  chWorkSheets:Range("A1:G1"):Merge().
  chWorkSheets:Columns("D:D"):AutoFit().
  chWorkSheets:PageSetup:Orientation    = 1.
  chWorkSheets:PageSetup:FitToPagesWide = 1.
/*   chExcelApplication:Visible = TRUE. */
  chExcelApplication:ActiveWorkbook:SaveAs(cFilename,51,,,,,,,,,,).  
  chExcelApplication:QUIT.
  RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
  RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
  RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */
  ASSIGN chWorksheets       = ?
         chWorkbooks        = ?
         chExcelApplication = ?.
  
/* Försök med flik2 */
IF iRapporttyp = 3 THEN DO:
    CREATE "Excel.Application" chExcelApplication.  
    chExcelApplication:Visible = FALSE.                                     
    chWorkbooks = chExcelApplication:Workbooks:OpenText(cTmpFile23_2,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).

  /*   STATUS DEFAULT "Setter aktivt ark...". */
    chWorkSheets = chExcelApplication:Sheets:Item(1).

  /*   STATUS DEFAULT "Setter overskrift...". */
    chWorkSheets:Range("A1:G1"):Font:Bold = TRUE.
    chWorkSheets:Range("A1:G1"):Font:Italic = TRUE.
    chWorkSheets:Range("A2:I2"):Font:Bold = TRUE.
    chWorkSheets:Range("A2:I2"):Font:Italic = TRUE.

    chWorkSheets:Range("A:A"):NumberFormat = "###0".
/*     IF SESSION:DATE-FORMAT = "ymd" THEN */
        chWorkSheets:Range("B:B"):NumberFormat = "ÅÅ-MM-DD".
/*     ELSE                                                     */
/*         chWorkSheets:Range("B:B"):NumberFormat = "DD-MM-ÅÅ". */
    chWorkSheets:Range("F:G"):NumberFormat = "# ##0,00".

  /*   STATUS DEFAULT "Setter overskrift...". */
    chWorkSheets:Range("A1:G1"):Merge().
    chWorkSheets:Columns("D:D"):AutoFit().
    chWorkSheets:PageSetup:Orientation    = 1.
    chWorkSheets:PageSetup:FitToPagesWide = 1.
/*   chExcelApplication:Visible = TRUE. */
  chExcelApplication:ActiveWorkbook:SaveAs(cFilename2,51,,,,,,,,,,).  
  chExcelApplication:QUIT.
  RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
  RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
  RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */
  ASSIGN chWorksheets       = ?
         chWorkbooks        = ?
         chExcelApplication = ?.
END.


  
/*   {swn.i} */

/*   STATUS DEFAULT "". */
/*   IF SEARCH(cTmpFile23_1) <> ? THEN  */
/*       OS-DELETE VALUE(cTmpFile23_1). */
/*   IF SEARCH(cTmpFile23_2) <> ? THEN  */
/*       OS-DELETE VALUE(cTmpFile23_2). */
 SESSION:DATE-FORMAT = cOrgDateFormat.

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
  chExcelApplication:ActiveWorkbook:SaveAs(cExcelFilename23,51,,,,,,,,,,).
  chExcelApplication:QUIT.
  RELEASE OBJECT chExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ToExcel_23) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToExcel_23 Procedure 
PROCEDURE ToExcel_23 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFilelist AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER cFliknamn_1 AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER cFliknamn_2 AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER cExcelFilename AS CHARACTER   NO-UNDO.

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

&IF DEFINED(EXCLUDE-ToExcel_32) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToExcel_32 Procedure 
PROCEDURE ToExcel_32 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cFilelist AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER cFliknamn_1 AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER cFliknamn_2 AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER cExcelFilename AS CHARACTER   NO-UNDO.
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
/*        "PREEM,VAKANT,PREEM SÅIFA,LOGPAY,UNO-X NO,UNO-X DK,VAKANT,VOLVO,NESTE,DKV,VAKANT,VAKANT,BANKKORT,VAKANT,VAKANT,VAKANT,UTA,BONUSKOR,OKDK,E100,EDC,TP24,ARIS,Eurowag,". */
       /* "PREEM,PREEM VISA,SÅIFA,TEPAR,HY/TEX NO,HY/TEX DK,SAAB/OPEL,VOLVO,NESTE,DKV,OK,UNO-X,BANKKORT,AMEX,DINERS,FINAX,UTA,BONUSKORT,CAMPING,,,,,,". */
       IF CAN-DO(cSubtypeNr,STRING(iSubType)) THEN
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

