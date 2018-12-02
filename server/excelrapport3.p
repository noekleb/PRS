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


&SCOPED-DEFINE Delim "~t"
&SCOPED-DEFINE D1 CHR(1)
DEF STREAM stA.

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR i                  AS INT NO-UNDO.
/* DEF VAR ii                 AS INT NO-UNDO. */
DEF VAR ix                 AS INT NO-UNDO.
DEF VAR iButikkNr          AS INT NO-UNDO. /*The shop number*/
DEF VAR tthParam           AS HANDLE NO-UNDO.
DEF VAR cSheetFileName     AS CHAR NO-UNDO.
DEF VAR cFilePath          AS CHAR NO-UNDO.
DEF VAR cFileExtent        AS CHAR NO-UNDO.
DEF VAR cButikkList        AS CHAR NO-UNDO.
DEF VAR cAvdelingList      AS CHAR NO-UNDO. 
DEF VAR cSheetName         AS CHAR NO-UNDO.
DEF VAR cReportName        AS CHAR NO-UNDO.
DEF VAR cTime              AS CHAR NO-UNDO.
DEF VAR cDelete            AS CHAR NO-UNDO.
DEF VAR iGeneralSysHid     AS INT INIT 227 NO-UNDO.
DEF VAR iGeneralSysGr      AS INT INIT 1   NO-UNDO. /*Used for mandatory fields like extent,filepath,EmptyRow@Breakpoint etc. */
DEF VAR iTranslateSysGr    AS INT INIT 10  NO-UNDO. /*Used for translation*/
DEF VAR iSysHid            AS INT  NO-UNDO.
DEF VAR iSysGr             AS INT  NO-UNDO.
DEF VAR cPerId             AS CHAR NO-UNDO.
DEF VAR iFraAarPerLinNr    AS INT  NO-UNDO.
DEF VAR iTilAarPerLinNr    AS INT  NO-UNDO.

DEFINE VARIABLE gdReportDate  AS DATE NO-UNDO. 
DEFINE VARIABLE gdTilDato     AS DATE NO-UNDO. 
DEFINE VARIABLE gdFraDato     AS DATE NO-UNDO. 
DEFINE VARIABLE iNextSend     AS INT  NO-UNDO. 

DEF TEMP-TABLE ttParam NO-UNDO
  FIELD iParamId    AS INT
  FIELD cParamGroup AS CHAR FORMAT 'x(50)' 
  FIELD cParamName  AS CHAR FORMAT 'x(50)'
  FIELD cParamValue AS CHAR FORMAT 'x(50)'
  INDEX iParamId IS PRIMARY UNIQUE iParamId
  INDEX cParamGroup cParamGroup
  INDEX cParamName cParamName
  .
tthParam = TEMP-TABLE ttParam:HANDLE.

/* For tempo-rapport */ 
DEFINE TEMP-TABLE TempoTemplate NO-UNDO
  FIELD Avdeling AS INT
  FIELD AvdelingNavn AS CHAR 
  FIELD RapportManed AS INT 
  FIELD RapportAr AS INT 
  FIELD AntallSolgt AS INT 
  FIELD BruttoSalg AS DEC 
  FIELD NettoSalg AS DEC
  FIELD DbKr AS DEC  
  FIELD Db% AS DEC .

DEFINE TEMP-TABLE PrevYearMonth NO-UNDO LIKE tempoTemplate.
DEFINE TEMP-TABLE SaleThisMonth NO-UNDO LIKE tempoTemplate. 

DEFINE TEMP-TABLE SaleByDay    NO-UNDO 
    FIELD butikkNavn AS CHAR 
    FIELD Dato AS DATE
    FIELD AntallSolgt AS INT 
    FIELD BruttoSalg AS DEC
    FIELD NettoSalg AS DEC 
    FIELD DbKr AS DEC  
    FIELD Db% AS DEC
    INDEX idx1 IS PRIMARY Dato .

DEFINE TEMP-TABLE Totals 
  FIELD hTableHandle AS HANDLE 
  FIELD AntallSolgt AS INT 
  FIELD BruttoSalg AS DEC
  FIELD NettoSalg AS DEC
  FIELD DBkr AS DEC
  FIELD Db% AS DEC .

DEF TEMP-TABLE ttSend
    FIELD iSendNum      AS INT
    FIELD cSendSubject  AS CHAR 
    FIELD cSendReceiver AS CHAR
    FIELD cSendBody     AS CHAR
    FIELD cSendFile     AS CHAR
    INDEX iSendNum IS PRIMARY UNIQUE iSendNum.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-addEmptyRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addEmptyRow Procedure 
FUNCTION addEmptyRow RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addSend) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addSend Procedure 
FUNCTION addSend RETURNS LOGICAL
  ( INPUT icReceiver AS CHAR,
    INPUT icSubject  AS CHAR,
    INPUT icBody     AS CHAR,
    INPUT icFile     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-buildFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD buildFilter Procedure 
FUNCTION buildFilter RETURNS CHARACTER
  (INPUT icTable AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-chkMandetoryFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD chkMandetoryFields Procedure 
FUNCTION chkMandetoryFields RETURNS LOGICAL
  (INPUT ipcWord AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-chkVarGr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD chkVarGr Procedure 
FUNCTION chkVarGr RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convExcelFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD convExcelFormat Procedure 
FUNCTION convExcelFormat RETURNS CHARACTER
  (INPUT ihField AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convFromJulianDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD convFromJulianDate Procedure 
FUNCTION convFromJulianDate RETURNS DATE
  (INPUT ipiNumDays AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD convText Procedure 
FUNCTION convText RETURNS CHARACTER
  (INPUT icText AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convToJulianDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD convToJulianDate Procedure 
FUNCTION convToJulianDate RETURNS INTEGER
  (INPUT idDate AS DATE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createParameter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createParameter Procedure 
FUNCTION createParameter RETURNS LOGICAL
  
  (INPUT icFieldGroup AS CHARACTER,
   INPUT icFieldName  AS CHARACTER,
   INPUT icFieldValue AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAlphaSeqNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAlphaSeqNo Procedure 
FUNCTION getAlphaSeqNo RETURNS CHARACTER
  ( INPUT iiSeqNo AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBreakText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBreakText Procedure 
FUNCTION getBreakText RETURNS CHARACTER
  ( INPUT icField AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDateCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDateCode Procedure 
FUNCTION getDateCode RETURNS INTEGER
  ( INPUT ipdDate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDateFromDateCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDateFromDateCode Procedure 
FUNCTION getDateFromDateCode RETURNS DATE

  (INPUT ipdDateCode AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFirstDayInMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFirstDayInMonth Procedure 
FUNCTION getFirstDayInMonth RETURNS DATE
  ( INPUT ipdDate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFormattedAvdeling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFormattedAvdeling Procedure 
FUNCTION getFormattedAvdeling RETURNS CHARACTER
  ( INPUT ipiAvdeling AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFormattedYearMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFormattedYearMonth Procedure 
FUNCTION getFormattedYearMonth RETURNS INTEGER
  ( INPUT ipdDate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLastSale) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLastSale Procedure 
FUNCTION getLastSale RETURNS DATE
  ( INPUT iiButikk AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParam Procedure 
FUNCTION getParam RETURNS CHARACTER (INPUT icGroup AS CHAR,
                   INPUT icName  AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWeekNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWeekNum Procedure 
FUNCTION getWeekNum RETURNS INTEGER
  ( INPUT idSomeDate     AS DATE,
    INPUT iiOutputLength AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-makePeriodHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD makePeriodHeader Procedure 
FUNCTION makePeriodHeader RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NumberOfSalesDaysThisMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NumberOfSalesDaysThisMonth Procedure 
FUNCTION NumberOfSalesDaysThisMonth RETURNS INTEGER

  (INPUT  ipdDay AS DATE,
   INPUT  ipiButikk AS INT ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParam Procedure 
FUNCTION setParam RETURNS LOGICAL
  ( INPUT icParamGroup AS CHAR,
    INPUT icParamName  AS CHAR,
    INPUT icParamValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD showParam Procedure 
FUNCTION showParam RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TotalNumberOfSalesDaysThisMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TotalNumberOfSalesDaysThisMonth Procedure 
FUNCTION TotalNumberOfSalesDaysThisMonth RETURNS INTEGER

(INPUT ipdDay AS DATE,
 INPUT ipiButikk AS INT ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-translateWord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD translateWord Procedure 
FUNCTION translateWord RETURNS CHARACTER
  (INPUT ipcWord AS CHAR)  FORWARD.

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
         HEIGHT             = 31.57
         WIDTH              = 64.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


/* IF iiSysHid NE 0 AND iiSysGr NE 0 THEN */
  RUN initializeObject.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-fetchStlinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchStlinje Procedure 
PROCEDURE fetchStlinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR cAarPerLinNr AS CHAR   NO-UNDO. /*varierer i henhold til cPerId... */
  DEF VAR cFieldList   AS CHAR   NO-UNDO.
  DEF VAR fFieldValue  AS DEC    NO-UNDO.

  DEF VAR cTypeId      AS CHAR   NO-UNDO.
  
  DEF VAR cListe          AS CHAR NO-UNDO.
  DEF VAR iCnt            AS INT  NO-UNDO.
  DEF VAR bWithEAN        AS LOG  NO-UNDO.
  
  DEF VAR bh              AS HANDLE NO-UNDO.
  
  ASSIGN 
    cTypeId         = getParam('Filter','stlinje.stTypeId')
    cPerId          = convText(getParam('Filter','stlinje.PerId'))
    cAarPerLinNr    = getParam('Filter','stlinje.aarPerLinNr')
/*     ix              = 0 */
    iFraAarPerLinNr = INT(ENTRY(1,cAarPerLinNr)) 
    iTilAarPerLinNr = INT(ENTRY(2,cAarPerLinNr)) 
  .
/*   tthstLinje:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().                                                                                                                                                                                   */
/*   FOR EACH stlinje NO-LOCK WHERE stlinje.butik = butiker.butik                                                                                                                                                                           */
/*                              AND stlinje.sttypeid          = cTypeId                                                                                                                                                                     */
/*                              AND stlinje.perid             = cPerId                                                                                                                                                                      */
/*                              AND stlinje.AarPerLinNr       GE iFraAarPerLinNr                                                                                                                                                            */
/*                              AND stlinje.AarPerLinNr       LE iTilAarPerLinNr                                                                                                                                                            */
/*                              AND CAN-FIND(vargr WHERE vargr.Vg = INT(stlinje.DataObjekt)):                                                                                                                                               */
/*     FIND FIRST vargr    WHERE vargr.vg = INT(stlinje.dataobjekt) NO-LOCK NO-ERROR.                                                                                                                                                       */
/*     FIND FIRST huvgr    OF vargr NO-LOCK NO-ERROR.                                                                                                                                                                                       */
/*     FIND FIRST avdeling OF huvgr  NO-LOCK NO-ERROR.                                                                                                                                                                                      */
/*     FIND FIRST butiker  WHERE butiker.butik = stlinje.butik NO-LOCK NO-ERROR.                                                                                                                                                            */
/*     bh = BUFFER stLinje:HANDLE.                                                                                                                                                                                                          */
/*     tthstLinje:DEFAULT-BUFFER-HANDLE:FIND-UNIQUE('where Hg = ' + STRING(HuvGr.Hg) + ' AND Vg = ' + STRING(vargr.Vg) + ' AND butik=' + ENTRY(i,cButikkList,'|')) NO-ERROR.                                                                */
/*   /*       MESSAGE 'where stTypeId = ' + QUOTER(cTypeId) + 'AND perid = ' + QUOTER(cPerId) + ' AND dataobjekt = ' + STRING(vargr.Vg) + ' AND butik=' + ENTRY(i,cButikkList,'|') */                                                       */
/*   /*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                                                                 */                                                       */
/*     IF tthstLinje:DEFAULT-BUFFER-HANDLE:AVAIL THEN                                                                                                                                                                                       */
/*     DO:                                                                                                                                                                                                                                  */
/*       cFieldList = 'antSolgt,vvarekost,verdisolgt'.                                                                                                                                                                                      */
/*       DO ii = 1 TO NUM-ENTRIES(cFieldList):                                                                                                                                                                                              */
/*         cFieldName = ENTRY(ii,cFieldList).                                                                                                                                                                                               */
/*         tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(cFieldName):BUFFER-VALUE = tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(cFieldName):BUFFER-VALUE + bh:BUFFER-FIELD(cFieldName):BUFFER-VALUE.                                      */
/*       END.                                                                                                                                                                                                                               */
/*     END.                                                                                                                                                                                                                                 */
/*     ELSE                                                                                                                                                                                                                                 */
/*     DO:                                                                                                                                                                                                                                  */
/*       ix = ix + 1.                                                                                                                                                                                                                       */
/*       tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-CREATE().                                                                                                                                                                                  */
/*       /*NB!!! Her kopierer jeg hele bufferet fra de buffere som skal kopieres. Om det er like feltnavn vil siste kopiering være gjeldene. På siste linje kan en se at jeg har utelatt 3 felt som ellers ville ha                         */
/*       overskrevet ovenforliggende felt... f.eks. Beskrivelse er feltet jeg ønsker fra produsent, om det ikke hadde vært lagt på utelatelses listen på stlinje, ville det være beskrivelse felt fra stlinje som hadde                     */
/*       vært gjeldene. Blir det et problem, må en ned på buffer-field nivå (se varemerke...) */                                                                                                                                            */
/*       IF AVAIL huvgr     THEN tthstlinje:DEFAULT-BUFFER-HANDLE:BUFFER-COPY(BUFFER huvgr:HANDLE).                                                                                                                                         */
/*       IF AVAIL avdeling  THEN tthstlinje:DEFAULT-BUFFER-HANDLE:BUFFER-COPY(BUFFER avdeling:HANDLE).                                                                                                                                      */
/*       IF AVAIL vargr     THEN tthstlinje:DEFAULT-BUFFER-HANDLE:BUFFER-COPY(BUFFER vargr:HANDLE).                                                                                                                                         */
/*       IF AVAIL butiker   THEN tthstlinje:DEFAULT-BUFFER-HANDLE:BUFFER-COPY(BUFFER butiker:HANDLE).                                                                                                                                       */
/*       IF AVAIL stlinje   THEN tthstlinje:DEFAULT-BUFFER-HANDLE:BUFFER-COPY(BUFFER stlinje:HANDLE,'hg,vg,beskrivelse').                                                                                                                   */
/*       ASSIGN                                                                                                                                                                                                                             */
/*         tthstlinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('Vg'):BUFFER-VALUE = vargr.Vg                                                                                                                                                      */
/*         tthstlinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('Hg'):BUFFER-VALUE = huvgr.Hg                                                                                                                                                      */
/*       .                                                                                                                                                                                                                                  */
/*      END.                                                                                                                                                                                                                                */
/*                                                                                                                                                                                                                                          */
/*     tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('DBkroner'):BUFFER-VALUE = tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('VerdiSolgt'):BUFFER-VALUE - tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('vvarekost'):BUFFER-VALUE.         */
/*     tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('DBpros'):BUFFER-VALUE = (tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('DBkroner'):BUFFER-VALUE / tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('verdisolgt'):BUFFER-VALUE) * 100 .   */
/*     tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('DBpros'):BUFFER-VALUE = IF tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('DBpros'):BUFFER-VALUE = ? THEN 0 ELSE tthstLinje:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD('DBpros'):BUFFER-VALUE. */
/*                           */
/* END. /*FOR each stlinje*/ */

/*     tthstlinje:WRITE-XML('file','c:\temp\ttstlinje.xml'). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSalesByMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSalesByMonth Procedure 
PROCEDURE getSalesByMonth :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipiButikk AS INT NO-UNDO. 
    DEFINE INPUT PARAMETER ipiAvdeling AS INT NO-UNDO. 
     
    DEFINE VARIABLE iYearMonth AS INT NO-UNDO. 
    DEFINE VARIABLE cAvdeling AS CHAR NO-UNDO. 


    cAvdeling = getFormattedAvdeling(ipiAvdeling).
    iYearMonth = getFormattedYearMonth(ipdDate). 

    FOR EACH  stlinje NO-LOCK WHERE 
              stlinje.butik       = ipiButikk   AND
              stlinje.sttypeid    = "AVDELING"  AND
              stlinje.perid       = "MANED"     AND 
              stlinje.aarperlinnr = iYearMonth  AND
              stlinje.dataobjekt  = cAvdeling :
                
          FIND avdeling WHERE 
               avdeling.avdelingnr = INT(stlinje.dataobjekt) NO-LOCK NO-ERROR. 
          
          CREATE PrevYearMonth. 

          ASSIGN 
              PrevYearMonth.Avdeling     = INT(stlinje.dataobjekt)
              PrevYearMonth.AvdelingNavn = IF AVAIL avdeling THEN avdeling.avdelingNavn ELSE ""
              PrevYearMonth.RapportManed = INT(SUBSTRING(STRING(stlinje.aarperlinnr),5))
              PrevYearMonth.RapportAr    = INT(SUBSTRING(STRING(stlinje.aarperlinnr),1,4))
              PrevYearMonth.AntallSolgt  = stlinje.antsolgt
              PrevYearMonth.BruttoSalg   = stlinje.mvaverdi + stlinje.verdiSolgt
              PrevYearMonth.NettoSalg    = stlinje.verdisolgt
              PrevYearMonth.DbKr         = stlinje.verdisolgt - stlinje.vvarekost
              PrevYearMonth.Db%          = (PrevYearMonth.DbKr * 100 )  / PrevYearMonth.NettoSalg. 


        /* Kalkuler totaler */ 
         FIND FIRST Totals WHERE 
                    Totals.hTableHandle = BUFFER PrevYearMonth:TABLE-HANDLE NO-ERROR.

         IF NOT AVAIL totals THEN CREATE totals.
         ASSIGN 
             totals.hTableHandle = BUFFER PrevYearMonth:TABLE-HANDLE
             totals.AntallSolgt = totals.AntallSolgt + PrevYearMonth.antallSolgt
             totals.BruttoSalg = totals.BruttoSalg + PrevYearMonth.BruttoSalg
             totals.NettoSalg =  totals.NettoSalg + PrevYearMonth.NettoSalg
             totals.DBkr      = totals.dbkr + PrevYearMonth.dbkr
             totals.db% = totals.dbkr * 100 / totals.nettoSalg.

     END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSalesThisMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSalesThisMonth Procedure 
PROCEDURE getSalesThisMonth :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdDate AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipiButikk AS INT NO-UNDO. 
    DEFINE INPUT PARAMETER ipiAvdeling AS INT NO-UNDO. 
     
    DEFINE VARIABLE iCnt AS INT NO-UNDO. 
    DEFINE VARIABLE iFromDate AS INT NO-UNDO. 
    DEFINE VARIABLE iToDate AS INT NO-UNDO. 
    DEFINE VARIABLE cAvdeling AS CHAR NO-UNDO. 
    DEFINE VARIABLE dDate AS DATE NO-UNDO. 
    DEFINE VARIABLE cButikkNavn AS CHAR NO-UNDO. 

    DEFINE BUFFER stLinje FOR stlinje. 
    DEFINE BUFFER butiker FOR butiker. 

    FIND butiker WHERE butiker.butik = ipiButikk NO-LOCK NO-ERROR.
    cButikkNavn = IF AVAIL butiker THEN butiker.butnamn ELSE "".

    ASSIGN 
      iFromDate = getDateCode(getFirstDayInMonth(ipdDate))
      iToDate   = getDateCode(ipdDate). 
      cAvdeling = getFormattedAvdeling(ipiAvdeling).

      DO iCnt = iFromDate TO iToDate : 

              FOR EACH  stlinje NO-LOCK WHERE 
                 stlinje.butik       = ipiButikk  AND
                 stlinje.sttypeid    = "AVDELING" AND
                 stlinje.perid       = "DAG"      AND 
                 stlinje.aarperlinnr = iCnt       AND
                 stlinje.dataobjekt  = cAvdeling  :

          FIND avdeling WHERE avdeling.avdelingnr = INT(stlinje.dataobjekt) NO-LOCK NO-ERROR. 
             
          FIND FIRST SaleThisMonth WHERE 
                     SaleThisMonth.Avdeling = INT(stlinje.dataobjekt) NO-ERROR. 

          IF NOT AVAIL SaleThisMonth THEN CREATE SaleThisMonth. 

          /* Akkumulate sales this month */ 
          ASSIGN 
              SaleThisMonth.Avdeling     = INT(stlinje.dataobjekt)
              SaleThisMonth.AvdelingNavn = IF AVAIL avdeling THEN avdeling.avdelingNavn ELSE ""
              SaleThisMonth.RapportManed = INT(SUBSTRING(STRING(stlinje.aarperlinnr),5))
              SaleThisMonth.RapportAr    = INT(SUBSTRING(STRING(stlinje.aarperlinnr),1,4))
              SaleThisMonth.AntallSolgt  = SaleThisMonth.AntallSolgt + stlinje.antsolgt
              SaleThisMonth.BruttoSalg   = SaleThisMonth.BruttoSalg + (stlinje.mvaverdi + stlinje.verdiSolgt)
              SaleThisMonth.DbKr         = SaleThisMonth.DbKr + ( stlinje.verdisolgt - stlinje.vvarekost)
              SaleThisMonth.NettoSalg    = SaleThisMonth.NettoSalg + stlinje.verdiSolgt
              SaleThisMonth.Db%          = (SaleThisMonth.DbKr * 100 )  / SaleThisMonth.NettoSalg.

         /* Kalkuler totaler */ 
         FIND FIRST Totals WHERE 
                    Totals.hTableHandle = BUFFER SaleThisMonth:TABLE-HANDLE NO-ERROR.

         IF NOT AVAIL totals THEN CREATE totals.
         ASSIGN 
             totals.hTableHandle = BUFFER SaleThisMonth:TABLE-HANDLE
             totals.AntallSolgt = totals.AntallSolgt + SaleThisMonth.antallSolgt
             totals.BruttoSalg = totals.BruttoSalg + SaleThisMonth.BruttoSalg
             totals.NettoSalg =  totals.NettoSalg + SaleThisMonth.NettoSalg
             totals.DBkr      = totals.dbkr + SaleThisMonth.dbkr
             totals.db% = totals.dbkr * 100 / totals.nettoSalg.

          /* ------------------------------------------------------------------------------*/ 
          /*  Dags salg for alle avdelinger som er med i avd-listen (selection )           */ 
          /* ------------------------------------------------------------------------------*/ 

          dDate = getDateFromDateCode(icnt). 

          FIND FIRST SaleByDay WHERE 
                     SaleByDay.dato = dDate NO-ERROR.
          
          IF NOT AVAIL SaleByDay THEN CREATE SaleByDay.

          ASSIGN 
              SaleByDay.Butikknavn   = cButikkNavn
              SaleByDay.AntallSolgt  = SaleByDay.AntallSolgt + stlinje.antsolgt
              SaleByDay.BruttoSalg   = SaleByDay.BruttoSalg + (stlinje.mvaverdi + stlinje.verdiSolgt)
              SaleByDay.NettoSalg    = SaleByDay.NettoSalg + stlinje.verdisolgt
              SaleByDay.DbKr         = SaleByDay.DbKr + ( stlinje.verdisolgt - stlinje.vvarekost)
              SaleByDay.Db%          = (SaleByDay.DbKr * 100 )  / SaleByDay.NettoSalg 
              SaleByDay.dato         = dDate. 

          /* Kalkuler totaler for tabell */ 
          FIND FIRST Totals WHERE 
                     Totals.hTableHandle = BUFFER SaleByDay:TABLE-HANDLE NO-ERROR.
          
          IF NOT AVAIL totals THEN CREATE totals.
          ASSIGN 
              totals.hTableHandle = BUFFER SaleByDay:TABLE-HANDLE
              totals.AntallSolgt = totals.AntallSolgt + SaleByDay.AntallSolgt
              totals.BruttoSalg = totals.BruttoSalg + SaleByDay.BruttoSalg
              totals.NettoSalg =  totals.NettoSalg + SaleByDay.NettoSalg
              totals.DBkr      = totals.dbkr + SaleByDay.dbkr
              totals.db% = totals.dbkr * 100 / totals.nettoSalg.

        END. 
     END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitializeObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject Procedure 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cTmpFileName AS CHAR NO-UNDO.
  DEFINE VARIABLE cDelete AS CHAR NO-UNDO.
  DEFINE VARIABLE cEmailAddr AS CHAR NO-UNDO. 
  DEFINE VARIABLE opcReportFile AS CHAR NO-UNDO. 
  DEFINE VARIABLE cReportFileList AS CHAR NO-UNDO. 

  ASSIGN cTime = STRING(TIME).

  RUN makeParam.
  
  ASSIGN 
    gdTildato      = DATE(getParam('Filter','tildato'))
    gdFradato      = DATE(getParam('Filter','Fradato'))
    gdReportDate   = gdTilDato
    cFilePath      = getParam('Filter','advFilePath')
    cFileExtent    = getParam('Filter','advFileExtent')
    cButikkList    = getParam('Filter','stlinje.butik')
    cAvdelingList  = getParam('Filter','avdelingNr')
    cEmailAddr     = getParam('Filter','rptemailaddr')
    cReportName    = getParam('Filter','advRapportNavn')
    cReportName    = IF cReportName = '' THEN 'TempoRapport' ELSE cReportName
  .

   FILE-INFO:FILE-NAME = cFilePath. 
   cFilePath = FILE-INFO:FULL-PATHNAME + '\'. 

  /* showParam().    */
     
  DO i = 1 TO NUM-ENTRIES(cButikkList,'|'):                                      
     iButikkNr = INT(ENTRY(i,cButikkList,'|')).                                              
     RUN processReport(iButikkNr,cAvdelingList,OUTPUT opcReportFile).   
     cReportFileList = cReportFileList + 
                       (IF cReportFileList = '' THEN '' ELSE ',' ) 
                       + opcReportFile. 
  END.  
                                                                        
  addSend(cEmailAddr,'Tempo Rapport','',cReportFileList).

  RUN processSendMail.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-makeParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE makeParam Procedure 
PROCEDURE makeParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR qh          AS HANDLE NO-UNDO.
  DEF VAR cFieldValue AS CHAR   NO-UNDO.
  
  IF VALID-HANDLE(ihBuffer) AND ihBuffer:TABLE-HANDLE:PRIVATE-DATA NE "batch" THEN
  DO:
    CREATE QUERY qh.
    qh:SET-BUFFERS(ihBuffer).
    qh:QUERY-PREPARE('for each ' + ihBuffer:NAME).
    qh:QUERY-OPEN().
    qh:GET-FIRST().

    DO WHILE ihBuffer:AVAILABLE:
      setParam('Filter',ihBuffer:BUFFER-FIELD('parameter1'):BUFFER-VALUE,ihBuffer:BUFFER-FIELD('parameter2'):BUFFER-VALUE).      
      qh:GET-NEXT().
    END.
    DELETE OBJECT qh.    
  END.
  ELSE  /*Fast rapport*/
  DO:
    ASSIGN 
      iSysHId = INT(ENTRY(1,icParam,'|'))
      iSysGr  = INT(ENTRY(2,icParam,'|'))
    .
    IF iSysHid GT 0 AND iSysGr GT 0 THEN
    DO:
      FOR EACH syspara NO-LOCK WHERE SysPara.SysHid = iSysHid
                                 AND SysPara.SysGr  = iSysGr:
        setParam('Filter',SysPara.Parameter1,sysPara.Parameter2).

      END.
    END.
    /*Her må det legges til parameter for stlinje.aarPerLinNr, kommaseparert*/
    CASE getParam('Filter','stlinje.PerId'):
      WHEN 'Dag'   THEN
      DO:
        ASSIGN 
          cFieldValue = STRING(YEAR(TODAY),'9999') + STRING(convToJulianDate(TODAY - 1),'999') 
                        + ',' + STRING(YEAR(TODAY),'9999') + STRING(convToJulianDate(TODAY - 1),'999')
        .
      END.
      WHEN 'Uke'   THEN /*Må byttes om til dag*/
      DO:
/*         FIND ttParam WHERE ttParam.cParamName = 'stlinje.PerId' NO-ERROR.                              */
/*         IF AVAIL ttParam THEN                                                                          */
/*           ttParam.cParamValue = 'Dag'.                                                                 */
/*                                                                                                        */
/*         ASSIGN                                                                                         */
/*           cFieldValue = STRING(YEAR(TODAY),'9999') + STRING(convToJulianDate(TODAY - 7),'999')         */
/*                         + ',' + STRING(YEAR(TODAY),'9999') + STRING(convToJulianDate(TODAY - 1),'999') */
/*         .                                                                                              */
        ASSIGN
          cFieldValue = STRING(getWeekNum(TODAY - 7,0)) + STRING(getWeekNum(TODAY - 7,2),'999')
                + ',' + STRING(getWeekNum(TODAY,0)) + STRING(getWeekNum(TODAY - 7,2),'999').
        .
      END.
      WHEN 'Måned' THEN /*Må byttes om til dag*/
      DO:
/*         FIND ttParam WHERE ttParam.cParamName = 'stlinje.PerId' NO-ERROR.                              */
/*         IF AVAIL ttParam THEN                                                                          */
/*           ttParam.cParamValue = 'Dag'.                                                                 */
/*                                                                                                        */
/*         ASSIGN                                                                                         */
/*           cFieldValue = STRING(YEAR(TODAY),'9999') + STRING(convToJulianDate(TODAY - 30),'999')        */
/*                         + ',' + STRING(YEAR(TODAY),'9999') + STRING(convToJulianDate(TODAY - 1),'999') */
/*         .                                                                                              */
        ASSIGN
          cFieldValue =   STRING(YEAR(TODAY),'9999') + STRING(MONTH(TODAY - DAY(TODAY)),'999') 
                        + ',' + STRING(YEAR(TODAY),'9999') + STRING(MONTH(TODAY - DAY(TODAY)),'999').

        .
      END.
      WHEN 'År'   THEN
      DO:
        ASSIGN 
          cFieldValue = STRING(YEAR(TODAY),'9999') + STRING(1,'999')
                        + ',' + STRING(YEAR(TODAY),'9999') + STRING(1,'999').
        .
      END.
    END CASE.
    setParam('Filter','stlinje.aarperLinNr',cFieldValue).

  END.

/*  tthParam:WRITE-XML('file','c:\temp\goo2.xml').    */

/* /*dag=julians dato,uke=uke,mnd=mnd og år=år*  (julians: aktuelldato - 31.12.(aktuellår - 1) )*/              */
/* /*     cParamList  = 'hgr,sasong,ravdnr,lopnr,levnr,levkod,levfarkod,prodnr,rabkod,vg,vgkat' */              */
/*   setParam('Filter','FilePath','c:\temp\').                                                               */
/*   setParam('Filter','FileExtent','.txt').                                                                 */
/*                                                                                                              */
/*   setParam('Filter','artbas.hg','10').                                                                    */
/* /*   setParam(1,'artbas.lopnr','1'). */                                                                   */
/* /*   setParam(1,'artbas.vgkat','1'). */                                                                   */
/*   setParam('Filter','stlinje.stTypeId','artikkel').                                                       */
/*   setParam('Filter','stlinje.Perid','uke').                                                               */
/*   setParam('Filter','stlinje.butik','1,2').                                                               */
/*   setParam('Filter','stlinje.aar','2005,2008').                                                           */
/*   setParam('Filter','stlinje.perlinnr','001,012'). /*i dette eksemple vil det være fra uke 1 til uke 12*/ */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processReport Procedure 
PROCEDURE processReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipiButikkNr AS INT NO-UNDO. 
    DEFINE INPUT PARAMETER  ipcAvdelingList AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER opcReportFile AS CHAR NO-UNDO.                                         

    DEFINE VARIABLE iCnt AS INT NO-UNDO. 
    DEFINE VARIABLE dLastYear AS DATE NO-UNDO. 
    DEFINE VARIABLE iTotSalgsDager AS INT NO-UNDO. 
    DEFINE VARIABLE iSalgsDager AS INT NO-UNDO. 

    FIND butiker WHERE butiker.butik = iButikkNr NO-LOCK NO-ERROR.
    IF NOT AVAIL butiker THEN NEXT.
    
    EMPTY TEMP-TABLE PrevYearMonth NO-ERROR.
    EMPTY TEMP-TABLE SaleThisMonth NO-ERROR.
    EMPTY TEMP-TABLE SaleByDay NO-ERROR. 

    DO iCnt = 1 TO NUM-ENTRIES(ipcAvdelingList,"|"):
       dLastYear = DATE(MONTH(gdReportDate),1,YEAR(gdReportDate) - 1).
       RUN getSalesByMonth  (dLastYear,ipiButikkNr,INT(ENTRY(iCnt,ipcAvdelingList,"|"))).
       RUN getSalesThisMonth(gdReportDate,ipiButikkNr,INT(ENTRY(iCnt,ipcAvdelingList,"|"))).
    END.


    /*Må sjekke at det ikke blir flere tegn enn 32 (begrensning i excel*/
    ASSIGN 
      cSheetName = RIGHT-TRIM(TRIM(STRING(butiker.butik) + ' ' + butiker.butnamn,' '),' ')
      cSheetName = REPLACE(cSheetName,':','-')
      cSheetName = IF LENGTH(cSheetName) GT 30 THEN SUBSTRING(cSheetName,1,30) ELSE cSheetName
      cSheetFileName = cFilePath + TRIM(STRING(butiker.butik),' ') + cTime + cFileExtent
      iSalgsDager = NumberOfSalesDaysThisMonth(gdReportDate,iButikkNr)
      iTotSalgsDager = TotalNumberOfSalesDaysThisMonth(gdReportDate,iButikkNr).

    opcReportFile = cFilePath + 'BID' + 
                    TRIM(STRING(butiker.butik,"9999"))  + 'Tempo' + 
                    STRING(YEAR(gdReportDate),"9999") + 
                    STRING(MONTH(gdReportDate),"99")  + 
                    STRING(DAY(gdReportDate),"99") + '_'  + cTime + cFileExtent.

    RUN writeFile(opcReportFile,
                  butiker.butnamn,
                  gdReportDate,
                  iSalgsDager,
                  iTotSalgsDager).
    
    /* legg til - output table for prcedure */                                                                                              
    IF VALID-HANDLE(ihBuffer) THEN
    DO:
      ihBuffer:FIND-FIRST('where parameter1 = "AdvRapportNavn"') NO-ERROR.
      IF ihbuffer:AVAIL THEN
        ihBuffer:BUFFER-FIELD('Parameter2'):BUFFER-VALUE = opcReportFile.
    END. 
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processSendMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processSendMail Procedure 
PROCEDURE processSendMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH ttSend:
    createParameter('SendMail',ttSend.cSendReceiver,ttSend.cSendSubject + '¤' 
                  + ttSend.cSendBody + '¤' 
                  + ttSend.cSendFile).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WeekNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WeekNum Procedure 
PROCEDURE WeekNum :
/************************************************************************************
        PROCEDURE: weeknum.p

        PURPOSE:   Calculates the week-number for a given date

        SYNTAX:    RUN samples/weeknum.p (INPUT in, OUTPUT out).

        REMARKS:   This code calculates the week-number for the date given.
                   The format is YYYYWW

        PARAMETERS:
            INPUT:  date
            OUTPUT: week number

        AUTHORS:   Judy Rothermal
        DATE:      February 1993

        LAST INSPECTED:
        INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.         */
 
/*Code_Start*/

/* Assumptions:                                                     */
/* 1. Weeks start on MONDAYS                                        */
/* 2. If January 1st falls on Friday, Saturday, Sunday or Monday    */
/*    then week 1 for this year will start on the first Monday      */
/*    the same year. If not, week 1 will start on the last Monday   */
/*    previous year.                                                */
/*    (In other words: At least 4 of the seven days of week 1 for   */
/*     a given year must fall into this year)                       */


  DEFINE INPUT  PARAMETER indate   AS DATE.  /* Input date , eg 10/17/90 */
  DEFINE OUTPUT PARAMETER yyyyww   AS INT.   /* Output week, eg 9042     */
  
  DEFINE VARIABLE yr   AS INT.  /* Year of indate, eg 1990      */
  DEFINE VARIABLE d1   AS INT.  /* Weekday of 1/1 current year, eg 2  */
                                /* (01/01/90 is a Monday)      */
  DEFINE VARIABLE dat1 AS DATE. /* Starting date of week 1     */
  DEFINE VARIABLE wn   AS INT.  /* Week number , eg 45         */
  
  ASSIGN
    yr   = YEAR(indate)
    d1   = WEEKDAY(DATE( 1 , 1 , yr))
    dat1 = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                            DATE(1, 10, yr) - d1 )
    wn   = TRUNCATE((indate - dat1 + 7) / 7 , 0)
    yyyyww = yr * 100 + wn.
  
  IF wn < 1 THEN       /* Week 52 or 53 previous year ? */
  ASSIGN
    yr     = yr - 1
    d1     = WEEKDAY(DATE( 1 , 1 , yr))
    dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                              DATE(1, 10, yr) - d1 )
    wn     = TRUNCATE((indate - dat1 + 7) / 7 , 0)
    yyyyww = yr * 100 + wn.
  
  ELSE IF wn > 52 THEN  /* Week 53 this year or week 1 next year ? */
  ASSIGN
    yr     = yr + 1
    d1     = WEEKDAY(DATE( 1 , 1 , yr))
    yyyyww = IF d1 EQ 6 OR d1 EQ 7 OR d1 EQ 1
                THEN (yr - 1) * 100 + 53 ELSE yr * 100 + 1.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-writeFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE writeFile Procedure 
PROCEDURE writeFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:          
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER ipcButikkNavn AS CHAR NO-UNDO. 
  DEFINE INPUT PARAMETER ipdDate AS DATE NO-UNDO. 
  DEFINE INPUT PARAMETER ipiSalgsDager AS INT NO-UNDO. 
  DEFINE INPUT PARAMETER ipiAntSalgsDager AS INT NO-UNDO. 

  DEFINE VARIABLE cXLSTemplate AS CHAR INIT "server\excelrapport3Tempo.template.xls" NO-UNDO.
  DEFINE VARIABLE lOpenXLS AS LOGICAL INIT FALSE NO-UNDO. 

  cXLSTemplate = SEARCH(cXLSTemplate).
  FILE-INFO:FILENAME = cXLSTemplate. 
  cXLSTemplate = FILE-INFO:FULL-PATHNAME. 

  CREATE WIDGET-POOL "excel".
    
  RUN excelrapport3Tempo.p 
    (cXLSTemplate,
     lOpenXLS,
     ipcFileName,
     ipcButikkNavn,
     ipdDate, 
     ipiSalgsDager,
     ipiAntSalgsDager,
     TABLE saleThisMonth,
     TABLE prevYearMonth,
     TABLE saleByDay). 

  DELETE WIDGET-POOL "excel".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-addEmptyRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addEmptyRow Procedure 
FUNCTION addEmptyRow RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF CAN-FIND(FIRST syspara WHERE syspara.syshid      = iGeneralSysHid
                              AND syspara.sysgr       = iGeneralSysGr
                              AND syspara.beskrivelse = 'EmptyRow@BreakPoint') THEN 
  DO:  
    ix = ix + 1.
    PUT STREAM stA UNFORMATTED '' + CHR(10).
  END.

  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addSend) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addSend Procedure 
FUNCTION addSend RETURNS LOGICAL
  ( INPUT icReceiver AS CHAR,
    INPUT icSubject  AS CHAR,
    INPUT icBody     AS CHAR,
    INPUT icFile     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
RUN sendmail.p(cSubject,cTo,cMessage,cVedleggListe)
------------------------------------------------------------------------------*/
  FIND LAST ttSend NO-LOCK NO-ERROR.

  iNextSend = IF NOT AVAIL ttSend THEN 1 ELSE iNextSend + 1.
  CREATE ttSend.
  ASSIGN 
      ttSend.iSendNum = iNextSend
      ttSend.cSendReceiver = icReceiver
      ttSend.cSendSubject  = icSubject
      ttSend.cSendBody     = icBody
      ttSend.cSendFile     = icFile
      NO-ERROR.
  
  RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-buildFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION buildFilter Procedure 
FUNCTION buildFilter RETURNS CHARACTER
  (INPUT icTable AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cReturn AS CHAR NO-UNDO.
  DEF VAR cValue  AS CHAR NO-UNDO.
  
  ASSIGN
    cReturn = 'WHERE'
  .

  /*Sjekk om det finnes Hg og Vg*/
  chkVarGr().

  FOR EACH ttParam WHERE ttParam.cParamName BEGINS icTable:
    IF ttParam.cParamValue = ? OR ttParam.cParamValue = '' THEN NEXT.
    ASSIGN 
      cValue  = REPLACE(ttParam.cParamValue,'|',',')
      cReturn = cReturn + ' LOOKUP(' +  'STRING(' +  ttParam.cParamName + ')' + ',' + QUOTER(cValue) + ') GT 0 AND'
    .
  END.
  cReturn = RIGHT-TRIM(cReturn,'AND').
  RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-chkMandetoryFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION chkMandetoryFields Procedure 
FUNCTION chkMandetoryFields RETURNS LOGICAL
  (INPUT ipcWord AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ocWord AS CHAR NO-UNDO.
  DEF BUFFER bsyspara FOR syspara.

  FIND FIRST syspara WHERE syspara.syshid      = iGeneralSysHid
                       AND syspara.sysgr       = iGeneralSysGr
                       AND syspara.beskrivelse = ipcWord NO-LOCK NO-ERROR.
  IF NOT AVAIL syspara THEN
  DO TRANSACTION:
    FOR LAST bsyspara WHERE bsyspara.syshid = iGeneralSysHid AND bsyspara.sysgr = iGeneralSysGr NO-LOCK:
      LEAVE.
    END.
    CREATE syspara.
    ASSIGN 
      syspara.syshid  = iGeneralSysHid
      syspara.sysgr   = iGeneralSysGr
      syspara.paranr  = IF AVAIL bsyspara THEN bsyspara.paranr + 1 ELSE 1
      syspara.beskrivelse = ipcWord
      syspara.parameter1  = ipcWord
    .
  END.
  RETURN AVAIL syspara.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-chkVarGr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION chkVarGr Procedure 
FUNCTION chkVarGr RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cAvdNr AS CHAR NO-UNDO.
  DEF VAR cHg    AS CHAR NO-UNDO.
  DEF VAR cVg    AS CHAR NO-UNDO.

  /*Sjekk om artas.vg finnes som filter, hvis så, dropp ut og la det kjøres som normalt*/
  FIND FIRST ttParam WHERE ttParam.cParamName = 'artbas.Vg' NO-ERROR.
  IF AVAIL ttParam THEN RETURN ?.

  /*Hvis Hg ikke finnes, og avdelingnr ikke finnes, kjør som normalt*/
  FIND FIRST ttParam WHERE ttParam.cParamName = 'Hg' NO-ERROR.
  IF NOT AVAIL ttParam THEN
  DO:
    FIND FIRST ttParam WHERE ttParam.cParamName = 'Avdelingnr' NO-ERROR.
    IF NOT AVAIL ttParam THEN 
      RETURN ?.
  END.

  /*Kjør med Hg først, om denne ikke finnes, sjekk mot avdelingnr */
  FIND FIRST ttParam WHERE ttParam.cParamName = "Hg" NO-LOCK NO-ERROR.
  IF AVAIL ttParam THEN
  DO:
    cHg = ttParam.cParamValue.
    cHg = REPLACE(cHg,'|',',').
  END.
  ELSE 
  DO:
    FIND FIRST ttParam WHERE ttParam.cParamName = "Avdelingnr" NO-LOCK NO-ERROR.
    IF AVAIL ttParam THEN
    DO:
      cAvdNr = ttParam.cParamValue.
      cAvdNr = REPLACE(cAvdNr,'|',',').
      
      FOR EACH Huvgr NO-LOCK WHERE LOOKUP(STRING(huvgr.avdelingnr),cAvdNr) GT 0:
        cHg = cHg + ',' + STRING(Huvgr.Hg).
      END.
      cHg = LEFT-TRIM(TRIM(cHg,','),',').
    END.
  END.
  FOR EACH VarGr NO-LOCK WHERE LOOKUP(STRING(vargr.hg),cHg) GT 0:
    cVg = cVg + ',' + STRING(vargr.vg).
  END.
  cVg = LEFT-TRIM(TRIM(cVg,','),',').
  cVg = REPLACE(cVg,',','|').

  FIND FIRST ttParam WHERE ttParam.cParamName = 'artbas.Vg' NO-ERROR.
  IF AVAIL ttParam THEN
    ttParam.cParamValue = cVg.
  ELSE
    setParam('Filter','artbas.Vg',cVg).      


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convExcelFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION convExcelFormat Procedure 
FUNCTION convExcelFormat RETURNS CHARACTER
  (INPUT ihField AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(ihField) THEN 
  DO:
    CASE ihField:DATA-TYPE:
      WHEN 'CHAR' OR WHEN 'CHARACTER' THEN RETURN '@'.
      WHEN 'INT'  OR WHEN 'INTEGER'   THEN RETURN '0'.
      WHEN 'DEC'  OR WHEN 'DECIMAL'   THEN 
      DO:
       /*hvis % i label, så sett det til 0,0*/
       IF INDEX(ihField:LABEL,'%') GT 0 THEN RETURN '0,0'.
       ELSE RETURN '# ##0,00'.
      END.
      OTHERWISE RETURN ''.
    END CASE.
  END.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convFromJulianDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION convFromJulianDate Procedure 
FUNCTION convFromJulianDate RETURNS DATE
  (INPUT ipiNumDays AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN DATE('31/12/' + STRING(YEAR(TODAY) - 1)) + ipiNumDays.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION convText Procedure 
FUNCTION convText RETURNS CHARACTER
  (INPUT icText AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  CASE icText:
    WHEN 'År'    THEN RETURN 'aar'.
    WHEN 'Måned' THEN RETURN 'maned'.
    WHEN 'aar'   THEN RETURN 'År'.
    WHEN 'maned' THEN RETURN 'Måned'.
    OTHERWISE RETURN icText.
  END CASE.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-convToJulianDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION convToJulianDate Procedure 
FUNCTION convToJulianDate RETURNS INTEGER
  (INPUT idDate AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR iYear AS INT NO-UNDO.
  iYear = YEAR(idDate) - 1.

  RETURN (idDate - DATE((IF SESSION:DATE-FORMAT = "mdy" THEN '12/31/' ELSE '31/12/') + STRING(iYear))).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createParameter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createParameter Procedure 
FUNCTION createParameter RETURNS LOGICAL
  
  (INPUT icFieldGroup AS CHARACTER,
   INPUT icFieldName  AS CHARACTER,
   INPUT icFieldValue AS CHARACTER) :

  DEF VAR iNextSysPara AS INT    NO-UNDO.
  
  ihBuffer:FIND-LAST('where syshid=' + STRING(999999) + ' and sysgr = 1') NO-ERROR.
  iNextSysPara = IF ihBuffer:AVAIL THEN ihBuffer:BUFFER-FIELD('paranr'):BUFFER-VALUE ELSE 0.
  
  ihBuffer:BUFFER-CREATE().
  ASSIGN 
    iNextSysPara = iNextSysPara + 1
    ihBuffer:BUFFER-FIELD('sysHid'):BUFFER-VALUE       = STRING(999999)
    ihBuffer:BUFFER-FIELD('sysGr'):BUFFER-VALUE        = STRING(1)
    ihBuffer:BUFFER-FIELD('paranr'):BUFFER-VALUE       = STRING(iNextSysPara)
    ihBuffer:BUFFER-FIELD('hjelpetekst1'):BUFFER-VALUE = icFieldGroup
    ihBuffer:BUFFER-FIELD('parameter1'):BUFFER-VALUE   = icFieldName
    ihBuffer:BUFFER-FIELD('parameter2'):BUFFER-VALUE   = icFieldValue
  .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getAlphaSeqNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAlphaSeqNo Procedure 
FUNCTION getAlphaSeqNo RETURNS CHARACTER
  ( INPUT iiSeqNo AS INT ) :
/*------------------------------------------------------------------------------
  Purpose: Return corresponding letter for a sequence number 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ocValue     AS CHAR NO-UNDO.

IF iiSeqNo < 0 OR iiSeqNo > 80 THEN RETURN "".
ELSE IF iiSeqNo < 27 THEN
  ocValue = CHR(64 + iiSeqNo).
ELSE IF iiSeqNo < 54 THEN
  ocValue = "A" + CHR(64 - 26 + iiSeqNo).
ELSE IF iiSeqNo < 81 THEN
  ocValue = "B" + CHR(64 - 26 + iiSeqNo).

RETURN ocValue.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBreakText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBreakText Procedure 
FUNCTION getBreakText RETURNS CHARACTER
  ( INPUT icField AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    Benyttes for å kunne endre felt for oppslag, eks. Om Vg kommer inn, ønsker vi VgBeskr som visning
------------------------------------------------------------------------------*/
  CASE icField:
    WHEN 'Avdelingnr' THEN RETURN 'Avdelingnavn'.
    WHEN 'Hg'         THEN RETURN 'HgBeskr'.
    WHEN 'Vg'         THEN RETURN 'VgBeskr'.
    WHEN 'RAvdNr'     THEN RETURN 'RAvdBeskrivelse'.
    WHEN 'LevKod'     THEN RETURN 'Levnamn'.
    WHEN 'LevNamn'    THEN RETURN 'Levnamn'.
    WHEN 'ProdNr'     THEN RETURN 'Beskrivelse'.
    WHEN 'VMid'       THEN RETURN 'VMbeskrivelse'.
  END CASE.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDateCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDateCode Procedure 
FUNCTION getDateCode RETURNS INTEGER
  ( INPUT ipdDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN YEAR(ipdDate) * 1000 + (ipdDate - DATE(12,31,YEAR(ipdDate) - 1)).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDateFromDateCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDateFromDateCode Procedure 
FUNCTION getDateFromDateCode RETURNS DATE

  (INPUT ipdDateCode AS INT):
   
   DEFINE VARIABLE yyyy AS INT NO-UNDO. 
   yyyy = INT(ipdDateCode / 1000).
   RETURN (DATE(12,31,yyyy - 1) +  (ipdDateCode - yyyy * 1000)). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFirstDayInMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFirstDayInMonth Procedure 
FUNCTION getFirstDayInMonth RETURNS DATE
  ( INPUT ipdDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN DATE(MONTH(ipdDate),1,YEAR(ipdDate)).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFormattedAvdeling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFormattedAvdeling Procedure 
FUNCTION getFormattedAvdeling RETURNS CHARACTER
  ( INPUT ipiAvdeling AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN STRING(ipiAvdeling,"9999").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFormattedYearMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFormattedYearMonth Procedure 
FUNCTION getFormattedYearMonth RETURNS INTEGER
  ( INPUT ipdDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN INT(YEAR(ipdDate) * 1000 + MONTH(ipdDate)).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLastSale) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLastSale Procedure 
FUNCTION getLastSale RETURNS DATE
  ( INPUT iiButikk AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND LAST stLinje WHERE stlinje.sttypeid = 'ButStat'
                      AND stlinje.butik    = iiButikk
                      AND stlinje.aar      > 1970
                      AND stlinje.perid    = 'Dag' NO-LOCK NO-ERROR.
  IF AVAIL stLinje THEN 
    RETURN DATE('01/01/' + STRING(stlinje.aar)) + (stlinje.perlinnr - 1).
  ELSE RETURN ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParam Procedure 
FUNCTION getParam RETURNS CHARACTER (INPUT icGroup AS CHAR,
                   INPUT icName  AS CHAR):
  FIND FIRST ttParam WHERE ttParam.cParamGroup = icGroup AND ttParam.cParamName = icName NO-ERROR.
  RETURN IF AVAIL ttParam THEN ttParam.cParamValue ELSE ''.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWeekNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWeekNum Procedure 
FUNCTION getWeekNum RETURNS INTEGER
  ( INPUT idSomeDate     AS DATE,
    INPUT iiOutputLength AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iWeekNum AS INT NO-UNDO.
  
  RUN WeekNum (idSomeDate, OUTPUT iWeekNum).
  
  IF iWeekNum NE ? THEN
    CASE iiOutputLength:
      WHEN 2 THEN RETURN INT(SUBSTR(STRING(iWeekNum),5)).
      WHEN 4 THEN RETURN INT(SUBSTR(STRING(iWeekNum),3)).
      OTHERWISE RETURN iWeekNum.
    END CASE.
  ELSE RETURN 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-makePeriodHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION makePeriodHeader Procedure 
FUNCTION makePeriodHeader RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cYear  AS CHAR EXTENT 2 FORMAT '9999' NO-UNDO.
  DEF VAR iMonthWeek AS INT  EXTENT 2 FORMAT '99'   NO-UNDO. /*Either month or week depend on cPerId*/
  
  DEF VAR cReturn AS CHAR NO-UNDO.
  
  ASSIGN 
    cYear[1]  = SUBSTRING(STRING(iFraAarPerLinNr),1,4)
    cYear[2]  = SUBSTRING(STRING(iTilAarPerLinNr),1,4)
    iMonthWeek[1] = INT(SUBSTRING(STRING(iFraAarPerLinNr),5))
    iMonthWeek[2] = INT(SUBSTRING(STRING(iTilAarPerLinNr),5))
  .
  CASE cperid:
    WHEN 'aar'    THEN cReturn = cYear[1] + ' - ' + cYear[2].
    WHEN 'maned'  THEN cReturn = cYear[1] + '-' + STRING(iMonthWeek[1],'99') + ' - ' + cYear[2] + '-' + STRING(iMonthWeek[2],'99') .
    WHEN 'uke'    THEN cReturn = cYear[1] + '-' + STRING(iMonthWeek[1],'99') + ' - ' + cYear[2] + '-' + STRING(iMonthWeek[2],'99') .
    WHEN 'dag'    THEN cReturn = STRING(convFromJulianDate(iFraAarPerLinNr)) + ' - ' + STRING(convFromJulianDate(iTilAarPerLinNr)).
  END CASE.
  RETURN cReturn.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NumberOfSalesDaysThisMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NumberOfSalesDaysThisMonth Procedure 
FUNCTION NumberOfSalesDaysThisMonth RETURNS INTEGER

  (INPUT  ipdDay AS DATE,
   INPUT  ipiButikk AS INT ):

   DEFINE VARIABLE iCnt AS INT NO-UNDO. 
   DEFINE VARIABLE iNumDays AS INT NO-UNDO. 
   DEFINE VARIABLE iDay AS INT NO-UNDO. 
   DEFINE VARIABLE dDate AS DATE NO-UNDO. 
   DEFINE VARIABLE d31decFGaar AS DATE NO-UNDO.
   
   DEFINE BUFFER stLinje FOR stlinje. 

   DO iCnt = 1 TO DAY(ipdDay): 

     dDate = DATE(MONTH(ipdDay),iCnt,YEAR(ipdDay)).
     d31decFGaar = DATE(12,31,YEAR(dDate) - 1).
     iDay = YEAR(dDate) * 1000 + (dDate - d31decFGaar).
                                             
     FIND FIRST stlinje WHERE 
                stlinje.butik       = ipiButikk AND
                stlinje.sttypeid    = "BUTSTAT" AND
                stlinje.perid       = "DAG"     AND
                stlinje.aarperlinnr = iDay 
                NO-LOCK NO-ERROR.

     IF AVAIL stLinje THEN iNumDays = iNumdays + 1. 
   END.
   RETURN iNumDays.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParam Procedure 
FUNCTION setParam RETURNS LOGICAL
  ( INPUT icParamGroup AS CHAR,
    INPUT icParamName  AS CHAR,
    INPUT icParamValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ttParam WHERE ttParam.cParamGroup = icParamGroup 
                 AND ttParam.cParamName  = icParamName 
               NO-ERROR.
  IF AVAIL ttParam THEN
    ttParam.cParamValue = icParamValue.
  ELSE
  DO:
    CREATE ttParam.
    ASSIGN 
      ix                  = ix + 1
      ttParam.iParamID    = ix
      ttParam.cParamGroup = icParamGroup
      ttParam.cParamName  = icParamName
      ttParam.cParamValue = icParamValue
    .
  END.
  RETURN TRUE.  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION showParam Procedure 
FUNCTION showParam RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FOR EACH ttParam:
    MESSAGE ttParam.cParamName SKIP 
           ttParam.cParamValue VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TotalNumberOfSalesDaysThisMonth) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TotalNumberOfSalesDaysThisMonth Procedure 
FUNCTION TotalNumberOfSalesDaysThisMonth RETURNS INTEGER

(INPUT ipdDay AS DATE,
 INPUT ipiButikk AS INT ):
   
   DEF VAR iNumDays AS INT NO-UNDO. 
   DEF VAR iMonth AS INT NO-UNDO. 
   DEF VAR tDay AS DATE NO-UNDO.
   DEF VAR fDay AS DATE NO-UNDO. 
   DEF VAR iWeekDay AS INT NO-UNDO. 
   DEF VAR firstSunday AS DATE NO-UNDO. 
   DEF VAR nextSunday AS DATE NO-UNDO. 
   DEF VAR iCnt AS INT INIT 1 NO-UNDO. 
  
   iMonth = MONTH(ipdDay). 
   tDay   = DATE(iMonth,25,YEAR(ipdDay)) + 10. 
   tDay   = DATE(MONTH(tDay),1,YEAR(tDay)). 
   fDay   = DATE(iMonth,1,YEAR(ipdDay)). 
  
   iNumDays = tDay - fDay. 
   iWeekDay = WEEKDAY(fDay).
  
   firstSunday = ((7 - (iWeekday - 1) MOD 7))  + fDay.
   NextSunday = firstSunday + 7. 
  
   DO WHILE NextSunday LE tDay:
      iCnt = iCnt + 1. 
      NextSunday = nextSunday + 7.
   END.
  
   RETURN iNumDays - iCnt. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-translateWord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION translateWord Procedure 
FUNCTION translateWord RETURNS CHARACTER
  (INPUT ipcWord AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ocWord AS CHAR NO-UNDO.
  DEF BUFFER bsyspara FOR syspara.

  FIND FIRST syspara WHERE syspara.syshid      = iGeneralSysHid
                       AND syspara.sysgr       = iTranslateSysGr
                       AND syspara.beskrivelse = ipcWord NO-LOCK NO-ERROR.
  IF NOT AVAIL syspara THEN
  DO TRANSACTION:
    FOR LAST bsyspara WHERE bsyspara.syshid = iGeneralSysHid AND bsyspara.sysgr = iTranslateSysGr NO-LOCK:
      LEAVE.
    END.
    CREATE syspara.
    ASSIGN 
      syspara.syshid  = iGeneralSysHid
      syspara.sysgr   = iTranslateSysGr
      syspara.paranr  = IF AVAIL bsyspara THEN bsyspara.paranr + 1 ELSE 1
      syspara.beskrivelse = ipcWord
      syspara.parameter1  = ipcWord
      ocWord              = ipcWord
    .
  END.
  ELSE ocWord = syspara.parameter1.

  RETURN ocWord.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

