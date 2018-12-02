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

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cFileName       AS CHAR   NO-UNDO.
DEF VAR cCurrNumFormat  AS CHAR   NO-UNDO.
DEF VAR cColumn         AS CHAR EXTENT 10 NO-UNDO.
DEF VAR cTransParam     AS CHAR   NO-UNDO.
DEF VAR cFieldList      AS CHAR   NO-UNDO.
DEF VAR cLabelList      AS CHAR   NO-UNDO.
DEF VAR cRowidList      AS CHAR   NO-UNDO.
DEF VAR cQueryPhrase    AS CHAR   NO-UNDO.
DEF VAR i               AS INT    NO-UNDO.
DEF VAR iCount          AS INT    NO-UNDO.
DEF VAR iWeek           AS INT    NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hFIeld          AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getAlphaSeqNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAlphaSeqNo Procedure 
FUNCTION getAlphaSeqNo RETURNS CHARACTER
  ( INPUT iiSeqNo AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldHandle Procedure 
FUNCTION getFieldHandle RETURNS HANDLE (INPUT icFieldName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRowidWherePhrase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRowidWherePhrase Procedure 
FUNCTION getRowidWherePhrase RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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
RUN InitializeObject.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InitializeObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject Procedure 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                                                                     
ASSIGN 
  cCurrNumFormat  = SESSION:NUMERIC-FORMAT
/*   cTransParam     = DYNAMIC-FUNCTION("InputParam" IN SOURCE-PROCEDURE). */
  cTransParam     = icParam
  cFieldList      = ENTRY(1,cTransParam,CHR(1))
  cLabelList      = ENTRY(2,cTransParam,CHR(1))
  cRowidList      = ENTRY(3,cTransParam,CHR(1))
  cQueryPhrase    = ENTRY(4,cTransParam,CHR(1))
  cQueryPhrase    = REPLACE(cQueryPhrase,"and ArtikkelNr < 0","")
.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER VarebokLinje:HANDLE,BUFFER ArtBas:HANDLE,BUFFER Varemerke:HANDLE,BUFFER sasong:HANDLE,BUFFER regnskapsavdeling:HANDLE,BUFFER VarGr:HANDLE).

/*Using LOOKUP instead of CAN-DO*/
IF cRowidList NE '' THEN
  hQuery:QUERY-PREPARE("FOR EACH VarebokLinje NO-LOCK WHERE " + getRowidWherePhrase()
                       + ",FIRST ArtBas OF VarebokLinje NO-LOCK,FIRST Varemerke OF ArtBas NO-LOCK,FIRST sasong OF Vareboklinje,FIRST regnskapsavdeling OF artbas, FIRST VarGr OF Vareboklinje"
                       + IF ENTRY(2,cQueryPhrase,";") NE "" THEN 
                           " BY LevNamn BY " + ENTRY(2,cQueryPhrase,";")
                         ELSE " BY VarebokLinje.LevNamn BY VarebokLinje.Vg BY VarebokLinje.Pris"
                        ).
ELSE
  hQuery:QUERY-PREPARE("FOR EACH VarebokLinje NO-LOCK " 
                     + ENTRY(1,cQueryPhrase,";") 
                     + ",FIRST ArtBas OF VarebokLinje NO-LOCK,FIRST Varemerke OF ArtBas NO-LOCK,FIRST sasong OF Vareboklinje,FIRST regnskapsavdeling OF artbas, FIRST VarGr OF Vareboklinje"
                     + IF ENTRY(2,cQueryPhrase,";") NE "" THEN 
/*                      + IF NUM-ENTRIES(cQueryPhrase,";") > 1 THEN */
                         " BY LevNamn BY " + ENTRY(2,cQueryPhrase,";")
                       ELSE " BY VarebokLinje.LevNamn BY VarebokLinje.Vg BY VarebokLinje.Pris"
                      ).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

IF NOT AVAIL VarebokLinje THEN DO:
  ocReturn = "Varebok ikke funnet".
  RETURN.
END.

cFileName       = SESSION:TEMP-DIR + STRING(VarebokLinje.VarebokNr) + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".xls".

/* MESSAGE hQuery:PREPARE-STRING Skip(3)  */
/*   cFileName skip                       */
/*                                        */
/* VIEW-AS ALERT-BOX INFO BUTTONS OK.     */

FIND VarebokHode WHERE VarebokHode.VarebokNr = VarebokLinje.VarebokNr NO-LOCK NO-ERROR.

OUTPUT TO VALUE(cFileName).

PUT UNFORMATTED REPLACE(cLabelList,',',CHR(9)) CHR(10).
/*
fPrisExMVA     = Pris / (1 + Mva% / 100).

CASE icChangedField:
  WHEN "InnkjopsPris" THEN 
    ASSIGN Varekost       = InnkjopsPris - InnkjopsPris * forhRab% / 100
           supVarekost    = InnkjopsPris - InnkjopsPris * supRab% / 100
           KjedeInnkPris  = InnkjopsPris - InnkjopsPris * KjedeRab% / 100
           .
  WHEN "Varekost"      THEN forhRab%      = (InnkjopsPris - Varekost) / InnkjopsPris * 100.
  WHEN "supVarekost"   THEN supRab%       = (InnkjopsPris - supVarekost) / InnkjopsPris * 100.
  WHEN "forhRab%"      THEN Varekost      = InnkjopsPris - InnkjopsPris * forhRab% / 100.
  WHEN "supRab%"       THEN supVarekost   = InnkjopsPris - InnkjopsPris * supRab% / 100.           
  WHEN "KjedeRab%"     THEN KjedeInnkPris = InnkjopsPris - InnkjopsPris * KjedeRab% / 100.           
  WHEN "KjedeInnkPris" THEN KjedeRab%     = (InnkjopsPris - KjedeInnkPris) / InnkjopsPris * 100.
  WHEN "KjedeSupRab%"     THEN KjedeSupInnkPris = InnkjopsPris - InnkjopsPris * KjedeSupRab% / 100.           
  WHEN "KjedeSupInnkPris" THEN KjedeSupRab%     = (InnkjopsPris - KjedeSupInnkPris) / InnkjopsPris * 100.
END CASE.

*/
iCount = 1. /*Header*/
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCount = iCount + 1.
  DO i = 1 TO NUM-ENTRIES(cFieldList):
    hField = GetFieldHandle(ENTRY(i,cFieldList)).
    
    IF i = LOOKUP('rowid',cFieldList) THEN 
    DO:
      PUT UNFORMATTED STRING(hQuery:GET-BUFFER-HANDLE(1):ROWID) CHR(9).
    END.
    ELSE 
    IF i = LOOKUP('varekost',cFieldList) THEN /*Nto.innpr.forh*/
    DO:
      cColumn[1] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('innkjopspris',cFieldList)).
      cColumn[2] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('forhrab%',cFieldList)).
      PUT UNFORMATTED '=' cColumn[1] STRING(iCount) ' - (' cColumn[1] STRING(iCount) 
                        + ' * '  cColumn[2] STRING(iCount) ' / 100)' CHR(9).
    END.
/*     ELSE                                                                                 */
/*     IF i = LOOKUP('kjederab%',cFieldList) THEN /*Nto.innpr.forh*/                        */
/*     DO:                                                                                  */
/*       cColumn[1] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('innkjopspris',cFieldList)).  */
/*       cColumn[2] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('kjedeinnkpris',cFieldList)). */
/*       PUT UNFORMATTED '=(' cColumn[1] STRING(iCount) ' - ' cColumn[2] STRING(iCount)     */
/*                         + ') / '  cColumn[1] STRING(iCount) ' * 100' CHR(9).             */
/*     END.                                                                                 */
    ELSE 
    IF i = LOOKUP('kjedeinnkpris',cFieldList) THEN /*Nto.innpr.forh*/
    DO:
      cColumn[1] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('innkjopspris',cFieldList)).
      cColumn[2] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('kjederab%',cFieldList)).
      PUT UNFORMATTED '=' cColumn[1] STRING(iCount) ' - ' cColumn[1] STRING(iCount) 
                        + ' * '  cColumn[2] STRING(iCount) ' / 100' CHR(9).
    END.
/*     ELSE                                                                                    */
/*     IF i = LOOKUP('kjedesuprab%',cFieldList) THEN /*Nto.innpr.forh*/                        */
/*     DO:                                                                                     */
/*       cColumn[1] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('innkjopspris',cFieldList)).     */
/*       cColumn[2] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('kjedesupinnkpris',cFieldList)). */
/*       PUT UNFORMATTED '=(' cColumn[1] STRING(iCount) ' - ' cColumn[2] STRING(iCount)        */
/*                         + ') / '  cColumn[1] STRING(iCount) ' * 100' CHR(9).                */
/*     END.                                                                                    */
    ELSE 
    IF i = LOOKUP('supvarekost',cFieldList) THEN /*Nto.innpr.forh*/
    DO:
      cColumn[1] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('innkjopspris',cFieldList)).
      cColumn[2] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('suprab%',cFieldList)).
      PUT UNFORMATTED '=' cColumn[1] STRING(iCount) ' - (' cColumn[1] STRING(iCount) 
                        + ' * '  cColumn[2] STRING(iCount) ' / 100)' CHR(9).
    END.
    ELSE 
    IF i = LOOKUP('kjedesupinnkpris',cFieldList) THEN /*Nto.innpr.forh*/
    DO:
      cColumn[1] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('innkjopspris',cFieldList)).
      cColumn[2] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('kjedesuprab%',cFieldList)).
      PUT UNFORMATTED '=' cColumn[1] STRING(iCount) ' - (' cColumn[1] STRING(iCount) 
                        + ' * '  cColumn[2] STRING(iCount) ' / 100)' CHR(9).
    END.
    ELSE
    IF i = LOOKUP('levuke',cFieldList) THEN 
    DO:
      hField = GetFieldHandle('LevDato1').
      PUT UNFORMATTED (IF getWeekNum(hField:BUFFER-VALUE,0) = 0 OR getWeekNum(hField:BUFFER-VALUE,0) = ? THEN '0' ELSE STRING(getWeekNum(hField:BUFFER-VALUE,0),'9999') + STRING(getWeekNum(hField:BUFFER-VALUE,2),'999')) CHR(9).
    END.
    ELSE
    IF i = LOOKUP('forhKalkyle',cFieldList) THEN 
    DO:
      cColumn[1] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('pris',cFieldList)).
      cColumn[2] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('varekost',cFieldList)).
      PUT UNFORMATTED '=' cColumn[1] STRING(iCount) ' / ' cColumn[2] STRING(iCount) CHR(9).
    END.
    ELSE 
    IF i = LOOKUP('supKalkyle',cFieldList) THEN 
    DO:
      cColumn[1] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('pris',cFieldList)).
      cColumn[2] = DYNAMIC-FUNCTION("getAlphaSeqNo",LOOKUP('supvarekost',cFieldList)).
      PUT UNFORMATTED '=' + cColumn[1] + STRING(iCount) + ' / ' + cColumn[2] + STRING(iCount) CHR(9).
    END.
    ELSE 
    DO:        
      IF VALID-HANDLE(hField) THEN 
      DO:
        IF hField:DATA-TYPE = 'CHARACTER' THEN
          PUT UNFORMATTED (IF hField:BUFFER-VALUE = ? THEN '' ELSE QUOTER(STRING(hField:BUFFER-VALUE))) CHR(9) .
        ELSE
          PUT UNFORMATTED (IF hField:BUFFER-VALUE = ? THEN '' ELSE STRING(hField:BUFFER-VALUE)) CHR(9) .
      IF hField:NAME = 'kampanjeuker' THEN
      END.
      ELSE
        PUT UNFORMATTED 'N/A' CHR(9).
    END.
  END.
  PUT UNFORMATTED CHR(10).

  hQuery:GET-NEXT().
END.
      
OUTPUT CLOSE.
DELETE OBJECT hQuery.

SESSION:NUMERIC-FORMAT = cCurrNumFormat.

IF iCount = 0 THEN
  ocReturn = "Vareboken inneholder ingen artikler".
ELSE 
  ASSIGN ocReturn = cFileName + "|" + STRING(iCount)
         obOk     = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

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

&IF DEFINED(EXCLUDE-getFieldHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldHandle Procedure 
FUNCTION getFieldHandle RETURNS HANDLE (INPUT icFieldName AS CHAR):
  
  DEF VAR ix      AS INT    NO-UNDO.
  DEF VAR hReturn AS HANDLE NO-UNDO.

  DO ix = 1 TO hQuery:NUM-BUFFERS:
    hReturn = hQuery:GET-BUFFER-HANDLE(ix):BUFFER-FIELD(icFieldName) NO-ERROR.
    IF VALID-HANDLE(hReturn) THEN RETURN hReturn.
  END.
  RETURN hReturn.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRowidWherePhrase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRowidWherePhrase Procedure 
FUNCTION getRowidWherePhrase RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR i            AS INT NO-UNDO.
  DEF VAR cReturnValue AS CHAR NO-UNDO.
  cReturnValue = ''.
  DO i = 1 TO NUM-ENTRIES(cRowidList):
    cReturnValue = cReturnValue + ' ROWID(vareboklinje) = TO-ROWID(' + QUOTER(ENTRY(i,cRowidList)) + ') OR '. 
  END.
  RETURN RIGHT-TRIM(RIGHT-TRIM(cReturnValue,' '),'OR').

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
      WHEN 0 THEN RETURN INT(SUBSTR(STRING(iWeekNum),1,4)). /*Need to get the year*/
      OTHERWISE RETURN iWeekNum.
    END CASE.
  ELSE RETURN 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

