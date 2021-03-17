DEF VAR bok           AS LOG NO-UNDO.
DEF VAR tth           AS HANDLE NO-UNDO.
DEF VAR ix            AS INT  NO-UNDO.
DEF VAR cFieldPosList AS CHAR NO-UNDO.
DEF VAR cTableName    AS CHAR NO-UNDO.

DEF TEMP-TABLE ttSort1 NO-UNDO
    FIELD iSeq   AS INT
    FIELD iSeq2  AS INT
    FIELD cText1 AS CHAR
    FIELD cText2 AS CHAR
    .
DEF BUFFER bttSort1 FOR ttSort1.

DEF TEMP-TABLE ttAttribute     
    FIELD hObject                 AS HANDLE
    FIELD cName                   AS CHAR    /* Display, Update, Input, SortColumn, Desc ... */
    FIELD cValue                  AS CHAR
    INDEX idxObject  hObject
    INDEX idxName    cName
    .
DEF BUFFER bttAttribute FOR ttAttribute.

DEF TEMP-TABLE ttBufferDef NO-UNDO
    FIELD cBuffer     AS CHAR
    FIELD iBufferNum  AS INT
    FIELD cCriteria   AS CHAR
    INDEX idxBufferNum IS PRIMARY iBufferNum
    .
DEF TEMP-TABLE ttFieldDef NO-UNDO
    FIELD iBufferNum       AS INT
    FIELD cFieldDef        AS CHAR
    FIELD cFieldName       AS CHAR
    FIELD cClientFieldName AS CHAR
    FIELD iFieldPos        AS INT
    FIELD bCalculated      AS LOG
    FIELD bVisible         AS LOG
    FIELD iFieldSeq        AS INT
    INDEX idxField IS PRIMARY iBufferNum iFieldSeq
    INDEX idxFieldPos iFieldPos
    .
DEF BUFFER bttFieldDef FOR ttFieldDef.

  FUNCTION setAttribute RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR,
    INPUT icValue   AS CHAR )  FORWARD.


FUNCTION doReposList RETURNS CHAR (INPUT icAll  AS CHAR,
                                    INPUT icMove AS CHAR) FORWARD.

FUNCTION rebuildList RETURNS CHAR (INPUT icList  AS CHAR,
                                   INPUT icField AS CHAR,
                                   INPUT icTo    AS INT)   FORWARD.

/* RUN ConstructBrowseOrQuery(?,'artbas;!artikkelnr;RAvdNr;hg|Label@1;vg;levkod;beskr;LevFargKod'                  */
/*                                     + ',stlinje;antSolgt;Vvarekost;VerdiSolgt'                                  */
/*                                     + ';+DBkroner|decimal|->>>>>>>9.99|DB kroner'                               */
/*                                     + ';+DBpros|decimal|->>>9.99|Brutto %;butik|Butikknr@3'                     */
/*                                     + ',huvgr;avdelingnr@5'                                                     */
/*                                     + ',avdeling;avdelingnavn|Tust|x(50)'                                       */
/*                            ,'WHERE true, first stlinje of artbas, first huvgr, first avdeling','',OUTPUT bOk).  */
/*                                                                                                                 */
/* RUN makeTT.                                                                                                     */

PROCEDURE ConstructBrowseOrQuery :
/*------------------------------------------------------------------------------
  Purpose:    Decompose parameters to NewBrowse or NewQuery functions.
              For the browse these attributes can be set for the placeholder rectangle (design object) 
              before NewBrowse is called:
              - getrecordcoount         (if the initial query should return records)
              - querysort               (if the initial query should return records)
              - querydesc               (if the initial query should return records)
              - 1stSortColumn           (when returning data and multiple sort columns)
              - 1stSortColumnDesc       (when returning data and multiple sort columns. Value: desc)
              - 2ndSortColumn           (when returning data and multiple sort columns)
              - 2ndSortColumnDesc       (when returning data and multiple sort columns. Value: desc)
              - 3rdSortColumn           (when returning data and multiple sort columns)
              - 3rdSortColumnDesc       (when returning data and multiple sort columns. Value: desc)
              - 4thSortColumn           (when returning data and multiple sort columns)
              - 4thSortColumnDesc       (when returning data and multiple sort columns. Value: desc)
              - temptablehandle         (if the browse should be built on a local temp-table)
                If this attribute is set also remeber to set "uselocaldata","yes"
              - calcfieldproc           (name of server procedure containing calculated field procs)
              - periodfield<fieldname>,<periods>
                Example: wmqy (week month quarter year - if none is set the periode is assumed m (month))
                Usage is for enabling distinct queries (and totals) on periodes based on a date field in the database 
              - querystatfields         (comma-separated list of fields that should be accumulated on the server)
              - distinctcolumns         (comma-separated list of distinct columns)
              - accumfields             (comma-separated list of accumulated fields pr distinct row) 
              
              A query doesn't have a placeholder and hence any pre-create attributes must be set
              as a comma-separated list (see NewQuery)
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihObject            AS HANDLE NO-UNDO.
DEF INPUT  PARAM icBuffersAndFields  AS CHAR   NO-UNDO.
DEF INPUT  PARAM icQueryCriteria     AS CHAR   NO-UNDO.
DEF INPUT  PARAM icOtherProp         AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOk                AS LOG    NO-UNDO.

DEF VAR iy                     AS INT    NO-UNDO.
DEF VAR ix                     AS INT    NO-UNDO.
DEF VAR cQueryJoin             AS CHAR   NO-UNDO.
DEF VAR cBuffersAndFields      AS CHAR   NO-UNDO.
DEF VAR cViewBuffersAndFields  AS CHAR   NO-UNDO.
DEF VAR cBufferFieldDefs       AS CHAR   NO-UNDO.
DEF VAR cBaseTableFilterFields AS CHAR   NO-UNDO.
DEF VAR cBaseTableFields       AS CHAR   NO-UNDO.
DEF VAR cAllViewFields         AS CHAR   NO-UNDO.
DEF VAR cAllViewFieldsReal     AS CHAR   NO-UNDO.
DEF VAR cAllCalcFields         AS CHAR   NO-UNDO.
DEF VAR cAllJoinViewFields     AS CHAR   NO-UNDO.
DEF VAR cNoDisplayFields       AS CHAR   NO-UNDO.
DEF VAR cCalcFieldProc         AS CHAR   NO-UNDO.
DEF VAR cSortMap               AS CHAR   NO-UNDO.
DEF VAR cBufferFields          AS CHAR   NO-UNDO.
DEF VAR cViewBufferFieldDefs   AS CHAR   NO-UNDO.
DEF VAR cBufferList            AS CHAR   NO-UNDO.
DEF VAR cFieldReposList        AS CHAR   NO-UNDO.
DEF VAR cDbFieldReposList      AS CHAR   NO-UNDO.
DEF VAR iFieldIx               AS INT    NO-UNDO.
DEF VAR iFldPos                AS INT    NO-UNDO.
DEF VAR cNotExistList          AS CHAR   NO-UNDO.
DEF VAR cDistinctList          AS CHAR   NO-UNDO.
DEF VAR cAccumList             AS CHAR   NO-UNDO.
DEF VAR cLocalCalcFieldList    AS CHAR   NO-UNDO.

DO ix = 1 TO LENGTH(icQueryCriteria):
  IF SUBSTR(icQueryCriteria,ix,6) = "first " OR
     SUBSTR(icQueryCriteria,ix,5) = "last " OR
     SUBSTR(icQueryCriteria,ix,5) = "each " 
     THEN DO:
    DO iy = ix TO ix - 10 BY -1:
      IF SUBSTR(icQueryCriteria,iy,1) = "," THEN LEAVE.
    END.
    LEAVE.
  END.
END.
IF iy > 0 THEN DO:
  setAttribute(ihObject,"querywhere",SUBSTR(icQueryCriteria,1,iy - 1)).
  setAttribute(ihObject,"queryjoin",SUBSTR(icQueryCriteria,iy)).
END.
ELSE
  setAttribute(ihObject,"querywhere",icQueryCriteria).

/* Interprete initial comma-separated parameter string (for the browse the parameters can be set on the design object): */
DO ix = 1 TO NUM-ENTRIES(icOtherProp):
  IF ENTRY(1,ENTRY(ix,icOtherProp),"|") = "SORT" THEN DO:
    setAttribute(ihObject,"querysort",REPLACE(ENTRY(1,ENTRY(2,ENTRY(ix,icOtherProp),"|")," "),";"," ")).
    IF NUM-ENTRIES(ENTRY(2,ENTRY(ix,icOtherProp),"|")," ") > 1 THEN
      setAttribute(ihObject,"querydesc",REPLACE(ENTRY(2,ENTRY(2,ENTRY(ix,icOtherProp),"|")," "),";"," ")).
  END.
  ELSE IF ENTRY(1,ENTRY(ix,icOtherProp),"|") = "TEMP-TABLE" THEN DO:
    setAttribute(ihObject,"temptablehandle",ENTRY(2,ENTRY(ix,icOtherProp),"|")).
    setAttribute(ihObject,"uselocaldata","yes").
  END.
  ELSE IF ENTRY(1,ENTRY(ix,icOtherProp),"|") = "getrecordcount" THEN 
    setAttribute(ihObject,"getrecordcount","yes").
  ELSE IF ENTRY(1,ENTRY(ix,icOtherProp),"|") = "calcfieldproc" THEN DO:
    cCalcFieldProc = ENTRY(2,ENTRY(ix,icOtherProp),"|").
    setAttribute(ihObject,"calcfieldproc",cCalcFieldProc).
    DYNAMIC-FUNCTION("setCalcFieldProc",cCalcFieldProc).
  END.
  ELSE IF ENTRY(1,ENTRY(ix,icOtherProp),"|") = "periodfield" THEN 
    setAttribute(ihObject,"addperiod" + ENTRY(1,ENTRY(2,ENTRY(ix,icOtherProp),"|")," "),ENTRY(2,ENTRY(2,ENTRY(ix,icOtherProp),"|")," ")).
END.

EMPTY TEMP-TABLE ttBufferDef.
EMPTY TEMP-TABLE ttFieldDef.

DO ix = 1 TO NUM-ENTRIES(icBuffersAndFields):
  CREATE ttBufferDef.  
  ASSIGN ttBufferDef.cBuffer     = ENTRY(1,ENTRY(ix,icBuffersAndFields),";")
         ttBufferDef.iBufferNum  = ix
         ttBufferDef.cCriteria   = TRIM(ENTRY(ix,icQueryCriteria))
         .
  IF ttBufferDef.cBuffer BEGINS "NOT EXIST " THEN 
    ASSIGN ttBufferDef.cBuffer    = TRIM(SUBSTR(ttBufferDef.cBuffer,11))
           cNotExistList          = cNotExistList + ENTRY(1,ttBufferDef.cBuffer,";") + ",".

  DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,icBuffersAndFields),";"):
    CREATE ttFieldDef.
    ASSIGN ttFieldDef.iBufferNum  = ix
           ttFieldDef.cFieldDef   = TRIM(ENTRY(1,ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"@"))
           ttFieldDef.iFieldPos   = IF NUM-ENTRIES(ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"@") > 1 THEN 
                                      INT(ENTRY(2,ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"@"))
                                    ELSE 0
           ttFieldDef.cFieldName  = IF ttFieldDef.cFieldDef BEGINS "!+" OR ttFieldDef.cFieldDef BEGINS "+!" THEN SUBSTR(ENTRY(1,ttFieldDef.cFieldDef,"|"),3)
                                    ELSE IF ttFieldDef.cFieldDef BEGINS "!" OR ttFieldDef.cFieldDef BEGINS "+" THEN SUBSTR(ENTRY(1,ttFieldDef.cFieldDef,"|"),2)
                                    ELSE ENTRY(1,ttFieldDef.cFieldDef,"|")
           ttFieldDef.bCalculated = ttFieldDef.cFieldDef BEGINS "+" OR
                                    ttFieldDef.cFieldDef BEGINS "!+"
           ttFieldDef.bVisible    = NOT ttFieldDef.cFieldDef BEGINS "!" AND
                                    NOT ttFieldDef.cFieldDef BEGINS "+!"
           iFieldIx               = iFieldIx + 10
           ttFieldDef.iFieldSeq   = iFieldIx
           .
    IF R-INDEX(ttFieldDef.cFieldName,"[") > 0 THEN
      ttFieldDef.cFieldName = SUBSTR(ttFieldDef.cFieldName,1,R-INDEX(ttFieldDef.cFieldName,"[") - 1).
    IF ttFieldDef.bCalculated AND NUM-ENTRIES(ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"|") GE 4 AND ENTRY(4,ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"|") = "" THEN
      cLocalCalcFieldList = cLocalCalcFieldList + ttFieldDef.cFieldName + ",".
    ELSE IF ttFieldDef.bCalculated AND NUM-ENTRIES(ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"|") LT 4 THEN 
      cLocalCalcFieldList = cLocalCalcFieldList + ttFieldDef.cFieldName + ",".
    IF ttFieldDef.cFieldName BEGINS "distinct " THEN
      ASSIGN ttFieldDef.cFieldName = TRIM(SUBSTR(ttFieldDef.cFieldName,10))
             cDistinctList = cDistinctList + ttFieldDef.cFieldName + ",".
    IF ttFieldDef.cFieldName BEGINS "accum " THEN
      ASSIGN ttFieldDef.cFieldName = TRIM(SUBSTR(ttFieldDef.cFieldName,7))
             cAccumList = cAccumList + ttFieldDef.cFieldName + ",".
  END.
  IF ix > 1 THEN DO:
    setAttribute(ihObject,"bufferjoin" + ttBufferDef.cBuffer,"," + TRIM(ENTRY(ix,icQueryCriteria))).
    cQueryJoin = cQueryJoin + "," + TRIM(ENTRY(ix,icQueryCriteria)).
  END.
END.
IF cNotExistList NE "" THEN
  setAttribute(ihObject,"noexistbuffers",TRIM(cNotExistList,",")).
IF cDistinctList NE "" THEN
  setAttribute(ihObject,"distinctcolumns",TRIM(cDistinctList,",")).
IF cAccumList NE "" THEN
  setAttribute(ihObject,"accumfields",TRIM(cAccumList,",")).
IF cLocalCalcFieldList NE "" THEN
  setAttribute(ihObject,"localcalcfields",TRIM(cLocalCalcFieldList,",")).

FOR EACH ttAttribute
    WHERE ttAttribute.hObject = ihObject
      AND ttAttribute.cName   BEGINS "addperiod":
  FIND FIRST ttFieldDef
       WHERE ttFieldDef.cFieldName = SUBSTR(ttAttribute.cName,10)
         AND ttFieldDef.bVisible   
         AND NOT ttFieldDef.bCalculated
       NO-ERROR.
  IF AVAIL ttFieldDef THEN DO ix = 1 TO LENGTH(ttAttribute.cValue):
    IF CAN-DO("w,m,q,y",SUBSTR(ttAttribute.cValue,ix,1)) THEN DO:
      CREATE bttFieldDef.
      ASSIGN bttFieldDef.iBufferNum  = ttFieldDef.iBufferNum
             bttFieldDef.bVisible    = YES
             bttFieldDef.bCalculated = YES.
      CASE SUBSTR(ttAttribute.cValue,ix,1):
        WHEN "w" THEN 
          ASSIGN bttFieldDef.iFieldSeq   = ttFieldDef.iFieldSeq + 2
                 bttFieldDef.cFieldName  = "jb_week_" + ttFieldDef.cFieldName
                 bttFieldDef.cFieldDef   = "+" + bttFieldDef.cFieldName + "|CHARACTER|x(6)|JB_weeknum(" + ttFieldDef.cFieldName + ")".
        WHEN "m" THEN 
          ASSIGN bttFieldDef.iFieldSeq   = ttFieldDef.iFieldSeq + 4
                 bttFieldDef.cFieldName  = "jb_month_" + ttFieldDef.cFieldName
                 bttFieldDef.cFieldDef   = "+" + bttFieldDef.cFieldName + "|CHARACTER|x(6)|JB_month(" + ttFieldDef.cFieldName + ")".
        WHEN "q" THEN 
          ASSIGN bttFieldDef.iFieldSeq   = ttFieldDef.iFieldSeq + 6
                 bttFieldDef.cFieldName  = "jb_quarter_" + ttFieldDef.cFieldName
                 bttFieldDef.cFieldDef   = "+" + bttFieldDef.cFieldName + "|CHARACTER|x(6)|JB_quarter(" + ttFieldDef.cFieldName + ")".
        WHEN "y" THEN 
          ASSIGN bttFieldDef.iFieldSeq   = ttFieldDef.iFieldSeq + 8
                 bttFieldDef.cFieldName  = "jb_year_" + ttFieldDef.cFieldName
                 bttFieldDef.cFieldDef   = "+" + bttFieldDef.cFieldName + "|CHARACTER|x(6)|JB_year(" + ttFieldDef.cFieldName + ")".

      END CASE. 
      IF ix = 1 THEN cSortMap = cSortMap + "," + bttFieldDef.cFieldName + ";" + ttFieldDef.cFieldName + ",".
    END.
  END.
END.
setAttribute(ihObject,"sortmap",TRIM(cSortMap,",")).

FOR EACH ttBufferDef:
  ASSIGN cBuffersAndFields     = cBuffersAndFields + ttBufferDef.cBuffer
         cViewBuffersAndFields = cViewBuffersAndFields + ttBufferDef.cBuffer
         cBufferFieldDefs      = ""
         cBufferFields         = ""
         cViewBufferFieldDefs  = ""
         cBufferList           = cBufferList + ttBufferDef.cBuffer + ",".

  IF ttBufferDef.iBufferNum = 1 THEN
    setAttribute(ihObject,"basetable",ttBufferDef.cBuffer).

  FOR EACH ttFieldDef 
      WHERE ttBufferDef.iBufferNum = ttFieldDef.iBufferNum
      BY ttFieldDef.iFieldSeq:

    ASSIGN cBufferFieldDefs = cBufferFieldDefs + ttFieldDef.cFieldDef + ";"
           cBufferFields    = cBufferFields + ttFieldDef.cFieldName + ",".
  
    IF CAN-DO(cAllViewFields,ttFieldDef.cFieldName) THEN DO:
      setAttribute(ihObject,"fieldbuffer" + ttFieldDef.cFieldName + STRING(ttFieldDef.iBufferNum)
                           ,ttBufferDef.cBuffer).
      setAttribute(ihObject,"orgdbfield" + ttFieldDef.cFieldName + STRING(ttFieldDef.iBufferNum),ttFieldDef.cFieldName).
      ttFieldDef.cClientFieldName = ttFieldDef.cFieldName + STRING(ttFieldDef.iBufferNum).
    END.
    ELSE DO:
      setAttribute(ihObject,"fieldbuffer" + ttFieldDef.cFieldName,ttBufferDef.cBuffer).
      setAttribute(ihObject,"orgdbfield" + ttFieldDef.cFieldName,ttFieldDef.cFieldName).
      ttFieldDef.cClientFieldName = ttFieldDef.cFieldName.
    END.

    IF ttFieldDef.bVisible AND NOT ttFieldDef.bCalculated AND ttBufferDef.iBufferNum = 1 THEN
      cBaseTableFilterFields = cBaseTableFilterFields + ttFieldDef.cFieldName + ",".
  
    IF ttBufferDef.iBufferNum = 1 AND NOT ttFieldDef.bCalculated THEN
      cBaseTableFields = cBaseTableFields + ttFieldDef.cFieldName + ",".

    IF ttFieldDef.bVisible THEN
      ASSIGN cAllViewFields       = cAllViewFields       + ttFieldDef.cFieldName + ","
             cViewBufferFieldDefs = cViewBufferFieldDefs + ttFieldDef.cFieldDef + ";"
             .
        
    IF ttFieldDef.bCalculated THEN DO:
      cAllCalcFields = cAllCalcFields + ttFieldDef.cFieldName + ",".

      IF NUM-ENTRIES(ttFieldDef.cFieldDef,"|") > 3 AND ENTRY(4,ttFieldDef.cFieldDef,"|") MATCHES "*(ROWID*" THEN
        setAttribute(ihObject,"calcphrase" + ttFieldDef.cFieldName,ENTRY(4,ttFieldDef.cFieldDef,"|")).
    END.
    
    IF NOT ttFieldDef.bVisible THEN
      cNoDisplayFields = cNoDisplayFields + ttFieldDef.cFieldName + ",".
    
    IF ttBufferDef.iBufferNum > 1 AND ttFieldDef.bVisible THEN
      cAllJoinViewFields = cAllJoinViewFields + ",".

  END.

  ASSIGN cBufferFieldDefs      = TRIM(cBufferFieldDefs,";")
         cViewBufferFieldDefs  = TRIM(cViewBufferFieldDefs,";")
         cBuffersAndFields     = TRIM(cBuffersAndFields + ";" + cBufferFieldDefs,";") + ","
         cViewBuffersAndFields = TRIM(cViewBuffersAndFields + ";" + cViewBufferFieldDefs,";") + ","
         .
  
  setAttribute(ihObject,"bufferfields" + ttBufferDef.cBuffer,TRIM(cBufferFields,",")).
END.
/* TEMP-TABLE ttBufferDef:HANDLE:WRITE-XML('file','c:\tmp\slettme_bufferdef_data.xml').  */
/* TEMP-TABLE ttFieldDef:HANDLE:WRITE-XML('file','c:\tmp\slettme_fielddef_data.xml').    */

ix = 0.
EMPTY TEMP-TABLE ttSort1.
FOR EACH ttFieldDef 
    WHERE ttFieldDef.bVisible
    BY ttFieldDef.iFieldSeq:
  ix = ix + 1.
  cFieldPosList = cFieldPosList + ttFieldDef.cClientFieldName + ','. 
  IF ttFieldDef.iFieldPos > 0 THEN 
  DO:
    CREATE ttSort1.
    ASSIGN ttSort1.iSeq   = ttFieldDef.iFieldPos            /* "To" position */
           ttSort1.iSeq2  = ix                              /* "From" position */
           ttSort1.cText1 = ttFieldDef.cClientFieldName + ";"
           ttSort1.cText2 = ttFieldDef.cFieldName + ";".
  END.
END.
cFieldPosList = TRIM(cFieldPosList).

FOR EACH ttSort1 
  BY ttSort1.iSeq 
/*   BY ttSort1.iSeq DESC */
  :
  ASSIGN cFieldReposList   = cFieldReposList + ttSort1.cText1 + STRING(ttSort1.iSeq2) + ";" + STRING(ttSort1.iSeq) + ","
         cDbFieldReposList = cDbFieldReposList + ttSort1.cText2 + STRING(ttSort1.iSeq2) + ";" + STRING(ttSort1.iSeq) + ",".
END.
  cFieldPosList = doReposList(TRIM(cFieldPosList,','),TRIM(cDbFieldReposList,',')).
END PROCEDURE.

FUNCTION setAttribute RETURNS LOGICAL
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR,
    INPUT icValue   AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hBuffer AS HANDLE NO-UNDO.

FIND FIRST ttAttribute 
     WHERE ttAttribute.hObject = ihObject
       AND ttAttribute.cName   = icName
     NO-ERROR.
IF NOT AVAIL ttAttribute THEN DO:
  CREATE ttAttribute.
  ASSIGN ttAttribute.hObject = ihObject
         ttAttribute.cName   = icName.
END.
IF icName = "querywhere" AND icValue NE ttAttribute.cValue THEN DO:
  ttAttribute.cValue = icValue.
  RETURN TRUE.
END.
ELSE IF icName = "buffersandfields" THEN DO:
  ttAttribute.cValue = REPLACE(icValue,"!","").
  RETURN TRUE.
END.

ttAttribute.cValue = icValue.

RETURN TRUE.   

END FUNCTION.

PROCEDURE makeTT:
  DEF OUTPUT PARAMETER otth AS HANDLE NO-UNDO.

  DEF VAR bh            AS HANDLE NO-UNDO.
  DEF VAR bhf           AS HANDLE NO-UNDO.

  DEF VAR cTable        AS CHAR NO-UNDO.
  DEF VAR cField        AS CHAR NO-UNDO.
  DEF VAR cClientField  AS CHAR NO-UNDO.
  DEF VAR cTableList    AS CHAR NO-UNDO.
  DEF VAR cFieldContent AS CHAR NO-UNDO.
  DEF VAR cCalcField    AS CHAR NO-UNDO.
  DEF VAR cHiddenField  AS CHAR NO-UNDO.

  /*Used to create the fields*/
  DEF VAR cDataType     AS CHAR NO-UNDO.
  DEF VAR cLabel        AS CHAR NO-UNDO.
  DEF VAR cFormat       AS CHAR NO-UNDO.

  DEF VAR cBuffer       AS CHAR NO-UNDO.
  DEF VAR ix            AS INT  NO-UNDO.
  
  DEF BUFFER bufttFieldDef FOR ttFieldDef.

  cTableName = DYNAMIC-FUNCTION('getAttribute',?,'ttTableName').
  IF cTableName = '' THEN 
  DO:
    MESSAGE 'Attribute ttTableName må settes for å navngi temp-table'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    LEAVE.
  END.
  
  CREATE TEMP-TABLE tth.
  FOR EACH ttBufferDef:
    cTable = ttBufferDef.cBuffer.
    IF NUM-ENTRIES(cTable,'_') GT 1 THEN
      cTable = ENTRY(2,cTable,'_'). 
    CREATE BUFFER bh FOR TABLE cTable.
    FOR EACH ttFieldDef WHERE ttFieldDef.iBufferNum = ttBufferDef.iBufferNum:
      ASSIGN 
        cField        = ttFieldDef.cFieldName
        cClientField  = ttFieldDef.cClientFieldName
        cFieldContent = ttFieldDef.cFieldDef
      .
/*       MESSAGE 'TABLE:' cTable SKIP 'Field:' cField SKIP 'Clientfield:' cFieldContent */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                           */
      IF ttFieldDef.bCalculated THEN
      DO:
        CASE NUM-ENTRIES(cFieldContent,'|'):
          WHEN 4 THEN tth:ADD-NEW-FIELD(cClientField,ENTRY(2,cFieldContent,'|'),0,ENTRY(3,cFieldContent,'|'),'',ENTRY(4,cFieldContent,'|')).    
          OTHERWISE 
              MESSAGE 'Feil antall felt i kalkulert felt'
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END CASE.
      END.
      ELSE
      IF VALID-HANDLE(bh) THEN
      DO: 
        ASSIGN 
          cDataType = bh:BUFFER-FIELD(cField):DATA-TYPE
          cLabel    = bh:BUFFER-FIELD(cField):LABEL
          cFormat   = bh:BUFFER-FIELD(cField):FORMAT
        .
        IF INDEX(cFieldContent,'|') GT 0 THEN
        DO:
          IF NUM-ENTRIES(cFieldContent,'|') GE 2 THEN
            cLabel    = ENTRY(2,cFieldContent,'|').
          ELSE IF NUM-ENTRIES(cFieldContent,'|') GE 3 THEN
            cFormat   = ENTRY(3,cFieldContent,'|').
          ELSE 
            MESSAGE 'Too many delimiters in field ' cField 
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*                       MESSAGE 'Field:' cClientField SKIP cLabel */
/*                         VIEW-AS ALERT-BOX INFO BUTTONS OK.      */
            tth:ADD-NEW-FIELD(cClientField
                             ,cDataType
                             ,bh:BUFFER-FIELD(cField):EXTENT
                             ,cFormat
                             ,bh:BUFFER-FIELD(cField):INIT
                             ,cLabel).
        END.
        IF INDEX(cTable,'_') GT 1 THEN 
        DO:
          tth:ADD-NEW-FIELD(cClientField
                           ,cDataType
                           ,bh:BUFFER-FIELD(cField):EXTENT
                           ,cFormat
                           ,bh:BUFFER-FIELD(cField):INIT
                           ,cLabel).
        END.
        ELSE
        DO:
          bhf = bh:BUFFER-FIELD(cField):HANDLE NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
            tth:ADD-NEW-FIELD(cClientField
                              ,cDataType
                              ,bh:BUFFER-FIELD(cField):EXTENT
                              ,cFormat
                              ,bh:BUFFER-FIELD(cField):INIT
                              ,cLabel).
          ELSE
            tth:ADD-LIKE-FIELD(bhf:NAME,bhf).
        END.
      END.
    END.

  END.
  /*Add table specific*/
  IF cTableName BEGINS 'ttstlinje' THEN
  DO:
    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'butik') 
       AND CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'artikkelnr') THEN 
    DO:
      tth:ADD-NEW-INDEX("idxUnique",TRUE,TRUE).
      tth:ADD-INDEX-FIELD('idxUnique','butik').
      tth:ADD-INDEX-FIELD('idxUnique','artikkelnr').
    END.
    
    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'artikkelnr') THEN
    DO:
      tth:ADD-NEW-INDEX("idxArtikkelNr",FALSE,FALSE).
      tth:ADD-INDEX-FIELD('idxArtikkelNr','artikkelnr').
    END.
    
    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'butik') THEN
    DO:
      tth:ADD-NEW-INDEX('idxButik',FALSE,FALSE).
      tth:ADD-INDEX-FIELD('idxButik','butik').
    END.
    
    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'antsolgt') THEN
    DO:
      tth:ADD-NEW-INDEX("idxAntSolgt",FALSE,FALSE).
      tth:ADD-INDEX-FIELD('idxAntSolgt','antSolgt').
    END.
    
    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'Vvarekost') THEN
    DO:
      tth:ADD-NEW-INDEX("idxVvarekost",FALSE,FALSE).
      tth:ADD-INDEX-FIELD('idxVvarekost','Vvarekost').
    END.

    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'Verdisolgt') THEN
    DO:
      tth:ADD-NEW-INDEX("idxVerdiSolgt",FALSE,FALSE).
      tth:ADD-INDEX-FIELD('idxVerdiSolgt','VerdiSolgt').
    END.
    
    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'DBkroner') THEN
    DO:
      tth:ADD-NEW-INDEX("idxDBkroner",FALSE,FALSE).
      tth:ADD-INDEX-FIELD('idxDBkroner','DBkroner').
    END.
    
    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'avdelingnr')
       AND CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'Hg')
       AND CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'Vg') THEN
    DO:
      tth:ADD-NEW-INDEX("idxAvdelingnr",FALSE,FALSE).
      tth:ADD-INDEX-FIELD('idxAvdelingnr','avdelingnr').
      tth:ADD-INDEX-FIELD('idxAvdelingnr','Hg').
      tth:ADD-INDEX-FIELD('idxAvdelingnr','Vg').
    END.
    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'levnamn') THEN
    DO:
      tth:ADD-NEW-INDEX("idxlevnamn",FALSE,FALSE).
      tth:ADD-INDEX-FIELD('idxlevnamn','levnamn').
    END.
    
    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'vmid') THEN
    DO:
      tth:ADD-NEW-INDEX("idxvmid",FALSE,FALSE).
      tth:ADD-INDEX-FIELD('idxvmid','vmid').
    END.
    
    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'prodnr') THEN
    DO:
      tth:ADD-NEW-INDEX("idxprodnr",FALSE,FALSE).
      tth:ADD-INDEX-FIELD('idxprodnr','prodnr').
    END.
    
    IF CAN-FIND(FIRST bufttFieldDef WHERE ttFieldDef.cFieldName = 'RAvdnr') THEN
    DO:
      tth:ADD-NEW-INDEX("idxRAvdNr",FALSE,FALSE).
      tth:ADD-INDEX-FIELD('idxRAvdNr','RAvdNr').
    END.

  END.

  tth:TEMP-TABLE-PREPARE(cTableName).
  otth = tth.
  
END PROCEDURE.

FUNCTION doReposList RETURNS CHAR (INPUT icAll  AS CHAR,
                                   INPUT icMove AS CHAR):
  DEF VAR cField AS CHAR NO-UNDO.
  DEF VAR iFrom  AS INT NO-UNDO.
  DEF VAR iTo    AS INT NO-UNDO.
  
  DO ix = 1 TO NUM-ENTRIES(icMove,','):

    ASSIGN 
      cField   = ENTRY(1,ENTRY(ix,icMove),';')
      iFrom    = INT(ENTRY(2,ENTRY(ix,icMove),';'))
      iTo      = INT(ENTRY(3,ENTRY(ix,icMove),';'))
    .
    icAll = rebuildList(icAll,cField,iTo).
  END.
  DYNAMIC-FUNCTION('setAttribute',?,'ReposList',icAll).
END FUNCTION.

FUNCTION rebuildList RETURNS CHAR (INPUT icList  AS CHAR,
                                   INPUT icField AS CHAR ,
                                   INPUT iiTo    AS INT):
  DEF VAR cSelect AS CHAR VIEW-AS SELECTION-LIST SIZE 1 BY 1.
  DEF FRAME f cSelect .
  
  ASSIGN 
    cSelect:LIST-ITEMS = icList
  .
  
  cSelect:DELETE(cSelect:ENTRY(cSelect:LOOKUP(icField))).
  cSelect:SCROLL-TO-ITEM(iiTo).
  cSelect:INSERT(icField,iiTo).
  RETURN cSelect:LIST-ITEMS.

END FUNCTION.

FUNCTION getAttribute RETURNS CHARACTER
  ( INPUT ihObject  AS HANDLE,
    INPUT icName    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttAttribute 
     WHERE ttAttribute.hObject = ihObject
       AND ttAttribute.cName   = icName
     NO-ERROR.
IF AVAIL ttAttribute THEN
  RETURN ttAttribute.cValue.
ELSE RETURN "".   

END FUNCTION.
