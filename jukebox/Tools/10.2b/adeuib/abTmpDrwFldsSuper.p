/* Super proc to hold attributes for additional attirbutes for field selection
   Created: 03.04.08 by brynjar@chemistry.no
---------------------------------------------------------------------*/
DEF VAR cDbFieldUsage       AS CHAR NO-UNDO.
DEF VAR cOptions            AS CHAR NO-UNDO.
DEF VAR cClipBoardFormat    AS CHAR NO-UNDO.
DEF VAR cClipBoardDelimiter AS CHAR NO-UNDO.
DEF VAR cFieldPrefix        AS CHAR NO-UNDO.
DEF VAR cFieldHelp          AS CHAR NO-UNDO.
DEF VAR bRemoveDuplicates   AS LOG  NO-UNDO.

DEF TEMP-TABLE ttFieldDef
    FIELD cDb         AS CHAR
    FIELD cTable      AS CHAR
    FIELD cField      AS CHAR
    FIELD cNewField   AS CHAR
    FIELD cDefVar     AS CHAR
    FIELD iTableSeq   AS INT
    FIELD iSeq        AS INT
    FIELD cHelp       AS CHAR
    FIELD cTooltip    AS CHAR
    FIELD bTaken      AS LOG 
    INDEX idxField IS PRIMARY cField 
    .


/* setters: */
FUNCTION setDbFieldUsage RETURNS LOGICAL (INPUT icDbFieldUsage AS CHAR):
  IF NUM-ENTRIES(icDbFieldUsage,"|") > 1 THEN
    ASSIGN cOptions      = ENTRY(2,icDbFieldUsage,"|")
           cDbFieldUsage = ENTRY(1,icDbFieldUsage,"|").
  ELSE
    cDbFieldUsage = icDbFieldUsage.
    RETURN YES.
END FUNCTION.

FUNCTION setFieldSelectClipBoardFormat RETURNS LOGICAL (INPUT icClipBoardFormat AS CHAR):
  cClipBoardFormat = icClipBoardFormat.
  RETURN YES.
END FUNCTION.

FUNCTION setFieldSelectClipBoardDelimiter RETURNS LOGICAL (INPUT icClipBoardDelimiter AS CHAR):
  cClipBoardDelimiter = icClipBoardDelimiter.
  RETURN YES.
END FUNCTION.

FUNCTION setFieldSelectFieldPrefix RETURNS LOGICAL (INPUT icFieldPrefix AS CHAR):
  cFieldPrefix = icFieldPrefix.
  RETURN YES.
END FUNCTION.

FUNCTION setFieldSelectFieldHelp RETURNS LOGICAL (INPUT icFieldHelp AS CHAR):
  cFieldHelp = icFieldHelp.
  RETURN YES.
END FUNCTION.

FUNCTION setFieldSelectRemoveDuplicates RETURNS LOGICAL (INPUT ibRemoveDuplicates AS LOG):
  bRemoveDuplicates = ibRemoveDuplicates.
  RETURN YES.
END FUNCTION.
                                 
/* getters: */
FUNCTION getDbFieldUsage RETURNS CHARACTER():
  RETURN cDbFieldUsage.
END FUNCTION.

FUNCTION getUseDbPrefix RETURNS LOGICAL():
  RETURN cDbFieldUsage = "abdefault".
END FUNCTION.

FUNCTION getDbFieldUsageOptions RETURNS CHARACTER():
  RETURN cOptions.
END FUNCTION.

FUNCTION getDbFieldToVarTooltip RETURNS CHARACTER(INPUT icFieldName AS CHAR):
  FIND FIRST ttFieldDef
       WHERE ttFieldDef.cField = icFieldName
       NO-ERROR.
  IF AVAIL ttFieldDef AND ttFieldDef.cTooltip NE ? THEN
    RETURN ttFieldDef.cTooltip.
  ELSE IF NOT AVAIL ttFieldDef THEN DO:
    FIND FIRST ttFieldDef
         WHERE ttFieldDef.cNewField = icFieldName
         NO-ERROR.
    IF AVAIL ttFieldDef AND ttFieldDef.cTooltip NE ? THEN
      RETURN ttFieldDef.cTooltip.
  END.
  RETURN "".
END.

FUNCTION getDbFieldToVarHelp RETURNS CHARACTER(INPUT icFieldName AS CHAR):
  FIND FIRST ttFieldDef
       WHERE ttFieldDef.cNewField = icFieldName
       NO-ERROR.
  IF AVAIL ttFieldDef AND ttFieldDef.cHelp NE ? THEN
    RETURN ttFieldDef.cHelp.
  ELSE IF NOT AVAIL ttFieldDef THEN DO:
    FIND FIRST ttFieldDef
         WHERE ttFieldDef.cField = icFieldName
         NO-ERROR.
    IF AVAIL ttFieldDef AND ttFieldDef.cHelp NE ? THEN
      RETURN ttFieldDef.cHelp.
  END.
  RETURN "".
END.

FUNCTION getDbFieldCorrVarName RETURNS CHARACTER(INPUT icFieldName AS CHAR):
  FIND FIRST ttFieldDef
       WHERE ttFieldDef.cField = icFieldName
         AND (IF NOT bRemoveDuplicates THEN NOT ttFieldDef.bTaken ELSE TRUE)
       NO-ERROR.
  IF AVAIL ttFieldDef AND ttFieldDef.cNewField NE "" THEN DO:
    ttFieldDef.bTaken = YES.
    RETURN ttFieldDef.cNewField.
  END.
  ELSE IF NOT bRemoveDuplicates THEN DO: 
    FIND FIRST ttFieldDef
         WHERE ttFieldDef.cField BEGINS icFieldName + "-"
           AND NOT ttFieldDef.bTaken
         NO-ERROR.
    IF AVAIL ttFieldDef AND ttFieldDef.cNewField NE "" THEN DO:
      ttFieldDef.bTaken = YES.
      RETURN ttFieldDef.cNewField.
    END.
  END.
  RETURN icFieldName.
END.


FUNCTION creFillInDefFromDb RETURN CHARACTER
  (INPUT icFieldList AS CHAR):

  DEF VAR ocFieldDef  AS CHAR NO-UNDO.
  
  DEF VAR ix          AS INT    NO-UNDO.
  DEF VAR hQuery      AS HANDLE NO-UNDO.
  DEF VAR cCurrDb     AS CHAR   NO-UNDO.
  DEF VAR cNewField   AS CHAR   NO-UNDO.
  DEF VAR h_fieldBuf  AS HANDLE NO-UNDO.
  DEF VAR h_fileBuf   AS HANDLE NO-UNDO.
  DEF VAR cDelim      AS CHAR   NO-UNDO.
  DEF VAR cClipList   AS CHAR   NO-UNDO.
  DEF VAR iDuplCnt    AS INT    NO-UNDO.
  DEF VAR cCurrTable  AS CHAR   NO-UNDO.
  DEF VAR iTabSeq     AS INT    NO-UNDO.
        
  DEF BUFFER bttFieldDef FOR ttFieldDef.
    
  CASE cClipBoardFormat:
    WHEN "comma"     THEN cDelim = ",".
    WHEN "space"     THEN cDelim = " ".
    WHEN "semicolon" THEN cDelim = ";".
    WHEN "other"     THEN cDelim = cClipBoardDelimiter.
  END CASE.
  
  icFieldList = REPLACE(icFieldList,"Temp-Tables.","").

  DO ix = 1 TO NUM-ENTRIES(icFieldList):
    CREATE ttFieldDef.
    
    IF NUM-ENTRIES(ENTRY(ix,icFieldList),".") = 3 THEN
      ASSIGN ttFieldDef.cDb     = ENTRY(1,ENTRY(ix,icFieldList),".")
             ttFieldDef.cTable  = ENTRY(2,ENTRY(ix,icFieldList),".")
             ttFieldDef.cField  = ENTRY(3,ENTRY(ix,icFieldList),".")
             .
    ELSE 
      ASSIGN ttFieldDef.cTable  = ENTRY(1,ENTRY(ix,icFieldList),".")
             ttFieldDef.cField  = ENTRY(2,ENTRY(ix,icFieldList),".")
             .
    iDuplCnt = 0.
    FOR EACH bttFieldDef
        WHERE bttFieldDef.cField = ttFieldDef.cField
          AND bttFieldDef.cTable NE ttFieldDef.cTable:
      iDuplCnt = iDuplCnt + 1.
    END.
  
    IF ttFieldDef.cTable NE cCurrTable THEN
      iTabSeq = iTabSeq + 1.
    ASSIGN ttFieldDef.cNewField = cFieldPrefix + ttFieldDef.cField
           ttFieldDef.iSeq      = ix
           ttFieldDef.iTableSeq = iTabSeq
           cCurrTable           = ttFieldDef.cTable
           .
                                                                                                 
   IF iDuplCnt > 0 THEN                                                   
     ttFieldDef.cNewField = ttFieldDef.cNewField + "-" + STRING(iDuplCnt + 1). 
  END.
  
  FOR EACH ttFieldDef
      BREAK BY ttFieldDef.cDb
            BY ttFieldDef.iTableSeq
/*             BY ttFieldDef.cTable */
            BY ttFieldDef.iSeq:

    IF FIRST-OF(ttFieldDef.cDb) THEN DO:
      cCurrDb = ttFieldDef.cDb + (IF ttFieldDef.cDb NE "" THEN "." ELSE "").
      CREATE BUFFER h_fieldBuf FOR TABLE cCurrDb + "_field".
      CREATE BUFFER h_fileBuf  FOR TABLE cCurrDb + "_file".
      CREATE QUERY hQuery.
      hQuery:SET-BUFFERS(h_fieldBuf,h_fileBuf).
    END.
    IF cClipBoardFormat = "jukebox" THEN DO:
      IF FIRST-OF(ttFieldDef.iTableSeq) THEN
        cClipList = cClipList + (IF cClipList NE "" THEN '  + ",' ELSE "") + ttFieldDef.cTable + '"' + CHR(10).
      cClipList = cClipList + '    + ";' + ttFieldDef.cField + '"' + CHR(10).
    END.
  
    hQuery:QUERY-PREPARE("FOR EACH _field WHERE _field-name = '" 
                       + (IF INDEX(ttFieldDef.cField,"[") > 0 THEN SUBSTR(ttFieldDef.cField,1,INDEX(ttFieldDef.cField,"[") - 1)
                          ELSE ttFieldDef.cField) + "' NO-LOCK"
                       + ",FIRST _file OF _field WHERE _file-name = '" + ttFieldDef.cTable + "' NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
  
    IF h_fieldBuf:AVAIL THEN DO:
      
      IF h_fieldBuf:BUFFER-FIELD("_extent"):BUFFER-VALUE > 0 THEN 
        ttFieldDef.cNewField = REPLACE(REPLACE(ttFieldDef.cNewField,"[","_"),"]","").

      ttFieldDef.cDefVar = 'DEFINE VARIABLE ' + ttFieldDef.cNewField 
                         + ' AS ' + h_fieldBuf:BUFFER-FIELD("_data-type"):BUFFER-VALUE
                         + ' FORMAT "' + h_fieldBuf:BUFFER-FIELD("_format"):BUFFER-VALUE + '"'
                         + (IF h_fieldBuf:BUFFER-FIELD("_initial"):BUFFER-VALUE NE "" AND h_fieldBuf:BUFFER-FIELD("_initial"):BUFFER-VALUE NE ? THEN
                              (' INITIAL ' + (IF h_fieldBuf:BUFFER-FIELD("_data-type"):BUFFER-VALUE = "CHARACTER" THEN '"' ELSE "")
                              + h_fieldBuf:BUFFER-FIELD("_initial"):BUFFER-VALUE
                              + (IF h_fieldBuf:BUFFER-FIELD("_data-type"):BUFFER-VALUE = "CHARACTER" THEN '"' ELSE ""))
                            ELSE "")
                         + ' LABEL "' + (IF h_fieldBuf:BUFFER-FIELD("_label"):BUFFER-VALUE NE ? THEN h_fieldBuf:BUFFER-FIELD("_label"):BUFFER-VALUE ELSE "") + '"'
                         .
    
      ttFieldDef.cDefVar = ttFieldDef.cDefVar + ".".
    
      CASE cFieldHelp:
        WHEN "fh_t"      THEN ttFieldDef.cToolTip = h_fieldBuf:BUFFER-FIELD("_help"):BUFFER-VALUE.
        WHEN "fh_h"      THEN ttFieldDef.cHelp = h_fieldBuf:BUFFER-FIELD("_help"):BUFFER-VALUE.
        WHEN "fh_ht"     THEN ASSIGN ttFieldDef.cHelp = h_fieldBuf:BUFFER-FIELD("_help"):BUFFER-VALUE
                                     ttFieldDef.cTooltip = cHelp.
        WHEN "fh_h-de_t" THEN ASSIGN ttFieldDef.cHelp = h_fieldBuf:BUFFER-FIELD("_help"):BUFFER-VALUE
                                     ttFieldDef.cTooltip = h_fieldBuf:BUFFER-FIELD("_desc"):BUFFER-VALUE.
      END CASE.
    END.
    ELSE 
      MESSAGE PROGRAM-NAME(1) SKIP
              "Field " ttFieldDef.cField " was not found in data dictionary"
              VIEW-AS ALERT-BOX INFORMATION.
  
    IF LAST-OF(ttFieldDef.cDb) THEN DO:
      DELETE OBJECT hQuery.
      DELETE OBJECT h_fieldBuf.
      DELETE OBJECT h_fileBuf.
    END.
  END.
           
  FOR EACH ttFieldDef 
      BY ttFieldDef.iSeq:
    ocFieldDef = ocFieldDef + ttFieldDef.cDefVar + CHR(10) + CHR(10).
    IF cClipBoardFormat NE "jukebox" AND cClipBoardFormat NE "" THEN
      cClipList = cClipList + (IF cClipList NE "" THEN cDelim ELSE "") + ttFieldDef.cNewField.
  END.
  ocFieldDef = ocFieldDef + CHR(10).
  
  IF cClipList NE "" THEN
    CLIPBOARD:VALUE = cClipList.
  
  RETURN ocFieldDef.
END FUNCTION.
