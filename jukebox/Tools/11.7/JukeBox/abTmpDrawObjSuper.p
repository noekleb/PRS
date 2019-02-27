&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description : Super proc to hold attributes for additional attirbutes for field selection

   Created:  03.04.08 by brynjar@chemistry.no
   Modified: 09.01.13 by brynjar@chemistry.no
           - added code-generation used by adeuib\_drwflds.p
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{adeuib/uniwidg.i}           /* Universal Widget TEMP-TABLE definition   */
{adeuib/sharvars.i}          /* Define _h_win, _frmx, _next_draw etc.    */
{adeuib/layout.i}            /* Layout temp-table definitions            */  
{adeuib/custwidg.i}          /* Custom Object and Palette definitions    */
{src/adm2/globals.i}
{adecomm/oeideservice.i}
{adeuib/triggers.i}          /* trigger code */
{adeuib/brwscols.i}

{jukebox/ttJBoxTool.i}
{jukebox/tt_field.i NEW}


DEF VAR cDbFieldUsage        AS CHAR NO-UNDO.
DEF VAR cOptions             AS CHAR NO-UNDO.
DEF VAR cClipBoardFormat     AS CHAR NO-UNDO.
DEF VAR cClipBoardDelimiter  AS CHAR NO-UNDO.
DEF VAR cFieldPrefix         AS CHAR NO-UNDO.
DEF VAR cFieldHelp           AS CHAR NO-UNDO.
DEF VAR bRemoveDuplicates    AS LOG  NO-UNDO.
DEF VAR cFieldMapName        AS CHAR NO-UNDO.
DEF VAR cQryForFieldMap      AS CHAR NO-UNDO.
DEF VAR cVariablesFromDbList AS CHAR NO-UNDO.
DEF VAR cDefSection          AS CHAR NO-UNDO.
DEF VAR cInitObject          AS CHAR NO-UNDO.
DEF VAR cTablesInUse         AS CHAR NO-UNDO.
DEF VAR cFieldsInUse         AS CHAR NO-UNDO.
DEF VAR cFirstTableRef       AS CHAR NO-UNDO.
DEF VAR cQryObjectsInUse     AS CHAR NO-UNDO.
DEF VAR cFmObjectsInUse      AS CHAR NO-UNDO.
DEF VAR cTbObjectsInUse      AS CHAR NO-UNDO.
DEF VAR bCancelQueryRebuild  AS LOG  NO-UNDO.
DEF VAR bNewJukeBoxObject    AS LOG  NO-UNDO.
DEF VAR cLastToolChoose      AS CHAR NO-UNDO.
DEF VAR cSelectedBrwOrQry    AS CHAR NO-UNDO.
DEF VAR cFirstTableList      AS CHAR NO-UNDO.
DEF VAR bNotJukeBoxObject    AS LOG  NO-UNDO.
DEF VAR cActiveWidget        AS CHAR NO-UNDO.
DEF VAR cCreFieldViewAS      AS CHAR NO-UNDO.

DEF STREAM JBoxStream.

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
DEF TEMP-TABLE ttObj 
    FIELD cObjectName    AS CHAR 
    FIELD cWidgetName    AS CHAR
    FIELD hWindow        as handle
    FIELD cObjectType    AS CHAR
    FIELD cFirstTable    AS CHAR
    FIELD cViewCode      AS CHAR
    FIELD cTtCode        AS CHAR
    FIELD cJoinCode      AS CHAR
    FIELD cQueryCode     AS CHAR
    FIELD bFreeform      AS LOG
    FIELD cObjectState   AS CHAR
    .
DEF TEMP-TABLE ttObjRel
    FIELD cFromWidgetName AS CHAR
    FIELD hFromWindow     as handle
    FIELD cToWidgetName   AS CHAR
    FIELD hToWindow       as handle
    .

DEF TEMP-TABLE ttObjTable
    FIELD cWidgetName  AS CHAR
    FIELD hWindow      as handle
    FIELD cObjectTable AS CHAR
    FIELD cObjectDb    AS CHAR
    FIELD iTableSeq    AS INT
    FIELD cFieldList   AS CHAR
/*     FIELD cCalcSrcTbl  AS CHAR  */
    .
DEF TEMP-TABLE ttObjField
    FIELD cWidgetName  AS CHAR
    FIELD hWindow      AS HANDLE
    FIELD cObjectTable AS CHAR
    FIELD iFieldSeq    AS INT
    FIELD cFieldName   AS CHAR
    FIELD cDataType    AS CHAR
    FIELD cFormat      AS CHAR
    FIELD cLabel       AS CHAR
    FIELD fWidth       AS DEC
    .

DEF TEMP-TABLE ttObjTTfieldDef
    FIELD cWidgetName  AS CHAR
    FIELD hWindow      AS HANDLE
    FIELD cTableName   AS CHAR
    FIELD iFieldSeq    AS INT
    FIELD cFieldName   AS CHAR
    FIELD cDataType    AS CHAR
    FIELD cFormat      AS CHAR
    FIELD cLabel       AS CHAR
    FIELD cInit        AS CHAR
    FIELD cComment     AS CHAR
    FIELD cCalcFldProc AS CHAR /* for calculation */
    FIELD cProcName    AS CHAR /* for calculation */
    FIELD cCalcSrcTbl  AS CHAR
    FIELD cSourceTable AS CHAR
    FIELD cDbField     AS CHAR
    .
DEF VAR hbttObjTTfieldDef AS HANDLE NO-UNDO.
hbttObjTTfieldDef = BUFFER ttObjTTfieldDef:HANDLE.

/* Helper tables for analyzing definitions: */
DEF TEMP-TABLE ttObjViewDef
    FIELD cWidgetName  AS CHAR
    FIELD cTableName   AS CHAR
    FIELD cCode        AS CHAR
    .
DEF TEMP-TABLE ttObjJoinDef
    FIELD cWidgetName  AS CHAR
    FIELD cTableName   AS CHAR
    FIELD cCode        AS CHAR
    .
DEF TEMP-TABLE ttObjCalcProcDef
    FIELD cWidgetName  AS CHAR
    FIELD cTableName   AS CHAR
    FIELD cCode        AS CHAR
    .
DEF TEMP-TABLE ttObjTTDef
    FIELD cWidgetName  AS CHAR
    FIELD cTableName   AS CHAR
    FIELD cCode        AS CHAR
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-creFillInDefFromDb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD creFillInDefFromDb Procedure 
FUNCTION creFillInDefFromDb RETURNS CHARACTER
  ( INPUT icFieldList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-creFunction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD creFunction Procedure 
FUNCTION creFunction RETURNS CHARACTER
  ( INPUT icCode AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-creJBoxTool) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD creJBoxTool Procedure 
FUNCTION creJBoxTool RETURNS LOGICAL
  ( INPUT ihToolBuff AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-creTrigger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD creTrigger Procedure 
FUNCTION creTrigger RETURNS CHARACTER
  ( INPUT icCode AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-creViewAsPhrase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD creViewAsPhrase Procedure 
FUNCTION creViewAsPhrase RETURNS CHARACTER
  ( INPUT icDataType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EmptyTts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EmptyTts Procedure 
FUNCTION EmptyTts RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fixFieldFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fixFieldFormat Procedure 
FUNCTION fixFieldFormat RETURNS CHARACTER
  ( INPUT ihFieldBuf AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQueryJoin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixQueryJoin Procedure 
FUNCTION FixQueryJoin RETURNS CHARACTER
  ( INPUT icQueryString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrowseTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBrowseTable Procedure 
FUNCTION getBrowseTable RETURNS CHARACTER
  (INPUT icBrowseName AS CHAR,
   INPUT ibModify     AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldCorrVarName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDbFieldCorrVarName Procedure 
FUNCTION getDbFieldCorrVarName RETURNS CHARACTER
  ( INPUT icFieldName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDbFieldName Procedure 
FUNCTION getDbFieldName RETURNS CHARACTER
  ( INPUT icDbName    AS CHAR,
    INPUT icTableName AS CHAR,
    INPUT icFieldName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldToVarHelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDbFieldToVarHelp Procedure 
FUNCTION getDbFieldToVarHelp RETURNS CHARACTER
  (INPUT icFieldName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldToVarTooltip) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDbFieldToVarTooltip Procedure 
FUNCTION getDbFieldToVarTooltip RETURNS CHARACTER
  ( INPUT icFieldName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDbFieldUsage Procedure 
FUNCTION getDbFieldUsage RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldUsageOptions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDbFieldUsageOptions Procedure 
FUNCTION getDbFieldUsageOptions RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDefSectionCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDefSectionCode Procedure 
FUNCTION getDefSectionCode RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldsInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldsInUse Procedure 
FUNCTION getFieldsInUse RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFirstTableList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFirstTableList Procedure 
FUNCTION getFirstTableList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFmObjectsInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFmObjectsInUse Procedure 
FUNCTION getFmObjectsInUse RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getHandleForObjectTtFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHandleForObjectTtFields Procedure 
FUNCTION getHandleForObjectTtFields RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getInitObjectCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getInitObjectCode Procedure 
FUNCTION getInitObjectCode RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsJukeBoxObject Procedure 
FUNCTION getIsJukeBoxObject RETURNS LOGICAL
  ( INPUT icWidgetName AS CHAR,
    INPUT ihWin        AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsNotJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIsNotJukeBoxObject Procedure 
FUNCTION getIsNotJukeBoxObject RETURNS LOGICAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getJBoxUserContrProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getJBoxUserContrProperties Procedure 
FUNCTION getJBoxUserContrProperties RETURNS CHARACTER
  ( INPUT ir_U          AS RECID,
    INPUT icUserControl AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLastToolChoose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLastToolChoose Procedure 
FUNCTION getLastToolChoose RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNewJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNewJukeBoxObject Procedure 
FUNCTION getNewJukeBoxObject RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectCalcProcs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectCalcProcs Procedure 
FUNCTION getObjectCalcProcs RETURNS CHARACTER
  ( INPUT icWidget AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectFieldList Procedure 
FUNCTION getObjectFieldList RETURNS CHARACTER
  ( INPUT icWidget AS CHARACTER,
    INPUT icDelim  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectPrimaryTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectPrimaryTable Procedure 
FUNCTION getObjectPrimaryTable RETURNS CHARACTER
  ( INPUT icWidgetName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectQueryDef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectQueryDef Procedure 
FUNCTION getObjectQueryDef RETURNS CHARACTER
  ( INPUT icWidgetName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectTTdef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectTTdef Procedure 
FUNCTION getObjectTTdef RETURNS CHARACTER
  ( INPUT icWidgetName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectTtFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getObjectTtFieldList Procedure 
FUNCTION getObjectTtFieldList RETURNS CHARACTER
  ( INPUT icWidget AS CHAR,
    INPUT icDelim  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryKeyFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPrimaryKeyFields Procedure 
FUNCTION getPrimaryKeyFields RETURNS CHARACTER
  ( INPUT icTable AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQryObjectsInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQryObjectsInUse Procedure 
FUNCTION getQryObjectsInUse RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRelatedObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRelatedObject Procedure 
FUNCTION getRelatedObject RETURNS CHARACTER
  ( INPUT icSrcObject     AS CHAR,
    INPUT icSrcObjectType AS CHAR,
    INPUT icTargetObjType AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTableDbName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTableDbName Procedure 
FUNCTION getTableDbName RETURNS CHARACTER
  ( INPUT icTableName AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTablesInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTablesInUse Procedure 
FUNCTION getTablesInUse RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTbObjectsInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTbObjectsInUse Procedure 
FUNCTION getTbObjectsInUse RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUseDbPrefix) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUseDbPrefix Procedure 
FUNCTION getUseDbPrefix RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getVariablesFromDbList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVariablesFromDbList Procedure 
FUNCTION getVariablesFromDbList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD initJukeBoxObject Procedure 
FUNCTION initJukeBoxObject RETURNS LOGICAL
  ( INPUT icWidgetName AS CHAR,
    INPUT ihWindow     as handle,
    INPUT icType       AS CHAR,
    INPUT ibFreeForm   AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-selectProcForObjectInitCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD selectProcForObjectInitCode Procedure 
FUNCTION selectProcForObjectInitCode RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setActiveWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setActiveWidget Procedure 
FUNCTION setActiveWidget RETURNS LOGICAL
  ( INPUT icActiveWidget AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCancelQueryRebuild) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCancelQueryRebuild Procedure 
FUNCTION setCancelQueryRebuild RETURNS LOGICAL
  ( INPUT ibCancelQueryRebuild AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCreFieldViewAs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCreFieldViewAs Procedure 
FUNCTION setCreFieldViewAs RETURNS LOGICAL
  ( INPUT icViewAs AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDbFieldUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDbFieldUsage Procedure 
FUNCTION setDbFieldUsage RETURNS LOGICAL
  ( INPUT icDbFieldUsage AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldMapNameAndQry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldMapNameAndQry Procedure 
FUNCTION setFieldMapNameAndQry RETURNS LOGICAL
  ( INPUT icFieldMapName   AS CHAR,
    INPUT icQryForFieldMap AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldSelectClipBoardDelimiter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldSelectClipBoardDelimiter Procedure 
FUNCTION setFieldSelectClipBoardDelimiter RETURNS LOGICAL
  ( INPUT icClipBoardDelimiter AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldSelectClipBoardFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldSelectClipBoardFormat Procedure 
FUNCTION setFieldSelectClipBoardFormat RETURNS LOGICAL
  ( INPUT icClipBoardFormat AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldSelectFieldHelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldSelectFieldHelp Procedure 
FUNCTION setFieldSelectFieldHelp RETURNS LOGICAL
  ( INPUT icFieldHelp AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldSelectFieldPrefix) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldSelectFieldPrefix Procedure 
FUNCTION setFieldSelectFieldPrefix RETURNS LOGICAL
  ( INPUT icFieldPrefix AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldSelectRemoveDuplicates) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldSelectRemoveDuplicates Procedure 
FUNCTION setFieldSelectRemoveDuplicates RETURNS LOGICAL
  ( INPUT ibRemoveDuplicates AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldsInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldsInUse Procedure 
FUNCTION setFieldsInUse RETURNS LOGICAL
  ( INPUT icFieldsInUse AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFirstColumnEnabled) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFirstColumnEnabled Procedure 
FUNCTION setFirstColumnEnabled RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setIsNotJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setIsNotJukeBoxObject Procedure 
FUNCTION setIsNotJukeBoxObject RETURNS LOGICAL
  ( INPUT ibNotJukeBoxObject AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setJBoxUserContrProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setJBoxUserContrProperties Procedure 
FUNCTION setJBoxUserContrProperties RETURNS LOGICAL
  ( INPUT ir_U          AS RECID,
    INPUT icUserControl AS CHAR,
    INPUT icControlType AS CHAR,
    INPUT icCode        AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNewJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNewJukeBoxObject Procedure 
FUNCTION setNewJukeBoxObject RETURNS LOGICAL
  ( INPUT ibNewJukeBoxObject AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSelectedBrwOrQry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSelectedBrwOrQry Procedure 
FUNCTION setSelectedBrwOrQry RETURNS LOGICAL
  ( INPUT icSelectedBrwOrQry AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTablesInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTablesInUse Procedure 
FUNCTION setTablesInUse RETURNS LOGICAL
  ( INPUT icTablesInUse AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setToolChoose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setToolChoose Procedure 
FUNCTION setToolChoose RETURNS LOGICAL
  ( INPUT icLastToolChoose AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateTtObj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD updateTtObj Procedure 
FUNCTION updateTtObj RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

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
         HEIGHT             = 41.05
         WIDTH              = 68.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
IF NUM-DBS > 1 THEN DO: /* Build cache */
  IF NOT AVAIL _U THEN 
    FIND FIRST _U WHERE RECID(_U) = _query-u-rec
         NO-ERROR.
  IF AVAIL _U AND _U._TYPE = "browse" THEN DO:
    SESSION:SET-WAIT-STATE("general").
    RUN jukebox\fillTt_field.p("").
    SESSION:SET-WAIT-STATE("").  
  END.
END. 

/* RUN toexcelviafile.p (htt_field,0). */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-buildJBoxToolbarDef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildJBoxToolbarDef Procedure 
PROCEDURE buildJBoxToolbarDef :
/*------------------------------------------------------------------------------
  Purpose:     Build the toolbar definition and put it in the rectangles PRIVATE-DATA
  Parameters:  <none>
  Notes:       is called from eiter _drawobj.p or _prpobj.p
               The best place for this would be in selectTools.w but the rectangle 
               doesn't exist when it is called in _drawobj.p
------------------------------------------------------------------------------*/
DEF INPUT PARAM ir_U       AS RECID NO-UNDO.
DEF INPUT PARAM ihToolBuff AS HANDLE NO-UNDO.
DEF INPUT PARAM icTbName   AS CHAR NO-UNDO.
DEF INPUT PARAM icMenuDef  AS CHAR NO-UNDO.
DEF INPUT PARAM icMenuProp AS CHAR NO-UNDO.

creJBoxTool(ihToolBuff).

IF ir_U NE ? THEN
  FIND _U WHERE RECID(_U) = ir_U.

FIND FIRST ttJBoxTool NO-ERROR.
FOR EACH ttJBoxTool
    WHERE ttJBoxTool.bSelect
      AND NOT ttJBoxTool.bDeleted
    BY ttJBoxTool.iSeq
    :

  IF NOT CAN-DO("menu,sub-menu",ttJBoxTool.cType) AND (ttJBoxTool.bCreProc OR ttJBoxTool.cCode NE "") THEN
    RUN genOverrideProc(ttJBoxTool.cTool + "Record",ttJBoxTool.cCode).
END.

IF ir_U NE ? THEN
  _U._PRIVATE-DATA = icMenuDef + CHR(1) + icMenuProp.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-findObjectRelations) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findObjectRelations Procedure 
PROCEDURE findObjectRelations :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ir_P AS RECID NO-UNDO.

DEF VAR cDefLine  AS CHAR NO-UNDO.
DEF VAR bReadLine AS LOG NO-UNDO.
DEF VAR ix        AS INT NO-UNDO.

FIND FIRST _TRG
     WHERE _TRG._tSECTION = "_PROCEDURE":U
       AND _TRG._pRECID   = RECID(_P)
       AND _TRG._tEVENT   = "InitializeObject":U
     NO-ERROR.
IF AVAIL _TRG THEN DO:

  DO ix = 1 TO NUM-ENTRIES(_TRG._tCODE,CHR(10)):
    cDefLine = TRIM(ENTRY(ix,_TRG._tCODE,CHR(10))).
    IF cDefLine MATCHES "*:TOOLBAR-OBJECT*" THEN DO:
        
    END.
    /*
      ASSIGN bReadLine  = YES
             cCurrTable = SUBSTR(ENTRY(1,cDefLine," "),4).
    IF (cTablesInUse = "" OR CAN-DO(cTablesInUse,cCurrTable)) AND bReadLine 
        AND (cDefLine MATCHES "*:displayFields*" OR cDefLine MATCHES "*:updateFields*") AND NUM-ENTRIES(cDefLine,"'") > 1 THEN DO:
      cDefLine = ENTRY(2,cDefLine,"'").
      IF cTablesInUse = "" THEN cTablesInUse = cCurrTable.
      DO iz = 1 TO NUM-ENTRIES(cDefLine):
        cFieldsInUse = cFieldsInUse + (IF cFieldsInUse NE "" THEN CHR(10) ELSE "") + cCurrTable + "." + ENTRY(iz,cDefLine).
      END.
      bReadLine = NO.
    END.
    */
  END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-findTablesAndFieldsInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findTablesAndFieldsInUse Procedure 
PROCEDURE findTablesAndFieldsInUse :
/* Purpose:    Read code from Definitions, display-triggers and InitializeObject */

DEF VAR cDefLine       AS CHAR NO-UNDO.
DEF VAR ix             AS INT  NO-UNDO.
DEF VAR iy             AS INT  NO-UNDO.
DEF VAR iz             AS INT  NO-UNDO.
DEF VAR bReadView      AS LOG  NO-UNDO.
DEF VAR cCurrDb        AS CHAR NO-UNDO.
DEF VAR cCurrTable     AS CHAR NO-UNDO.
DEF VAR cCurrField     AS CHAR NO-UNDO.
DEF VAR cFieldProp     AS CHAR NO-UNDO.
DEF VAR cPropVal       AS CHAR NO-UNDO.
DEF VAR bReadTTdef     AS LOG  NO-UNDO.
DEF VAR cTtName        AS CHAR NO-UNDO.
DEF VAR cCodeLine      AS CHAR NO-UNDO.
DEF VAR bReadLine      AS LOG  NO-UNDO.
DEF VAR bReadJoin      AS LOG  NO-UNDO.
DEF VAR cOrgDecFormat  AS CHAR NO-UNDO.
DEF VAR bReadCalcProc  AS LOG  NO-UNDO.
DEF VAR cCalcProcFuncs AS CHAR NO-UNDO.
DEF VAR cDbFld         AS CHAR NO-UNDO.

ASSIGN cTablesInUse      = ""
       cFieldsInUse      = ""
       cFirstTableRef    = ""
       cFirstTableList   = ""
       cQryObjectsInUse  = ""
       cFmObjectsInUse   = ""
       cTbObjectsInUse   = ""
       cOrgDecFormat     = SESSION:NUMERIC-FORMAT
       SESSION:NUMERIC-FORMAT = "AMERICAN" 
       bNewJukeBoxObject = NO
       bNotJukeBoxObject = NO
       .
FIND _P WHERE _P._WINDOW-HANDLE = _h_win NO-ERROR.

IF AVAIL _P THEN DO:
  EmptyTts().
  cCurrDb = IF _db_name NE "" THEN _db_name ELSE LDBNAME("DICTDB").

  FIND FIRST _TRG
       WHERE _TRG._tSECTION = "_CUSTOM":U
         AND _TRG._pRECID   = RECID(_P)
         AND _TRG._tEVENT   = "_DEFINITIONS":U
       NO-ERROR.
  IF AVAIL _TRG THEN DO:

    /* Find all objects and their table and field references based on code in definitions: */
    DO ix = 1 TO NUM-ENTRIES(_TRG._tCODE,CHR(10)):
      ASSIGN cDefLine    = TRIM(ENTRY(ix,_TRG._tCODE,CHR(10)))
             cCodeLine = ENTRY(ix,_TRG._tCODE,CHR(10)).
      REPEAT WHILE INDEX(cDefLine,"  ") > 0:
        cDefLine = REPLACE(cDefLine,"  "," ").
      END.

      IF cDefLine MATCHES "*AS JBoxToolbar*" THEN DO:
        FIND FIRST ttObj WHERE ttObj.cObjectName = ENTRY(3,cDefLine," ") NO-ERROR.
        IF NOT AVAIL ttObj THEN DO:
          CREATE ttObj.
          ASSIGN ttObj.cObjectName = ENTRY(3,cDefLine," ")
                 ttObj.hWindow = _P._WINDOW-HANDLE
                 ttObj.cObjectType = "toolbar"
                 ttObj.cWidgetName = SUBSTR(ENTRY(3,cDefLine," "),2).
        END.
      END.

      IF cDefLine BEGINS "END FUNCTION." AND bReadView THEN 
        ASSIGN ttObjViewDef.cCode = ttObjViewDef.cCode + CHR(10) + cCodeLine 
               bReadView = NO.
      ELSE IF cDefLine BEGINS "END FUNCTION." AND bReadJoin THEN 
        ASSIGN ttObjJoinDef.cCode = ttObjJoinDef.cCode + CHR(10) + cCodeLine 
               bReadJoin = NO.
      ELSE IF cDefLine BEGINS "END FUNCTION." AND bReadCalcProc THEN 
        ASSIGN ttObjCalcProcDef.cCode = ttObjCalcProcDef.cCode + CHR(10) + cCodeLine 
               bReadCalcProc = NO.

      IF bReadView THEN 
        ASSIGN cDefLine = REPLACE(REPLACE(cDefLine,'"',"'")," ","")
               ttObjViewDef.cCode = ttObjViewDef.cCode + CHR(10) + cCodeLine.

      IF bReadView AND (cDefLine BEGINS "'" OR cDefLine BEGINS "+',") THEN DO:
        cDefLine = REPLACE(cDefLine,"'","").
        cCurrTable = ENTRY(NUM-ENTRIES(cDefLine),cDefLine).
        cDefLine = REPLACE(cDefLine,".","").
        IF cFirstTableRef = "" THEN cFirstTableRef = cCurrTable.
        IF AVAIL ttObj THEN DO:
          IF ttObj.cFirstTable = "" THEN ttObj.cFirstTable = cCurrTable.
          FIND FIRST ttObjTable 
               WHERE ttObjTable.cWidgetName  = ttObj.cWidgetName
                 AND ttObjTable.cObjectTable = cCurrTable
               NO-ERROR.
          IF NOT AVAIL ttObjTable THEN DO:
            CREATE ttObjTable.
            ASSIGN ttObjTable.cWidgetName  = ttObj.cWidgetName 
                   ttObjTable.hWindow      = _h_win
                   ttObjTable.cObjectDb    = getTableDbName(cCurrTable)
                   cCurrTable              = ENTRY(NUM-ENTRIES(cCurrTable,"."),cCurrTable,".")
                   ttObjTable.cObjectTable = cCurrTable     
                   ttObjTable.iTableSeq    = iy
                   iy                      = iy + 1
                   .
          END.
          IF cActiveWidget = "" OR ttObj.cWidgetName = cActiveWidget THEN
            cTablesInUse = cTablesInUse + (IF cTablesInUse NE "" THEN "," ELSE "") + cCurrTable. 
        END.
      END.
      /* Find available fields for browse from view-definition */
      ELSE IF bReadView AND (cDefLine BEGINS "+';") THEN DO:
        cDefLine = REPLACE(cDefLine,"+';","").
        cDefLine = REPLACE(cDefLine,"'","").
        cDefLine = REPLACE(cDefLine,".","").
        /* Calculated field - add procedure name to field properties for temp-table */
        IF cDefLine BEGINS "+" THEN DO:
          cDefLine = SUBSTR(cDefLine,2).
          FIND FIRST ttObjTtFieldDef 
               WHERE ttObjTtFieldDef.cFieldName = ENTRY(1,cDefLine,"|")
                 AND ttObjTtFieldDef.hWindow = _h_win
               NO-ERROR.
          IF AVAIL ttObjTtFieldDef THEN
            ASSIGN ttObjTtFieldDef.cProcName = TRIM(ENTRY(4,cCodeLine,"|"),"'") /* to keep spaces in expressions */
                   ttObjTtFieldDef.cCalcSrcTbl = cCurrTable.
          ELSE cDefLine = "".
          cDefLine = ENTRY(1,cDefLine,"|").
        END.
        ELSE DO:
          IF INDEX(cDefLine,"[") > 0 THEN
            ASSIGN cDbFld   = cDefLine
                   cDefLine = SUBSTR(REPLACE(cDefLine,"[","_"),1,LENGTH(cDefLine) - 1).
          ELSE cDbFld = getDbFieldName ("",cCurrTable,cDefLine).

          FIND FIRST ttObjTtFieldDef 
               WHERE ttObjTtFieldDef.cFieldName  = ENTRY(1,cDefLine,"|")
                 AND ttObjTTfieldDef.cTableName  = cCurrTable
                 AND ttObjTtFieldDef.cSourceTable = ""
                 AND ttObjTtFieldDef.cDbField = ""
                 AND ttObjTtFieldDef.hWindow  = _h_win
               NO-ERROR.
          IF NOT AVAIL ttObjTtFieldDef THEN
            FIND FIRST ttObjTtFieldDef 
                 WHERE ttObjTtFieldDef.cFieldName  BEGINS ENTRY(1,cDefLine,"|")
                   AND ttObjTTfieldDef.cTableName  = cCurrTable
                   AND ttObjTtFieldDef.cSourceTable = ""
                   AND ttObjTtFieldDef.cDbField = ""
                   AND ttObjTtFieldDef.hWindow  = _h_win
                 NO-ERROR.
          IF NOT AVAIL ttObjTtFieldDef THEN 
            FIND FIRST ttObjTtFieldDef 
                 WHERE ttObjTtFieldDef.cFieldName = ENTRY(1,cDefLine,"|")
                   AND ttObjTtFieldDef.cSourceTable = ""
                   AND ttObjTtFieldDef.cDbField = ""
                   AND ttObjTtFieldDef.hWindow = _h_win
                 NO-ERROR.
          IF NOT AVAIL ttObjTtFieldDef THEN 
            FIND FIRST ttObjTtFieldDef 
                 WHERE ttObjTtFieldDef.cFieldName BEGINS ENTRY(1,cDefLine,"|")
                   AND ttObjTtFieldDef.cSourceTable = ""
                   AND ttObjTtFieldDef.cDbField = ""
                   AND ttObjTtFieldDef.hWindow = _h_win
                 NO-ERROR.
          IF AVAIL ttObjTtFieldDef THEN DO:
            ASSIGN ttObjTtFieldDef.cSourceTable = cCurrTable
                   ttObjTtFieldDef.cDbField = cDbFld.
            IF cActiveWidget = "" OR CAN-DO(cTablesInUse,cCurrTable) THEN
              cFieldsInUse = cFieldsInUse + (IF cFieldsInUse NE "" THEN CHR(10) ELSE "") + cCurrTable + "." + cDbFld.
          END.
          ELSE MESSAGE "Field" ENTRY(1,cDefLine,"|") "not found in temp-table def for" cCurrTable SKIP
                       "Probabel cause is that same field is in multiple joined tables"
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.

        IF AVAIL ttObjTable THEN 
          ttObjTable.cFieldList = ttObjTable.cFieldList + (IF ttObjTable.cFieldList NE "" THEN "," ELSE "") + cDefLine.
      END.

      /* Find field defs for t-tables: */
      ELSE IF bReadTTdef THEN DO:
        ttObjTTdef.cCode = ttObjTTdef.cCode + CHR(10) + cCodeLine.
        
        IF cDefLine BEGINS "INDEX " THEN bReadTTdef = NO.
        ELSE IF cDefLine BEGINS "FIELD" THEN DO:
          CREATE ttObjTTfieldDef.
          ASSIGN ttObjTTfieldDef.cTableName = cTtName
                 ttObjTTfieldDef.hWindow    = _h_win
                 ttObjTTfieldDef.cFieldName = ENTRY(2,cDefLine," ")
                 ttObjTTfieldDef.cDataType  = ENTRY(4,cDefLine," ")
                 ttObjTTfieldDef.iFieldSeq  = ix
                 cPropVal = ""
                 .
          DO iz = NUM-ENTRIES(cDefLine," ") TO 2 BY -1:
            IF SUBSTR(ENTRY(iz,cDefLine," "),LENGTH(ENTRY(iz,cDefLine," "))) = "/" THEN
              cPropVal = ENTRY(iz,cDefLine," ").
            ELSE IF SUBSTR(ENTRY(iz,cDefLine," "),1,1) = "/" THEN
              ASSIGN ttObjTTfieldDef.cComment = ENTRY(iz,cDefLine," ") + (IF cPropVal NE "" THEN " " ELSE "") + cPropVal
                     cPropVal = ""
                     .
            ELSE
              CASE ENTRY(iz,cDefLine," "):
                WHEN "FORMAT" THEN
                  ASSIGN ttObjTTfieldDef.cFormat = TRIM(TRIM(TRIM(cPropVal,":U"),"'"),'"')
                         cPropVal = "".
                WHEN "LABEL"  THEN 
                  ASSIGN ttObjTTfieldDef.cLabel  = TRIM(cPropVal,":U")
                         cPropVal = "".
                WHEN "INIT"  THEN 
                  ASSIGN ttObjTTfieldDef.cInit  = TRIM(cPropVal,":U")
                         cPropVal = "".
                OTHERWISE cPropVal = ENTRY(iz,cDefLine," ") + (IF cPropVal NE "" THEN " " ELSE "") + cPropVal.
              END CASE.
          END.
            
        END.
      END.
      ELSE IF bReadJoin THEN
        ASSIGN ttObjJoinDef.cCode = ttObjJoinDef.cCode + CHR(10) + cCodeLine
               cDefLine = LEFT-TRIM(cDefLine,"RETURN")
               cDefLine = TRIM(cDefLine)
               cDefLine = RIGHT-TRIM(cDefLine,".")
               cDefLine = TRIM(cDefLine,"'")
               cDefLine = REPLACE(cDefLine,"EACH ",CHR(10) + "    EACH ")
               cDefLine = REPLACE(cDefLine,"FIRST ",CHR(10) + "    FIRST ")
               cDefLine = REPLACE(cDefLine,"LAST ",CHR(10) + "    LAST ")
               ttObj.cQueryCode = "EACH " + ttObj.cFirstTable + " NO-LOCK," + cDefLine + " INDEXED-REPOSITION"
               .
      ELSE IF bReadCalcProc THEN DO:
        ASSIGN ttObjCalcProcDef.cCode = ttObjCalcProcDef.cCode + CHR(10) + cCodeLine
               cDefLine = LEFT-TRIM(cDefLine,"RETURN")
               cDefLine = TRIM(cDefLine,"+")
               cDefLine = TRIM(cDefLine)
               cDefLine = RIGHT-TRIM(cDefLine,".")
               cDefLine = TRIM(cDefLine,"'")
               cDefLine = TRIM(cDefLine,",")
               .
        IF NUM-ENTRIES(cDefLine,"*") > 1 THEN DO:
          cCalcProcFuncs = TRIM(ENTRY(2,cDefLine,"*")).
          DO iz = 1 TO NUM-ENTRIES(cCalcProcFuncs):
            FIND FIRST ttObjTtFieldDef 
                 WHERE ttObjTtFieldDef.cProcName = ENTRY(1,cCalcProcFuncs,"|")
                 NO-ERROR.
            IF AVAIL ttObjTtFieldDef THEN 
              ttObjTtFieldDef.cCalcFldProc = ENTRY(1,cDefLine,"'").              
          END.
        END.
      END.

      /* Determine next part: */
      IF cDefLine BEGINS "FUNCTION getBuffersAndFields" THEN DO:
        CREATE ttObj.
        ASSIGN ttObj.hWindow     = _h_win
               ttObj.cWidgetName = ENTRY(1,SUBSTR(cDefLine,29)," ")
               ttObj.cObjectName = "o" + ttObj.cWidgetName
               ttObj.cObjectType = (IF ttObj.cWidgetName BEGINS "brw" THEN "browse" ELSE "query")
               iy                   = 1
               . 
        CREATE ttObjViewDef.
        ASSIGN ttObjViewDef.cWidgetName = ttObj.cWidgetName
               ttObjViewDef.cCode       = cCodeLine
               bReadView = YES.
        IF cFirstTable NE "" THEN
          cFirstTableList = cFirstTableList + (IF cFirstTableList NE "" THEN "," ELSE "") + cFirstTable.  
      END. 

      IF cDefLine BEGINS "FUNCTION getQueryJoin" THEN DO:
        CREATE ttObjJoinDef.
        ASSIGN ttObjJoinDef.cWidgetName = ttObj.cWidgetName
               ttObjJoinDef.cCode       = cCodeLine.
        bReadJoin = YES.  
      END. 

      IF cDefLine BEGINS "FUNCTION getCalcFieldProc" THEN DO:
        CREATE ttObjCalcProcDef.
        ASSIGN ttObjCalcProcDef.cWidgetName = ttObj.cWidgetName
               ttObjCalcProcDef.cCode       = cCodeLine.
        bReadCalcProc = YES.  
      END. 

      IF cDefLine MATCHES "DEF* TEMP-TABLE *" AND NOT cDefLine MATCHES "*BUFFER*" THEN DO:
        CREATE ttObjTTdef.
        ASSIGN cTtName    = ENTRY(3,cDefLine," ")
               bReadTTdef = YES
               ttObjTTdef.cTableName = cTtName
               ttObjTTdef.cCode      = cCodeLine
               .      
      END.

      IF (cDefLine MATCHES "*AS* JBoxQuery*" OR cDefLine MATCHES "*AS* JBoxBrowse *") AND NOT CAN-DO(cQryObjectsInUse,ENTRY(3,cDefLine," ")) THEN
        cQryObjectsInUse = cQryObjectsInUse + (IF cQryObjectsInUse NE "" THEN "," ELSE "") + ENTRY(3,cDefLine," ").
      ELSE IF cDefLine MATCHES "*AS* JBoxFieldMap*" AND NOT CAN-DO(cFmObjectsInUse,ENTRY(3,cDefLine," ")) THEN
        cFmObjectsInUse = cFmObjectsInUse + (IF cFmObjectsInUse NE "" THEN "," ELSE "") + ENTRY(3,cDefLine," ").
      ELSE IF cDefLine MATCHES "*AS* JBoxToolbar*" AND NOT CAN-DO(cTbObjectsInUse,ENTRY(3,cDefLine," ")) THEN
        cTbObjectsInUse = cTbObjectsInUse + (IF cTbObjectsInUse NE "" THEN "," ELSE "") + ENTRY(3,cDefLine," ").
    END.
  END.

  /* assign widget names to ttdef: */
  updateTtObj().

  /* Examine display trg for FREEFORM browse: */
  FOR EACH _TRG 
      WHERE _TRG._tSECTION = "_CONTROL":U
        AND _TRG._pRECID   = RECID(_P)
        AND _TRG._tEVENT   = "DISPLAY":U
        AND _TRG._tSPECIAL = "_DISPLAY-FIELDS"
      :
    /* If freeform query */
    FIND FIRST ttObj 
         WHERE ttObj.cFirstTable = ENTRY(1,TRIM(_TRG._tCODE),".") 
         NO-ERROR.
    IF AVAIL ttObj THEN DO ix = 1 TO NUM-ENTRIES(_TRG._tCODE,CHR(10)):
      ttObj.bFreeform = YES.

      cDefLine = TRIM(ENTRY(ix,_TRG._tCODE,CHR(10))).
      cDefLine = REPLACE(cDefLine,"'","").
      cDefLine = REPLACE(cDefLine,'"','').
      IF cDefLine BEGINS "enable " THEN LEAVE.
      cCurrField = ENTRY(2,ENTRY(1,cDefLine," "),".") NO-ERROR.
      FIND FIRST ttObjTTfieldDef
           WHERE ttObjTTfieldDef.cFieldName = cCurrField
           NO-ERROR.
      IF NOT AVAIL ttObjTTfieldDef THEN DO:
        MESSAGE "Field" cCurrField "is not (longer) part of temp-table definition" SKIP
                "Cancel you change and correct this before doing more modifications"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.  
        NEXT.
      END.
      /* Not calc field: */
      IF ttObjTTfieldDef.cProcName = "" THEN DO:
        FIND FIRST ttObjTable
             WHERE ttObjTable.cWidgetName = ttObj.cWidgetName
               AND CAN-DO(ttObjTable.cFieldList,cCurrField)
             NO-ERROR.
        IF NOT AVAIL ttObjTable THEN DO:
          /* Could it be duplicate? */
          FIND FIRST ttObjTable
               WHERE ttObjTable.cWidgetName = ttObj.cWidgetName
                 AND CAN-DO(ttObjTable.cFieldList,SUBSTR(cCurrField,1,LENGTH(cCurrField) - 1))
                 AND ttObjTable.iTableSeq = INT(SUBSTR(cCurrField,LENGTH(cCurrField) - 1))
               NO-ERROR.
          IF NOT AVAIL ttObjTable THEN DO:
            /* Surely then it must be an extent field */
            cCurrField = SUBSTR(cCurrField,1,R-INDEX(cCurrField,"_") - 1) + "[" + SUBSTR(cCurrField,R-INDEX(cCurrField,"_") + 1) + "]".
            FIND FIRST ttObjTable
                 WHERE ttObjTable.cWidgetName = ttObj.cWidgetName
                   AND CAN-DO(ttObjTable.cFieldList,cCurrField)
                 NO-ERROR.
            IF NOT AVAIL ttObjTable THEN DO:
              cCurrField = SUBSTR(cCurrField,1,INDEX(cCurrField,"[") - 1).
              NEXT.
            END.
          END.
        END.
      END.
      /* Calculated field: */
      ELSE DO:
        FIND FIRST ttObjTable
             WHERE ttObjTable.cWidgetName = ttObj.cWidgetName
               AND ttObjTable.cObjectTable = "_<CALC>"
             NO-ERROR.
        IF NOT AVAIL ttObjTable THEN DO:
          CREATE ttObjTable.
          ASSIGN ttObjTable.cWidgetName  = ttObj.cWidgetName
                 ttObjTable.hWindow      = _h_win
                 ttObjTable.cObjectTable = "_<CALC>"
                 .
        END.
      END.

      CREATE ttObjField.
      ASSIGN ttObjField.cWidgetName  = ttObjTable.cWidgetName
             ttObjField.hWindow      = _h_win
             ttObjField.cObjectTable = ttObjTable.cObjectTable
             ttObjField.iFieldSeq    = ix
             ttObjField.cFieldName   = cCurrField
             .
      REPEAT WHILE INDEX(cDefLine,"  ") > 0:
        cDefLine = REPLACE(cDefLine,"  "," ").
      END.
      cPropVal = "".
      DO iz = NUM-ENTRIES(cDefLine," ") TO 2 BY -1:
        CASE ENTRY(iz,cDefLine," "):
          WHEN "FORMAT" THEN
            ASSIGN ttObjField.cFormat = TRIM(cPropVal,":U")
                   cPropVal = "".
          WHEN "LABEL"  THEN 
            ASSIGN ttObjField.cLabel  = TRIM(cPropVal,":U")
                   cPropVal = "".
          WHEN "WIDTH"  THEN
            ASSIGN ttObjField.fWidth  = DEC(cPropVal)
                   cPropVal = "".
          OTHERWISE cPropVal = ENTRY(iz,cDefLine," ") + (IF cPropVal NE "" THEN " " ELSE "") + cPropVal.
        END CASE.
      END.

      ttObjField.cDataType = ttObjTTfieldDef.cDataType.
      IF ttObjField.cFormat = "" THEN ttObjField.cFormat = ttObjTTfieldDef.cFormat.
      IF ttObjField.cLabel  = "" THEN ttObjField.cLabel  = ttObjTTfieldDef.cLabel.

    END.
  END.
  FOR EACH _U 
      WHERE _U._WINDOW-HANDLE = _h_win:
    FOR EACH _BC
        WHERE _BC._x-recid = _U._x-recid
          AND NOT CAN-DO(cFieldsInUse,_BC._DISP-NAME):
      cFieldsInUse = cFieldsInUse + (IF cFieldsInUse NE "" THEN CHR(10) ELSE "") + _BC._DISP-NAME.
    END.
  END.
  
  IF cFieldsInUse = "" THEN DO:
    FIND FIRST _TRG
         WHERE _TRG._tSECTION = "_PROCEDURE":U
           AND _TRG._pRECID   = RECID(_P)
           AND _TRG._tEVENT   = "InitializeObject":U
         NO-ERROR.
    IF AVAIL _TRG THEN DO:

      DO ix = 1 TO NUM-ENTRIES(_TRG._tCODE,CHR(10)):
        cDefLine = TRIM(ENTRY(ix,_TRG._tCODE,CHR(10))).
        IF cDefLine MATCHES "*NEW JBoxFieldMap*" THEN
          ASSIGN bReadLine  = YES
                 cCurrTable = SUBSTR(ENTRY(1,cDefLine," "),4).
        IF (cTablesInUse = "" OR CAN-DO(cTablesInUse,cCurrTable)) AND bReadLine 
            AND (cDefLine MATCHES "*:displayFields*" OR cDefLine MATCHES "*:updateFields*") AND NUM-ENTRIES(cDefLine,"'") > 1 THEN DO:
          cDefLine = ENTRY(2,cDefLine,"'").
          IF cTablesInUse = "" THEN cTablesInUse = cCurrTable.
          DO iz = 1 TO NUM-ENTRIES(cDefLine):
            cFieldsInUse = cFieldsInUse + (IF cFieldsInUse NE "" THEN CHR(10) ELSE "") + cCurrTable + "." + ENTRY(iz,cDefLine).
          END.
          bReadLine = NO.
        END.
      END.
    END.
  END.
  RUN findObjectRelations (RECID(_P)).
END.
cFirstTableList = cFirstTableList + (IF cFirstTableList NE "" THEN "," ELSE "") + cFirstTableRef.
cFirstTableRef = ENTRY(1,cFirstTableList).

SESSION:NUMERIC-FORMAT = cOrgDecFormat.

/* DEF VAR hbttObjTable AS HANDLE NO-UNDO.   */
/* hbttObjTable =  BUFFER ttObjTable:HANDLE. */
/* RUN toexcelviafile.p (hbttObjTable,0).    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genBrowseDisplay) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genBrowseDisplay Procedure 
PROCEDURE genBrowseDisplay :
/*------------------------------------------------------------------------------
  Purpose:   (re)creates the display trigger for a browse if fields have been modified
              as well as the temp-table and view definition
  Parameters:  <none>
  Notes:      LEGACY 
------------------------------------------------------------------------------*/
  DEF INPUT PARAM icBrowseName AS CHAR NO-UNDO.

  DEF VAR cDisplTrg         AS CHAR NO-UNDO.
  DEF VAR cTtFieldDef       AS CHAR NO-UNDO.
  DEF VAR cViewDef          AS CHAR NO-UNDO.
  DEF VAR cFldName          AS CHAR NO-UNDO.
  DEF VAR cDbExt            AS CHAR NO-UNDO.
  DEF VAR cTtExt            AS CHAR NO-UNDO.
  DEF VAR cTblList          AS CHAR NO-UNDO.
  DEF VAR ix                AS INT  NO-UNDO.
  DEF VAR iy                AS INT  NO-UNDO.
  DEF VAR cRowIdentIdx      AS CHAR NO-UNDO.
  DEF VAR cCurrDb           AS CHAR NO-UNDO.
  DEF VAR cFldList          AS CHAR NO-UNDO.
  DEF VAR cQueryJoin        AS CHAR NO-UNDO.
  DEF VAR cTbl              AS CHAR NO-UNDO.
  DEF VAR cCalcProcs        AS CHAR NO-UNDO.
  DEF VAR cCalcFld          AS CHAR NO-UNDO.
  DEF VAR cCalcProcFlds     AS CHAR NO-UNDO.
  DEF VAR cCalcProcFunc     AS CHAR NO-UNDO.
  DEF VAR cCustomCalcProcs  AS CHAR NO-UNDO.
  DEF VAR bFreeform         AS LOG  NO-UNDO.
  DEF VAR cInitObjCode      AS CHAR NO-UNDO.
  DEF VAR cCode1            AS CHAR NO-UNDO.
  DEF VAR cCode2            AS CHAR NO-UNDO.
  DEF VAR cInitProc         AS CHAR NO-UNDO.

  IF bCancelQueryRebuild THEN DO:
    bCancelQueryRebuild = NO.
    RETURN.
  END.

  DEF BUFFER ttObj FOR ttObj.

/*   DEF VAR h_bc AS HANDLE NO-UNDO.  */
/*   h_bc = BUFFER ttObj:HANDLE.   */
/*   RUN TOexcelviafile.p(h_bc,-1).   */
  

  FIND _P WHERE _P._WINDOW-HANDLE = _h_win NO-ERROR.
  FIND _U WHERE RECID(_U) = _query-u-rec NO-ERROR.
  FIND _C WHERE RECID(_C) = _U._x-recid NO-ERROR.
  FIND _Q WHERE RECID(_Q) = _C._q-recid NO-ERROR.

  IF NOT AVAIL _P OR NOT AVAIL _U OR NOT AVAIL _C OR NOT AVAIL _Q THEN RETURN.

  cCurrDb = IF _db_name NE "" THEN _db_name ELSE LDBNAME("DICTDB").

  FIND FIRST _TRG 
       WHERE _TRG._tSECTION = "_CONTROL":U
         AND _TRG._pRECID   = RECID(_P)
         AND _TRG._wRECID   = RECID(_U)
         AND _TRG._tEVENT   = "DISPLAY":U
         AND _TRG._tSPECIAL = "_DISPLAY-FIELDS"
       NO-ERROR.
  IF AVAIL _TRG THEN bFreeform = YES.

  FIND FIRST ttObj 
       WHERE ttObj.cWidgetName = _U._NAME
         AND ttObj.hWindow = _h_win
       NO-ERROR.        

  IF /* AVAIL _TRG AND */ AVAIL ttObj THEN DO:
    ASSIGN cTtFieldDef = "DEF TEMP-TABLE " + ttObj.cFirstTable + CHR(10).
           cViewDef    = "FUNCTION getBuffersAndFields" + ttObj.cWidgetName + " RETURNS CHARACTER():" + CHR(10)
                       + "  RETURN".

    cTblList = _Q._TblList.
    DO ix = 1 TO NUM-ENTRIES(cTblList):
      cTbl = ENTRY(NUM-ENTRIES(ENTRY(1,ENTRY(ix,cTblList)," "),"."),ENTRY(1,ENTRY(ix,cTblList)," "),".").
      cViewDef = cViewDef + CHR(10) + "  " + (IF ix = 1 THEN "  '" ELSE "+ ',") + cTbl + "'".
      FOR EACH _BC 
          WHERE _BC._x-recid = RECID(_U)
            AND _BC._TABLE   = cTbl
          BY _BC._SEQUENCE:
        cViewDef = cViewDef + CHR(10) + "     + ';" + _BC._NAME + "'". 
        IF CAN-DO(cFldList,_BC._NAME) THEN 
          ASSIGN _BC._DEF-VALEXP = _BC._NAME + STRING(ix)
                 _BC._DISP-NAME  = _BC._DISP-NAME + STRING(ix).
        ELSE _BC._DEF-VALEXP = _BC._NAME.
        cFldList = cFldList + (IF cFldList NE "" THEN "," ELSE "") + _BC._DEF-VALEXP.
      END.
      FOR EACH _BC 
          WHERE _BC._x-recid = RECID(_U)
            AND _BC._TABLE   = ?
            AND _BC._DBNAME  = "_<CALC>" 
            AND ENTRY(1,_BC._DISP-NAME,".") = cTbl
            AND _BC._DEF-VALEXP NE ""
          BY _BC._SEQUENCE:
        cCalcFld = _BC._DEF-VALEXP.

        IF NUM-ENTRIES(cCalcFld,"|") > 1 AND ENTRY(1,cCalcFld,"|") NE "" THEN DO:
          IF NOT CAN-DO(cCalcProcs,ENTRY(1,cCalcFld,"|")) THEN
            ASSIGN cCalcProcs    = cCalcProcs + (IF cCalcProcs NE "" THEN "," ELSE "") + ENTRY(1,cCalcFld,"|")
                   cCalcProcFlds = cCalcProcFlds + (IF cCalcProcFlds NE "" THEN ";" ELSE "") + cCalcFld
                   .
          ELSE cCalcProcFlds = cCalcProcFlds + (IF cCalcProcFlds NE "" THEN "," ELSE "") + ENTRY(2,cCalcFld,"|").
        END.
        cCalcFld = ENTRY(NUM-ENTRIES(cCalcFld,"|"),cCalcFld,"|").

        cViewDef = cViewDef + CHR(10) + "     + ';+" + ENTRY(NUM-ENTRIES(ENTRY(1,_BC._DISP-NAME),"."),ENTRY(1,_BC._DISP-NAME," "),".")
                 + "|" + (IF _BC._DATA-TYPE NE ? THEN _BC._DATA-TYPE ELSE "CHARACTER")
                 + "||" + cCalcFld + "|" + _BC._LABEL + "'"
                 . 
      END.
    END.
    cViewDef = cViewDef + CHR(10) + "     ." + CHR(10) + "END FUNCTION.".

    FOR EACH _BC 
        WHERE _BC._x-recid   = RECID(_U)
           BY _BC._SEQUENCE:         

      IF _BC._DEF-VALEXP NE "" AND SUBSTR(_BC._DEF-VALEXP,LENGTH(_BC._DEF-VALEXP)) = "]" THEN
        ASSIGN cDbExt    = SUBSTR(_BC._DEF-VALEXP,INDEX(_BC._DEF-VALEXP,"["))
               cTtExt    = "_" + REPLACE(SUBSTR(cDbExt,2),"]","")
               cFldName  = SUBSTR(_BC._DEF-VALEXP,1,INDEX(_BC._DEF-VALEXP,"[") - 1)
              .
      ELSE IF _BC._DEF-VALEXP = "" OR (_BC._TABLE = ? AND _BC._DBNAME = "_<CALC>") THEN DO:
        ASSIGN cFldName = ENTRY(2,ENTRY(1,_BC._DISP-NAME," "),".")
/*         ASSIGN cFldName = ENTRY(NUM-ENTRIES(ENTRY(1,_BC._DISP-NAME," "),"'"),ENTRY(1,_BC._DISP-NAME," "),".") */
               cDbExt   = ""
               cTtExt   = ""
               .
      END.
      ELSE 
        ASSIGN cFldName = _BC._DEF-VALEXP
               cDbExt   = ""
               cTtExt   = ""
               .

      IF _BC._VISIBLE THEN
        cDisplTrg = cDisplTrg + (IF cDisplTrg NE '' THEN CHR(10) ELSE '')
                  + ttObj.cFirstTable + '.' + cFldName + cTtExt
                  + (IF _BC._FORMAT NE "" AND _BC._FORMAT NE ? THEN ' FORMAT "' + _BC._FORMAT + '"' 
                     ELSE IF _BC._DEF-FORMAT NE "" AND _BC._DEF-FORMAT NE ? THEN ' FORMAT "' + _BC._DEF-FORMAT + '"'
                     ELSE "")
                  + (IF _BC._LABEL  NE "" AND _BC._LABEL  NE ? THEN ' LABEL "'  + _BC._LABEL  + '"' 
                     ELSE IF _BC._DEF-LABEL  NE "" AND _BC._DEF-LABEL  NE ? THEN ' LABEL "'  + _BC._DEF-LABEL  + '"' 
                     ELSE "")
                  + (IF _BC._WIDTH NE 0 AND _BC._WIDTH NE ? THEN " WIDTH " + REPLACE(STRING(_BC._WIDTH),",",".") ELSE "")
                  .
      cTtFieldDef = cTtFieldDef + "    FIELD " + cFldName + cTtExt
                  + " AS " + (IF _BC._DATA-TYPE NE ? THEN _BC._DATA-TYPE ELSE "CHAR") + CHR(10)
                  .
      IF cTtExt NE "" THEN
        cTtFieldDef  = cTtFieldDef + "    FIELD jbextent" + cTtExt + "_" + cFldName + " AS " + _BC._DATA-TYPE + " /* placeholder for calculation */" + CHR(10).
      IF bFreeform THEN DELETE _BC.
      ELSE
        ASSIGN _BC._LABEL  = (IF _BC._LABEL NE "" THEN _BC._LABEL ELSE _BC._DEF-LABEL)
               _BC._FORMAT = (IF _BC._FORMAT NE "" THEN _BC._FORMAT ELSE _BC._DEF-FORMAT).
               _BC._HELP   = REPLACE(_BC._HELP,'"',"'")
              .
    END.
    IF cDisplTrg NE "" THEN DO:
      IF bFreeform AND AVAIL _TRG THEN _TRG._tCODE = cDisplTrg + CHR(10) + "ENABLE " + ENTRY(1,cDisplTrg," ").

      DO ix = 1 TO NUM-ENTRIES(cTblList):
        cTtFieldDef  = cTtFieldDef + "    FIELD RowIdent" + STRING(ix) + " AS CHARACTER " + CHR(10).
        cRowIdentIdx = cRowIdentIdx + " RowIdent" + STRING(ix).  
      END.

      cTtFieldDef = cTtFieldDef
                  + "    FIELD RowCount AS INTEGER" + CHR(10)
                  + (IF ttObj.cObjectType = "browse" THEN
                     "    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1" + CHR(10)
                   + "    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'" + CHR(10)
                   ELSE "")
                 + "    INDEX idxRowids " + cRowIdentIdx.

      FIND FIRST _TRG
           WHERE _TRG._tSECTION = "_CUSTOM":U
             AND _TRG._pRECID   = RECID(_P)
             AND _TRG._tEVENT   = "_DEFINITIONS":U
           NO-ERROR.
      IF AVAIL _TRG THEN DO:
        IF ttObj.cObjectState = "new" THEN
          _TRG._tCODE = _TRG._tCODE + CHR(10) + ttObj.cTtCode + cTtFieldDef + CHR(10) + "    ."
                      + CHR(10) + "DEF BUFFER v_" + ttObj.cFirstTable + " FOR TEMP-TABLE " + ttObj.cFirstTable + "." + CHR(10).
        ELSE
          _TRG._tCODE = REPLACE(_TRG._tCODE,ttObj.cTtCode,cTtFieldDef).

        ASSIGN cQueryJoin  = (IF NUM-ENTRIES(_Q._4GLQury,CHR(10)) > 1 THEN SUBSTR(_Q._4GLQury,INDEX(_Q._4GLQury,CHR(10))) ELSE "")
               cQueryJoin  = REPLACE(cQueryJoin,cCurrDb + ".","")
               .
        REPEAT WHILE INDEX(cQueryJoin,"  ") > 0:
          cQueryJoin = REPLACE(cQueryJoin,"  "," ").
        END.

        IF cCalcProcs NE "" THEN DO:
          DO ix = 1 TO NUM-ENTRIES(cCalcProcFlds,";"):
            IF TRIM(ENTRY(ix,cCalcProcs),"+") NE "" THEN
              DO iy = 1 TO NUM-ENTRIES(ENTRY(2,ENTRY(ix,cCalcProcFlds,";"),"|")):
                cCustomCalcProcs = cCustomCalcProcs + CHR(10)
                                 + (IF cCustomCalcProcs NE "" THEN "   + '," ELSE "     '")
                                 + ENTRY(ix,cCalcProcs) + "' /* " + ENTRY(iy,ENTRY(2,ENTRY(ix,cCalcProcFlds,";"),"|")) + " */"
                                 .
              END.
              /*
              cCustomCalcProcs = cCustomCalcProcs + CHR(10)
                          + (IF cCustomCalcProcs NE "" THEN "   + '," ELSE "     '")
                          + ENTRY(ix,cCalcProcs) + "' /* " + ENTRY(2,ENTRY(ix,cCalcProcFlds,";"),"|") + " */"
                            .
              */              
          END.
          IF cCustomCalcProcs NE "" THEN
            cCalcProcFunc = CHR(10)
                          + "FUNCTION getCalcFieldProc" + ttObj.cWidgetName + " RETURNS CHARACTER():" + CHR(10)
                          + "  RETURN " 
                          + cCustomCalcProcs  + CHR(10) + "     ." + CHR(10)
                          + "END FUNCTION."
                          .
        END.

        ASSIGN cQueryJoin       = REPLACE(cQueryJoin,CHR(10),"")
               cQueryJoin       = REPLACE(cQueryJoin,", EACH",",EACH")
               cQueryJoin       = REPLACE(cQueryJoin,", FIRST",",FIRST")
               cQueryJoin       = REPLACE(cQueryJoin,", LAST",",LAST")
               cQueryJoin       = REPLACE(cQueryJoin," NO-LOCK, NO-LOCK,"," NO-LOCK,")
               cQueryJoin       = REPLACE(cQueryJoin,"INDEXED-REPOSITION","")
               cQueryJoin       = REPLACE(cQueryJoin," NO-LOCK  NO-LOCK"," NO-LOCK")
               cQueryJoin       = TRIM(cQueryJoin)
               cQueryJoin       = DYNAMIC-FUNCTION("FixQueryOperators",cQueryJoin)
               cQueryJoin       = FixQueryJoin(cQueryJoin)
               cViewDef         = cViewDef + CHR(10)
                                + "FUNCTION getQueryJoin" + ttObj.cWidgetName + " RETURNS CHARACTER():" + CHR(10)
                                + "  RETURN '" + cQueryJoin + "'." + CHR(10)
                                + "END FUNCTION."
                                + cCalcProcFunc
              .
        IF ttObj.cObjectState = "new" THEN
          _TRG._tCODE = _TRG._tCODE + CHR(10) + CHR(10) + cViewDef.
        ELSE
          _TRG._tCODE = REPLACE(_TRG._tCODE,ttObj.cViewCode,cViewDef).
      END.
      IF ttObj.cObjectState = "new" THEN DO:
        /* Insert the code for InitializeObject */
        FIND FIRST _TRG
             WHERE _TRG._tSECTION = "_PROCEDURE":U
               AND _TRG._pRECID   = RECID(_P)
               AND _TRG._tEVENT   = "InitializeObject":U
             NO-ERROR.
        IF NOT AVAIL _TRG THEN DO:
          cInitProc = selectProcForObjectInitCode().
          IF cInitProc NE "" THEN
            FIND FIRST _TRG
                 WHERE _TRG._tSECTION = "_PROCEDURE":U
                   AND _TRG._pRECID   = RECID(_P)
                   AND _TRG._tEVENT   = cInitProc
                 NO-ERROR.
        END.
        IF AVAIL _TRG THEN DO:
          cInitObjCode = "  oBrw" + ttObj.cFirstTable + " = NEW JBoxBrowse(brw" + ttObj.cFirstTable + ":HANDLE)." + CHR(10).
          IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
            _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",cInitObjCode + CHR(10) + "END." + CHR(10) + "oBrw" + ttObj.cFirstTable + ":OpenQuery().").              
          ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
            ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
                   cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
                   cCode2 = REPLACE(cCode2,"END.",cInitObjCode + CHR(10) + "END." + CHR(10) + "oBrw" + ttObj.cFirstTable + ":OpenQuery().")
                   _TRG._tCODE = cCode1 + cCode2
                   .
          ELSE 
            _TRG._tCODE = _TRG._tCODE + CHR(10) + cInitObjCode.         
        END.

        FIND FIRST _TRG
             WHERE _TRG._tSECTION = "_CUSTOM":U
               AND _TRG._pRECID   = RECID(_P)
               AND _TRG._tEVENT   = "_MAIN-BLOCK":U
             NO-ERROR.
        
        IF AVAIL _TRG AND _TRG._tCODE MATCHES "*oBrw<>:BROWSE-HANDLE*" THEN
          _TRG._tCODE = REPLACE(_TRG._tCODE,"/*" + CHR(123) + "incl/conttrigg.i oBrw<>:BROWSE-HANDLE" + CHR(125) + " */",CHR(123) + "incl/conttrigg.i oBrw" + ttObj.cFirstTable + ":BROWSE-HANDLE" + CHR(125)).
      END.
      /* re-create the definition temp-tables so that the user will keep changes until ok or close */
      RUN findTablesAndFieldsInUse.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genBrwOrQryDef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genBrwOrQryDef Procedure 
PROCEDURE genBrwOrQryDef :
/*------------------------------------------------------------------------------
  Purpose:    (re)creates the temp-table, viewdef and disp.trigger (for browse)
  Parameters: _WINDOW-HANDLE, WidgetName, Type (browse or query)
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWin          AS HANDLE NO-UNDO.
DEF INPUT PARAM icBrwOrQryName AS CHAR   NO-UNDO.
DEF INPUT PARAM icType         AS CHAR   NO-UNDO.

DEF VAR cDisplTrg         AS CHAR NO-UNDO.
DEF VAR cTtFieldDef       AS CHAR NO-UNDO.
DEF VAR cViewDef          AS CHAR NO-UNDO.
DEF VAR cFldName          AS CHAR NO-UNDO.
DEF VAR cDbExt            AS CHAR NO-UNDO.
DEF VAR cTtExt            AS CHAR NO-UNDO.
DEF VAR cTblList          AS CHAR NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iy                AS INT  NO-UNDO.
DEF VAR cRowIdentIdx      AS CHAR NO-UNDO.
DEF VAR cCurrDb           AS CHAR NO-UNDO.
DEF VAR cFldList          AS CHAR NO-UNDO.
DEF VAR cQueryJoin        AS CHAR NO-UNDO.
DEF VAR cTbl              AS CHAR NO-UNDO.
DEF VAR cCalcProcs        AS CHAR NO-UNDO.
DEF VAR cCalcFld          AS CHAR NO-UNDO.
DEF VAR cCalcProcFlds     AS CHAR NO-UNDO.
DEF VAR cCalcProcFunc     AS CHAR NO-UNDO.
DEF VAR cCustomCalcProcs  AS CHAR NO-UNDO.
DEF VAR cFirstTable       AS CHAR NO-UNDO.

IF bCancelQueryRebuild THEN DO:
  bCancelQueryRebuild = NO.
  RETURN.
END.

/*
DEF VAR h_bc AS HANDLE NO-UNDO.
h_bc = BUFFER ttWhere:HANDLE.
RUN TOexcelviafile.p(h_bc,-1).
*/  

IF ihWin = ? THEN ihWin = _h_win.

FIND _P WHERE _P._WINDOW-HANDLE = ihWin NO-ERROR.
FIND _U WHERE RECID(_U) = _query-u-rec NO-ERROR.
FIND _C WHERE RECID(_C) = _U._x-recid NO-ERROR.
FIND _Q WHERE RECID(_Q) = _C._q-recid NO-ERROR.

MESSAGE AVAIL _P SKIP AVAIL _U SKIP _Q._TblList
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

IF NOT AVAIL _P OR NOT AVAIL _U OR NOT AVAIL _C OR NOT AVAIL _Q THEN RETURN.

cCurrDb = IF _db_name NE "" THEN _db_name ELSE LDBNAME("DICTDB").
cFirstTable = ENTRY(NUM-ENTRIES(ENTRY(1,_Q._TblList),"."),ENTRY(1,_Q._TblList),"."). 

MESSAGE cFirstTable
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
RETURN.

FIND FIRST _TRG 
     WHERE _TRG._tSECTION = "_CONTROL":U
       AND _TRG._pRECID   = RECID(_P)
       AND _TRG._wRECID   = RECID(_U)
       AND _TRG._tEVENT   = "DISPLAY":U
       AND _TRG._tSPECIAL = "_DISPLAY-FIELDS"
     NO-ERROR.

FIND FIRST ttObj 
     WHERE ttObj.cWidgetName = _U._NAME
     NO-ERROR.        

IF AVAIL _TRG AND AVAIL ttObj THEN DO:
  ASSIGN cTtFieldDef = "DEF TEMP-TABLE " + ttObj.cFirstTable + CHR(10).
         cViewDef    = "FUNCTION getBuffersAndFields" + ttObj.cWidgetName + " RETURNS CHARACTER():" + CHR(10)
                     + "  RETURN".

  cTblList = _Q._TblList.
  DO ix = 1 TO NUM-ENTRIES(cTblList):
    cTbl = ENTRY(2,ENTRY(1,ENTRY(ix,cTblList)," "),".").
    cViewDef = cViewDef + CHR(10) + "  " + (IF ix = 1 THEN "  '" ELSE "+ ',") + cTbl + "'".
    FOR EACH _BC 
        WHERE _BC._x-recid = RECID(_U)
          AND _BC._TABLE   = cTbl
        BY _BC._SEQUENCE:
      cViewDef = cViewDef + CHR(10) + "     + ';" + _BC._NAME + "'". 
      IF CAN-DO(cFldList,_BC._NAME) THEN _BC._DEF-VALEXP = _BC._NAME + STRING(ix).
      ELSE _BC._DEF-VALEXP = _BC._NAME.
      cFldList = cFldList + (IF cFldList NE "" THEN "," ELSE "") + _BC._DEF-VALEXP.
    END.
    FOR EACH _BC 
        WHERE _BC._x-recid = RECID(_U)
          AND _BC._TABLE   = ?
          AND _BC._DBNAME  = "_<CALC>" 
          AND ENTRY(1,_BC._DISP-NAME,".") = cTbl
        BY _BC._SEQUENCE:
      cCalcFld = _BC._DEF-VALEXP.

      IF NUM-ENTRIES(cCalcFld,"|") > 1 THEN DO:
        IF NOT CAN-DO(cCalcProcs,ENTRY(1,cCalcFld,"|")) THEN
          ASSIGN cCalcProcs    = cCalcProcs + (IF cCalcProcs NE "" THEN "," ELSE "") + ENTRY(1,cCalcFld,"|")
                 cCalcProcFlds = cCalcProcFlds + (IF cCalcProcFlds NE "" THEN ";" ELSE "") + cCalcFld
                 .
        ELSE cCalcProcFlds = cCalcProcFlds + "," + ENTRY(2,cCalcFld,"|").
      END.
      cCalcFld = ENTRY(NUM-ENTRIES(cCalcFld,"|"),cCalcFld,"|").

      cViewDef = cViewDef + CHR(10) + "     + ';+" + ENTRY(2,ENTRY(1,_BC._DISP-NAME," "),".")
               + "|" + (IF _BC._DATA-TYPE NE ? THEN _BC._DATA-TYPE ELSE "CHARACTER")
               + "||" + cCalcFld + "'"
               . 
    END.
  END.
  cViewDef = cViewDef + CHR(10) + "     ." + CHR(10) + "END FUNCTION.".

  FOR EACH _BC 
      WHERE _BC._x-recid   = RECID(_U)
         BY _BC._SEQUENCE:         

    IF _BC._DEF-VALEXP NE "" AND SUBSTR(_BC._DEF-VALEXP,LENGTH(_BC._DEF-VALEXP)) = "]" THEN
      ASSIGN cDbExt    = SUBSTR(_BC._DEF-VALEXP,INDEX(_BC._DEF-VALEXP,"["))
             cTtExt    = "_" + REPLACE(SUBSTR(cDbExt,2),"]","")
             cFldName  = SUBSTR(_BC._DEF-VALEXP,1,INDEX(_BC._DEF-VALEXP,"[") - 1)
            .
    ELSE IF _BC._DEF-VALEXP = "" OR (_BC._TABLE = ? AND _BC._DBNAME = "_<CALC>") THEN DO:
      ASSIGN cFldName = ENTRY(2,ENTRY(1,_BC._DISP-NAME," "),".")
             cDbExt   = ""
             cTtExt   = ""
             .
    END.
    ELSE 
      ASSIGN cFldName = _BC._DEF-VALEXP
             cDbExt   = ""
             cTtExt   = ""
             .

    IF _BC._VISIBLE THEN
      cDisplTrg = cDisplTrg + (IF cDisplTrg NE '' THEN CHR(10) ELSE '')
                + ttObj.cFirstTable + '.' + cFldName + cTtExt
                + (IF _BC._FORMAT NE "" AND _BC._FORMAT NE ? THEN ' FORMAT "' + _BC._FORMAT + '"' 
                   ELSE IF _BC._DEF-FORMAT NE "" AND _BC._DEF-FORMAT NE ? THEN ' FORMAT "' + _BC._DEF-FORMAT + '"'
                   ELSE "")
                + (IF _BC._LABEL  NE "" AND _BC._LABEL  NE ? THEN ' LABEL "'  + _BC._LABEL  + '"' 
                   ELSE IF _BC._DEF-LABEL  NE "" AND _BC._DEF-LABEL  NE ? THEN ' LABEL "'  + _BC._DEF-LABEL  + '"' 
                   ELSE "")
                + (IF _BC._WIDTH NE 0 AND _BC._WIDTH NE ? THEN " WIDTH " + REPLACE(STRING(_BC._WIDTH),",",".") ELSE "")
                .
    cTtFieldDef = cTtFieldDef + "    FIELD " + cFldName + cTtExt
                + " AS " + (IF _BC._DATA-TYPE NE ? THEN _BC._DATA-TYPE ELSE "CHAR") + CHR(10)
                .
    IF cTtExt NE "" THEN
      cTtFieldDef  = cTtFieldDef + "    FIELD jbextent" + cTtExt + "_" + cFldName + " AS " + _BC._DATA-TYPE + " /* placeholder for calculation */" + CHR(10).
    DELETE _BC.
  END.
  IF cDisplTrg NE "" THEN DO:
    _TRG._tCODE = cDisplTrg + CHR(10) + "ENABLE " + ENTRY(1,cDisplTrg," ").

    DO ix = 1 TO NUM-ENTRIES(cTblList):
      cTtFieldDef  = cTtFieldDef + "    FIELD RowIdent" + STRING(ix) + " AS CHARACTER " + CHR(10).
      cRowIdentIdx = cRowIdentIdx + " RowIdent" + STRING(ix).  
    END.

    cTtFieldDef = cTtFieldDef
                + "    FIELD RowCount AS INTEGER" + CHR(10)
                + (IF ttObj.cObjectType = "browse" THEN
                   "    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1" + CHR(10)
                 + "    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'" + CHR(10)
                 ELSE "")
               + "    INDEX idxRowids " + cRowIdentIdx.

    FIND FIRST _TRG
         WHERE _TRG._tSECTION = "_CUSTOM":U
           AND _TRG._pRECID   = RECID(_P)
           AND _TRG._tEVENT   = "_DEFINITIONS":U
         NO-ERROR.
    IF AVAIL _TRG THEN DO:
      ASSIGN _TRG._tCODE = REPLACE(_TRG._tCODE,ttObj.cTtCode,cTtFieldDef)
             cQueryJoin  = (IF NUM-ENTRIES(_Q._4GLQury,CHR(10)) > 1 THEN SUBSTR(_Q._4GLQury,INDEX(_Q._4GLQury,CHR(10))) ELSE "")
             cQueryJoin  = REPLACE(cQueryJoin,cCurrDb + ".","")
             .
      REPEAT WHILE INDEX(cQueryJoin,"  ") > 0:
        cQueryJoin = REPLACE(cQueryJoin,"  "," ").
      END.

      IF cCalcProcs NE "" THEN DO:
        DO ix = 1 TO NUM-ENTRIES(cCalcProcFlds,";"):
          IF TRIM(ENTRY(ix,cCalcProcs),"+") NE "" THEN
            cCustomCalcProcs = cCustomCalcProcs + CHR(10)
                        + (IF cCustomCalcProcs NE "" THEN "   + '," ELSE "     '")
                        + ENTRY(ix,cCalcProcs) + "' /* " + ENTRY(2,ENTRY(ix,cCalcProcFlds,";"),"|") + " */"
                          .
        END.
        IF cCustomCalcProcs NE "" THEN
          cCalcProcFunc = CHR(10)
                        + "FUNCTION getCalcFieldProc" + ttObj.cWidgetName + " RETURNS CHARACTER():" + CHR(10)
                        + "  RETURN " 
                        + cCustomCalcProcs  + CHR(10) + "     ." + CHR(10)
                        + "END FUNCTION."
                        .
      END.

      ASSIGN cQueryJoin       = REPLACE(cQueryJoin,CHR(10),"")
             cQueryJoin       = REPLACE(cQueryJoin,", EACH",",EACH")
             cQueryJoin       = REPLACE(cQueryJoin,", FIRST",",FIRST")
             cQueryJoin       = REPLACE(cQueryJoin,", LAST",",LAST")
             cQueryJoin       = REPLACE(cQueryJoin,"INDEXED-REPOSITION","")
             cQueryJoin       = TRIM(cQueryJoin)
             cViewDef         = cViewDef + CHR(10)
                              + "FUNCTION getQueryJoin" + ttObj.cWidgetName + " RETURNS CHARACTER():" + CHR(10)
                              + "  RETURN '" + cQueryJoin + "'." + CHR(10)
                              + "END FUNCTION."
                              + cCalcProcFunc
             _TRG._tCODE = REPLACE(_TRG._tCODE,ttObj.cViewCode,cViewDef)
             .
    END.
    /* re-create the definition temp-tables so that the user will keep changes until ok or close */
    RUN findTablesAndFieldsInUse.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genDotNetControl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genDotNetControl Procedure 
PROCEDURE genDotNetControl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icControl     AS CHAR NO-UNDO.
DEF INPUT PARAM icCode        AS CHAR NO-UNDO.

DEF BUFFER b_trg FOR _TRG.

DEF VAR cTableRef AS CHAR NO-UNDO.
DEF VAR ix        AS INT  NO-UNDO.
DEF VAR cPackage  AS CHAR NO-UNDO.
DEF VAR cLine     AS CHAR NO-UNDO.
DEF VAR cField    AS CHAR NO-UNDO.
DEF VAR cCode1    AS CHAR NO-UNDO.
DEF VAR cCode2    AS CHAR NO-UNDO.
DEF VAR cInitProc AS CHAR NO-UNDO.

IF NUM-ENTRIES(icControl,".") > 1 THEN
  ASSIGN cPackage = SUBSTR(icControl,1,R-INDEX(icControl,"."))
         icControl = SUBSTR(icControl,R-INDEX(icControl,".") + 1)
         .
DO ix = 1 TO NUM-ENTRIES(icCode,CHR(10)):
  cLine = ENTRY(ix,icCode,CHR(10)).
  IF cLine MATCHES "*:HANDLE)." THEN
    ASSIGN cField = ENTRY(NUM-ENTRIES(cLine),cLine)
           cField = ENTRY(1,cField,":").
END.
IF cField = "" THEN cField = icControl.

IF NOT AVAIL _P THEN RETURN.

FIND LAST _U WHERE _U._NAME BEGINS "EDITOR-"
     USE-INDEX _HANDLE
     NO-ERROR.
IF AVAIL _U THEN 
  _U._NAME  = cField. /* + "_AssignedByJbox". */

FIND FIRST b_TRG
     WHERE b_TRG._tSECTION = "_CUSTOM":U
       AND b_TRG._pRECID   = RECID(_P)
       AND b_TRG._tEVENT   = "_DEFINITIONS":U
     NO-ERROR.
IF AVAIL b_TRG THEN DO:
  IF b_TRG._tCODE MATCHES "*/* &SCOPED-DEFINE AdvGuiWin */*" THEN
    b_trg._tCODE = REPLACE(b_trg._tCODE,"/* &SCOPED-DEFINE AdvGuiWin */","&SCOPED-DEFINE AdvGuiWin").

  IF NOT b_TRG._tCODE MATCHES "*o" + icControl + "*" THEN  
    b_TRG._tCODE = b_TRG._tCODE + CHR(10) + 
                  "DEF VAR o" + icControl + " AS " + cPackage + icControl + " NO-UNDO.".             
END.

FIND FIRST b_trg
     WHERE b_trg._tSECTION = "_PROCEDURE":U
       AND b_trg._pRECID   = RECID(_P)
       AND b_trg._tEVENT   = "InitializeObject":U
     NO-ERROR.
IF NOT AVAIL _TRG THEN DO:
  cInitProc = selectProcForObjectInitCode().
  IF cInitProc NE "" THEN
    FIND FIRST _TRG
         WHERE _TRG._tSECTION = "_PROCEDURE":U
           AND _TRG._pRECID   = RECID(_P)
           AND _TRG._tEVENT   = cInitProc
         NO-ERROR.
END.

IF NOT AVAIL b_trg THEN RETURN.

IF NOT b_trg._tCODE MATCHES "*InitializeComponents*" THEN DO:
  IF b_trg._tCODE MATCHES "*END.*" AND INDEX(b_trg._tCODE,"END.") = R-INDEX(b_trg._tCODE,"END.") THEN 
    b_trg._tCODE = REPLACE(b_trg._tCODE,"END.","  RUN InitializeComponents." + CHR(10) + "END.").         
  ELSE IF b_trg._tCODE MATCHES "*END.*" THEN 
    ASSIGN cCode1 = SUBSTR(b_trg._tCODE,1,R-INDEX(b_trg._tCODE,"END.") - 1)  
           cCode2 = SUBSTR(b_trg._tCODE,R-INDEX(b_trg._tCODE,"END."))
           cCode2 = REPLACE(cCode2,"END.","  RUN InitializeComponents." + CHR(10) + "END.")
           b_trg._tCODE = cCode1 + cCode2
           .
  ELSE
    b_trg._tCODE = b_trg._tCODE + CHR(10) + "RUN InitializeComponents.".                
END.

FIND FIRST _trg 
     WHERE _trg._tSECTION = "_PROCEDURE":U
       AND _trg._pRECID   = RECID(_P)
       AND _trg._tEVENT   = "InitializeComponents"
     NO-ERROR.

IF NOT AVAIL _trg THEN DO:
  CREATE _trg.
  BUFFER-COPY b_trg TO _trg
  ASSIGN _trg._tEVENT = "InitializeComponents"
         _trg._tCODE  = "&IF DEFINED(AdvGuiWin) &THEN"
                      + CHR(10) + REPLACE("DO WITH FRAME <&FRAME-NAME}:","<",CHR(123))
                      + CHR(10) + CHR(10)
                      + "END."
                      + CHR(10) + "&ENDIF"
                      + CHR(10) + "END PROCEDURE."
                      .
END.

IF NOT _TRG._tCODE MATCHES "*o" + icControl + "*" THEN DO:
  IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
    _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",icCode + CHR(10) + "END.").         
  ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
    ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
           cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
           cCode2 = REPLACE(cCode2,"END.",icCode + CHR(10) + "END.")
           _TRG._tCODE = cCode1 + cCode2
           .
  ELSE 
    _TRG._tCODE = _TRG._tCODE + CHR(10) + icCode.                
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genJlwTabFolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genJlwTabFolder Procedure 
PROCEDURE genJlwTabFolder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icTabName  AS CHAR NO-UNDO.
DEF INPUT PARAM icCode     AS CHAR NO-UNDO.

DEF BUFFER b_trg FOR _TRG.

DEF VAR bOk       AS LOG  NO-UNDO.
DEF VAR cCode1    AS CHAR NO-UNDO.
DEF VAR cCode2    AS CHAR NO-UNDO.
DEF VAR cInitProc AS CHAR NO-UNDO.

IF NOT AVAIL _P THEN RETURN.

FIND LAST _U WHERE _U._NAME BEGINS "RECT-"
     USE-INDEX _HANDLE
     NO-ERROR.
IF AVAIL _U THEN 
  _U._NAME  = icTabName. /* + "_AssignedByJbox". */

FIND FIRST b_TRG
     WHERE b_TRG._tSECTION = "_CUSTOM":U
       AND b_TRG._pRECID   = RECID(_P)
       AND b_TRG._tEVENT   = "_DEFINITIONS":U
     NO-ERROR.
IF AVAIL b_TRG THEN DO:

  IF NOT b_TRG._tCODE MATCHES "*o" + icTabName + "*" THEN  
    b_TRG._tCODE = b_TRG._tCODE + CHR(10) + 
                  "DEF VAR o" + icTabName + " AS JBoxJlwTabs NO-UNDO.".             
END.

FIND FIRST _trg
     WHERE _trg._tSECTION = "_PROCEDURE":U
       AND _trg._pRECID   = RECID(_P)
       AND _trg._tEVENT   = "InitializeObject":U
     NO-ERROR.
IF NOT AVAIL _TRG THEN DO:
  cInitProc = selectProcForObjectInitCode().
  IF cInitProc NE "" THEN
    FIND FIRST _TRG
         WHERE _TRG._tSECTION = "_PROCEDURE":U
           AND _TRG._pRECID   = RECID(_P)
           AND _TRG._tEVENT   = cInitProc
         NO-ERROR.
END.

IF NOT AVAIL _trg THEN 
  RETURN.  

IF icCode NE "" THEN DO:

  IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
    _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",icCode + CHR(10) + "END.").         
  ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
    ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
           cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
           cCode2 = REPLACE(cCode2,"END.",icCode + CHR(10) + "END.")
           _TRG._tCODE = cCode1 + cCode2
           .
  ELSE IF AVAIL _TRG THEN
    _TRG._tCODE = _TRG._tCODE + CHR(10) + icCode.                

END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genMsTabFolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genMsTabFolder Procedure 
PROCEDURE genMsTabFolder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icTabName  AS CHAR NO-UNDO.
DEF INPUT PARAM icCode     AS CHAR NO-UNDO.

DEF BUFFER b_trg FOR _TRG.

DEF VAR bOk       AS LOG  NO-UNDO.
DEF VAR cCode1    AS CHAR NO-UNDO.
DEF VAR cCode2    AS CHAR NO-UNDO.
DEF VAR cInitProc AS CHAR NO-UNDO.

IF NOT AVAIL _P THEN RETURN.

FIND LAST _U WHERE _U._NAME BEGINS "RECT-"
     USE-INDEX _HANDLE
     NO-ERROR.
IF AVAIL _U THEN 
  _U._NAME  = icTabName. /* + "_AssignedByJbox". */

FIND FIRST b_TRG
     WHERE b_TRG._tSECTION = "_CUSTOM":U
       AND b_TRG._pRECID   = RECID(_P)
       AND b_TRG._tEVENT   = "_DEFINITIONS":U
     NO-ERROR.
IF AVAIL b_TRG THEN DO:
  IF b_TRG._tCODE MATCHES "*/* &SCOPED-DEFINE AdvGuiWin */*" THEN
    b_trg._tCODE = REPLACE(b_trg._tCODE,"/* &SCOPED-DEFINE AdvGuiWin */","&SCOPED-DEFINE AdvGuiWin").

  IF NOT b_TRG._tCODE MATCHES "*o" + icTabName + "*" THEN  
    b_TRG._tCODE = b_TRG._tCODE + CHR(10) + 
                  "DEF VAR o" + icTabName + " AS JBoxMsTabs NO-UNDO.".             
END.

FIND FIRST _trg
     WHERE _trg._tSECTION = "_PROCEDURE":U
       AND _trg._pRECID   = RECID(_P)
       AND _trg._tEVENT   = "InitializeObject":U
     NO-ERROR.
IF NOT AVAIL _TRG THEN DO:
  cInitProc = selectProcForObjectInitCode().
  IF cInitProc NE "" THEN
    FIND FIRST _TRG
         WHERE _TRG._tSECTION = "_PROCEDURE":U
           AND _TRG._pRECID   = RECID(_P)
           AND _TRG._tEVENT   = cInitProc
         NO-ERROR.
END.

IF NOT AVAIL _trg THEN 
  RETURN.  

IF icCode NE "" THEN DO:

  IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
    _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",icCode + CHR(10) + "END.").         
  ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
    ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
           cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
           cCode2 = REPLACE(cCode2,"END.",icCode + CHR(10) + "END.")
           _TRG._tCODE = cCode1 + cCode2
           .
  ELSE IF AVAIL _TRG THEN
    _TRG._tCODE = _TRG._tCODE + CHR(10) + icCode.                

END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genOverrideProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genOverrideProc Procedure 
PROCEDURE genOverrideProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icProcName   AS CHAR NO-UNDO.
DEF INPUT PARAM icCode       AS CHAR NO-UNDO.

DEF BUFFER b_trg FOR _TRG.

DEF VAR cTableRef AS CHAR NO-UNDO.
DEF VAR bOk       AS LOG  NO-UNDO.
DEF VAR cCode     AS CHAR NO-UNDO.
DEF VAR ix        AS INT  NO-UNDO.
DEF VAR cCode1    AS CHAR NO-UNDO.
DEF VAR cCode2    AS CHAR NO-UNDO.

IF NOT AVAIL _P THEN RETURN.

cCode = TRIM(icCode).

IF cCode BEGINS "FUNCTION " THEN DO:
  creFunction(cCode).
  RETURN.
END.
IF cCode BEGINS "ON " THEN DO:
  creTrigger(cCode).
  RETURN.
END.

IF icProcName = "" THEN RETURN.

IF cCode = "" THEN cCode = CHR(10) + CHR(10) + "RUN SUPER." + CHR(10).

FIND FIRST b_trg
     WHERE b_trg._tSECTION = "_PROCEDURE":U
       AND b_trg._pRECID   = RECID(_P)
       AND b_trg._tEVENT   = "InitializeObject":U
     NO-ERROR.
IF NOT AVAIL b_trg THEN DO:
  MESSAGE "Code missing procedure InitializeObject"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.  
END. 

FIND FIRST _TRG
     WHERE _TRG._tSECTION = "_PROCEDURE":U
       AND _TRG._pRECID   = RECID(_P)
       AND _TRG._tEVENT   = icProcName
     NO-ERROR.
IF NOT AVAIL _TRG THEN DO:
  CREATE _TRG.
  BUFFER-COPY b_trg TO _TRG
    ASSIGN _TRG._tEVENT = icProcName
           _TRG._tCODE  = REPLACE(cCode,"DEFAULT-FRAME",CHR(123) + "&FRAME-NAME}")
                        + CHR(10) + CHR(10) + "END PROCEDURE.".
           .
END.
ELSE IF icCode NE "" THEN DO:
  IF _TRG._tCODE = "" THEN
    _TRG._tCODE  = REPLACE(cCode,"DEFAULT-FRAME",CHR(123) + "&FRAME-NAME}")
                 + CHR(10) + CHR(10) + "END PROCEDURE.".
  ELSE DO:
    MESSAGE "Append existing code in " icProcName " with" SKIP
            cCode SKIP(1)
            "Select NO to replace existing code:" SKIP(1)
            SUBSTR(_TRG._tCODE,1,500) SKIP "..."
            VIEW-AS ALERT-BOX INFO BUTTONS YES-NO-CANCEL UPDATE bOK.
    IF bOk THEN DO:

      IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
        _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",cCode + CHR(10) + "END.").         
      ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
        ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
               cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
               cCode2 = REPLACE(cCode2,"END.",cCode + CHR(10) + "END.")
               _TRG._tCODE = cCode1 + cCode2
               .
      ELSE IF AVAIL _TRG THEN
        _TRG._tCODE = _TRG._tCODE + CHR(10) + cCode.                

      /*
      IF NUM-ENTRIES(_TRG._tCODE,"END.") > 1 THEN DO:
        ix = NUM-ENTRIES(_TRG._tCODE," END.").
        ENTRY(ix,_TRG._tCODE," END.") = 
           REPLACE(cCode,"DEFAULT-FRAME",CHR(123) + "&FRAME-NAME}") + CHR(10) + ENTRY(ix,_TRG._tCODE,"END.").
      END.
      ELSE       
        _TRG._tCODE = REPLACE(_TRG._tCode,"END PROCEDURE.",_TRG._tCODE + CHR(10) + CHR(10)
                              + REPLACE(cCode,"DEFAULT-FRAME",CHR(123) + "&FRAME-NAME}"))
                              + CHR(10) + CHR(10) + "END PROCEDURE."
                            .
      MESSAGE "_TRG._tCODE" SKIP _TRG._tCODE
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
          */
    END.
    ELSE IF NOT bOk THEN
      _TRG._tCODE  = REPLACE(cCode,"DEFAULT-FRAME",CHR(123) + "&FRAME-NAME}")
                   + CHR(10) + CHR(10) + "END PROCEDURE.".
  END.
END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genPopupCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genPopupCode Procedure 
PROCEDURE genPopupCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icTbName      AS CHAR NO-UNDO.
DEF INPUT  PARAM icToolsList   AS CHAR NO-UNDO.
DEF INPUT  PARAM icToolbarDef  AS CHAR NO-UNDO.
DEF INPUT  PARAM icBorder      AS CHAR NO-UNDO.
DEF INPUT  PARAM icAssignToQry AS CHAR NO-UNDO.
DEF INPUT  PARAM icAssignToFm  AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk          AS LOG  NO-UNDO INIT YES.

DEF VAR cTableRef AS CHAR NO-UNDO.
DEF VAR cCode1    AS CHAR NO-UNDO.
DEF VAR cCode2    AS CHAR NO-UNDO.
DEF VAR cInitProc AS CHAR NO-UNDO.

IF icTbName = "" THEN DO:
  IF (icAssignToQry BEGINS "oBrw" OR icAssignToQry BEGINS "oQry")
     AND CAN-DO(cTablesInUse,SUBSTR(icAssignToQry,5)) THEN
    cTableRef = SUBSTR(icAssignToQry,5).
  ELSE IF icAssignToFm BEGINS "oFm" 
     AND CAN-DO(cTablesInUse,SUBSTR(icAssignToFm,4)) THEN
    cTableRef = SUBSTR(icAssignToFm,4).
  ELSE cTableRef = cFirstTableRef.

  icTbName = "popup" + cTableRef.
END.

IF NOT AVAIL _P THEN RETURN.

icToolbarDef = ENTRY(1,icToolbarDef,CHR(1)).

FIND FIRST _TRG
     WHERE _TRG._tSECTION = "_CUSTOM":U
       AND _TRG._pRECID   = RECID(_P)
       AND _TRG._tEVENT   = "_DEFINITIONS":U
     NO-ERROR.
IF AVAIL _TRG THEN DO: 
  IF NOT _TRG._tCODE MATCHES "*o" + icTbName + "*" THEN  
    _TRG._tCODE = _TRG._tCODE + CHR(10) + 
                  "DEF VAR o" + icTbName + " AS JBoxPopupMenu NO-UNDO.".             
END.

FIND FIRST _TRG
     WHERE _TRG._tSECTION = "_PROCEDURE":U
       AND _TRG._pRECID   = RECID(_P)
       AND _TRG._tEVENT   = "InitializeObject":U
     NO-ERROR.

IF NOT AVAIL _TRG THEN DO:
  cInitProc = selectProcForObjectInitCode().
  IF cInitProc NE "" THEN
    FIND FIRST _TRG
         WHERE _TRG._tSECTION = "_PROCEDURE":U
           AND _TRG._pRECID   = RECID(_P)
           AND _TRG._tEVENT   = cInitProc
         NO-ERROR.
END.

IF AVAIL _TRG AND NOT _TRG._tCODE MATCHES "*" + icTbName + "*" THEN DO:
  cInitObject = "  o" + icTbName + " = NEW JBoxPopupMenu()." + CHR(10).
  IF icToolbarDef NE "" THEN
    cInitObject = cInitObject + "  o" + icTbName + ":AddToolGroup('" + icToolbarDef + "')." + CHR(10).

  IF _TRG._tCODE MATCHES "*" + icAssignToQry + "*" AND NOT _TRG._tCODE MATCHES "*" + icAssignToQry + ":POPUP-MENU-OBJECT*" THEN
    cInitObject = cInitObject + CHR(10) 
                + "  " + icAssignToQry + ":POPUP-MENU-OBJECT = o" + icTbName + ".". 
  
  IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
    _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",cInitObject + CHR(10) + "END.").         
  ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
    ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
           cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
           cCode2 = REPLACE(cCode2,"END.",cInitObject + CHR(10) + "END.")
           _TRG._tCODE = cCode1 + cCode2
           .
  ELSE IF AVAIL _TRG THEN
    _TRG._tCODE = _TRG._tCODE + CHR(10) + cInitObject.                
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genSearchFieldCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genSearchFieldCode Procedure 
PROCEDURE genSearchFieldCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.
DEF INPUT PARAM icCode      AS CHAR NO-UNDO.

DEF BUFFER b_trg FOR _TRG.

DEF VAR bOk       AS LOG  NO-UNDO.
DEF VAR cCode1    AS CHAR NO-UNDO.
DEF VAR cCode2    AS CHAR NO-UNDO.

IF NOT AVAIL _P THEN RETURN.

FIND LAST _U WHERE _U._NAME BEGINS "RECT-"
     USE-INDEX _HANDLE
     NO-ERROR.
IF AVAIL _U THEN 
  _U._NAME  = icFieldName. /* + "_AssignedByJbox". */


FIND FIRST _trg
     WHERE _trg._tSECTION = "_PROCEDURE":U
       AND _trg._pRECID   = RECID(_P)
       AND _trg._tEVENT   = "InitializeObject":U
     NO-ERROR.
IF NOT AVAIL _trg THEN DO:
  MESSAGE "Code belongs in procedure InitializeObject which does not exist"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.  
END. 

IF icCode NE "" THEN DO:
  IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
    _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",icCode + CHR(10) + "END.").         
  ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
    ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
           cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
           cCode2 = REPLACE(cCode2,"END.",icCode + CHR(10) + "END.")
           _TRG._tCODE = cCode1 + cCode2
           .
  ELSE IF AVAIL _TRG THEN
    _TRG._tCODE = _TRG._tCODE + CHR(10) + icCode.                

END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genToolbarCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genToolbarCode Procedure 
PROCEDURE genToolbarCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icTbName      AS CHAR NO-UNDO.
DEF INPUT  PARAM icToolsList   AS CHAR NO-UNDO.
DEF INPUT  PARAM icToolbarDef  AS CHAR NO-UNDO.
DEF INPUT  PARAM icBorder      AS CHAR NO-UNDO.
DEF INPUT  PARAM icAssignToQry AS CHAR NO-UNDO.
DEF INPUT  PARAM icAssignToFm  AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk          AS LOG  NO-UNDO INIT YES.

DEF VAR cTableRef AS CHAR NO-UNDO.
DEF VAR cCode1    AS CHAR NO-UNDO.
DEF VAR cCode2    AS CHAR NO-UNDO.
DEF VAR cInitProc AS CHAR NO-UNDO.

IF icTbName = "" THEN DO:
  IF (icAssignToQry BEGINS "oBrw" OR icAssignToQry BEGINS "oQry")
     AND CAN-DO(cTablesInUse,SUBSTR(icAssignToQry,5)) THEN
    cTableRef = SUBSTR(icAssignToQry,5).
  ELSE IF icAssignToFm BEGINS "oFm" 
     AND CAN-DO(cTablesInUse,SUBSTR(icAssignToFm,4)) THEN
    cTableRef = SUBSTR(icAssignToFm,4).
  ELSE cTableRef = cFirstTableRef.

  icTbName = "tb" + cTableRef.
END.

IF NOT AVAIL _P THEN RETURN.

FIND LAST _U 
     WHERE _U._WINDOW-HANDLE = _P._WINDOW-HANDLE
       AND _U._TYPE = "rectangle" 
       AND NOT _U._DELETED
     USE-INDEX _HANDLE
    NO-ERROR.
IF AVAIL _U  THEN 
  _U._NAME  = icTbName.
ELSE DO:
  MESSAGE "Error when retrieving toolbar placeholder" SKIP _U._NAME
          VIEW-AS ALERT-BOX ERROR.
  obOk = NO.
  RETURN.
END.

FIND FIRST _TRG
     WHERE _TRG._tSECTION = "_CUSTOM":U
       AND _TRG._pRECID   = RECID(_P)
       AND _TRG._tEVENT   = "_DEFINITIONS":U
     NO-ERROR.
IF AVAIL _TRG THEN DO: 
  IF NOT _TRG._tCODE MATCHES "*o" + icTbName + "*" THEN  
    _TRG._tCODE = _TRG._tCODE + CHR(10) + 
                  "DEF VAR o" + icTbName + " AS JBoxToolbar NO-UNDO.".             
END.

FIND FIRST _TRG
     WHERE _TRG._tSECTION = "_PROCEDURE":U
       AND _TRG._pRECID   = RECID(_P)
       AND _TRG._tEVENT   = "InitializeObject":U
     NO-ERROR.
IF NOT AVAIL _TRG THEN DO:
  cInitProc = selectProcForObjectInitCode().
  IF cInitProc NE "" THEN
    FIND FIRST _TRG
         WHERE _TRG._tSECTION = "_PROCEDURE":U
           AND _TRG._pRECID   = RECID(_P)
           AND _TRG._tEVENT   = cInitProc
         NO-ERROR.
END.

IF AVAIL _TRG AND NOT _TRG._tCODE MATCHES "*" + icTbName + "*" THEN DO:
  cInitObject = "  o" + icTbName + " = NEW JBoxToolbar(" + icTbName + ":HANDLE)." + CHR(10).
/*     IF icToolsList NE "" THEN                                                                               */
/*       cInitObject = cInitObject + "  oTb" + cTableRef + ":AddToolGroup('" + icToolsList + "')." + CHR(10).  */

  IF _TRG._tCODE MATCHES "*" + icAssignToQry + "*" AND NOT _TRG._tCODE MATCHES "*" + icAssignToQry + ":TOOLBAR-OBJECT*" THEN
    cInitObject = cInitObject + CHR(10) 
                + "  " + icAssignToQry + ":TOOLBAR-OBJECT = o" + icTbName + ".". 
  
  IF _TRG._tCODE MATCHES "*" + icAssignToFm + "*" AND NOT _TRG._tCODE MATCHES "*" + icAssignToFm + ":TOOLBAR-OBJECT*" THEN
    cInitObject = cInitObject + CHR(10) 
                + "  " + icAssignToFm + ":TOOLBAR-OBJECT = o" + icTbName + ".".     

  IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
    _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",cInitObject + CHR(10) + "END.").         
  ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
    ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
           cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
           cCode2 = REPLACE(cCode2,"END.",cInitObject + CHR(10) + "END.")
           _TRG._tCODE = cCode1 + cCode2
           .
  ELSE IF AVAIL _TRG THEN
    _TRG._tCODE = _TRG._tCODE + CHR(10) + cInitObject.                
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genViewerCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genViewerCode Procedure 
PROCEDURE genViewerCode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icViewName  AS CHAR NO-UNDO.
DEF INPUT PARAM icCode     AS CHAR NO-UNDO.

DEF BUFFER b_trg FOR _TRG.

DEF VAR bOk       AS LOG  NO-UNDO.
DEF VAR cCode1    AS CHAR NO-UNDO.
DEF VAR cCode2    AS CHAR NO-UNDO.

IF NOT AVAIL _P THEN RETURN.

FIND LAST _U WHERE _U._NAME BEGINS "RECT-"
     USE-INDEX _HANDLE
     NO-ERROR.
IF AVAIL _U THEN 
  _U._NAME  = icViewName. /* + "_AssignedByJbox". */

FIND FIRST b_TRG
     WHERE b_TRG._tSECTION = "_CUSTOM":U
       AND b_TRG._pRECID   = RECID(_P)
       AND b_TRG._tEVENT   = "_DEFINITIONS":U
     NO-ERROR.
IF AVAIL b_TRG THEN DO:

  IF NOT b_TRG._tCODE MATCHES "*o" + icViewName + "*" THEN  
    b_TRG._tCODE = b_TRG._tCODE + CHR(10) + 
                  "DEF VAR o" + icViewName + " AS JBoxViewer NO-UNDO.".             
END.

FIND FIRST _trg
     WHERE _trg._tSECTION = "_PROCEDURE":U
       AND _trg._pRECID   = RECID(_P)
       AND _trg._tEVENT   = "InitializeObject":U
     NO-ERROR.
IF NOT AVAIL _trg THEN DO:
  MESSAGE "Code belongs in procedure InitializeObject which does not exist"
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.  
END. 

IF icCode NE "" THEN DO:
  IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
    _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",icCode + CHR(10) + "END.").         
  ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
    ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
           cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
           cCode2 = REPLACE(cCode2,"END.",icCode + CHR(10) + "END.")
           _TRG._tCODE = cCode1 + cCode2
           .
  ELSE IF AVAIL _TRG THEN
    _TRG._tCODE = _TRG._tCODE + CHR(10) + icCode.                

END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBttField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getBttField Procedure 
PROCEDURE getBttField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       called from adeuib\_fldinfo.p
------------------------------------------------------------------------------*/
DEF INPUT PARAM p_db_name   AS CHAR NO-UNDO.
DEF INPUT PARAM p_tbl_name  AS CHAR NO-UNDO.
DEF INPUT PARAM p_fld_name  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ohtt_field AS HANDLE NO-UNDO.

DEFINE QUERY qDb FOR DICTDB._db   FIELDS(), 
                     DICTDB._file FIELDS(), 
                     DICTDB._field. 

p_fld_name = ENTRY(1,p_fld_name,"[").

IF p_db_name NE ? AND p_db_name NE "" THEN
  FIND FIRST tt_field 
       WHERE tt_field.cDbName = p_db_name
         AND tt_field.cTableName = p_tbl_name
         AND tt_field._field-name = p_fld_name
       NO-ERROR.
ELSE 
  FIND FIRST tt_field 
       WHERE tt_field.cTableName = p_tbl_name
         AND tt_field._field-name = p_fld_name
       NO-ERROR.

IF NOT AVAIL tt_field AND NUM-DBS > 1 THEN DO:    
  SESSION:SET-WAIT-STATE("general").
  RUN jukebox\fillTt_field.p("").
  SESSION:SET-WAIT-STATE("").

  IF p_db_name NE ? AND p_db_name NE "" THEN
    FIND FIRST tt_field 
         WHERE tt_field.cDbName = p_db_name
           AND tt_field.cTableName = p_tbl_name
           AND tt_field._field-name = p_fld_name
         NO-ERROR.
  ELSE 
    FIND FIRST tt_field 
         WHERE tt_field.cTableName = p_tbl_name
           AND tt_field._field-name = p_fld_name
         NO-ERROR.
END. /* Build cache */

IF NOT AVAILABLE tt_field THEN DO:

  OPEN QUERY qDB FOR 
      EACH DICTDB._db  
           WHERE DICTDB._db._db-name = (IF p_db_name = ldbname("DICTDB":U) OR p_db_name = "" THEN ?                                         
                                        ELSE p_db_name) 
           NO-LOCK,
      EACH DICTDB._file OF DICTDB._db 
           WHERE DICTDB._file._file-name = p_tbl_name 
           AND   LOOKUP(DICTDB._FILE._OWNER,"PUB,_FOREIGN":U) > 0 
           NO-LOCK,
      EACH DICTDB._field OF DICTDB._file 
           WHERE _field._field-name = p_fld_name          
           NO-LOCK.

  GET NEXT qDB.
  IF AVAIL DICTDB._field THEN DO:
    CREATE tt_field.
/*    BUFFER-COPY DICTDB._field TO tt_field. */
    ASSIGN tt_field._Data-type  = DICTDB._field._Data-Type
           tt_field._Field-Name = DICTDB._field._Field-Name
           tt_field._Initial    = DICTDB._field._Initial
           tt_field._Label      = DICTDB._field._Label
           tt_field._Help       = DICTDB._field._Help
           tt_field._Extent     = DICTDB._field._Extent
           tt_field._Desc       = DICTDB._Field._Desc
           tt_field._Format     = DICTDB._field._Format
           tt_field._Col-Label  = DICTDB._field._Col-label
           tt_field.cDbName     = p_db_name
           tt_field.cTableName  = p_tbl_name
           .
  END.    
END.
IF AVAIL tt_field THEN 
  ohtt_field = htt_field.
ELSE MESSAGE "Could not find field info for" SKIP
             "p_db_name" p_db_name SKIP
             "p_tbl_name" p_tbl_name SKIP
             "p_fld_name" p_fld_name SKIP(1)
             "Callstack: " SKIP 
             PROGRAM-NAME(2) SKIP
             PROGRAM-NAME(3)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-creFillInDefFromDb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION creFillInDefFromDb Procedure 
FUNCTION creFillInDefFromDb RETURNS CHARACTER
  ( INPUT icFieldList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ocFieldDef  AS CHAR NO-UNDO.

DEF VAR ix            AS INT    NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR cCurrDb       AS CHAR   NO-UNDO.
DEF VAR cNewField     AS CHAR   NO-UNDO.
DEF VAR h_fieldBuf    AS HANDLE NO-UNDO.
DEF VAR h_fileBuf     AS HANDLE NO-UNDO.
DEF VAR cDelim        AS CHAR   NO-UNDO.
DEF VAR cClipList     AS CHAR   NO-UNDO.
DEF VAR iDuplCnt      AS INT    NO-UNDO.
DEF VAR cCurrTable    AS CHAR   NO-UNDO.
DEF VAR iTabSeq       AS INT    NO-UNDO.
DEF VAR cPriIdxFlds   AS CHAR   NO-UNDO.
DEF VAR cPriKeyFlds   AS CHAR   NO-UNDO.
DEF VAR cUpdFlds      AS CHAR   NO-UNDO.
DEF VAR cDispFlds     AS CHAR   NO-UNDO.
DEF VAR cPrimaryTable AS CHAR   NO-UNDO.
DEF VAR cFieldMap     AS CHAR   NO-UNDO.
DEF VAR cFileName     AS CHAR   NO-UNDO.      
DEF VAR cCode1        AS CHAR   NO-UNDO.
DEF VAR cCode2        AS CHAR   NO-UNDO.
DEF VAR cInitProc     AS CHAR   NO-UNDO.

DEF BUFFER bttFieldDef FOR ttFieldDef.

FOR EACH ttFieldDef: DELETE ttFieldDef. END.

IF (cDbFieldUsage = "" OR cDbFieldUsage = "ABdefault") AND cClipBoardFormat = "" THEN RETURN "".

CASE cClipBoardFormat:
  WHEN "comma"     THEN cDelim = ",".
  WHEN "space"     THEN cDelim = " ".
  WHEN "semicolon" THEN cDelim = ";".
  WHEN "other"     THEN cDelim = cClipBoardDelimiter.
END CASE.

icFieldList = REPLACE(icFieldList,"Temp-Tables.","").
icFieldList = TRIM(icFieldList,".").

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
    BY ttFieldDef.iTableSeq:
  cPrimaryTable = ttFieldDef.cTable.
  cPriIdxFlds = getPrimaryKeyFields(ttFieldDef.cTable).
  LEAVE.
END.

FOR EACH ttFieldDef
    BREAK BY ttFieldDef.cDb
          BY ttFieldDef.iTableSeq
          BY ttFieldDef.iSeq:


  IF FIRST-OF(ttFieldDef.cDb) THEN DO:
    cCurrDb = ttFieldDef.cDb + (IF ttFieldDef.cDb NE "" THEN "." ELSE "").

/*     CREATE BUFFER h_fieldBuf FOR TABLE cCurrDb + "_field". */
/*     CREATE BUFFER h_fileBuf  FOR TABLE cCurrDb + "_file".  */
/*     CREATE QUERY hQuery.                                   */
/*     hQuery:SET-BUFFERS(h_fieldBuf,h_fileBuf).              */
  END.
  IF cClipBoardFormat = "jukebox" THEN DO:
    IF FIRST-OF(ttFieldDef.iTableSeq) THEN
      cClipList = cClipList + (IF cClipList NE "" THEN '  + ",' ELSE "") + ttFieldDef.cTable + '"' + CHR(10).
    cClipList = cClipList + '    + ";' + ttFieldDef.cField + '"' + CHR(10).
  END.

/*   hQuery:QUERY-PREPARE("FOR EACH _field WHERE _field-name = '"                                                                */
/*                      + (IF INDEX(ttFieldDef.cField,"[") > 0 THEN SUBSTR(ttFieldDef.cField,1,INDEX(ttFieldDef.cField,"[") - 1) */
/*                         ELSE ttFieldDef.cField) + "' NO-LOCK"                                                                 */
/*                      + ",FIRST _file OF _field WHERE _file-name = '" + ttFieldDef.cTable + "' NO-LOCK").                      */
/*   hQuery:QUERY-OPEN().                                                                                                        */
/*   hQuery:GET-FIRST().                                                                                                         */
/*                                                                                                                               */
/*   IF NOT h_fieldbuf:AVAILABLE THEN                                                                                            */
    RUN getBttField (cCurrDb,ttFieldDef.cTable,ttFieldDef.cField,OUTPUT h_fieldBuf).

  IF h_fieldBuf:AVAIL THEN DO:

    IF h_fieldBuf:BUFFER-FIELD("_extent"):BUFFER-VALUE > 0 THEN 
      ttFieldDef.cNewField = REPLACE(REPLACE(ttFieldDef.cNewField,"[","_"),"]","").

    IF ttFieldDef.iTableSeq = 1 THEN DO:
      cUpdFlds = cUpdFlds + (IF cUpdFlds NE "" THEN "," ELSE "") +  ttFieldDef.cNewField.
      IF CAN-DO(cPriIdxFlds,ttFieldDef.cNewField) THEN
        cPriKeyFlds = cPriKeyFlds + (IF cPriKeyFlds NE "" THEN "," ELSE "") + ttFieldDef.cNewField.
    END.
    ELSE 
      cDispFlds = cDispFlds + (IF cDispFlds NE "" THEN "," ELSE "") + ttFieldDef.cNewField.

    ttFieldDef.cDefVar = 'DEFINE VARIABLE ' + ttFieldDef.cNewField 
                       + ' AS ' + h_fieldBuf:BUFFER-FIELD("_data-type"):BUFFER-VALUE + CHR(10)
                       + ' LABEL "' + (IF h_fieldBuf:BUFFER-FIELD("_label"):BUFFER-VALUE NE ? THEN h_fieldBuf:BUFFER-FIELD("_label"):BUFFER-VALUE ELSE "") + '"'
                       + ' FORMAT "' + fixFieldFormat(h_fieldBuf) + '"'
/*                        + ' FORMAT "' + h_fieldBuf:BUFFER-FIELD("_format"):BUFFER-VALUE + '"' */
                       + creViewASPhrase(h_fieldBuf:BUFFER-FIELD("_data-type"):BUFFER-VALUE)
                       + (IF h_fieldBuf:BUFFER-FIELD("_initial"):BUFFER-VALUE NE "" AND h_fieldBuf:BUFFER-FIELD("_initial"):BUFFER-VALUE NE ? THEN
                            (' INITIAL ' + (IF h_fieldBuf:BUFFER-FIELD("_data-type"):BUFFER-VALUE = "CHARACTER" THEN '"' ELSE "")
                            + h_fieldBuf:BUFFER-FIELD("_initial"):BUFFER-VALUE
                            + (IF h_fieldBuf:BUFFER-FIELD("_data-type"):BUFFER-VALUE = "CHARACTER" THEN '"' ELSE ""))
                          ELSE "")
/*                          + ' LABEL "' + (IF h_fieldBuf:BUFFER-FIELD("_label"):BUFFER-VALUE NE ? THEN h_fieldBuf:BUFFER-FIELD("_label"):BUFFER-VALUE ELSE "") + '"' */
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

/*   IF LAST-OF(ttFieldDef.cDb) THEN DO: */
/*     DELETE OBJECT hQuery NO-ERROR. */
/*     DELETE OBJECT h_fieldBuf. */
/*     DELETE OBJECT h_fileBuf.  */
/*   END. */
END.
         
IF cFieldMapName NE "" THEN DO:
  ASSIGN cDefSection = "DEF VAR " + cFieldMapName + " AS JBoxFieldMap NO-UNDO.".
         cInitObject = "  " + cFieldMapName + REPLACE(" = NEW JBoxFieldMap(FRAME <&FRAME-NAME}:HANDLE).","<",CHR(123)) + CHR(10) 
                     + (IF cUpdFlds NE "" THEN
                         "  " + cFieldMapName + ":updateFields = '" + cUpdFlds + "'." + CHR(10)
                        ELSE "")
                     + (IF cDispFlds NE "" THEN
                         "  " + cFieldMapName + ":displayFields = '" + cDispFlds + "'." + CHR(10)
                        ELSE "")
                     + (IF cPriKeyFlds NE "" THEN
                         "  " + cFieldMapName + ":primaryKeyFields = '" + cPriKeyFlds + "'." + CHR(10)
                        ELSE "")
                     .
  FIND FIRST _TRG
       WHERE _TRG._tSECTION = "_CUSTOM":U
         AND _TRG._pRECID   = RECID(_P)
         AND _TRG._tEVENT   = "_DEFINITIONS":U
       NO-ERROR.
  IF AVAIL _TRG AND NOT _TRG._tCODE MATCHES "*" + cFieldMapName + "*" THEN 
    _TRG._tCODE = _TRG._tCODE + CHR(10)               
                + "DEF VAR " + cFieldMapName + " AS JBoxFieldMap NO-UNDO.".               
  cDefSection = ENTRY(3,cDefSection," "). /* Now this is the name of the fieldmap variable */
  /* Insert the code for InitializeObject */
  FIND FIRST _TRG
       WHERE _TRG._tSECTION = "_PROCEDURE":U
         AND _TRG._pRECID   = RECID(_P)
         AND _TRG._tEVENT   = "InitializeObject":U
       NO-ERROR.
  IF NOT AVAIL _TRG THEN DO:
    cInitProc = selectProcForObjectInitCode().
    IF cInitProc NE "" THEN
      FIND FIRST _TRG
           WHERE _TRG._tSECTION = "_PROCEDURE":U
             AND _TRG._pRECID   = RECID(_P)
             AND _TRG._tEVENT   = cInitProc
           NO-ERROR.
  END.

  IF AVAIL _TRG AND NOT _TRG._tCODE MATCHES "*" + cFieldMapName + "*" THEN DO:
    IF cQryForFieldMap NE "" THEN 
      cInitObject = cInitObject + CHR(10)
                  + "  " + cFieldMapName
                  + (IF cQryForFieldMap BEGINS "oBrw" THEN ":BROWSE-OBJECT" ELSE ":QUERY-OBJECT") 
                  + " = " + cQryForFieldMap + ".".

    IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
      _TRG._tCODE = REPLACE(_TRG._tCODE,"END.",cInitObject + CHR(10) + "END.").               
    ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
      ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
             cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
             cCode2 = REPLACE(cCode2,"END.",cInitObject + CHR(10) + "END.")
             _TRG._tCODE = cCode1 + cCode2
             .
    ELSE 
      _TRG._tCODE = _TRG._tCODE + CHR(10) + cInitObject.              
  END.
END.

IF cClipBoardFormat = "JBoxFieldMap" THEN 
  cFieldMap = "/* Code for Definitions: */" + CHR(10) + CHR(10)
            + "DEF VAR " + cFieldMapName + " AS JBoxFieldMap NO-UNDO." + CHR(10) + CHR(10) 
            + "/* Code for InitializeObject (first update-field gets focus on new/edit): */" + CHR(10)
            + "  " + cFieldMapName + " = NEW JBoxFieldMap(FRAME FRAME-NAME:HANDLE). /* <- curly brackets and & missing */" + CHR(10)
            + (IF cUpdFlds NE "" THEN
                "  " + cFieldMapName + ":updateFields = '" + cUpdFlds + "'." + CHR(10)
               ELSE "")
            + (IF cDispFlds NE "" THEN
                "  " + cFieldMapName + ":displayFields = '" + cDispFlds + "'." + CHR(10)
               ELSE "")
            + (IF cPriKeyFlds NE "" THEN
                "  " + cFieldMapName + ":primaryKeyFields = '" + cPriKeyFlds + "'." + CHR(10)
               ELSE "")
            + CHR(10) + CHR(10)
            + "/* Assuming a corresponding query exists (could be a BROWSE-OBJECT): */" + CHR(10)
            + "  " + cFieldMapName + ":QUERY-OBJECT = oQry" + cPrimaryTable + "." + CHR(10) + CHR(10)
            + "  Assuming you have placed a rectangle placeholder called tb" + cPrimaryTable + " on the frame and" + CHR(10)
            + "  want to create a toolbar with corresponding window menu items ('File' f.ex) and associate it with your FieldMap and Query: " + CHR(10) + CHR(10)
            + "  /* Code for Definitions: */" + CHR(10)
            + "DEF VAR oTb" + cPrimaryTable + " AS JBoxToolbar NO-UNDO." + CHR(10) + CHR(10) 
            + "/* Code for InitializeObject: */" + CHR(10)
            + "  oTb" + cPrimaryTable + " = NEW JBoxToolbar(tb" + cPrimaryTable + ":HANDLE,'File')." + CHR(10)
            + "  oTb" + cPrimaryTable + ":AddTool('New')." + CHR(10)
            + "  oTb" + cPrimaryTable + ":AddToolGroup('Filter,Excel')." + CHR(10) + CHR(10)
            + "  oQry" + cPrimaryTable + ":TOOLBAR-OBJECT = oTb" + cPrimaryTable + "." + CHR(10)
            + "  " + cFieldMapName + ":TOOLBAR-OBJECT = oTb" + cPrimaryTable + "." + CHR(10)
            .

ASSIGN cFieldMapName = ""
       cQryForFieldMap = "".

FOR EACH ttFieldDef 
    BY ttFieldDef.iSeq:
  ocFieldDef = ocFieldDef + ttFieldDef.cDefVar + CHR(10) + CHR(10).
  IF NOT CAN-DO("jukebox,JBoxFieldMap",cClipBoardFormat) AND cClipBoardFormat NE "" THEN
    cClipList = cClipList + (IF cClipList NE "" THEN cDelim ELSE "") + ttFieldDef.cNewField.
  cVariablesFromDbList = cVariablesFromDbList + (IF cVariablesFromDbList NE "" THEN "," ELSE "") + ttFieldDef.cNewField.
END.
IF cClipBoardFormat = "JBoxFieldMap" THEN cClipList = cFieldMap.

ocFieldDef = ocFieldDef + CHR(10).

IF cClipList NE "" THEN
  CLIPBOARD:VALUE = cClipList.

RETURN ocFieldDef.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-creFunction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION creFunction Procedure 
FUNCTION creFunction RETURNS CHARACTER
  ( INPUT icCode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFileName AS CHAR NO-UNDO.

cFileName = SESSION:TEMP-DIRECTORY + STRING(DAY(TODAY)) + STRING(TIME) + ".func".

OUTPUT STREAM JBoxStream TO VALUE(cFileName).
PUT STREAM JBoxStream UNFORMATTED "&ANALYZE-SUSPEND _EXPORT-NUMBER AB_v10r12" SKIP.
PUT STREAM JBoxStream UNFORMATTED "&ANALYZE-RESUME" SKIP.
PUT STREAM JBoxStream UNFORMATTED "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged  ^" SKIP.
PUT STREAM JBoxStream UNFORMATTED icCode.
IF NOT icCode MATCHES "*END FUNCTION." THEN
  PUT STREAM JBoxStream UNFORMATTED SKIP "END FUNCTION.".

PUT STREAM JBoxStream UNFORMATTED SKIP(1) "/* _UIB-CODE-BLOCK-END */" SKIP.
PUT STREAM JBoxStream UNFORMATTED "&ANALYZE-RESUME" SKIP.

OUTPUT STREAM JBoxStream CLOSE.

RUN adeuib/_qssuckr.p (cFileName, "", "IMPORT":U, NO).

RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-creJBoxTool) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION creJBoxTool Procedure 
FUNCTION creJBoxTool RETURNS LOGICAL
  ( INPUT ihToolBuff AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hQ AS HANDLE NO-UNDO.

EMPTY TEMP-TABLE ttJBoxTool.

CREATE QUERY hQ.
hQ:SET-BUFFERS(ihToolBuff).
hQ:QUERY-PREPARE("FOR EACH " + ihToolBuff:NAME).
hQ:QUERY-OPEN().
hQ:GET-FIRST().
REPEAT WHILE NOT hQ:QUERY-OFF-END:
  CREATE ttJBoxTool.
  httJBoxTool:BUFFER-COPY(ihToolBuff).
  hQ:GET-NEXT().
END.

DELETE OBJECT hQ.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-creTrigger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION creTrigger Procedure 
FUNCTION creTrigger RETURNS CHARACTER
  ( INPUT icCode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFileName AS CHAR NO-UNDO.

cFileName = SESSION:TEMP-DIRECTORY + STRING(DAY(TODAY)) + STRING(TIME) + ".trigg".

OUTPUT STREAM JBoxStream TO VALUE(cFileName).
PUT STREAM JBoxStream UNFORMATTED "&ANALYZE-SUSPEND _EXPORT-NUMBER AB_v10r12" SKIP.
PUT STREAM JBoxStream UNFORMATTED "&ANALYZE-RESUME" SKIP.
PUT STREAM JBoxStream UNFORMATTED "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MyTrigger  ^" SKIP.
PUT STREAM JBoxStream UNFORMATTED icCode.

PUT STREAM JBoxStream UNFORMATTED SKIP(1) "/* _UIB-CODE-BLOCK-END */" SKIP.
PUT STREAM JBoxStream UNFORMATTED "&ANALYZE-RESUME" SKIP.

OUTPUT STREAM JBoxStream CLOSE.

RUN adeuib/_qssuckr.p (cFileName, "", "IMPORT":U, NO).

RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-creViewAsPhrase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION creViewAsPhrase Procedure 
FUNCTION creViewAsPhrase RETURNS CHARACTER
  ( INPUT icDataType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cViewAS AS CHAR NO-UNDO.

DEF VAR cType         AS CHAR   NO-UNDO.
DEF VAR cSubType      AS CHAR   NO-UNDO.

IF cCreFieldViewAS NE "" THEN DO:
  cType           = ENTRY(1,cCreFieldViewAs," ").
  IF NUM-ENTRIES(cCreFieldViewAs," ") > 1 THEN
    cSubType        = ENTRY(2,cCreFieldViewAs," ").
  cCreFieldViewAS = "". 
END.
ELSE RETURN "".

CASE cType:
  WHEN "combo-box" THEN
    cViewAs = cViewAs + "    VIEW-AS COMBO-BOX INNER-LINES 25".
  WHEN "editor" THEN
    cViewAs = cViewAs + "     VIEW-AS EDITOR SCROLLBAR-VERTICAL SIZE 20 BY 3".
  WHEN "radio-set" THEN
    cViewAs = cViewAs + "     VIEW-AS RADIO-SET HORIZONTAL".
  WHEN "toggle-box" THEN
    cViewAs = cViewAs + "     VIEW-AS TOGGLE-BOX".
  WHEN "fill-in" THEN DO:
    IF cSubType = "TEXT" THEN
      cViewAs = cViewAs + "     VIEW-AS TEXT".          
    ELSE
      cViewAs = cViewAs + "     VIEW-AS FILL-IN".          
  END.
END CASE.

IF CAN-DO("combo-box,radio-set",cType) THEN DO:        
  CASE cSubType:
    WHEN "list-item-pairs" THEN
      cViewAs = cViewAs + '     LIST-ITEM-PAIRS "Item 1","1"'.
    WHEN "list-items" THEN
      cViewAs = cViewAs + '     LIST-ITEMS "Item 1"'.
    OTHERWISE  /* radio-set */
      cViewAs = cViewAs + '     RADIO-BUTTONS'.
  END CASE.

  CASE cSubType:
    WHEN "list-item-pairs" OR WHEN "LIST-ITEMS" THEN
      cViewAs = cViewAs + '     DROP-DOWN-LIST'.
    OTHERWISE DO: /* radio-set */
      CASE icDataType:
        WHEN "integer" OR WHEN "decimal" THEN
          cViewAs = cViewAs + '          "Item 1", 1,'.
        OTHERWISE
          cViewAs = cViewAs + '          "Item 1", "1",'.
      END CASE.
    END.
  END CASE.

  IF cType = "radio-set" THEN DO:
    CASE icDataType:
      WHEN "integer" OR WHEN "decimal" THEN
        cViewAs = cViewAs + '          "Item 1", 1,'.
      OTHERWISE
        cViewAs = cViewAs + '          "Item 2", "1"'.
    END CASE.
  END.

END.

cViewAS = cViewAS + " NO-UNDO".

RETURN cViewAS.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EmptyTts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EmptyTts Procedure 
FUNCTION EmptyTts RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FOR EACH ttObj
    WHERE ttObj.hWindow = _h_win:
  FOR EACH ttObjRel
      WHERE ttObjRel.cFromWidgetName = ttObj.cWidgetName
        AND ttObjRel.hFromWindow     = _h_win:
    DELETE ttObjRel.
  END.
  FOR EACH ttObjTable      
      WHERE ttObjTable.cWidgetName = ttObj.cWidgetName
        AND ttObjTable.hWindow = _h_win: 
    DELETE ttObjTable. 
  END.
  FOR EACH ttObjField      
      WHERE ttObjField.cWidgetName = ttObj.cWidgetName
        AND ttObjField.hWindow = _h_win
      : 
    DELETE ttObjField. 
  END.
  FOR EACH ttObjTtFieldDef 
      WHERE ttObjTtFieldDef.cWidgetName = ttObj.cWidgetName
        AND ttObjTtFieldDef.hWindow = _h_win
      : 
    DELETE ttObjTtFieldDef. 
  END.
  DELETE ttObj.
END.
EMPTY TEMP-TABLE ttObjViewDef.
EMPTY TEMP-TABLE ttObjCalcProcDef.
EMPTY TEMP-TABLE ttObjJoinDef.
EMPTY TEMP-TABLE ttObjTTDef.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fixFieldFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fixFieldFormat Procedure 
FUNCTION fixFieldFormat RETURNS CHARACTER
  ( INPUT ihFieldBuf AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFormat   AS CHAR NO-UNDO.
DEF VAR iFormat   AS INT  NO-UNDO.
DEF VAR cDataType AS CHAR NO-UNDO.

cDataType = ihFieldBuf::_DATA-TYPE.

CASE cDataType:
  WHEN "CHARACTER" THEN DO:
    IF ihFieldBuf::_FORMAT BEGINS "x(" THEN DO:
      cFormat = SUBSTR(ihFieldBuf::_FORMAT,3,LENGTH(ihFieldBuf::_FORMAT) - 3).
      iFormat = INT(cFormat) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR AND iFormat > 256 THEN 
        cFormat = "x(256)".
      ELSE cFormat = ihFieldBuf::_FORMAT.
    END.
    ELSE cFormat = ihFieldBuf::_FORMAT.
  END.
  OTHERWISE cFormat = ihFieldBuf::_FORMAT.
END CASE.


RETURN cFormat. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQueryJoin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixQueryJoin Procedure 
FUNCTION FixQueryJoin RETURNS CHARACTER
  ( INPUT icQueryString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNewString  AS CHAR NO-UNDO.
DEF VAR cPart       AS CHAR NO-UNDO.
DEF VAR cNewPart    AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR iy          AS INT  NO-UNDO.
DEF VAR ixWhere     AS INT  NO-UNDO.
DEF VAR iNumNoLock  AS INT  NO-UNDO.

ASSIGN icQueryString = REPLACE(icQueryString,",FIRST",CHR(3) + "FIRST")
       icQueryString = REPLACE(icQueryString,", FIRST",CHR(3) + "FIRST")
       icQueryString = REPLACE(icQueryString,",LAST",CHR(3) + "LAST")
       icQueryString = REPLACE(icQueryString,", LAST",CHR(3) + "LAST")
       icQueryString = REPLACE(icQueryString,",EACH",CHR(3) + "EACH")
       icQueryString = REPLACE(icQueryString,", EACH",CHR(3) + "EACH")
       .


DO ix = 1 TO NUM-ENTRIES(icQueryString,CHR(3)):
  ASSIGN ixWhere  = 0
         cPart    = ENTRY(ix,icQueryString,CHR(3))
         cNewPart = "".
  IF INDEX(cPart,"no-lock") > 0 THEN
    REPEAT WHILE INDEX(cPart,"no-lock") NE R-INDEX(cPart,"no-lock"):
      cPart = SUBSTR(cPart,1,INDEX(cPart,"no-lock") - 1) + SUBSTR(cPart,INDEX(cPart,"no-lock") + 8).
    END.
  IF INDEX(cPart,"outer-join") > 0 THEN
    REPEAT WHILE INDEX(cPart,"outer-join") NE R-INDEX(cPart,"outer-join"):
      cPart = SUBSTR(cPart,1,INDEX(cPart,"outer-join") - 1) + SUBSTR(cPart,INDEX(cPart,"outer-join") + 11).
    END.
/*   MESSAGE cPart SKIP NUM-ENTRIES(cPart," NO-LOCK") */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.           */
  /*
  DO iy = 1 TO NUM-ENTRIES(cPart," "):
    IF ENTRY(iy,cPart," ") = "where" THEN DO:
      ixWhere = ixWhere + 1.
      IF ixWhere < 2 THEN
        cNewPart = cNewPart + " " + ENTRY(iy,cPart," ") + " ".
      ELSE 
        cNewPart = cNewPart + " AND ".
    END.
    ELSE IF ENTRY(iy,cPart," ") NE "" THEN 
      cNewPart = cNewPart + ENTRY(iy,cPart," ") + " ".
    ELSE
      cNewPart = cNewPart + " ".
  END.
  */
  cNewString = cNewString + TRIM(cPart) + ",".
END.

RETURN RIGHT-TRIM(cNewString,","). 

/*
MESSAGE 1 icJoin
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
IF NUM-ENTRIES(icJoin,"NO-LOCK") > 1 THEN DO:
  DO ix = 1 TO NUM-ENTRIES(icJoin,"NO-LOCK"):
    cModJoin = cModJoin + (IF cModJoin NE "" THEN " " ELSE "") + ENTRY(ix,icJoin,"NO-LOCK").
  END.
  icJoin = cModJoin + " NO-LOCK".
END.

IF NUM-ENTRIES(icJoin,"INDEXED-REPOSITION") > 1 THEN DO:
  cModJoin = "".
  DO ix = 1 TO NUM-ENTRIES(icJoin,"INDEXED-REPOSITION"):
    cModJoin = cModJoin + (IF cModJoin NE "" THEN " " ELSE "") + ENTRY(ix,icJoin,"INDEXED-REPOSITION").
  END.
  icJoin = cModJoin + " INDEXED-REPOSITION".
END.

MESSAGE 2 icJoin
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
RETURN icJoin. 
*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBrowseTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBrowseTable Procedure 
FUNCTION getBrowseTable RETURNS CHARACTER
  (INPUT icBrowseName AS CHAR,
   INPUT ibModify     AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  Build _Q and _BC as input for query and field maintenance
            Called by adeuib\_prpobj.p            
    Notes:  _Q is always the same for freeform 
            When regular (visible columns) it should only contain the first buffer for SAVE (not ibModify)
            _BC is also always the same for freeform
            For "regular" browse (with visible columns) there are two modes 
            ibModify = YES: Prepare for modification, ie table references according to db
            ibModify = NO: Prepare for save, ie table references for temp-table 
------------------------------------------------------------------------------*/
DEF VAR cTableList    AS CHAR NO-UNDO.
DEF VAR extnt         AS INT  NO-UNDO.
DEF VAR intl          AS CHAR NO-UNDO.
DEF VAR valmsg        AS CHAR NO-UNDO.
DEF VAR valmsg-sa     AS CHAR NO-UNDO.
DEF VAR cCurrDb       AS CHAR NO-UNDO.
DEF VAR cTblQry       AS CHAR NO-UNDO.
DEF VAR cWhere        AS CHAR NO-UNDO.
DEF VAR cPrevTbl      AS CHAR NO-UNDO.
DEF VAR ix            AS INT  NO-UNDO.
DEF VAR cDbField      AS CHAR NO-UNDO.
DEF VAR cABquery      AS CHAR NO-UNDO.
DEF VAR cTtField      AS CHAR NO-UNDO.
DEF VAR cTblListJoin  AS CHAR NO-UNDO.
DEF VAR iOfWherePos   AS INT  NO-UNDO.

FIND _P WHERE _P._WINDOW-HANDLE = _h_win NO-ERROR.
FIND _U WHERE RECID(_U) = _query-u-rec NO-ERROR.
FIND _C WHERE RECID(_C) = _U._x-recid NO-ERROR.
FIND _Q WHERE RECID(_Q) = _C._q-recid NO-ERROR.

IF NOT AVAIL _Q THEN DO:
  MESSAGE "Query definition record (_Q) not available" SKIP 
          "Close and re-open file"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RETURN "".
END.

cCurrDb = IF _db_name NE "" THEN _db_name ELSE LDBNAME("DICTDB").

FIND FIRST ttObj
     WHERE ttObj.cWidgetName = icBrowseName
       AND ttObj.hWindow = _P._WINDOW-HANDLE
     NO-ERROR.
IF NOT AVAIL ttObj THEN DO:
  FIND FIRST ttObj
       WHERE ttObj.cWidgetName = icBrowseName
       NO-ERROR.
END.

IF AVAIL ttObj THEN DO:
  ASSIGN _Q._4GLQury = ttObj.cQueryCode
         _Q._TblList = ""
         .

  FOR EACH ttObjTable 
      WHERE ttObjTable.cWidgetName = ttObj.cWidgetName
        AND ttObjTable.iTableSeq > 0
         BY ttObjTable.iTableSeq
      :
    cTblQry = TRIM(ENTRY(ttObjTable.iTableSeq,ttObj.cQueryCode,CHR(10))).
    REPEAT WHILE INDEX(cTblQry,"  ") > 0:
      cTblQry = REPLACE(cTblQry,"  "," ").
    END.
    
    ASSIGN _Q._Where[ttObjTable.iTableSeq] = ""
           _Q._JoinCode[ttObjTable.iTableSeq] = ""
           .

    IF ttObjTable.iTableSeq > 1 AND (ttObj.bFreeform OR ibModify) THEN DO:
      IF ENTRY(3,cTblQry," ") = "NO-LOCK" THEN
        iOfWherePos = 4.
      ELSE 
        iOfWherePos = 3.

      IF ENTRY(iOfWherePos,cTblQry," ") = "WHERE" THEN DO:
        cTblListJoin = " WHERE ...".
        _Q._JoinCode[ttObjTable.iTableSeq] = "".
        DO ix = iOfWherePos + 1 TO NUM-ENTRIES(cTblQry," "):
          IF ENTRY(ix,cTblQry," ") = "WHERE" THEN LEAVE.
          ELSE _Q._JoinCode[ttObjTable.iTableSeq] = _Q._JoinCode[ttObjTable.iTableSeq] + ENTRY(ix,cTblQry," ") + " ".
        END.
      END.
      ELSE DO:
        cTblListJoin = " OF " + ENTRY(iOfWherePos + 1,cTblQry," ").
/*         _Q._JoinCode[ttObjTable.iTableSeq] = cTblQry. */
        IF INDEX(cTblQry," WHERE ") > 0 THEN
          ASSIGN _Q._JoinCode[ttObjTable.iTableSeq] = SUBSTR(cTblQry,1,INDEX(cTblQry," WHERE "))
                 _Q._Where[ttObjTable.iTableSeq] = SUBSTR(cTblQry,INDEX(cTblQry," WHERE ") + 7).
      END.

      _Q._JoinCode[ttObjTable.iTableSeq] = TRIM(_Q._JoinCode[ttObjTable.iTableSeq]).
  
      _Q._TblList = _Q._TblList + (IF _Q._TblList NE "" THEN "," ELSE "") + ttObjTable.cObjectDb + "." + ttObjTable.cObjectTable + cTblListJoin.
    END.
    ELSE IF ttObjTable.iTableSeq = 1 THEN _Q._TblList = ttObjTable.cObjectDb + "." + ttObjTable.cObjectTable. /* Only one table ref before writing file */

    cPrevTbl = ttObjTable.cObjectTable.
  END.
END.
ELSE RETURN "".

ix = 1.
IF ttObj.bFreeform THEN DO:
  FOR EACH _BC WHERE _BC._x-recid = _query-u-rec:
    DELETE _BC.
  END.

  FOR EACH ttObjField
      WHERE ttObjField.cWidgetName = icBrowseName
        AND ttObjField.hWindow = _P._WINDOW-HANDLE
     ,FIRST ttObjTTfieldDef
            WHERE ttObjTTfieldDef.cFieldName = ttObjField.cFieldName
              AND ttObjTTfieldDef.hWindow = _P._WINDOW-HANDLE
     ,FIRST ttObjTable
            WHERE ttObjTable.cObjectTable = ttObjField.cObjectTable
              AND ttObjTable.hWindow = _P._WINDOW-HANDLE
      BY ttObjField.iFieldSeq
      :

    IF AVAIL _U THEN DO:
      FIND FIRST _BC 
           WHERE _BC._x-recid = _query-u-rec
             AND _BC._TABLE   = ttObjField.cObjectTable
             AND _BC._NAME    = ttObjField.cFieldName
           NO-ERROR.
      IF NOT AVAIL _BC THEN DO:
        CREATE _BC.
        ASSIGN _BC._x-recid   = RECID(_U)
               _BC._DBNAME    = (IF ttObjField.cObjectTable = "_<CALC>" THEN "_<CALC>" ELSE cCurrDb)
               _BC._TABLE     = (IF ttObjField.cObjectTable = "_<CALC>" THEN ? ELSE ttObjField.cObjectTable)
               _BC._NAME      = ttObjField.cFieldName
               _BC._DISP-NAME = ttObjField.cFieldName
               _BC._SEQUENCE  = ix
               ix = ix + 1
               .
            
        IF ttObjField.cObjectTable NE "_<CALC>" THEN DO:
          IF R-INDEX(ttObjField.cFieldName,"[") > 0 THEN
            cDbField = SUBSTR(ttObjField.cFieldName,1,R-INDEX(ttObjField.cFieldName,"[") - 1).
          ELSE 
            cDbField = ttObjField.cFieldName.
          RUN adeuib/_fldinfo.p (INPUT _BC._DBNAME,
                                 INPUT _BC._TABLE,
                                 INPUT cDbField,
                                 OUTPUT _BC._DEF-LABEL,
                                 OUTPUT _BC._DEF-LABEL-ATTR,
                                 OUTPUT _BC._DEF-FORMAT,
                                 OUTPUT _BC._DEF-FORMAT-ATTR,
                                 OUTPUT _BC._DATA-TYPE,
                                 OUTPUT _BC._DEF-HELP,
                                 OUTPUT _BC._DEF-HELP-ATTR,
                                 OUTPUT extnt,
                                 OUTPUT intl,
                                 OUTPUT _BC._DEF-DESC,
                                 OUTPUT _BC._DEF-VALEXP,
                                 OUTPUT valmsg,
                                 OUTPUT valmsg-sa, 
                                 OUTPUT _BC._MANDATORY).

        END.
        ELSE DO:
          ASSIGN _BC._DBNAME     = "_<CALC>"
                 _BC._TABLE      = ?
                 _BC._DEF-VALEXP = ttObjTtfieldDef.cCalcFldProc + "|" + ttObjTTfieldDef.cProcName
                 _BC._DISP-NAME  = ttObjTtFieldDef.cCalcSrcTbl + "." + ttObjField.cFieldName
                 _BC._DATA-TYPE  = ttObjField.cDataType
                 _BC._NAME       = ""
                 .
        END.
        IF ttObjField.cLabel  NE "" THEN _BC._LABEL  = ttObjField.cLabel.  ELSE _BC._LABEL = _BC._DEF-LABEL.
        IF ttObjField.cFormat NE "" THEN _BC._FORMAT = ttObjField.cFormat. ELSE _BC._FORMAT = _BC._DEF-FORMAT.
        _BC._WIDTH  = ttObjField.fWidth.
      END.
    END.
    IF ttObjField.cObjectTable NE "_<CALC>" AND NOT CAN-DO(cTableList,cCurrDb + "." + ttObjField.cObjectTable) THEN
      cTableList = cTableList + (IF cTableList NE "" THEN "," ELSE "") + cCurrDb + "." + ttObjField.cObjectTable.
  END.
END.
ELSE DO:
  IF NOT ibModify THEN
    cFieldsInUse = "".

  FOR EACH _BC
      WHERE _BC._x-recid = RECID(_U)
      :
    cTtField = ENTRY(NUM-ENTRIES(_BC._DISP-NAME,"."),_BC._DISP-NAME,".").

    IF R-INDEX(cTtField,"[") > 0 THEN
      ASSIGN cDbField = cTtField
             cTtField = SUBSTR(REPLACE(cTtField,"[","_"),1,LENGTH(cTtField) - 1).
    ELSE cDbField = cTtField.

    FIND FIRST ttObjTTfieldDef
         WHERE ttObjTTfieldDef.hWindow     = _P._WINDOW-HANDLE
           AND ttObjTTfieldDef.cWidgetName = ttObj.cWidgetName
           AND ttObjTTfieldDef.cFieldName  = cTtField
        NO-ERROR.
    IF AVAIL ttObjTTfieldDef THEN DO:
      IF ttObjTTfieldDef.cProcName NE "" THEN DO:
        ASSIGN _BC._DBNAME     = "_<CALC>"
               _BC._TABLE      = ?
               _BC._DEF-VALEXP = ttObjTtfieldDef.cCalcFldProc + "|" + ttObjTTfieldDef.cProcName
               _BC._DISP-NAME  = (IF ibModify THEN ttObjTtFieldDef.cCalcSrcTbl ELSE ttObjTTfieldDef.cTableName) + "." + ttObjTTFieldDef.cFieldName
               _BC._DATA-TYPE  = ttObjTtFieldDef.cDataType
               _BC._NAME       = ""
               .
      END.
      ELSE DO:
        FIND FIRST ttObjTable 
             WHERE ttObjTable.cWidgetName  = ttObjTTfieldDef.cWidgetName
               AND ttObjTable.cObjectTable = ttObjTTfieldDef.cSourceTable
             NO-ERROR.

        ASSIGN _BC._DBNAME    = IF ibModify AND AVAIL ttObjTable THEN ttObjTable.cObjectDb ELSE cCurrDb
               _BC._TABLE     = (IF ibModify THEN ttObjTTfieldDef.cSourceTable ELSE ttObjTTfieldDef.cTableName)
               _BC._NAME      = (IF ibModify THEN ttObjTTfieldDef.cDbField ELSE cTtField)
               _BC._DATA-TYPE = ttObjTtFieldDef.cDataType
               _BC._FORMAT    = IF _BC._FORMAT NE "" THEN _BC._FORMAT ELSE IF ttObjTtFieldDef.cFormat NE "" THEN ttObjTtFieldDef.cFormat ELSE _BC._DEF-FORMAT
               _BC._LABEL     = IF _BC._LABEL NE "" THEN _BC._LABEL ELSE IF ttObjTtFieldDef.cLabel NE "" THEN ttObjTtFieldDef.cLabel ELSE _BC._DEF-LABEL
               _BC._DISP-NAME = (IF NUM-DBS > 1 AND ibModify AND AVAIL ttObjTable THEN ttObjTable.cObjectDb + "." ELSE "")
                              + (IF ibModify THEN ttObjTTfieldDef.cSourceTable ELSE ttObjTTfieldDef.cTableName) + "."
                              + (IF ibModify THEN ttObjTTfieldDef.cDbField ELSE cTtField) /* ttObjTTfieldDef.cFieldName */
               .
        IF _BC._DATA-TYPE = "datetime" AND NUM-ENTRIES(_BC._FORMAT) = 2 THEN
          _BC._FORMAT = REPLACE(_BC._FORMAT,","," ").
      
        IF NOT CAN-DO(cTableList,cCurrDb + "." + _BC._TABLE) THEN
          cTableList = cTableList + (IF cTableList NE "" THEN "," ELSE "") + cCurrDb + "." + _BC._TABLE.

        IF NOT ibModify THEN 
          cFieldsInUse = cFieldsInUse + (IF cFieldsInUse NE "" THEN CHR(10) ELSE "")  
                       + (IF NUM-DBS > 1 AND AVAIL ttObjTable THEN ttObjTable.cObjectDb + "." ELSE "") 
                       + ttObjTTfieldDef.cSourceTable + "." + ttObjTTfieldDef.cDbField
                       .
      END.
    END.
  END.
END.

/*
DEF VAR h_bc AS HANDLE NO-UNDO.
h_bc = BUFFER _BC:HANDLE.
RUN toexcelviafile.p (h_bc,-1).

MESSAGE "cTableList" SKIP cTableList SKIP(1)
        "_Q._TblList" SKIP _Q._TblList SKIP(1)
        "_Q._4GLQury" SKIP _Q._4GLQury SKIP(1)
        "_Q._OptionList" SKIP _Q._OptionList SKIP(1)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/    
RETURN _Q._TblList. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldCorrVarName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDbFieldCorrVarName Procedure 
FUNCTION getDbFieldCorrVarName RETURNS CHARACTER
  ( INPUT icFieldName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
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

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDbFieldName Procedure 
FUNCTION getDbFieldName RETURNS CHARACTER
  ( INPUT icDbName    AS CHAR,
    INPUT icTableName AS CHAR,
    INPUT icFieldName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: FIND the correct field name for table 
    Notes: Duplicate field names has the table sequence num as suffix 
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

DEFINE QUERY qDb FOR DICTDB._db   FIELDS(), 
                     DICTDB._file FIELDS(), 
                     DICTDB._field. 

IF CAN-DO("1,2,3,4,5,6,7,8,9",SUBSTR(icFieldName,LENGTH(icFieldName))) THEN
  DO ix = 1 TO 2:
    OPEN QUERY qDB FOR 
      EACH DICTDB._db  
           WHERE DICTDB._db._db-name = (IF icDbName = ldbname("DICTDB":U) OR icDbName = "" THEN ?                                         
                                        ELSE icDbName) 
           NO-LOCK,
      EACH DICTDB._file OF DICTDB._db 
           WHERE DICTDB._file._file-name                      = icTableName 
           AND   LOOKUP(DICTDB._FILE._OWNER,"PUB,_FOREIGN":U) > 0 
           NO-LOCK,
      EACH DICTDB._field OF DICTDB._file 
           WHERE _field._field-name = icFieldName          
           NO-LOCK.
  
    GET NEXT qDB.
    IF AVAIL DICTDB._field THEN LEAVE.
    ELSE IF ix = 1 THEN icFieldName = SUBSTR(icFieldName,1,LENGTH(icFieldName) - 1).
  END.

RETURN icFieldName. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldToVarHelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDbFieldToVarHelp Procedure 
FUNCTION getDbFieldToVarHelp RETURNS CHARACTER
  (INPUT icFieldName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
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

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldToVarTooltip) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDbFieldToVarTooltip Procedure 
FUNCTION getDbFieldToVarTooltip RETURNS CHARACTER
  ( INPUT icFieldName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
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

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDbFieldUsage Procedure 
FUNCTION getDbFieldUsage RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cDbFieldUsage.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDbFieldUsageOptions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDbFieldUsageOptions Procedure 
FUNCTION getDbFieldUsageOptions RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cOptions.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDefSectionCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDefSectionCode Procedure 
FUNCTION getDefSectionCode RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cDefSection.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFieldsInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldsInUse Procedure 
FUNCTION getFieldsInUse RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF bNewJukeBoxObject THEN RETURN "".
ELSE
  RETURN cFieldsInUse. /* Really: fields in query for preselection in adecomm\_tbsel.p */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFirstTableList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFirstTableList Procedure 
FUNCTION getFirstTableList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cTableList AS CHAR NO-UNDO.
FOR EACH ttObjTable WHERE iTableSeq = 1:
  cTableList = cTableList + (IF cTableList NE "" THEN "," ELSE "") + ttObjTable.cObjectTable.
END.
RETURN cTableList.
/*
RETURN cFirstTableList.
*/
END FUNCTION.


/*

DEF TEMP-TABLE ttObjTable
    FIELD cWidgetName  AS CHAR
    FIELD hWindow      AS HANDLE
    FIELD cObjectTable AS CHAR
    FIELD iTableSeq    AS INT
    */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFmObjectsInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFmObjectsInUse Procedure 
FUNCTION getFmObjectsInUse RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cFmObjectsInUse. /*  */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getHandleForObjectTtFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHandleForObjectTtFields Procedure 
FUNCTION getHandleForObjectTtFields RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hbttObjTTfieldDef.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getInitObjectCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getInitObjectCode Procedure 
FUNCTION getInitObjectCode RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cInitObject.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsJukeBoxObject Procedure 
FUNCTION getIsJukeBoxObject RETURNS LOGICAL
  ( INPUT icWidgetName AS CHAR,
    INPUT ihWin        AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttObj      
     WHERE ttObj.cWidgetName = icWidgetName
       AND ttObj.hWindow = ihWin 
     NO-ERROR.
IF NOT AVAIL ttObj THEN DO:
  FIND FIRST ttObj      
       WHERE ttObj.cWidgetName = icWidgetName
       NO-ERROR.
  IF AVAIL ttObj THEN
    MESSAGE PROGRAM-NAME(2) " søkte bare med widgetname for å finne objekt" SKIP
            "ihWin" ihWin SKIP
            "ttObj.hWindow" ttObj.hWindow
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

RETURN AVAIL ttObj.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIsNotJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIsNotJukeBoxObject Procedure 
FUNCTION getIsNotJukeBoxObject RETURNS LOGICAL
  ( ) :
/*------------------------------------------------------------------------------
  Purpose: This setting makes it possible to override the getIsJukeBoxObject 
    Notes: Reset by procedure findTablesAndFieldsInUse 
------------------------------------------------------------------------------*/
RETURN  bNotJukeBoxObject.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getJBoxUserContrProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getJBoxUserContrProperties Procedure 
FUNCTION getJBoxUserContrProperties RETURNS CHARACTER
  ( INPUT ir_U          AS RECID,
    INPUT icUserControl AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER b_trg FOR _TRG.

DEF VAR cReturn      AS CHAR NO-UNDO.
DEF VAR ix           AS INT  NO-UNDO.

FIND _U WHERE RECID(_U) = ir_U NO-ERROR.
IF NOT AVAIL _P THEN RETURN "".
IF NOT AVAIL _U THEN RETURN "".

FIND FIRST _trg 
     WHERE _trg._tSECTION = "_PROCEDURE":U
       AND _trg._pRECID   = RECID(_P)
       AND _trg._tEVENT   = "InitializeComponents"
     NO-ERROR.

IF NOT AVAIL _trg THEN RETURN "".

DO ix = 1 TO NUM-ENTRIES(_trg._tCODE,CHR(10)):
  IF ENTRY(ix,_trg._tCODE,CHR(10)) MATCHES "*" + icUserControl + ":*" THEN 
    cReturn = cReturn + ENTRY(ix,_trg._tCODE,CHR(10)) + CHR(10).
END.

RETURN cReturn. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLastToolChoose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLastToolChoose Procedure 
FUNCTION getLastToolChoose RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cToolChoose AS CHAR NO-UNDO.
  ASSIGN cToolChoose     = cLastToolChoose
/*          cLastToolChoose = "" */
         .
  RETURN cToolChoose.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNewJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNewJukeBoxObject Procedure 
FUNCTION getNewJukeBoxObject RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN bNewJukeBoxObject.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectCalcProcs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectCalcProcs Procedure 
FUNCTION getObjectCalcProcs RETURNS CHARACTER
  ( INPUT icWidget AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cProcList AS CHAR NO-UNDO.

IF icWidget = "" THEN icWidget = cActiveWidget.


FOR EACH ttObjCalcProcDef 
    WHERE (IF icWidget NE "" THEN ttObjCalcProcDef.cWidgetName = icWidget ELSE TRUE):
  IF NUM-ENTRIES(ttObjCalcProcDef.cCode,"'") > 1 THEN DO:
    IF NOT CAN-DO(cProcList,ENTRY(2,ttObjCalcProcDef.cCode,"'")) THEN
      cProcList = cProcList + (IF cProcList NE "" THEN "," ELSE "") + ENTRY(2,ttObjCalcProcDef.cCode,"'").      
  END.
END.
RETURN cProcList.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectFieldList Procedure 
FUNCTION getObjectFieldList RETURNS CHARACTER
  ( INPUT icWidget AS CHARACTER,
    INPUT icDelim  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFieldList AS CHAR NO-UNDO.
IF icDelim = "" THEN icDelim = ",".

FOR EACH ttObjField /* WHERE ttObjField.cWidgetName = icWidget */:
  cFieldList = cFieldList + (IF cFieldList NE "" THEN icDelim ELSE "") + ttObjField.cFieldName
             + (IF icDelim = "|" THEN "|" + ttObjField.cFieldName ELSE "").
END.
RETURN cFieldList.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectPrimaryTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectPrimaryTable Procedure 
FUNCTION getObjectPrimaryTable RETURNS CHARACTER
  ( INPUT icWidgetName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST ttObj 
       WHERE ttObj.cWidgetName = icWidgetName
       NO-ERROR.
  IF AVAIL ttObj THEN
    RETURN ttObj.cFirstTable.
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectQueryDef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectQueryDef Procedure 
FUNCTION getObjectQueryDef RETURNS CHARACTER
  ( INPUT icWidgetName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST ttObj
       WHERE ttObj.cWidgetName = icWidgetName
       NO-ERROR.
  IF AVAIL ttObj THEN DO:
/*     MESSAGE "ttObj.cQueryCode" ttObj.cQueryCode */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.      */
    RETURN ttObj.cQueryCode.
  END.
  ELSE 
    RETURN "". 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectTTdef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectTTdef Procedure 
FUNCTION getObjectTTdef RETURNS CHARACTER
  ( INPUT icWidgetName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST ttObj 
       WHERE ttObj.cWidgetName = icWidgetName
       NO-ERROR.
  IF AVAIL ttObj THEN
    RETURN ttObj.cTtCode.
  ELSE 
    RETURN "". 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getObjectTtFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getObjectTtFieldList Procedure 
FUNCTION getObjectTtFieldList RETURNS CHARACTER
  ( INPUT icWidget AS CHAR,
    INPUT icDelim  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFieldList AS CHAR NO-UNDO.
IF icDelim = "" THEN icDelim = ",".

FOR EACH ttObjTTfieldDef 
    WHERE ttObjTTfieldDef.cWidgetName = icWidget:
  cFieldList = cFieldList + (IF cFieldList NE "" THEN icDelim ELSE "") + ttObjTTfieldDef.cFieldName
             + (IF icDelim = "|" THEN "|" + ttObjTTfieldDef.cFieldName ELSE "").
END.
RETURN cFieldList.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryKeyFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPrimaryKeyFields Procedure 
FUNCTION getPrimaryKeyFields RETURNS CHARACTER
  ( INPUT icTable AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hBuffer        AS HANDLE NO-UNDO.
DEF VAR cPKfields      AS CHAR NO-UNDO.
DEF VAR ix             AS INT NO-UNDO.
DEF VAR iy             AS INT NO-UNDO.

CREATE BUFFER hBuffer FOR TABLE icTable NO-ERROR.

IF VALID-HANDLE(hBuffer) THEN DO:
  DO ix = 1 TO 100:
    IF hBuffer:INDEX-INFORMATION(ix) NE ? THEN DO:
      IF ENTRY(2,hBuffer:INDEX-INFORMATION(ix)) = "1" AND ENTRY(3,hBuffer:INDEX-INFORMATION(ix)) = "1" THEN DO:
        DO iy = 5 TO NUM-ENTRIES(hBuffer:INDEX-INFORMATION(ix)) BY 2:
          cPKfields = cPKfields + ENTRY(iy,hBuffer:INDEX-INFORMATION(ix)) + ",".
        END.
        LEAVE.
      END.
    END.
    ELSE LEAVE.
  END.  
  IF cPKfields = "" THEN
    DO ix = 1 TO 100:
      IF hBuffer:INDEX-INFORMATION(ix) NE ? THEN DO:
        IF ENTRY(2,hBuffer:INDEX-INFORMATION(ix)) = "1" THEN DO:
          DO iy = 5 TO NUM-ENTRIES(hBuffer:INDEX-INFORMATION(ix)) BY 2:
            cPKfields = cPKfields + ENTRY(iy,hBuffer:INDEX-INFORMATION(ix)) + ",".
          END.
          LEAVE.
        END.
      END.
      ELSE LEAVE.
    END.  
  DELETE OBJECT hBuffer.
END.

RETURN TRIM(cPKfields,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQryObjectsInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQryObjectsInUse Procedure 
FUNCTION getQryObjectsInUse RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cQryObjectsInUse. /*  */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRelatedObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRelatedObject Procedure 
FUNCTION getRelatedObject RETURNS CHARACTER
  ( INPUT icSrcObject     AS CHAR,
    INPUT icSrcObjectType AS CHAR,
    INPUT icTargetObjType AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  Examine definitions to try to resolve relatioships with other objects
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER b_trg FOR _TRG.

DEF VAR cLine          AS CHAR NO-UNDO.
DEF VAR ix             AS INT  NO-UNDO.
DEF VAR ocReturn       AS CHAR NO-UNDO.
DEF VAR cObjLeftOfEQ   AS CHAR NO-UNDO.
DEF VAR cObjRightOfEQ  AS CHAR NO-UNDO.
DEF VAR cAssignType    AS CHAR NO-UNDO.
DEF VAR cLeftOfEQType  AS CHAR NO-UNDO.

IF NOT AVAIL _P THEN RETURN "".

FIND FIRST b_trg
     WHERE b_trg._tSECTION = "_PROCEDURE":U
       AND b_trg._pRECID   = RECID(_P)
       AND b_trg._tEVENT   = "InitializeObject":U
     NO-ERROR.
IF NOT AVAIL b_trg THEN RETURN "".


DO ix = 1 TO NUM-ENTRIES(b_trg._tCODE,CHR(10)):
  cLine = ENTRY(ix,b_trg._tCODE,CHR(10)).
  IF cLine MATCHES "*" + icSrcObject + "*" AND NUM-ENTRIES(cLine,"=") = 2 AND NUM-ENTRIES(cLine,":") = 2 THEN DO:
    ASSIGN cObjLeftOfEQ  = TRIM(ENTRY(1,cLine,":"))
           cObjRightOfEQ = TRIM(ENTRY(NUM-ENTRIES(cLine," "),cLine," "),".")
           cAssignType   = ENTRY(1,ENTRY(1,ENTRY(2,cLine,":")," "),"-")  /* Ie: "QUERY" extracted from ":QUERY-HANDLE" */
           .

    cLeftOfEQType = (IF cObjLeftOfEQ BEGINS "ob" THEN "browse"
                     ELSE IF cObjLeftOfEQ BEGINS "oq" THEN "query"
                     ELSE IF cObjLeftOfEQ BEGINS "ot" THEN "toolbar"
                     ELSE "fieldmap").

    /* To find the targtet object the source object is either 
         - on left side of = and the assign type equals target type
         - on rigth side of = and the assign equals source type
       or source object is : */
    IF cObjLeftOfEQ = icSrcObject OR cObjRightOfEQ = icSrcObject THEN DO:
      IF cObjLeftOfEQ = icSrcObject AND CAN-DO(icTargetObjType,cAssignType) THEN
        ocReturn = cObjRightOfEQ.
      ELSE IF cObjRightOfEQ = icSrcObject AND cAssignType = icSrcObjectType AND CAN-DO(icTargetObjType,cLeftOfEQType) THEN 
        ocReturn = cObjLeftOfEQ.
    END.
  END.
END.

RETURN ocReturn.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTableDbName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTableDbName Procedure 
FUNCTION getTableDbName RETURNS CHARACTER
  ( INPUT icTableName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: FIND the correct field name for table 
    Notes: Duplicate field names has the table sequence num as suffix 
------------------------------------------------------------------------------*/
DEF VAR cDbName AS CHAR NO-UNDO.

IF NUM-ENTRIES(icTableName,".") > 1 THEN
  cDbName = ENTRY(1,icTableName,".").
ELSE
  RUN JukeBox\dbNameForTable.p (icTableName,OUTPUT cDbName).

RETURN cDbName.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTablesInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTablesInUse Procedure 
FUNCTION getTablesInUse RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cTablesInUse. /* Really: tables in query for preselection in adecomm\_tbsel.p */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTbObjectsInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTbObjectsInUse Procedure 
FUNCTION getTbObjectsInUse RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cTbObjectsInUse. /*  */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUseDbPrefix) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUseDbPrefix Procedure 
FUNCTION getUseDbPrefix RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cDbFieldUsage = "abdefault".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getVariablesFromDbList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVariablesFromDbList Procedure 
FUNCTION getVariablesFromDbList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN cVariablesFromDbList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION initJukeBoxObject Procedure 
FUNCTION initJukeBoxObject RETURNS LOGICAL
  ( INPUT icWidgetName AS CHAR,
    INPUT ihWindow     as handle,
    INPUT icType       AS CHAR,
    INPUT ibFreeForm   AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
CREATE ttObj.
ASSIGN ttObj.cObjectName  = "o" + icWidgetName
       ttObj.cWidgetName  = icWidgetName
       ttObj.hWindow      = ihWindow
       ttObj.cObjectType  = icType
       ttObj.bFreeform    = ibFreeForm
       ttObj.cObjectState = "new"
       .

CASE icType:
  WHEN "browse" THEN
    ASSIGN ttObj.cTtCode     = "/*** Start instance property definitions for JBoxBrowse object " + ttObj.cObjectName + " ***/"
                                + CHR(10) + "DEF VAR " + ttObj.cObjectName + " AS JBoxBrowse NO-UNDO." + CHR(10)
           ttObj.cFirstTable = SUBSTR(icWidgetName,4)
           .
END CASE.

RELEASE ttObj.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-selectProcForObjectInitCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION selectProcForObjectInitCode Procedure 
FUNCTION selectProcForObjectInitCode RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cProcList AS CHAR NO-UNDO.

DEF BUFFER b_TRG FOR _TRG.

FOR EACH b_TRG 
    WHERE b_TRG._tSECTION = "_PROCEDURE":U
      AND b_TRG._pRECID   = RECID(_P)
      AND NOT CAN-DO("enable_UI,disable_UI",b_TRG._tEVENT) 
    :
  cProcList = cProcList + (IF cProcList NE "" THEN "|" ELSE "") + b_TRG._tEvent.
END.
IF cProcList NE "" THEN DO:
  MESSAGE "Procedure InitializeObject is missing from the container" SKIP 
          "Select procedure to insert code for initialization of object"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  JBoxSession:Instance:SimpleSelectList(cProcList).
  IF JBoxSession:Instance:SelectListOk THEN
    RETURN JBoxSession:Instance:SelectListValue.
END.


RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setActiveWidget) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setActiveWidget Procedure 
FUNCTION setActiveWidget RETURNS LOGICAL
  ( INPUT icActiveWidget AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cActiveWidget = icActiveWidget.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCancelQueryRebuild) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCancelQueryRebuild Procedure 
FUNCTION setCancelQueryRebuild RETURNS LOGICAL
  ( INPUT ibCancelQueryRebuild AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  bCancelQueryRebuild = ibCancelQueryRebuild.
  RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCreFieldViewAs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCreFieldViewAs Procedure 
FUNCTION setCreFieldViewAs RETURNS LOGICAL
  ( INPUT icViewAs AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cCreFieldViewAS = icViewAs.

IF cCreFieldViewAs = ? THEN cCreFieldViewAs = "".

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDbFieldUsage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDbFieldUsage Procedure 
FUNCTION setDbFieldUsage RETURNS LOGICAL
  ( INPUT icDbFieldUsage AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NUM-ENTRIES(icDbFieldUsage,"|") > 1 THEN
  ASSIGN cOptions      = ENTRY(2,icDbFieldUsage,"|")
         cDbFieldUsage = ENTRY(1,icDbFieldUsage,"|").
ELSE
  cDbFieldUsage = icDbFieldUsage.
    
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldMapNameAndQry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldMapNameAndQry Procedure 
FUNCTION setFieldMapNameAndQry RETURNS LOGICAL
  ( INPUT icFieldMapName   AS CHAR,
    INPUT icQryForFieldMap AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN cFieldMapName   = icFieldMapName
       cQryForFieldMap = icQryForFieldMap
       .
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldSelectClipBoardDelimiter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldSelectClipBoardDelimiter Procedure 
FUNCTION setFieldSelectClipBoardDelimiter RETURNS LOGICAL
  ( INPUT icClipBoardDelimiter AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  cClipBoardDelimiter = icClipBoardDelimiter.
  RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldSelectClipBoardFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldSelectClipBoardFormat Procedure 
FUNCTION setFieldSelectClipBoardFormat RETURNS LOGICAL
  ( INPUT icClipBoardFormat AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  cClipBoardFormat = icClipBoardFormat.
  RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldSelectFieldHelp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldSelectFieldHelp Procedure 
FUNCTION setFieldSelectFieldHelp RETURNS LOGICAL
  ( INPUT icFieldHelp AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  cFieldHelp = icFieldHelp.
  RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldSelectFieldPrefix) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldSelectFieldPrefix Procedure 
FUNCTION setFieldSelectFieldPrefix RETURNS LOGICAL
  ( INPUT icFieldPrefix AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  cFieldPrefix = icFieldPrefix.
  RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldSelectRemoveDuplicates) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldSelectRemoveDuplicates Procedure 
FUNCTION setFieldSelectRemoveDuplicates RETURNS LOGICAL
  ( INPUT ibRemoveDuplicates AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  bRemoveDuplicates = ibRemoveDuplicates.
  RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldsInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldsInUse Procedure 
FUNCTION setFieldsInUse RETURNS LOGICAL
  ( INPUT icFieldsInUse AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  cFieldsInUse = icFieldsInUse. /* Really: fields in query, set by adeuib\_drwaobj.p */
  RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFirstColumnEnabled) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFirstColumnEnabled Procedure 
FUNCTION setFirstColumnEnabled RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Set the first browse column to enabled so that sorting can be done
            Called from adeuib/_get4gl.p
    Notes:  
------------------------------------------------------------------------------*/
FOR EACH _U WHERE _U._WINDOW-HANDLE = _h_win 
    AND _U._TYPE = "browse"
    AND CAN-FIND(FIRST ttObj
                 WHERE ttObj.hWindow = _U._WINDOW-HANDLE AND ttObj.cWidgetName = _U._NAME)
    :
  FOR EACH _BC
      WHERE _BC._x-recid = RECID(_U)
      :
    ASSIGN _BC._ENABLED = YES
           _BC._HELP = REPLACE(_BC._HELP,'"',"'").
    LEAVE.
  END.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setIsNotJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setIsNotJukeBoxObject Procedure 
FUNCTION setIsNotJukeBoxObject RETURNS LOGICAL
  ( INPUT ibNotJukeBoxObject AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bNotJukeBoxObject = ibNotJukeBoxObject.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setJBoxUserContrProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setJBoxUserContrProperties Procedure 
FUNCTION setJBoxUserContrProperties RETURNS LOGICAL
  ( INPUT ir_U          AS RECID,
    INPUT icUserControl AS CHAR,
    INPUT icControlType AS CHAR,
    INPUT icCode        AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF BUFFER b_trg FOR _TRG.

DEF VAR cControlName AS CHAR NO-UNDO.
DEF VAR cCode1       AS CHAR NO-UNDO.
DEF VAR cCode2       AS CHAR NO-UNDO.
DEF VAR ix           AS INT  NO-UNDO.
DEF VAR cContrProp   AS CHAR NO-UNDO.
DEF VAR cCodeLine    AS CHAR NO-UNDO.

IF icCode = "" THEN RETURN NO.

FIND _U WHERE RECID(_U) = ir_U NO-ERROR.
IF NOT AVAIL _P THEN RETURN NO.
IF NOT AVAIL _U THEN RETURN NO.

FIND FIRST b_trg
     WHERE b_trg._tSECTION = "_PROCEDURE":U
       AND b_trg._pRECID   = RECID(_P)
       AND b_trg._tEVENT   = "InitializeObject"
     NO-ERROR.
IF NOT AVAIL b_trg THEN RETURN NO.

IF NOT b_trg._tCODE MATCHES "*InitializeComponents*" THEN DO:
  IF b_trg._tCODE MATCHES "*END.*" AND INDEX(b_trg._tCODE,"END.") = R-INDEX(b_trg._tCODE,"END.") THEN 
    b_trg._tCODE = REPLACE(b_trg._tCODE,"END.","  RUN InitializeComponents." + CHR(10) + "END.").         
  ELSE IF b_trg._tCODE MATCHES "*END.*" THEN 
    ASSIGN cCode1 = SUBSTR(b_trg._tCODE,1,R-INDEX(b_trg._tCODE,"END.") - 1)  
           cCode2 = SUBSTR(b_trg._tCODE,R-INDEX(b_trg._tCODE,"END."))
           cCode2 = REPLACE(cCode2,"END.","  RUN InitializeComponents." + CHR(10) + "END.")
           b_trg._tCODE = cCode1 + cCode2
           .
  ELSE
    b_trg._tCODE = b_trg._tCODE + CHR(10) + "RUN InitializeComponents.".                
END.

FIND FIRST _trg 
     WHERE _trg._tSECTION = "_PROCEDURE":U
       AND _trg._pRECID   = RECID(_P)
       AND _trg._tEVENT   = "InitializeComponents"
     NO-ERROR.

IF NOT AVAIL _trg THEN DO:
  CREATE _trg.
  BUFFER-COPY b_trg TO _trg
  ASSIGN _trg._tEVENT = "InitializeComponents"
         _trg._tCODE  = REPLACE("DO WITH FRAME <&FRAME-NAME}:","<",CHR(123))
                      + CHR(10) + CHR(10)
                      + "END."
                      + CHR(10)
                      + CHR(10) + "END PROCEDURE."
                      .
END.

/* MESSAGE "icUserControl" icUserControl SKIP(1)         */
/*         "icControlType" icControlType SKIP(1)         */
/*         "icCode" icCode SKIP(1)                       */
/*         _trg._tCODE MATCHES "*" + icUserControl + "*" */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                */

DO ix = 1 TO NUM-ENTRIES(icCode,CHR(10)):
  cCodeLine  = TRIM(ENTRY(ix,icCode,CHR(10))).
  cContrProp = ENTRY(1,cCodeLine," ").

  IF NOT _trg._tCODE MATCHES "*" + cContrProp + "*" THEN DO:
    IF _TRG._tCODE MATCHES "*END.*" AND INDEX(_TRG._tCODE,"END.") = R-INDEX(_TRG._tCODE,"END.") THEN 
      _trg._tCODE = REPLACE(_trg._tCODE,"END.",icCode + CHR(10) + "END.").         
    ELSE IF _TRG._tCODE MATCHES "*END.*" THEN 
      ASSIGN cCode1 = SUBSTR(_TRG._tCODE,1,R-INDEX(_TRG._tCODE,"END.") - 1)  
             cCode2 = SUBSTR(_TRG._tCODE,R-INDEX(_TRG._tCODE,"END."))
             cCode2 = REPLACE(cCode2,"END.",cCodeLine + CHR(10) + "END.")
             _TRG._tCODE = cCode1 + cCode2
             .
    ELSE 
      _trg._tCODE = _trg._tCODE + CHR(10) + "  " + cCodeLine.                
  END.
  ELSE DO:
      
  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setNewJukeBoxObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNewJukeBoxObject Procedure 
FUNCTION setNewJukeBoxObject RETURNS LOGICAL
  ( INPUT ibNewJukeBoxObject AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN bNewJukeBoxObject = ibNewJukeBoxObject
       cActiveWidget = "".
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSelectedBrwOrQry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSelectedBrwOrQry Procedure 
FUNCTION setSelectedBrwOrQry RETURNS LOGICAL
  ( INPUT icSelectedBrwOrQry AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cSelectedBrwOrQry = icSelectedBrwOrQry.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTablesInUse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTablesInUse Procedure 
FUNCTION setTablesInUse RETURNS LOGICAL
  ( INPUT icTablesInUse AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  cTablesInUse = icTablesInUse. /* Really: tables in query, set by adeuib\_drwaobj.p */
  RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setToolChoose) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setToolChoose Procedure 
FUNCTION setToolChoose RETURNS LOGICAL
  ( INPUT icLastToolChoose AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  cLastToolChoose = icLastToolChoose.
  RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateTtObj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION updateTtObj Procedure 
FUNCTION updateTtObj RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  code extracted from findTablesAndFieldsInUse to save space
    Notes:  
------------------------------------------------------------------------------*/
  FOR EACH ttObj:
    FOR EACH ttObjTTfieldDef
        WHERE ttObjTTfieldDef.cTableName = ttObj.cFirstTable
        :
      ttObjTTfieldDef.cWidgetName = ttObj.cWidgetName.
    END.
    FOR EACH ttObjTTDef
        WHERE ttObjTTDef.cTableName = ttObj.cFirstTable
        :
      ttObj.cTtCode = ttObjTTdef.cCode.
    END.
    FOR EACH ttObjViewDef
        WHERE ttObjViewDef.cWidgetName = ttObj.cWidgetName
        :
      ttObj.cViewCode = ttObjViewDef.cCode.
    END.
    FOR EACH ttObjJoinDef
        WHERE ttObjJoinDef.cWidgetName = ttObj.cWidgetName
        :
      ASSIGN ttObj.cViewCode = ttObj.cViewCode + CHR(10) + ttObjJoinDef.cCode
             ttObj.cJoinCode = ttObj.cJoinCode + CHR(10) + ttObjJoinDef.cCode.
    END.
    FOR EACH ttObjCalcProcDef
        WHERE ttObjCalcProcDef.cWidgetName = ttObj.cWidgetName
        :
      ttObj.cViewCode = ttObj.cViewCode + CHR(10) + ttObjCalcProcDef.cCode.
    END.
  END.

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

