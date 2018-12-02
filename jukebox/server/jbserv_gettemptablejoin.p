&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : jbserv_gettemptablejoin.p
    Purpose     : Run a server-side query and return a batch of records from it.

    Parameters  :
    - iiBatchSize:        Number of rows to batch
    - iiStartRow:         After how many iterations should the query start populating return temp-table
    - icDirection:        "": foreward, "desc": backwards (the user scrolls backwards)
    - icBuffersAndFields: List of buffers and corresponding bufferfields and calculated fields.
                          No field spec retrieves all:
                          "<buffer1>;<field1>;<field2>..,<buffer2>;..
                          Calculated fields:
                          <fieldname|type|format|procedure.p|label>
    - icQueryCriteria:    WHERE <crit for buffer1>,EACH ..
    - icStatFields:       Comma-separated list of fields that should be accumulated.
                          If list equals the constant "rowcount" the query will be counted (see under, for output param). 
                          NB! If any statfields are specified the query will run to it's end, 
                          regardless of batch size. However only the specified number of records (batch size)
                          will be returned to the client.  
    - icParam:            At this time only used for filename, log-file, (normally specified in CustDevMode.i)                                                               
    
    - ohTempTable:        Output temp-table handle
    - ocStatFieldsAndValues: Separated list of fields and values. First field is always named "rowcount",
                          regardless of "rowcount" is specified in icStatFields (as long as icStatFields <> ""): 
                          "rowcount"|<rowcount-value>;<statfield1>|<statfield1-value;..
    
    Usage:
    - Get data and data definitions for a browse or query 
      (the call for defining a browse or query is different, see NewBrowse and NewQuery functions in JBoxUIlib.p)
    - Define a temp-table to use with f.ex a browse: 
      Specify only calculated fields and a buffer that will always be there, f.ex _file
    - To get rowcount for a browse, set the attribute "getrecordcount":
      DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes").
      The returned value will be assigned the HELP attribute for the browse (remember to set status-area for the window)
      NOTE: If a calculated field value may return "skiprow", i.e it decides wether the row should be counted or not
            AND the only stat.field is "rowcount" you must switch on bCheckForSkipRow to make sure that the returned count is correct.
    - Calculated fields:
      The simple alterative is to write one .p procedure to perform the calculation. 
      Sometimes for performance reasons assemble calculations in one .p and assign the name of the procedure as an attribute
      to the browse or query:
        DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","mycalc.p[;myothercalc.p;mythird..]"). 
        If the query object is also retrieving data when defined (in NewBrowse or NewQuery) the
        attribute must be assigned as part of the last parameter:
        "<sort|sortfield,calcfieldproc|mycalc.p[;myothercalc.p;mytird..],..."
        You can combine persistent routine(s) with separate .p's. If the "calcfieldproc" approach is used
        the procedure(s) loades persistent and the run performs a lot quicker.
      A procedure (internal or external) can be called 3 different ways:
      1. Simple conversion:
        CharTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p(IntTime)|Time
        IntTime is the database field (ref jbserv_int_to_hhmm_time.p)
      2. ROWID:
        CharTime|CHARACTER|x(5)|my.p|Time
        ROWID for current buffer is passed automatically
      3. ROWID + value:
        CharTime|CHARACTER|x(5)|my.p(ROWID<value>|Time
        To enable a parameter passed from the client to govern the business logic.   
        Example: To only return customers with more than 10 shipped orders:
                 DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldparamOrderCount","10¤shipped").
                 (Note the delimiter. Comma, semicolon and pipe are reserved for JukeBox)
      Additional parameters for all 3 types are:
      - INPUT icSessionId AS CHAR
      - OUTPUT ocReturn   AS CHAR      
      The calculation routine can be used to skip a row from the result set by returning "skiprow" as return value.
      You can also supply an initial value for a calculated field without calling an external procedure:       
        MonthsInYear|INTEGER|>9|12|Months
      The procedures for calculated fields are executed in the sequence they are defined. This gives an opportunity to optimize
      data retrieval by letting the first field do calculations for the others. 
      Example: A customer browse should show both order count and total order value. This can be solved by one loop through the orders
      for the ordercount field (1st field) that calculates both values and stores the order value in a variable that is picked up 
      in the procedure for order value. (This requires the use of a "calcfieldproc").
            
      To retrieve distinct rows define the column like this:      
        ..;DISTINCT <field name>;..
      or like this for a calculate field:
        +DISTINCT RegionSalesRep|CHARACTER|x(10)|distinct_region_salesrep.p
            
      Be very careful to examine data volumns before using sort on calculated fields
      or retrieving distinct rows. Both these functions require a full scan of the query before 
      starting to retrieve any data.                                                                         
    
    - To enable filtering on joined tables:
      Add the prescan queries to the additional parameter string, one per joined buffer:
      ...,prescanquery;FOR EACH Item WHERE ItemName BEGINS 'croc'¤EACH OrderLine..
   
    - For dynamic filtering on calculated fields:
      Add the fieldname, operator and value to the additional param string, one pr field/operator/value:
      ...,LineTotal¤>¤1000|LineTotal¤<¤2000|LinePrice¤>¤100...
    
    - Calculated field may be skipped if they are not in view (see the currViewFields attribute) and
      they are listed in the noMandatoryCalcFields attribute (and they are not part of the filter or sort string)  
           
    Author(s)   : Brynjar Hasle, brynjar@chemistry.no
    Created     : Jan 2004
    Notes       : 
    bug-fixes   : 20.oct.08 Changed this test in ReduceQueries:
                 AND ttQuery.cQryBuffer    = bttQuery.cQryBuffer
              /* Old: AND ttQuery.iQryBufferNum = bttQuery.iQryBufferNum */
                  20.oct.08 Added this test to DecomposeQuery:
                 IF ttQuery.cQryString BEGINS "BY " THEN 
                    ASSIGN ttQuery.cQrySort   = ttQuery.cQryString
                           ttQuery.cQryString = "".

    Modifications:
                 26.aug.11 Optimized sorting on calculated/accumulated fields:
                  When traversing the query result set to enable sorting or accumulation only the 
                  calculated fields that are needed are processed. Other calculations are done when 
                  producing the result set

    The algorithm can now be described as follows:
       - Create the result set table definition based on input list of buffers and fields
       - If any prescan (i.e sub) queries (try to) execute them to limit the number of hits in the primary table
       - If sorting on calculated values, grouping or accumulation process the whole result set and do the neccessary calculations.
         Neccessary being the fields involved in the sorting, grouping, accumulation or calculation of totals.
         When grouping all calculated fields are processed since a calculated fields may inflict on the total
         and total number of records by returning "skiprow" as the output parameter. 
         If there are only grand totals they are re-calculated when producing the result set.
                           
                11.oct.11 Further optimizing for sorting on ONE calculated value:
                 Instead of looping through the special fields for each query row the right temp-table record 
                 is in scope before traversing records
                 
        16.03.12: Added code to prevent adding to accumulation more than once pr field pr rowid
        06.08.18: If the buffer sequence has been changed (i.e orgBuffersAndFields has value)
                  duplicates are handled so that values appear in the right columns.
                  
** */
DEF INPUT  PARAM icSessionId              AS CHAR NO-UNDO. 
DEF INPUT  PARAM iiBatchSize              AS INT  NO-UNDO.
DEF INPUT  PARAM iiStartRow               AS INT  NO-UNDO.
DEF INPUT  PARAM icDirection              AS CHAR NO-UNDO.
DEF INPUT  PARAM icBuffersAndFields       AS CHAR NO-UNDO.
DEF INPUT  PARAM icQueryCriteria          AS CHAR NO-UNDO.
DEF INPUT  PARAM icStatFields             AS CHAR NO-UNDO.
DEF INPUT  PARAM icParam                  AS CHAR NO-UNDO. /* f.ex logfile-name */
DEF OUTPUT PARAM TABLE-HANDLE ohTempTable.
DEF OUTPUT PARAM ocStatFieldsAndValues    AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn                 AS CHAR NO-UNDO.

DEF VAR hBuffer               AS HANDLE NO-UNDO EXTENT 100.
DEF VAR iBufferCount          AS INT    NO-UNDO.
DEF VAR hBuffer2              AS HANDLE NO-UNDO.
DEF VAR httBuffer             AS HANDLE NO-UNDO.
DEF VAR hQuery                AS HANDLE NO-UNDO.
DEF VAR cAddedFields          AS CHAR   NO-UNDO.            /* All fields added to temp-table */
DEF VAR cDuplicateFields      AS CHAR   NO-UNDO EXTENT 100. /* Duplicate fields pr buffer num */
DEF VAR cExceptionFields      AS CHAR   NO-UNDO EXTENT 100. 
DEF VAR cStatBufferFields     AS CHAR   NO-UNDO EXTENT 100. /* Stat.fields pr buffer */
DEF VAR cAccumBufferFields    AS CHAR   NO-UNDO EXTENT 100. /* Accumulated.fields pr buffer */
DEF VAR fStatBufferValues     AS DEC    NO-UNDO EXTENT 100.  
DEF VAR cCalcStatFields       AS CHAR   NO-UNDO.            
DEF VAR fCalcStatValues       AS DEC    NO-UNDO EXTENT 100. 
DEF VAR cFieldPairsList       AS CHAR   NO-UNDO EXTENT 100. 
DEF VAR cField                AS CHAR   NO-UNDO.
DEF VAR hField                AS HANDLE NO-UNDO.
DEF VAR hField2               AS HANDLE NO-UNDO.
DEF VAR ix                    AS INT    NO-UNDO.
DEF VAR iy                    AS INT    NO-UNDO.
DEF VAR iz                    AS INT    NO-UNDO.
DEF VAR ia                    AS INT    NO-UNDO.
DEF VAR iq                    AS INT    NO-UNDO.
DEF VAR iCount                AS INT    NO-UNDO.
DEF VAR iQueryCount           AS INT    NO-UNDO.
DEF VAR cQueryString          AS CHAR   NO-UNDO.
DEF VAR cSortString           AS CHAR   NO-UNDO.
DEF VAR bOk                   AS LOG    NO-UNDO.
/* Extra fields to add to temp-table (and hence to buffer on client) */
DEF VAR cExtraField           AS CHAR   NO-UNDO.
DEF VAR cExtraDataType        AS CHAR   NO-UNDO.
DEF VAR cExtraFormat          AS CHAR   NO-UNDO.
DEF VAR cExtraInit            AS CHAR   NO-UNDO. 
DEF VAR cExtraParam           AS CHAR   NO-UNDO.
DEF VAR cCallProcOutput       AS CHAR   NO-UNDO.
DEF VAR cExtraLabel           AS CHAR   NO-UNDO.
DEF VAR cFieldName            AS CHAR   NO-UNDO.
DEF VAR cBufferList           AS CHAR   NO-UNDO.  
DEF VAR cOverrideLabel        AS CHAR   NO-UNDO.
DEF VAR cOverrideFormat       AS CHAR   NO-UNDO.
DEF VAR iSelected             AS INT    NO-UNDO.
DEF VAR cLogFile              AS CHAR   NO-UNDO.
DEF VAR cBufferFieldList      AS CHAR   NO-UNDO.
DEF VAR cTmpStatOutput        AS CHAR   NO-UNDO.
DEF VAR cCalcFieldProcs       AS CHAR   NO-UNDO.
DEF VAR hCalcFieldProcs       AS HANDLE NO-UNDO EXTENT 10.
DEF VAR cSortPhrase           AS CHAR   NO-UNDO.
DEF VAR cSortFields           AS CHAR   NO-UNDO.
DEF VAR cCalcSortFields       AS CHAR   NO-UNDO.
DEF VAR cCalcSortFldTypes     AS CHAR   NO-UNDO.
DEF VAR httPreScan            AS HANDLE NO-UNDO.
DEF VAR hBuffPreScan          AS HANDLE NO-UNDO.
DEF VAR hPreScanBuffer        AS HANDLE NO-UNDO.
DEF VAR cAccumFields          AS CHAR   NO-UNDO.
DEF VAR cAccumFldTypes        AS CHAR   NO-UNDO.
DEF VAR bIndexOnRowids        AS LOG    NO-UNDO.
DEF VAR hPreScanRowIdCol      AS HANDLE NO-UNDO.
DEF VAR bDistinctRows         AS LOG    NO-UNDO.
DEF VAR cFirstMainQueryEntry  AS CHAR   NO-UNDO.
                            
DEF VAR httPreScanJoin        AS HANDLE NO-UNDO.
DEF VAR hBuffPreScanJoin      AS HANDLE NO-UNDO.
DEF VAR cPreScanJoinQueries   AS CHAR   NO-UNDO.
DEF VAR iPreScanLimit         AS INT    NO-UNDO. 
DEF VAR cCalcFieldFilter      AS CHAR   NO-UNDO.
DEF VAR bCalcFieldFilter      AS LOG    NO-UNDO.
DEF VAR bCheckForSkipRow      AS LOG    NO-UNDO.
DEF VAR hPreScanCountDistinct AS HANDLE NO-UNDO.
DEF VAR hPreScanAccumField    AS HANDLE NO-UNDO.
DEF VAR hAverageFillField     AS HANDLE NO-UNDO.
DEF VAR cReturnOKmessage      AS CHAR   NO-UNDO.
DEF VAR bReturnQueryInfo      AS LOG    NO-UNDO.
DEF VAR cNotExistList         AS CHAR   NO-UNDO.
DEF VAR hNotCanDoTT           AS HANDLE NO-UNDO EXTENT 10.
DEF VAR cSkipCalcFields       AS CHAR   NO-UNDO.
DEF VAR cUniquBuffer          AS CHAR   NO-UNDO.
DEF VAR hJbAPI                AS HANDLE NO-UNDO.
DEF VAR cBufferAccessCheckList AS CHAR  NO-UNDO.  
DEF VAR cBufferAccessExprList AS CHAR   NO-UNDO.
DEF VAR hBufferAccessCheck    AS HANDLE NO-UNDO.
DEF VAR cReturnMessage        AS CHAR   NO-UNDO.
DEF VAR iStopAfter            AS INT    NO-UNDO.
DEF VAR iMainQueryStopAfter   AS INT    NO-UNDO.
DEF VAR bMainQueryStopped     AS LOG    NO-UNDO INIT YES.
DEF VAR cOrgBuffersAndFields  AS CHAR   NO-UNDO. /* passed in when buffer sequence has been changed */
DEF VAR cOrgBufferList        AS CHAR   NO-UNDO. /* used when buffer seq is changed to determing org sequence. Used for RowIdent* fields */
DEF VAR iOrgBufferNum         AS INT    NO-UNDO EXTENT 30. /* reference to original sequence# for buffer RowIdent - used when buffer seq is changed */

/* Tables qry defs. Used in ProcessPreScanJoins 
   to be able to append org. query string (instance 0) with prescan criteria */ 
DEF TEMP-TABLE ttQuery NO-UNDO
    FIELD iQueryNum       AS INT
    FIELD iQryBufferNum   AS INT
    FIELD cQryBuffer      AS CHAR
    FIELD cQryJoinString  AS CHAR
    FIELD cQryString      AS CHAR
    FIELD bContainsWHERE  AS LOG
    FIELD bOuterJoin      AS LOG
    FIELD cQrySort        AS CHAR
    FIELD bPreScanOnly    AS LOG
    .
DEF BUFFER bttQuery FOR ttQuery.

DEF TEMP-TABLE ttQueryBufferLists NO-UNDO
    FIELD iQueryNum        AS INT
    FIELD cQryBufferList   AS CHAR
    FIELD iNumQryBuffs     AS INT
    FIELD bPassExec        AS LOG
    FIELD cFirstQueryComp  AS CHAR
    FIELD cSecondQueryComp AS CHAR
    FIELD bMandatory       AS LOG
    .
DEF BUFFER bttQueryBufferLists FOR ttQueryBufferLists.

/* This table holds one record for each field that needs special treatment during retrieval: */

DEF TEMP-TABLE ttSpecFlds  NO-UNDO
    FIELD iCallBufferNum           AS INT          /* The source buffer num in the query to feed the calc.proc */
    FIELD iFieldNum                AS INT          /* Seqnum within buffer */
    FIELD cFillField               AS CHAR         /* Name of calculated field */
    FIELD cFillFieldType           AS CHAR         /* Datatype for calculated field */
    FIELD hSourceField             AS HANDLE       /* Two purposes: Handle to parameter field for calculation or handle to distinct column */
    FIELD hSourceField2            AS HANDLE       /* Handle to additional parameter for jb_calc */
    FIELD hTargetBuff              AS HANDLE 
    FIELD hTargetQuery             AS HANDLE
    FIELD cTargetQuery             AS CHAR
    FIELD cCallProc                AS CHAR         /* Name of procedure for calculation */
    FIELD cCallProcParam           AS CHAR         /* String parameter to calc.procedure */
    FIELD bGrandTotal              AS LOG          /* If the calc.field is in the list of grand totals */
    FIELD bDistinct                AS LOG          
    FIELD bAccum                   AS LOG          
    FIELD b2phAcc                  AS LOG          /* Indicates if the calculated field should be invoked on the accumulated resultset */
    FIELD hCalcFieldProc           AS HANDLE       /* Handle to the calc proc if it is in a persistent procedure */
    FIELD cCalcSortField           AS CHAR         /* If the calc.field is member of the sort fields put it's name here as well (equals cFillField) */
    FIELD cFilterOperator          AS CHAR         /* If the field is part of the filter.. */
    FIELD cFilterValue             AS CHAR         /* If the field is part of the filter.. */
    FIELD iExtent                  AS INT          /* The extent for an array field */
    FIELD bCalcByPreScan           AS LOG          /* Indicates if the value was calculated by prescan */
    FIELD bDirectAssign            AS LOG
    FIELD hCompareFields           AS HANDLE EXTENT 12   /* For dynamic field comparison (max, min) */
    FIELD iCntCompareFields        AS INT                /* For dynamic field comparison (max, min) */
    INDEX idxCallBuffer  IS PRIMARY UNIQUE  iCallBufferNum iFieldNum
    INDEX idxCallBuffPrescan  iCallBufferNum bCalcByPreScan iFieldNum  
    .

/* Temp table current row contribution to grand total, accum by distinct and calculated sort columns for the current row.
   PreScan: Calculated field values, accum by distinct and calculated sort column values are added to the PreScan table,
            if nothing prevented the row to be used (that would be one of the calculation procedures to return "skiprow")
   No PreScan: Calculated field values are added to the output table buffer
            if nothing prevented the row to be used (that would be one of the calculation procedures to return "skiprow")
    */
DEF TEMP-TABLE ttRowSpecVal  NO-UNDO
    FIELD iCallBufferNum   AS INT          /* The source buffer num in the query to feed the calc.proc */
    FIELD cFillField       AS CHAR         /* Name of calculated field */
    FIELD hFillField       AS HANDLE       /* Handle to field in the output temp-table where the corresponding values from PreScan 
                                              are moved in ProcessQuery */
    FIELD cCallProc        AS CHAR         /* Name of procedure for calculation (either in persistent or external procedure)
                                              Used here to check if the value comes from a calc.field or the source buffer */
    FIELD hSourceField     AS HANDLE       /* Two purposes: Handle to parameter field for calculation or handle to distinct column */
    FIELD bAccum           AS LOG          /* Here for same reason as bDistinct (approx) */
    FIELD b2phAcc          AS LOG          /* Indicates if the calculated field should be invoked on the accumulated resultset */
    FIELD cCurrCallOutput  AS CHAR         /* Keeps the last output value from the calculated field when processing the query */
    FIELD fRowValue        AS DEC          /* Value for grand total field for current row */
    FIELD fGrandTotal      AS DEC          /* The accumulated value for GrandTotal fields */
    FIELD hPreScanField    AS HANDLE       /* Handle to corresponding field in the PreScan table when prescan or in output table when no prescan
                                              (added for preformance reason) */
    FIELD bOnlyGrandTotal  AS LOG          /* Fields that ar just grand totals should not be stored in the PreScan table (accum values kept in ttSpecFlds) */
    FIELD bCalcByPreScan   AS LOG          /* Indicates if the value was calculated by prescan */
    INDEX idxCallBuffer IS UNIQUE PRIMARY iCallBufferNum cFillField
    INDEX idxCalcByPreScan bCalcByPreScan    
    .

DEF TEMP-TABLE ttAccumProcessed NO-UNDO
    FIELD iCallBufferNum AS INT
    FIELD iFieldNum      AS INT
    FIELD rRowid         AS ROWID
    INDEX idxFieldRow iCallBufferNum iFieldNum rRowid
    .

DEF TEMP-TABLE ttCalcContains
    FIELD cCalcValue AS CHAR
    INDEX idxCalcValue  AS WORD-INDEX cCalcValue
    .
    
DEF TEMP-TABLE ttOrgBuffsAndFlds
    FIELD iBufferNum     AS INT
    FIELD cBufferName    AS CHAR
    FIELD cFieldName     AS CHAR
    FIELD bDuplicate     AS LOG
    .   

DEF STREAM sLog.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-AddTTindex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddTTindex Procedure 
FUNCTION AddTTindex RETURNS LOGICAL
  ( INPUT icPrimaryBufferFlds AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AppendTTbuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AppendTTbuffers Procedure 
FUNCTION AppendTTbuffers RETURNS LOGICAL
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AssignStringValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AssignStringValue Procedure 
FUNCTION AssignStringValue RETURNS CHARACTER
  ( INPUT ihField       AS HANDLE,
    INPUT icUpdateValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CalcValueOk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CalcValueOk Procedure 
FUNCTION CalcValueOk RETURNS LOGICAL
  ( INPUT cCalcValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckFieldLevelSecurity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckFieldLevelSecurity Procedure 
FUNCTION CheckFieldLevelSecurity RETURNS CHARACTER
  ( INPUT icMode AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ComposeQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ComposeQuery Procedure 
FUNCTION ComposeQuery RETURNS CHARACTER
  ( INPUT iiQueryNum AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateExtentCalcField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateExtentCalcField Procedure 
FUNCTION CreateExtentCalcField RETURNS CHARACTER
  ( INPUT ihBuffer     AS HANDLE,
    INPUT iiBufferNum  AS INT,
    INPUT iiFieldNum   AS INT,
    INPUT icFieldName  AS CHAR,
    INPUT icDataType   AS CHAR,
    INPUT icFormat     AS CHAR,
    INPUT icLabel      AS CHAR,
    INPUT ibDistinct   AS LOG,
    INPUT ibAccum      AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateFieldPairLists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateFieldPairLists Procedure
FUNCTION UnpackOrgBuffersAndFields RETURNS LOGICAL 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-DebugPreScan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DebugPreScan Procedure 
FUNCTION DebugPreScan RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DecomposeQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DecomposeQuery Procedure 
FUNCTION DecomposeQuery RETURNS LOGICAL
  ( INPUT iiQueryNum    AS INT,
    INPUT icQueryString AS CHAR,
    INPUT icBufferList  AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FinalizeCreateQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FinalizeCreateQuery Procedure 
FUNCTION FinalizeCreateQuery RETURNS CHARACTER
  ( INPUT icPrimBufferDbFlds AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixQuery Procedure 
FUNCTION FixQuery RETURNS CHARACTER
  ( INPUT icQueryCriteria AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQueryJoin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixQueryJoin Procedure 
FUNCTION FixQueryJoin RETURNS CHARACTER
  ( INPUT icQueryCriteria AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQueryOperators) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixQueryOperators Procedure 
FUNCTION FixQueryOperators RETURNS CHARACTER
  ( INPUT icQueryString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getActiveQueryBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getActiveQueryBuffer Procedure 
FUNCTION getActiveQueryBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getASuserId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getASuserId Procedure 
FUNCTION getASuserId RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCalcProcHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCalcProcHandle Procedure
FUNCTION getCalcProcHandle RETURNS HANDLE 
  (INPUT icProcName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getCodeMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCodeMaster Procedure 
FUNCTION getCodeMaster RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompanyId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCompanyId Procedure 
FUNCTION getCompanyId RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLanguageCode Procedure 
FUNCTION getLanguageCode RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryKeyFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPrimaryKeyFields Procedure 
FUNCTION getPrimaryKeyFields RETURNS CHARACTER
  (INPUT ihBuffer AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQueryBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQueryBuffer Procedure 
FUNCTION getQueryBuffer RETURNS HANDLE
  ( INPUT icBufferName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetQueryFalseCrit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetQueryFalseCrit Procedure 
FUNCTION GetQueryFalseCrit RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getResultSetBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getResultSetBuffer Procedure 
FUNCTION getResultSetBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogThis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LogThis Procedure 
FUNCTION LogThis RETURNS LOGICAL
  ( INPUT icLogEntry  AS CHAR,
    INPUT icLogText   AS CHAR,
    INPUT iiSkipLines AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OnlyCalcFldsInBufferDef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OnlyCalcFldsInBufferDef Procedure 
FUNCTION OnlyCalcFldsInBufferDef RETURNS LOGICAL
  ( INPUT icBufferDef AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrescanIncludedInMain) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PrescanIncludedInMain Procedure 
FUNCTION PrescanIncludedInMain RETURNS LOGICAL
  ( INPUT icPreScanBufferList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrimaryBufferSpeedOk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PrimaryBufferSpeedOk Procedure 
FUNCTION PrimaryBufferSpeedOk RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessPreScanJoins) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ProcessPreScanJoins Procedure 
FUNCTION ProcessPreScanJoins RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-QueryOpen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD QueryOpen Procedure 
FUNCTION QueryOpen RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReduceQueries) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReduceQueries Procedure 
FUNCTION ReduceQueries RETURNS LOGICAL
  ( INPUT iiMinQueryNum AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCalcFieldFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCalcFieldFilter Procedure 
FUNCTION setCalcFieldFilter RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCheckForSkipRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCheckForSkipRow Procedure 
FUNCTION setCheckForSkipRow RETURNS LOGICAL
  ( INPUT ibCheckForSkipRow AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldBufferSwap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldBufferSwap Procedure
FUNCTION setFieldBufferSwap RETURNS LOGICAL 
  (INPUT icBufferName   AS CHAR,
   INPUT iiBufferNum    AS INT,
   INPUT ihBufferField  AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setJbCalcParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setJbCalcParams Procedure 
FUNCTION setJbCalcParams RETURNS CHARACTER
  ( INPUT icCalcProc  AS CHAR,
    INPUT iiBufferNum AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setReturnMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setReturnMessage Procedure 
FUNCTION setReturnMessage RETURNS LOGICAL
  ( INPUT icReturnMessage AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startASlib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startASlib Procedure 
FUNCTION startASlib RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TooManyHits) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TooManyHits Procedure 
FUNCTION TooManyHits RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserRowAccess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UserRowAccess Procedure 
FUNCTION UserRowAccess RETURNS LOGICAL
  ( INPUT ihBuffer AS HANDLE )  FORWARD.

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
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 39.14
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

/*{incl/weeknum.i}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{incl/validatesession.i}
{incl/custom_calcfieldprocs.i}

IF icParam NE "" THEN DO ix = 1 TO NUM-ENTRIES(icParam):
  IF ENTRY(1,ENTRY(ix,icParam),";") = "logfile" THEN DO:
    ETIME(TRUE).
    cLogFile = ENTRY(2,ENTRY(ix,icParam),";").
    LogThis(STRING(TODAY) + " -- " + STRING(TIME,"HH:MM:SS"),FILL("-",30),1).
    LogThis("icParam",icParam,1).
  END.
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "calcfieldproc" THEN
    DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,icParam),";"):
      IF LOOKUP(ENTRY(iy,ENTRY(ix,icParam),";"),cCalcFieldProcs,";") = 0 THEN
        cCalcFieldProcs = cCalcFieldProcs + ENTRY(iy,ENTRY(ix,icParam),";") + ";".
    END.
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "indexonrowids" THEN
    bIndexOnRowids = ENTRY(2,ENTRY(ix,icParam),";") = "yes". 
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "prescanquery" THEN
    cPreScanJoinQueries = REPLACE(REPLACE(ENTRY(2,ENTRY(ix,icParam),";"),"OUTER-JOIN",""),"NO-LOCK","").
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "calcfieldfilter" THEN
    cCalcFieldFilter = REPLACE(ENTRY(2,ENTRY(ix,icParam),";"),CHR(1),",").
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "prescanlimit" THEN
    iPreScanLimit = INT(ENTRY(2,ENTRY(ix,icParam),";")) NO-ERROR.
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "returnqueryinfo" THEN
    bReturnQueryInfo = LOGICAL(ENTRY(2,ENTRY(ix,icParam),";")) NO-ERROR.
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "noexistbuffers" THEN
    cNotExistList = REPLACE(ENTRY(2,ENTRY(ix,icParam),";"),"¤",",").
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "skipcalcfields" THEN
    cSkipCalcFields = REPLACE(ENTRY(2,ENTRY(ix,icParam),";"),"¤",",").
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "uniqueBuffer" THEN
    cUniquBuffer = ENTRY(2,ENTRY(ix,icParam),";").
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "orgBuffersAndFields" THEN
    cOrgBuffersAndFields = REPLACE(REPLACE(ENTRY(2,ENTRY(ix,icParam),";"),"¤",","),CHR(1),";").
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "queryStopAfter" THEN
    iMainQueryStopAfter = INT(ENTRY(2,ENTRY(ix,icParam),";")) NO-ERROR.
END.

cPreScanJoinQueries = REPLACE(cPreScanJoinQueries,CHR(3),"|").

ix = 1.
/* Start any persistent procedures for calculated fields: */
IF cCalcFieldProcs NE "" AND NOT icQueryCriteria BEGINS "where false" THEN DO:
/*   cCalcFieldProcs = TRIM(cCalcFieldProcs,";"). */
  DO ix = 1 TO NUM-ENTRIES(TRIM(cCalcFieldProcs,";"),";"):
    IF SEARCH(ENTRY(ix,cCalcFieldProcs,";")) = ? AND SEARCH(SUBSTR(ENTRY(ix,cCalcFieldProcs,";"),1,LENGTH(ENTRY(ix,cCalcFieldProcs,";")) - 1) + "r") = ? THEN DO:
      ocReturn = "Couldn't find server program for calculated fields: " + ENTRY(ix,cCalcFieldProcs,";").
      RETURN.
    END.
    ELSE RUN VALUE(ENTRY(ix,cCalcFieldProcs,";")) PERSIST SET hCalcFieldProcs[ix].
  END.
END.
ELSE cCalcFieldProcs = " ".
hCalcFieldProcs[ix] = THIS-PROCEDURE.

icQueryCriteria = REPLACE(TRIM(icQueryCriteria)," distinct "," ").

LogThis("Persistent CalcFieldProcs",cCalcFieldProcs,1).
LogThis("CalcField Filter",cCalcFieldFilter,1).
IF cPreScanJoinQueries NE "" THEN DO ix = 1 TO NUM-ENTRIES(cPreScanJoinQueries,CHR(28)):
  LogThis(IF ix = 1 THEN "Prescan queries" ELSE "",REPLACE(REPLACE(ENTRY(ix,cPreScanJoinQueries,CHR(28)),"¤",","),CHR(1),"|"),
          IF ix = NUM-ENTRIES(cPreScanJoinQueries,CHR(28)) THEN 1 ELSE 0).
END.
LogThis("Main query",icQueryCriteria,1).
LogThis("BuffersAndFields",icBuffersAndFields,1).
LogThis("orgBuffersAndFields",cOrgBuffersAndFields,1).
LogThis("Batch size",STRING(iiBatchSize),0).
LogThis("iiStartRow:",STRING(iiStartRow),0).
LogThis("icDirection:",icDirection,0).
LogThis("QueryStopAfter:",STRING(iMainQueryStopAfter),1).

IF TRIM(cOrgBuffersAndFields) NE "" THEN
  UnpackOrgBuffersAndFields().

IF NOT icQueryCriteria BEGINS "where false" THEN DO:
  IF INDEX(icQueryCriteria," BY ") > 0 THEN
    cSortPhrase = SUBSTR(icQueryCriteria,INDEX(icQueryCriteria," BY ")).
  ELSE IF icQueryCriteria BEGINS "BY " THEN
    cSortPhrase = " " + icQueryCriteria.
  IF cSortPhrase NE "" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cSortPhrase," "):
      IF NOT CAN-DO("' ',BY,DESC",ENTRY(ix,cSortPhrase," ")) THEN
        cSortFields = cSortFields + (IF INDEX(ENTRY(ix,cSortPhrase," "),".") > 0 THEN
                                       SUBSTR(ENTRY(ix,cSortPhrase," "),INDEX(ENTRY(ix,cSortPhrase," "),".") + 1)
                                     ELSE ENTRY(ix,cSortPhrase," ")) + ",".
    END.
    cSortFields = TRIM(cSortFields,",").
  END.
END.
ELSE DO:
  IF INDEX(icQueryCriteria," BY ") > 0 THEN   
    icQueryCriteria = SUBSTR(icQueryCriteria,1,INDEX(icQueryCriteria," BY ")).
END.

ASSIGN icStatFields = TRIM(icStatFields,",")
       iBufferCount = NUM-ENTRIES(icBuffersAndFields).

IF cCalcFieldFilter NE "" AND icStatFields = "rowcount" THEN
  bCheckForSkipRow = YES.

LogThis("Sorting",cSortPhrase + "  Sortfields: " + cSortFields,1).
LogThis("icStatFields:",icStatFields,1).
LogThis("bCheckForSkipRow:",STRING(bCheckForSkipRow),1).

CREATE QUERY hQuery.
IF icDirection = "" THEN
  hQuery:FORWARD-ONLY = YES.
CREATE TEMP-TABLE ohTempTable.
RUN CreateQuery (OUTPUT ocReturn).

LogThis("Result buffer indexes",httBuffer:INDEX-INFORMATION(1),1).
  
FOR EACH ttSpecFlds
    WHERE ttSpecFlds.cCallProc = "jb_calc":
  IF NOT VALID-HANDLE(ttSpecFlds.hSourceField) OR NOT VALID-HANDLE(ttSpecFlds.hSourceField2) THEN
    setJbCalcParams(ttSpecFlds.cCallProc,0).
END.

DO ix = 1 TO 10:
  IF VALID-HANDLE(hCalcFieldProcs[ix]) AND CAN-DO(hCalcFieldProcs[ix]:INTERNAL-ENTRIES,"setResultSetBuffer") THEN
    DYNAMIC-FUNCTION("setResultSetBuffer" IN hCalcFieldProcs[ix],httBuffer).
END.

IF cNotExistList NE "" THEN DO:    
  AppendTTbuffers().  
  LogThis("Not exist buffers",cNotExistList,1).
  LogThis("Appended query",icQueryCriteria,1).
END.

IF cLogFile NE "" THEN DO:
  LogThis("Duplicate fields:","",0).
  DO ix = 1 TO iBufferCount:
    LogThis(hBuffer[ix]:NAME + ":",cDuplicateFields[ix],0).
  END.
  LogThis("","",1). 
  LogThis("Fields not returned:","",0).
  DO ix = 1 TO iBufferCount:
    LogThis(hBuffer[ix]:NAME + ":",cExceptionFields[ix],0).
  END.
  LogThis("cBufferList",cBufferList,0).
  LogThis("cOrgBufferList",cOrgBufferList,1).
  LogThis("cAccumFields:",cAccumFields,1).
  LogThis("cCalcStatFields:",cCalcStatFields,1).
  LogThis("cCalcSortFields",cCalcSortFields + " cCalcSortFldTypes: " + cCalcSortFldTypes,2).

  LogThis("Extra fields:","",1).

  IF cSkipCalcFields NE "" THEN
    LogThis("SkipCalcFields:",cSkipCalcFields,1).
    
  DO ix = 1 TO 10:
    IF cFieldPairsList[ix] NE "" THEN
      LogThis("Fieldpairs, buffer# " + STRING(ix),cFieldPairsList[ix],0).
  END.    
  DO ix = 1 TO 10:
    IF cExceptionFields[ix] NE "" THEN
      LogThis("Field exceptions, buffer# " + STRING(ix),cExceptionFields[ix],0).   
  END.    
END.

IF cCalcFieldFilter NE "" THEN DO:
  ocReturn = setCalcFieldFilter().
  IF ocReturn NE "" THEN RETURN.
END.

/* Assign the specific handle to the calculated and/or distinct fields in the temp-table (for performance): */
bOK = TRUE.
FOR EACH ttSpecFlds:
   /* The ttRowSpecVal tables keeps all values for special fields as they are
      obtained either as calculated or grand total fields (who also can be calculated): */
   CREATE ttRowSpecVal.
   BUFFER-COPY ttSpecFlds TO ttRowSpecVal.   
   ttRowSpecVal.hFillField    = httBuffer:BUFFER-FIELD(ttRowSpecVal.cFillField).

   IF CAN-DO("jb_weeknum,jb_month,jb_quarter,jb_year,jb_date_dt,jb_weeknum_dt,jb_month_dt,jb_quarter_dt,jb_year_dt,jb_calc,jb_can-find,jb_total",ttSpecFlds.cCallProc) AND icStatFields NE ""
      AND cCalcFieldFilter MATCHES "*jb_*_*" THEN
     bCheckForSkipRow = YES.

   LogThis(ttSpecFlds.cFillField, 
           ttSpecFlds.cCallProc + "(" 
         + ttSpecFlds.cCallProcParam + ")" 
         + " Distinct: " + STRING(ttSpecFlds.bDistinct) 
         + " Accum: " + STRING(ttSpecFlds.bAccum) 
         + " 2.phase calc: " + STRING(ttSpecFlds.b2phAcc) 
         + " Buffernum: " + STRING(ttSpecFlds.iCallBufferNum)
         + " Statfld: " + STRING(ttSpecFlds.bGrandTotal)
         + " CalcSortFld: " + STRING(ttSpecFlds.cCalcSortField NE "")
         + " Filter operator: " + ttSpecFlds.cFilterOperator
         + " Filter value: " + ttSpecFlds.cFilterValue
         + " Extent: " + STRING(ttSpecFlds.iExtent)
            ,0).
END.
IF NOT bOK THEN DO:
  ocReturn = "Failed to assign handle for distinct and/or calculated field".
  RETURN.
END.

LogThis("","",1).
LogThis("Etime, query def: ", STRING(ETIME / 1000),2).

IF ocReturn NE "" THEN RETURN.

/* IF cCalcFieldFilter NE "" THEN DO: */
/*   ocReturn = setCalcFieldFilter(). */
/*   IF ocReturn NE "" THEN RETURN.   */
/* END.                               */

IF bReturnQueryInfo THEN
  CheckFieldLevelSecurity("exec").

IF iMainQueryStopAfter NE 0 THEN
  DO STOP-AFTER iMainQueryStopAfter ON STOP UNDO, LEAVE:
    RUN ProcessQuery (OUTPUT ocReturn).
    bMainQueryStopped = NO.
  END.
ELSE DO:    
  bMainQueryStopped = NO.
  RUN ProcessQuery (OUTPUT ocReturn).
END.

IF ocReturn NE "" THEN DO:
  ocReturn = ocReturn + (IF bReturnQueryInfo THEN "¤" + TRIM(cReturnOKmessage,",") ELSE "").
  RETURN.
END.

RUN WrapUp.

IF VALID-HANDLE(ohTempTable) THEN DELETE OBJECT ohTempTable.


ocReturn = cReturnMessage + (IF bReturnQueryInfo THEN "¤" + TRIM(cReturnOKmessage,",") ELSE "").

LogThis("Return param: ", ocReturn,1).
LogThis("","",2).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CreateQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateQuery Procedure 
PROCEDURE CreateQuery :
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

DEF VAR bDistinct          AS LOG    NO-UNDO.
DEF VAR bAccum             AS LOG    NO-UNDO.
DEF VAR bExt               AS LOG    NO-UNDO.
DEF VAR ixf                AS INT    NO-UNDO.
DEF VAR cBufDef            AS CHAR   NO-UNDO.
DEF VAR bFieldSpec         AS LOG    NO-UNDO.
DEF VAR cRealFldSpc        AS CHAR   NO-UNDO.
DEF VAR cRealDbFldSpc      AS CHAR   NO-UNDO.
DEF VAR cPrimBufDbFlds     AS CHAR   NO-UNDO.
DEF VAR bOnlyCalcFieldsDef AS LOG    NO-UNDO.
DEF VAR cOrgFldNumExt      AS CHAR   NO-UNDO.

DO ix = 1 TO iBufferCount:
  ASSIGN cBufferFieldList = ""
         ixf              = 0
         cBufDef       = ENTRY(ix,icBuffersAndFields)
         .

  IF ENTRY(1,cBufDef,";") MATCHES "buf*_*" THEN DO:
    CREATE BUFFER hBuffer[ix] FOR TABLE SUBSTR(ENTRY(1,cBufDef,";"),6)
                  BUFFER-NAME ENTRY(1,cBufDef,";") NO-ERROR.
    IF ERROR-STATUS:ERROR OR hBuffer[ix] = ? THEN DO:
      ocReturn = "Table " + SUBSTR(ENTRY(1,cBufDef,";"),6) + " doesn't exist in database. Buffer definition: " + ENTRY(1,cBufDef,";").
      LEAVE.
    END.
  END.
  ELSE DO:
    CREATE BUFFER hBuffer[ix] FOR TABLE ENTRY(1,cBufDef,";") NO-ERROR.
    IF ERROR-STATUS:ERROR OR hBuffer[ix] = ? THEN DO:
      ocReturn = "Table " + ENTRY(1,cBufDef,";") + " doesn't exist in database".
      LEAVE.
    END.
  END.

  bOnlyCalcFieldsDef = OnlyCalcFldsInBufferDef(cBufDef).
  LogThis("Bufferdef " + STRING(ix) + ":",cBufDef,0).

  IF NUM-ENTRIES(cBufDef,";") > 1 AND ENTRY(2,cBufDef,";") NE "" THEN DO:
    IF NOT bFieldSpec AND NOT bOnlyCalcFieldsDef THEN
      bFieldSpec = YES.
/*     bFieldSpec = YES. */
    FieldSpec:
    DO iy = 2 TO NUM-ENTRIES(cBufDef,";"):
      ASSIGN bDistinct    = FALSE
             bAccum       = FALSE
             bExt      = FALSE
             cExtraFormat = ""
             cExtraLabel  = "".
      /* Calculated field (or just an extra field): */
      IF ENTRY(iy,cBufDef,";") BEGINS "+" OR SUBSTR(ENTRY(1,ENTRY(iy,cBufDef,";"),"|"),LENGTH(ENTRY(1,ENTRY(iy,cBufDef,";"),"|")),1) = "]" THEN DO:
        DO iz = 1 TO NUM-ENTRIES(ENTRY(iy,cBufDef,";"),"|"):
          CASE iz:
            WHEN 1 THEN DO:
              IF SUBSTR(ENTRY(iz,ENTRY(iy,cBufDef,";"),"|"),LENGTH(ENTRY(iz,ENTRY(iy,cBufDef,";"),"|")),1) = "]" THEN                
                ASSIGN bExt     = YES
                       cExtraField = ENTRY(iz,ENTRY(iy,cBufDef,";"),"|").
              ELSE cExtraField  = SUBSTR(ENTRY(iz,ENTRY(iy,cBufDef,";"),"|"),2).

              IF TRIM(cExtraField) BEGINS "ACCUM " THEN 
                ASSIGN bAccum       = TRUE
                       cExtraField  = TRIM(SUBSTR(cExtraField,INDEX(cExtraField,"accum ") + 6)).
              ELSE IF TRIM(cExtraField) BEGINS "DISTINCT " THEN
                ASSIGN bDistinct   = TRUE
                       cExtraField = TRIM(SUBSTR(cExtraField,INDEX(cExtraField,"distinct ") + 8)).
              IF CAN-DO(cSkipCalcFields,cExtraField) THEN NEXT FieldSpec.
            END.
            WHEN 2 THEN DO:
              IF bExt AND NOT cExtraField BEGINS "+" THEN 
                cExtraLabel = ENTRY(iz,ENTRY(iy,cBufDef,";"),"|").
              ELSE                  
                cExtraDataType = ENTRY(iz,ENTRY(iy,cBufDef,";"),"|").
            END.
            WHEN 3 THEN 
              cExtraFormat   = REPLACE(ENTRY(iz,ENTRY(iy,cBufDef,";"),"|"),"<",",").
            WHEN 4 THEN DO:
              ASSIGN cExtraInit  = ENTRY(iz,ENTRY(iy,cBufDef,";"),"|")
                     cExtraParam = IF cExtraInit MATCHES "*(*" THEN SUBSTR(cExtraInit,INDEX(cExtraInit,"(") + 1,R-INDEX(cExtraInit,")") - INDEX(cExtraInit,"(") - 1) ELSE ""
                     cExtraInit  = IF cExtraInit MATCHES "*(*" THEN TRIM(SUBSTR(cExtraInit,1,INDEX(cExtraInit,"(") - 1)) ELSE cExtraInit
                     bOk         = R-INDEX(cExtraInit,".p") > 0 OR TRIM(ENTRY(1,icQueryCriteria)) = "WHERE false"
                     iq          = 0.
              IF NOT bOk THEN DO iq = 1 TO NUM-ENTRIES(cCalcFieldProcs,";"):
                IF CAN-DO(hCalcFieldProcs[iq]:INTERNAL-ENTRIES,cExtraInit) THEN DO:
                  bOK = TRUE.
                  LEAVE.
                END.
              END.
              IF bOk THEN DO:
                CREATE ttSpecFlds.
                ASSIGN ttSpecFlds.iCallBufferNum  = ix
                       ixf                             = ixf + 1
                       ttSpecFlds.iFieldNum       = ixf
                       ttSpecFlds.cFillField      = cExtraField
                       ttSpecFlds.cFillFieldType  = cExtraDataType
                       ttSpecFlds.cCallProc       = cExtraInit
                       ttSpecFlds.cCallProcParam  = REPLACE(cExtraParam,CHR(1),",")
                       ttSpecFlds.bGrandTotal     = CAN-DO(icStatFields,cExtraField) OR bAccum
                       ttSpecFlds.bDistinct       = bDistinct
                       ttSpecFlds.bAccum          = bAccum
                       ttSpecFlds.b2phAcc         = cExtraField BEGINS "2ph_" 
                       ttSpecFlds.hCalcFieldProc  = IF iq > 0 THEN hCalcFieldProcs[iq] ELSE ?
                       .
                /* If the param is a field grab handle: */
                IF cExtraParam NE "" AND NOT cExtraParam BEGINS "ROWID" THEN DO:
                  IF CAN-DO("jb_calc,jb_can-find,jb_total,jb_max,jb_min,jb_random",ttSpecFlds.cCallProc) THEN DO:
                    ocReturn = setJBCalcParams(ttSpecFlds.cCallProc,ix). IF ocReturn NE "" THEN LEAVE.
                  END.
                  ELSE
                    ttSpecFlds.hSourceField = hBuffer[ix]:BUFFER-FIELD(ENTRY(NUM-ENTRIES(cExtraParam,"."),cExtraParam,".")).
                END. 

                /* If the calc. value is used for sorting go an extra round first (see under) and get the values */
                IF CAN-DO(cSortFields,cExtraField) THEN
                  ASSIGN ttSpecFlds.cCalcSortField = cExtraField
                         cCalcSortFields                = cCalcSortFields + cExtraField + ","
                         cCalcSortFldTypes              = cCalcSortFldTypes + cExtraDataType + ",".

                IF bAccum THEN
                  ASSIGN cAccumFields   = cAccumFields + cExtraField + ","
                         cAccumFldTypes = cAccumFldTypes + cExtraDataType + ",".
              END.
              cExtraInit = "".
            END.
            WHEN 5 THEN cExtraLabel    = ENTRY(iz,ENTRY(iy,cBufDef,";"),"|").
          END CASE.
        END.
        iz = iz - 1.
        IF iz < 2 AND NOT bExt THEN DO:
          ocReturn = "Missing data type for extra field " + cExtraField.
          NEXT.
        END.
        IF NOT bExt THEN CASE iz:
          WHEN 2 THEN ohTempTable:ADD-NEW-FIELD(cExtraField,cExtraDataType).
          WHEN 3 THEN ohTempTable:ADD-NEW-FIELD(cExtraField,cExtraDataType,0,cExtraFormat).
          WHEN 4 THEN DO:
            IF cExtraInit = "" THEN
              ohTempTable:ADD-NEW-FIELD(cExtraField,cExtraDataType,0,cExtraFormat).
            ELSE
              ohTempTable:ADD-NEW-FIELD(cExtraField,cExtraDataType,0,cExtraFormat,cExtraInit).
          END.
          WHEN 5 THEN 
            ohTempTable:ADD-NEW-FIELD(cExtraField,
                                      cExtraDataType,0,
                                      cExtraFormat,
                                      cExtraInit,
                                      cExtraLabel,
                                      IF cExtraLabel MATCHES "*" + CHR(10) + "*" THEN
                                        REPLACE(cExtraLabel,CHR(10),"!")
                                      ELSE ?) NO-ERROR.
        END CASE.
        ELSE DO:
          ixf = ixf + 1.
          ocReturn = CreateExtentCalcField(hBuffer[ix],ix,ixf,cExtraField,cExtraDataType,cExtraFormat,cExtraLabel,bDistinct,bAccum).  
          IF ocReturn NE "" THEN RETURN.
        END. 
        cExtraInit = "".
        
        IF CAN-DO(icStatFields,cExtraField) THEN 
          cCalcStatFields = cCalcStatFields + cExtraField + ",".
      END. /* Calculated field */

      /* DB buffer field: */
      ELSE DO:
        cField = ENTRY(1,ENTRY(iy,cBufDef,";"),"|").

        IF cField BEGINS "accum " THEN DO:
          ASSIGN cField                      = TRIM(SUBSTR(cField,7))
                 cAccumBufferFields[ix]      = cAccumBufferFields[ix] + cField + ","
                 bAccum                      = TRUE
                 .
          IF NOT CAN-DO(icStatFields,cField) THEN
            icStatFields = icStatFields + "," + cField.
        END.
        ELSE IF cField BEGINS "distinct " THEN DO: 
          CREATE ttSpecFlds.
          ASSIGN cField                     = TRIM(SUBSTR(cField,10))
                 ttSpecFlds.iCallBufferNum  = ix
                 ixf                        = ixf + 1
                 ttSpecFlds.iFieldNum       = ixf
                 ttSpecFlds.cFillField      = cField
                 ttSpecFlds.bDistinct       = TRUE
                 bDistinct                  = TRUE.
        END.
        hField = hBuffer[ix]:BUFFER-FIELD(cField) NO-ERROR.
        IF NOT VALID-HANDLE(hField) THEN DO:
          ocReturn = "Field " + ENTRY(1,ENTRY(iy,cBufDef,";"),"|") + CHR(10) + 
                     "doesn't exist in buffer " + hBuffer[ix]:NAME.
          NEXT.
        END.
        ELSE IF bDistinct THEN
          ASSIGN ttSpecFlds.hSourceField = hField
                 ttSpecFlds.cFillFieldType = hField:DATA-TYPE.

        cOrgFldNumExt = "".
        IF cOrgBuffersAndFields NE "" THEN DO:
          FIND FIRST ttOrgBuffsAndFlds
               WHERE ttOrgBuffsAndFlds.cBufferName = ENTRY(1,cBufDef,";")
                 AND ttOrgBuffsAndFlds.cFieldName  = cField
                 AND ttOrgBuffsAndFlds.bDuplicate
                 NO-ERROR.
          IF AVAIL ttOrgBuffsAndFlds THEN cOrgFldNumExt = STRING(ttOrgBuffsAndFlds.iBufferNum).       
        END.         

        IF NOT CAN-DO(cAddedFields,hField:NAME + cOrgFldNumExt) THEN DO:
          /* Check for overrides of format and label: */
          IF NUM-ENTRIES(ENTRY(iy,cBufDef,";"),"|") > 1 THEN DO:
            DO iz = 2 TO NUM-ENTRIES(ENTRY(iy,cBufDef,";"),"|"):
              IF iz = 2 THEN DO:
                IF ENTRY(iz,ENTRY(iy,cBufDef,";"),"|") NE "=" THEN 
                  cOverrideLabel = ENTRY(iz,ENTRY(iy,cBufDef,";"),"|").
                ELSE 
                  cOverrideLabel = hField:LABEL.
              END.
              ELSE IF iz = 3 THEN 
                cOverrideFormat = REPLACE(ENTRY(iz,ENTRY(iy,cBufDef,";"),"|"),"<",",").
            END.
            IF iz = 3 THEN /* No override on format */
              cOverrideFormat = hField:FORMAT.

            ohTempTable:ADD-NEW-FIELD(hField:NAME + cOrgFldNumExt,hField:DATA-TYPE,0,cOverrideFormat,
                                     IF hField:DATA-TYPE = "CHARACTER" THEN ""
                                     ELSE IF hField:DATA-TYPE = "DATE" THEN "?"
                                     ELSE IF hField:DATA-TYPE = "DATETIME" THEN ""
                                     ELSE IF hField:DATA-TYPE = "LOGICAL" THEN "no"
                                     ELSE "0",
                                     cOverrideLabel,
                                     IF cOverrideLabel MATCHES "*" + CHR(10) + "*" THEN
                                       REPLACE(cOverrideLabel,CHR(10),"!")
                                     ELSE ?)
                        NO-ERROR.
            IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
              ocReturn = "Error in create of field " + ENTRY(1,ENTRY(iy,cBufDef,";"),"|") + CHR(10) + 
                         ERROR-STATUS:GET-MESSAGE(1).
              NEXT.
            END.
          END.
          ELSE ohTempTable:ADD-LIKE-FIELD(hField:NAME + cOrgFldNumExt,hField).

          ASSIGN cAddedFields     = cAddedFields + hField:NAME + cOrgFldNumExt + ","
                 cBufferFieldList = cBufferFieldList + hField:NAME + cOrgFldNumExt + ",".

          IF cOrgFldNumExt NE "" THEN
            cFieldPairsList[ix] = cFieldPairsList[ix] + (IF cFieldPairsList[ix] NE "" THEN "," ELSE "") + hField:NAME + "," + hField:NAME + cOrgFldNumExt.
                        
          /* Check ahead if field is also in subsequent buffers. If so put in exceptionlist for buffer-copy
             - but only when subsequent buffer has no field-list. If that's the case the duplicate-list will step in */
          ELSE DO ia = ix + 1 TO iBufferCount:
            IF NUM-ENTRIES(ENTRY(ia,icBuffersAndFields),";") > 1 THEN DO:

              IF ENTRY(1,ENTRY(ia,icBuffersAndFields),";") MATCHES "buf*_*" THEN 
                CREATE BUFFER hBuffer2 FOR TABLE SUBSTR(TRIM(ENTRY(1,ENTRY(ia,icBuffersAndFields)),";"),6)
                              BUFFER-NAME ENTRY(1,ENTRY(ia,icBuffersAndFields),";") NO-ERROR.
              ELSE CREATE BUFFER hBuffer2 FOR TABLE TRIM(ENTRY(1,ENTRY(ia,icBuffersAndFields),";")) NO-ERROR.

              hField2 = hBuffer2:BUFFER-FIELD(hField:NAME) NO-ERROR.
              IF VALID-HANDLE(hField2) THEN 
                cExceptionFields[ia] = cExceptionFields[ia] + hField2:NAME + ",".
              IF VALID-HANDLE(hBuffer2) THEN DELETE OBJECT hBuffer2.
            END.
          END.
          IF CAN-DO(icStatFields,hField:NAME + cOrgFldNumExt) THEN DO:
            CREATE ttSpecFlds.
            ASSIGN ttSpecFlds.iCallBufferNum  = ix
                   ixf                        = ixf + 1
                   ttSpecFlds.iFieldNum       = ixf
                   ttSpecFlds.cFillField      = hField:NAME + cOrgFldNumExt
                   ttSpecFlds.cFillFieldType  = hField:DATA-TYPE
                   ttSpecFlds.hSourceField    = hField
                   ttSpecFlds.bGrandTotal     = TRUE
                   ttSpecFlds.bAccum          = bAccum
                   cStatBufferFields[ix]      = cStatBufferFields[ix] + hField:NAME + cOrgFldNumExt + ","
                   .
          END.
        END.
        /* The field name already exists in the temp-table buffer. Add a suffix to make it unique: */
        ELSE DO:
          /* Check for overrides of format and label also here: */
          IF NUM-ENTRIES(ENTRY(iy,cBufDef,";"),"|") > 1 THEN DO:
            DO iz = 2 TO NUM-ENTRIES(ENTRY(iy,cBufDef,";"),"|"):
              IF iz = 2 THEN DO:
                IF ENTRY(iz,ENTRY(iy,cBufDef,";"),"|") NE "=" THEN 
                  cOverrideLabel = ENTRY(iz,ENTRY(iy,cBufDef,";"),"|").
                ELSE 
                  cOverrideLabel = hField:LABEL.
              END.
              ELSE IF iz = 3 THEN
                cOverrideFormat = ENTRY(iz,ENTRY(iy,cBufDef,";"),"|").
            END.
            IF iz = 3 THEN
              cOverrideFormat = hField:FORMAT. /* No override on format */

            ohTempTable:ADD-NEW-FIELD(hField:NAME + STRING(ix),hField:DATA-TYPE,0,cOverrideFormat,
                                     IF hField:DATA-TYPE = "CHARACTER" THEN ""
                                     ELSE IF hField:DATA-TYPE = "DATE" THEN "?"
                                     ELSE IF hField:DATA-TYPE = "LOGICAL" THEN "no"
                                     ELSE "0",
                                     cOverrideLabel,
                                     IF cOverrideLabel MATCHES "*" + CHR(10) + "*" THEN
                                       REPLACE(cOverrideLabel,CHR(10),"!")
                                     ELSE ?).
          END.
          ELSE ohTempTable:ADD-LIKE-FIELD(hField:NAME + STRING(ix),hField).

          cDuplicateFields[ix] = cDuplicateFields[ix] + hField:NAME + ",".
          icQueryCriteria = REPLACE(icQueryCriteria,hField:NAME + STRING(ix),hField:NAME).

          IF CAN-DO(icStatFields,hField:NAME + STRING(ix)) THEN DO:
            CREATE ttSpecFlds.
            ASSIGN ttSpecFlds.iCallBufferNum  = ix
                   ixf                        = ixf + 1
                   ttSpecFlds.iFieldNum       = ixf
                   ttSpecFlds.cFillField      = hField:NAME + STRING(ix)
                   ttSpecFlds.cFillFieldType  = hField:DATA-TYPE
                   ttSpecFlds.hSourceField    = hField
                   ttSpecFlds.bGrandTotal     = TRUE
                   ttSpecFlds.bAccum          = bAccum
                   .
            cStatBufferFields[ix] = cStatBufferFields[ix] + hField:NAME + STRING(ix) + ",".
          END.

          /* If the distinct field is a duplicate field correct the name now: */
          IF bDistinct THEN
            ttSpecFlds.cFillField = hField:NAME + STRING(ix).
            
          /* If the field buffer was originally in another sequence make sure that its value is swapped to the correct column: 
          IF cOrgBuffersAndFields NE "" THEN
            setFieldBufferSwap(ENTRY(1,cBufDef,";"),ix,hField).   */
        END. /* Duplicate field */

      END. /* DB buffer field */
    END.
    cBufferFieldList = TRIM(cBufferFieldList,",").

    /* When we have a field list we don't want the other fields. Hence make an exception list for buffer-copy: */
    IF NOT bOnlyCalcFieldsDef THEN DO iq = 1 TO hBuffer[ix]:NUM-FIELDS:
      IF NOT CAN-DO(cBufferFieldList,hBuffer[ix]:BUFFER-FIELD(iq):NAME) AND
         NOT CAN-DO(cExceptionFields[ix],hBuffer[ix]:BUFFER-FIELD(iq):NAME) THEN
       cExceptionFields[ix] = cExceptionFields[ix] + hBuffer[ix]:BUFFER-FIELD(iq):NAME + ",".
    END.
  END. /* Field list for buffer */

  /* No field list is given and we grab all fields from the buffer and add to the temp-table
     Also builds the actual field list to be returned by cReturnOKmessage: */
  IF ((ix = 1 OR bFieldSpec = NO) AND NUM-ENTRIES(cBufDef,";") = 1) OR (bOnlyCalcFieldsDef AND NOT bFieldSpec) THEN DO:
    ASSIGN cRealFldSpc   = cBufDef + "|"
           cRealDbFldSpc = cBufDef + "|".
    DO iy = 1 TO hBuffer[ix]:NUM-FIELDS:
      hField = hBuffer[ix]:BUFFER-FIELD(iy) NO-ERROR.

      /* Goo: */
      cFieldName = hField:NAME.
      IF INDEX(cFieldName,"blob") > 0 OR INDEX(cFieldName,"clob") > 0 THEN NEXT.

      DO iz = 0 TO hField:EXTENT:
        IF hField:EXTENT > 0 AND iz = 0 THEN NEXT.

        IF hField:EXTENT > 0 THEN cFieldName = hField:NAME + "[" + STRING(iz) + "]".

        cRealDbFldSpc = cRealDbFldSpc + cFieldName + "|".
  
        IF hField:EXTENT = 0 THEN DO:
          IF NOT CAN-DO(cAddedFields,hField:NAME) THEN DO:
            ohTempTable:ADD-LIKE-FIELD(hField:NAME,hField).
            ASSIGN cAddedFields   = cAddedFields + hField:NAME + ","
                   cRealFldSpc = cRealFldSpc + hField:NAME + "|".
          END.
          ELSE DO:
            ohTempTable:ADD-LIKE-FIELD(hField:NAME + STRING(ix),hField).
            ASSIGN cDuplicateFields[ix] = cDuplicateFields[ix] + hField:NAME + ","
                   cRealFldSpc       = cRealFldSpc + hField:NAME + STRING(ix) + "|".
          END.
          IF CAN-DO(icStatFields,cFieldName) THEN DO:
            CREATE ttSpecFlds.
            ASSIGN ttSpecFlds.iCallBufferNum  = ix
                   ixf                             = ixf + 1
                   ttSpecFlds.iFieldNum       = ixf
                   ttSpecFlds.cFillField      = hField:NAME
                   ttSpecFlds.cFillFieldType  = hField:DATA-TYPE
                   ttSpecFlds.hSourceField    = hField
                   ttSpecFlds.bGrandTotal     = TRUE.
            cStatBufferFields[ix] = cStatBufferFields[ix] + hField:NAME + ",".
          END.
        END.
        ELSE DO:
          ixf = ixf + 1.
          ocReturn = CreateExtentCalcField(hBuffer[ix],ix,ixf,cFieldName,"",hField:FORMAT,
                                           hField:LABEL + "[" + STRING(iz) + "]",
                                           NO,NO).  
          IF ocReturn NE "" THEN RETURN.
          cRealFldSpc = cRealFldSpc + cFieldName + "|".
          IF CAN-DO(icStatFields,cFieldName) THEN 
            cCalcStatFields = cCalcStatFields + cFieldName + ",".
        END.  
      END.
    END.

    IF bReturnQueryInfo THEN
      cReturnOKmessage = cReturnOKmessage  
                       + RIGHT-TRIM(",bufferfieldspec;" + cRealFldSpc,"|")
                       + RIGHT-TRIM(",bufferdbfieldspec;" + cRealDbFldSpc,"|")
                       .
  END.

  cExceptionFields[ix] = TRIM(cDuplicateFields[ix] + cExceptionFields[ix],",").
  cDuplicateFields[ix] = TRIM(cDuplicateFields[ix],",").

  IF cOrgBufferList NE "" THEN DO:
    ohTempTable:ADD-NEW-FIELD("RowIdent" + STRING(LOOKUP(hBuffer[ix]:NAME,cOrgBufferList)),"CHARACTER").
    iOrgBufferNum[ix] = LOOKUP(hBuffer[ix]:NAME,cOrgBufferList).
  END.
  ELSE DO:
    ohTempTable:ADD-NEW-FIELD("RowIdent" + STRING(ix),"CHARACTER").
    iOrgBufferNum[ix] = ix.
  END.  

  hQuery:ADD-BUFFER(hBuffer[ix]).
  cBufferList = cBufferList + hBuffer[ix]:NAME + ",".
  IF ix = 1 THEN cPrimBufDbFlds = cAddedFields.
END.
FinalizeCreateQuery(cPrimBufDbFlds).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExtentFieldValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtentFieldValue Procedure 
PROCEDURE ExtentFieldValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihExtentField AS HANDLE NO-UNDO.
DEF INPUT  PARAM iiExtent      AS INT    NO-UNDO.
DEF OUTPUT PARAM ocValue       AS CHAR   NO-UNDO.

ocValue = STRING(ihExtentField:BUFFER-VALUE(iiExtent)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillExtent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillExtent Procedure 
PROCEDURE FillExtent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-JB_calc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_calc Procedure 
PROCEDURE JB_calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihField1 AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihField2 AS HANDLE NO-UNDO.
DEF INPUT  PARAM icOper   AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue  AS CHAR   NO-UNDO.

DEF VAR cDelimiter AS CHAR NO-UNDO.
DEF VAR cOper1     AS CHAR NO-UNDO.
DEF VAR cOper2     AS CHAR NO-UNDO.

CASE icOper:
  WHEN "*"   THEN ocValue = STRING(ihField1:BUFFER-VALUE * ihField2:BUFFER-VALUE) NO-ERROR.
  /* WHEN "%"   THEN ocValue = STRING(ihField1:BUFFER-VALUE * ihField2:BUFFER-VALUE / 100) NO-ERROR */
  WHEN "%"   THEN ocValue = STRING(ihField1:BUFFER-VALUE / ihField2:BUFFER-VALUE * 100) NO-ERROR.
  WHEN "/"   THEN ocValue = STRING(ihField1:BUFFER-VALUE / ihField2:BUFFER-VALUE) NO-ERROR. 
  WHEN "+"   THEN ocValue = STRING(ihField1:BUFFER-VALUE + ihField2:BUFFER-VALUE) NO-ERROR. 
  WHEN "-"   THEN ocValue = STRING(ihField1:BUFFER-VALUE - ihField2:BUFFER-VALUE) NO-ERROR. 
  WHEN "MOD" THEN ocValue = STRING(ihField1:BUFFER-VALUE MOD ihField2:BUFFER-VALUE) NO-ERROR. 
  OTHERWISE DO:
    IF icOper BEGINS "CONCAT" THEN DO:
      cDelimiter = SUBSTR(icOper,7).
      IF cDelimiter = "" THEN cDelimiter = " ".
      ELSE IF LENGTH(cDelimiter) = 2 THEN cDelimiter = TRIM(cDelimiter).
      ELSE IF cDelimiter = "comma" THEN cDelimiter = ", ".
      ocValue = STRING(ihField1:BUFFER-VALUE) + cDelimiter + STRING(ihField2:BUFFER-VALUE) NO-ERROR. 
    END.
    ELSE IF icOper BEGINS "COND" THEN DO:
      IF (CAN-DO("INTEGER,DECIMAL",ihField2:DATA-TYPE) AND ihField2:BUFFER-VALUE NE 0) 
         OR (ihField2:DATA-TYPE = "CHARACTER" AND ihField2:BUFFER-VALUE NE "")
         OR (ihField2:DATA-TYPE BEGINS "DATE" AND ihField2:BUFFER-VALUE NE ?) 
         OR (ihField2:DATA-TYPE = "LOGICAL" AND ihField2:BUFFER-VALUE) THEN
        ocValue = STRING(ihField2:BUFFER-VALUE).
      ELSE   
        ocValue = STRING(ihField1:BUFFER-VALUE).
    END.  
    ELSE IF icOper MATCHES "*TODAY*" AND ihField1:DATA-TYPE = "DATE" AND ihField2:DATA-TYPE = "DATE" THEN DO:
      ASSIGN cOper1 = ENTRY(1,icOper," ")
             cOper2 = ENTRY(3,icOper," ")
             ocValue = "no".
      IF CAN-DO("LE,<=",cOper1) AND ihField1:BUFFER-VALUE LE TODAY THEN ocValue = "yes".            
      IF CAN-DO("LT,<",cOper1)  AND ihField1:BUFFER-VALUE LT TODAY THEN ocValue = "yes".            
      IF CAN-DO("GE,>=",cOper2) AND TODAY <= ihField2:BUFFER-VALUE THEN ocValue = "yes". ELSE ocValue = "no".            
      IF CAN-DO("GT,>",cOper2)  AND TODAY < ihField2:BUFFER-VALUE THEN ocValue = "yes". ELSE ocValue = "no".            
/*     LogThis("Calc date int","icOper: " + icOper + " cOper1: " + cOper1 + " cOper2: " + cOper2 + " field1: " + ihField1:STRING-VALUE + " field2: " + ihField2:STRING-VALUE + " ocValue: " + ocValue,0).*/
      
    END.  
  END.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-JB_can-find) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_can-find Procedure 
PROCEDURE JB_can-find :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM iiBufferNum    AS INT NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.

DEF VAR ix      AS INT  NO-UNDO.
DEF VAR cModExp AS CHAR NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(ttSpecFlds.cCallProcParam,"¤") BY 2:
  IF ix < NUM-ENTRIES(ttSpecFlds.cCallProcParam,"¤") THEN
    cModExp = cModExp + ENTRY(ix,ttSpecFlds.cCallProcParam,"¤") + STRING(hBuffer[iiBufferNum]:BUFFER-FIELD(ENTRY(ix + 1,ttSpecFlds.cCallProcParam,"¤")):BUFFER-VALUE).
  ELSE 
    cModExp = cModExp + ENTRY(ix,ttSpecFlds.cCallProcParam,"¤").
END.

ttSpecFlds.hTargetBuff:FIND-FIRST(cModExp) NO-ERROR.
ocValue = STRING(ttSpecFlds.hTargetBuff:AVAILABLE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-JB_date_dt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_date_dt Procedure
PROCEDURE JB_date_dt:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM idDate      AS DATETIME NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

ocValue = STRING(DATE(idDate),"99/99/9999").


END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-JB_hhmm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_hhmm Procedure 
PROCEDURE JB_hhmm :
/*------------------------------------------------------------------------------
  Purpose:     Conversion of time to hh:mm time 
  Parameters:  <none>
  Notes:       Standard conversion
------------------------------------------------------------------------------*/
DEF INPUT  PARAM iiTime      AS INT  NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

ocValue = STRING(iiTime,"HH:MM").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-JB_hhmmss) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_hhmmss Procedure 
PROCEDURE JB_hhmmss :
/*------------------------------------------------------------------------------
  Purpose:     Conversion of time to hh:mm:ss time 
  Parameters:  <none>
  Notes:       Standard conversion
------------------------------------------------------------------------------*/
DEF INPUT  PARAM iiTime      AS INT  NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

ocValue = STRING(iiTime,"HH:MM:SS").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-JB_max) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_max Procedure
PROCEDURE JB_max:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFldArray AS HANDLE NO-UNDO EXTENT 12.
DEF INPUT PARAM iCntFlds   AS INT    NO-UNDO.
DEF OUTPUT PARAM ocReturn  AS CHAR   NO-UNDO.

CASE iCntFlds:
  WHEN 2  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE)
                                     ).
  WHEN 3  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE
                                     )).
  WHEN 4  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE
                                     )).
  WHEN 5  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE
                                     )).
  WHEN 6  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE
                                     )).
  WHEN 7  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE
                                     )).
  WHEN 8  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE,
                                     ihFldArray[8]:BUFFER-VALUE
                                     )).
  WHEN 9  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE,
                                     ihFldArray[8]:BUFFER-VALUE,
                                     ihFldArray[9]:BUFFER-VALUE
                                     )).
  WHEN 10 THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE,
                                     ihFldArray[8]:BUFFER-VALUE,
                                     ihFldArray[9]:BUFFER-VALUE,
                                     ihFldArray[10]:BUFFER-VALUE
                                     )).
  WHEN 11 THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE,
                                     ihFldArray[8]:BUFFER-VALUE,
                                     ihFldArray[9]:BUFFER-VALUE,
                                     ihFldArray[10]:BUFFER-VALUE,
                                     ihFldArray[11]:BUFFER-VALUE
                                     )).
  WHEN 12 THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE,
                                     ihFldArray[8]:BUFFER-VALUE,
                                     ihFldArray[9]:BUFFER-VALUE,
                                     ihFldArray[10]:BUFFER-VALUE,
                                     ihFldArray[11]:BUFFER-VALUE,
                                     ihFldArray[12]:BUFFER-VALUE
                                     )).
END.
    
END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-JB_min) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_min Procedure
PROCEDURE JB_min:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFldArray AS HANDLE NO-UNDO EXTENT 12.
DEF INPUT PARAM iCntFlds   AS INT    NO-UNDO.
DEF OUTPUT PARAM ocReturn  AS CHAR   NO-UNDO.

CASE iCntFlds:
  WHEN 2  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE)
                                     ).
  WHEN 3  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE
                                     )).
  WHEN 4  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE
                                     )).
  WHEN 5  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE
                                     )).
  WHEN 6  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE
                                     )).
  WHEN 7  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE
                                     )).
  WHEN 8  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE,
                                     ihFldArray[8]:BUFFER-VALUE
                                     )).
  WHEN 9  THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE,
                                     ihFldArray[8]:BUFFER-VALUE,
                                     ihFldArray[9]:BUFFER-VALUE
                                     )).
  WHEN 10 THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE,
                                     ihFldArray[8]:BUFFER-VALUE,
                                     ihFldArray[9]:BUFFER-VALUE,
                                     ihFldArray[10]:BUFFER-VALUE
                                     )).
  WHEN 11 THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE,
                                     ihFldArray[8]:BUFFER-VALUE,
                                     ihFldArray[9]:BUFFER-VALUE,
                                     ihFldArray[10]:BUFFER-VALUE,
                                     ihFldArray[11]:BUFFER-VALUE
                                     )).
  WHEN 12 THEN ocReturn = STRING(MAX(ihFldArray[1]:BUFFER-VALUE,
                                     ihFldArray[2]:BUFFER-VALUE,
                                     ihFldArray[3]:BUFFER-VALUE,
                                     ihFldArray[4]:BUFFER-VALUE,
                                     ihFldArray[5]:BUFFER-VALUE,
                                     ihFldArray[6]:BUFFER-VALUE,
                                     ihFldArray[7]:BUFFER-VALUE,
                                     ihFldArray[8]:BUFFER-VALUE,
                                     ihFldArray[9]:BUFFER-VALUE,
                                     ihFldArray[10]:BUFFER-VALUE,
                                     ihFldArray[11]:BUFFER-VALUE,
                                     ihFldArray[12]:BUFFER-VALUE
                                     )).
END.

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-JB_month) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_month Procedure 
PROCEDURE JB_month :
/*------------------------------------------------------------------------------
  Purpose:     Conversion of date to month 
  Parameters:  <none>
  Notes:       Standard conversion
------------------------------------------------------------------------------*/
DEF INPUT  PARAM idDate      AS DATE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

ocValue = STRING(YEAR(idDate)) + STRING(MONTH(idDate),"99").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-JB_month_dt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_month_dt Procedure
PROCEDURE JB_month_dt:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM idDate      AS DATETIME NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

ocValue = STRING(YEAR(idDate)) + STRING(MONTH(idDate),"99").


END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-JB_quarter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_quarter Procedure 
PROCEDURE JB_quarter :
/*------------------------------------------------------------------------------
  Purpose:     Conversion of date to quarter 
  Parameters:  <none>
  Notes:       Standard conversion
------------------------------------------------------------------------------*/
DEF INPUT  PARAM idDate      AS DATE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

DEF VAR iMonth AS INT NO-UNDO.

iMonth = MONTH(idDate).
IF iMonth < 4 THEN
  ocValue = STRING(YEAR(idDate)) + "Q1".
ELSE IF iMonth < 7 THEN
  ocValue = STRING(YEAR(idDate)) + "Q2".
ELSE IF iMonth < 10 THEN
  ocValue = STRING(YEAR(idDate)) + "Q3".
ELSE 
  ocValue = STRING(YEAR(idDate)) + "Q4".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-JB_quarter_ts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_quarter_ts Procedure
PROCEDURE JB_quarter_ts:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM idDate      AS DATETIME NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

DEF VAR iMonth AS INT NO-UNDO.

iMonth = MONTH(idDate).
IF iMonth < 4 THEN
  ocValue = STRING(YEAR(idDate)) + "Q1".
ELSE IF iMonth < 7 THEN
  ocValue = STRING(YEAR(idDate)) + "Q2".
ELSE IF iMonth < 10 THEN
  ocValue = STRING(YEAR(idDate)) + "Q3".
ELSE 
  ocValue = STRING(YEAR(idDate)) + "Q4".

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-JB_total) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_total Procedure 
PROCEDURE JB_total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM iiBufferNum    AS INT NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.

DEF VAR ix      AS INT  NO-UNDO.
DEF VAR cModExp AS CHAR NO-UNDO.
DEF VAR fCalc   AS DEC  NO-UNDO.
DEF VAR fTotal  AS DEC  NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(ttSpecFlds.cTargetQuery,"¤") BY 2:
  IF ix < NUM-ENTRIES(ttSpecFlds.cTargetQuery,"¤") THEN
    cModExp = cModExp + ENTRY(ix,ttSpecFlds.cTargetQuery,"¤") + STRING(hBuffer[iiBufferNum]:BUFFER-FIELD(ENTRY(ix + 1,ttSpecFlds.cTargetQuery,"¤")):BUFFER-VALUE).
  ELSE 
    cModExp = cModExp + ENTRY(ix,ttSpecFlds.cTargetQuery,"¤").
END.
ttSpecFlds.hTargetQuery:QUERY-PREPARE(cModExp) NO-ERROR.

IF NOT ERROR-STATUS:ERROR THEN DO:
  ttSpecFlds.hTargetQuery:QUERY-OPEN().
  ttSpecFlds.hTargetQuery:GET-FIRST().
  REPEAT WHILE NOT ttSpecFlds.hTargetQuery:QUERY-OFF-END:
    CASE ttSpecFlds.cCallProcParam:
      WHEN "*"     THEN fCalc = ttSpecFlds.hSourceField:BUFFER-VALUE * ttSpecFlds.hSourceField2:BUFFER-VALUE NO-ERROR.
      WHEN "/"     THEN fCalc = ttSpecFlds.hSourceField:BUFFER-VALUE / ttSpecFlds.hSourceField2:BUFFER-VALUE NO-ERROR. 
      WHEN "+"     THEN fCalc = ttSpecFlds.hSourceField:BUFFER-VALUE + ttSpecFlds.hSourceField2:BUFFER-VALUE NO-ERROR. 
      WHEN "-"     THEN fCalc = ttSpecFlds.hSourceField:BUFFER-VALUE - ttSpecFlds.hSourceField2:BUFFER-VALUE NO-ERROR. 
      WHEN "MOD"   THEN fCalc = ttSpecFlds.hSourceField:BUFFER-VALUE MOD ttSpecFlds.hSourceField2:BUFFER-VALUE NO-ERROR. 
      WHEN "COUNT" THEN fCalc = 1.
      OTHERWISE fCalc = ttSpecFlds.hSourceField:BUFFER-VALUE.
    END CASE.

    IF fCalc NE ? THEN fTotal = fTotal + fCalc.

    ttSpecFlds.hTargetQuery:GET-NEXT().
  END.
  ocValue = STRING(fTotal).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-JB_weeknum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_weeknum Procedure 
PROCEDURE JB_weeknum :
/*------------------------------------------------------------------------------
  Purpose:     Conversion of date to week 
  Parameters:  <none>
  Notes:       Standard conversion
------------------------------------------------------------------------------*/
DEF INPUT  PARAM inDate      AS DATE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

DEFINE VAR yyyyww   AS INT   NO-UNDO.   /* Output week, eg 9042     */

DEFINE VARIABLE yr   AS INT  NO-UNDO.  /* Year of indate, eg 1990      */
DEFINE VARIABLE d1   AS INT  NO-UNDO.  /* Weekday of 1/1 current year, eg 2  */
                              /* (01/01/90 is a Monday)      */
DEFINE VARIABLE dat1 AS DATE NO-UNDO. /* Starting date of week 1     */
DEFINE VARIABLE wn   AS INT  NO-UNDO.  /* Week number , eg 45         */

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

ocValue = STRING(yyyyww).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-JB_weeknum_dt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_weeknum_dt Procedure
PROCEDURE JB_weeknum_dt:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM inDate      AS DATETIME NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

RUN JB_weeknum (DATE(inDate),icSessionId,OUTPUT ocValue).

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-JB_year) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_year Procedure 
PROCEDURE JB_year :
/*------------------------------------------------------------------------------
  Purpose:     Conversion of date to year 
  Parameters:  <none>
  Notes:       Standard conversion
------------------------------------------------------------------------------*/
DEF INPUT  PARAM idDate      AS DATE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

ocValue = STRING(YEAR(idDate)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-JB_year_dt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_year_dt Procedure
PROCEDURE JB_year_dt:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM idDate      AS DATETIME NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

ocValue = STRING(YEAR(idDate)).


END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-PreScanAllRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PreScanAllRows Procedure 
PROCEDURE PreScanAllRows :
/*------------------------------------------------------------------------------
  Purpose:     Scan all query rows for distinct columns or sorted calculated fields
  Parameters:  <none>
  Notes:       When sorting on calculated values or retrieving distinct rows the whole result-set must be traversed first
               and the calculated sortfields and/or distinct rows are put in a temp-table that will be the primary table in ProcessQuery.
               Also other calculated fields and accumulated (i.e. accumulated pr distinct row) and grand totals are calculated here.
  Modified:    16.03.12: Added code to prevent adding to accumulation more than once pr table/field/rowid
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM ocReturn       AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK           AS LOG  NO-UNDO INIT TRUE.
                                
DEF VAR iCntDist          AS INT    NO-UNDO.
DEF VAR hDistFlds         AS HANDLE NO-UNDO EXTENT 20.
DEF VAR iNumDistFlds      AS INT    NO-UNDO.
DEF VAR cDistVal          AS CHAR   NO-UNDO.
DEF VAR hPreScanDistCol   AS HANDLE NO-UNDO.
DEF VAR hPreScanAddDistRowids AS HANDLE NO-UNDO EXTENT 20.
DEF VAR iUniquBuffer          AS INT    NO-UNDO.

DEF VAR iCntCalcFld    AS INT NO-UNDO.
DEF VAR bSimplePreScan AS LOG NO-UNDO.

bDistinctRows = CAN-FIND(FIRST ttSpecFlds WHERE ttSpecFlds.bDistinct).
iUniquBuffer = LOOKUP(cUniquBuffer,cBufferList).

CREATE TEMP-TABLE httPreScan.

/* Add all special fields that are not just grand totals to the prescan table
   (Values for grand totals are stored in ttSpecFlds): */
FOR EACH ttSpecFlds:
/*   IF NOT ttSpecFlds.bAccum AND ttSpecFlds.bGrandTotal AND ttSpecFlds.cCalcSortField = "" THEN NEXT.  */
  httPreScan:ADD-NEW-FIELD(ttSpecFlds.cFillField,ttSpecFlds.cFillFieldType).
END.
/* Since we travel through all rows we might as well replace all joins criteria with ROWID's (this is a must when doing distinct search): */
DO ix = 2 TO iBufferCount:
  httPreScan:ADD-NEW-FIELD("rRelPreScan" + STRING(ix),"ROWID").
END.
/* When sorting on a calculated column, index it (them) for optimal performance in ProcessQuery: */
IF CAN-FIND(FIRST ttSpecFlds WHERE ttSpecFlds.cCalcSortField NE "") THEN DO:
  httPreScan:ADD-NEW-INDEX("idxCalcFields").
  FOR EACH ttSpecFlds WHERE ttSpecFlds.cCalcSortField NE "":
    httPreScan:ADD-INDEX-FIELD("idxCalcFields",ttSpecFlds.cFillField).
  END.
END.

httPreScan:ADD-NEW-FIELD("rRelPreScan","ROWID").         /* Rowid for first buffer in query - to be able to join to the PreScan table */
httPreScan:ADD-NEW-FIELD("cDistVal","CHARACTER").  
httPreScan:ADD-NEW-INDEX("idxDistinct").
httPreScan:ADD-INDEX-FIELD("idxDistinct","cDistVal").
httPreScan:TEMP-TABLE-PREPARE("ttPreScan") NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = ERROR-STATUS:GET-MESSAGE(1) + CHR(10) + "Error in definition of PreScan table".
  RETURN.
END.
hBuffPreScan = httPreScan:DEFAULT-BUFFER-HANDLE.

/* Get direct access to all fields in the prescan table (performance gain when chained attribs are avoided): */
ASSIGN hPreScanBuffer        = httPreScan:DEFAULT-BUFFER-HANDLE
       hPreScanDistCol   = hPreScanBuffer:BUFFER-FIELD("cDistVal")
       hPreScanCountDistinct = hPreScanBuffer:BUFFER-FIELD("jbCountDistinct")
       hPreScanRowIdCol      = hPreScanBuffer:BUFFER-FIELD("rRelPreScan").
FOR EACH ttSpecFlds,
    FIRST ttRowSpecVal OF ttSpecFlds:
  IF NOT ttSpecFlds.bAccum AND ttSpecFlds.bGrandTotal AND ttSpecFlds.cCalcSortField = "" THEN 
    ttRowSpecVal.bOnlyGrandTotal = TRUE.
    
  ttRowSpecVal.hPreScanField = hPreScanBuffer:BUFFER-FIELD(ttRowSpecVal.cFillField).

  IF NOT VALID-HANDLE(hPreScanAccumField) AND ttSpecFlds.bAccum AND ttSpecFlds.bGrandTotal THEN
    hPreScanAccumField = ttRowSpecVal.hPreScanField.
END.
DO ix = 2 TO iBufferCount:
  hPreScanAddDistRowids[ix]  = hPreScanBuffer:BUFFER-FIELD("rRelPreScan" + STRING(ix)).
END.

/* Now open the query without sorting to gain speed: */
IF INDEX(cQueryString," BY ") > 0 THEN
  cQueryString = SUBSTR(cQueryString,1,INDEX(cQueryString," BY ")).

LogThis("QueryString, all rows: ",cQueryString,1).

ocReturn = QueryOpen().
IF ocReturn NE "" THEN DO:
  obOk = FALSE.
  RETURN.
END.

/* Gather the handles to the database buffer-fields for distinct columns: */
FOR EACH ttSpecFlds
    WHERE ttSpecFlds.cCallProc = ""
      AND ttSpecFlds.bDistinct:
  iNumDistFlds = iNumDistFlds + 1.
  hDistFlds[iNumDistFlds] = ttSpecFlds.hSourceField.
END.
/* Mark special fields and values calculated here as calculated by prescan */
FOR EACH ttSpecFlds
    WHERE NOT ttSpecFlds.b2phAcc
      and (ttSpecFlds.bDistinct OR ttSpecFlds.cCalcSortField NE "" OR ttSpecFlds.bGrandTotal 
           OR ttSpecFlds.cFilterOperator NE "" OR ttSpecFlds.cFillField = "jbCountDistinct" OR bDistinctRows 
           OR bCheckForSkipRow) /* 13.nov.14 */
    ,first ttRowSpecVal of ttSpecFlds
    :
  ASSIGN ttSpecFlds.bCalcByPreScan    = YES 
         ttRowSpecVal.bCalcByPreScan = YES 
         .
         
  IF ttSpecFlds.cFillField NE "jbCountDistinct" THEN 
    iCntCalcFld = iCntCalcFld + 1. 
end.   

IF cLogFile NE "" AND CAN-FIND(FIRST ttSpecFlds WHERE ttSpecFlds.bCalcByPreScan) THEN DO:
  LogThis("Calculations done during prescan:","",0).
  FOR EACH ttSpecFlds
      WHERE ttSpecFlds.bCalcByPreScan:
    LogThis(ttSpecFlds.cFillField, 
          ttSpecFlds.cCallProc + "(" 
        + ttSpecFlds.cCallProcParam + ")" 
        + " Distinct: " + STRING(ttSpecFlds.bDistinct)
        + " CalcSortField NE '': " + STRING(ttSpecFlds.cCalcSortField NE "")
        + " GrandTotal: " + STRING(ttSpecFlds.bGrandTotal)
        + " cFilterOperator: " + ttSpecFlds.cFilterOperator
            ,0). 
  END.
  LogThis("","",1).
END.

/************ 10.10.11: */
IF iCntCalcFld = 1 AND NOT bDistinctRows THEN DO:
  bSimplePreScan = yes.
  FIND FIRST ttSpecFlds
       WHERE ttSpecFlds.bCalcByPreScan
         AND ttSpecFlds.cFillField NE "jbCountDistinct"
       NO-ERROR.
  IF NOT AVAIL ttSpecFlds THEN DO:
    LogThis("Error in simple prescan: ","Couldn't find prescan field",1).
    RETURN.      
  END.
  FIND FIRST ttRowSpecVal OF ttSpecFlds.
  
  LogThis("Simple prescan: ",ttSpecFlds.cFillField,1).
END.       
/* 10.10.11 ************/       


hQuery:GET-FIRST().

PRESCANLOOP:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  /************ 10.10.11 No looping through query buffers ttSpecFlds when only one calculated field and no accumulation */
  IF bSimplePreScan THEN DO:
    IF hBuffer[ttSpecFlds.iCallBufferNum]:AVAIL THEN do:
    
      /* Either the calculated values: */
      IF ttSpecFlds.cCallProc NE "" THEN DO:
        IF VALID-HANDLE(ttSpecFlds.hCalcFieldProc) THEN DO:
          IF ttSpecFlds.iExtent > 0 THEN
            RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc
               (ttSpecFlds.hSourceField,ttSpecFlds.iExtent, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
          ELSE IF ttSpecFlds.cCallProc = "jb_calc" THEN 
            RUN JB_calc (ttSpecFlds.hSourceField,ttSpecFlds.hSourceField2,ttSpecFlds.cCallProcParam,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
          ELSE IF ttSpecFlds.cCallProc = "jb_can-find" THEN 
            RUN JB_can-find (ttSpecFlds.iCallBufferNum,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
          ELSE IF ttSpecFlds.cCallProc = "jb_total" THEN 
            RUN JB_total (ttSpecFlds.iCallBufferNum,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
          ELSE IF ttSpecFlds.cCallProc = "jb_max" THEN 
            RUN JB_max (ttSpecFlds.hCompareFields,ttSpecFlds.iCntCompareFields,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
          ELSE IF ttSpecFlds.cCallProcParam BEGINS "ROWID" THEN
            RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc
               (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,SUBSTR(ttSpecFlds.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
          ELSE IF ttSpecFlds.cCallProcParam NE "" THEN
            RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc
                (ttSpecFlds.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
          ELSE
            RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc
                (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
        END.
        ELSE DO:
          IF ttSpecFlds.cCallProcParam BEGINS "ROWID" THEN
            RUN VALUE(ttSpecFlds.cCallProc)
                (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,SUBSTR(ttSpecFlds.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
          ELSE IF ttSpecFlds.cCallProcParam NE "" THEN
            RUN VALUE(ttSpecFlds.cCallProc)
                (ttSpecFlds.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
          ELSE
            RUN VALUE(ttSpecFlds.cCallProc)
                (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
        END.
  
        IF ttRowSpecVal.cCurrCallOutput = "skiprow" OR (bCalcFieldFilter AND NOT CalcValueOk(ttRowSpecVal.cCurrCallOutput)) THEN DO:
          hQuery:GET-NEXT().
          NEXT PRESCANLOOP.
        END.
        ELSE IF ttSpecFlds.bDistinct AND ttRowSpecVal.cCurrCallOutput NE ? THEN
          cDistVal = cDistVal + ttRowSpecVal.cCurrCallOutput.
        ELSE IF ttSpecFlds.bGrandTotal THEN DO: /* That includes also the accum fields */
          ttRowSpecVal.fRowValue = DEC(ttRowSpecVal.cCurrCallOutput) NO-ERROR.
          IF ERROR-STATU:ERROR THEN DO:
            ocReturn = "Server calculation routine " + ttSpecFlds.cCallProc + " returns invalid value for accumulation: " + ttRowSpecVal.cCurrCallOutput.
            RETURN.
          END.
        END.
      END. /* Calculated values */
      /* Or accum or GrandTotal fields from the database buffer: */
      ELSE IF ttSpecFlds.bGrandTotal THEN DO:
        ttRowSpecVal.fRowValue = DEC(ttSpecFlds.hSourceField:BUFFER-VALUE) NO-ERROR.
        IF ERROR-STATU:ERROR THEN DO:
          ocReturn = "Accumulation of field " + ttSpecFlds.cFillField + " on assignment due to a mismatch in datatype (only numbers can be accumulated)".
          RETURN.
        END.
      END.
  
    END.
    ELSE ttRowSpecVal.fRowValue = 0.
    
  END.
  /* 10.10.11 ************/
  
  ELSE DO:  
    cDistVal = "".
    DO iy = 1 TO iBufferCount:
      IF hBuffer[iy]:AVAIL THEN
  
        /* Go get values for calculated and/or accumulated and GrandTotal fields for current buffer: */
        FOR EACH ttSpecFlds
            WHERE ttSpecFlds.iCallBufferNum = iy
              AND ttSpecFlds.bCalcByPrescan
               BY ttSpecFlds.iFieldNum:
  
          FIND FIRST ttRowSpecVal OF ttSpecFlds.
          ttRowSpecVal.fRowValue = 0.

          IF ttSpecFlds.bGrandTotal AND iy NE iUniquBuffer THEN DO:
            FIND FIRST ttAccumProcessed 
                 WHERE ttAccumProcessed.iCallBufferNum = iy
                   AND ttAccumProcessed.iFieldNum = ttSpecFlds.iFieldNum
                   AND ttAccumProcessed.rRowid = hBuffer[ttSpecFlds.iCallBufferNum]:ROWID
                 NO-ERROR.
            IF AVAIL ttAccumProcessed THEN NEXT.
            CREATE ttAccumProcessed.
            ASSIGN ttAccumProcessed.iCallBufferNum = iy
                   ttAccumProcessed.iFieldNum = ttSpecFlds.iFieldNum
                   ttAccumProcessed.rRowid = hBuffer[ttSpecFlds.iCallBufferNum]:ROWID
                   .
          END.

          /* Either the calculated values: */
          IF ttSpecFlds.cCallProc NE "" THEN DO:
            IF VALID-HANDLE(ttSpecFlds.hCalcFieldProc) THEN DO:
              IF ttSpecFlds.iExtent > 0 THEN
                RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc
                   (ttSpecFlds.hSourceField,ttSpecFlds.iExtent, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProc = "jb_calc" THEN 
                RUN JB_calc (ttSpecFlds.hSourceField,ttSpecFlds.hSourceField2,ttSpecFlds.cCallProcParam,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProc = "jb_can-find" THEN 
                RUN JB_can-find (ttSpecFlds.iCallBufferNum,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProc = "jb_total" THEN 
                RUN JB_total (ttSpecFlds.iCallBufferNum,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProc = "jb_max" THEN 
                RUN JB_max (ttSpecFlds.hCompareFields,ttSpecFlds.iCntCompareFields,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProcParam BEGINS "ROWID" THEN
                RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc
                   (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,SUBSTR(ttSpecFlds.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProcParam NE "" THEN
                RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc
                    (ttSpecFlds.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE
                RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc
                    (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              IF ttSpecFlds.bDirectAssign THEN
                AssignStringValue(ttRowSpecVal.hFillField,ttRowSpecVal.cCurrCallOutput).
            END.
            ELSE DO:
              IF ttSpecFlds.cCallProcParam BEGINS "ROWID" THEN
                RUN VALUE(ttSpecFlds.cCallProc)
                    (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,SUBSTR(ttSpecFlds.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
              ELSE IF ttSpecFlds.cCallProcParam NE "" THEN
                RUN VALUE(ttSpecFlds.cCallProc)
                    (ttSpecFlds.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
              ELSE
                RUN VALUE(ttSpecFlds.cCallProc)
                    (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
            END.
  
            IF ttRowSpecVal.cCurrCallOutput = "skiprow" OR (bCalcFieldFilter AND NOT CalcValueOk(ttRowSpecVal.cCurrCallOutput)) THEN DO:
              hQuery:GET-NEXT().
              NEXT PRESCANLOOP.
            END.
            ELSE IF ttSpecFlds.bDistinct AND ttRowSpecVal.cCurrCallOutput NE ? THEN
              cDistVal = cDistVal + ttRowSpecVal.cCurrCallOutput.
            ELSE IF ttSpecFlds.bGrandTotal THEN DO: /* That includes also the accum fields */
              ttRowSpecVal.fRowValue = DEC(ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              IF ERROR-STATU:ERROR THEN DO:
                ocReturn = "Server calculation routine " + ttSpecFlds.cCallProc + " returns invalid value for accumulation: " + ttRowSpecVal.cCurrCallOutput.
                RETURN.
              END.
            END.
          END. /* Calculated values */
          /* Or accum or GrandTotal fields from the database buffer: */
          ELSE IF ttSpecFlds.bGrandTotal THEN DO:
            ttRowSpecVal.fRowValue = DEC(ttSpecFlds.hSourceField:BUFFER-VALUE) NO-ERROR.
            IF ERROR-STATU:ERROR THEN DO:
              ocReturn = "Accumulation of field " + ttSpecFlds.cFillField + " on assignment due to a mismatch in datatype (only numbers can be accumulated)".
              RETURN.
            END.
          END.
        END.
      
      ELSE 
        FOR EACH ttRowSpecVal WHERE ttRowSpecVal.iCallBufferNum = iy:
          ttRowSpecVal.fRowValue = 0.
        END.
    END.    
  END. 
  
  bOk = FALSE.
  IF bDistinctRows THEN DO:
    DO ix = 1 TO iNumDistFlds:
      IF hDistFlds[ix]:BUFFER-VALUE NE ? THEN
        cDistVal = cDistVal + STRING(hDistFlds[ix]:BUFFER-VALUE).
    END.
    IF cDistVal = ? THEN DO:
      ocReturn = "Warning: A distinct column has unknown value. Values for the row were not added to totals".
      hQuery:GET-NEXT().
      NEXT PRESCANLOOP.
    END.
    bOk = hBuffPreScan:FIND-FIRST("WHERE cDistVal = " + QUOTER(cDistVal)) NO-ERROR.
    IF bOk THEN hPreScanCountDistinct:BUFFER-VALUE = hPreScanCountDistinct:BUFFER-VALUE + 1.
  END.
  IF NOT bOK THEN DO:   /* If distinct value doesn't exist or there is no check on distinct */
    hBuffPreScan:BUFFER-CREATE().
    ASSIGN hPreScanRowIdCol:BUFFER-VALUE      = hBuffer[1]:ROWID
           hPreScanDistCol:BUFFER-VALUE   = cDistVal
           hPreScanCountDistinct:BUFFER-VALUE = 1
           iCntDist                     = iCntDist + 1.
    DO ix = 2 TO iBufferCount:
      IF hBuffer[ix]:AVAIL THEN
        ASSIGN hPreScanAddDistRowids[ix]:BUFFER-VALUE = hBuffer[ix]:ROWID.
    END.
  END.

  /******/
  /* Now we are sure that the row should be included and we can move the corresponding values: */
  IF bSimplePreScan THEN DO:
    IF ttRowSpecVal.cCallProc NE "" THEN
      AssignStringValue(ttRowSpecVal.hPreScanField,ttRowSpecVal.cCurrCallOutput).
    ELSE ttRowSpecVal.hPreScanField:BUFFER-VALUE = ttRowSpecVal.hSourceField:BUFFER-VALUE.

    IF ttRowSpecVal.fRowValue NE ? THEN
      ttRowSpecVal.fGrandTotal = ttRowSpecVal.fGrandTotal + ttRowSpecVal.fRowValue.
  END.
  ELSE DO:
    FOR EACH ttRowSpecVal
        WHERE ttRowSpecVal.bCalcByPrescan: 
  /*      WHERE NOT ttRowSpecVal.b2phAcc: 10.10.11  */
      IF ttRowSpecVal.bAccum THEN
        ttRowSpecVal.hPreScanField:BUFFER-VALUE = ttRowSpecVal.hPreScanField:BUFFER-VALUE + ttRowSpecVal.fRowValue.
      ELSE IF ttRowSpecVal.cCallProc NE "" THEN
        AssignStringValue(ttRowSpecVal.hPreScanField,ttRowSpecVal.cCurrCallOutput).
      ELSE ttRowSpecVal.hPreScanField:BUFFER-VALUE = ttRowSpecVal.hSourceField:BUFFER-VALUE.
  
      IF ttRowSpecVal.fRowValue NE ? THEN
        ttRowSpecVal.fGrandTotal = ttRowSpecVal.fGrandTotal + ttRowSpecVal.fRowValue.
    END.
  end.
  /*******/

  
  iCount = iCount + 1.  
  IF iCount > 10000000 THEN LEAVE.

  hQuery:GET-NEXT().
END.

/* The query must be prepared again - this time with the temp-table containing the calculated values as the primary: */
DELETE OBJECT hQuery.
CREATE QUERY hQuery.
hQuery:ADD-BUFFER(hBuffPreScan).
DO ix = 1 TO iBufferCount:
  IF VALID-HANDLE(hBuffer[ix]) THEN
    hQuery:ADD-BUFFER(hBuffer[ix]).
  ELSE LEAVE.
END.
cQueryString = "FOR EACH ttPreScan,EACH " + ENTRY(1,ENTRY(1,icBuffersAndFields),";") + " NO-LOCK " 
              + "WHERE ROWID(" + ENTRY(1,ENTRY(1,icBuffersAndFields),";") + ") = ttPreScan.rRelPreScan".

DO ix = 2 TO iBufferCount:
  cQueryString = cQueryString + ",EACH " 
                 + ENTRY(ix,cBufferList) + " NO-LOCK " 
                 + "WHERE ROWID(" + ENTRY(ix,cBufferList) + ") = ttPreScan.rRelPreScan" + STRING(ix)
                 + (IF CAN-FIND(FIRST ttQuery WHERE ttQuery.iQueryNum = 0 AND ttQuery.iQryBuffer = ix AND ttQuery.bOuterJoin) THEN
                     " OUTER-JOIN" 
                    ELSE "")
                   .
END.
cQueryString = cQueryString + cSortPhrase.

IF iCount > 10000000 THEN
  ocReturn = "Warning: 10 million records is the limit for preparing a sort on a calculated column or get distinct values. This limit was reached..".

IF bDistinctRows THEN
  iQueryCount = iCntDist.
ELSE DO:
  IF iCount > 0 THEN iCount = iCount + 1.
  iQueryCount = iCount.
END.  
      
LogThis("iCntDist",STRING(iCntDist),0).
LogThis("PreScanCount",STRING(iCount),0).
LogThis("Etime, prescan all rows: ",STRING(ETIME / 1000),2).


/* DebugPreScan().  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessQuery Procedure 
PROCEDURE ProcessQuery :
/*------------------------------------------------------------------------------
  Purpose:     Run the query and populate output temp-table
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

DEF VAR bPreScanDidAllCalc AS LOG NO-UNDO.
DEF VAR b2phAcc            AS LOG NO-UNDO.
DEF VAR bPreScanAll        AS LOG NO-UNDO.

DEF VAR cCompanyTag        AS CHAR NO-UNDO.
DEF VAR cCompanyQuery      AS CHAR NO-UNDO.
DEF VAR cCompZeroQuery     AS CHAR NO-UNDO.
DEF VAR cCodeMasterQuery   AS CHAR NO-UNDO.

IF icQueryCriteria MATCHES "*(*Company)" OR icQueryCriteria MATCHES "*(Codemaster)" THEN DO:
  ASSIGN cCompanyTag     = SUBSTR(icQueryCriteria,R-INDEX(icQueryCriteria,"("))
         icQueryCriteria = SUBSTR(icQueryCriteria,1,R-INDEX(icQueryCriteria,"(") - 1)
         icQueryCriteria = TRIM(icQueryCriteria)
         icQueryCriteria = SUBSTR(icQueryCriteria,INDEX(icQueryCriteria, " "))
         .
  IF cCompanyTag = "(+Company)" THEN
    cCompanyQuery = "WHERE (iJBoxCompanyId = 0 OR iJBoxCompanyId = " + STRING(iCurrCompanyId) + ") AND".
  ELSE
    ASSIGN cCompanyQuery     = "WHERE iJBoxCompanyId = " + STRING(iCurrCompanyId) + " AND"
           cCodeMasterQuery  = "WHERE iJBoxCompanyId = " + STRING(iCodeMaster) + " AND"
           cCompZeroQuery    = "WHERE iJBoxCompanyId = 0 AND".

  LogThis("CompanyQuery",cCompanyQuery,0).
  LogThis("CodeMasterQuery",cCodeMasterQuery,0).
  LogThis("CompanyZeroQuery",cCompZeroQuery,1).
END.

IF icQueryCriteria BEGINS "WHERE false" THEN
  icQueryCriteria = GetQueryFalseCrit() + SUBSTR(icQueryCriteria,12).
/* ELSE IF SEARCH("jbserv_getbuffer_rowaccess_list.p") NE ? OR SEARCH("jbserv_getbuffer_rowaccess_list.r") NE ? THEN                                      */
/*   RUN jbserv_getbuffer_rowaccess_list.p(cBufferList,cCurrUserId,OUTPUT cBufferAccessCheckList,OUTPUT cBufferAccessExprList,OUTPUT hBufferAccessCheck). */

IF cPreScanJoinQueries NE "" THEN DO:
  IF cCompanyTag NE "" THEN DO:
    ocReturn = "Automatic filtering on company not supported for pre-scan query types".
    RETURN.
  END.
  DecomposeQuery(0,icQueryCriteria,cBufferList).
  ocReturn = ProcessPreScanJoins().
  IF ocReturn NE "" THEN RETURN.
END.
ELSE
  cQueryString = "FOR EACH " + ENTRY(1,cBufferList) + " NO-LOCK " + cCompanyQuery + icQueryCriteria.

/* Now - if the query is sorted on calculated fields or has distinct columns 
   we must perfor an extra step
   where we run throug the whole set first: */
IF CAN-FIND(FIRST ttSpecFlds WHERE ttSpecFlds.bDistinct OR ttSpecFlds.cCalcSortField NE "") THEN DO:
  IF cCompanyTag NE "" THEN DO:
    ocReturn = "Automatic filtering on company not supported for accumulation or sorting on calculated fields".
    RETURN.
  END.

  IF cPreScanJoinQueries = "" THEN DecomposeQuery(0,icQueryCriteria,cBufferList).
  RUN PreScanAllRows (OUTPUT ocReturn,OUTPUT bOk).
  IF NOT bOK THEN RETURN.  
  bPreScanAll = yes.
  IF CAN-FIND(FIRST ttSpecFlds WHERE ttSpecFlds.b2phAcc) THEN
    b2phAcc = YES.
  IF NOT CAN-FIND(FIRST ttSpecFlds WHERE NOT ttSpecFlds.bCalcByPreScan) THEN
    bPreScanDidAllCalc = YES.
END. 
ELSE  /* When no prescan the target for calculated field values are in the output temp-table: */
  FOR EACH ttSpecFlds,
      FIRST ttRowSpecVal OF ttSpecFlds:
    IF ttSpecFlds.bGrandTotal THEN NEXT.
    ttRowSpecVal.hFillField = httBuffer:BUFFER-FIELD(ttRowSpecVal.cFillField).
  END.

LogThis("cQueryString",cQueryString,1).
IF icStatFields NE "" THEN 
  LogThis("Run calc.fld rowcount",STRING(NOT bPreScanDidAllCalc AND (icStatFields NE "rowcount" OR bCheckForSkipRow OR bCalcFieldFilter OR cAccumFields NE "")),1).

ocReturn = QueryOpen().
IF ocReturn NE "" THEN RETURN.

iCount = 1.
IF icDirection = "" THEN
  hQuery:GET-FIRST().
ELSE 
  hQuery:GET-LAST().


IF NOT hQuery:GET-BUFFER-HANDLE(1):AVAIL AND cCompanyTag = "(Codemaster)" AND iCodeMaster NE iCurrCompanyId THEN DO:
  cQueryString = "FOR EACH " + ENTRY(1,cBufferList) + " NO-LOCK " + cCodeMasterQuery + icQueryCriteria.
  ocReturn = QueryOpen().
  IF ocReturn NE "" THEN RETURN.
  IF icDirection = "" THEN
    hQuery:GET-FIRST().
  ELSE 
    hQuery:GET-LAST().
END.

IF NOT hQuery:GET-BUFFER-HANDLE(1):AVAIL AND (cCompanyTag = "(Company)" OR cCompanyTag = "(Codemaster)") THEN DO:
  cQueryString = "FOR EACH " + ENTRY(1,cBufferList) + " NO-LOCK " + cCompZeroQuery + icQueryCriteria.
  ocReturn = QueryOpen().
  IF ocReturn NE "" THEN RETURN.
  IF icDirection = "" THEN
    hQuery:GET-FIRST().
  ELSE 
    hQuery:GET-LAST().
END.

IF iiBatchSize = 0 THEN iiBatchSize = 100000.
ELSE IF bDistinctRows AND iiBatchSize < 3000 AND iQueryCount < 3000 THEN 
  iiBatchSize = 3000.

IF icStatFields NE "" AND NOT icStatFields BEGINS "rowcount" THEN
  icStatFields = "rowcount," + icStatFields.

QUERYLOOP:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  IF iCount GT iiStartRow AND iCount - iiStartRow LE iiBatchSize THEN DO:
    httBuffer:BUFFER-CREATE.
    IF NOT bPreScanDidAllCalc THEN DO iy = 1 TO iBufferCount:
/*     IF NOT bPreScanDidAllCalc OR b2phAcc THEN DO iy = 1 TO iBufferCount: */
      IF hBuffer[iy]:AVAIL THEN DO:
        IF cBufferAccessCheckList NE "" AND NOT UserRowAccess(hBuffer[iy]) THEN DO:
          httBuffer:BUFFER-DELETE.
          IF icDirection = "" THEN
            hQuery:GET-NEXT().
          ELSE 
            hQuery:GET-PREV().
          NEXT QUERYLOOP.
        END.
        /* Go get values for calculated and/or accumulated and GrandTotal fields for current buffer: */
        FOR EACH ttSpecFlds
            WHERE ttSpecFlds.iCallBufferNum = iy
              AND NOT ttSpecFlds.bCalcByPreScan
               BY ttSpecFlds.iFieldNum:

          FIND FIRST ttRowSpecVal OF ttSpecFlds.

          /* Either the calculated values: */
          IF ttSpecFlds.cCallProc NE "" THEN DO:
            IF VALID-HANDLE(ttSpecFlds.hCalcFieldProc) THEN DO:
              IF ttSpecFlds.iExtent > 0 THEN
                RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc
                   (ttSpecFlds.hSourceField,ttSpecFlds.iExtent, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProc = "jb_calc" THEN 
                RUN JB_calc (ttSpecFlds.hSourceField,ttSpecFlds.hSourceField2,ttSpecFlds.cCallProcParam,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProc = "jb_can-find" THEN 
                RUN JB_can-find (ttSpecFlds.iCallBufferNum,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProc = "jb_total" THEN 
                RUN JB_total (ttSpecFlds.iCallBufferNum,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProc = "jb_max" THEN 
                RUN JB_max (ttSpecFlds.hCompareFields,ttSpecFlds.iCntCompareFields,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProcParam BEGINS "ROWID" THEN 
                RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc 
                   (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,SUBSTR(ttSpecFlds.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecFlds.cCallProcParam NE "" THEN 
                RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc 
                    (ttSpecFlds.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              ELSE 
                RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc 
                    (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              IF ttSpecFlds.bDirectAssign THEN
                AssignStringValue(ttRowSpecVal.hFillField,ttRowSpecVal.cCurrCallOutput).
            END.
            ELSE DO:
              IF ttSpecFlds.cCallProcParam BEGINS "ROWID" THEN
                RUN VALUE(ttSpecFlds.cCallProc) 
                    (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,SUBSTR(ttSpecFlds.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
              ELSE IF ttSpecFlds.cCallProcParam NE "" THEN
                RUN VALUE(ttSpecFlds.cCallProc) 
                    (ttSpecFlds.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
              ELSE 
                RUN VALUE(ttSpecFlds.cCallProc) 
                    (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
            END.

            IF ttRowSpecVal.cCurrCallOutput = "skiprow" OR (bCalcFieldFilter AND NOT CalcValueOk(ttRowSpecVal.cCurrCallOutput)) THEN DO:
              /* If any accumulations were done by prescan they need to be adjusted since the row should not be included: */
              FOR EACH ttRowSpecVal where ttRowSpecVal.bCalcByPreScan
                  AND ttRowSpecVal.bAccum:
                ttRowSpecVal.fGrandTotal = ttRowSpecVal.fGrandTotal - ttRowSpecVal.fRowValue.
              END.
              httBuffer:BUFFER-DELETE.
              IF icDirection = "" THEN
                hQuery:GET-NEXT().
              ELSE 
                hQuery:GET-PREV().
              NEXT QUERYLOOP.
            END.
            ELSE IF ttSpecFlds.bGrandTotal THEN DO: 
              ttRowSpecVal.fRowValue = DEC(ttRowSpecVal.cCurrCallOutput) NO-ERROR.
              IF ERROR-STATU:ERROR THEN DO:
                ocReturn = "Server calculation routine " + ttSpecFlds.cCallProc + " returns invalid value for accumulation: " + ttRowSpecVal.cCurrCallOutput.
                LogThis(ocReturn,"",1).
                RETURN.
              END.
            END.
          END. /* Calculated values */
          /* Or GrandTotal fields from the database buffer: */
          ELSE IF ttSpecFlds.bGrandTotal THEN DO:
            ttRowSpecVal.fRowValue = DEC(ttSpecFlds.hSourceField:BUFFER-VALUE) NO-ERROR.
            IF ERROR-STATU:ERROR THEN DO:
              ocReturn = "Accumulation of field " + ttSpecFlds.cFillField + " on assignment due to a mismatch in datatype (only numbers can be accumulated)".
              LogThis(ocReturn,"",1).
              RETURN.
            END.
          END.
        END.
      END.
      ELSE FOR EACH ttRowSpecVal WHERE ttRowSpecVal.iCallBufferNum = iy:
        ASSIGN ttRowSpecVal.fRowValue = 0
               ttRowSpecVal.cCurrCallOutput = "".
      END.
    END.

    httBuffer:BUFFER-FIELD("RowCount"):BUFFER-VALUE = iCount.
    DO iy = 1 TO iBufferCount:
      IF hBuffer[iy]:AVAIL THEN DO:
        IF cFieldPairsList[iy] NE "" THEN
          httBuffer:BUFFER-COPY(hBuffer[iy],cExceptionFields[iy],cFieldPairsList[iy]).
        ELSE DO:
          httBuffer:BUFFER-COPY(hBuffer[iy],cExceptionFields[iy]).
          DO iz = 1 TO NUM-ENTRIES(cDuplicateFields[iy]):
            httBuffer:BUFFER-FIELD(ENTRY(iz,cDuplicateFields[iy]) + STRING(iy)):BUFFER-VALUE = hBuffer[iy]:BUFFER-FIELD(ENTRY(iz,cDuplicateFields[iy])):BUFFER-VALUE.
          END.
        END.  
        httBuffer:BUFFER-FIELD("RowIdent" + STRING(iOrgBufferNum[iy])):BUFFER-VALUE = STRING(hBuffer[iy]:ROWID).
      END.
    END.

    /* Now we are sure that the row should be included and we can move the corresponding values: */
/*     IF NOT bPreScanDidAllCalc THEN */
      FOR EACH ttRowSpecVal WHERE not ttRowSpecVal.bCalcByPreScan:
        IF ttRowSpecVal.cCallProc NE "" THEN
          AssignStringValue(ttRowSpecVal.hFillField,ttRowSpecVal.cCurrCallOutput).
        IF ttRowSpecVal.fRowValue NE ? THEN
          ttRowSpecVal.fGrandTotal = ttRowSpecVal.fGrandTotal + ttRowSpecVal.fRowValue.
      END.
      
    /* All calculations were done during prescan and are kept in the prescan table. Move them to their corresponding fields in the output table: */
/*     ELSE */
      FOR EACH ttRowSpecVal where ttRowSpecVal.bCalcByPreScan: 
        IF VALID-HANDLE(hPreScanAccumField) AND ttRowSpecVal.hPreScanField = hPreScanCountDistinct THEN
          hAverageFillField:BUFFER-VALUE = hPreScanAccumField:BUFFER-VALUE / hPreScanCountDistinct:BUFFER-VALUE.
        ttRowSpecVal.hFillField:BUFFER-VALUE = ttRowSpecVal.hPreScanField:BUFFER-VALUE.
/*         IF ttRowSpecVal.b2phAcc THEN DO:                                                              */
/*           AssignStringValue(ttRowSpecVal.hFillField,ttRowSpecVal.cCurrCallOutput).              */
/*           IF ttRowSpecVal.fRowValue NE ? THEN                                                         */
/*             ttRowSpecVal.fGrandTotal = ttRowSpecVal.fGrandTotal + ttRowSpecVal.fRowValue. */
/*         END.                                                                                                */
      END.

    iSelected = iSelected + 1.
  END.

  /* If we want query totals and there was no prescan, add up these here for the rows that are not returned to the client.
     NOTE: If the only stat.field is rowcount we still might have to check calculated values in case they decide 
           wether the row should be counted or not */ 
  ELSE IF icStatFields NE "" AND NOT bPreScanDidAllCalc THEN DO:
    IF icStatFields NE "rowcount" OR bCheckForSkipRow OR cAccumFields NE "" THEN DO:
      DO iy = 1 TO iBufferCount:
        IF hBuffer[iy]:AVAIL THEN DO:
  
          /* Go get values for calculated and/or accumulated and GrandTotal fields for current buffer: */
          FOR EACH ttSpecFlds
              WHERE ttSpecFlds.iCallBufferNum = iy
                AND (IF bCheckForSkipRow THEN TRUE ELSE ttSpecFlds.bGrandTotal)
                and not ttSpecFlds.bCalcByPreScan
                 BY ttSpecFlds.iFieldNum:
  
            FIND FIRST ttRowSpecVal OF ttSpecFlds.
  
            /* Either the calculated values: */
            IF ttSpecFlds.cCallProc NE "" THEN DO:
              IF VALID-HANDLE(ttSpecFlds.hCalcFieldProc) THEN DO:
                IF ttSpecFlds.iExtent > 0 THEN
                  RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc
                     (ttSpecFlds.hSourceField,ttSpecFlds.iExtent, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
                ELSE IF ttSpecFlds.cCallProc = "jb_calc" THEN 
                  RUN JB_calc (ttSpecFlds.hSourceField,ttSpecFlds.hSourceField2,ttSpecFlds.cCallProcParam,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
                ELSE IF ttSpecFlds.cCallProc = "jb_can-find" THEN 
                  RUN JB_can-find (ttSpecFlds.iCallBufferNum,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
                ELSE IF ttSpecFlds.cCallProc = "jb_total" THEN 
                  RUN JB_total (ttSpecFlds.iCallBufferNum,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
                ELSE IF ttSpecFlds.cCallProc = "jb_max" THEN 
                  RUN JB_max (ttSpecFlds.hCompareFields,ttSpecFlds.iCntCompareFields,OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
                ELSE IF ttSpecFlds.cCallProcParam BEGINS "ROWID" THEN 
                  RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc 
                     (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,SUBSTR(ttSpecFlds.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
                ELSE IF ttSpecFlds.cCallProcParam NE "" THEN 
                  RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc 
                      (ttSpecFlds.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
                ELSE 
                  RUN VALUE(ttSpecFlds.cCallProc) IN ttSpecFlds.hCalcFieldProc 
                      (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput) NO-ERROR.
                IF ttSpecFlds.bDirectAssign THEN
                  AssignStringValue(ttRowSpecVal.hFillField,ttRowSpecVal.cCurrCallOutput).
              END.
              ELSE DO:
                IF ttSpecFlds.cCallProcParam BEGINS "ROWID" THEN
                  RUN VALUE(ttSpecFlds.cCallProc) 
                      (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,SUBSTR(ttSpecFlds.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
                ELSE IF ttSpecFlds.cCallProcParam NE "" THEN
                  RUN VALUE(ttSpecFlds.cCallProc) 
                      (ttSpecFlds.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
                ELSE 
                  RUN VALUE(ttSpecFlds.cCallProc) 
                      (hBuffer[ttSpecFlds.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecVal.cCurrCallOutput).
              END.
  
              IF ttRowSpecVal.cCurrCallOutput = "skiprow" OR (bCalcFieldFilter AND NOT CalcValueOk(ttRowSpecVal.cCurrCallOutput)) THEN DO:
                /* If any accumulations were done by prescan they need to be adjusted since the row should not be included: */
                FOR EACH ttRowSpecVal where ttRowSpecVal.bCalcByPreScan
                    AND ttRowSpecVal.bAccum:
                  ttRowSpecVal.fGrandTotal = ttRowSpecVal.fGrandTotal - ttRowSpecVal.fRowValue.
                END.
                IF icDirection = "" THEN
                  hQuery:GET-NEXT().
                ELSE 
                  hQuery:GET-PREV().
                NEXT QUERYLOOP.
              END.
              ELSE IF ttSpecFlds.bGrandTotal THEN DO: 
                ttRowSpecVal.fRowValue = DEC(ttRowSpecVal.cCurrCallOutput) NO-ERROR.
                IF ERROR-STATU:ERROR THEN DO:
                  ocReturn = "Server calculation routine " + ttSpecFlds.cCallProc + " returns invalid value for accumulation: " + ttRowSpecVal.cCurrCallOutput.
                  LogThis(ocReturn,"",1).
                  RETURN.
                END.
              END.
            END. /* Calculated values */
            /*  GrandTotal fields from the database buffer: */
            ELSE IF ttSpecFlds.bGrandTotal THEN DO:
              ttRowSpecVal.fRowValue = DEC(ttSpecFlds.hSourceField:BUFFER-VALUE) NO-ERROR.
              IF ERROR-STATU:ERROR THEN DO:
                ocReturn = "Accumulation of field " + ttSpecFlds.cFillField + " on assignment due to a mismatch in datatype (only numbers can be accumulated)".
                RETURN.
              END.
            END.
          END.
        END.
        ELSE 
          FOR EACH ttRowSpecVal 
              WHERE ttRowSpecVal.iCallBufferNum = iy
                AND NOT ttRowSpecVal.bCalcByPreScan:
            ASSIGN ttRowSpecVal.fRowValue = 0
                   ttRowSpecVal.cCurrCallOutput = "".
          END.
      END.
  
      /* Add up grand totals: */
      FOR EACH ttRowSpecVal 
          WHERE not ttRowSpecVal.bCalcByPreScan 
            and ttRowSpecVal.fRowValue NE ?:
        ttRowSpecVal.fGrandTotal = ttRowSpecVal.fGrandTotal + ttRowSpecVal.fRowValue.
      END.
    END.
  END. /* adding up query totals for rows not returned to the client */

  ELSE IF iSelected = iiBatchSize THEN DO:
    iCount = iCount - 1.
    LEAVE.
  END.   

  iCount = iCount + 1.

  IF icDirection = "" THEN
    hQuery:GET-NEXT().
  ELSE 
    hQuery:GET-PREV().
END.

IF iQueryCount = 0 THEN iQueryCount = iCount.

LogThis("Count: ",STRING(iQueryCount),1).
LogThis("Etime, run: ",STRING(ETIME / 1000),2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WrapUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WrapUp Procedure 
PROCEDURE WrapUp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO ix = 1 TO NUM-ENTRIES(cCalcFieldProcs,";") - 1:
  IF VALID-HANDLE(hCalcFieldProcs[ix]) THEN DO:
    IF CAN-DO(hCalcFieldProcs[ix]:INTERNAL-ENTRIES,"CleanUp") THEN
      RUN CleanUp IN hCalcFieldProcs[ix].
    DELETE PROCEDURE hCalcFieldProcs[ix] NO-ERROR.  
  END. 
END.

IF VALID-HANDLE(httPreScan) THEN DELETE OBJECT httPreScan.
IF VALID-HANDLE(hBufferAccessCheck) THEN DELETE OBJECT hBufferAccessCheck.

IF icStatFields NE "" THEN DO:
  ocStatFieldsAndValues = "rowcount|" + STRING(iQueryCount) + ";".
  FOR EACH ttSpecFlds
      WHERE ttSpecFlds.bGrandTotal
     ,FIRST ttRowSpecVal OF ttSpecFlds:
    ocStatFieldsAndValues = ocStatFieldsAndValues + ttSpecFlds.cFillField + "|" + STRING(ttRowSpecVal.fGrandTotal) + ";".
  END.
  ocStatFieldsAndValues = TRIM(ocStatFieldsAndValues,";").
END.

IF bMainQueryStopped THEN 
  ocStatFieldsAndValues = "rowcount|0;querystop|" + STRING(iMainQueryStopAfter).

FOR EACH ttSpecFlds:
  IF VALID-HANDLE(ttSpecFlds.hTargetBuff) THEN
    DELETE OBJECT ttSpecFlds.hTargetBuff NO-ERROR.
  IF VALID-HANDLE(ttSpecFlds.hTargetQuery) THEN
    DELETE OBJECT ttSpecFlds.hTargetQuery.
END.

DO ix = 1 TO iBufferCount:
  IF VALID-HANDLE(hBuffer[ix]) THEN
    DELETE OBJECT hBuffer[ix].
  ELSE LEAVE.
END.
IF VALID-HANDLE(hBuffer2) THEN DELETE OBJECT hBuffer2.

LogThis("icStatFields: ", icStatFields,1).
LogThis("cCalcStatFields: ", cCalcStatFields,1). 
LogThis("StatOutput: ", ocStatFieldsAndValues,1).

IF VALID-HANDLE(httPreScan) THEN
  DELETE OBJECT httPreScan.
IF VALID-HANDLE(httPreScanJoin) THEN
  DELETE OBJECT httPreScanJoin.

DELETE OBJECT hQuery.

IF VALID-HANDLE(hJbAPI) THEN DELETE PROCEDURE hJbAPI.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-AddTTindex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddTTindex Procedure 
FUNCTION AddTTindex RETURNS LOGICAL
  ( INPUT icPrimaryBufferFlds AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cPrimaryKeyFlds AS CHAR NO-UNDO.
  
cPrimaryKeyFlds = getPrimaryKeyFields(hBuffer[1]).

IF cPrimaryKeyFlds NE "" AND CAN-DO(icPrimaryBufferFlds,ENTRY(1,cPrimaryKeyFlds)) THEN DO:
  ohTempTable:ADD-NEW-INDEX("idxPrimary").
  DO ix = 1 TO NUM-ENTRIES(cPrimaryKeyFlds):
    IF CAN-DO(icPrimaryBufferFlds,ENTRY(ix,cPrimaryKeyFlds)) THEN 
      ohTempTable:ADD-INDEX-FIELD("idxPrimary",ENTRY(ix,cPrimaryKeyFlds)).
  END.
END.

ohTempTable:ADD-NEW-INDEX("idxRowids").
DO ix = 1 TO iBufferCount:
  ohTempTable:ADD-INDEX-FIELD("idxRowids","RowIdent" + STRING(ix)).
END.


ohTempTable:ADD-NEW-INDEX("idxRowCount").
ohTempTable:ADD-INDEX-FIELD("idxRowCount","RowCount").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AppendTTbuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AppendTTbuffers Procedure 
FUNCTION AppendTTbuffers RETURNS LOGICAL
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cQueryString    AS CHAR   NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR cBufferMatch    AS CHAR   NO-UNDO.
DEF VAR cQueryPart      AS CHAR   NO-UNDO.
DEF VAR cNewQueryString AS CHAR   NO-UNDO.
DEF VAR cSortString     AS CHAR   NO-UNDO.

IF INDEX(icQueryCriteria," by ") > 0 THEN
  ASSIGN cQueryString = SUBSTR(icQueryCriteria,1,INDEX(icQueryCriteria," by "))
         cSortString  = SUBSTR(icQueryCriteria,INDEX(icQueryCriteria," by ")).
ELSE cQueryString = TRIM(icQueryCriteria).

ASSIGN cNotExistList = TRIM(cNotExistList,",")       
       cQueryString = REPLACE(cQueryString,",   FIRST",",FIRST")
       cQueryString = REPLACE(cQueryString,",  FIRST",",FIRST")
       cQueryString = REPLACE(cQueryString,",   LAST",",LAST")
       cQueryString = REPLACE(cQueryString,",  LAST",",LAST")
       cQueryString = REPLACE(cQueryString,",   EACH",",EACH")
       cQueryString = REPLACE(cQueryString,",  EACH",",EACH")
       cQueryString = REPLACE(cQueryString,",FIRST","¤FIRST")
       cQueryString = REPLACE(cQueryString,", FIRST","¤FIRST")
       cQueryString = REPLACE(cQueryString,",LAST","¤LAST")
       cQueryString = REPLACE(cQueryString,", LAST","¤LAST")
       cQueryString = REPLACE(cQueryString,",EACH","¤EACH")
       cQueryString = REPLACE(cQueryString,", EACH","¤EACH")
       .

DO ix = 1 TO NUM-ENTRIES(cQueryString,"¤"):
  ASSIGN cQueryPart   = ENTRY(ix,cQueryString,"¤")
         cBufferMatch = ENTRY(1,TRIM(SUBSTR(cQueryPart,7))," "). 

  IF CAN-DO(cNotExistList,cBufferMatch) AND NOT cQueryPart MATCHES "*outer-join*" THEN cQueryPart = cQueryPart + " OUTER-JOIN".

  cNewQueryString = cNewQueryString + (IF cNewQueryString NE "" THEN "," ELSE "") + cQueryPart. 
END.

DO ix = 1 TO NUM-ENTRIES(cNotExistList):
  CREATE TEMP-TABLE hNotCanDoTT[ix].
  hNotCanDoTT[ix]:ADD-NEW-FIELD("iDummyTTfield" + STRING(ix),"INTEGER").
  hNotCanDoTT[ix]:TEMP-TABLE-PREPARE("ttNotExist" + STRING(ix)).
  hNotCanDoTT[ix]:DEFAULT-BUFFER-HANDLE:BUFFER-CREATE().
  hQuery:ADD-BUFFER(hNotCanDoTT[ix]:DEFAULT-BUFFER-HANDLE).
  cNewQueryString = cNewQueryString + ",FIRST ttNotExist" + STRING(ix) + " WHERE NOT AVAIL " + ENTRY(ix,cNotExistList). 
END.

icQueryCriteria = cNewQueryString + cSortString.

RETURN YES. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AssignStringValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AssignStringValue Procedure 
FUNCTION AssignStringValue RETURNS CHARACTER
  ( INPUT ihField       AS HANDLE,
    INPUT icUpdateValue AS CHAR ) :

IF NOT VALID-HANDLE(ihField) THEN RETURN "".

CASE ihField:DATA-TYPE:
  WHEN "character" THEN
    ihField:BUFFER-VALUE = icUpdateValue NO-ERROR.
  WHEN "date" THEN
    ihField:BUFFER-VALUE = DATE(icUpdateValue) NO-ERROR.
  WHEN "decimal" THEN
    ihField:BUFFER-VALUE = DEC(icUpdateValue) NO-ERROR.
  WHEN "integer" THEN
    ihField:BUFFER-VALUE = INT(icUpdateValue) NO-ERROR.
  WHEN "logical" THEN
    ihField:BUFFER-VALUE = (IF icUpdateValue = "yes" OR icUpdateValue = "true" THEN TRUE ELSE FALSE)  NO-ERROR.
  OTHERWISE DO:
    RUN jbserv_assignstringvalue.p (hField,0,icUpdateValue,OUTPUT bOk).
    IF NOT bOk THEN
      RETURN "Error assigning string value " + icUpdateValue.
  END.
END CASE.
IF ERROR-STATUS:ERROR THEN
  RETURN ERROR-STATUS:GET-MESSAGE(1).   
ELSE 
  RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CalcValueOk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CalcValueOk Procedure 
FUNCTION CalcValueOk RETURNS LOGICAL
  ( INPUT cCalcValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Check if value for calculated field passes filter crieria
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bPass       AS LOG  NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR cOperator   AS CHAR NO-UNDO.
DEF VAR cCurrFilter AS CHAR NO-UNDO.

IF NOT CAN-DO(cCalcFieldFilter,ttSpecFlds.cFillField) THEN RETURN TRUE.

DO ix = 1 TO NUM-ENTRIES(ttSpecFlds.cFilterOperator):
  ASSIGN cOperator   = ENTRY(ix,ttSpecFlds.cFilterOperator)
         cCurrFilter = ENTRY(ix,ttSpecFlds.cFilterValue,"|").

  CASE ttSpecFlds.cFillFieldType:
    WHEN "CHARACTER" THEN
      CASE cOperator:
        WHEN "=" OR WHEN "EQ" THEN 
          bPass = cCalcValue = cCurrFilter.
        WHEN "<>" OR WHEN "NE" THEN 
          bPass = cCalcValue NE cCurrFilter.
        WHEN ">" OR WHEN "GT" THEN 
          bPass = cCalcValue GT cCurrFilter.
        WHEN "<" OR WHEN "LT" THEN
          bPass = cCalcValue LT cCurrFilter.
        WHEN ">=" OR WHEN "GE" THEN 
          bPass = cCalcValue GE cCurrFilter.
        WHEN "<=" OR WHEN "LE" THEN 
          bPass = cCalcValue LE cCurrFilter.
        WHEN "matches" THEN
          bPass = cCalcValue MATCHES cCurrFilter.
        WHEN "begins" THEN
          bPass = cCalcValue BEGINS cCurrFilter.
        WHEN "contains" THEN DO:
          CREATE ttCalcContains.
          ttCalcContains.cCalcValue = cCalcValue.
          FOR EACH ttCalcContains 
              WHERE ttCalcContains.cCalcValue CONTAINS cCurrFilter:
            bPass = YES.
          END.
          FOR EACH ttCalcContains: DELETE ttCalcContains. END.
        END.
      END CASE.
    WHEN "DATE" THEN
      CASE cOperator:
        WHEN "=" OR WHEN "EQ" THEN 
          bPass = DATE(cCalcValue) = DATE(cCurrFilter).
        WHEN "<>" OR WHEN "NE" THEN 
          bPass = DATE(cCalcValue) NE DATE(cCurrFilter).
        WHEN ">" OR WHEN "GT" THEN 
          bPass = DATE(cCalcValue) GT DATE(cCurrFilter).
        WHEN "<" OR WHEN "LT" THEN
          bPass = DATE(cCalcValue) LT DATE(cCurrFilter).
        WHEN ">=" OR WHEN "GE" THEN 
          bPass = DATE(cCalcValue) GE DATE(cCurrFilter).
        WHEN "<=" OR WHEN "LE" THEN 
          bPass = DATE(cCalcValue) LE DATE(cCurrFilter).
      END CASE.
    WHEN "DECIMAL" THEN
      CASE cOperator:
        WHEN "=" OR WHEN "EQ" THEN 
          bPass = DECIMAL(cCalcValue) = DECIMAL(cCurrFilter).
        WHEN "<>" OR WHEN "NE" THEN 
          bPass = DECIMAL(cCalcValue) NE DECIMAL(cCurrFilter).
        WHEN ">" OR WHEN "GT" THEN 
          bPass = DECIMAL(cCalcValue) GT DECIMAL(cCurrFilter).
        WHEN "<" OR WHEN "LT" THEN
          bPass = DECIMAL(cCalcValue) LT DECIMAL(cCurrFilter).
        WHEN ">=" OR WHEN "GE" THEN 
          bPass = DECIMAL(cCalcValue) GE DECIMAL(cCurrFilter).
        WHEN "<=" OR WHEN "LE" THEN 
          bPass = DECIMAL(cCalcValue) LE DECIMAL(cCurrFilter).
      END CASE.
    WHEN "INTEGER" THEN
      CASE cOperator:
        WHEN "=" OR WHEN "EQ" THEN 
          bPass = INTEGER(cCalcValue) = INTEGER(cCurrFilter).
        WHEN "<>" OR WHEN "NE" THEN 
          bPass = INTEGER(cCalcValue) NE INTEGER(cCurrFilter).
        WHEN ">" OR WHEN "GT" THEN 
          bPass = INTEGER(cCalcValue) GT INTEGER(cCurrFilter).
        WHEN "<" OR WHEN "LT" THEN
          bPass = INTEGER(cCalcValue) LT INTEGER(cCurrFilter).
        WHEN ">=" OR WHEN "GE" THEN 
          bPass = INTEGER(cCalcValue) GE INTEGER(cCurrFilter).
        WHEN "<=" OR WHEN "LE" THEN 
          bPass = INTEGER(cCalcValue) LE INTEGER(cCurrFilter).
      END CASE.
    WHEN "LOGICAL" THEN DO:
      IF cCalcValue = "" THEN cCalcValue = "no".
      CASE cOperator:
        WHEN "=" OR WHEN "EQ" THEN
          bPass = (CAN-DO("yes,true",cCurrFilter) AND CAN-DO("yes,true",cCalcValue)) OR
                  (CAN-DO("no,false",cCurrFilter) AND CAN-DO("no,false",cCalcValue)).
        WHEN "<>" OR WHEN "NE" THEN
          bPass = (NOT CAN-DO("yes,true",cCurrFilter) AND NOT CAN-DO("yes,true",cCalcValue)) OR
                  (NOT CAN-DO("no,false",cCurrFilter) AND NOT CAN-DO("no,false",cCalcValue)).
      END CASE.
    END.  
  END CASE.
  IF NOT bPass THEN LEAVE.
END.

RETURN bPass.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckFieldLevelSecurity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckFieldLevelSecurity Procedure 
FUNCTION CheckFieldLevelSecurity RETURNS CHARACTER
  ( INPUT icMode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix                 AS INT    NO-UNDO.
DEF VAR iy                 AS INT    NO-UNDO.
DEF VAR cNotCanReadFlds    AS CHAR   NO-UNDO.
DEF VAR cNotCanWriteFlds   AS CHAR   NO-UNDO.
DEF VAR cNotCanReadBuffs   AS CHAR   NO-UNDO.
DEF VAR cNotCanCreateBuffs AS CHAR   NO-UNDO.
DEF VAR cNotCanWriteBuffs  AS CHAR   NO-UNDO.
DEF VAR cNotCanDeleteBuffs AS CHAR   NO-UNDO.

DO ix = 1 TO iBufferCount:
  DO iy = 1 TO hBuffer[ix]:NUM-FIELDS:
    IF NOT hBuffer[ix]:BUFFER-FIELD(iy):CAN-READ THEN DO:
      IF icMode = "exec" AND NOT CAN-DO(cExceptionFields[ix],hBuffer[ix]:BUFFER-FIELD(iy):NAME) THEN
        cExceptionFields[ix] = cExceptionFields[ix] + "," + hBuffer[ix]:BUFFER-FIELD(iy):NAME.

      cNotCanReadFlds = cNotCanReadFlds + hBuffer[ix]:BUFFER-FIELD(iy):NAME + 
                         (IF CAN-DO(cDuplicateFields[ix],hBuffer[ix]:BUFFER-FIELD(iy):NAME) THEN STRING(ix) ELSE "")
                        + ",".

      LogThis("Field access denied for " + cCurrUserId,hBuffer[ix]:BUFFER-FIELD(iy):NAME + " (" + icMode + ")",1).
    END.
    IF NOT hBuffer[ix]:BUFFER-FIELD(iy):CAN-WRITE THEN 
      cNotCanWriteFlds = cNotCanWriteFlds + hBuffer[ix]:BUFFER-FIELD(iy):NAME + ",".
  END.
  cExceptionFields[ix] = RIGHT-TRIM(cExceptionFields[ix],",").

  IF NOT hBuffer[ix]:CAN-READ THEN 
    cNotCanReadBuffs = cNotCanReadBuffs + hBuffer[ix]:NAME + ",".
  IF NOT hBuffer[ix]:CAN-CREATE THEN 
    cNotCanCreateBuffs = cNotCanCreateBuffs + hBuffer[ix]:NAME + ",".
  IF NOT hBuffer[ix]:CAN-WRITE THEN 
    cNotCanWriteBuffs = cNotCanWriteBuffs + hBuffer[ix]:NAME + ",".
  IF NOT hBuffer[ix]:CAN-DELETE THEN 
    cNotCanDeleteBuffs = cNotCanDeleteBuffs + hBuffer[ix]:NAME + ",".
END.

IF icMode = "exec" THEN
  ASSIGN cNotCanReadFlds    = RIGHT-TRIM(cNotCanReadFlds,",")
         cNotCanWriteFlds   = RIGHT-TRIM(cNotCanWriteFlds,",")
         cNotCanReadBuffs   = RIGHT-TRIM(cNotCanReadBuffs,",")
         cNotCanWriteBuffs  = RIGHT-TRIM(cNotCanWriteBuffs,",")
         cNotCanCreateBuffs = RIGHT-TRIM(cNotCanCreateBuffs,",")
         cNotCanDeleteBuffs = RIGHT-TRIM(cNotCanDeleteBuffs,",")
         cReturnOKmessage = cReturnOKmessage 
         + RIGHT-TRIM(",nodbreadaccessfields;"    + REPLACE(cNotCanReadFlds,",","|"),";")
         + RIGHT-TRIM(",nodbwriteaccessfields;"   + REPLACE(cNotCanWriteFlds,",","|"),";")
         + RIGHT-TRIM(",nodbreadaccessbuffers;"   + REPLACE(cNotCanReadBuffs,",","|"),";")
         + RIGHT-TRIM(",nodbwriteaccessbuffers;"  + REPLACE(cNotCanWriteBuffs,",","|"),";") 
         + RIGHT-TRIM(",nodbcreateaccessbuffers;" + REPLACE(cNotCanCreateBuffs,",","|"),";") 
         + RIGHT-TRIM(",nodbdeleteaccessbuffers;" + REPLACE(cNotCanDeleteBuffs,",","|"),";") 
         .

RETURN cNotCanReadFlds.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ComposeQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ComposeQuery Procedure 
FUNCTION ComposeQuery RETURNS CHARACTER
  ( INPUT iiQueryNum AS INT) :
/*------------------------------------------------------------------------------
  Purpose: Compose a query string from decomposed query elements  
    Notes: iiQueryNum = 0 -> org. query
           The direction for adding buffers is opposite of the org.query from prescan queries
------------------------------------------------------------------------------*/
DEF VAR cQueryString   AS CHAR NO-UNDO.
DEF VAR bWhereExist    AS LOG  NO-UNDO.
DEF VAR bOutJoin       AS LOG  NO-UNDO.
DEF VAR bNoPrescanExec AS LOG  NO-UNDO.
DEF VAR ix             AS INT  NO-UNDO.
DEF VAR cRevQuery      AS CHAR NO-UNDO.

IF iiQueryNum = -1 THEN
  ASSIGN bNoPrescanExec = YES
         iiQueryNum     = 0.

IF iiQueryNum = 0 THEN FOR EACH ttQuery 
    WHERE ttQuery.iQueryNum = iiQueryNum
       BREAK BY ttQuery.iQueryNum BY ttQuery.iQryBufferNum:

  IF FIRST-OF(ttQuery.iQueryNum) THEN DO:
    ASSIGN bOutJoin     = ttQuery.bOuterJoin
           cQueryString = cQueryString + IF NOT bNoPrescanExec AND ttQuery.cQryString BEGINS "WHERE " THEN 
                                           "AND" + SUBSTR(ttQuery.cQryString,6) 
                                         ELSE ttQuery.cQryString.
    IF ttQuery.iQryBufferNum = 1 AND cQueryString MATCHES "*USE-INDEX*" THEN DO:
      DO ix = 2 TO NUM-ENTRIES(cQueryString," "):
        IF ENTRY(ix,cQueryString," ") = "USE-INDEX" OR ENTRY(ix - 1,cQueryString," ") = "USE-INDEX" THEN NEXT.
        cRevQuery = cRevQuery + ENTRY(ix,cQueryString," ") + " ".
      END.
      cQueryString = entry(1,cQueryString," ") + " " + cRevQuery.
    END.
  END.
  ELSE
    cQueryString = cQueryString + FixQueryJoin(ttQuery.cQryJoinString + " " + ttQuery.cQryString).

  IF LAST-OF(ttQuery.iQryBufferNum) THEN DO:
    IF LOOKUP("NO-LOCK",ttQuery.cQryString," ") = 0 THEN
      cQueryString = cQueryString + " NO-LOCK".
    IF ttQuery.bOuterJoin THEN
      cQueryString = cQueryString + " OUTER-JOIN".
  END.
  IF LAST-OF(ttQuery.iQueryNum) THEN
    cQueryString = cQueryString + " " + ttQuery.cQrySort.
END.
  
ELSE FOR EACH ttQuery 
    WHERE ttQuery.iQueryNum = iiQueryNum
       BREAK BY ttQuery.iQueryNum BY ttQuery.iQryBufferNum DESC:

  IF FIRST-OF(ttQuery.iQueryNum) THEN
    ASSIGN bOutJoin     = ttQuery.bOuterJoin
           cQueryString = cQueryString + ttQuery.cQryString.
  ELSE
    cQueryString = cQueryString + FixQueryJoin(ttQuery.cQryJoinString + " " + ttQuery.cQryString).

  IF LAST-OF(ttQuery.iQryBufferNum) AND LOOKUP("NO-LOCK",ttQuery.cQryString," ") = 0 THEN
    cQueryString = cQueryString + " NO-LOCK".
END.

RETURN REPLACE(cQueryString,"¤",",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateExtentCalcField) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateExtentCalcField Procedure 
FUNCTION CreateExtentCalcField RETURNS CHARACTER
  ( INPUT ihBuffer     AS HANDLE,
    INPUT iiBufferNum  AS INT,
    INPUT iiFieldNum   AS INT,
    INPUT icFieldName  AS CHAR,
    INPUT icDataType   AS CHAR,
    INPUT icFormat     AS CHAR,
    INPUT icLabel      AS CHAR,
    INPUT ibDistinct   AS LOG,
    INPUT ibAccum      AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hField  AS HANDLE NO-UNDO.
DEF VAR iExtent AS INT    NO-UNDO.

iExtent = INT(SUBSTR(icFieldName,R-INDEX(icFieldName,"[") + 1,R-INDEX(icFieldName,"]") - R-INDEX(icFieldName,"[") - 1)) NO-ERROR.

IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN RETURN "Invalid definition of extent field: " + icFieldName + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).

IF NOT icFieldName BEGINS "+" THEN DO:
  hField = ihBuffer:BUFFER-FIELD(SUBSTR(icFieldName,1,R-INDEX(icFieldName,"[") - 1)) NO-ERROR.
  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN RETURN "Invalid definition of extent field: " + icFieldName + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
   
  CREATE ttSpecFlds.
  ASSIGN ttSpecFlds.iCallBufferNum  = iiBufferNum
         ttSpecFlds.iFieldNum       = iiFieldNum
         ttSpecFlds.cFillField      = icFieldName
         ttSpecFlds.cFillFieldType  = hField:DATA-TYPE
         ttSpecFlds.cCallProc       = "ExtentFieldValue"
         ttSpecFlds.hSourceField    = hField
         ttSpecFlds.bGrandTotal     = CAN-DO(icStatFields,icFieldName) OR ibAccum
         ttSpecFlds.bDistinct       = ibDistinct
         ttSpecFlds.bAccum          = ibAccum
         ttSpecFlds.b2phAcc         = NO
         ttSpecFlds.hCalcFieldProc  = THIS-PROCEDURE
         ttSpecFlds.iExtent         = iExtent
         .
  
  IF icFormat = "" THEN icFormat = hField:FORMAT.
  IF icLabel  = "" THEN icLabel  = hField:LABEL + "[" + STRING(iExtent) + "]".
  
  ohTempTable:ADD-NEW-FIELD(icFieldName,
                            hField:DATA-TYPE,0,
                            icFormat,
                            "",
                            icLabel,
                            IF cExtraLabel MATCHES "*" + CHR(10) + "*" THEN
                              REPLACE(cExtraLabel,CHR(10),"!")
                            ELSE ?) NO-ERROR.

  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN RETURN "Error when creating output tt field for extent field: " + icFieldName + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
  
  ohTempTable:ADD-NEW-FIELD("jbextent_" + STRING(iExtent) + "_" + hField:NAME,hField:DATA-TYPE).
END.
ELSE DO:
  icFieldName = SUBSTR(icFieldName,2,INDEX(icFieldName,"[") - 2).

  CREATE ttSpecFlds.
  ASSIGN ttSpecFlds.iCallBufferNum  = iiBufferNum
         ttSpecFlds.iFieldNum       = iiFieldNum
         ttSpecFlds.cFillField      = icFieldName
         ttSpecFlds.cFillFieldType  = icDataType
         ttSpecFlds.cCallProc       = ""
         ttSpecFlds.hSourceField    = ?
         ttSpecFlds.bGrandTotal     = NO
         ttSpecFlds.bDistinct       = NO
         ttSpecFlds.bAccum          = NO
         ttSpecFlds.b2phAcc         = NO
         ttSpecFlds.hCalcFieldProc  = THIS-PROCEDURE
         ttSpecFlds.iExtent         = iExtent
         .

  ohTempTable:ADD-NEW-FIELD(icFieldName,
                            icDataType,iExtent,
                            icFormat,
                            "",
                            icLabel,
                            IF cExtraLabel MATCHES "*" + CHR(10) + "*" THEN
                              REPLACE(cExtraLabel,CHR(10),"!")
                            ELSE ?) NO-ERROR.


  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN RETURN "Error when creating output tt field for extent field: " + icFieldName + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
  
/*   ohTempTable:ADD-NEW-FIELD("jbextent_" + STRING(iExtent) + "_" + icFieldName,icDataType).  */
END.

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateFieldPairLists) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateFieldPairLists Procedure
FUNCTION UnpackOrgBuffersAndFields RETURNS LOGICAL 
  (  ):
/*------------------------------------------------------------------------------
 Purpose: If the buffer sequence has been changed (i.e orgBuffersAndFields has value)
          duplicates must be handled so that values appear in the right columns.
          Here the original duplicate fields are marked up so that when the
          result-table is created the colums are named according to their original
          name(extension i.e buffer#) irrespective of their current source buffer.
          The BUFFER-COPY statement (in ProcessQuery) will then use a field-pairs parameter.
              
          Also the number suffix of the RowIdent column must be set according to the original 
          buffer sequence so that any exclusion of duplicate rows on the client will be done correctly    
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cBufDef AS CHAR NO-UNDO.
DEF BUFFER bttOrgBuffsAndFlds FOR ttOrgBuffsAndFlds.

DO ix = 1 TO NUM-ENTRIES(cOrgBuffersAndFields):
  cBufDef = ENTRY(ix,cOrgBuffersAndFields).
  cOrgBufferList = cOrgBufferList + (IF cOrgBufferList NE "" THEN "," ELSE "") + ENTRY(1,cBufDef,";"). 
  DO iy = 2 TO NUM-ENTRIES(cBufDef,";"):
    CREATE ttOrgBuffsAndFlds.
    ASSIGN ttOrgBuffsAndFlds.cBufferName = ENTRY(1,cBufDef,";")
           ttOrgBuffsAndFlds.iBufferNum  = ix
           ttOrgBuffsAndFlds.cFieldName  = ENTRY(1,ENTRY(iy,cBufDef,";"),"|")
           .
  END.    
END.

FOR EACH ttOrgBuffsAndFlds
    BREAK BY ttOrgBuffsAndFlds.cFieldName
    :
  ix = ix + 1.
  IF LAST-OF(ttOrgBuffsAndFlds.cFieldName) THEN DO:
    IF ix > 1 THEN DO:
      iy = 0.
      FOR EACH bttOrgBuffsAndFlds
          WHERE bttOrgBuffsAndFlds.cFieldName  = ttOrgBuffsAndFlds.cFieldName
          :
        iy = iy + 1.
        IF iy > 1 THEN    
          bttOrgBuffsAndFlds.bDuplicate = YES.
      END.
    END.  
    ix = 0.          
  END.
END.             

RETURN YES.

END FUNCTION.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-DebugPreScan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DebugPreScan Procedure 
FUNCTION DebugPreScan RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR fTot AS DEC NO-UNDO.

  FOR EACH ttRowSpecVal
      WHERE ttRowSpecVal.fGrandTotal NE ?: 
    logThis(ttRowSpecVal.cFillField,STRING(ttRowSpecVal.fGrandTotal),0).
    fTot = fTot + ttRowSpecVal.fGrandTotal.
  END.
  logthis("Sum",STRING(fTot),1).

/*
MESSAGE PROGRAM-NAME(1) SKIP
        SUBSTR(icQueryCriteria,INDEX(icQueryCriteria," ")) SKIP(2)
        ENTRY(1,SUBSTR(icQueryCriteria,INDEX(icQueryCriteria," ")))
        VIEW-AS ALERT-BOX.
DEF VAR hqPre AS HANDLE NO-UNDO.
DEF VAR iicnt AS INT NO-UNDO.
CREATE QUERY hqPre.
hqPre:ADD-BUFFER(hBuffPreScan).
hqPre:ADD-BUFFER(hBuffer[1]).
hqPre:QUERY-PREPARE("FOR EACH ttPreScan,EACH " + ENTRY(1,ENTRY(1,icBuffersAndFields),";") + " NO-LOCK " 
                   + "WHERE ROWID(" + ENTRY(1,ENTRY(1,icBuffersAndFields),";") + ") = ttPreScan.rRelPreScan"
/*                    + " AND " + ENTRY(1,SUBSTR(icQueryCriteria,INDEX(icQueryCriteria," ")))  */
                    ).
hqPre:QUERY-OPEN().
MESSAGE PROGRAM-NAME(1) SKIP
        hqPre:PREPARE-STRING SKIP
        VIEW-AS ALERT-BOX.
hqPre:GET-FIRST().
REPEAT WHILE NOT hqPre:QUERY-OFF-END:
  iicnt = iicnt + 1.
  hqPre:GET-NEXT().
END.
MESSAGE PROGRAM-NAME(1) SKIP
        iicnt SKIP
        VIEW-AS ALERT-BOX.
*/        
  
RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DecomposeQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DecomposeQuery Procedure 
FUNCTION DecomposeQuery RETURNS LOGICAL
  ( INPUT iiQueryNum    AS INT,
    INPUT icQueryString AS CHAR,
    INPUT icBufferList  AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iQryBuffNum AS INT  NO-UNDO.
DEF VAR iSkipUntil  AS INT  NO-UNDO.
DEF VAR cMainEntry  AS CHAR NO-UNDO.
DEF VAR cSubEntry   AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.
DEF VAR iy          AS INT  NO-UNDO.
DEF VAR iLastBuff   AS INT  NO-UNDO INIT 100.

ASSIGN icQueryString = TRIM(icQueryString)
       icQueryString = REPLACE(icQueryString,",   FIRST",",FIRST")
       icQueryString = REPLACE(icQueryString,",  FIRST",",FIRST")
       icQueryString = REPLACE(icQueryString,",   LAST",",LAST")
       icQueryString = REPLACE(icQueryString,",  LAST",",LAST")
       icQueryString = REPLACE(icQueryString,",   EACH",",EACH")
       icQueryString = REPLACE(icQueryString,",  EACH",",EACH")
       icQueryString = REPLACE(icQueryString,",FIRST","¤FIRST")
       icQueryString = REPLACE(icQueryString,", FIRST","¤FIRST")
       icQueryString = REPLACE(icQueryString,",LAST","¤LAST")
       icQueryString = REPLACE(icQueryString,", LAST","¤LAST")
       icQueryString = REPLACE(icQueryString,",EACH","¤EACH")
       icQueryString = REPLACE(icQueryString,", EACH","¤EACH")
       icQueryString = REPLACE(icQueryString,",",CHR(29))
       icQueryString = REPLACE(icQueryString,"¤FIRST",",FIRST")
       icQueryString = REPLACE(icQueryString,"¤LAST",",LAST")
       icQueryString = REPLACE(icQueryString,"¤EACH",",EACH")
       .

DO ix = 1 TO MIN(NUM-ENTRIES(icBufferList),NUM-ENTRIES(icQueryString) - NUM-ENTRIES(cNotExistList)):
  cMainEntry = TRIM(ENTRY(ix,icQueryString)). 

  IF iiQueryNum = 0 AND ix = 1 THEN 
    cFirstMainQueryEntry = cMainEntry.

  IF iSkipUntil NE 0 AND ix < iSkipUntil THEN NEXT.
  ELSE iSkipUntil = 0.

  IF ix = 1 OR (cMainEntry BEGINS "FIRST " OR cMainEntry BEGINS "LAST " OR cMainEntry BEGINS "EACH ") THEN DO:
    CREATE ttQuery.
    ASSIGN iQryBuffNum            = iQryBuffNum + 1
           iLastBuff              = iLastBuff - 1
           cMainEntry             = cMainEntry
           ttQuery.iQueryNum      = iiQueryNum
           ttQuery.cQryBuffer     = ENTRY(iQryBuffNum,icBufferList)
           ttQuery.iQryBufferNum  = IF LOOKUP(ttQuery.cQryBuffer,cBufferList) = 0 THEN iLastBuff ELSE LOOKUP(ttQuery.cQryBuffer,cBufferList)
           ttQuery.bPreScanOnly   = iiQueryNum > 0 AND iQryBuffNum > 1
           ttQuery.bContainsWHERE = (cMainEntry MATCHES "* where *" OR cMainEntry BEGINS "where ") AND NOT ttQuery.bPreScanOnly
           ttQuery.bOuterJoin     = cMainEntry MATCHES "* OUTER-JOIN*"
           .
    IF iQryBuffNum > 1 THEN
      ttQuery.cQryJoinString = "," + cMainEntry.
    ELSE
      ttQuery.cQryString = cMainEntry.

    IF ttQuery.cQryString BEGINS "BY " THEN 
      ASSIGN ttQuery.cQrySort   = ttQuery.cQryString
             ttQuery.cQryString = "".
    ELSE IF INDEX(ttQuery.cQryString," BY ") > 0 THEN 
      ASSIGN ttQuery.cQrySort   = SUBSTR(ttQuery.cQryString,INDEX(ttQuery.cQryString," BY "))
             ttQuery.cQryString = SUBSTR(ttQuery.cQryString,1,INDEX(ttQuery.cQryString," BY ")).
    ELSE IF INDEX(ttQuery.cQryJoinString," BY ") > 0 THEN 
      ASSIGN ttQuery.cQrySort   = SUBSTR(ttQuery.cQryJoinString,INDEX(ttQuery.cQryJoinString," BY "))
             ttQuery.cQryJoinString = SUBSTR(ttQuery.cQryJoinString,1,INDEX(ttQuery.cQryJoinString," BY ")).
    
    ASSIGN ttQuery.cQryString     = REPLACE(ttQuery.cQryString," OUTER-JOIN"," ")
           ttQuery.cQryJoinString = REPLACE(ttQuery.cQryJoinString," OUTER-JOIN"," ")      
           ttQuery.cQryString     = REPLACE(ttQuery.cQryString," NO-LOCK"," ")
           ttQuery.cQryJoinString = REPLACE(ttQuery.cQryJoinString," NO-LOCK"," ")
           ttQuery.cQryString     = REPLACE(ttQuery.cQryString,CHR(29),",")
           ttQuery.cQryJoinString = REPLACE(ttQuery.cQryJoinString,CHR(29),",")
           ttQuery.cQryString     = FixQuery(ttQuery.cQryString)
           ttQuery.cQryJoinString = FixQuery(ttQuery.cQryJoinString)
           .
  END.

END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FinalizeCreateQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FinalizeCreateQuery Procedure 
FUNCTION FinalizeCreateQuery RETURNS CHARACTER
  ( INPUT icPrimBufferDbFlds AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ocReturn AS CHAR NO-UNDO.
DEF VAR cTTname  AS CHAR NO-UNDO.

ohTempTable:ADD-NEW-FIELD("RowCount","INTEGER").

CREATE ttSpecFlds.
ASSIGN ttSpecFlds.iCallBufferNum  = 1
       ttSpecFlds.iFieldNum       = 10000
       ttSpecFlds.cFillField      = "jbCountDistinct"
       ttSpecFlds.cFillFieldType  = "INTEGER"
       ttSpecFlds.hSourceField    = ?
       ttSpecFlds.bGrandTotal     = NO
       ttSpecFlds.bAccum          = YES.
ohTempTable:ADD-NEW-FIELD("jbCountDistinct","INTEGER",0,">>>,>>>,>>9",1).

ohTempTable:ADD-NEW-FIELD("jbAverage","DECIMAL",0,"->>>,>>>,>>9.99",0).

/* Done when the browse or query is defined to speed up duplicate-check on client: */
IF bIndexOnRowids THEN AddTTindex(icPrimBufferDbFlds). 

ASSIGN cCalcStatFields   = TRIM(cCalcStatFields,",")
       cCalcSortFields   = TRIM(cCalcSortFields,",")
       cCalcSortFldTypes = TRIM(cCalcSortFldTypes,",")
       cAccumFields      = TRIM(cAccumFields,",")
       icStatFields      = TRIM(icStatFields,",")
       cBufferList       = TRIM(cBufferList,",").

IF INDEX(ENTRY(1,ENTRY(1,icBuffersAndFields),";"),".") > 0 THEN
  cTTname = ENTRY(2,ENTRY(1,ENTRY(1,icBuffersAndFields),";"),".").
ELSE
  cTTname = ENTRY(1,ENTRY(1,icBuffersAndFields),";").

ohTempTable:TEMP-TABLE-PREPARE(cTTname) NO-ERROR.
IF ERROR-STATUS:ERROR OR ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
  ocReturn = ERROR-STATUS:GET-MESSAGE(1).
  RETURN ocReturn.
END.

ASSIGN httBuffer         = ohTempTable:DEFAULT-BUFFER-HANDLE
       hAverageFillField = httBuffer:BUFFER-FIELD("jbAverage").

  
RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixQuery Procedure 
FUNCTION FixQuery RETURNS CHARACTER
  ( INPUT icQueryCriteria AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cQuery    AS CHAR NO-UNDO.
DEF VAR cJoin     AS CHAR NO-UNDO.
DEF VAR iy        AS INT  NO-UNDO.
DEF VAR iz        AS INT  NO-UNDO.
DEF VAR cTemp1    AS CHAR NO-UNDO.
DEF VAR cTemp2    AS CHAR NO-UNDO.

DO iz = 1 TO LENGTH(icQueryCriteria):
  IF SUBSTR(icQueryCriteria,iz,6) = "first " OR
     SUBSTR(icQueryCriteria,iz,5) = "last " OR
     SUBSTR(icQueryCriteria,iz,5) = "each " 
     THEN DO:
    DO iy = iz TO iz - 10 BY -1:
      IF SUBSTR(icQueryCriteria,iy,1) = "," THEN LEAVE.
    END.
    LEAVE.
  END.
END.

IF iy > 0 THEN 
  ASSIGN cQuery = SUBSTR(icQueryCriteria,1,iy - 1)
         cJoin  = SUBSTR(icQueryCriteria,iy)
         .
ELSE
  cQuery = icQueryCriteria.

ASSIGN cTemp1 = SUBSTR(cQuery,1,INDEX(cQuery,"where") + 4)
       cTemp2 = SUBSTR(cQuery,INDEX(cQuery,"where") + 5)
       icQueryCriteria = cTemp1 + REPLACE(cTemp2,"where"," and") + cJoin
       icQueryCriteria = REPLACE(icQueryCriteria," AND ( AND ("," AND ((").

RETURN icQueryCriteria.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQueryJoin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixQueryJoin Procedure 
FUNCTION FixQueryJoin RETURNS CHARACTER
  ( INPUT icQueryCriteria AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF INDEX(icQueryCriteria," and ") > 0 AND INDEX(icQueryCriteria," where ") = 0 THEN
  icQueryCriteria = SUBSTR(icQueryCriteria,1,INDEX(icQueryCriteria," and "))
                  + "where "
                  + SUBSTR(icQueryCriteria,INDEX(icQueryCriteria," and ") + 5).

RETURN icQueryCriteria.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixQueryOperators) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixQueryOperators Procedure 
FUNCTION FixQueryOperators RETURNS CHARACTER
  ( INPUT icQueryString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

DO ix = 1 TO 5:
  icQueryString = REPLACE(icQueryString,"  "," ").
END.

LogThis("Incorrect query string",icQueryString,1).

ASSIGN icQueryString = REPLACE(icQueryString," where where "," where ")
       icQueryString = REPLACE(icQueryString," where true where"," where ")
       icQueryString = REPLACE(icQueryString," and and "," and ")
       icQueryString = REPLACE(icQueryString," and ( and "," and ( ")
       icQueryString = REPLACE(icQueryString," where ( and "," where ( ")
       icQueryString = REPLACE(icQueryString," and ( where "," and ( ")
       icQueryString = REPLACE(icQueryString," and or "," or ")
       icQueryString = REPLACE(icQueryString," or and "," or ")
       .
IF SUBSTR(icQueryString,LENGTH(icQueryString)) = "," THEN
  icQueryString = SUBSTR(icQueryString,1,LENGTH(icQueryString) - 1).       

LogThis("Corrected query string",icQueryString,1).

RETURN icQueryString. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getActiveQueryBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getActiveQueryBuffer Procedure 
FUNCTION getActiveQueryBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF AVAIL ttSpecFlds THEN
  RETURN hBuffer[ttSpecFlds.iCallBufferNum].
ELSE
  RETURN ?. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getASuserId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getASuserId Procedure 
FUNCTION getASuserId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCurrUserId.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCalcProcHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCalcProcHandle Procedure
FUNCTION getCalcProcHandle RETURNS HANDLE 
  ( INPUT icProcName AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE hCalcProc AS HANDLE NO-UNDO.

DEF BUFFER ttSpecFlds FOR ttSpecFlds.

FOR EACH ttSpecFlds:
  IF ttSpecFlds.hCalcFieldProc:FILE-NAME = icProcName THEN
    hCalcProc = ttSpecFlds.hCalcFieldProc.
END.

RETURN hCalcProc.

END FUNCTION.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getCodeMaster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCodeMaster Procedure 
FUNCTION getCodeMaster RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCodeMaster.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getCompanyId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCompanyId Procedure 
FUNCTION getCompanyId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCurrCompanyId. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLanguageCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLanguageCode Procedure 
FUNCTION getLanguageCode RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCurrLanguage.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrimaryKeyFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPrimaryKeyFields Procedure 
FUNCTION getPrimaryKeyFields RETURNS CHARACTER
  (INPUT ihBuffer AS HANDLE):

DEF VAR cPKfields      AS CHAR NO-UNDO.
DEF VAR ix             AS INT  NO-UNDO.
DEF VAR iy             AS INT  NO-UNDO.

DO ix = 1 TO 100:
  IF ihBuffer:INDEX-INFORMATION(ix) NE ? THEN DO:
    IF ENTRY(3,ihBuffer:INDEX-INFORMATION(ix)) = "1" THEN DO:
      DO iy = 5 TO NUM-ENTRIES(ihBuffer:INDEX-INFORMATION(ix)) BY 2:
        cPKfields = cPKfields + ENTRY(iy,ihBuffer:INDEX-INFORMATION(ix)) + ",".
      END.
      LEAVE.
    END.
  END.
  ELSE LEAVE.
END.  

RETURN TRIM(cPKfields,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getQueryBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQueryBuffer Procedure 
FUNCTION getQueryBuffer RETURNS HANDLE
  ( INPUT icBufferName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hRetBuffer AS HANDLE NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.

IF icBufferName = "ttPreScan" THEN 
  RETURN hBuffPreScan.

DO ix = 1 TO iBufferCount:
  IF hBuffer[ix]:NAME = icBufferName THEN DO:
    hRetBuffer = hBuffer[ix] NO-ERROR.
    LEAVE.
  END.
END.

IF NOT VALID-HANDLE(hRetBuffer) AND VALID-HANDLE(hBuffPreScan) THEN
  hRetBuffer = hBuffPreScan.

RETURN hRetBuffer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetQueryFalseCrit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetQueryFalseCrit Procedure 
FUNCTION GetQueryFalseCrit RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cPKfields      AS CHAR NO-UNDO.
DEF VAR idx1           AS INT NO-UNDO.
DEF VAR idx2           AS INT NO-UNDO.
DEF VAR hIdxFld        AS HANDLE NO-UNDO.
DEF VAR cWhereString   AS CHAR NO-UNDO INIT "WHERE ".

DO idx1 = 1 TO 100:
  IF hBuffer[1]:INDEX-INFORMATION(idx1) NE ? THEN DO:
    IF ENTRY(3,hBuffer[1]:INDEX-INFORMATION(idx1)) = "1" THEN DO:
      DO idx2 = 5 TO NUM-ENTRIES(hBuffer[1]:INDEX-INFORMATION(idx1)) BY 2:
        cPKfields = cPKfields + ENTRY(idx2,hBuffer[1]:INDEX-INFORMATION(idx1)) + ",".
      END.
      LEAVE.
    END.
  END.
  ELSE LEAVE.
END.  

cPkFields = TRIM(cPKfields,",").

IF cPKfields NE "" THEN 
  DO idx1 = 1 TO NUM-ENTRIES(cPKfields):
    IF idx1 > 1 THEN cWhereString = cWhereString + " AND ".
    hIdxFld = hBuffer[1]:BUFFER-FIELD(ENTRY(idx1,cPKfields)).
    IF VALID-HANDLE(hIdxFld) THEN DO:
      CASE hIdxFld:DATA-TYPE:
        WHEN "recid"     THEN RETURN "WHERE false".
        WHEN "character" THEN cWhereString = cWhereString + hIdxFld:NAME + " = 'zaaabbbcccaaaz'".
        WHEN "date"      THEN cWhereString = cWhereString + hIdxFld:NAME + " = DATE('01/11/1111')".
        WHEN "decimal"   THEN cWhereString = cWhereString + hIdxFld:NAME + " = -9876567654233".
        WHEN "integer"   THEN cWhereString = cWhereString + hIdxFld:NAME + " = 2047483648".
        WHEN "logical"   THEN DO:
          IF idx1 = 1 THEN RETURN "WHERE false".
          ELSE cWhereString = cWhereString + hIdxFld:NAME + " = ?".
        END.
      END CASE.
    END.
  END.
ELSE RETURN "WHERE false".

IF TRIM(cWhereString) = "WHERE" THEN cWhereString = "WHERE false".

RETURN cWhereString.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getResultSetBuffer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getResultSetBuffer Procedure 
FUNCTION getResultSetBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN httBuffer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LogThis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LogThis Procedure 
FUNCTION LogThis RETURNS LOGICAL
  ( INPUT icLogEntry  AS CHAR,
    INPUT icLogText   AS CHAR,
    INPUT iiSkipLines AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF cLogFile NE "" AND NOT CAN-DO("_file,_field",ENTRY(1,ENTRY(1,icBuffersAndFields),";")) THEN DO:
  OUTPUT STREAM sLog TO VALUE(cLogFile) APPEND.
  PUT STREAM sLog UNFORMATTED icLogEntry icLogText AT 30.
  CASE iiSkipLines:
    WHEN 0 THEN PUT STREAM sLog SKIP.
    WHEN 1 THEN PUT STREAM sLog SKIP(1).
    WHEN 2 THEN PUT STREAM sLog SKIP(2).
    WHEN 3 THEN PUT STREAM sLog SKIP(3).
    WHEN 4 THEN PUT STREAM sLog SKIP(4).
    WHEN 5 THEN PUT STREAM sLog SKIP(5).
  END CASE.
  OUTPUT STREAM sLog CLOSE.
  IF icLogEntry MATCHES "*Etime*" THEN ETIME(TRUE).
END.
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OnlyCalcFldsInBufferDef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OnlyCalcFldsInBufferDef Procedure 
FUNCTION OnlyCalcFldsInBufferDef RETURNS LOGICAL
  ( INPUT icBufferDef AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Check if the only fields in a definition for a query buffer are calculated
           If so, also all other fields should be added to the returned temp-table 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

IF NUM-ENTRIES(icBufferDef,";") = 1 THEN RETURN NO.

DO ix = 2 TO NUM-ENTRIES(icBufferDef,";"):
  IF NOT (ENTRY(ix,icBufferDef,";") BEGINS "+" OR SUBSTR(ENTRY(1,ENTRY(ix,icBufferDef,";"),"|"),LENGTH(ENTRY(1,ENTRY(ix,icBufferDef,";"),"|")),1) = "]") THEN 
    RETURN NO.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrescanIncludedInMain) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PrescanIncludedInMain Procedure 
FUNCTION PrescanIncludedInMain RETURNS LOGICAL
  ( INPUT icPreScanBufferList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Check if all buffers in a prescan are included in the main query
            If not, the prescan is mandatory 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cMainQueryBufferList AS CHAR NO-UNDO.
DEF VAR ix                   AS INT  NO-UNDO.

FOR EACH ttQuery NO-LOCK
    WHERE ttQuery.iQueryNum = 0:
  cMainQueryBufferList = cMainQueryBufferList + ttQuery.cQryBuffer + ",".
END.
cMainQueryBufferList = TRIM(cMainQueryBufferList,",").

DO ix = 1 TO NUM-ENTRIES(icPreScanBufferList):
  IF NOT CAN-DO(cMainQueryBufferList,ENTRY(ix,icPreScanBufferList)) THEN RETURN NO.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrimaryBufferSpeedOk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PrimaryBufferSpeedOk Procedure 
FUNCTION PrimaryBufferSpeedOk RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hPreScanQuery     AS HANDLE NO-UNDO.
DEF VAR hPreScanBuffer    AS HANDLE NO-UNDO.
DEF VAR bPassPrescan      AS LOG    NO-UNDO.
DEF VAR iCurrPrescanLimit AS INT    NO-UNDO. 
DEF VAR hRecordCountQuery AS HANDLE NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.

FIND FIRST ttQuery
     WHERE ttQuery.iQueryNum = 0
       AND ttQuery.iQryBufferNum = 1
     NO-ERROR.
IF AVAIL ttQuery THEN DO:
  CREATE QUERY hPreScanQuery.
  hPreScanQuery:FORWARD-ONLY = YES.
  IF ttQuery.cQryBuffer MATCHES "buf*_*" THEN 
    CREATE BUFFER hPreScanBuffer FOR TABLE SUBSTR(ttQuery.cQryBuffer,6)
                  BUFFER-NAME ttQuery.cQryBuffer NO-ERROR.
  ELSE
    CREATE BUFFER hPreScanBuffer FOR TABLE ttQuery.cQryBuffer NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    LogThis("Error in Primary buffer speed check. Invalid buffer name:",ttQuery.cQryBuffer,0).
    RETURN YES.
  END.  

  hPreScanQuery:ADD-BUFFER(hPreScanBuffer).

  LogThis("Primary buffer speed check:","FOR EACH " + ttQuery.cQryBuffer + " NO-LOCK " + ttQuery.cQryString,0).

  bOk = hPreScanQuery:QUERY-PREPARE("FOR EACH " + ttQuery.cQryBuffer + " NO-LOCK " + ttQuery.cQryString) NO-ERROR.
  IF NOT bOk THEN DO:
    DELETE OBJECT httPreScanJoin.
    DELETE OBJECT hPreScanQuery.
    LogThis("Error in query primary buffer:","Invalid querystring: " + ttQuery.cQryString + CHR(10) + "Buffer: " + ttQuery.cQryBuffer,0). 
    RETURN NO.
  END.

  LogThis("Primary buffer index:",hPreScanQuery:INDEX-INFORMATION(1),0).

  IF hPreScanQuery:INDEX-INFORMATION(1) BEGINS "whole-index" THEN DO:
    CREATE QUERY hRecordCountQuery.
    hRecordCountQuery:FORWARD-ONLY = YES.
    hRecordCountQuery:ADD-BUFFER(hPreScanBuffer).
    bOk = hRecordCountQuery:QUERY-PREPARE("FOR EACH " + ttQuery.cQryBuffer + " NO-LOCK") NO-ERROR.
    IF bOk THEN DO:
      hRecordCountQuery:QUERY-OPEN().
      hRecordCountQuery:GET-FIRST().
      REPEAT WHILE NOT hRecordCountQuery:QUERY-OFF-END:
        ix = ix + 1.
        IF ix > 1000 THEN DO:
          bPassPrescan = YES.  
          LEAVE.
        END. 
        hRecordCountQuery:GET-NEXT().
      END.
      DELETE OBJECT hRecordCountQuery.
    END.
  END.
  ELSE DO:
    ASSIGN iCurrPrescanLimit = iPrescanLimit
           iPrescanLimit     = 1000
           iStopAfter        = 1.
    bPassPrescan = TooManyHits(hPreScanQuery).
    iPrescanLimit = iCurrPrescanLimit.
  END.

  LogThis("Primary buffer speed ok:",STRING(NOT bPassPrescan),1).

  DELETE OBJECT hPreScanBuffer.
  DELETE OBJECT hPreScanQuery.

END.

RETURN NOT bPassPrescan.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessPreScanJoins) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ProcessPreScanJoins Procedure 
FUNCTION ProcessPreScanJoins RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Optimize and excecute pre-scan queries, ie queries specified on fields in joined tables.
            Steps:
              - Decompose the query strings to ttQuery to be able to manage manipulation
              - Try to reduce number of queries to execute by merging queries that span the same buffers
              - Check for index usage on MANY relations from the base table for the origianl query
                If there is no index skip prescan (the criteria will be added to the org.query)
              - Reduce (remaining) prescan query definitions to the orginal query and add the 
                resulting temp-table from pre-scan as first buffer
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iNumQueries      AS INT    NO-UNDO.
DEF VAR hPreScanQuery    AS HANDLE NO-UNDO.
DEF VAR hPreScanBuff     AS HANDLE NO-UNDO EXTENT 10.
DEF VAR cPreScanQuery    AS CHAR   NO-UNDO.
DEF VAR cPreScanBuffLst  AS CHAR   NO-UNDO.
DEF VAR hRowid           AS HANDLE NO-UNDO.
DEF VAR hcRowid          AS HANDLE NO-UNDO.
DEF VAR iPreScanCount    AS INT    NO-UNDO.
DEF VAR bPassExecution   AS LOG    NO-UNDO.
DEF VAR bPreScanExec     AS LOG    NO-UNDO.
DEF VAR hCurrRowIdBuff   AS HANDLE NO-UNDO.
DEF VAR httAddPreScan    AS HANDLE NO-UNDO EXTENT 10. /* <- first entry will always be empty */
DEF VAR hAddPreScanBuff  AS HANDLE NO-UNDO EXTENT 10.
DEF VAR hAddPreScanRowid AS HANDLE NO-UNDO EXTENT 10.
DEF VAR iCountPrescan    AS INT    NO-UNDO.
DEF VAR cFirstComp       AS CHAR   NO-UNDO.
DEF VAR cSecondComp      AS CHAR   NO-UNDO.
DEF VAR bSameQuery       AS LOG    NO-UNDO.
DEF VAR bSkipPrescan     AS LOG    NO-UNDO.
DEF VAR iStartTime       AS INT    NO-UNDO.

iStartTime = TIME.

bSkipPrescan = PrimaryBufferSpeedOk().

ASSIGN iStopAfter          = 2
       iNumQueries         = NUM-ENTRIES(cPreScanJoinQueries,CHR(28)).

CREATE TEMP-TABLE httPreScanJoin.
httPreScanJoin:ADD-NEW-FIELD("rPreScanJoin","ROWID").
httPreScanJoin:ADD-NEW-FIELD("cPreScanJoin","CHARACTER").
httPreScanJoin:ADD-NEW-INDEX("idxRowid",YES,YES).
httPreScanJoin:ADD-INDEX-FIELD("idxRowid","cPreScanJoin").
httPreScanJoin:TEMP-TABLE-PREPARE("ttPreScanJoin") NO-ERROR.
ASSIGN hBuffPreScanJoin = httPreScanJoin:DEFAULT-BUFFER-HANDLE
       hRowid           = hBuffPreScanJoin:BUFFER-FIELD("rPreScanJoin")
       hcRowid          = hBuffPreScanJoin:BUFFER-FIELD("cPreScanJoin").

DO ix = 1 TO iNumQueries:
  IF TRIM(ENTRY(ix,cPreScanJoinQueries,CHR(28))) = "" THEN NEXT.

  ASSIGN cPreScanQuery   = ENTRY(ix,cPreScanJoinQueries,CHR(28))
         bPassExecution = NO.

  DO iy = 1 TO 5:
    cPreScanQuery = REPLACE(cPreScanQuery,",  ",", ").
  END.

  ASSIGN cPreScanQuery   = REPLACE(cPreScanQuery,"¤FIRST",",FIRST")
         cPreScanQuery   = REPLACE(cPreScanQuery,"¤ FIRST",",FIRST")
         cPreScanQuery   = REPLACE(cPreScanQuery,"¤LAST",",LAST")
         cPreScanQuery   = REPLACE(cPreScanQuery,"¤ LAST",",LAST")
         cPreScanQuery   = REPLACE(cPreScanQuery,"¤EACH",",EACH")
         cPreScanQuery   = REPLACE(cPreScanQuery,"¤ EACH",",EACH")
         cPreScanQuery   = TRIM(cPreScanQuery) 
         cFirstComp      = ENTRY(1,cPreScanQuery)
         cSecondComp     = IF NUM-ENTRIES(cPreScanQuery) > 1 THEN ENTRY(2,cPreScanQuery) ELSE ""
         cPreScanBuffLst = ENTRY(1,cPreScanQuery," ")
         .

  IF cPreScanQuery BEGINS "for each " THEN 
    ASSIGN cPreScanQuery   = TRIM(SUBSTR(cPreScanQuery,10))
           cFirstComp      = TRIM(SUBSTR(cFirstComp,10))
           cPreScanBuffLst = ENTRY(1,cPreScanQuery," ")
           .

/*   IF cPreScanBuffLst = ENTRY(1,cBufferList) THEN DO:    */
/*     LogThis("Prescan doesn't apply:",cPreScanQuery,0).  */
/* /*     NEXT.  */                                        */
/*   END.                                                  */

  DO iy = 2 TO NUM-ENTRIES(cPreScanQuery):
    cPreScanBuffLst = cPreScanBuffLst + "," + ENTRY(2,ENTRY(iy,cPreScanQuery)," ").
  END.

  IF ENTRY(NUM-ENTRIES(cPreScanBuffLst),cPreScanBuffLst) NE ENTRY(1,cBufferList) THEN DO:
    bSameQuery = YES.
    DO iy = 2 TO NUM-ENTRIES(cFirstComp," "):
      IF ENTRY(iy - 1,cFirstMainQueryEntry," ") NE ENTRY(iy,cFirstComp," ") THEN DO:
        bSameQuery = NO.
        LEAVE.
      END.
    END.
    IF bSameQuery THEN NEXT.

    bPassExecution = YES.
    LogThis("Prescan doesn't apply:",cPreScanQuery,0).
  END.

  CREATE ttQueryBufferLists.
  ASSIGN ttQueryBufferLists.iQueryNum        = ix
         ttQueryBufferLists.cQryBufferList   = cPreScanBuffLst
         ttQueryBufferLists.iNumQryBuffs     = NUM-ENTRIES(cPreScanBuffLst)
         ttQueryBufferLists.bPassExec        = bPassExecution OR bSkipPrescan
         ttQueryBufferLists.cFirstQueryComp  = REPLACE(cFirstComp,CHR(1),",")
         ttQueryBufferLists.cSecondQueryComp = REPLACE(cSecondComp,CHR(1),",")
         .
  DecomposeQuery(ix,SUBSTR(cPreScanQuery,INDEX(cPreScanQuery," ")),cPreScanBuffLst).
  
  IF NOT PrescanIncludedInMain(cPreScanBuffLst) THEN 
    ttQueryBufferLists.bMandatory = YES.

  LogThis("Prescan query def:",cPreScanQuery,0).
  LogThis("Prescan qry buffs:",cPreScanBuffLst,IF ttQueryBufferLists.bPassExec THEN 0 ELSE 1).
  IF ttQueryBufferLists.bPassExec AND NOT bSkipPrescan AND NOT ttQueryBufferLists.bMandatory THEN DO:
    LogThis("        NOTE:","Last buffer in prescan query not equal to query base table. Query will not be executed.",0).
    LogThis("","This is a valid situation if the buffer sequence has been modified",1).
  END.
  ELSE IF NOT ttQueryBufferLists.bMandatory AND bSkipPrescan THEN 
    LogThis("Prescan skipped:","Prescan skipped since primary buffer query is sufficiently fast and all prescan buffers are included in main query",0).
  ELSE IF ttQueryBufferLists.bMandatory AND NOT bPassExecution THEN
    LogThis("Prescan mandatory:","Prescan is mandatory even if primary buffer query is sufficiently fast since not all prescan buffers are included in main query",0).
  ELSE IF ttQueryBufferLists.bMandatory AND bPassExecution THEN DO:
    LogThis("INVALID PRESCAN:","Prescan is defined on buffer(s) not included in main query with altered buffer sequence",0).
    RETURN "Prescan is defined on buffer(s) not included in main query with altered buffer sequence" + CHR(10) + cPreScanQuery.
  END.

END.

ReduceQueries(1).

/* Check only first query component first: */
FOR EACH ttQueryBufferLists 
    WHERE NOT ttQueryBufferLists.bPassExec
      AND NOT ttQueryBufferLists.bMandatory
    :
  ASSIGN cPreScanQuery  = "FOR EACH " + REPLACE(REPLACE(ttQueryBufferLists.cFirstQueryComp,"¤",","),CHR(3),"|")
         bPassExecution = NO.

  CREATE QUERY hPreScanQuery.
  hPreScanQuery:FORWARD-ONLY = YES.

  IF ENTRY(1,ttQueryBufferLists.cQryBufferList) MATCHES "buf*_*" THEN 
    CREATE BUFFER hPreScanBuff[1] FOR TABLE SUBSTR(ENTRY(1,ttQueryBufferLists.cQryBufferList),6)
                  BUFFER-NAME ENTRY(1,ttQueryBufferLists.cQryBufferList).
  ELSE
    CREATE BUFFER hPreScanBuff[1] FOR TABLE ENTRY(1,ttQueryBufferLists.cQryBufferList).

/*   CREATE BUFFER hPreScanBuff[1] FOR TABLE ENTRY(1,ttQueryBufferLists.cQryBufferList). */
  hPreScanQuery:ADD-BUFFER(hPreScanBuff[1]).

  LogThis("Prescan first buffer:",cPreScanQuery,0).

  bOk = hPreScanQuery:QUERY-PREPARE(cPreScanQuery) NO-ERROR.
  IF NOT bOk THEN DO:
    DELETE OBJECT httPreScanJoin.
    DELETE OBJECT hPreScanQuery.
    RETURN "Error in prescan query definition: Invalid querystring: " + cPreScanQuery + CHR(10) + "Buffer: " + ENTRY(1,ttQueryBufferLists.cQryBufferList). 
  END.
  
  LogThis("Prescan one buffer index",hPreScanQuery:INDEX-INFORMATION(1),0).

  FIND FIRST ttQuery
       WHERE ttQuery.iQueryNum     = 0
         AND ttQuery.iQryBufferNum = LOOKUP(ENTRY(1,ttQueryBufferLists.cQryBufferList),cBufferList)
       NO-ERROR.
  IF AVAIL ttQuery AND ttQuery.cQryJoinString BEGINS ",EACH " THEN DO:
    IF hPreScanQuery:INDEX-INFORMATION(1) BEGINS "whole-index" THEN DO:
      ttQueryBufferLists.bPassExec = YES.
      LogThis("Prescan excecution","skipped due do no index match",1).
    END.
    ELSE 
      ttQueryBufferLists.bPassExec = TooManyHits(hPreScanQuery).
  END.
  ELSE ttQueryBufferLists.bPassExec = TooManyHits(hPreScanQuery).

/*   ttQueryBufferLists.bPassExec = TooManyHits(hPreScanQuery). */

  DELETE OBJECT hPreScanBuff[1].
  DELETE OBJECT hPreScanQuery.
END.

/* If more than two components, check the first two: */
FOR EACH ttQueryBufferLists 
    WHERE NOT ttQueryBufferLists.bPassExec
      AND ttQueryBufferLists.iNumQryBuffs > 2
      AND NOT ttQueryBufferLists.bMandatory
    :
  ASSIGN cPreScanQuery  = REPLACE("FOR EACH " + REPLACE(ttQueryBufferLists.cFirstQueryComp,"¤",",") + "," + REPLACE(ttQueryBufferLists.cSecondQueryComp,"¤",","),CHR(3),"|")
         bPassExecution = NO.

  CREATE QUERY hPreScanQuery.
  hPreScanQuery:FORWARD-ONLY = YES.

  DO iy = 1 TO 2:
    IF ENTRY(iy,ttQueryBufferLists.cQryBufferList) MATCHES "buf*_*" THEN 
      CREATE BUFFER hPreScanBuff[iy] FOR TABLE SUBSTR(ENTRY(iy,ttQueryBufferLists.cQryBufferList),6)
                    BUFFER-NAME ENTRY(iy,ttQueryBufferLists.cQryBufferList).
    ELSE
      CREATE BUFFER hPreScanBuff[iy] FOR TABLE ENTRY(iy,ttQueryBufferLists.cQryBufferList).
    hPreScanQuery:ADD-BUFFER(hPreScanBuff[iy]).
  END.
  hPreScanQuery:SET-BUFFERS(hPreScanBuff[1],hPreScanBuff[2]).

  LogThis("Prescan first two buffers:",cPreScanQuery,1).

  bOk = hPreScanQuery:QUERY-PREPARE(cPreScanQuery) NO-ERROR.
  IF NOT bOk THEN DO:
    DELETE OBJECT httPreScanJoin.
    DELETE OBJECT hPreScanQuery.
    RETURN "Error in prescan query definition: Invalid querystring: " + cPreScanQuery + CHR(10) + "Buffer: " + ENTRY(1,ttQueryBufferLists.cQryBufferList). 
  END.
  
  LogThis("Prescan two buffers index",hPreScanQuery:INDEX-INFORMATION(1),0).
  LogThis("",hPreScanQuery:INDEX-INFORMATION(2),1).

  IF ttQueryBufferLists.cSecondQueryComp BEGINS "EACH " AND hPreScanQuery:INDEX-INFORMATION(2) BEGINS "whole-index" THEN DO:
    ttQueryBufferLists.bPassExec = YES.
    LogThis("Prescan excecution","skipped due do no index match",1).
  END.
  ELSE       
    ttQueryBufferLists.bPassExec = TooManyHits(hPreScanQuery).

  DELETE OBJECT hPreScanBuff[1].
  DELETE OBJECT hPreScanBuff[2].
  DELETE OBJECT hPreScanQuery.
END.

/* Check prescan for whole queries */
FOR EACH ttQueryBufferLists WHERE NOT ttQueryBufferLists.bPassExec OR ttQueryBufferLists.bMandatory:
  
  ASSIGN cPreScanQuery  = FixQuery(ComposeQuery(ttQueryBufferLists.iQueryNum))
         bPassExecution = NO.

  CREATE QUERY hPreScanQuery.
  hPreScanQuery:FORWARD-ONLY = YES.
  DO iy = 1 TO ttQueryBufferLists.iNumQryBuffs:
    IF ENTRY(iy,ttQueryBufferLists.cQryBufferList) MATCHES "buf*_*" THEN 
      CREATE BUFFER hPreScanBuff[iy] FOR TABLE SUBSTR(ENTRY(iy,ttQueryBufferLists.cQryBufferList),6)
                    BUFFER-NAME ENTRY(iy,ttQueryBufferLists.cQryBufferList).
    ELSE
      CREATE BUFFER hPreScanBuff[iy] FOR TABLE ENTRY(iy,ttQueryBufferLists.cQryBufferList).
    hPreScanQuery:ADD-BUFFER(hPreScanBuff[iy]).
  END.

  FIND FIRST ttQuery
       WHERE ttQuery.iQueryNum = 0
         AND ttQuery.iQryBufferNum = 1. 
  cPreScanQuery = REPLACE(REPLACE("FOR EACH " + hPreScanBuff[1]:NAME + " " + cPreScanQuery,CHR(1),","),CHR(3),"|").

  LogThis("Prescan query exec:",cPreScanQuery,0).

  bOk = hPreScanQuery:QUERY-PREPARE(cPreScanQuery) NO-ERROR.
  IF NOT bOk THEN DO:
    DELETE OBJECT httPreScanJoin.
    DELETE OBJECT hPreScanQuery.
    DO iy = 2 TO iCountPrescan:
      DELETE OBJECT httAddPreScan[iy] NO-ERROR.
    END.
    RETURN "Error in prescan query definition: Invalid querystring: " + cPreScanQuery + CHR(10) + "Bufferlist: " + ttQueryBufferLists.cQryBufferList. 
  END.
  
  DO iy = 1 TO ttQueryBufferLists.iNumQryBuffs:
    LogThis("Prescan index usage",hPreScanQuery:INDEX-INFORMATION(iy),0).
  END.

  IF NOT ttQueryBufferLists.bMandatory /* AND ttQueryBufferLists.iNumQryBuffs > 2 */ THEN DO:
    FIND FIRST ttQuery
         WHERE ttQuery.iQueryNum     = 0
           AND ttQuery.iQryBufferNum = LOOKUP(ENTRY(1,ttQueryBufferLists.cQryBufferList),cBufferList)
         NO-ERROR.
    IF AVAIL ttQuery AND ttQuery.cQryJoinString BEGINS ",EACH " THEN DO:
      IF hPreScanQuery:INDEX-INFORMATION(1) BEGINS "whole-index" THEN DO:
        bPassExecution = YES.
        LogThis("Prescan excecution","skipped due do no index match",1).
      END.
      ELSE 
        bPassExecution = TooManyHits(hPreScanQuery).
    END.
    ELSE bPassExecution = TooManyHits(hPreScanQuery).
  END.
  ELSE bPassExecution = NO.

  IF NOT bPassExecution THEN DO:
    bPreScanExec  = YES.

    bOk = hPreScanQuery:QUERY-OPEN() NO-ERROR.
    IF NOT bOk THEN DO:
      DELETE OBJECT httPreScanJoin.
      DELETE OBJECT hPreScanQuery.
      DO iy = 2 TO iCountPrescan:
        DELETE OBJECT httAddPreScan[iy] NO-ERROR.
      END.
      RETURN "Error in prescan query definition: Invalid querystring: " + CHR(10) + cPreScanQuery + CHR(10) + " Bufferlist: " + ttQueryBufferLists.cQryBufferList. 
    END.
    iCountPrescan = iCountPrescan + 1.

    IF iCountPrescan > 1 THEN DO:
      CREATE TEMP-TABLE httAddPreScan[iCountPrescan].
      httAddPreScan[iCountPrescan]:ADD-NEW-FIELD("cPreScanJoin","CHARACTER").
      httAddPreScan[iCountPrescan]:ADD-NEW-INDEX("idxRowid",YES,YES).
      httAddPreScan[iCountPrescan]:ADD-INDEX-FIELD("idxRowid","cPreScanJoin").
      httAddPreScan[iCountPrescan]:TEMP-TABLE-PREPARE("ttPreScanJoin" + STRING(iCountPrescan)) NO-ERROR.
      ASSIGN hAddPreScanBuff[iCountPrescan]   = httAddPreScan[iCountPrescan]:DEFAULT-BUFFER-HANDLE
             hAddPreScanRowid[iCountPrescan]  = hAddPreScanBuff[iCountPrescan]:BUFFER-FIELD("cPreScanJoin").
    END.

    hCurrRowIdBuff = hPreScanBuff[ttQueryBufferLists.iNumQryBuffs].
    hPreScanQuery:GET-FIRST().
    REPEAT WHILE NOT hPreScanQuery:QUERY-OFF-END:
      bOk = hBuffPreScanJoin:FIND-FIRST("WHERE cPreScanJoin = '" + STRING(hCurrRowIdBuff:ROWID) + "'") NO-ERROR.
      IF NOT bOk AND iCountPrescan = 1 THEN DO:
        hBuffPreScanJoin:BUFFER-CREATE().
        ASSIGN hRowid:BUFFER-VALUE = hCurrRowIdBuff:ROWID
               hcRowid:BUFFER-VALUE = STRING(hCurrRowIdBuff:ROWID) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN 
          iPreScanCount = iPreScanCount + 1.
      END.
      ELSE IF iCountPrescan > 1 THEN DO:
        bOk = hAddPreScanBuff[iCountPrescan]:FIND-FIRST("WHERE cPreScanJoin = '" + STRING(hCurrRowIdBuff:ROWID) + "'") NO-ERROR.
        IF NOT bOk THEN DO:
          hAddPreScanBuff[iCountPrescan]:BUFFER-CREATE().
          hAddPreScanRowid[iCountPrescan]:BUFFER-VALUE = hcRowid:BUFFER-VALUE NO-ERROR.
        END.
      END.
      hPreScanQuery:GET-NEXT().
      IF iPreScanCount > 10000000 THEN LEAVE.
    END.
  END.
  DO iy = 1 TO ttQueryBufferLists.iNumQryBuffs:
    DELETE OBJECT hPreScanBuff[iy].
  END.
  DELETE OBJECT hPreScanQuery.
END.

IF bPreScanExec THEN DO:
  IF iCountPrescan > 1 THEN DO:
    LogThis("1st prescan count:",STRING(iPreScanCount),1).
    CREATE QUERY hPreScanQuery.
    hPreScanQuery:FORWARD-ONLY = YES.
    hPreScanQuery:SET-BUFFERS(hBuffPrescanJoin).
    hPreScanQuery:QUERY-PREPARE("FOR EACH ttPreScanJoin").
    hPreScanQuery:QUERY-OPEN().
    hPreScanQuery:GET-FIRST().
    REPEAT WHILE NOT hPreScanQuery:QUERY-OFF-END:
      DO iy = 2 TO iCountPrescan:
        bOk = hAddPreScanBuff[iy]:FIND-UNIQUE("WHERE cPreScanJoin = '" + hcRowid:BUFFER-VALUE + "'") NO-ERROR.
        IF NOT bOk THEN DO:
          hBuffPrescanJoin:BUFFER-DELETE().
          iPreScanCount = iPreScanCount - 1.
          LEAVE.
        END.
      END.
      hPreScanQuery:GET-NEXT().
    END.
    DO iy = 2 TO iCountPrescan:
      DELETE OBJECT httAddPreScan[iy] NO-ERROR.
    END.
    DELETE OBJECT hPreScanQuery.
  END.

  DELETE OBJECT hQuery.
  CREATE QUERY hQuery.
  hQuery:ADD-BUFFER(hBuffPreScanJoin).
  DO ix = 1 TO iBufferCount: 
    IF VALID-HANDLE(hBuffer[ix]) THEN
      hQuery:ADD-BUFFER(hBuffer[ix]).
    ELSE LEAVE.
  END.
  
  IF icQueryCriteria BEGINS "WHERE" THEN
    icQueryCriteria = " AND" + SUBSTR(icQueryCriteria,6).
END.

CREATE ttQueryBufferLists.
ASSIGN ttQueryBufferLists.iQueryNum      = 0
       ttQueryBufferLists.cQryBufferList = cBufferList
       ttQueryBufferLists.iNumQryBuffs   = iBufferCount.

ReduceQueries(0).

IF bPreScanExec THEN
  cQueryString = REPLACE("FOR EACH ttPreScanJoin,EACH " + ENTRY(1,cBufferList)
               + " WHERE ROWID(" + ENTRY(1,cBufferList) + ") = ttPreScanJoin.rPreScanJoin "
               +  ComposeQuery(0),CHR(1),",").
ELSE DO:
  cQueryString = "FOR EACH " + ENTRY(1,cBufferList) + " " + REPLACE(ComposeQuery(-1),CHR(1),",").
  IF NOT cQueryString MATCHES "*" + cSortPhrase THEN
    cQueryString = cQueryString + cSortPhrase.
END.

LogThis("Prescan query count:",STRING(iPreScanCount),1).

LogThis("Tot sec, prescan queries: ", STRING(TIME - iStartTime),2).

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-QueryOpen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION QueryOpen Procedure 
FUNCTION QueryOpen RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNoAccessFields AS CHAR NO-UNDO.

bOk = hQuery:QUERY-PREPARE(cQueryString) NO-ERROR.
IF NOT bOk THEN DO:
  cQueryString = FixQueryOperators(cQueryString).
  bOk = hQuery:QUERY-PREPARE(cQueryString) NO-ERROR.
  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
    LogThis("Query error",ERROR-STATUS:GET-MESSAGE(1),1).
    IF ERROR-STATUS:GET-NUMBER(1) = 7328 AND
       ENTRY(1,TRIM(ERROR-STATUS:GET-MESSAGE(1))," ") = ENTRY(NUM-ENTRIES(cQueryString," "),cQueryString," ")
       THEN DO:
      cQueryString = SUBSTR(cQueryString,1,INDEX(cQueryString," BY ")).           
      LogThis("Removed sort phrase",cQueryString,1).
      bOk = hQuery:QUERY-PREPARE(cQueryString) NO-ERROR.
    END.
  END.
END.
IF NOT bOk THEN DO:
  IF ERROR-STATUS:GET-NUMBER(1) = 233 THEN DO:
    cNoAccessFields = CheckFieldLevelSecurity("repair").
    DO ix = 1 TO NUM-ENTRIES(cNoAccessFields):
      IF CAN-DO(cSortFields,ENTRY(ix,cNoAccessFields)) THEN DO:
        cQueryString = SUBSTR(cQueryString,1,INDEX(cQueryString," BY ")).
        LEAVE.
      END.
    END.
    bOk = hQuery:QUERY-PREPARE(cQueryString) NO-ERROR.
    LogThis("Corrected query string",cQueryString,1).
    IF ERROR-STATUS:GET-NUMBER(1) = 233 THEN 
      RETURN REPLACE(ERROR-STATUS:GET-MESSAGE(1),"Insufficient access","Insufficient read access").           .
  END.
  ELSE IF ERROR-STATUS:GET-NUMBER(1) = 234 THEN
    RETURN REPLACE(ERROR-STATUS:GET-MESSAGE(1),"Insufficient access","Insufficient read access").
  ELSE 
    RETURN 
/*            icBuffersAndFields + CHR(10) + CHR(10) + */
           "Bufferlist:  " + cBufferList + CHR(10) + CHR(10) + 
           "Querystring:" + CHR(10) + cQueryString + CHR(10) + CHR(10) +
           ERROR-STATUS:GET-MESSAGE(1)
           .
END.

IF cLogFile NE "" THEN DO:
  LogThis("Index usage:","",0).
  DO iy = 1 TO hQuery:NUM-BUFFERS:
    LogThis(hQuery:GET-BUFFER-HANDLE(iy):NAME,hQuery:INDEX-INFORMATION(iy),0).
  END.
  LogThis("","",1).
END.

bOk = hQuery:QUERY-OPEN() NO-ERROR.
IF NOT bOK AND ERROR-STATUS:GET-NUMBER(1) = 141 THEN DO: /* Index field too long */
  cSortString = "SUBSTR(".
  DO iy = INDEX(cQueryString," BY ") + 4 TO LENGTH(cQueryString):
    cSortString = cSortString + IF SUBSTR(cQueryString,iy,1) = " " THEN ",1,50) "
                                ELSE SUBSTR(cQueryString,iy,1).
  END.
  IF R-INDEX(cQueryString,"DESC") = 0 THEN
    cSortString = cSortString + ",1,50) ".
  SUBSTR(cQueryString,INDEX(cQueryString," BY ") + 4) = cSortString.
  hQuery:QUERY-PREPARE(cQueryString) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
    RETURN cQueryString + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
  bOk = hQuery:QUERY-OPEN() NO-ERROR.
END.
IF NOT bOk THEN 
  RETURN "FOR EACH " + ENTRY(1,ENTRY(1,icBuffersAndFields),";") + " NO-LOCK " + icQueryCriteria + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReduceQueries) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReduceQueries Procedure 
FUNCTION ReduceQueries RETURNS LOGICAL
  ( INPUT iiMinQueryNum AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  If possible reduce number of prescan join queries. When all buffers in one query
            are included in another the queries can be merged, ie the one with lesser buffers 
            is deleted after it's critera has been merged with more buffers.
            If the query cannot be delete it will still be added criteria for matching buffers
            
            Called twice from ProcessPreScanQueries: 
              First to reduce the number of pre-scan queries.
              Second to merge the remaining prescan criteria to the orginal query 
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bIncluded   AS LOG  NO-UNDO.
FOR EACH ttQueryBufferLists 
    WHERE ttQueryBufferLists.iQueryNum GE iiMinQueryNum
    BY ttQueryBufferLists.iNumQryBuffs DESC:

  FOR EACH bttQueryBufferLists
      WHERE bttQueryBufferLists.iQueryNum GE iiMinQueryNum
        AND ROWID(bttQueryBufferLists) NE ROWID(ttQueryBufferLists):

    bIncluded = YES.
    DO ix = 1 TO NUM-ENTRIES(bttQueryBufferLists.cQryBufferList):
      bIncluded = CAN-DO(ttQueryBufferLists.cQryBufferList,ENTRY(ix,bttQueryBufferLists.cQryBufferList)).
      IF NOT bIncluded THEN LEAVE.
    END.

    FOR EACH bttQuery
        WHERE bttQuery.iQueryNum  = bttQueryBufferLists.iQueryNum            
          AND bttQuery.cQryString NE "":
      FIND FIRST ttQuery
           WHERE ttQuery.iQueryNum     = ttQueryBufferLists.iQueryNum
             AND ttQuery.cQryBuffer    = bttQuery.cQryBuffer
/*              AND ttQuery.iQryBufferNum = bttQuery.iQryBufferNum */
           NO-ERROR.
      
      IF AVAIL ttQuery THEN DO:
        IF ttQuery.cQryString MATCHES "* WHERE *" THEN
          ttQuery.cQryString = REPLACE(ttQuery.cQryString + ") AND (" + bttQuery.cQryString + ")"," WHERE "," WHERE (").
        ELSE IF ttQuery.cQryJoinString MATCHES "* WHERE *" THEN DO:
          IF bttQuery.cQryString BEGINS "WHERE " THEN
            ttQuery.cQryString = " AND (" + SUBSTR(bttQuery.cQryString,6) + ")".
          ELSE
            ttQuery.cQryString = REPLACE(ttQuery.cQryString + " AND (" + bttQuery.cQryString + ")"," WHERE "," ").
        END.
        ELSE 
          ttQuery.cQryString = TRIM(ttQuery.cQryString + " " + bttQuery.cQryString).

        ttQuery.bOuterJoin = NO.
        ttQuery.cQryString = FixQuery(ttQuery.cQryString).
      END.
    END. 
    IF bIncluded AND bttQueryBufferLists.iQueryNum > 0 THEN DO:
      IF iiMinQueryNum > 0 THEN
        LogThis("Redundant prescan:",bttQueryBufferLists.cFirstQueryComp + "," + bttQueryBufferLists.cSecondQueryComp,0).
      FOR EACH bttQuery
          WHERE bttQuery.iQueryNum = bttQueryBufferLists.iQueryNum:
        DELETE bttQuery.
      END.
      DELETE bttQueryBufferLists.
    END.
  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCalcFieldFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCalcFieldFilter Procedure 
FUNCTION setCalcFieldFilter RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNewList  AS CHAR NO-UNDO.
DEF VAR cNewValue AS CHAR NO-UNDO.

cCalcFieldFilter = REPLACE(cCalcFieldFilter,CHR(1),",").
cCalcFieldFilter = REPLACE(cCalcFieldFilter,CHR(3),";").

IF cCalcFieldFilter = "¤=¤" THEN RETURN "".
        
DO ix = 1 TO NUM-ENTRIES(cCalcFieldFilter,"|"):
  IF TRIM(ENTRY(ix,cCalcFieldFilter,"|")) = "" THEN NEXT.

  IF NUM-ENTRIES(ENTRY(ix,cCalcFieldFilter,"|"),"¤") NE 3 THEN
    RETURN "Error in definition of filter for calculated field: " + ENTRY(ix,cCalcFieldFilter,"|") + CHR(10) +
           "Fields must defined as <field>¤<operator>¤<value>".
  FIND FIRST ttSpecFlds
       WHERE ttSpecFlds.cFillField = TRIM(ENTRY(1,ENTRY(ix,cCalcFieldFilter,"|"),"¤"))
       NO-ERROR.
  IF NOT AVAIL ttSpecFlds THEN 
    RETURN "Error in definition of filter for calculated field: " + ENTRY(1,ENTRY(ix,cCalcFieldFilter,"|"),"¤") + CHR(10) +
           "Field is not defined as a calculated"
           .
  ASSIGN ttSpecFlds.cFilterOperator = ttSpecFlds.cFilterOperator + TRIM(ENTRY(2,ENTRY(ix,cCalcFieldFilter,"|"),"¤")) + ","
         ttSpecFlds.cFilterValue    = ttSpecFlds.cFilterValue + TRIM(ENTRY(3,ENTRY(ix,cCalcFieldFilter,"|"),"¤")) + "|"
         cNewList                        = cNewList + TRIM(ENTRY(1,ENTRY(ix,cCalcFieldFilter,"|"),"¤")) + ",".
END.

FOR EACH ttSpecFlds:
  ASSIGN ttSpecFlds.cFilterOperator = TRIM(ttSpecFlds.cFilterOperator,",")
         ttSpecFlds.cFilterValue    = TRIM(ttSpecFlds.cFilterValue,"|").
END.

ASSIGN bCalcFieldFilter = YES
       cCalcFieldFilter = TRIM(cNewList,",").

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setCheckForSkipRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCheckForSkipRow Procedure 
FUNCTION setCheckForSkipRow RETURNS LOGICAL
  ( INPUT ibCheckForSkipRow AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose: Enforce an accurat count based on return values from calculated fields 
    Notes: May be a performance killer since the query now has to check all rows
           for calculated fields
------------------------------------------------------------------------------*/
IF ibCheckForSkipRow NE ? THEN
  bCheckForSkipRow = ibCheckForSkipRow.

RETURN bCheckForSkipRow.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldBufferSwap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldBufferSwap Procedure
FUNCTION setFieldBufferSwap RETURNS LOGICAL 
  (INPUT icBufferName   AS CHAR,
   INPUT iiBufferNum    AS INT,
   INPUT ihBufferField  AS HANDLE):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE result AS LOGICAL NO-UNDO.

    RETURN result.

END FUNCTION.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setJbCalcParams) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setJbCalcParams Procedure 
FUNCTION setJbCalcParams RETURNS CHARACTER
  ( INPUT icCalcProc  AS CHAR,
    INPUT iiBufferNum AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFld1             AS CHAR NO-UNDO.
DEF VAR cFld2             AS CHAR NO-UNDO.
DEF VAR cOper             AS CHAR NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iy                AS INT  NO-UNDO.
DEF VAR cParam            AS CHAR NO-UNDO.
DEF VAR cFirst            AS CHAR NO-UNDO.
DEF VAR cTargetBuff       AS CHAR NO-UNDO.
DEF VAR cElement          AS CHAR   NO-UNDO.
DEF VAR hFld              AS HANDLE NO-UNDO.
DEF VAR cPriKeyFlds       AS CHAR NO-UNDO.
DEF VAR cQueryExp         AS CHAR NO-UNDO.
DEF VAR cTargetFields     AS CHAR NO-UNDO.
DEF VAR cQuery            AS CHAR NO-UNDO.
DEF VAR cTmpCallProcParam AS CHAR NO-UNDO.
DEF VAR cFunction         AS CHAR NO-UNDO.

DEF BUFFER bttSpecFlds FOR ttSpecFlds.

CASE icCalcProc:
  WHEN "jb_calc" THEN DO:
    cTmpCallProcParam = ttSpecFlds.cCallProcParam.
    IF INDEX(ttSpecFlds.cCallProcParam," CONCAT") > 0 THEN DO:
      ASSIGN cFld1 = TRIM(ENTRY(1,ttSpecFlds.cCallProcParam," CONCAT"))
             cFld2 = SUBSTR(ttSpecFlds.cCallProcParam,INDEX(ttSpecFlds.cCallProcParam," CONCAT") + 7)
             ttSpecFlds.cCallProcParam = "CONCAT"
             .
      IF NUM-ENTRIES(cFld2," ") > 1 THEN 
        ASSIGN ttSpecFlds.cCallProcParam = "CONCAT" + SUBSTR(cFld2,1,R-INDEX(cFld2," "))
               cFld2 = SUBSTR(cFld2,R-INDEX(cFld2," ") + 1)
               .
    END.
    ELSE IF INDEX(ttSpecFlds.cCallProcParam," * ") > 0 THEN
      ASSIGN cFld1 = TRIM(ENTRY(1,ttSpecFlds.cCallProcParam," * "))
             cFld2 = TRIM(SUBSTR(ttSpecFlds.cCallProcParam,INDEX(ttSpecFlds.cCallProcParam," * ") + 2))
             ttSpecFlds.cCallProcParam = "*"
             .
    ELSE IF INDEX(ttSpecFlds.cCallProcParam," % ") > 0 THEN
      ASSIGN cFld1 = TRIM(ENTRY(1,ttSpecFlds.cCallProcParam," % "))
             cFld2 = TRIM(SUBSTR(ttSpecFlds.cCallProcParam,INDEX(ttSpecFlds.cCallProcParam," % ") + 2))
             ttSpecFlds.cCallProcParam = "%"
             .
    ELSE IF INDEX(ttSpecFlds.cCallProcParam," / ") > 0 THEN
      ASSIGN cFld1 = TRIM(ENTRY(1,ttSpecFlds.cCallProcParam," / "))
             cFld2 = TRIM(SUBSTR(ttSpecFlds.cCallProcParam,INDEX(ttSpecFlds.cCallProcParam," / ") + 2))
             ttSpecFlds.cCallProcParam = "/"
             .
    ELSE IF INDEX(ttSpecFlds.cCallProcParam," + ") > 0 THEN
      ASSIGN cFld1 = TRIM(ENTRY(1,ttSpecFlds.cCallProcParam," + "))
             cFld2 = TRIM(SUBSTR(ttSpecFlds.cCallProcParam,INDEX(ttSpecFlds.cCallProcParam," + ") + 2))
             ttSpecFlds.cCallProcParam = "+"
             .
    ELSE IF INDEX(ttSpecFlds.cCallProcParam," - ") > 0 THEN
      ASSIGN cFld1 = TRIM(ENTRY(1,ttSpecFlds.cCallProcParam," - "))
             cFld2 = TRIM(SUBSTR(ttSpecFlds.cCallProcParam,INDEX(ttSpecFlds.cCallProcParam," - ") + 2))
             ttSpecFlds.cCallProcParam = "-"
             .
    ELSE IF INDEX(ttSpecFlds.cCallProcParam," MOD ") > 0 THEN
      ASSIGN cFld1 = TRIM(ENTRY(1,ttSpecFlds.cCallProcParam," MOD "))
             cFld2 = TRIM(SUBSTR(ttSpecFlds.cCallProcParam,INDEX(ttSpecFlds.cCallProcParam," MOD ") + 4))
             ttSpecFlds.cCallProcParam = "MOD"
             .
    ELSE IF NUM-ENTRIES(ttSpecFlds.cCallProcParam," ") > 2 AND ENTRY(3,ttSpecFlds.cCallProcParam," ") = "TODAY" THEN DO:
      ASSIGN cFld1 = ENTRY(1,ttSpecFlds.cCallProcParam," ")
             cFld2 = ENTRY(5,ttSpecFlds.cCallProcParam," ")
             ttSpecFlds.cCallProcParam = ENTRY(2,ttSpecFlds.cCallProcParam," ") + " TODAY " + ENTRY(4,ttSpecFlds.cCallProcParam," ")
             NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:       
        LogThis("Warning","Invalid parameter spesification for jb_calc: " + ttSpecFlds.cCallProcParam + ": " + ERROR-STATUS:GET-MESSAGE(1),0).
        RETURN "".
      END.  
    END.          
    ELSE DO:
       LogThis("Warning","Invalid parameter spesification for jb_calc: " + ttSpecFlds.cCallProcParam,0).
       RETURN "".
    END.   

    IF iiBufferNum > 0 THEN DO: 
      IF NUM-ENTRIES(cFld1,".") = 1 THEN DO:
        ttSpecFlds.hSourceField = hBuffer[iiBufferNum]:BUFFER-FIELD(cFld1) NO-ERROR.
        IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN 
          ttSpecFlds.cCallProcParam = cTmpCallProcParam.  /* <- To be able to try again with calc.field from the output TT */               
      END.
      ELSE ttSpecFlds.cCallProcParam = cTmpCallProcParam.  /* <- To be able to try again with field from specific buffer */               
      IF NUM-ENTRIES(cFld2,".") = 1 THEN DO:
        ttSpecFlds.hSourceField2 = hBuffer[iiBufferNum]:BUFFER-FIELD(cFld2) NO-ERROR.
        IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN
          ttSpecFlds.cCallProcParam = cTmpCallProcParam.  /* <- To be able to try again with calc.field from the output TT */               
      END.
      ELSE ttSpecFlds.cCallProcParam = cTmpCallProcParam.  /* <- To be able to try again with field from specific buffer */               
    END.
    ELSE DO:
      IF NOT VALID-HANDLE(ttSpecFlds.hSourceField) THEN DO:
        IF NUM-ENTRIES(cFld1,".") > 1 THEN 
          ttSpecFlds.hSourceField = hBuffer[LOOKUP(ENTRY(1,cFld1,"."),cBufferList)]:BUFFER-FIELD(ENTRY(2,cFld1,".")).   
        ELSE DO:
          ttSpecFlds.hSourceField = httBuffer:BUFFER-FIELD(cFld1).
          FIND FIRST bttSpecFlds
               WHERE bttSpecFlds.cFillField = cFld1
               NO-ERROR.
          bttSpecFlds.bDirectAssign  = YES.                /* <- Calc.field must be assigned directly after calculation */  
        END.
      END.

      IF NOT VALID-HANDLE(ttSpecFlds.hSourceField2) THEN DO:
        IF NUM-ENTRIES(cFld2,".") > 1 THEN 
          ttSpecFlds.hSourceField2 = hBuffer[LOOKUP(ENTRY(1,cFld2,"."),cBufferList)]:BUFFER-FIELD(ENTRY(2,cFld2,".")).   
        ELSE DO:
          ttSpecFlds.hSourceField2 = httBuffer:BUFFER-FIELD(cFld2).
          FIND FIRST bttSpecFlds
               WHERE bttSpecFlds.cFillField = cFld2
               NO-ERROR.
          bttSpecFlds.bDirectAssign  = YES.                /* <- Calc.field must be assigned directly after calculation */  
        END.
      END.

      /*
      IF NOT VALID-HANDLE(ttSpecFlds.hSourceField) THEN DO:
        ttSpecFlds.hSourceField  = httBuffer:BUFFER-FIELD(ENTRY(NUM-ENTRIES(cFld1,"."),cFld1,".")
                                                          ) NO-ERROR.
        FIND FIRST bttSpecFlds
             WHERE bttSpecFlds.cFillField = ttSpecFlds.hSourceField:NAME
             .
        bttSpecFlds.bDirectAssign  = YES.                /* <- Field must be assigned directly after calculation */  
      END.
      IF NOT VALID-HANDLE(ttSpecFlds.hSourceField2) THEN DO:
        ttSpecFlds.hSourceField2 = httBuffer:BUFFER-FIELD(ENTRY(NUM-ENTRIES(cFld2,"."),cFld2,".")) NO-ERROR.
        FIND FIRST bttSpecFlds
             WHERE bttSpecFlds.cFillField = ttSpecFlds.hSourceField:NAME
             NO-ERROR.
        IF AVAIL bttSpecFlds THEN
          bttSpecFlds.bDirectAssign  = YES.         /* <- If first field in calculation is calculated it must be assigned right after calculation */  
      END.
      */
    END.
  END.

  WHEN "jb_can-find" THEN DO:
    cParam = TRIM(ttSpecFlds.cCallProcParam).
    DO ix = 1 TO 5:
      cParam = REPLACE(cParam,"  "," ").
    END.
    IF cParam BEGINS "FIRST " THEN cParam = SUBSTRING(cParam,7).
    
    DO ix = 1 TO NUM-ENTRIES(cParam," "):
      ASSIGN cElement  = ENTRY(ix,cParam," ")
             cFunction = "".
             
      IF ix = 1 THEN DO:
        cTargetBuff = cElement.
        CREATE BUFFER ttSpecFlds.hTargetBuff FOR TABLE cElement NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          RETURN "Error in can-find phrase for calculated field" + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
      END.
      ELSE IF ix = 2 AND cElement = "OF" THEN DO:
        cPriKeyFlds = getPrimaryKeyFields(hBuffer[iiBufferNum]).
        cQueryExp = "WHERE ".
        DO iy = 1 TO NUM-ENTRIES(cPriKeyFlds):
          cQueryExp = cQueryExp 
                    + (IF cQueryExp BEGINS "WHERE " THEN "" ELSE " AND ")
                    + cTargetBuff + "." + ENTRY(iy,cPriKeyFlds) + " = "
                    + (IF hBuffer[iiBufferNum]:BUFFER-FIELD(ENTRY(iy,cPriKeyFlds)):DATA-TYPE = "CHARACTER" THEN "'" ELSE "")
                    + "¤" + ENTRY(iy,cPriKeyFlds) + "¤"
                    + (IF hBuffer[iiBufferNum]:BUFFER-FIELD(ENTRY(iy,cPriKeyFlds)):DATA-TYPE = "CHARACTER" THEN "'" ELSE "")
                    .
        END.
      END.
      ELSE IF ix = 2 AND cElement = "WHERE" THEN
        cQueryExp = "WHERE ".
      ELSE IF ix = 2 THEN 
        RETURN "Invalid can-find expression. Target buffer name must be followed by OF or WHERE".
      ELSE IF ix > 2 THEN DO:
        IF ix = 3 AND ENTRY(2,cParam," ") = "OF" THEN NEXT.

        IF cElement = "WHERE" THEN cElement = "AND".

        IF NOT CAN-DO("=,<,>,<>,EQ,GT,LT,NE,AND,OR,(,),'",cElement) THEN DO:
          IF cElement BEGINS "STRING(" THEN 
            ASSIGN cFunction = "STRING"
                   cElement = TRIM(SUBSTR(cElement,8),")").
          IF NUM-ENTRIES(cElement,".") > 1 THEN cElement = ENTRY(2,cElement,".").
          hFld = ttSpecFlds.hTargetBuff:BUFFER-FIELD(cElement) NO-ERROR.
          IF VALID-HANDLE(hFld) AND NOT CAN-DO(cTargetFields,cElement) THEN
            ASSIGN cQueryExp   = cQueryExp + ENTRY(ix,cParam," ")
                   cTargetFields = cTargetFields + (IF cTargetFields NE "" THEN "," ELSE "") + cElement.
          ELSE DO:
            hFld = hBuffer[iiBufferNum]:BUFFER-FIELD(cElement) NO-ERROR.
            IF VALID-HANDLE(hFld) THEN
              cQueryExp = cQueryExp 
                          + (IF hBuffer[iiBufferNum]:BUFFER-FIELD(cElement):DATA-TYPE = "CHARACTER" OR cFunction = "STRING" THEN "'" ELSE "")
                          + "¤" + cElement + "¤"
                          + (IF hBuffer[iiBufferNum]:BUFFER-FIELD(cElement):DATA-TYPE = "CHARACTER" OR cFunction = "STRING" THEN "'" ELSE "")
                          .
            ELSE cQueryExp = cQueryExp + " " + cElement.
          END.
        END.
        ELSE cQueryExp = cQueryExp + " " + cElement + " ".
      END.
    END.

    IF cQueryExp = "" THEN cQueryExp = SUBSTRING(cParam,INDEX(cParam," ")).
    ttSpecFlds.cCallProcParam = cQueryExp.
  END.

  WHEN "jb_total" THEN DO:
    IF NUM-ENTRIES(ttSpecFlds.cCallProcParam,"¤") < 2 THEN
      RETURN "Invalid definition of jb_total calc.field".

    cParam = REPLACE(ttSpecFlds.cCallProcParam,"no-lock","").
    DO ix = 1 TO 5:
      cParam = REPLACE(cParam,"  "," ").
    END.

    ASSIGN cQuery = TRIM(ENTRY(1,cParam,"¤"))
           cParam = TRIM(ENTRY(2,cParam,"¤"))
           .
    
    IF cQuery BEGINS "FOR " THEN cQuery = SUBSTR(cQuery,5).
    cTargetBuff = ENTRY(2,cQuery," ").

    CREATE BUFFER ttSpecFlds.hTargetBuff FOR TABLE cTargetBuff NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      RETURN "Invalid definition of jb_total calc.field" + CHR(10)
           + "Query must be defined as EACH/FIRST/LAST <buffer> rather than " + cQuery.
      
    cQueryExp = "FOR EACH " + ENTRY(2,cQuery," ") + " NO-LOCK WHERE ".

    DO ix = 3 TO NUM-ENTRIES(cQuery," "):
      cElement = ENTRY(ix,cQuery," ").
      IF ix = 3 AND cElement = "OF" THEN DO:
        cPriKeyFlds = getPrimaryKeyFields(hBuffer[iiBufferNum]).
        DO iy = 1 TO NUM-ENTRIES(cPriKeyFlds):
          cQueryExp = cQueryExp 
                    + (IF cQueryExp MATCHES "*WHERE " THEN "" ELSE " AND ")
                    + cTargetBuff + "." + ENTRY(iy,cPriKeyFlds) + " = "
                    + (IF hBuffer[iiBufferNum]:BUFFER-FIELD(ENTRY(iy,cPriKeyFlds)):DATA-TYPE = "CHARACTER" THEN "'" ELSE "")
                    + "¤" + ENTRY(iy,cPriKeyFlds) + "¤"
                    + (IF hBuffer[iiBufferNum]:BUFFER-FIELD(ENTRY(iy,cPriKeyFlds)):DATA-TYPE = "CHARACTER" THEN "'" ELSE "")
                    .
        END.
      END.
      ELSE IF ix > 3 THEN DO:
        IF ix = 4 AND ENTRY(3,cQuery," ") = "OF" THEN NEXT.

        IF cElement = "WHERE" THEN cElement = "AND".

        IF NOT CAN-DO("=,<,>,<>,EQ,GT,LT,NE,AND,OR,(,),'",cElement) THEN DO:
          IF NUM-ENTRIES(cElement,".") > 1 THEN cElement = ENTRY(2,cElement,".").
          hFld = ttSpecFlds.hTargetBuff:BUFFER-FIELD(cElement) NO-ERROR.
          IF VALID-HANDLE(hFld) AND NOT CAN-DO(cTargetFields,cElement) THEN
            ASSIGN cQueryExp   = cQueryExp + ENTRY(ix,cQuery," ")
                   cTargetFields = cTargetFields + (IF cTargetFields NE "" THEN "," ELSE "") + cElement.
          ELSE DO:
            hFld = hBuffer[iiBufferNum]:BUFFER-FIELD(cElement) NO-ERROR.
            IF VALID-HANDLE(hFld) THEN
              cQueryExp = cQueryExp 
                          + (IF hBuffer[iiBufferNum]:BUFFER-FIELD(cElement):DATA-TYPE = "CHARACTER" THEN "'" ELSE "")
                          + "¤" + cElement + "¤"
                          + (IF hBuffer[iiBufferNum]:BUFFER-FIELD(cElement):DATA-TYPE = "CHARACTER" THEN "'" ELSE "")
                          .
            ELSE cQueryExp = cQueryExp + " " + cElement.
          END.
        END.
        ELSE cQueryExp = cQueryExp + " " + cElement + " ".
      END.
    END.
    ttSpecFlds.cTargetQuery = cQueryExp.

    CREATE QUERY ttSpecFlds.hTargetQuery.
    ttSpecFlds.hTargetQuery:SET-BUFFERS(ttSpecFlds.hTargetBuff).
    ttSpecFlds.hTargetQuery:FORWARD-ONLY = YES.

    IF cParam = "COUNT" THEN
      ttSpecFlds.cCallProcParam = "COUNT".
    ELSE DO:
      IF INDEX(cParam," * ") > 0 THEN
        ASSIGN cFld1 = TRIM(ENTRY(1,cParam," * "))
               cFld2 = TRIM(SUBSTR(cParam,INDEX(cParam," * ") + 2))
               ttSpecFlds.cCallProcParam = "*"
               .
      ELSE IF INDEX(cParam," / ") > 0 THEN
        ASSIGN cFld1 = TRIM(ENTRY(1,cParam," / "))
               cFld2 = TRIM(SUBSTR(cParam,INDEX(cParam," / ") + 2))
               ttSpecFlds.cCallProcParam = "/"
               .
      ELSE IF INDEX(cParam," + ") > 0 THEN
        ASSIGN cFld1 = TRIM(ENTRY(1,cParam," + "))
               cFld2 = TRIM(SUBSTR(cParam,INDEX(cParam," + ") + 2))
               ttSpecFlds.cCallProcParam = "+"
               .
      ELSE IF INDEX(cParam," - ") > 0 THEN
        ASSIGN cFld1 = TRIM(ENTRY(1,cParam," - "))
               cFld2 = TRIM(SUBSTR(cParam,INDEX(cParam," - ") + 2))
               ttSpecFlds.cCallProcParam = "-"
               .
      ELSE IF INDEX(cParam," MOD ") > 0 THEN
        ASSIGN cFld1 = TRIM(ENTRY(1,cParam," MOD "))
               cFld2 = TRIM(SUBSTR(cParam,INDEX(cParam," MOD ") + 4))
               ttSpecFlds.cCallProcParam = "MOD"
               .
      ELSE cFld1 = cParam.
  
      ttSpecFlds.hSourceField  = ttSpecFlds.hTargetBuff:BUFFER-FIELD(ENTRY(NUM-ENTRIES(cFld1,"."),cFld1,".")).
      IF cFld2 NE "" THEN
        ttSpecFlds.hSourceField2 = ttSpecFlds.hTargetBuff:BUFFER-FIELD(ENTRY(NUM-ENTRIES(cFld2,"."),cFld2,".")).      
    END.
  END.
  WHEN "jb_max" OR WHEN "jb_min" THEN DO:
    IF NUM-ENTRIES(ttSpecFlds.cCallProcParam," ") = 1 THEN DO:
      ttSpecFlds.hCompareFields[1] = hBuffer[iiBufferNum]:BUFFER-FIELD(TRIM(ttSpecFlds.cCallProcParam)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
        LogThis("Error","Invalid field spesification for jb_max/min: " + ttSpecFlds.cCallProcParam,0).
      ELSE
        ttSpecFlds.iCntCompareFields = hBuffer[iiBufferNum]:BUFFER-FIELD(TRIM(ttSpecFlds.cCallProcParam)):EXTENT.
    END.  
    ELSE DO ix = 1 TO NUM-ENTRIES(ttSpecFlds.cCallProcParam," "):
      ttSpecFlds.iCntCompareFields = ix.
      ttSpecFlds.hCompareFields[ix] = hBuffer[iiBufferNum]:BUFFER-FIELD(ENTRY(ix,ttSpecFlds.cCallProcParam," ")) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
        LogThis("Error","Invalid field spesification for jb_max/min: " + ENTRY(ix,ttSpecFlds.cCallProcParam),0).
    END.  
  END.
END CASE.

RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setReturnMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setReturnMessage Procedure 
FUNCTION setReturnMessage RETURNS LOGICAL
  ( INPUT icReturnMessage AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
cReturnMessage = icReturnMessage.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-startASlib) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startASlib Procedure 
FUNCTION startASlib RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hJbAPI) THEN
  RUN jbserv_api_for_server.p PERSIST SET hJbAPI (icSessionId).
SOURCE-PROCEDURE:ADD-SUPER-PROCEDURE(hJbAPI).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TooManyHits) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TooManyHits Procedure 
FUNCTION TooManyHits RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  Used to check a prescan query which primary buffer is in a many-to-one relation
            with the primary table of the primary query (,EACH Order OF Customer).
            Even if the criteria is indexed the query count might be very big (all "shipped" orders)
            and it can slow the return of data dramatically.
            So - what should be the limit? Hard to tell but we go for 1000 records. It could be a multiple of the batch size
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iHitCnt   AS INT NO-UNDO.
DEF VAR iLimit    AS INT NO-UNDO.
DEF VAR bCloseQry AS LOG NO-UNDO.
DEF VAR iTimeOut  AS INT NO-UNDO INIT 1000.
DEF VAR bStopped  AS LOG NO-UNDO INIT YES.

IF ihQuery:GET-BUFFER-HANDLE(1):NAME MATCHES "buf*_*" THEN
  ASSIGN iLimit   = 100000
         iTimeOut = 100000.
ELSE DO:
  IF iPrescanLimit = 0 THEN DO:
    IF iiBatchSize < 501 THEN
      iLimit = iiBatchSize * 100. /* 30. */
    ELSE iLimit = 10000.
  END.
  ELSE iLimit = iPrescanLimit.
  
  iTimeOut = MAX(1000,iLimit / 5).
END.

IF iStopAfter = 0 THEN iStopAfter = 2.

/* iLimit = 100000. iTimeOut = 10000. */

DO STOP-AFTER iStopAfter ON STOP UNDO, LEAVE:
  IF NOT ihQuery:IS-OPEN THEN DO:
    ihQuery:QUERY-OPEN().
    bCloseQry = YES.
  END.
  
  ETIME(TRUE). 
  ihQuery:GET-FIRST().
/*   IF ETIME < 500 THEN */
    REPEAT WHILE NOT ihQuery:QUERY-OFF-END:
      iHitCnt = iHitCnt + 1.
      IF iHitCnt = iLimit /* OR ETIME > iTimeOut */ THEN LEAVE.
      ihQuery:GET-NEXT().
    END.
  IF bCloseQry THEN 
    ihQuery:QUERY-CLOSE().
  
/*   IF iHitCnt > iLimit THEN                                                                                                                         */
/*     LogThis("Query excecution","skipped due do to a possibly large table scan: (" + STRING(iHitCnt) + "+, checklimit: " + STRING(iLimit) + ")",1). */
/*   ELSE IF ETIME > iTimeOut THEN                                */
/*     LogThis("Query excecution","skipped due do to timeout",1). */

  bStopped = NO.
END.
IF iHitCnt = iLimit THEN
  LogThis("Query excecution","skipped due do to a possibly large table scan: (" + STRING(iHitCnt) + "+)",1).
IF bStopped THEN
  LogThis("Query excecution","stopped due do to STOP-AFTER " + STRING(iStopAfter) + " sec",1).

IF NOT (iHitCnt = iLimit OR bStopped) THEN
  LogThis("Query speedcheck","Timeout: " + STRING(bStopped) + " (limit: " + STRING(iStopAfter) + " sec). Hitcnt: " + STRING(iHitCnt) + " (limit: " + STRING(iLimit) + "). Time used: " + STRING(ETIME / 1000),1).

RETURN iHitCnt = iLimit /* OR ETIME > iTimeOut */ OR bStopped.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UserRowAccess) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UserRowAccess Procedure 
FUNCTION UserRowAccess RETURNS LOGICAL
  ( INPUT ihBuffer AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bAccess AS LOG NO-UNDO.
IF CAN-DO(cBufferAccessCheckList,ihBuffer:NAME) THEN DO:
  bAccess = hBufferAccessCheck:FIND-FIRST(ENTRY(LOOKUP(ihBuffer:NAME,cBufferAccessCheckList),cBufferAccessExprList,"|"),NO-LOCK) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN RETURN bAccess.
  ELSE RETURN NO.
END.
ELSE RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

