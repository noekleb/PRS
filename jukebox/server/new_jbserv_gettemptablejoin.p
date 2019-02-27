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

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
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
DEF VAR cExceptionFields      AS CHAR   NO-UNDO EXTENT 100. /* Duplicate fields pr buffer num that are not in the BuffersAndFields list */
DEF VAR cStatBufferFields     AS CHAR   NO-UNDO EXTENT 100. /* Stat.fields pr buffer */
DEF VAR cAccumBufferFields    AS CHAR   NO-UNDO EXTENT 100. /* Accumulated.fields pr buffer */
DEF VAR fStatBufferValues     AS DEC    NO-UNDO EXTENT 100.  /* Stat.values pr buffer: extent = bufferext * 10 + fieldindex */
DEF VAR cCalcStatFields       AS CHAR   NO-UNDO.            /* List of calculated fields that should be added */
DEF VAR fCalcStatValues       AS DEC    NO-UNDO EXTENT 100.   /* Aggreg. values, calc.stat fields */
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
DEF VAR cExtraInit            AS CHAR   NO-UNDO. /* when matching *.p* the parameter is considered as a call to a .p and decomposed to cCallProc + cCallProcParam */ 
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
DEF VAR iPreScanLimit         AS INT    NO-UNDO. /* max number of records for a prescan */
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

/* Tables for decomposed query definitions. Used in ProcessPreScanJoins
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

/* This table holds one record for each field that needs special treatment during retrieval:
  - Calculated fields
  - Distinct fields (components of the combined distinct value for the row
  - Accumulated fields (accumulated by distinct value and GrandTotal
  - Grand total fields */

DEF TEMP-TABLE ttSpecialFields  NO-UNDO
    FIELD iCallBufferNum  AS INT          /* The source buffer num in the query to feed the calc.proc */
    FIELD iFieldNum       AS INT          /* Seqnum within buffer */
    FIELD cFillField      AS CHAR         /* Name of calculated field */
    FIELD cFillFieldType  AS CHAR         /* Datatype for calculated field */
    FIELD hSourceField    AS HANDLE       /* Two purposes: Handle to parameter field for calculation or handle to distinct column */
    FIELD cCallProc       AS CHAR         /* Name of procedure for calculation (either in persistent or external procedure) */
    FIELD cCallProcParam  AS CHAR         /* String parameter to calc.procedure (context parameter in addition to rowid) */
    FIELD bGrandTotal     AS LOG          /* If the calc.field is in the list of grand totals */
    FIELD bDistinct       AS LOG          /* A distinct field can either be a calculated field or a field in the buffer (all values for distinct fields are combined) */
    FIELD bAccum          AS LOG          /* If the field should be accumulated pr distinct value. (If so it is also added to the fieldlist for grand totals) */
    FIELD b2phAcc         AS LOG          /* Indicates if the calculated field should be invoked on the accumulated resultset */
    FIELD hCalcFieldProc  AS HANDLE       /* Handle to the calc proc if it is in a persistent procedure */
    FIELD cCalcSortField  AS CHAR         /* If the calc.field is member of the sort fields put it's name here as well (equals cFillField) */
    FIELD cFilterOperator AS CHAR         /* If the field is part of the filter.. */
    FIELD cFilterValue    AS CHAR         /* If the field is part of the filter.. */
    FIELD iExtent         AS INT          /* The extent for an array field */
    field bCalcByPreScan  as log          /* Indicates if the value was calculated by prescan */
    INDEX idxCallBuffer  IS PRIMARY UNIQUE  iCallBufferNum iFieldNum
    .

/* Temp table current row contribution to grand total, accum by distinct and calculated sort columns for the current row.
   PreScan: Calculated field values, accum by distinct and calculated sort column values are added to the PreScan table,
            if nothing prevented the row to be used (that would be one of the calculation procedures to return "skiprow")
   No PreScan: Calculated field values are added to the output table buffer
            if nothing prevented the row to be used (that would be one of the calculation procedures to return "skiprow")
    */
DEF TEMP-TABLE ttRowSpecialValues  NO-UNDO
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
    FIELD bOnlyGrandTotal  AS LOG          /* Fields that ar just grand totals should not be stored in the PreScan table (accum values kept in ttSpecialFields) */
    field bCalcByPreScan   as log          /* Indicates if the value was calculated by prescan */
    INDEX idxCallBuffer IS UNIQUE PRIMARY iCallBufferNum cFillField
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

&IF DEFINED(EXCLUDE-getASuserId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getASuserId Procedure 
FUNCTION getASuserId RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

&IF DEFINED(EXCLUDE-TooManyHits) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TooManyHits Procedure 
FUNCTION TooManyHits RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

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
         HEIGHT             = 36.57
         WIDTH              = 68.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl/weeknum.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{incl/validatesession.i}

IF icParam NE "" THEN DO ix = 1 TO NUM-ENTRIES(icParam):
  IF ENTRY(1,ENTRY(ix,icParam),";") = "logfile" THEN DO:
    ETIME(TRUE).
    cLogFile = ENTRY(2,ENTRY(ix,icParam),";").
    LogThis(STRING(TODAY) + " -- " + STRING(TIME,"HH:MM:SS"),FILL("-",30),1).
    LogThis("icParam",icParam,1).
  END.
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "calcfieldproc" THEN
    DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,icParam),";"):
      cCalcFieldProcs = cCalcFieldProcs + ENTRY(iy,ENTRY(ix,icParam),";") + ";".
    END.
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "indexonrowids" THEN
    bIndexOnRowids = ENTRY(2,ENTRY(ix,icParam),";") = "yes". 
  ELSE IF ENTRY(1,ENTRY(ix,icParam),";") = "prescanquery" THEN
    cPreScanJoinQueries = ENTRY(2,ENTRY(ix,icParam),";").
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
END.

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
IF cPreScanJoinQueries NE "" THEN DO ix = 1 TO NUM-ENTRIES(cPreScanJoinQueries,"|"):
  LogThis(IF ix = 1 THEN "Prescan queries" ELSE "",REPLACE(ENTRY(ix,cPreScanJoinQueries,"|"),"¤",","),
          IF ix = NUM-ENTRIES(cPreScanJoinQueries,"|") THEN 1 ELSE 0).
END.
LogThis("Main query",icQueryCriteria,1).
LogThis("BuffersAndFields",icBuffersAndFields,1).
LogThis("Batch size",STRING(iiBatchSize),1).


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

CREATE QUERY hQuery.
IF icDirection = "" THEN
  hQuery:FORWARD-ONLY = YES.
CREATE TEMP-TABLE ohTempTable.
RUN CreateQuery (OUTPUT ocReturn).

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
  LogThis("cBufferList",cBufferList,1).
  LogThis("cAccumFields:",cAccumFields,1).
  LogThis("cCalcStatFields:",cCalcStatFields,1).
  LogThis("cCalcSortFields",cCalcSortFields + " cCalcSortFldTypes: " + cCalcSortFldTypes,2).

  LogThis("Extra fields:","",0).

  IF cSkipCalcFields NE "" THEN
    LogThis("SkipCalcFields:",cSkipCalcFields,1).
END.

/* Assign the specific handle to the calculated and/or distinct fields in the temp-table (for performance): */
bOK = TRUE.
FOR EACH ttSpecialFields:
   /* The ttRowSpecialValues tables keeps all values for special fields as they are
      obtained either as calculated or grand total fields (who also can be calculated): */
   CREATE ttRowSpecialValues.
   BUFFER-COPY ttSpecialFields TO ttRowSpecialValues.   
   ttRowSpecialValues.hFillField    = httBuffer:BUFFER-FIELD(ttRowSpecialValues.cFillField).

   IF CAN-DO("jb_week,jb_month,jb_quarter,jb_year",ttSpecialFields.cCallProc) AND icStatFields NE ""
      AND cCalcFieldFilter MATCHES "*jb_*_*" THEN
     bCheckForSkipRow = YES.

   LogThis(ttSpecialFields.cFillField, 
           ttSpecialFields.cCallProc + "(" 
         + ttSpecialFields.cCallProcParam + ")" 
         + " Distinct: " + STRING(ttSpecialFields.bDistinct) 
         + " Accum: " + STRING(ttSpecialFields.bAccum) 
         + " 2.phase calc: " + STRING(ttSpecialFields.b2phAcc) 
         + " Buffernum: " + STRING(ttSpecialFields.iCallBufferNum)
         + " Statfld: " + STRING(ttSpecialFields.bGrandTotal)
         + " CalcSortFld: " + ttSpecialFields.cCalcSortField
         + " Extent: " + STRING(ttSpecialFields.iExtent)
            ,0).
END.
IF NOT bOK THEN DO:
  ocReturn = "Failed to assign handle for distinct and/or calculated field".
  RETURN.
END.

LogThis("","",1).
LogThis("Etime, query def: ", STRING(ETIME / 1000),2).

IF ocReturn NE "" THEN RETURN.

IF cCalcFieldFilter NE "" THEN DO:
  ocReturn = setCalcFieldFilter().
  IF ocReturn NE "" THEN RETURN.
END.

IF bReturnQueryInfo THEN
  CheckFieldLevelSecurity("exec").

RUN ProcessQuery (OUTPUT ocReturn).

IF ocReturn NE "" THEN DO:
  ocReturn = ocReturn + (IF bReturnQueryInfo THEN "¤" + TRIM(cReturnOKmessage,",") ELSE "").
  RETURN.
END.

RUN WrapUp.

IF VALID-HANDLE(ohTempTable) THEN DELETE OBJECT ohTempTable.

LogThis("","",2).

ocReturn = ocReturn + (IF bReturnQueryInfo THEN "¤" + TRIM(cReturnOKmessage,",") ELSE "").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CreateQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateQuery Procedure 
PROCEDURE CreateQuery :
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

DEF VAR bDistinct              AS LOG    NO-UNDO.
DEF VAR bAccum                 AS LOG    NO-UNDO.
DEF VAR bAverage               AS LOG    NO-UNDO.
DEF VAR bExtent                AS LOG    NO-UNDO.
DEF VAR ixf                    AS INT    NO-UNDO.
DEF VAR cBufferDef             AS CHAR   NO-UNDO.
DEF VAR bFieldSpec             AS LOG    NO-UNDO.
DEF VAR cRealFieldSpec         AS CHAR   NO-UNDO.
DEF VAR cRealDbFieldSpec       AS CHAR   NO-UNDO.
DEF VAR cPrimBufferDbFlds      AS CHAR   NO-UNDO.

DO ix = 1 TO iBufferCount:
  ASSIGN cBufferFieldList = ""
         ixf              = 0
         cBufferDef       = ENTRY(ix,icBuffersAndFields)
         .

  IF ENTRY(1,cBufferDef,";") MATCHES "buf*_*" THEN DO:
    CREATE BUFFER hBuffer[ix] FOR TABLE SUBSTR(ENTRY(1,cBufferDef,";"),6)
                  BUFFER-NAME ENTRY(1,cBufferDef,";") NO-ERROR.
    IF ERROR-STATUS:ERROR OR hBuffer[ix] = ? THEN DO:
      ocReturn = "Table " + SUBSTR(ENTRY(1,cBufferDef,";"),6) + " doesn't exist in database. Buffer definition: " + ENTRY(1,cBufferDef,";").
      LEAVE.
    END.
  END.
  ELSE DO:
    CREATE BUFFER hBuffer[ix] FOR TABLE ENTRY(1,cBufferDef,";") NO-ERROR.
    IF ERROR-STATUS:ERROR OR hBuffer[ix] = ? THEN DO:
      ocReturn = "Table " + ENTRY(1,cBufferDef,";") + " doesn't exist in database".
      LEAVE.
    END.
  END.

  LogThis("Bufferdef " + STRING(ix) + ":",cBufferDef,0).

  IF NUM-ENTRIES(cBufferDef,";") > 1 AND ENTRY(2,cBufferDef,";") NE "" THEN DO:
    bFieldSpec = YES.
    FieldSpec:
    DO iy = 2 TO NUM-ENTRIES(cBufferDef,";"):
      ASSIGN bDistinct    = FALSE
             bAccum       = FALSE
             bExtent      = FALSE
             cExtraFormat = ""
             cExtraLabel  = "".
      /* Calculated field (or just an extra field): */
      IF ENTRY(iy,cBufferDef,";") BEGINS "+" OR SUBSTR(ENTRY(1,ENTRY(iy,cBufferDef,";"),"|"),LENGTH(ENTRY(1,ENTRY(iy,cBufferDef,";"),"|")),1) = "]" THEN DO:
        DO iz = 1 TO NUM-ENTRIES(ENTRY(iy,cBufferDef,";"),"|"):
          CASE iz:
            WHEN 1 THEN DO:
              IF SUBSTR(ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|"),LENGTH(ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|")),1) = "]" THEN                
                ASSIGN bExtent     = YES
                       cExtraField = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
              ELSE cExtraField  = SUBSTR(ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|"),2).

              IF TRIM(cExtraField) BEGINS "ACCUM " THEN 
                ASSIGN bAccum       = TRUE
                       cExtraField  = TRIM(SUBSTR(cExtraField,INDEX(cExtraField,"accum ") + 6)).
              ELSE IF TRIM(cExtraField) BEGINS "DISTINCT " THEN
                ASSIGN bDistinct   = TRUE
                       cExtraField = TRIM(SUBSTR(cExtraField,INDEX(cExtraField,"distinct ") + 8)).
              IF CAN-DO(cSkipCalcFields,cExtraField) THEN NEXT FieldSpec.
            END.
            WHEN 2 THEN DO:
              IF bExtent AND NOT cExtraField BEGINS "+" THEN 
                cExtraLabel = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
              ELSE                  
                cExtraDataType = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
            END.
            WHEN 3 THEN 
              cExtraFormat   = REPLACE(ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|"),"<",",").
            WHEN 4 THEN DO:
              ASSIGN cExtraInit  = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|")
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
                CREATE ttSpecialFields.
                ASSIGN ttSpecialFields.iCallBufferNum  = ix
                       ixf                             = ixf + 1
                       ttSpecialFields.iFieldNum       = ixf
                       ttSpecialFields.cFillField      = cExtraField
                       ttSpecialFields.cFillFieldType  = cExtraDataType
                       ttSpecialFields.cCallProc       = cExtraInit
                       ttSpecialFields.cCallProcParam  = REPLACE(cExtraParam,CHR(1),",")
                       ttSpecialFields.bGrandTotal     = CAN-DO(icStatFields,cExtraField) OR bAccum
                       ttSpecialFields.bDistinct       = bDistinct
                       ttSpecialFields.bAccum          = bAccum
                       ttSpecialFields.b2phAcc         = cExtraField BEGINS "2ph_" 
                       ttSpecialFields.hCalcFieldProc  = IF iq > 0 THEN hCalcFieldProcs[iq] ELSE ?
                       .
                /* If the parameter is a field grab it's handle: */
                IF cExtraParam NE "" AND NOT cExtraParam BEGINS "ROWID" THEN  
                  ttSpecialFields.hSourceField = hBuffer[ix]:BUFFER-FIELD(ENTRY(NUM-ENTRIES(cExtraParam,"."),cExtraParam,".")).

                /* If the calculated value is used for sorting go an extra round first (see under) and get the values */
                IF CAN-DO(cSortFields,cExtraField) THEN
                  ASSIGN ttSpecialFields.cCalcSortField = cExtraField
                         cCalcSortFields                = cCalcSortFields + cExtraField + ","
                         cCalcSortFldTypes              = cCalcSortFldTypes + cExtraDataType + ",".

                IF bAccum THEN
                  ASSIGN cAccumFields   = cAccumFields + cExtraField + ","
                         cAccumFldTypes = cAccumFldTypes + cExtraDataType + ",".
              END.
              cExtraInit = "".
            END.
            WHEN 5 THEN cExtraLabel    = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
          END CASE.
        END.
        iz = iz - 1.
        IF iz < 2 AND NOT bExtent THEN DO:
          ocReturn = "Missing data type for extra field " + cExtraField.
          NEXT.
        END.
        IF NOT bExtent THEN CASE iz:
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
        cField = ENTRY(1,ENTRY(iy,cBufferDef,";"),"|").

        IF cField BEGINS "accum " THEN DO:
          ASSIGN cField                      = TRIM(SUBSTR(cField,7))
                 cAccumBufferFields[ix]      = cAccumBufferFields[ix] + cField + ","
                 bAccum                      = TRUE
                 .
          IF NOT CAN-DO(icStatFields,cField) THEN
            icStatFields = icStatFields + "," + cField.
        END.
        ELSE IF cField BEGINS "distinct " THEN DO: 
          CREATE ttSpecialFields.
          ASSIGN cField                          = TRIM(SUBSTR(cField,10))
                 ttSpecialFields.iCallBufferNum  = ix
                 ixf                             = ixf + 1
                 ttSpecialFields.iFieldNum       = ixf
                 ttSpecialFields.cFillField      = cField
                 ttSpecialFields.bDistinct       = TRUE
                 bDistinct                       = TRUE.
        END.
        hField = hBuffer[ix]:BUFFER-FIELD(cField) NO-ERROR.
        IF NOT VALID-HANDLE(hField) THEN DO:
          ocReturn = "Field " + ENTRY(1,ENTRY(iy,cBufferDef,";"),"|") + CHR(10) + 
                     "doesn't exist in buffer " + hBuffer[ix]:NAME.
          NEXT.
        END.
        ELSE IF bDistinct THEN
          ASSIGN ttSpecialFields.hSourceField = hField
                 ttSpecialFields.cFillFieldType = hField:DATA-TYPE.

        IF NOT CAN-DO(cAddedFields,hField:NAME) THEN DO:
          /* Check for overrides of format and label: */
          IF NUM-ENTRIES(ENTRY(iy,cBufferDef,";"),"|") > 1 THEN DO:
            DO iz = 2 TO NUM-ENTRIES(ENTRY(iy,cBufferDef,";"),"|"):
              IF iz = 2 THEN DO:
                IF ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|") NE "=" THEN 
                  cOverrideLabel = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
                ELSE 
                  cOverrideLabel = hField:LABEL.
              END.
              ELSE IF iz = 3 THEN 
                cOverrideFormat = REPLACE(ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|"),"<",",").
            END.
            IF iz = 3 THEN /* No override on format */
              cOverrideFormat = hField:FORMAT.

            ohTempTable:ADD-NEW-FIELD(hField:NAME,hField:DATA-TYPE,0,cOverrideFormat,
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
              ocReturn = "Error in create of field " + ENTRY(1,ENTRY(iy,cBufferDef,";"),"|") + CHR(10) + 
                         ERROR-STATUS:GET-MESSAGE(1).
              NEXT.
            END.
          END.
          ELSE ohTempTable:ADD-LIKE-FIELD(hField:NAME,hField).

          ASSIGN cAddedFields     = cAddedFields + hField:NAME + ","
                 cBufferFieldList = cBufferFieldList + hField:NAME + ",".

          /* Check ahead if field is also in subsequent buffers. If so put in exceptionlist for buffer-copy
             - but only when subsequent buffer has no field-list. If that's the case the duplicate-list will step in */
          DO ia = ix + 1 TO iBufferCount:
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
          IF CAN-DO(icStatFields,hField:NAME) THEN DO:
            CREATE ttSpecialFields.
            ASSIGN ttSpecialFields.iCallBufferNum  = ix
                   ixf                             = ixf + 1
                   ttSpecialFields.iFieldNum       = ixf
                   ttSpecialFields.cFillField      = hField:NAME
                   ttSpecialFields.cFillFieldType  = hField:DATA-TYPE
                   ttSpecialFields.hSourceField    = hField
                   ttSpecialFields.bGrandTotal     = TRUE
                   ttSpecialFields.bAccum          = bAccum
                   .

            cStatBufferFields[ix] = cStatBufferFields[ix] + hField:NAME + ",".
          END.
        END.
        /* The field name already exists in the temp-table buffer. Add a suffix to make it unique: */
        ELSE DO:
          /* Check for overrides of format and label also here: */
          IF NUM-ENTRIES(ENTRY(iy,cBufferDef,";"),"|") > 1 THEN DO:
            DO iz = 2 TO NUM-ENTRIES(ENTRY(iy,cBufferDef,";"),"|"):
              IF iz = 2 THEN DO:
                IF ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|") NE "=" THEN 
                  cOverrideLabel = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
                ELSE 
                  cOverrideLabel = hField:LABEL.
              END.
              ELSE IF iz = 3 THEN
                cOverrideFormat = ENTRY(iz,ENTRY(iy,cBufferDef,";"),"|").
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
            CREATE ttSpecialFields.
            ASSIGN ttSpecialFields.iCallBufferNum  = ix
                   ixf                             = ixf + 1
                   ttSpecialFields.iFieldNum       = ixf
                   ttSpecialFields.cFillField      = hField:NAME + STRING(ix)
                   ttSpecialFields.cFillFieldType  = hField:DATA-TYPE
                   ttSpecialFields.hSourceField    = hField
                   ttSpecialFields.bGrandTotal     = TRUE
                   ttSpecialFields.bAccum          = bAccum
                   .
            cStatBufferFields[ix] = cStatBufferFields[ix] + hField:NAME + STRING(ix) + ",".
          END.

          /* If the distinct field is a duplicate field correct the name now: */
          IF bDistinct THEN
            ttSpecialFields.cFillField = hField:NAME + STRING(ix).
        END. /* Duplicate field */

      END. /* DB buffer field */
    END.
    cBufferFieldList = TRIM(cBufferFieldList,",").

    /* When we have a field list we don't want the other fields. Hence make an exception list for buffer-copy: */
    DO iq = 1 TO hBuffer[ix]:NUM-FIELDS:
      IF NOT CAN-DO(cBufferFieldList,hBuffer[ix]:BUFFER-FIELD(iq):NAME) AND
         NOT CAN-DO(cExceptionFields[ix],hBuffer[ix]:BUFFER-FIELD(iq):NAME) THEN
       cExceptionFields[ix] = cExceptionFields[ix] + hBuffer[ix]:BUFFER-FIELD(iq):NAME + ",".
    END.
  END. /* Field list for buffer */

  /* No field list is given and we grab all fields from the buffer and add to the temp-table
     (rarely used and does NOT have all the handling neccessary for duplicates!).
     Also builds the actual field list to be returned by cReturnOKmessage: */
  ELSE IF (ix = 1 OR bFieldSpec = NO) AND NUM-ENTRIES(cBufferDef,";") = 1 THEN DO:
    ASSIGN cRealFieldSpec   = cBufferDef + "|"
           cRealDbFieldSpec = cBufferDef + "|".

    DO iy = 1 TO hBuffer[ix]:NUM-FIELDS:
      hField = hBuffer[ix]:BUFFER-FIELD(iy) NO-ERROR.

      /* Goo: */
      cFieldName = hField:NAME.
      IF INDEX(cFieldName,"blob") > 0 OR INDEX(cFieldName,"clob") > 0 THEN NEXT.

      DO iz = 0 TO hField:EXTENT:
        IF hField:EXTENT > 0 AND iz = 0 THEN NEXT.

        IF hField:EXTENT > 0 THEN cFieldName = hField:NAME + "[" + STRING(iz) + "]".

        cRealDbFieldSpec = cRealDbFieldSpec + cFieldName + "|".
  
        IF hField:EXTENT = 0 THEN DO:
          IF NOT CAN-DO(cAddedFields,hField:NAME) THEN DO:
            ohTempTable:ADD-LIKE-FIELD(hField:NAME,hField).
            ASSIGN cAddedFields   = cAddedFields + hField:NAME + ","
                   cRealFieldSpec = cRealFieldSpec + hField:NAME + "|".
          END.
          ELSE DO:
            ohTempTable:ADD-LIKE-FIELD(hField:NAME + STRING(ix),hField).
            ASSIGN cDuplicateFields[ix] = cDuplicateFields[ix] + hField:NAME + ","
                   cRealFieldSpec       = cRealFieldSpec + hField:NAME + STRING(ix) + "|".
          END.
          IF CAN-DO(icStatFields,cFieldName) THEN DO:
            CREATE ttSpecialFields.
            ASSIGN ttSpecialFields.iCallBufferNum  = ix
                   ixf                             = ixf + 1
                   ttSpecialFields.iFieldNum       = ixf
                   ttSpecialFields.cFillField      = hField:NAME
                   ttSpecialFields.cFillFieldType  = hField:DATA-TYPE
                   ttSpecialFields.hSourceField    = hField
                   ttSpecialFields.bGrandTotal     = TRUE.
            cStatBufferFields[ix] = cStatBufferFields[ix] + hField:NAME + ",".
          END.
        END.
        ELSE DO:
          ixf = ixf + 1.
          ocReturn = CreateExtentCalcField(hBuffer[ix],ix,ixf,cFieldName,"",hField:FORMAT,
                                           hField:LABEL + "[" + STRING(iz) + "]",
                                           NO,NO).  
          IF ocReturn NE "" THEN RETURN.
          cRealFieldSpec = cRealFieldSpec + cFieldName + "|".
          IF CAN-DO(icStatFields,cFieldName) THEN 
            cCalcStatFields = cCalcStatFields + cFieldName + ",".
        END.  
      END.
    END.

    IF bReturnQueryInfo THEN
      cReturnOKmessage = cReturnOKmessage  
                       + RIGHT-TRIM(",bufferfieldspec;" + cRealFieldSpec,"|")
                       + RIGHT-TRIM(",bufferdbfieldspec;" + cRealDbFieldSpec,"|")
                       .
  END.

  cExceptionFields[ix] = TRIM(cDuplicateFields[ix] + cExceptionFields[ix],",").
  cDuplicateFields[ix] = TRIM(cDuplicateFields[ix],",").

  ohTempTable:ADD-NEW-FIELD("RowIdent" + STRING(ix),"CHARACTER").

  hQuery:ADD-BUFFER(hBuffer[ix]).
  cBufferList = cBufferList + hBuffer[ix]:NAME + ",".
  IF ix = 1 THEN cPrimBufferDbFlds = cAddedFields.
END.
FinalizeCreateQuery(cPrimBufferDbFlds).

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

&IF DEFINED(EXCLUDE-JB_weeknum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JB_weeknum Procedure 
PROCEDURE JB_weeknum :
/*------------------------------------------------------------------------------
  Purpose:     Conversion of date to week 
  Parameters:  <none>
  Notes:       Standard conversion
------------------------------------------------------------------------------*/
DEF INPUT  PARAM idDate      AS DATE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

DEF VAR iWeek AS INT NO-UNDO.
RUN Weeknum (idDate,OUTPUT iWeek).

ocValue = STRING(iWeek).

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

&IF DEFINED(EXCLUDE-PreScanAllRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PreScanAllRows Procedure 
PROCEDURE PreScanAllRows :
/*------------------------------------------------------------------------------
  Purpose:     Scan all query rows for distinct columns or sorted calculated fields
  Parameters:  <none>
  Notes:       When sorting on calculated values or retrieving distinct rows the whole result-set must be traversed first
               and the calculated sortfields and/or distinct rows are put in a temp-table that will be the primary table in ProcessQuery.
               Also other calculated fields and accumulated (i.e. accumulated pr distinct row) and grand totals are calculated here.
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM ocReturn       AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK           AS LOG  NO-UNDO INIT TRUE.
                                
DEF VAR iCountDistinct          AS INT    NO-UNDO.
DEF VAR hDistinctFields         AS HANDLE NO-UNDO EXTENT 20.
DEF VAR iNumDistinctFields      AS INT    NO-UNDO.
DEF VAR cDistinctValue          AS CHAR   NO-UNDO.
DEF VAR hPreScanDistinctCol     AS HANDLE NO-UNDO.
/* DEF VAR hPreScanCountDistinct   AS HANDLE NO-UNDO. */
DEF VAR hPreScanAddDistRowids   AS HANDLE NO-UNDO EXTENT 20.

bDistinctRows = CAN-FIND(FIRST ttSpecialFields WHERE ttSpecialFields.bDistinct).

CREATE TEMP-TABLE httPreScan.

/* Add all special fields that are not just grand totals to the prescan table
   (Values for grand totals are stored in ttSpecialFields): */
FOR EACH ttSpecialFields:
/*   IF NOT ttSpecialFields.bAccum AND ttSpecialFields.bGrandTotal AND ttSpecialFields.cCalcSortField = "" THEN NEXT.  */
  httPreScan:ADD-NEW-FIELD(ttSpecialFields.cFillField,ttSpecialFields.cFillFieldType).
END.
/* Since we travel through all rows we might as well replace all joins criteria with ROWID's (this is a must when doing distinct search): */
DO ix = 2 TO iBufferCount:
  httPreScan:ADD-NEW-FIELD("rRelPreScan" + STRING(ix),"ROWID").
END.
/* When sorting on a calculated column, index it (them) for optimal performance in ProcessQuery: */
IF CAN-FIND(FIRST ttSpecialFields WHERE ttSpecialFields.cCalcSortField NE "") THEN DO:
  httPreScan:ADD-NEW-INDEX("idxCalcFields").
  FOR EACH ttSpecialFields WHERE ttSpecialFields.cCalcSortField NE "":
    httPreScan:ADD-INDEX-FIELD("idxCalcFields",ttSpecialFields.cFillField).
  END.
END.

httPreScan:ADD-NEW-FIELD("rRelPreScan","ROWID").         /* Rowid for first buffer in query - to be able to join to the PreScan table */
httPreScan:ADD-NEW-FIELD("cDistinctValue","CHARACTER").  
httPreScan:ADD-NEW-INDEX("idxDistinct").
httPreScan:ADD-INDEX-FIELD("idxDistinct","cDistinctValue").
httPreScan:TEMP-TABLE-PREPARE("ttPreScan") NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = ERROR-STATUS:GET-MESSAGE(1) + CHR(10) + "Error in definition of PreScan table".
  RETURN.
END.
hBuffPreScan = httPreScan:DEFAULT-BUFFER-HANDLE.

/* Get direct access to all fields in the prescan table (performance gain when chained attribs are avoided): */
ASSIGN hPreScanBuffer        = httPreScan:DEFAULT-BUFFER-HANDLE
       hPreScanDistinctCol   = hPreScanBuffer:BUFFER-FIELD("cDistinctValue")
       hPreScanCountDistinct = hPreScanBuffer:BUFFER-FIELD("jbCountDistinct")
       hPreScanRowIdCol      = hPreScanBuffer:BUFFER-FIELD("rRelPreScan").
FOR EACH ttSpecialFields,
    FIRST ttRowSpecialValues OF ttSpecialFields:
  IF NOT ttSpecialFields.bAccum AND ttSpecialFields.bGrandTotal AND ttSpecialFields.cCalcSortField = "" THEN 
    ttRowSpecialValues.bOnlyGrandTotal = TRUE.
    
  ttRowSpecialValues.hPreScanField = hPreScanBuffer:BUFFER-FIELD(ttRowSpecialValues.cFillField).

  IF NOT VALID-HANDLE(hPreScanAccumField) AND ttSpecialFields.bAccum AND ttSpecialFields.bGrandTotal THEN
    hPreScanAccumField = ttRowSpecialValues.hPreScanField.
END.
DO ix = 2 TO iBufferCount:
  hPreScanAddDistRowids[ix]  = hPreScanBuffer:BUFFER-FIELD("rRelPreScan" + STRING(ix)).
END.

/* Now open the query without sorting to gain speed: */
IF INDEX(cQueryString," BY ") > 0 THEN
  cQueryString = SUBSTR(cQueryString,1,INDEX(cQueryString," BY ")).

LogThis("cQueryString, all rows: ",cQueryString,1).

ocReturn = QueryOpen().
IF ocReturn NE "" THEN DO:
  obOk = FALSE.
  RETURN.
END.

/* Gather the handles to the database buffer-fields for distinct columns: */
FOR EACH ttSpecialFields
    WHERE ttSpecialFields.cCallProc = ""
      AND ttSpecialFields.bDistinct:
  iNumDistinctFields = iNumDistinctFields + 1.
  hDistinctFields[iNumDistinctFields] = ttSpecialFields.hSourceField.
END.
/* Mark special fields and values calculated here as calculated by prescan */
FOR EACH ttSpecialFields
    WHERE NOT ttSpecialFields.b2phAcc
      and (ttSpecialFields.bDistinct OR ttSpecialFields.cCalcSortField NE "" or ttSpecialFields.bGrandTotal 
           or ttSpecialFields.cFilterOperator ne "" or bCheckForSkipRow)
    ,first ttRowSpecialValues of ttSpecialFields
    :
  assign ttSpecialFields.bCalcByPreScan    = yes
         ttRowSpecialValues.bCalcByPreScan = yes
         .
end.

hQuery:GET-FIRST().

PRESCANLOOP:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  cDistinctValue = "".
  DO iy = 1 TO iBufferCount:
    IF hBuffer[iy]:AVAIL THEN

      /* Go get values for calculated and/or accumulated and GrandTotal fields for current buffer: */
      FOR EACH ttSpecialFields
          WHERE ttSpecialFields.iCallBufferNum = iy
            and ttSpecialFields.bCalcByPrescan
             BY ttSpecialFields.iFieldNum:

        FIND FIRST ttRowSpecialValues OF ttSpecialFields.

        /* Either the calculated values: */
        IF ttSpecialFields.cCallProc NE "" THEN DO:
          IF VALID-HANDLE(ttSpecialFields.hCalcFieldProc) THEN DO:
            IF ttSpecialFields.iExtent > 0 THEN
              RUN VALUE(ttSpecialFields.cCallProc) IN ttSpecialFields.hCalcFieldProc
                 (ttSpecialFields.hSourceField,ttSpecialFields.iExtent, OUTPUT ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
            ELSE IF ttSpecialFields.cCallProcParam BEGINS "ROWID" THEN
              RUN VALUE(ttSpecialFields.cCallProc) IN ttSpecialFields.hCalcFieldProc
                 (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,SUBSTR(ttSpecialFields.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
            ELSE IF ttSpecialFields.cCallProcParam NE "" THEN
              RUN VALUE(ttSpecialFields.cCallProc) IN ttSpecialFields.hCalcFieldProc
                  (ttSpecialFields.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
            ELSE
              RUN VALUE(ttSpecialFields.cCallProc) IN ttSpecialFields.hCalcFieldProc
                  (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
          END.
          ELSE DO:
            IF ttSpecialFields.cCallProcParam BEGINS "ROWID" THEN
              RUN VALUE(ttSpecialFields.cCallProc)
                  (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,SUBSTR(ttSpecialFields.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput).
            ELSE IF ttSpecialFields.cCallProcParam NE "" THEN
              RUN VALUE(ttSpecialFields.cCallProc)
                  (ttSpecialFields.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput).
            ELSE
              RUN VALUE(ttSpecialFields.cCallProc)
                  (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput).
          END.

          IF ttRowSpecialValues.cCurrCallOutput = "skiprow" OR (bCalcFieldFilter AND NOT CalcValueOk(ttRowSpecialValues.cCurrCallOutput)) THEN DO:
            hQuery:GET-NEXT().
            NEXT PRESCANLOOP.
          END.
          ELSE IF ttSpecialFields.bDistinct AND ttRowSpecialValues.cCurrCallOutput NE ? THEN
            cDistinctValue = cDistinctValue + ttRowSpecialValues.cCurrCallOutput.
          ELSE IF ttSpecialFields.bGrandTotal THEN DO: /* That includes also the accum fields */
            ttRowSpecialValues.fRowValue = DEC(ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
            IF ERROR-STATU:ERROR THEN DO:
              ocReturn = "Server calculation routine " + ttSpecialFields.cCallProc + " returns invalid value for accumulation: " + ttRowSpecialValues.cCurrCallOutput.
              RETURN.
            END.
          END.
        END. /* Calculated values */
        /* Or accum or GrandTotal fields from the database buffer: */
        ELSE IF ttSpecialFields.bGrandTotal THEN DO:
          ttRowSpecialValues.fRowValue = DEC(ttSpecialFields.hSourceField:BUFFER-VALUE) NO-ERROR.
          IF ERROR-STATU:ERROR THEN DO:
            ocReturn = "Accumulation of field " + ttSpecialFields.cFillField + " on assignment due to a mismatch in datatype (only numbers can be accumulated)".
            RETURN.
          END.
        END.
      END.
    
    ELSE 
      FOR EACH ttRowSpecialValues WHERE ttRowSpecialValues.iCallBufferNum = iy:
        ttRowSpecialValues.fRowValue = 0.
      END.
  END.
  

  bOk = FALSE.
  IF bDistinctRows THEN DO:
    DO ix = 1 TO iNumDistinctFields:
      IF hDistinctFields[ix]:BUFFER-VALUE NE ? THEN
        cDistinctValue = cDistinctValue + STRING(hDistinctFields[ix]:BUFFER-VALUE).
    END.
    IF cDistinctValue = ? THEN DO:
      ocReturn = "Warning: A distinct column has unknown value. Values for the row were not added to totals".
      hQuery:GET-NEXT().
      NEXT PRESCANLOOP.
    END.
    bOk = hBuffPreScan:FIND-FIRST("WHERE cDistinctValue = " + QUOTER(cDistinctValue)) NO-ERROR.
    IF bOk THEN hPreScanCountDistinct:BUFFER-VALUE = hPreScanCountDistinct:BUFFER-VALUE + 1.
  END.
  IF NOT bOK THEN DO:   /* If distinct value doesn't exist or there is no check on distinct */
    hBuffPreScan:BUFFER-CREATE().
    ASSIGN hPreScanRowIdCol:BUFFER-VALUE      = hBuffer[1]:ROWID
           hPreScanDistinctCol:BUFFER-VALUE   = cDistinctValue
           hPreScanCountDistinct:BUFFER-VALUE = 1
           iCountDistinct                     = iCountDistinct + 1.
    DO ix = 2 TO iBufferCount:
      IF hBuffer[ix]:AVAIL THEN
        ASSIGN hPreScanAddDistRowids[ix]:BUFFER-VALUE = hBuffer[ix]:ROWID.
    END.
  END.

  /* Now we are sure that the row should be included and we can move the corresponding values: */
  FOR EACH ttRowSpecialValues
      WHERE NOT ttRowSpecialValues.b2phAcc: 
    IF ttRowSpecialValues.bAccum THEN
      ttRowSpecialValues.hPreScanField:BUFFER-VALUE = ttRowSpecialValues.hPreScanField:BUFFER-VALUE + ttRowSpecialValues.fRowValue.
    ELSE IF ttRowSpecialValues.cCallProc NE "" THEN
      AssignStringValue(ttRowSpecialValues.hPreScanField,ttRowSpecialValues.cCurrCallOutput).
    ELSE ttRowSpecialValues.hPreScanField:BUFFER-VALUE = ttRowSpecialValues.hSourceField:BUFFER-VALUE.

    IF ttRowSpecialValues.fRowValue NE ? THEN
      ttRowSpecialValues.fGrandTotal = ttRowSpecialValues.fGrandTotal + ttRowSpecialValues.fRowValue.
  END.
  
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
  iQueryCount = iCountDistinct.
ELSE
  iQueryCount = iCount.
      
LogThis("iCountDistinct",STRING(iCountDistinct),0).
LogThis("PreScanCount",STRING(iCount),0).
LogThis("Etime, prescan all rows: ",STRING(ETIME / 1000),2).

/* DebugPreScan()  */

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
def var bPreScanAll        as LOG no-undo.

IF icQueryCriteria BEGINS "WHERE false" THEN
  icQueryCriteria = GetQueryFalseCrit() + SUBSTR(icQueryCriteria,12).

IF cPreScanJoinQueries NE "" THEN DO:
  DecomposeQuery(0,icQueryCriteria,cBufferList).
  ocReturn = ProcessPreScanJoins().
  IF ocReturn NE "" THEN RETURN.
END.
ELSE
  cQueryString = "FOR EACH " + ENTRY(1,cBufferList) + " NO-LOCK " + icQueryCriteria.

/* Now - if the query is sorted on calculated fields or has distinct columns 
   we must perfor an extra step
   where we run throug the whole set first: */
IF CAN-FIND(FIRST ttSpecialFields WHERE ttSpecialFields.bDistinct OR ttSpecialFields.cCalcSortField NE "") THEN DO:
  IF cPreScanJoinQueries = "" THEN DecomposeQuery(0,icQueryCriteria,cBufferList).
  RUN PreScanAllRows (OUTPUT ocReturn,OUTPUT bOk).
  IF NOT bOK THEN RETURN.  
  bPreScanAll = yes.
  IF CAN-FIND(FIRST ttSpecialFields WHERE ttSpecialFields.b2phAcc) THEN
    b2phAcc = YES.
  if not CAN-FIND(FIRST ttSpecialFields where not ttSpecialFields.bCalcByPreScan) THEN
    bPreScanDidAllCalc = yes.
END. 
ELSE  /* When no prescan the target for calculated field values are in the output temp-table: */
  FOR EACH ttSpecialFields,
      FIRST ttRowSpecialValues OF ttSpecialFields:
    IF ttSpecialFields.bGrandTotal THEN NEXT.
    ttRowSpecialValues.hFillField = httBuffer:BUFFER-FIELD(ttRowSpecialValues.cFillField).
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
      IF hBuffer[iy]:AVAIL THEN 

        /* Go get values for calculated and/or accumulated and GrandTotal fields for current buffer: */
        FOR EACH ttSpecialFields
            WHERE ttSpecialFields.iCallBufferNum = iy
              and not ttSpecialFields.bCalcByPreScan
               BY ttSpecialFields.iFieldNum:

          FIND FIRST ttRowSpecialValues OF ttSpecialFields.

          /* Either the calculated values: */
          IF ttSpecialFields.cCallProc NE "" THEN DO:
            IF VALID-HANDLE(ttSpecialFields.hCalcFieldProc) THEN DO:
              IF ttSpecialFields.iExtent > 0 THEN
                RUN VALUE(ttSpecialFields.cCallProc) IN ttSpecialFields.hCalcFieldProc
                   (ttSpecialFields.hSourceField,ttSpecialFields.iExtent, OUTPUT ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecialFields.cCallProcParam BEGINS "ROWID" THEN 
                RUN VALUE(ttSpecialFields.cCallProc) IN ttSpecialFields.hCalcFieldProc 
                   (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,SUBSTR(ttSpecialFields.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
              ELSE IF ttSpecialFields.cCallProcParam NE "" THEN 
                RUN VALUE(ttSpecialFields.cCallProc) IN ttSpecialFields.hCalcFieldProc 
                    (ttSpecialFields.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
              ELSE 
                RUN VALUE(ttSpecialFields.cCallProc) IN ttSpecialFields.hCalcFieldProc 
                    (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
            END.
            ELSE DO:
              IF ttSpecialFields.cCallProcParam BEGINS "ROWID" THEN
                RUN VALUE(ttSpecialFields.cCallProc) 
                    (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,SUBSTR(ttSpecialFields.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput).
              ELSE IF ttSpecialFields.cCallProcParam NE "" THEN
                RUN VALUE(ttSpecialFields.cCallProc) 
                    (ttSpecialFields.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput).
              ELSE 
                RUN VALUE(ttSpecialFields.cCallProc) 
                    (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput).
            END.

            IF ttRowSpecialValues.cCurrCallOutput = "skiprow" OR (bCalcFieldFilter AND NOT CalcValueOk(ttRowSpecialValues.cCurrCallOutput)) THEN DO:
              httBuffer:BUFFER-DELETE.
              IF icDirection = "" THEN
                hQuery:GET-NEXT().
              ELSE 
                hQuery:GET-PREV().
              NEXT QUERYLOOP.
            END.
            ELSE IF ttSpecialFields.bGrandTotal THEN DO: 
              ttRowSpecialValues.fRowValue = DEC(ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
              IF ERROR-STATU:ERROR THEN DO:
                ocReturn = "Server calculation routine " + ttSpecialFields.cCallProc + " returns invalid value for accumulation: " + ttRowSpecialValues.cCurrCallOutput.
                RETURN.
              END.
            END.
          END. /* Calculated values */
          /* Or GrandTotal fields from the database buffer: */
          ELSE IF ttSpecialFields.bGrandTotal THEN DO:
            ttRowSpecialValues.fRowValue = DEC(ttSpecialFields.hSourceField:BUFFER-VALUE) NO-ERROR.
            IF ERROR-STATU:ERROR THEN DO:
              ocReturn = "Accumulation of field " + ttSpecialFields.cFillField + " on assignment due to a mismatch in datatype (only numbers can be accumulated)".
              RETURN.
            END.
          END.
        END.
      ELSE FOR EACH ttRowSpecialValues WHERE ttRowSpecialValues.iCallBufferNum = iy:
        ttRowSpecialValues.fRowValue = 0.
      END.
    END.

    httBuffer:BUFFER-FIELD("RowCount"):BUFFER-VALUE = iCount.
    DO iy = 1 TO iBufferCount:
      IF hBuffer[iy]:AVAIL THEN DO:
        httBuffer:BUFFER-COPY(hBuffer[iy],cExceptionFields[iy]).
        DO iz = 1 TO NUM-ENTRIES(cDuplicateFields[iy]):
          httBuffer:BUFFER-FIELD(ENTRY(iz,cDuplicateFields[iy]) + STRING(iy)):BUFFER-VALUE = hBuffer[iy]:BUFFER-FIELD(ENTRY(iz,cDuplicateFields[iy])):BUFFER-VALUE.
        END.
        httBuffer:BUFFER-FIELD("RowIdent" + STRING(iy)):BUFFER-VALUE = STRING(hBuffer[iy]:ROWID).
      END.
    END.

    /* Now we are sure that the row should be included and we can move the corresponding values: */
/*     IF NOT bPreScanDidAllCalc THEN */
      FOR EACH ttRowSpecialValues WHERE not ttRowSpecialValues.bCalcByPreScan:
        IF ttRowSpecialValues.cCallProc NE "" THEN
          AssignStringValue(ttRowSpecialValues.hFillField,ttRowSpecialValues.cCurrCallOutput).
        IF ttRowSpecialValues.fRowValue NE ? THEN
          ttRowSpecialValues.fGrandTotal = ttRowSpecialValues.fGrandTotal + ttRowSpecialValues.fRowValue.
      END.
      
    /* All calculations were done during prescan and are kept in the prescan table. Move them to their corresponding fields in the output table: */
/*     ELSE */
      FOR EACH ttRowSpecialValues where ttRowSpecialValues.bCalcByPreScan: 
        IF VALID-HANDLE(hPreScanAccumField) AND ttRowSpecialValues.hPreScanField = hPreScanCountDistinct THEN
          hAverageFillField:BUFFER-VALUE = hPreScanAccumField:BUFFER-VALUE / hPreScanCountDistinct:BUFFER-VALUE.
        ttRowSpecialValues.hFillField:BUFFER-VALUE = ttRowSpecialValues.hPreScanField:BUFFER-VALUE.
/*         IF ttRowSpecialValues.b2phAcc THEN DO:                                                              */
/*           AssignStringValue(ttRowSpecialValues.hFillField,ttRowSpecialValues.cCurrCallOutput).              */
/*           IF ttRowSpecialValues.fRowValue NE ? THEN                                                         */
/*             ttRowSpecialValues.fGrandTotal = ttRowSpecialValues.fGrandTotal + ttRowSpecialValues.fRowValue. */
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
          FOR EACH ttSpecialFields
              WHERE ttSpecialFields.iCallBufferNum = iy
                AND (IF bCheckForSkipRow THEN TRUE ELSE ttSpecialFields.bGrandTotal)
                and not ttSpecialFields.bCalcByPreScan
                 BY ttSpecialFields.iFieldNum:
  
            FIND FIRST ttRowSpecialValues OF ttSpecialFields.
  
            /* Either the calculated values: */
            IF ttSpecialFields.cCallProc NE "" THEN DO:
              IF VALID-HANDLE(ttSpecialFields.hCalcFieldProc) THEN DO:
                IF ttSpecialFields.cCallProcParam BEGINS "ROWID" THEN 
                  RUN VALUE(ttSpecialFields.cCallProc) IN ttSpecialFields.hCalcFieldProc 
                     (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,SUBSTR(ttSpecialFields.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
                ELSE IF ttSpecialFields.cCallProcParam NE "" THEN 
                  RUN VALUE(ttSpecialFields.cCallProc) IN ttSpecialFields.hCalcFieldProc 
                      (ttSpecialFields.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
                ELSE 
                  RUN VALUE(ttSpecialFields.cCallProc) IN ttSpecialFields.hCalcFieldProc 
                      (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
              END.
              ELSE DO:
                IF ttSpecialFields.cCallProcParam BEGINS "ROWID" THEN
                  RUN VALUE(ttSpecialFields.cCallProc) 
                      (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,SUBSTR(ttSpecialFields.cCallProcParam,6),icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput).
                ELSE IF ttSpecialFields.cCallProcParam NE "" THEN
                  RUN VALUE(ttSpecialFields.cCallProc) 
                      (ttSpecialFields.hSourceField:BUFFER-VALUE,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput).
                ELSE 
                  RUN VALUE(ttSpecialFields.cCallProc) 
                      (hBuffer[ttSpecialFields.iCallBufferNum]:ROWID,icSessionId, OUTPUT ttRowSpecialValues.cCurrCallOutput).
              END.
  
              IF ttRowSpecialValues.cCurrCallOutput = "skiprow" OR (bCalcFieldFilter AND NOT CalcValueOk(ttRowSpecialValues.cCurrCallOutput)) THEN DO:
                IF icDirection = "" THEN
                  hQuery:GET-NEXT().
                ELSE 
                  hQuery:GET-PREV().
                NEXT QUERYLOOP.
              END.
              ELSE IF ttSpecialFields.bGrandTotal THEN DO: 
                ttRowSpecialValues.fRowValue = DEC(ttRowSpecialValues.cCurrCallOutput) NO-ERROR.
                IF ERROR-STATU:ERROR THEN DO:
                  ocReturn = "Server calculation routine " + ttSpecialFields.cCallProc + " returns invalid value for accumulation: " + ttRowSpecialValues.cCurrCallOutput.
                  RETURN.
                END.
              END.
            END. /* Calculated values */
            /*  GrandTotal fields from the database buffer: */
            ELSE IF ttSpecialFields.bGrandTotal THEN DO:
              ttRowSpecialValues.fRowValue = DEC(ttSpecialFields.hSourceField:BUFFER-VALUE) NO-ERROR.
              IF ERROR-STATU:ERROR THEN DO:
                ocReturn = "Accumulation of field " + ttSpecialFields.cFillField + " on assignment due to a mismatch in datatype (only numbers can be accumulated)".
                RETURN.
              END.
            END.
          END.
        END.
        ELSE FOR EACH ttRowSpecialValues WHERE ttRowSpecialValues.iCallBufferNum = iy:
          ttRowSpecialValues.fRowValue = 0.
        END.
      END.
  
      /* Add up grand totals: */
      FOR EACH ttRowSpecialValues 
          WHERE not ttRowSpecialValues.bCalcByPreScan 
            and ttRowSpecialValues.fRowValue NE ?:
        ttRowSpecialValues.fGrandTotal = ttRowSpecialValues.fGrandTotal + ttRowSpecialValues.fRowValue.
      END.
    END.
  END. /* adding up query totals for rows not returned to the client */

  ELSE IF iSelected = iiBatchSize THEN LEAVE.

  iCount = iCount + 1.

  IF icDirection = "" THEN
    hQuery:GET-NEXT().
  ELSE 
    hQuery:GET-PREV().
END.

iCount = iCount - 1.

IF iQueryCount = 0 THEN iQueryCount = iCount.

LogThis("Count: ",STRING(iCount),1).
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

IF icStatFields NE "" THEN DO:
  ocStatFieldsAndValues = "rowcount|" + STRING(iQueryCount) + ";".
  FOR EACH ttSpecialFields
      WHERE ttSpecialFields.bGrandTotal
     ,FIRST ttRowSpecialValues OF ttSpecialFields:
    ocStatFieldsAndValues = ocStatFieldsAndValues + ttSpecialFields.cFillField + "|" + STRING(ttRowSpecialValues.fGrandTotal) + ";".
  END.
  ocStatFieldsAndValues = TRIM(ocStatFieldsAndValues,";").
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

IF NOT CAN-DO(cCalcFieldFilter,ttSpecialFields.cFillField) THEN RETURN TRUE.

DO ix = 1 TO NUM-ENTRIES(ttSpecialFields.cFilterOperator):
  ASSIGN cOperator   = ENTRY(ix,ttSpecialFields.cFilterOperator)
         cCurrFilter = ENTRY(ix,ttSpecialFields.cFilterValue,"|").

  CASE ttSpecialFields.cFillFieldType:
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
    WHEN "LOGICAL" THEN
      CASE cOperator:
        WHEN "=" OR WHEN "EQ" THEN
          bPass = (CAN-DO("yes,true",cCurrFilter) AND CAN-DO("yes,true",cCalcValue)) OR
                  (CAN-DO("no,false",cCurrFilter) AND CAN-DO("no,false",cCalcValue)).
        WHEN "<>" OR WHEN "NE" THEN
          bPass = (NOT CAN-DO("yes,true",cCurrFilter) AND NOT CAN-DO("yes,true",cCalcValue)) OR
                  (NOT CAN-DO("no,false",cCurrFilter) AND NOT CAN-DO("no,false",cCalcValue)).
      END CASE.
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
   
  CREATE ttSpecialFields.
  ASSIGN ttSpecialFields.iCallBufferNum  = iiBufferNum
         ttSpecialFields.iFieldNum       = iiFieldNum
         ttSpecialFields.cFillField      = icFieldName
         ttSpecialFields.cFillFieldType  = hField:DATA-TYPE
         ttSpecialFields.cCallProc       = "ExtentFieldValue"
         ttSpecialFields.hSourceField    = hField
         ttSpecialFields.bGrandTotal     = CAN-DO(icStatFields,icFieldName) OR ibAccum
         ttSpecialFields.bDistinct       = ibDistinct
         ttSpecialFields.bAccum          = ibAccum
         ttSpecialFields.b2phAcc         = NO
         ttSpecialFields.hCalcFieldProc  = THIS-PROCEDURE
         ttSpecialFields.iExtent         = iExtent
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

  CREATE ttSpecialFields.
  ASSIGN ttSpecialFields.iCallBufferNum  = iiBufferNum
         ttSpecialFields.iFieldNum       = iiFieldNum
         ttSpecialFields.cFillField      = icFieldName
         ttSpecialFields.cFillFieldType  = icDataType
         ttSpecialFields.cCallProc       = ""
         ttSpecialFields.hSourceField    = ?
         ttSpecialFields.bGrandTotal     = NO
         ttSpecialFields.bDistinct       = NO
         ttSpecialFields.bAccum          = NO
         ttSpecialFields.b2phAcc         = NO
         ttSpecialFields.hCalcFieldProc  = THIS-PROCEDURE
         ttSpecialFields.iExtent         = iExtent
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

&IF DEFINED(EXCLUDE-DebugPreScan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DebugPreScan Procedure 
FUNCTION DebugPreScan RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

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
       icQueryString = REPLACE(icQueryString,",","|")
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
           ttQuery.cQryString     = REPLACE(ttQuery.cQryString,"|",",")
           ttQuery.cQryJoinString = REPLACE(ttQuery.cQryJoinString,"|",",")
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

CREATE ttSpecialFields.
ASSIGN ttSpecialFields.iCallBufferNum  = 1
       ttSpecialFields.iFieldNum       = 10000
       ttSpecialFields.cFillField      = "jbCountDistinct"
       ttSpecialFields.cFillFieldType  = "INTEGER"
       ttSpecialFields.hSourceField    = ?
       ttSpecialFields.bGrandTotal     = NO
       ttSpecialFields.bAccum          = YES.
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

LogThis("Corrected query string",icQueryString,1).

RETURN icQueryString. 

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
DEF VAR iy             AS INT NO-UNDO.

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
  CREATE BUFFER hPreScanBuffer FOR TABLE ttQuery.cQryBuffer.

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
           iPrescanLimit     = 1000.
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

ASSIGN /*cPreScanJoinQueries = REPLACE(cPreScanJoinQueries,"  "," ")*/
       iNumQueries         = NUM-ENTRIES(cPreScanJoinQueries,"|").

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
  IF TRIM(ENTRY(ix,cPreScanJoinQueries,"|")) = "" THEN NEXT.

  ASSIGN cPreScanQuery   = ENTRY(ix,cPreScanJoinQueries,"|")
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
  ASSIGN cPreScanQuery  = "FOR EACH " + REPLACE(ttQueryBufferLists.cFirstQueryComp,"¤",",")
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

  ttQueryBufferLists.bPassExec = TooManyHits(hPreScanQuery).

  DELETE OBJECT hPreScanBuff[1].
  DELETE OBJECT hPreScanQuery.
END.

/* If more than two components, check the first two: */
FOR EACH ttQueryBufferLists 
    WHERE NOT ttQueryBufferLists.bPassExec
      AND ttQueryBufferLists.iNumQryBuffs > 2
      AND NOT ttQueryBufferLists.bMandatory
    :
  ASSIGN cPreScanQuery  = "FOR EACH " + REPLACE(ttQueryBufferLists.cFirstQueryComp,"¤",",") + "," + REPLACE(ttQueryBufferLists.cSecondQueryComp,"¤",",")
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

  LogThis("Prescan first two buffers:",cPreScanQuery,0).

  bOk = hPreScanQuery:QUERY-PREPARE(cPreScanQuery) NO-ERROR.
  IF NOT bOk THEN DO:
    DELETE OBJECT httPreScanJoin.
    DELETE OBJECT hPreScanQuery.
    RETURN "Error in prescan query definition: Invalid querystring: " + cPreScanQuery + CHR(10) + "Buffer: " + ENTRY(1,ttQueryBufferLists.cQryBufferList). 
  END.
  
  LogThis("Prescan two buffers index",hPreScanQuery:INDEX-INFORMATION(1),0).
  LogThis("",hPreScanQuery:INDEX-INFORMATION(2),0).

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
  cPreScanQuery = REPLACE("FOR EACH " + hPreScanBuff[1]:NAME + " " + cPreScanQuery,CHR(1),",").

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

  IF NOT ttQueryBufferLists.bMandatory THEN DO:
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
DEF VAR cNewList AS CHAR NO-UNDO.

cCalcFieldFilter = REPLACE(cCalcFieldFilter,CHR(1),",").
cCalcFieldFilter = REPLACE(cCalcFieldFilter,CHR(3),";").

IF cCalcFieldFilter = "¤=¤" THEN RETURN "".
        
DO ix = 1 TO NUM-ENTRIES(cCalcFieldFilter,"|"):
  IF TRIM(ENTRY(ix,cCalcFieldFilter,"|")) = "" THEN NEXT.

  IF NUM-ENTRIES(ENTRY(ix,cCalcFieldFilter,"|"),"¤") NE 3 THEN
    RETURN "Error in definition of filter for calculated field: " + ENTRY(ix,cCalcFieldFilter,"|") + CHR(10) +
           "Fields must defined as <field>¤<operator>¤<value>".
  FIND FIRST ttSpecialFields
       WHERE ttSpecialFields.cFillField = TRIM(ENTRY(1,ENTRY(ix,cCalcFieldFilter,"|"),"¤"))
       NO-ERROR.
  IF NOT AVAIL ttSpecialFields THEN 
    RETURN "Error in definition of filter for calculated field: " + ENTRY(1,ENTRY(ix,cCalcFieldFilter,"|"),"¤") + CHR(10) +
           "Field is not defined as a calculated"
           .
  ASSIGN ttSpecialFields.cFilterOperator = ttSpecialFields.cFilterOperator + TRIM(ENTRY(2,ENTRY(ix,cCalcFieldFilter,"|"),"¤")) + ","
         ttSpecialFields.cFilterValue    = ttSpecialFields.cFilterValue + TRIM(ENTRY(3,ENTRY(ix,cCalcFieldFilter,"|"),"¤")) + "|"
         cNewList                        = cNewList + TRIM(ENTRY(1,ENTRY(ix,cCalcFieldFilter,"|"),"¤")) + ",".
END.

FOR EACH ttSpecialFields:
  ASSIGN ttSpecialFields.cFilterOperator = TRIM(ttSpecialFields.cFilterOperator,",")
         ttSpecialFields.cFilterValue    = TRIM(ttSpecialFields.cFilterValue,"|").
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

IF ihQuery:GET-BUFFER-HANDLE(1):NAME MATCHES "buf*_*" THEN
  ASSIGN iLimit   = 100000
         iTimeOut = 100000.
ELSE DO:
  IF iPrescanLimit = 0 THEN DO:
    IF iiBatchSize < 501 THEN
      iLimit = iiBatchSize * 30.
    ELSE iLimit = 10000.
  END.
  ELSE iLimit = iPrescanLimit.
  
  iTimeOut = MAX(1000,iLimit / 5).
END.

IF NOT ihQuery:IS-OPEN THEN DO:
  ihQuery:QUERY-OPEN().
  bCloseQry = YES.
END.

ETIME(TRUE).
ihQuery:GET-FIRST().
IF ETIME < 500 THEN
  REPEAT WHILE NOT ihQuery:QUERY-OFF-END:
    iHitCnt = iHitCnt + 1.
    IF iHitCnt > iLimit OR ETIME > iTimeOut THEN LEAVE.
    ihQuery:GET-NEXT().
  END.
IF bCloseQry THEN 
  ihQuery:QUERY-CLOSE().

IF iHitCnt > iLimit THEN
  LogThis("Query excecution","skipped due do to a possibly large table scan",1).
ELSE IF ETIME > iTimeOut THEN
  LogThis("Query excecution","skipped due do to timeout",1).

RETURN iHitCnt > iLimit OR ETIME > iTimeOut.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

