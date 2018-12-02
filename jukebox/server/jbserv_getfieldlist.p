/* Retrieve a list of fields from a query.
   - Supports calculated fields and joined buffers
   F.ex to get a list of customer names for salesreps in a region:
      BuffersAndFields:  SalesRep,Customer;CustName
      QueryCriteria:     WHERE Region = 'NY',EACH Customer OF SalesRep NO-LOCK
      
   Modified: 09.08.10 by brynjar@chemistry.no
                    - Automated retrieval for default (zero company) plus / alternative current company 
   Modified: 23.03.11 by brynjar@chemistry.no
                    - Implemented "find first",i.e leave loop after first row is retrieved
                      Syntax is to start the querycriteria with FIRST   (+ space)
--------------------------------------------------------------------------------------*/
DEF INPUT  PARAM icSessionId        AS CHAR NO-UNDO. 
DEF INPUT  PARAM icBuffersAndFields AS CHAR NO-UNDO.
DEF INPUT  PARAM icQueryCriteria    AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocFieldPairs       AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn           AS CHAR NO-UNDO.

DEF VAR hBuffer       AS HANDLE NO-UNDO EXTENT 10.
DEF VAR httBuffer     AS HANDLE NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR cBufferFields AS CHAR NO-UNDO EXTENT 10. /* fields pr buffer num */
DEF VAR hField        AS HANDLE NO-UNDO.
DEF VAR iNumBuffers   AS INT NO-UNDO.
DEF VAR ix            AS INT NO-UNDO.
DEF VAR iy            AS INT NO-UNDO.
DEF VAR iz            AS INT NO-UNDO.
DEF VAR iq            AS INT NO-UNDO.
DEF VAR bDummy        AS LOG NO-UNDO.

DEF VAR cExtraField      AS CHAR NO-UNDO.
DEF VAR cExtraInit       AS CHAR NO-UNDO. /* when matching *.p* the parameter is considered as a call to a .p and decomposed to cExtraCall + cExtraCallParam */ 
DEF VAR cExtraCall       AS CHAR NO-UNDO.
DEF VAR cExtraParam  AS CHAR NO-UNDO.
DEF VAR cExtraCallOutput AS CHAR NO-UNDO.
DEF VAR cConcatFields    AS CHAR NO-UNDO.
DEF VAR cConcatValues    AS CHAR NO-UNDO.
DEF VAR cQueryString     AS CHAR NO-UNDO.
DEF VAR cSortString      AS CHAR NO-UNDO.
DEF VAR bOk              AS LOG  NO-UNDO.
DEF VAR cField           AS CHAR NO-UNDO.
DEF VAR cDistinctField   AS CHAR NO-UNDO.
DEF VAR cDistinctNum     AS CHAR NO-UNDO.
DEF VAR bExtent          AS LOG  NO-UNDO.
DEF VAR iExtent          AS INT  NO-UNDO.
DEF VAR bDistinct        AS LOG  NO-UNDO.
DEF VAR cBufferDef       AS CHAR NO-UNDO.
DEF VAR cRowPairs        AS CHAR NO-UNDO.
DEF VAR cCompanyTag      AS CHAR NO-UNDO.
DEF VAR cCompanyQuery    AS CHAR NO-UNDO.
DEF VAR cCompZeroQuery   AS CHAR NO-UNDO.
DEF VAR cCodeMasterQuery AS CHAR NO-UNDO.
DEF VAR bFindFirst       AS LOG  NO-UNDO.
DEF VAR cLogFile         AS CHAR NO-UNDO.

IF NUM-ENTRIES(icBuffersAndFields,"¤") > 1 THEN
  ASSIGN cLogFile = ENTRY(2,icBuffersAndFields,"¤")
         icBuffersAndFields = ENTRY(1,icBuffersAndFields,"¤")
         .

DEF TEMP-TABLE ttDistinct
    FIELD cDistinctValue AS CHAR
    INDEX idxDistinct cDistinctValue.


DEF TEMP-TABLE ttSpecialFields  NO-UNDO
    FIELD iCallBufferNum  AS INT          /* The source buffer num in the query to feed the calc.proc */
    FIELD iFieldNum       AS INT          /* Seqnum within buffer */
    FIELD cFillField      AS CHAR         /* Name of calculated field */
    FIELD cFillFieldType  AS CHAR         /* Datatype for calculated field */
    FIELD hSourceField    AS HANDLE       /* Two purposes: Handle to parameter field for calculation or handle to distinct column */
    FIELD cCallProc       AS CHAR         /* Name of procedure for calculation (either in persistent or external procedure) */
    FIELD cCallProcParam  AS CHAR         /* String parameter to calc.procedure (context parameter in addition to rowid) */
    FIELD bDistinct       AS LOG          /* A distinct field can either be a calculated field or a field in the buffer (all values for distinct fields are combined) */
    FIELD iExtent         AS INT          /* The extent for an array field */
    .

{incl/validatesession.i}

FUNCTION getASuserId  RETURNS CHARACTER FORWARD.
FUNCTION getCompanyId RETURNS INTEGER   FORWARD.

IF cLogFile NE "" THEN DO:
  OUTPUT TO VALUE(cLogFile) APPEND.
  PUT UNFORMATTED SKIP(1)
      STRING(TODAY) " " STRING(TIME,"HH:MM:SS") " ------------------ " SKIP
      "icBuffersAndFields: " icBuffersAndFields SKIP
      "icQueryCriteria:    " icQueryCriteria SKIP.
END.

CREATE QUERY hQuery.
hQuery:FORWARD-ONLY = YES.
hQuery:CACHE = 0.

QueryDefLoop:
DO ix = 1 TO NUM-ENTRIES(icBuffersAndFields):
  cBufferDef = ENTRY(ix,icBuffersAndFields).

  CREATE BUFFER hBuffer[ix] FOR TABLE ENTRY(1,cBufferDef,";") NO-ERROR.
  IF ERROR-STATUS:ERROR OR hBuffer[ix] = ? THEN DO: 
    DELETE OBJECT hQuery NO-ERROR. 
    DO iy = 1 TO ix - 1:
      DELETE OBJECT hBuffer[iy] NO-ERROR.
    END.
    IF NOT ENTRY(1,cBufferDef,";") BEGINS "JBox" THEN DO:
      ocReturn = "Invalid buffer definition: " + ENTRY(1,cBufferDef,";").
      IF cLogFile NE "" THEN
        PUT UNFORMATTED "ocReturn: " ocReturn SKIP
 	  		"Error:    " ERROR-STATUS:GET-MESSAGE(1) SKIP.
    END.
    RETURN.
  END.
  IF NUM-ENTRIES(cBufferDef,";") > 1 AND ENTRY(2,cBufferDef,";") NE "" THEN
    DO iy = 2 TO NUM-ENTRIES(cBufferDef,";"):
      ASSIGN cField    = ENTRY(iy,cBufferDef,";")
             bExtent   = NO
             iExtent   = 0
             bDistinct = NO
             .
      IF cField BEGINS "DISTINCT " OR cField BEGINS "!DISTINCT " OR cField BEGINS "+DISTINCT " THEN DO:
        IF cDistinctField NE "" THEN DO:
          ocReturn = "Maximum one DISTINCT field when creating a fieldlist:" + CHR(10) + icBuffersAndFields.
          LEAVE QueryDefLoop.
        END.
        IF cField BEGINS "DISTINCT" THEN
          cField = TRIM(SUBSTR(cField,INDEX(cField," ") + 1)).
        ELSE cField = TRIM(SUBSTR(cField,1,1) + SUBSTR(cField,INDEX(cField," ") + 1)).
        ASSIGN cDistinctField = cField
               cDistinctNum   = STRING(ix) + STRING(iy - 1).
      END.

      IF cField BEGINS "+" THEN DO:
        DO iz = 1 TO NUM-ENTRIES(cField,"|"):
          CASE iz:
            WHEN 1 THEN DO:
              cExtraField  = SUBSTR(ENTRY(iz,cField,"|"),2).

              IF TRIM(cExtraField) BEGINS "DISTINCT " THEN
                ASSIGN bDistinct   = TRUE
                       cExtraField = TRIM(SUBSTR(cExtraField,INDEX(cExtraField,"distinct ") + 8)).
            END.
            WHEN 2 THEN DO:
              ASSIGN cExtraInit  = ENTRY(iz,cField,"|")
                     cExtraParam = IF cExtraInit MATCHES "*(*" THEN SUBSTR(cExtraInit,INDEX(cExtraInit,"(") + 1,R-INDEX(cExtraInit,")") - INDEX(cExtraInit,"(") - 1) ELSE ""
                     cExtraInit  = IF cExtraInit MATCHES "*(*" THEN TRIM(SUBSTR(cExtraInit,1,INDEX(cExtraInit,"(") - 1)) ELSE cExtraInit
                     bOk         = R-INDEX(cExtraInit,".p") > 0.
              IF bOk THEN DO:
                CREATE ttSpecialFields.
                ASSIGN ttSpecialFields.iCallBufferNum  = ix
                       ttSpecialFields.cFillField      = cExtraField
                       ttSpecialFields.cCallProc       = cExtraInit
                       ttSpecialFields.cCallProcParam  = REPLACE(cExtraParam,CHR(1),",")
                       ttSpecialFields.bDistinct       = bDistinct
                       .
                /* If the parameter is a field grab it's handle: */
                IF cExtraParam NE "" AND NOT cExtraParam BEGINS "ROWID" THEN  
                  ttSpecialFields.hSourceField = hBuffer[ix]:BUFFER-FIELD(ENTRY(NUM-ENTRIES(cExtraParam,"."),cExtraParam,".")).
              END.
              ELSE DO:
                ocReturn = "Error in definition of calculated field: " + cExtraField + " for getFieldList" + CHR(10) + icBuffersAndFields.
	        IF cLogFile NE "" THEN
                  PUT UNFORMATTED "ocReturn: " ocReturn SKIP.
                LEAVE QueryDefLoop.
              END.
              cExtraInit = "".
            END.
          END CASE.
        END.
        iz = iz - 1.
        IF iz < 2 THEN DO:
          ocReturn = "Missing program spec for returning " + cExtraField + CHR(10) + "for getFieldList: " + icBuffersAndFields.
          IF cLogFile NE "" THEN
          PUT UNFORMATTED "ocReturn: " ocReturn SKIP.
          LEAVE QueryDefLoop.
        END.
        cBufferFields[ix] = cBufferFields[ix] + cExtraField + ",".
      END.

      ELSE IF SUBSTR(ENTRY(1,cField,"|"),LENGTH(ENTRY(1,cField,"|")),1) = "]" THEN DO:
        IF TRIM(cField) BEGINS "DISTINCT " THEN
          ASSIGN bDistinct   = TRUE
                 cField = TRIM(SUBSTR(cField,INDEX(cField,"distinct ") + 8)).
        iExtent = INT(SUBSTR(cField,R-INDEX(cField,"[") + 1,R-INDEX(cField,"]") - R-INDEX(cField,"[") - 1)) NO-ERROR.
        IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
          ocReturn = "Invalid definition of extent field: " + cField + CHR(10) + ERROR-STATUS:GET-MESSAGE(1) + CHR(10) +
                     "for getFieldList: " + icBuffersAndFields. 
          IF cLogFile NE "" THEN
            PUT UNFORMATTED "ocReturn: " ocReturn SKIP.
          LEAVE QueryDefLoop.
        END. 

        CREATE ttSpecialFields.
        ASSIGN ttSpecialFields.iCallBufferNum  = ix
               ttSpecialFields.cFillField      = cField
               ttSpecialFields.cCallProc       = "ExtentFieldValue"
               ttSpecialFields.iExtent         = iExtent
               ttSpecialFields.bDistinct       = bDistinct
               ttSpecialFields.hSourceField    = hBuffer[ix]:BUFFER-FIELD(SUBSTR(cField,1,R-INDEX(cField,"[") - 1))
               .
        cBufferFields[ix] = cBufferFields[ix] + cField + ",".
        
      END.
      ELSE IF NUM-ENTRIES(cField,"|") > 1 THEN DO:
        cConcatFields = REPLACE(cField,"|",",").

        DO iz = 1 TO NUM-ENTRIES(cConcatFields):
          hField = hBuffer[ix]:BUFFER-FIELD(ENTRY(iz,cConcatFields)) NO-ERROR.
          IF NOT VALID-HANDLE(hField) THEN DO:
            ocReturn = "Error in field definition for: " + ENTRY(iz,cConcatFields) + CHR(10) +
                        ERROR-STATUS:GET-MESSAGE(1) + CHR(10) + " in getFieldList: " + icBuffersAndFields + CHR(10) +
                       "(concatinated fields must be in the same buffer and they cannot be distinct)".
            LEAVE QueryDefLoop.
          END.
          cBufferFields[ix] = cBufferFields[ix] + hField:NAME + ",".
        END.
      END.

      ELSE DO:
        hField = hBuffer[ix]:BUFFER-FIELD(cField) NO-ERROR.
        IF NOT VALID-HANDLE(hField) THEN DO:
          ocReturn = "Error in field definition for: " + cField + CHR(10) +
                      ERROR-STATUS:GET-MESSAGE(1) + CHR(10) + " in getFieldList: " + icBuffersAndFields.
          IF cLogFile NE "" THEN
            PUT UNFORMATTED "ocReturn: " ocReturn SKIP.
          LEAVE QueryDefLoop.
        END.
        cBufferFields[ix] = cBufferFields[ix] + hField:NAME + ",".
      END.
    END.

  cBufferFields[ix] = TRIM(cBufferFields[ix],",").
  IF cLogFile NE "" THEN 
    PUT UNFORMATTED "ix - cBufferFields[ix]: " ix " - " cBufferFields[ix] SKIP.

  hQuery:ADD-BUFFER(hBuffer[ix]).
END.

IF ocReturn NE "" THEN DO:
  IF cLogFile NE "" THEN DO:
    PUT UNFORMATTED  "Error from buffer def: " ocReturn.
    OUTPUT CLOSE.
  END.

  DO ix = 1 TO 10:
    IF VALID-HANDLE(hBuffer[ix]) THEN
      DELETE OBJECT hBuffer[ix] NO-ERROR.
    ELSE LEAVE.
  END.
  DELETE OBJECT hQuery NO-ERROR.
  RETURN.
END.

/* Process the query - If querystring ends with (Company) or (+Company) append company crieria. + : Add company records to default */
IF icQueryCriteria BEGINS "FIRST " THEN
  ASSIGN bFindFirst = YES
         icQueryCriteria = SUBSTR(icQueryCriteria,7).

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
END.

cQueryString = "FOR EACH " + ENTRY(1,ENTRY(1,icBuffersAndFields),";") + " NO-LOCK " + cCompanyQuery + icQueryCriteria.

IF cLogFile NE "" THEN
  PUT UNFORMATTED "Query string: " cQueryString SKIP.

RUN OpenQuery.
IF ocReturn NE "" THEN DO:
  IF cLogFile NE "" THEN DO:
    PUT UNFORMATTED "Error from OpenQuery: " ocReturn SKIP.
    OUTPUT CLOSE.
  END.    
  RETURN.
END.

hQuery:GET-FIRST().

IF NOT hQuery:GET-BUFFER-HANDLE(1):AVAIL AND cCompanyTag = "(Codemaster)" AND iCodeMaster NE iCurrCompanyId THEN DO:
  cQueryString = "FOR EACH " + ENTRY(1,ENTRY(1,icBuffersAndFields),";") + " NO-LOCK " + cCodeMasterQuery + icQueryCriteria.
  RUN OpenQuery.
  IF ocReturn NE "" THEN DO:
    IF cLogFile NE "" THEN DO:
      PUT UNFORMATTED "Mod. query string: " cQueryString SKIP.
      PUT UNFORMATTED "Error from OpenQuery: " ocReturn SKIP.
      OUTPUT CLOSE.
    END.    
    RETURN.
  END.
  hQuery:GET-FIRST().
END.

IF NOT hQuery:GET-BUFFER-HANDLE(1):AVAIL AND (cCompanyTag = "(Company)" OR cCompanyTag = "(Codemaster)") THEN DO:
  cQueryString = "FOR EACH " + ENTRY(1,ENTRY(1,icBuffersAndFields),";") + " NO-LOCK " + cCompZeroQuery + icQueryCriteria.
  RUN OpenQuery.
  IF ocReturn NE "" THEN DO:
    IF cLogFile NE "" THEN DO:
      PUT UNFORMATTED "Mod. query string: " cQueryString SKIP.
      PUT UNFORMATTED "Error from OpenQuery: " ocReturn SKIP.
      OUTPUT CLOSE.
    END.    
    RETURN.
  END.
  hQuery:GET-FIRST().
END.

iNumBuffers = ix - 1.

queryLoop:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  cRowPairs = "".
  DO iy = 1 TO iNumBuffers:
    IF hBuffer[iy]:AVAIL THEN DO:
      DO iz = 1 TO NUM-ENTRIES(cBufferFields[iy]):
        FIND FIRST ttSpecialFields
             WHERE ttSpecialFields.cFillField = ENTRY(iz,cBufferFields[iy])
               AND ttSpecialFields.iCallBufferNum = iy
             NO-ERROR.
        IF AVAIL ttSpecialFields THEN DO:
          IF ttSpecialFields.iExtent > 0 THEN
            RUN VALUE(ttSpecialFields.cCallProc) (ttSpecialFields.hSourceField,ttSpecialFields.iExtent, OUTPUT cExtraCallOutput).
          ELSE IF ttSpecialFields.cCallProcParam BEGINS "ROWID" THEN
            RUN VALUE(ttSpecialFields.cCallProc) (hBuffer[iy]:ROWID,SUBSTR(ttSpecialFields.cCallProcParam,6),icSessionId, OUTPUT cExtraCallOutput).
          ELSE IF ttSpecialFields.cCallProcParam NE "" THEN
            RUN VALUE(ttSpecialFields.cCallProc) (hBuffer[iy]:BUFFER-FIELD(ttSpecialFields.cCallProcParam):BUFFER-VALUE,icSessionId,OUTPUT cExtraCallOutput).
          ELSE 
            RUN VALUE(ttSpecialFields.cCallProc) (hBuffer[iy]:ROWID,icSessionId,OUTPUT cExtraCallOutput).
          IF cExtraCallOutput NE "skiprow" THEN DO:              
            IF cExtraCallOutput BEGINS "distinct¤" THEN DO: /* Kept for backward compatibility */
              IF LOOKUP(ENTRY(2,cExtraCallOutput,"¤"),ocFieldPairs,"|") = 0 THEN
                cRowPairs = cRowPairs + ENTRY(2,cExtraCallOutput,"¤") + "|".
              ELSE DO:
                hQuery:GET-NEXT().
                NEXT QueryLoop.
              END.
            END.
            ELSE IF ttSpecialFields.bDistinct THEN DO:
              IF LOOKUP(cExtraCallOutput,ocFieldPairs,"|") = 0 THEN
                cRowPairs = cRowPairs + cExtraCallOutput + "|".
              ELSE DO:
                hQuery:GET-NEXT().
                NEXT QueryLoop.
              END.
            END.
            ELSE DO: 
              IF cExtraField = cDistinctField THEN DO:
                FIND FIRST ttDistinct 
                     WHERE ttDistinct.cDistinctValue = cExtraCallOutput
                     NO-ERROR.
                IF NOT AVAIL ttDistinct THEN DO:
                  CREATE ttDistinct.
                  ttDistinct.cDistinctValue = cExtraCallOutput.
                END.
                ELSE DO:
                  hQuery:GET-NEXT().
                  NEXT QueryLoop.
                END.
              END.
              cRowPairs = cRowPairs + cExtraCallOutput + "|".
            END.
          END.
          ELSE DO:
            hQuery:GET-NEXT().
            NEXT QueryLoop.
          END.
        END.
        ELSE IF cConcatFields NE "" 
                AND ENTRY(iz,cBufferFields[iy]) = ENTRY(1,cConcatFields) 
                AND iz < NUM-ENTRIES(cBufferFields[iy])
                THEN DO:
          cConcatValues = "".
          DO iq = 1 TO NUM-ENTRIES(cConcatFields):
            IF NOT VALID-HANDLE(hBuffer[iy]:BUFFER-FIELD(ENTRY(iq,cConcatFields))) THEN LEAVE.
            IF iq < NUM-ENTRIES(cConcatFields) THEN
              cConcatValues = cConcatValues + 
                              (IF hBuffer[iy]:BUFFER-FIELD(ENTRY(iq,cConcatFields)):BUFFER-VALUE NE ? THEN
                                 STRING(hBuffer[iy]:BUFFER-FIELD(ENTRY(iq,cConcatFields)):BUFFER-VALUE)
                               ELSE "") + " / ".
            ELSE
              cConcatValues = cConcatValues + 
                              (IF hBuffer[iy]:BUFFER-FIELD(ENTRY(iq,cConcatFields)):BUFFER-VALUE NE ? THEN
                                 STRING(hBuffer[iy]:BUFFER-FIELD(ENTRY(iq,cConcatFields)):BUFFER-VALUE)
                               ELSE "") + " ".
          END.
          cRowPairs = cRowPairs + TRIM(cConcatValues) + "|".
        END.
            
        ELSE DO:
          IF iz > 1
            AND cConcatFields NE ""
            AND iz LE NUM-ENTRIES(cConcatFields)
                THEN bDummy = FALSE.
          ELSE DO:
            IF ENTRY(iz,cBufferFields[iy]) = cDistinctField  AND STRING(iy) + STRING(iz) = cDistinctNum THEN DO:
              FIND FIRST ttDistinct 
                   WHERE ttDistinct.cDistinctValue = STRING(hBuffer[iy]:BUFFER-FIELD(ENTRY(iz,cBufferFields[iy])):BUFFER-VALUE)
                   NO-ERROR.
              IF NOT AVAIL ttDistinct THEN DO:
                CREATE ttDistinct.
                ttDistinct.cDistinctValue = STRING(hBuffer[iy]:BUFFER-FIELD(ENTRY(iz,cBufferFields[iy])):BUFFER-VALUE).
              END.
              ELSE DO:
                hQuery:GET-NEXT().
                NEXT QueryLoop.
              END.
            END.
            cRowPairs = cRowPairs + 
                        (IF hBuffer[iy]:BUFFER-FIELD(ENTRY(iz,cBufferFields[iy])):BUFFER-VALUE NE ? THEN
                           STRING(hBuffer[iy]:BUFFER-FIELD(ENTRY(iz,cBufferFields[iy])):BUFFER-VALUE)
                         ELSE "") + "|".
          END.
        END.
      END.
    END.
  END.
  ocFieldPairs = ocFieldPairs + cRowPairs.

  IF bFindFirst THEN 
    LEAVE.
  ELSE
    hQuery:GET-NEXT().
END.
ocFieldPairs = SUBSTR(ocFieldPairs,1,LENGTH(ocFieldPairs) - 1).

DO ix = 1 TO 10:
  IF VALID-HANDLE(hBuffer[ix]) THEN
    DELETE OBJECT hBuffer[ix].
  ELSE LEAVE.
END.
DELETE OBJECT hQuery NO-ERROR.

IF cLogFile NE "" THEN DO:
  PUT UNFORMATTED "ocFieldPairs: " ocFieldPairs SKIP.
  OUTPUT CLOSE.
END.    

PROCEDURE OpenQuery:
  hQuery:QUERY-PREPARE(cQueryString) NO-ERROR.
  IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
    ocReturn = cQueryString + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
    DO ix = 1 TO 10:
      IF VALID-HANDLE(hBuffer[ix]) THEN
        DELETE OBJECT hBuffer[ix].
        ELSE LEAVE.
    END.
    DELETE OBJECT hQuery.
    RETURN.
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
    IF ERROR-STATUS:ERROR THEN DO:
      ocReturn = cQueryString + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
      DO ix = 1 TO 10:
        IF VALID-HANDLE(hBuffer[ix]) THEN
          DELETE OBJECT hBuffer[ix].
          ELSE LEAVE.
      END.
      DELETE OBJECT hQuery.
      RETURN.
    END.
    bOk = hQuery:QUERY-OPEN() NO-ERROR.
  END.
  IF NOT bOk THEN DO:
    ocReturn = "FOR EACH " + ENTRY(1,ENTRY(1,icBuffersAndFields),";") + " NO-LOCK " + icQueryCriteria + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
    DO ix = 1 TO 10:
      IF VALID-HANDLE(hBuffer[ix]) THEN
        DELETE OBJECT hBuffer[ix].
        ELSE LEAVE.
    END.
    DELETE OBJECT hQuery.
    RETURN.
  END.
END.

PROCEDURE JB_rowid:
  DEF INPUT  PARAM irRowid     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR  NO-UNDO.

  ocValue = STRING(irRowid).

END PROCEDURE.

PROCEDURE JB_hhmm:
  DEF INPUT  PARAM iiTime      AS INT  NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  ocValue = STRING(iiTime,"HH:MM").
END PROCEDURE.

PROCEDURE JB_hhmmss:
  DEF INPUT  PARAM iiTime      AS INT  NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  ocValue = STRING(iiTime,"HH:MM:SS").
END PROCEDURE.

PROCEDURE JB_month:
  DEF INPUT  PARAM idDate      AS DATE NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  ocValue = STRING(YEAR(idDate)) + STRING(MONTH(idDate),"99").
END PROCEDURE.

PROCEDURE JB_quarter:
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

PROCEDURE JB_weeknum:
  DEF INPUT  PARAM idDate      AS DATE NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  DEF VAR iWeek AS INT NO-UNDO.
  RUN Weeknum (idDate,OUTPUT iWeek).

  ocValue = STRING(iWeek).
END.

PROCEDURE JB_year:
  DEF INPUT  PARAM idDate      AS DATE NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

  ocValue = STRING(YEAR(idDate)).
END PROCEDURE.

PROCEDURE ExtentFieldValue:    
  DEF INPUT  PARAM ihExtentField AS HANDLE NO-UNDO.
  DEF INPUT  PARAM iiExtent      AS INT    NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR   NO-UNDO.

  ocValue = STRING(ihExtentField:BUFFER-VALUE(iiExtent)).
END PROCEDURE.

FUNCTION getASuserId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCurrUserId.

END FUNCTION.

FUNCTION getCompanyId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCurrCompanyId. 

END FUNCTION.
