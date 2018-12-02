/* Retrieve values for one or more fields, or the ROWID for a buffer (table).
   - To get value from extent, add the extent number to the field-list:
     <column>;<extent>,<column>..  
   - To get the rowid, use ROWID as column name
   - To get the last record prefix the buffer name with 'last '
   - To check for a ambigouity prefix the record with 'unique '
     If ambigous the return value is AMBIGUOUS 
     
   Modified: 09.08.10 by brynjar@chemistry.no
           - Automated retrieval for default (zero company) alternative current company   
--------------------------------------------------------------------------*/
   
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icBufferName AS CHAR NO-UNDO.
DEF INPUT  PARAM icCriteria   AS CHAR NO-UNDO.
DEF INPUT  PARAM icColumns    AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValues     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR hBuffer          AS HANDLE NO-UNDO.
DEF VAR bOk              AS LOG    NO-UNDO.
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR bLast            AS LOG    NO-UNDO.
DEF VAR iExtent          AS INT    NO-UNDO.
DEF VAR hField           AS HANDLE NO-UNDO.
DEF VAR cCompanyTag      AS CHAR   NO-UNDO.
DEF VAR cCompanyQuery    AS CHAR   NO-UNDO.
DEF VAR cCompZeroQuery   AS CHAR   NO-UNDO.
DEF VAR cCodeMasterQuery AS CHAR NO-UNDO.

{incl/validatesession.i}

IF icBufferName BEGINS "last " THEN
  ASSIGN bLast = TRUE
         icBufferName = TRIM(SUBSTR(icBufferName,6)).
ELSE IF icBufferName BEGINS "first " THEN
  icBufferName = TRIM(SUBSTR(icBufferName,7)).
ELSE IF icBufferName BEGINS "unique " THEN
  ASSIGN bLast = ?
         icBufferName = TRIM(SUBSTR(icBufferName,8)).

icColumns = REPLACE(icColumns,";",",").

CREATE BUFFER hBuffer FOR TABLE icBufferName NO-ERROR.
IF ERROR-STATUS:ERROR OR hBuffer = ? THEN DO: 
  IF NOT icBufferName BEGINS "JBox" THEN
    ocReturn = "Invalid buffer definition: " + icBufferName.
  RETURN.
END.

IF icCriteria MATCHES "*(Company)" OR icCriteria MATCHES "*(Codemaster)" THEN 
  ASSIGN cCompanyTag      = SUBSTR(icCriteria,R-INDEX(icCriteria,"("))
         icCriteria       = SUBSTR(icCriteria,1,R-INDEX(icCriteria,"(") - 1)
         icCriteria       = TRIM(icCriteria)
         icCriteria       = SUBSTR(icCriteria,INDEX(icCriteria, " "))
         cCompanyQuery    = "WHERE iJBoxCompanyId = " + STRING(iCurrCompanyId) + " AND"
         cCodeMasterQuery = "WHERE iJBoxCompanyId = " + STRING(iCodeMaster) + " AND"
         cCompZeroQuery   = "WHERE iJBoxCompanyId = 0 AND"
         .
IF bLast THEN
  bOk = hBuffer:FIND-LAST(cCompanyQuery + icCriteria,NO-LOCK) NO-ERROR.
ELSE IF NOT bLast THEN
  bOk = hBuffer:FIND-FIRST(cCompanyQuery + icCriteria,NO-LOCK) NO-ERROR.
ELSE DO:
  bOk = hBuffer:FIND-UNIQUE(cCompanyQuery + icCriteria,NO-LOCK) NO-ERROR.
  
  IF hBuffer:AMBIGUOUS THEN 
    ocValues = "AMBIGUOUS".
END.

IF ERROR-STATUS:GET-NUMBER(1) NE 0 AND
   ERROR-STATUS:GET-NUMBER(1) NE 565 AND
   ERROR-STATUS:GET-NUMBER(1) NE 3166 AND
   ERROR-STATUS:GET-NUMBER(1) NE 138
   THEN DO:

  DELETE OBJECT hBuffer.

  ocReturn = icCriteria + CHR(10) + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
  IF ERROR-STATUS:GET-NUMBER(1) = 573 AND icCriteria MATCHES "*dec*" THEN
    ocReturn = ocReturn + CHR(10) + CHR(10) + "The error could be related to a decimal expr where comma is used as decimal point".
  RETURN.
END.

IF NOT bOk AND ocValues NE "AMBIGUOUS" AND cCompanyTag = "(Codemaster)" AND iCodeMaster NE iCurrCompanyId THEN DO:
  IF bLast THEN
    bOk = hBuffer:FIND-LAST(cCodeMasterQuery + icCriteria,NO-LOCK) NO-ERROR.
  ELSE IF NOT bLast THEN
    bOk = hBuffer:FIND-FIRST(cCodeMasterQuery + icCriteria,NO-LOCK) NO-ERROR.
  ELSE DO:
    bOk = hBuffer:FIND-UNIQUE(cCodeMasterQuery + icCriteria,NO-LOCK) NO-ERROR.

    IF hBuffer:AMBIGUOUS THEN 
      ocValues = "AMBIGUOUS".
  END.
END.

IF NOT bOk AND ocValues NE "AMBIGUOUS" AND (cCompanyTag = "(Company)" OR cCompanyTag = "(Codemaster)") THEN DO:
  IF bLast THEN
    bOk = hBuffer:FIND-LAST(cCompZeroQuery + icCriteria,NO-LOCK) NO-ERROR.
  ELSE IF NOT bLast THEN
    bOk = hBuffer:FIND-FIRST(cCompZeroQuery + icCriteria,NO-LOCK) NO-ERROR.
  ELSE DO:
    bOk = hBuffer:FIND-UNIQUE(cCompZeroQuery + icCriteria,NO-LOCK) NO-ERROR.

    IF hBuffer:AMBIGUOUS THEN 
      ocValues = "AMBIGUOUS".
  END.
END.

IF ocValues NE "AMBIGUOUS" THEN DO:
  IF bOK THEN DO:
    IF icColumns NE "ROWID" THEN DO ix = 1 TO NUM-ENTRIES(icColumns):
      IF NUM-ENTRIES(ENTRY(ix,icColumns),";") = 1 THEN DO:
        IF SUBSTR(ENTRY(ix,icColumns),LENGTH(ENTRY(ix,icColumns)),1) = "]" THEN DO:
          iExtent = INT(SUBSTR(ENTRY(ix,icColumns),R-INDEX(ENTRY(ix,icColumns),"[") + 1,R-INDEX(ENTRY(ix,icColumns),"]") - R-INDEX(ENTRY(ix,icColumns),"[") - 1)) NO-ERROR.
          IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO: 
            ocReturn = "Invalid definition of extent field: " + ENTRY(ix,icColumns) + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
            LEAVE.
          END.
          hField = hBuffer:BUFFER-FIELD(SUBSTR(ENTRY(ix,icColumns),1,R-INDEX(ENTRY(ix,icColumns),"[") - 1)) NO-ERROR.
          IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO: 
            ocReturn = "Invalid definition of extent field: " + ENTRY(ix,icColumns) + CHR(10) + ERROR-STATUS:GET-MESSAGE(1).
            LEAVE.
          END.

          ocValues = ocValues + (IF hField:BUFFER-VALUE[iExtent] NE ? THEN 
                                   hField:BUFFER-VALUE[iExtent]
                                 ELSE "") + "|".
        END.
        ELSE IF ENTRY(ix,icColumns) = "ROWID" THEN
          ocValues = ocValues + STRING(hBuffer:ROWID) + "|".
        ELSE 
          ocValues = ocValues + (IF hBuffer:BUFFER-FIELD(ENTRY(ix,icColumns)):BUFFER-VALUE NE ? THEN 
                                   hBuffer:BUFFER-FIELD(ENTRY(ix,icColumns)):BUFFER-VALUE 
                                 ELSE "") + "|".
      END.
      ELSE 
        ocValues = ocValues + (IF hBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,icColumns),";")):BUFFER-VALUE[INT(ENTRY(2,ENTRY(ix,icColumns),";"))] NE ? THEN 
                                 hBuffer:BUFFER-FIELD(ENTRY(1,ENTRY(ix,icColumns),";")):BUFFER-VALUE[INT(ENTRY(2,ENTRY(ix,icColumns),";"))]
                               ELSE "") + "|".
    END.
    ELSE ocValues = STRING(hBuffer:ROWID).
  END.
  ELSE ocValues = ?.
  
  IF icColumns NE "ROWID" THEN
    ocValues = SUBSTR(ocValues,1,LENGTH(ocValues) - 1).
END.

DELETE OBJECT hBuffer.


