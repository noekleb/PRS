/* Assign value for many records in customer
   Parameters:  <Fieldname>|<Value> 
      
            + <ROWID>|<List of rowid's>
         OR + <List of primary keys >
         
         OR temp-table containing CustNum 
   
   Created: 21.10.04 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEF VAR hCustBuff   AS HANDLE NO-UNDO.

hCustBuff = BUFFER Customer:HANDLE.

/* If the input parameter is a list, create temp-table records first */
IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam,"|") > 2 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("CustNum","Customer.CustNum").
  httTable:TEMP-TABLE-PREPARE("ttCustomer").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF ENTRY(3,icParam,"|") = "ROWID" THEN
    DO ix = 4 TO NUM-ENTRIES(icParam,"|"):
      FIND Customer WHERE ROWID(Customer) = TO-ROWID(ENTRY(ix,icParam,"|")) NO-LOCK NO-ERROR.
      IF AVAIL Customer THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER Customer:HANDLE).
      END.
    END.
  ELSE DO:
    DO ix = 3 TO NUM-ENTRIES(icParam,"|"):
      FIND Customer 
           WHERE Customer.CustNum = INT(ENTRY(ix,icParam,"|")) NO-LOCK NO-ERROR.
      IF AVAIL Customer THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER Customer:HANDLE).
      END.
    END.
  END.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().


DO TRANSACTION:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    obOK = hCustBuff:FIND-FIRST("WHERE CustNum = " + STRING(ihBuffer:BUFFER-FIELD("CustNum"):BUFFER-VALUE),
                                 EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
    IF obOk THEN DO:
      IF hCustBuff:BUFFER-FIELD(ENTRY(1,icParam,"|")):DATA-TYPE = "DECIMAL" THEN 
        hCustBuff:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = DEC(ENTRY(2,icParam,"|")).
      ELSE IF hCustBuff:BUFFER-FIELD(ENTRY(1,icParam,"|")):DATA-TYPE = "DATE" THEN
        hCustBuff:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = DATE(ENTRY(2,icParam,"|")).
      ELSE IF hCustBuff:BUFFER-FIELD(ENTRY(1,icParam,"|")):DATA-TYPE = "INTEGER" THEN
        hCustBuff:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = INT(ENTRY(2,icParam,"|")).
      ELSE IF hCustBuff:BUFFER-FIELD(ENTRY(1,icParam,"|")):DATA-TYPE = "LOGICAL" THEN
        hCustBuff:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = LOGICAL(ENTRY(2,icParam,"|")).
      ELSE 
        hCustBuff:BUFFER-FIELD(ENTRY(1,icParam,"|")):BUFFER-VALUE = ENTRY(2,icParam,"|").

    END.
    ELSE DO:
      ocReturn = "Customer "
                 + STRING(ihBuffer:BUFFER-FIELD("CustNum"):BUFFER-VALUE) +
                 " not avail for update".
      LEAVE.
    END.
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

