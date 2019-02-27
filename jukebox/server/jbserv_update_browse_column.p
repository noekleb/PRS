/* Update answerheader - adresselistelinje link
   Parameters:  <iAdresseListeHodeId> 
   
   Created: 21.06.11 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR cField        AS CHAR   NO-UNDO.
DEF VAR cTable        AS CHAR   NO-UNDO.
DEF VAR cValue        AS CHAR   NO-UNDO.
DEF VAR cRowIdent     AS CHAR   NO-UNDO.
DEF VAR cUserId       AS CHAR   NO-UNDO.
DEF VAR hUpdBuffer    AS HANDLE NO-UNDO.
DEF VAR hUpdField     AS HANDLE NO-UNDO.
DEF VAR hRowIdent     AS HANDLE NO-UNDO.
DEF VAR iExtent       AS INT    NO-UNDO.
DEF VAR bOk           AS LOG init true NO-UNDO.


ASSIGN cField   = ENTRY(1,icParam,"|")
       cValue   = ENTRY(2,icParam,"|")
       cUserId      = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE)
       .

IF NUM-ENTRIES(icParam,"|") > 2 THEN DO:
  cRowIdent = ENTRY(3,icParam,"|").
  IF NUM-ENTRIES(cField,".") < 2 THEN DO:
    ocReturn = "Invalid specification of update column. Use [<db.]table.field".
    RETURN.
  END.
  hRowIdent = ihBuffer:BUFFER-FIELD(cRowIdent) NO-ERROR.
  IF NOT VALID-HANDLE(hRowIdent) THEN DO:
    ocReturn = "Invalid specification of update rowid".
    RETURN.
  END.

  IF NUM-ENTRIES(cField,".") > 2 THEN 
    ASSIGN cTable = ENTRY(1,cField,".") + "." + ENTRY(2,cField,".")
           cField = ENTRY(3,cField,".")
           .
  ELSE 
    ASSIGN cTable = ENTRY(1,cField,".")
           cField = ENTRY(2,cField,".")
           .
END.
ELSE 
 ASSIGN cTable = ihBuffer:NAME
        hRowIdent = ihBuffer:BUFFER-FIELD("RowIdent1")
        .

IF INDEX(cField,"[") > 0 THEN
  ASSIGN iExtent = INT(SUBSTR(cField,INDEX(cField,"[") + 1,LENGTH(cField) - INDEX(cField,"[") - 1))
         cField  = SUBSTR(cField,1,INDEX(cField,"[") - 1)
         .

CREATE BUFFER hUpdBuffer FOR TABLE cTable NO-ERROR.
IF NOT VALID-HANDLE(hUpdBuffer) THEN DO:
  ocReturn = "Invalid specification of update table".
  RETURN.
END.
hUpdField = hUpdBuffer:BUFFER-FIELD(cField) NO-ERROR.
IF NOT VALID-HANDLE(hUpdField) THEN DO:
  ocReturn = "Invalid specification of update field".
  RETURN.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

DO TRANSACTION ON ERROR UNDO,LEAVE:
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  
    hUpdBuffer:FIND-BY-ROWID(TO-ROWID(hRowIdent:BUFFER-VALUE),EXCLUSIVE-LOCK,NO-WAIT).
    IF hUpdBuffer:AVAIL THEN DO:
      RUN jbserv_assignstringvalue.p (hUpdField,iExtent,cValue,OUTPUT bOk).
      IF not bOk THEN DO:
        ocReturn = "Invalid value for field".
        LEAVE.
      END.
    END.
  
              
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery NO-ERROR.
DELETE OBJECT hUpdBuffer NO-ERROR.

obOk = ocReturn = "".

