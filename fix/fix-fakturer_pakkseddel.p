DEF VAR ihBuffer AS HANDLE NO-UNDO.
DEF VAR ocReturn AS CHAR NO-UNDO.
DEF VAR obOk AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttPkSdlHode
  FIELD PkSdlId   AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  FIELD SendtDato AS DATE 
  FIELD PkSdlNr   AS CHARACTER 
  FIELD EkstId    AS CHARACTER 
  INDEX Pakkseddel PkSdlNr SendtDato
  . 
DEFINE TEMP-TABLE tmpPkSdlHode LIKE PkSdlHode.
   
DEFINE TEMP-TABLE ttpkSdlLinje LIKE PkSdlLinje
  .


FIND PkSdlHode EXCLUSIVE-LOCK WHERE
    PkSdlHode.PkSdlId = 136848 NO-ERROR.

DISPLAY
    PkSdlId
    PkSdlNr
    PksdlStatus
    .
/*
FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
    CREATE ttpkSdlLinje.              
    BUFFER-COPY pkSdlLinje TO ttpkSdlLinje.
END.
ihBuffer = BUFFER ttpkSdlLinje:HANDLE. 

MESSAGE 'Gurre'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

RUN pksdl_internsalg.p ('', ihBuffer,'' ,OUTPUT ocReturn, OUTPUT obOk).

MESSAGE ocReturn SKIP obOk
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
