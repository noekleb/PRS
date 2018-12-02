

/* ***************************  Main Block  *************************** */

/* ---------------------------------------- (c) 2014 CHSO --------------- */
/* ---- Program Name : putmMember.p                                  ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-2            
------------------------------------------------------------------------- */

DEFINE VARIABLE dDato AS DATE INIT TODAY NO-UNDO. 
                       
DEFINE VARIABLE hDset AS HANDLE NO-UNDO.
DEFINE VARIABLE lDebug AS LOGICAL INIT FALSE NO-UNDO. 


DEFINE TEMP-TABLE tt_stlinje LIKE skotex.stlinje. 

DEFINE VARIABLE iCnt AS INT NO-UNDO.
DEFINE VARIABLE hServer AS HANDLE NO-UNDO. 
DEFINE VARIABLE lConnected AS LOG NO-UNDO. 
DEFINE VARIABLE cConnectionString AS CHAR NO-UNDO.
DEFINE VARIABLE cSessionid AS CHAR NO-UNDO. 
DEFINE VARIABLE cCompanyId AS CHAR NO-UNDO. 
DEFINE VARIABLE cUserid AS CHAR NO-UNDO. 
DEFINE VARIABLE cPassword AS CHAR NO-UNDO. 
DEFINE VARIABLE lOK AS LOGICAL NO-UNDO.

DEFINE VARIABLE LastRegisterDateTime AS DATETIME NO-UNDO. 

DEFINE VARIABLE LastSendtDato AS DATE NO-UNDO. 
DEFINE VARIABLE LastSendtTid  AS INT NO-UNDO. 
DEFINE VARIABLE CurrentTime AS DATETIME NO-UNDO. 
DEFINE VARIABLE RemoteSystemId AS CHAR NO-UNDO. 
DEFINE VARIABLE RemoteTableId AS CHAR NO-UNDO. 



/* {syspara.i 5 1 1 cStoreNumber}
   {syspara.i 5 1 1 cConnectionString}
   {syspara.i 5 1 1 cUserid}
   {syspara.i 5 1 1 cPassword} */


DEFINE TEMP-TABLE RecordChanges 
    FIELD updateDateTime AS DATETIME 
    FIELD updateNumber AS INT 
    FIELD updateRowId AS ROWID. 


FUNCTION  getLocalUpdatedRecords   RETURNS LOGICAL (INPUT readFromDate AS DATETIME ):
    DEFINE VARIABLE iCnt AS INT NO-UNDO. 


    FIND bruker WHERE bruker.brukerid = "PRS" NO-LOCK NO-ERROR.
    IF NOT AVAIL bruker THEN
    RETURN FALSE.

    FOR EACH butikktilgang WHERE butikktilgang.BrGrpNr = bruker.BrGrpNr NO-LOCK:

    /*  sjekk mot tilgjengelige butikker bruker gruppe */ 
    /*  bruker gruppe 50 - tilgang */ 


        FOR EACH skotex.stLinje NO-LOCK WHERE 
                        stLinje.butik    = butikktilgang.butik AND 
                        stLinje.sttypeid = "butstat":
     
            /* added check for datetime ONLY GE RECORDS INCLUDED */
            IF stLinje.edato = ? THEN NEXT . 
            IF DATETIME(stLinje.edato,stLinje.etid * 1000) LT readFromDate THEN NEXT. 
             
            DO:
                CREATE RecordChanges. 
                iCnt = iCnt + 1. 
                RecordChanges.UpdateDateTime = DATETIME(stlinje.edato,stlinje.etid * 1000 ). 
                RecordChanges.UpdateNumber   = iCnt. 
                RecordChanges.updateRowid    = ROWID(stLinje). 
            END. 
        END.
    END. 

   
END.


FUNCTION  copyLocalUpdatedRecords   RETURNS LOGICAL ():

    FOR EACH RecordChanges NO-LOCK: 
        FIND skotex.stlinje WHERE ROWID(stlinje) = recordChanges.updateRowid NO-LOCK NO-ERROR. 
        IF AVAIL stlinje THEN
        DO:
            CREATE tt_stlinje. 
            BUFFER-COPY stlinje TO tt_stlinje. 
        END. 
    END.
    EMPTY TEMP-TABLE RecordChanges. 

    IF lDebug THEN TEMP-TABLE tt_stlinje:WRITE-XML("file","c:\tmp\tt_stlinje.xml",TRUE,?,?,FALSE,TRUE).

END.



/* default if not defined */ 
IF TRIM(cConnectionString) = ""  THEN
   cConnectionString = "-URL http://appfarm.netextend.no/aia/Aia?AppService=PRSTrans -sessionModel session-free".

cUserId        = "PRS". 
cPassword      = "PRSTest". 
cCompanyid     = "200".
RemoteTableId  = "stlinje" .
RemoteSystemId = "PRS".

CREATE SERVER hServer. 
lConnected = hServer:CONNECT(cConnectionString) NO-ERROR .

IF lConnected THEN
DO:
  RUN WSLogin.p ON hServer (cUserid, cPassword, OUTPUT cSessionId, OUTPUT lok) NO-ERROR. 

  IF NOT lok THEN
  DO:
      hServer:DISCONNECT(). 
      DELETE OBJECT hServer NO-ERROR. 
      DELETE OBJECT hDset NO-ERROR. 
      RETURN. 
  END.

  RUN getTableLastCreatedDateTime.p ON hServer 
      (cSessionId,
       cCompanyId, 
       RemoteTableId, 
       RemoteSystemId,
       FALSE, /* false = lastupdateddatefromRemoteSystem */
       OUTPUT LastRegisterDateTime, 
       OUTPUT lok). 
  
  getLocalUpdatedRecords   (LastRegisterDateTime).
  copyLocalUpdatedRecords ().

  /* Call General update routine based on origion Table RemoteSystemid */
  RUN putClientUpdatedRecords.p ON hServer (cSessionId,RemoteTableId,RemoteSystemId,TABLE tt_stlinje,OUTPUT lok).  
  EMPTY TEMP-TABLE tt_stlinje.
  

  hServer:DISCONNECT(). 
  DELETE OBJECT hServer NO-ERROR. 

END. 



  
