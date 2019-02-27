/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : getClientSessionId.p                          ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-2            
------------------------------------------------------------------------- */

DEFINE VARIABLE dDato AS DATE INIT TODAY NO-UNDO. 
                       
DEFINE VARIABLE hDset AS HANDLE NO-UNDO.
DEFINE VARIABLE lDebug AS LOGICAL INIT FALSE NO-UNDO. 
DEFINE TEMP-TABLE tt_nets NO-UNDO     LIKE nets .
DEFINE TEMP-TABLE tt_bonghode  NO-UNDO LIKE bonghode. 
DEFINE TEMP-TABLE tt_bonglinje NO-UNDO LIKE bonglinje. 
 

DEFINE TEMP-TABLE tt_Updated NO-UNDO
  FIELD transactionid AS CHAR
  FIELD sendtDato AS DATE 
  FIELD sendt AS LOGICAL 
  FIELD sendtTid AS INT. 

/* redefine for xml-export */
DEFINE VARIABLE tbl AS HANDLE NO-UNDO. 
DEFINE VARIABLE fld AS HANDLE NO-UNDO. 
tbl = BUFFER tt_bonglinje:HANDLE. 
fld = tbl:BUFFER-FIELD("mva%"):HANDLE.
fld:SERIALIZE-NAME = "mva_prosent".

CREATE DATASET hDset.
hDset:ADD-BUFFER( BUFFER tt_nets:HANDLE). 
hDset:ADD-BUFFER( BUFFER tt_bonghode:HANDLE).
hDset:ADD-BUFFER( BUFFER tt_bonglinje:HANDLE).

hDset:ADD-RELATION(BUFFER tt_nets:HANDLE,BUFFER tt_Bonghode:HANDLE,"b_id,b_id",TRUE,TRUE).
hDset:ADD-RELATION(BUFFER tt_bonghode:HANDLE,BUFFER tt_Bonglinje:HANDLE,"b_id,b_id",TRUE,TRUE).
                                           

DEFINE VARIABLE iCnt AS INT NO-UNDO.
DEFINE VARIABLE hServer AS HANDLE NO-UNDO. 
DEFINE VARIABLE lConnected AS LOG NO-UNDO. 
DEFINE VARIABLE cConnectionString AS CHAR NO-UNDO.
DEFINE VARIABLE cSessionid AS CHAR NO-UNDO. 
DEFINE VARIABLE cStoreNumber AS CHAR NO-UNDO. 
DEFINE VARIABLE cUserid AS CHAR NO-UNDO. 
DEFINE VARIABLE cPassword AS CHAR NO-UNDO. 
DEFINE VARIABLE lOK AS LOGICAL NO-UNDO.
DEFINE VARIABLE LastRegistrertDato AS DATE NO-UNDO. 
DEFINE VARIABLE LastRegistrertTid  AS INT NO-UNDO. 
DEFINE VARIABLE LastSendtDato AS DATE NO-UNDO. 
DEFINE VARIABLE LastSendtTid  AS INT NO-UNDO. 


{syspara.i 5 1 1 cStoreNumber}

/* {syspara.i 5 1 1 cConnectionString}
   {syspara.i 5 1 1 cUserid}
   {syspara.i 5 1 1 cPassword}
                                      */

/* default if not defined */ 
IF TRIM(cConnectionString) = ""  THEN
   cConnectionString = "-URL http://appfarm.netextend.no/aia/Aia?AppService=PRSTrans -sessionModel session-free".

cUserId   = "MXSport". 
cPassword = "netsIntegrasjon2014". 


CREATE SERVER hServer. 
lConnected = hServer:CONNECT(cConnectionString) NO-ERROR .

IF lConnected THEN
DO:
  RUN WSLogin.p ON hServer (cUserid, cPassword, OUTPUT cSessionId,OUTPUT lok) NO-ERROR. 

  IF NOT lok THEN
  DO:
      hServer:DISCONNECT(). 
      DELETE OBJECT hServer NO-ERROR. 
      DELETE OBJECT hDset NO-ERROR. 
      RETURN. 
  END.

  RUN getNetsLastCreatedDateTime.p ON hServer 
      (cSessionId,
       INT(cStoreNumber), 
       OUTPUT LastRegistrertDato, 
       OUTPUT LastRegistrertTid,
       OUTPUT lok). 

  FOR EACH nets WHERE 
           nets.RegistrertDato GE LastRegistrertDato  NO-LOCK: 
           
    if nets.RegistrertDato = ? then next. 
    
    if nets.RegistrertDato = LastRegistrertDato and 
       nets.RegistrertTid lt LastRegistrertTid then next.     


    CREATE tt_nets. 
    BUFFER-COPY nets TO tt_nets. 
  
    FIND bonghode WHERE 
         bonghode.b_id = nets.b_id NO-LOCK NO-ERROR. 

    IF AVAIL bonghode THEN
    DO:
      CREATE tt_bonghode. 
      BUFFER-COPY bonghode TO tt_bonghode. 
      FOR EACH bonglinje WHERE
             bonglinje.b_id = bonghode.b_id NO-LOCK:
          CREATE tt_bonglinje. 
          BUFFER-COPY bonglinje TO tt_bonglinje. 
      END.
    END. 
    iCnt = iCnt + 1. 
  END.
   
  IF lDebug THEN hDset:WRITE-XML("FILE","nets_export.xml",TRUE,?,?,FALSE,TRUE) NO-ERROR. 
                     
  RUN putNetsUpdated.p ON hServer
    (cSessionId, 
     INT(cStoreNumber),
     TABLE tt_nets, 
     TABLE tt_bonghode,
     TABLE tt_bonglinje) NO-ERROR. 
   
  EMPTY TEMP-TABLE tt_bonghode. 
  EMPTY TEMP-TABLE tt_bonglinje. 
  EMPTY TEMP-TABLE tt_nets. 


  FOR EACH nets WHERE nets.iJBoxCompanyId = INT(cStoreNumber) 
      AND nets.sendtdato NE ? NO-LOCK 
   BY nets.sendtDato DESCENDING BY nets.SendtTid DESCENDING: 
      LastSendtDato = nets.SendtDato.
      LastSendtTid  = nets.SendtTid. 
      LEAVE.
  END.

  RUN getNetsUpdated.p ON hServer
     (cSessionId,
      INT(cStoreNumber), 
      LastSendtDato, 
      LASTSendtTid, 
      OUTPUT TABLE tt_Updated, 
      OUTPUT lok).

  hServer:DISCONNECT(). 
  DELETE OBJECT hServer NO-ERROR. 

  DISABLE TRIGGERS FOR LOAD OF nets. 
  DISABLE TRIGGERS FOR DUMP OF nets. 

  IF lok THEN
  FOR EACH tt_Updated NO-LOCK:
      FIND nets WHERE nets.transactionid = tt_updated.transactionid NO-ERROR. 
      IF AVAIL nets THEN
      DO:
          ASSIGN 
          nets.sendtDato = tt_updated.sendtDato
          nets.sendtTid  = tt_updated.sendtTid
          nets.sendt     = tt_updated.sendt.
      END.
  END.

  EMPTY TEMP-TABLE tt_updated. 
END. 

DELETE OBJECT hDset NO-ERROR. 

QUIT.

