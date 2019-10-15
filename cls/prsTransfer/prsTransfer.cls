 
 /*------------------------------------------------------------------------
    File        : prsTransfer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Tue Oct 15 09:44:26 CEST 2019
    Notes       : 

FOR EACH sendtowebtrans WHERE 
    sendtowebtrans.batchtype = "PRICE" AND 
    date(sendtowebtrans.received) > DATE(10,1,2019) NO-LOCK:

    FIND sendtowebdata OF sendtowebtrans NO-LOCK NO-ERROR.
    IF AVAIL sendtowebdata THEN 
        DO:
            COPY-LOB sendtowebdata.blobdata TO FILE 
                ".\json\Web_" + sendtowebtrans.batchtype + STRING(sendtowebtrans.batchnr) + ".json".
        END.
END.
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.prsTransfer.prsTransfer: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE PROPERTY bConnected AS LOG  NO-UNDO GET. SET.

  DEFINE VARIABLE bTest AS LOG NO-UNDO.
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCmd AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDbName AS CHARACTER NO-UNDO.

  DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
      
  CONSTRUCTOR PUBLIC prsTransfer ( INPUT pcLogg AS CHARACTER  ):
    SUPER ().

    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().
    
    ASSIGN 
      cLogg = pcLogg
      .
      
    IF cLogg = '' THEN 
      cLogg = 'prsTransfer' + REPLACE(STRING(TODAY),'/','') /*+ '_' + REPLACE(STRING(TIME),':','')*/
      .
    IF SEARCH('test.txt') <> ? THEN 
      bTest = TRUE.

    settParametre().
    
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL connectDB(  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    CONNECT VALUE(cCmd) NO-ERROR.
    ASSIGN 
      bConnected = CONNECTED(cDbName)
      bResult = bConnected
      .  

    IF bTest THEN
    DO:
      IF bConnected THEN  
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '  connectDB(): DB ' + cDbName + ' oppkoblet.' 
            ). 
      ELSE 
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '  connectDB(): ** Klarte ikke koble opp DB ' + cDbName + '.' 
            ). 
    END.
    
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL disconnectDB(  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    DISCONNECT VALUE(cDbName) NO-ERROR.

    ASSIGN 
      bConnected = CONNECTED(cDbName)
      bResult = NOT bConnected
      .  

    IF bTest THEN
    DO:
      IF bConnected = FALSE THEN  
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '  connectDB(): DB ' + cDbName + ' nedkoblet.' 
            ). 
      ELSE 
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '  connectDB(): ** Klarte ikke koble ned DB ' + cDbName + '.' 
            ). 
    END.

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
     -db c:\db\gant\prstransfer -ld prstransfer -H localhost -S  10095 -U pub -P pub -N tcp
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL settParametre(  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cHost AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cService AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPwd AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProtocoll AS CHARACTER NO-UNDO.

    IF SEARCH('tnc.txt') <> ? THEN 
      ASSIGN
        cHost      = 'localhost'
        cService   = '10095'
        cUser      = 'pub'
        cPwd       = 'pub'
        .
    ELSE 
      ASSIGN
        cHost      = 'localhost'
        cService   = '10095'
        cUser      = 'pub'
        cPwd       = 'pub'
        .
            
    ASSIGN 
      cDbName    = 'prstransfer'
      cProtocoll = '-N tcp'
      cCmd       = '-db ' + cDbName + ' -ld ' + cDbName + ' -H ' + cHost + ' -S ' + cService + ' -U ' + cUser + ' -P ' + cPwd + ' ' + cProtocoll
      .
    
    RETURN bResult.

  END METHOD.

  DESTRUCTOR PUBLIC prsTransfer ( ):

  END DESTRUCTOR.

END CLASS.