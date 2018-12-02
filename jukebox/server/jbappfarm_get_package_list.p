/* Purpose: Get package list for an application
   Parameters: Application id
               computername
               username (on computer)
   Modified:  15.03.09 by brynjar:
            - Check if there are restart-packages due for install
              These are typical new_JBoxStartup.p or updates to the webclient
              Note: These packages must have a signature different (lower) than 
                    subsequent application so if that isn't true it is fixed here            
-------------------------------------------------------------------------*/                  
DEF INPUT  PARAM icApplicationId    AS CHAR NO-UNDO. 
DEF INPUT  PARAM icComputerName     AS CHAR NO-UNDO.
DEF INPUT  PARAM icComputerUserName AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE       ohTempTable.
DEF OUTPUT PARAM ocReturn           AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocSessionId        AS CHAR NO-UNDO.

DEF VAR icBufferName AS CHAR NO-UNDO.
DEF VAR icCriteria   AS CHAR NO-UNDO.

DEF VAR hBuffer          AS HANDLE NO-UNDO.
DEF VAR httBuffer        AS HANDLE NO-UNDO.
DEF VAR hQuery           AS HANDLE NO-UNDO.
DEF VAR cSize            AS CHAR   NO-UNDO.
DEF VAR cMaxVersion      AS CHAR   NO-UNDO.
DEF VAR cTakeOnlyPackage AS CHAR   NO-UNDO.
DEF VAR cTakeOnlySign    AS CHAR   NO-UNDO.

DEF BUFFER bJBoxLoginSession FOR JBoxLoginSession.

FIND FIRST JBoxLoginSession NO-LOCK
     WHERE JBoxLoginSession.cJBoxUserId = icComputerUserName
     NO-ERROR.
IF NOT AVAIL JBoxLoginSession THEN DO:
  IF OPSYS = "UNIX" THEN
    OUTPUT TO "./downloads.txt" APPEND.
  ELSE
    OUTPUT TO ".\downloads.txt" APPEND.
  PUT UNFORMATTED STRING(TODAY)        ";"
                  STRING(TIME,"HH:MM") ";"
                  icComputerUserName   ";"
                  icComputerName       ";"
                  icApplicationId 
                  SKIP.
  OUTPUT CLOSE.
END.


FIND JBoxApplication NO-LOCK
     WHERE JBoxApplication.iJBoxApplicationId = INT(icApplicationId)
     NO-ERROR.

IF AVAIL JBoxApplication THEN DO:

  CREATE JBoxLoginSession.
  ASSIGN JBoxLoginSession.cSessionId     = ENCODE(icComputerUserName + STRING(TODAY) + STRING(TIME))
         JBoxLoginSession.cJBoxUserId    = icComputerUserName
         JBoxLoginSession.iJBoxCompanyId = JBoxApplication.iJBoxCompanyId
         JBoxLoginSession.cContext       = JBoxApplication.cAppTitle
         JBoxLoginSession.cDateFormat    = "dmy"
         JBoxLoginSession.dCreated       = TODAY
         JBoxLoginSession.iLoginTime     = TIME
         .

  ocReturn = JBoxApplication.cAppTitle + "|"
           + JBoxApplication.cLanguages + "|"
           + JBoxApplication.cBaseLanguage
             .

  /* 15.04.09: */
  FOR EACH JBoxAppPackage NO-LOCK
      OF JBoxApplication
     ,FIRST JBoxPackage NO-LOCK
            OF JBoxAppPackage
            WHERE JBoxPackage.bRestart
      BY JBoxAppPackage.cSignature
      :
    IF cTakeOnlySign NE "" AND JBoxAppPackage.cSignature NE cTakeOnlySign THEN LEAVE.
    ELSE
      ASSIGN cTakeOnlyPackage = cTakeOnlyPackage + (IF cTakeOnlyPackage NE "" THEN "," ELSE "") + STRING(JBoxPackage.iJBoxPackageId)
             cTakeOnlySign    = JBoxAppPackage.cSignature.
  END.
  IF cTakeOnlyPackage NE "" THEN DO:
    FIND LAST bJBoxLoginSession NO-LOCK
         WHERE bJBoxLoginSession.cJBoxUserId = icComputerUserName
           AND bJBoxLoginSession.cContext BEGINS JBoxApplication.cAppTitle
           AND ENTRY(NUM-ENTRIES(bJBoxLoginSession.cContext," "),bJBoxLoginSession.cContext," ") GE cTakeOnlySign 
         NO-ERROR.
    IF AVAIL bJBoxLoginSession THEN
      cTakeOnlyPackage = "".
    ELSE
      FOR EACH JBoxAppPackage EXCLUSIVE-LOCK 
          OF JBoxApplication
             WHERE JBoxAppPackage.cSignature = cTakeOnlySign
               AND NOT CAN-DO(cTakeOnlyPackage,STRING(JBoxAppPackage.iJBoxPackageId))
          :
        JBoxAppPackage.cSignature = STRING(INTEGER(JBoxAppPackage.cSignature) + 1).
      END.
  END.
  /* End 15.04.09: */


  CREATE TEMP-TABLE ohTempTable.
  ohTempTable:CREATE-LIKE("JBoxAppPackage"). 
  ohTempTable:ADD-NEW-FIELD("bProPath","LOGICAL").
  ohTempTable:ADD-NEW-FIELD("fTotSize","DECIMAL").
  ohTempTable:ADD-NEW-FIELD("bComplete","LOGICAL").
  ohTempTable:ADD-NEW-FIELD("bRestart","LOGICAL").
  ohTempTable:TEMP-TABLE-PREPARE("JBoxAppPackage").
  
  CREATE BUFFER hBuffer FOR TABLE "JBoxAppPackage".
  
  httBuffer = ohTempTable:DEFAULT-BUFFER-HANDLE.
  
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " NO-LOCK WHERE iJBoxApplicationId = " + icApplicationId
                     + (IF cTakeOnlyPackage NE "" THEN " AND CAN-DO('" + cTakeOnlyPackage + "',STRING(iJBoxPackageId))" ELSE "") /* 15.04.09 */
                       ).
  hQuery:QUERY-OPEN().
  
  hQuery:GET-FIRST().
  
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FIND FIRST JBoxPackage 
         WHERE JBoxPackage.iJBoxPackageId = INT(hBuffer:BUFFER-FIELD("iJBoxPackageId"):BUFFER-VALUE)
         NO-LOCK NO-ERROR.
    IF AVAIL JBoxPackage AND JBoxPackage.bDownload THEN DO:
      httBuffer:BUFFER-CREATE.
      httBuffer:BUFFER-COPY(hBuffer).
      RUN jbappfarm_package_totsize.p (ROWID(JBoxPackage),"",OUTPUT cSize).
      ASSIGN httBuffer:BUFFER-FIELD("bPropath"):BUFFER-VALUE = JBoxPackage.bProPath
             httBuffer:BUFFER-FIELD("fTotSize"):BUFFER-VALUE = DEC(cSize)
             httBuffer:BUFFER-FIELD("bComplete"):BUFFER-VALUE = JBoxPackage.bComplete
             httBuffer:BUFFER-FIELD("bRestart"):BUFFER-VALUE = JBoxPackage.bRestart
             .
      IF httBuffer:BUFFER-FIELD("cSignature"):BUFFER-VALUE GT cMaxVersion THEN
        cMaxVersion = httBuffer:BUFFER-FIELD("cSignature"):BUFFER-VALUE.
    END.
    hQuery:GET-NEXT().
  END.
  
  JBoxLoginSession.cContext = JBoxLoginSession.cContext + " - " + cMaxVersion.

  DELETE OBJECT hBuffer.
  DELETE OBJECT hQuery.
  DELETE OBJECT ohTempTable.
  
  ocSessionId = JBoxLoginSession.cSessionId.
END.
ELSE DO:
  ocReturn = "Download of packages failed - invalid application id".
  IF OPSYS = "UNIX" THEN
    OUTPUT TO "./downloads.txt" APPEND.
  ELSE
    OUTPUT TO ".\downloads.txt" APPEND.
  PUT UNFORMATTED STRING(TODAY)        ";"
                  STRING(TIME,"HH:MM") ";"
                  icComputerUserName   ";"
                  icComputerName       ";"
                  icApplicationId      ";"
                  ocReturn
                  SKIP.
  OUTPUT CLOSE.
END.
