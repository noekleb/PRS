/* Copy user settings to a set of users
   Parameter string:
   <SourceFile>¤<ObjectName>¤<Context>¤<SettingNameList>¤<rowidlist(JBoxUser)
   Created 29 june 10 by brynjar@chemistry.no
-----------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cContext         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cObjectName      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRowIdList       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSettingNameList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSourceFile      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSourceUser      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ix               AS INTEGER     NO-UNDO.

DEF BUFFER bJBoxUserSetting FOR JBoxUserSetting.

ASSIGN cSourceFile      = ENTRY(1,icParam,"¤")
       cObjectName      = ENTRY(2,icParam,"¤")
       cContext         = ENTRY(3,icParam,"¤")
       cSettingNameList = ENTRY(4,icParam,"¤")
       cRowIdList       = ENTRY(5,icParam,"¤")
       cSourceUser      = DYNAMIC-FUNCTION("getASuserId")
       NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Error in parameters to server procedure " + PROGRAM-NAME(1) + CHR(10) 
           + ERROR-STATUS:GET-MESSAGE(1).
  RETURN.
END.

UpdateUserSettings:
DO TRANSACTION ON ERROR UNDO,LEAVE:
  IF CAN-FIND(FIRST bJBoxUserSetting NO-LOCK
              WHERE bJBoxUserSetting.cJBoxUserId = cSourceUser
                AND bJBoxUserSetting.cSourceFile = cSourceFile
                AND bJBoxUserSetting.cObjectName = cObjectName
                AND bJBoxUserSetting.cContext    = cContext
                AND CAN-DO(cSettingNameList,bJBoxUserSetting.cSettingName))
     THEN DO:
    
    FOR EACH bJBoxUserSetting NO-LOCK
        WHERE bJBoxUserSetting.cJBoxUserId = cSourceUser
          AND bJBoxUserSetting.cSourceFile = cSourceFile
          AND bJBoxUserSetting.cObjectName = cObjectName
          AND bJBoxUserSetting.cContext    = cContext
          AND CAN-DO(cSettingNameList,bJBoxUserSetting.cSettingName)
          :
      DO ix = 1 TO NUM-ENTRIES(cRowIdList):
        FIND FIRST JBoxUser NO-LOCK
             WHERE ROWID(JBoxUser) = TO-ROWID(ENTRY(ix,cRowIdList))
             NO-ERROR.
        IF AVAIL JBoxUser AND JBoxUser.cJBoxUserId NE cSourceUser THEN DO:
          FIND FIRST JBoxUserSetting EXCLUSIVE-LOCK
               WHERE JBoxUserSetting.cJBoxUserId  = JBoxUser.cJBoxUserId
                 AND JBoxUserSetting.cSourceFile  = cSourceFile
                 AND JBoxUserSetting.cObjectName  = cObjectName
                 AND JBoxUserSetting.cContext     = cContext
                 AND JBoxUserSetting.cSettingName = bJBoxUserSetting.cSettingName
               NO-WAIT NO-ERROR.
  
          IF LOCKED JBoxUserSetting THEN DO:
            ocReturn = "Usersettings are locked for update" + PROGRAM-NAME(1).
            UNDO, LEAVE UpdateUserSettings.
          END.
          ELSE IF NOT AVAIL JBoxUserSetting THEN DO:
            CREATE JBoxUserSetting.
            ASSIGN JBoxUserSetting.cJBoxUserId  = JBoxUser.cJBoxUserId
                   JBoxUserSetting.dCreated     = TODAY
                   JBoxUserSetting.cCreatedBy   = cSourceUser
                   JBoxUserSetting.cSourceFile  = cSourceFile
                   JBoxUserSetting.cObjectName  = cObjectName
                   JBoxUserSetting.cContext     = cContext
                   JBoxUserSetting.cSettingName = bJBoxUserSetting.cSettingName
                   .
          END.
          ELSE
            ASSIGN JBoxUserSetting.dModified   = TODAY
                   JBoxUserSetting.cModifiedBy = cSourceUser.

          JBoxUserSetting.cSetting = bJBoxUserSetting.cSetting. 
        END.
      END.
    END.
  END.
  ELSE 
    DO ix = 1 TO NUM-ENTRIES(cRowIdList):
      FIND FIRST JBoxUser NO-LOCK
           WHERE ROWID(JBoxUser) = TO-ROWID(ENTRY(ix,cRowIdList))
           NO-ERROR.
      IF AVAIL JBoxUser AND JBoxUser.cJBoxUserId NE cSourceUser THEN
        FOR EACH JBoxUserSetting EXCLUSIVE-LOCK
            WHERE JBoxUserSetting.cJBoxUserId  = JBoxUser.cJBoxUserId
              AND JBoxUserSetting.cSourceFile  = cSourceFile
              AND JBoxUserSetting.cObjectName  = cObjectName
              AND JBoxUserSetting.cContext     = cContext
              AND CAN-DO(cSettingNameList,JBoxUserSetting.cSettingName)
            :
          DELETE JBoxUserSetting.
        END.
    END.
END.

obOK = ocReturn = "".
