DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR ix                 AS INT    NO-UNDO.
DEF VAR bOK                AS LOG    NO-UNDO.
DEF VAR hBuffMenu          AS HANDLE NO-UNDO.
DEF VAR httBuffer          AS HANDLE NO-UNDO.
DEF VAR hQuery             AS HANDLE NO-UNDO.
DEF VAR hQueryUserGroupMem AS HANDLE NO-UNDO.
DEF VAR iCurrStyle         AS INT    NO-UNDO.
DEF VAR hBuffCompanyMenu   AS HANDLE NO-UNDO.
DEF VAR hBuffUser          AS HANDLE NO-UNDO.
DEF VAR hBuffUserMenu      AS HANDLE NO-UNDO.
DEF VAR hBuffUserGroupMem  AS HANDLE NO-UNDO.
DEF VAR hBuffCompanyUser   AS HANDLE NO-UNDO.
DEF VAR hBuffMenuTrans     AS HANDLE NO-UNDO.
DEF VAR bSuperUser         AS LOG    NO-UNDO.
DEF VAR cUserGroupList     AS CHAR   NO-UNDO.
DEF VAR bCompanyAdmin      AS LOG    NO-UNDO.
DEF VAR hLimitToCompAdm    AS HANDLE NO-UNDO.
DEF VAR hLimitToSuperUser  AS HANDLE NO-UNDO.
DEF VAR cLanguageParam     AS CHAR   NO-UNDO.
DEF VAR cDbList            AS CHAR   NO-UNDO.
DEF VAR bAppendMenuNum     AS LOG    NO-UNDO INIT YES.
DEF VAR bRestUnlessAllowed AS LOG    NO-UNDO.

DEF VAR iParentMenuId      AS INT    NO-UNDO.
DEF VAR iParentNodeIdx     AS INT    NO-UNDO.
DEF VAR cParentMenuType    AS CHAR   NO-UNDO.
DEF VAR cTabGroupTitle     AS CHAR   NO-UNDO.
DEF VAR cProgramList       AS CHAR   NO-UNDO.
DEF VAR iy                 AS INT    NO-UNDO.


CREATE TEMP-TABLE hTempTable.
CREATE BUFFER hBuffMenu FOR TABLE "JBoxMenu".
CREATE BUFFER hBuffUser FOR TABLE "JBoxUser" NO-ERROR.
CREATE BUFFER hBuffUserMenu FOR TABLE "JBoxUserMenu" NO-ERROR.
CREATE BUFFER hBuffUserGroupMem FOR TABLE "JBoxUserGroupMembers" NO-ERROR.
CREATE BUFFER hBuffCompanyMenu FOR TABLE "JBoxCompanyMenu" NO-ERROR.
CREATE BUFFER hBuffMenuTrans FOR TABLE "JBoxMenuTranslation" NO-ERROR.
hLimitToCompAdm   = hBuffMenu:BUFFER-FIELD("bLimitToCompanyAdmins") NO-ERROR.
hLimitToSuperUser = hBuffMenu:BUFFER-FIELD("bLimitToSuperUsers") NO-ERROR.

{incl/validatesession.i}

DO ix = 1 TO NUM-ENTRIES(cCurrContext):
  IF ENTRY(ix,cCurrContext) BEGINS "dblist" THEN
    cDbList = REPLACE(ENTRY(2,ENTRY(ix,cCurrContext),";"),"|",",").
END.

IF NUM-ENTRIES(icParam,"|") < 6 THEN DO:
  ocReturn = "Error in parameter definition for " + PROGRAM-NAME(1).
  RETURN.
END.

ASSIGN cTabGroupTitle = ENTRY(5,icParam,"|")
       cProgramList   = ENTRY(6,icParam,"|").

IF NUM-ENTRIES(icParam,"|") > 3 AND ENTRY(4,icParam,"|") NE "" THEN
  bRestUnlessAllowed = LOGICAL(ENTRY(4,icParam,"|")) NO-ERROR.

IF NUM-ENTRIES(icParam,"|") > 2 AND ENTRY(3,icParam,"|") NE "" THEN
  bAppendMenuNum = LOGICAL(ENTRY(3,icParam,"|")) NO-ERROR.

IF NUM-ENTRIES(icParam,"|") > 1 THEN
  ASSIGN cLanguageParam = ENTRY(2,icParam,"|")
         icParam = ENTRY(1,icParam,"|")
         .
IF VALID-HANDLE(hBuffMenuTrans) AND cLanguageParam NE "" THEN 
  cCurrLanguage = cLanguageParam.

IF VALID-HANDLE(hBuffUser) THEN DO:
  hBuffUser:FIND-FIRST("WHERE cJBoxUserId = '" + cCurrUserId + "'",NO-LOCK) NO-ERROR.  
  IF hBuffUser:AVAIL THEN
    bSuperUser = hBuffUser:BUFFER-FIELD("bSuperUser"):BUFFER-VALUE.
END.

IF VALID-HANDLE(hBuffUserGroupMem) THEN DO:
  CREATE QUERY hQueryUserGroupMem.
  hQueryUserGroupMem:SET-BUFFERS(hBuffUserGroupMem).
  hQueryUserGroupMem:QUERY-PREPARE("FOR EACH JBoxUserGroupMembers NO-LOCK WHERE cJBoxUserId = '" + cCurrUserId + "'").
  hQueryUserGroupMem:QUERY-OPEN().
  hQueryUserGroupMem:GET-FIRST().
  REPEAT WHILE NOT hQueryUserGroupMem:QUERY-OFF-END:
    cUserGroupList = cUserGroupList + (IF cUserGroupList NE "" THEN "," ELSE "") + STRING(hBuffUserGroupMem:BUFFER-FIELD("iJBoxUserGroupId"):BUFFER-VALUE).
    hQueryUserGroupMem:GET-NEXT().
  END.
END.

CREATE BUFFER hBuffCompanyUser FOR TABLE "JBoxCompanyUser" NO-ERROR.
IF VALID-HANDLE(hBuffCompanyUser) THEN DO:
  hBuffCompanyUser:FIND-FIRST("WHERE cJBoxUserId = '" + cCurrUserId + "' AND iJBoxCompanyId = " + STRING(iCurrCompanyId),NO-LOCK) NO-ERROR.  
  IF hBuffCompanyUser:AVAIL THEN
    bCompanyAdmin = hBuffCompanyUser:BUFFER-FIELD("bSuperUserCompany"):BUFFER-VALUE.
END.

FUNCTION CompanyAccess RETURNS LOGICAL (INPUT iiMenuId AS INT ): 
  DEF VAR bLimitToCompAdmin AS LOG NO-UNDO.

  IF bSuperUser THEN RETURN YES.

  IF VALID-HANDLE(hLimitToCompAdm) OR VALID-HANDLE(hLimitToSuperUser) THEN DO:
    hBuffMenu:FIND-FIRST("WHERE iJBoxMenuId = " + STRING(iiMenuId),NO-LOCK) NO-ERROR.
    IF hBuffMenu:AVAIL THEN DO:
      IF hLimitToSuperUser:BUFFER-VALUE THEN RETURN NO.
      ELSE bLimitToCompAdmin = hLimitToCompAdm:BUFFER-VALUE.
    END.
  END.
  IF bLimitToCompAdmin AND NOT bCompanyAdmin THEN RETURN NO.

  IF VALID-HANDLE(hBuffCompanyMenu) THEN DO:
    hBuffCompanyMenu:FIND-FIRST("WHERE iJBoxMenuId   = " + STRING(iiMenuId)
                              + " AND iJBoxCompanyId = " + STRING(iCurrCompanyId),NO-LOCK) NO-ERROR.
    IF NOT hBuffCompanyMenu:AVAIL THEN DO:
      hBuffCompanyMenu:FIND-FIRST("WHERE iJBoxMenuId   = " + STRING(iiMenuId),NO-LOCK) NO-ERROR.
      IF NOT bRestUnlessAllowed THEN
        RETURN NOT hBuffCompanyMenu:AVAIL.
      ELSE
        RETURN NO.
    END.
  END.
  
  RETURN YES.
END FUNCTION.

FUNCTION UserAccess RETURNS LOGICAL (INPUT iiMenuId AS INT ): 
  DEF VAR iy AS INT NO-UNDO.

  IF bSuperUser THEN RETURN YES.

  IF VALID-HANDLE(hBuffUserMenu) THEN DO:
    hBuffUserMenu:FIND-FIRST("WHERE cJBoxUserId = '" + cCurrUserId + "' AND iJBoxMenuId = " + STRING(iiMenuId),NO-LOCK) NO-ERROR.  
    IF NOT hBuffUserMenu:AVAIL THEN 
      DO iy = 1 TO NUM-ENTRIES(cUserGroupList):
        hBuffUserMenu:FIND-FIRST("WHERE iJBoxUserGroupId = " + ENTRY(iy,cUserGroupList) + " AND iJBoxMenuId = " + STRING(iiMenuId),NO-LOCK) NO-ERROR.  
        IF hBuffUserMenu:AVAIL THEN
          RETURN YES.
      END.
    ELSE RETURN YES.

    hBuffUserMenu:FIND-FIRST("WHERE iJBoxMenuId = " + STRING(iiMenuId),NO-LOCK) NO-ERROR.  
    IF NOT bRestUnlessAllowed THEN
      RETURN NOT hBuffUserMenu:AVAIL.
  END.
  ELSE RETURN YES.

  RETURN NO.
END.

FUNCTION DbAccess RETURNS LOGICAL (INPUT iiMenuId AS INT ): 

  DEF VAR iy AS INT NO-UNDO.

  IF bSuperUser THEN RETURN YES.

  hBuffMenu:FIND-FIRST("WHERE iJBoxMenuId = " + STRING(iiMenuId),NO-LOCK) NO-ERROR.

  IF hBuffMenu:AVAIL THEN 
    IF hBuffMenu:BUFFER-FIELD("cParameter"):BUFFER-VALUE BEGINS "db_" THEN DO:
      DO iy = 1 TO NUM-ENTRIES(cDbList):
        IF ENTRY(iy,cDbList) = SUBSTR(hBuffMenu:BUFFER-FIELD("cParameter"):BUFFER-VALUE,4) THEN RETURN YES.
      END.
      RETURN NO.
    END.
  
  RETURN YES.
END FUNCTION.


DO ix = 1 TO hBuffMenu:NUM-FIELDS:
  hTempTable:ADD-LIKE-FIELD(hBuffMenu:BUFFER-FIELD(ix):NAME,hBuffMenu:BUFFER-FIELD(ix)).
END.
hTempTable:ADD-NEW-FIELD("iParentMenuId","INTEGER").
hTempTable:ADD-NEW-FIELD("iRootMenuId","INTEGER").
hTempTable:ADD-NEW-FIELD("iLevel","INTEGER").
hTempTable:ADD-NEW-FIELD("iSeq","INTEGER").
hTempTable:ADD-NEW-FIELD("iNodeIndex","INTEGER").
hTempTable:ADD-NEW-FIELD("iColourCode","INTEGER").
hTempTable:ADD-NEW-FIELD("RowIdent1","CHARACTER").
hTempTable:ADD-NEW-FIELD("bDisabled","LOGICAL").
hTempTable:ADD-NEW-FIELD("hMenuItem","HANDLE").
hTempTable:ADD-NEW-FIELD("bMultiple","LOGICAL").
hTempTable:ADD-NEW-FIELD("cParentMenuType","CHARACTER").
hTempTable:ADD-NEW-FIELD("bHasChildSubMenu","LOGICAL").
hTempTable:ADD-NEW-FIELD("cParentImage","CHARACTER").
hTempTable:ADD-NEW-FIELD("iParentNodeIndex","INTEGER").
hTempTable:ADD-NEW-FIELD("bHasChildMenuItem","LOGICAL").

hTempTable:TEMP-TABLE-PREPARE("JBoxMenu").
httBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.

FIND FIRST JBoxMenu NO-LOCK
     WHERE JBoxMenu.cMenuType = "menu"
     NO-ERROR.
IF AVAIL JBoxMenu THEN DO:
  httBuffer:BUFFER-CREATE().
  httBuffer:BUFFER-COPY(BUFFER JBoxMenu:HANDLE).
  ASSIGN ix = ix + 1
         httBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE    = 0
         httBuffer:BUFFER-FIELD("iSeq"):BUFFER-VALUE             = 1
         httBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE       = ix
         httBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE           = 1
         httBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE        = STRING(ROWID(JBoxMenu))
         httBuffer:BUFFER-FIELD("cParentMenuType"):BUFFER-VALUE  = ""
         httBuffer:BUFFER-FIELD("iParentNodeIndex"):BUFFER-VALUE = 0
         cParentMenuType                                         = JBoxMenu.cMenuType
         iParentNodeIdx                                          = ix
         iParentMenuId                                           = JBoxMenu.iJBoxMenuId
         .

  FIND FIRST JBoxMenu NO-LOCK
       WHERE JBoxMenu.cMenuType = "tv-nav-bar"
       NO-ERROR.
  IF AVAIL JBoxMenu THEN DO:
    httBuffer:BUFFER-CREATE().
    httBuffer:BUFFER-COPY(BUFFER JBoxMenu:HANDLE).
    ASSIGN ix = ix + 1
           httBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE    = iParentMenuId
           httBuffer:BUFFER-FIELD("iSeq"):BUFFER-VALUE             = 1
           httBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE       = ix
           httBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE           = 2
           httBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE        = STRING(ROWID(JBoxMenu))
           httBuffer:BUFFER-FIELD("cParentMenuType"):BUFFER-VALUE  = cParentMenuType
           httBuffer:BUFFER-FIELD("iParentNodeIndex"):BUFFER-VALUE = iParentNodeIdx
           cParentMenuType                                         = JBoxMenu.cMenuType
           iParentNodeIdx                                          = ix
           iParentMenuId                                           = JBoxMenu.iJBoxMenuId
           .

    FIND FIRST JBoxMenu NO-LOCK
         WHERE JBoxMenu.cMenuType = "sub-menu"
         NO-ERROR.
    IF AVAIL JBoxMenu THEN DO:
      httBuffer:BUFFER-CREATE().
      httBuffer:BUFFER-COPY(BUFFER JBoxMenu:HANDLE).
      ASSIGN ix = ix + 1
             httBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE    = iParentMenuId
             httBuffer:BUFFER-FIELD("iSeq"):BUFFER-VALUE             = 1
             httBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE       = ix
             httBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE           = 3
             httBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE        = STRING(ROWID(JBoxMenu))
             httBuffer:BUFFER-FIELD("cParentMenuType"):BUFFER-VALUE  = cParentMenuType
             httBuffer:BUFFER-FIELD("iParentNodeIndex"):BUFFER-VALUE = iParentNodeIdx
             httBuffer::cMenuLabel                                   = cTabGroupTitle
             httBuffer::bHasChildMenuItem                            = YES
             cParentMenuType                                         = JBoxMenu.cMenuType
             iParentNodeIdx                                          = ix
             iParentMenuId                                           = JBoxMenu.iJBoxMenuId
             .

      DO iy = 1 TO NUM-ENTRIES(cProgramList):
        FIND FIRST JBoxMenu NO-LOCK
             WHERE JBoxMenu.cMenuType = "menu-item"
               AND JBoxMenu.cLaunch   = ENTRY(iy,cProgramList)
             NO-ERROR.
        IF AVAIL JBoxMenu THEN DO:
          httBuffer:BUFFER-CREATE().
          httBuffer:BUFFER-COPY(BUFFER JBoxMenu:HANDLE).
          ASSIGN ix = ix + 1
                 httBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE    = iParentMenuId
                 httBuffer:BUFFER-FIELD("iSeq"):BUFFER-VALUE             = iy
                 httBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE       = ix
                 httBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE           = 4
                 httBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE        = STRING(ROWID(JBoxMenu))
                 httBuffer:BUFFER-FIELD("cParentMenuType"):BUFFER-VALUE  = cParentMenuType
                 httBuffer:BUFFER-FIELD("iParentNodeIndex"):BUFFER-VALUE = iParentNodeIdx
                 .
        END.
      END.
    END.
  END.                          
END.
/*
run toexcelviafile.p (httBuffer,0).
*/

DELETE OBJECT hTempTable.
DELETE OBJECT hBuffMenu.
DELETE OBJECT hQueryUserGroupMem NO-ERROR.
DELETE OBJECT hBuffUser NO-ERROR.
DELETE OBJECT hBuffUserMenu NO-ERROR.
DELETE OBJECT hBuffUserGroupMem NO-ERROR.
DELETE OBJECT hBuffCompanyUser NO-ERROR.
DELETE OBJECT hBuffCompanyMenu NO-ERROR.
DELETE OBJECT hBuffMenuTrans NO-ERROR.

/*
PROCEDURE CreateMenu:
  DEF INPUT PARAM iiMenuId          AS INT  NO-UNDO.
  DEF INPUT PARAM iiParentMenuId    AS INT  NO-UNDO.
  DEF INPUT PARAM iiSeq             AS INT  NO-UNDO.
  DEF INPUT PARAM iiLevel           AS INT  NO-UNDO.
  DEF INPUT PARAM icParentMenuType  AS CHAR NO-UNDO.
  DEF INPUT PARAM icParentImage     AS CHAR NO-UNDO.
  DEF INPUT PARAM iiParentNodeIndex AS INT  NO-UNDO.

  IF NOT DbAccess(iiMenuId) THEN RETURN.

  IF bRestUnlessAllowed THEN DO:
    IF NOT (CompanyAccess(iiMenuId) OR UserAccess(iiMenuId)) THEN RETURN.
  END.
  ELSE DO:
    IF NOT CompanyAccess(iiMenuId) THEN RETURN.
    IF NOT UserAccess(iiMenuId) THEN RETURN.
  END.
  
  DEF VAR cImg           AS CHAR NO-UNDO.
  DEF VAR cMnuType       AS CHAR NO-UNDO.
  DEF VAR iParentNodeIdx AS INT  NO-UNDO.
  DEF VAR hBFImage       AS HANDLE NO-UNDO.
  DEF VAR hBFTvNavBar    AS HANDLE NO-UNDO.
  DEF VAR hBFMenuNumber  AS HANDLE NO-UNDO.

  DEF BUFFER bJBoxMenu       FOR JBoxMenu.
  ASSIGN hBFImage      = BUFFER bJBoxMenu:BUFFER-FIELD("cImage")
         hBFTvNavBar   = BUFFER bJBoxMenu:BUFFER-FIELD("iTvNavBarStyle")
         hBFMenuNumber = BUFFER bJBoxMenu:BUFFER-FIELD("cMenuNumber")
         NO-ERROR.

  DEF BUFFER bbJboxMenu      FOR JBoxMenu.
  DEF BUFFER bJboxMenuToMenu FOR JBoxMenuToMenu.

  FIND bJBoxMenu WHERE bJBoxMenu.iJBoxMenuId = iiMenuId NO-LOCK NO-ERROR.
  IF AVAIL bJBoxMenu THEN DO:

    IF bJBoxMenu.cMenuType = "menu-item" AND CAN-DO(cProgramList,bJBoxMenu.cLaunch) 
       OR (bJBoxMenu.cMenuType = "menu"       AND iMenuId = 0)
       OR (bJBoxMenu.cMenuType = "tv-nav-bar" AND iRibbonId = 0)
       OR (bJBoxMenu.cMenuType = "sub-menu"   AND iTabGroupId = 0)
       THEN DO:

      CASE bJBoxMenu.cMenuType:
        WHEN "menu"       THEN iMenuId     = bJBoxMenu.iJBoxMenuId.
        WHEN "tv-nav-bar" THEN iRibbonId   = bJBoxMenu.iJBoxMenuId.
        WHEN "sub-menu"   THEN iTabGroupId = bJBoxMenu.iJBoxMenuId.
      END CASE.

      httBuffer:BUFFER-CREATE().
      httBuffer:BUFFER-COPY(BUFFER bJBoxMenu:HANDLE).
      ASSIGN ix = ix + 1
             httBuffer:BUFFER-FIELD("iParentMenuId"):BUFFER-VALUE    = iiParentMenuId
             httBuffer:BUFFER-FIELD("iSeq"):BUFFER-VALUE             = iiSeq
             httBuffer:BUFFER-FIELD("iNodeIndex"):BUFFER-VALUE       = ix
             httBuffer:BUFFER-FIELD("iLevel"):BUFFER-VALUE           = iiLevel
             httBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE        = STRING(ROWID(bJBoxMenu))
             httBuffer:BUFFER-FIELD("cParentMenuType"):BUFFER-VALUE  = icParentMenuType
             httBuffer:BUFFER-FIELD("iParentNodeIndex"):BUFFER-VALUE = iiParentNodeIndex
             cMnuType                                                = bJBoxMenu.cMenuType
             iParentNodeIdx                                          = ix
             .
      IF VALID-HANDLE(hBFImage) THEN
        cImg = hBFImage:BUFFER-VALUE.
  
      IF VALID-HANDLE(hBFTvNavBar) THEN
        IF bJBoxMenu.cMenuType = "tv-nav-bar" THEN
          iCurrStyle = hBFTvNavBar:BUFFER-VALUE.
        ELSE
          httBuffer:BUFFER-FIELD("iTvNavBarStyle"):BUFFER-VALUE = iCurrStyle.
  
      IF VALID-HANDLE(hBuffMenuTrans) AND cCurrLanguage NE "" THEN DO:
        hBuffMenuTrans:FIND-FIRST("WHERE iJBoxMenuId = " + STRING(bJBoxMenu.iJBoxMenuId) + " AND cLanguage = '" + cCurrLanguage + "'",NO-LOCK) NO-ERROR.
        IF hBuffMenuTrans:AVAIL AND hBuffMenuTrans:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE NE "" THEN
          httBuffer:BUFFER-COPY(hBuffMenuTrans).
      END.
        
      IF bJBoxMenu.cMenuType = "menu-item" AND VALID-HANDLE(hBFMenuNumber) AND hBFMenuNumber:BUFFER-VALUE NE "" THEN
        httBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE = httBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE 
                                                          + (IF bAppendMenuNum THEN " (" + hBFMenuNumber:BUFFER-VALUE + ")" ELSE "").

    END.
  
    FOR EACH bJBoxMenuToMenu NO-LOCK
        WHERE bJBoxMenuToMenu.iToMenuId = bJboxMenu.iJBoxMenuId
       ,FIRST bbJboxMenu NO-LOCK
              WHERE bbJboxMenu.iJBoxMenuId = bJBoxMenuToMenu.iFromMenuId
                AND bbJboxMenu.cMenuType   = "sub-menu":
      httBuffer:BUFFER-FIELD("bHasChildSubMenu"):BUFFER-VALUE = YES.
      LEAVE.
    END.
    FOR EACH bJBoxMenuToMenu NO-LOCK
        WHERE bJBoxMenuToMenu.iToMenuId = bJboxMenu.iJBoxMenuId
       ,FIRST bbJboxMenu NO-LOCK
              WHERE bbJboxMenu.iJBoxMenuId = bJBoxMenuToMenu.iFromMenuId
                AND bbJboxMenu.cMenuType   = "MENU-ITEM":
      httBuffer:BUFFER-FIELD("bHasChildMenuItem"):BUFFER-VALUE = YES.
      LEAVE.
    END.

    FOR EACH bJBoxMenuToMenu NO-LOCK
        WHERE bJBoxMenuToMenu.iToMenuId = bJboxMenu.iJBoxMenuId
           BY bJBoxMenuToMenu.iSeq:
      RUN CreateMenu (bJBoxMenuToMenu.iFromMenuId,bJBoxMenuToMenu.iToMenuId,bJBoxMenuToMenu.iSeq,iiLevel + 1,cMnuType,cImg,iParentNodeIdx).
    END.
  END.
END PROCEDURE.
*/
