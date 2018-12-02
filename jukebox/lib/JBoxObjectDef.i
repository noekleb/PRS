DEF {1} SHARED VAR iDefaultSortFont          AS INT  NO-UNDO.
DEF {1} SHARED VAR iDefaultSortColor         AS INT  NO-UNDO.
DEF {1} SHARED VAR cBrowseSearchDefault      AS CHAR NO-UNDO INIT "goto".
DEF {1} SHARED VAR bTabOnReturn              AS LOG  NO-UNDO.
DEF {1} SHARED VAR bSetSortLabel             AS LOG  NO-UNDO.
DEF {1} SHARED VAR cDefActionList            AS CHAR NO-UNDO.
DEF {1} SHARED VAR cDefImageList             AS CHAR NO-UNDO.
DEF {1} SHARED VAR cPassiveFilterButton      AS CHAR NO-UNDO.
DEF {1} SHARED VAR cActiveFilterButton       AS CHAR NO-UNDO.
DEF {1} SHARED VAR cCtrlHotkeyActions        AS CHAR NO-UNDO.
DEF {1} SHARED VAR cCtrlHotkeys              AS CHAR NO-UNDO.
DEF {1} SHARED VAR cAltHotkeyActions         AS CHAR NO-UNDO.
DEF {1} SHARED VAR cAltHotkeys               AS CHAR NO-UNDO.
DEF {1} SHARED VAR bKeepExcel                AS LOG  NO-UNDO INIT TRUE. /* Keep excel running ant try to hook up next report to same instance */
DEF {1} SHARED VAR hCurrSourceProc           AS HANDLE NO-UNDO.
DEF {1} SHARED VAR hCurrWidget               AS HANDLE NO-UNDO.
DEF {1} SHARED VAR hCurrWindow               AS HANDLE NO-UNDO.
DEF {1} SHARED VAR hTmpObject                AS HANDLE NO-UNDO.
DEF {1} SHARED VAR cGlobSecDisabledActions   AS CHAR   NO-UNDO.
DEF {1} SHARED VAR cMarkAsc                  AS CHAR   NO-UNDO INIT " ^". 
DEF {1} SHARED VAR cMarkDesc                 AS CHAR   NO-UNDO INIT " v". 

DEF {1} SHARED TEMP-TABLE ttObject          /* Toolbar rectangle, browse, menu, buffer.. */
    FIELD hWindow                 AS HANDLE
    FIELD hObject                 AS HANDLE
    FIELD cObjectType             AS CHAR     /* browse, toolbar, menu.. */
    FIELD cState                  AS CHAR     /* Assigned by EventAction. F.ex "new" for toolbar, <sortcolumn>,<desc> for browse */
    FIELD hDesignObject           AS HANDLE
    FIELD hSourceProc             AS HANDLE 
    FIELD cObjectName             AS CHAR     /* Name taken from the name of the design object (most often) */
    FIELD cInitProc               AS CHAR
    FIELD cGenProc                AS CHAR
    INDEX idxObject   IS UNIQUE PRIMARY hObject 
    INDEX idxWindow   hWindow
    .

DEF {1} SHARED TEMP-TABLE ttObjectLink
    FIELD hFromObject             AS HANDLE
    FIELD hToObject               AS HANDLE
    FIELD cLinkType               AS CHAR
    FIELD cLinkInfo               AS CHAR
    FIELD cInitProc               AS CHAR
    FIELD iSeq                    AS INT
    INDEX idxFrom    hFromObject
    INDEX idxTo      hToObject
    .

DEF {1} SHARED TEMP-TABLE ttAttribute     
    FIELD hObject                 AS HANDLE
    FIELD cName                   AS CHAR    /* Display, Update, Input, SortColumn, Desc ... */
    FIELD cValue                  AS CHAR
    INDEX idxObject  hObject cName
    INDEX idxName    cName
    .

DEF {1} SHARED TEMP-TABLE ttEvent           /* Button choose, menu item choose, start-search, etc */
    FIELD hWindow                 AS HANDLE
    FIELD hWidget                 AS HANDLE   /* handle to event widget, ie button, menu-item, browse.. */
    FIELD hObject                 AS HANDLE   /* Handle to event object, ie toolbar, browse, menu.. */
    FIELD cName                   AS CHAR     /* "Choose", "start-search", etc */
    FIELD cAction                 AS CHAR     /* Type of action. F.ex "new" for a toolbar event will cause delete, new and copy buttons to be disabled */
    FIELD cMethod                 AS CHAR     /* Name for procedure to excecute */
    FIELD cWidgetType             AS CHAR
    FIELD bReturnNoApply          AS LOG
    FIELD cLabel                  AS CHAR
    INDEX idxObject  hObject
    . 
