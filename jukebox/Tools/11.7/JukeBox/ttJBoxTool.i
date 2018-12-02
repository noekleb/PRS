DEF {1} {2} TEMP-TABLE ttJBoxTool
    FIELD iSeq          AS INT  LABEL "Seq"         FORMAT "->>>>>9"
    FIELD cTool         AS CHAR LABEL "Tool"        FORMAT "x(20)"
    FIELD bSelect       AS LOG  LABEL "Select"      
    FIELD bDeleted      AS LOG  LABEL "Deleted"
    FIELD cViewTool     AS CHAR LABEL "Tool"        FORMAT "x(256)"
    FIELD cLabel        AS CHAR LABEL "Label"       FORMAT "x(30)"
    FIELD cImage        AS CHAR LABEL "Image"       FORMAT "x(30)"
    FIELD cViewAs       AS CHAR LABEL "View as"
    FIELD bToggle       AS LOG  LABEL "Toggle menu" 
    FIELD cType         AS CHAR
    FIELD cParentMenu   AS CHAR LABEL "Parent menu"
    FIELD bAlwaysEnable AS LOG  LABEL "Always enabled"
    FIELD cGroups       AS CHAR 
    FIELD cToolTip      AS CHAR LABEL "Tooltip"     FORMAT "x(100)"
    FIELD cDesc         AS CHAR LABEL "Description" FORMAT "x(30)"
    FIELD bExist        AS LOG  LABEL "Exist"
    FIELD bBuiltIn      AS LOG  LABEL "Built-in method"
    FIELD bCreProc      AS LOG
    FIELD cCode         AS CHAR
    FIELD cTip          AS CHAR
    FIELD iLevel        AS INT  LABEL "Level"
    FIELD U_recid       AS RECID
    FIELD fCol          AS DEC
    FIELD cMethod       AS CHAR
    FIELD cAccel        AS CHAR
    FIELD fWidth        AS DEC
    FIELD bMenuOnly     AS LOG  LABEL "Menu only"   
/*     FIELD cMenu         AS CHAR */
/*     FIELD cSubMenu      AS CHAR */
    FIELD hMenuFillIn   AS HANDLE
    FIELD hMenuToggle   AS HANDLE
    FIELD hMenuFilter   AS HANDLE
    FIELD cPlusMinus    AS CHAR 
    INDEX idxTool AS UNIQUE cTool cType bDeleted
    .
DEF BUFFER bttJBoxTool FOR ttJBoxTool.

DEF VAR httJBoxTool AS HANDLE NO-UNDO.
httJBoxTool = BUFFER ttJBoxTool:HANDLE.
