/* Start a utility built with JukeBox */               

DEF VAR hUtil      AS HANDLE NO-UNDO.
DEF VAR cSessfile  AS CHAR   NO-UNDO.
DEF VAR cLine      AS CHAR   NO-UNDO.
DEF VAR cSessId    AS CHAR   NO-UNDO.

RUN JBoxLoadLib.p ("ResizeLib.p,JBoxUIlib.p,JBoxASlib.p").
/*                   + (IF PROVERSION BEGINS "1" THEN ",JBoxFUlib.p" ELSE "")). */

cSessfile = SEARCH("incl/custdevmode.i").
IF cSessfile NE ? THEN DO:
  INPUT FROM VALUE(cSessfile).
  REPEAT:
    IMPORT UNFORMATTED cLine.
    IF cLine MATCHES '*setSessionId*' THEN
      cSessId = REPLACE(REPLACE(REPLACE(ENTRY(NUM-ENTRIES(cLine),cLine),'"',""),".",""),")","").
  END.
END.
IF cSessId = "" OR cSessId = "ocSessionId" THEN cSessId = "validsession".

DYNAMIC-FUNCTION("setSessionId",cSessId).
DYNAMIC-FUNCTION("setLanguageCode","EN").
DYNAMIC-FUNCTION("setAppTitle","AppComp").
DYNAMIC-FUNCTION("setBehaviour",
                  "DefaultSortFont|6," +   
                  "DefaultSortColor|15," + 
                  "BrowseSearchDefault|goto," +
                  "TabOnReturn|yes," +       
                  "SetSortLabel|yes"
                  ).      

IF DYNAMIC-FUNCTION("getFieldValues","_file","WHERE _file-name = 'JBoxSysParam'","_file-name") = ? THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"JBoxSysParam table not installed","","").
  RETURN.
END.

ON 'close':U OF THIS-PROCEDURE DO:
  IF VALID-HANDLE(THIS-PROCEDURE) AND THIS-PROCEDURE:PERSISTENT THEN
    DELETE PROCEDURE THIS-PROCEDURE.
END.

RUN JBoxSysParam.w PERSIST SET hUtil.
RUN InitializeObject IN hUtil.
RUN MoveToTop IN hUtil.

SUBSCRIBE TO "InvalidateHandle" IN hUtil.

WAIT-FOR "close" OF THIS-PROCEDURE.

PROCEDURE InvalidateHandle:
  DEF INPUT PARAM ihUtil AS HANDLE NO-UNDO.

  APPLY "close" TO THIS-PROCEDURE.
END PROCEDURE.

