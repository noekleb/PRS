/* Load one or many libraries */               

DEF INPUT PARAM icLibList AS CHAR NO-UNDO.

DEF VAR hLib         AS HANDLE NO-UNDO EXTENT 20.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR cPropath     AS CHAR   NO-UNDO.
DEF VAR cJukeBoxRoot AS CHAR   NO-UNDO.
                   
FUNCTION CheckForLib RETURNS LOGICAL
  (INPUT ihProc     AS HANDLE,
   INPUT icLibName  AS CHAR):

  IF NOT VALID-HANDLE(ihProc) THEN
    RETURN NO.
  ELSE REPEAT WHILE VALID-HANDLE(ihProc):
    IF ihProc:FILE-NAME = icLibName THEN RETURN YES.
    ELSE ihProc = ihProc:NEXT-SIBLING.
  END.
  RETURN NO.
END FUNCTION.

IF SEARCH("JBoxUIlib.p") = ? AND SEARCH("JBoxUIlib.r") = ? THEN DO:
  IF SEARCH("lib/JBoxUIlib.p") NE ? THEN 
    cJukeBoxRoot = SUBSTR(SEARCH("lib/JBoxUIlib.p"),1,R-INDEX(SEARCH("lib/JBoxUIlib.p"),chr(92)) - 4).
  ELSE IF SEARCH("lib/JBoxUIlib.r") NE ? THEN
    cJukeBoxRoot = SUBSTR(SEARCH("lib/JBoxUIlib.r"),1,R-INDEX(SEARCH("lib/JBoxUIlib.r"),chr(92)) - 4).
  ELSE IF SEARCH("jukebox/JBoxLoadLib.p") NE ? THEN
    cJukeBoxRoot = SUBSTR(SEARCH("jukebox/JBoxLoadLib.p"),1,R-INDEX(SEARCH("jukebox/JBoxLoadLib.p"),chr(92))).
  ELSE IF SEARCH("JBoxLoadLib.r") NE ? THEN
    cJukeBoxRoot = SUBSTR(SEARCH("jukebox/JBoxLoadLib.r"),1,R-INDEX(SEARCH("jukebox/JBoxLoadLib.r"),chr(92))).
  ELSE DO:
    MESSAGE "Jukebox libraries not loaded" VIEW-AS ALERT-BOX WARNING.
    RETURN.
  END.
  cPropath = SUBSTR(PROPATH,1,INDEX(PROPATH,"jukebox") + 7) + 
             cJukeBoxRoot + "lib," +
             cJukeBoxRoot + "oo," +
             cJukeBoxRoot + "obj," +
             cJukeBoxRoot + "img," +
             cJukeBoxRoot + "server," +
             cJukeBoxRoot + "winsrc," +
             cJukeBoxRoot + "winsrc/admin," +
             cJukeBoxRoot + "tools," +
             SUBSTR(PROPATH,INDEX(PROPATH,"jukebox") + 8).
  PROPATH = cPropath.
END.

DO ix = 1 TO NUM-ENTRIES(icLibList):
  IF NOT CheckForLib(SESSION:FIRST-PROCEDURE,ENTRY(ix,icLibList)) THEN DO:
/*     IF ENTRY(ix,icLibList) = "JBoxASlib.p" THEN */
/*       {incl/ttDataDict.i NEW}                   */
    RUN VALUE(ENTRY(ix,icLibList)) PERSIST SET hLib[ix].
    SESSION:ADD-SUPER-PROC(hLib[ix]).
  END.
END.

