  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  /* Uncomment and modify to fetch pre-selected rows from database:
  cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                "<table>",       /* Buffer(list) for query */
                                "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                "where <field1> begins 'b'").
  */

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "<table>"      
                      + ";<field1>"  
                      + ";<field2>"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "<field1>", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
    MESSAGE PROGRAM-NAME(1) SKIP
            cIdList SKIP
            cRowIdList
            VIEW-AS ALERT-BOX.
