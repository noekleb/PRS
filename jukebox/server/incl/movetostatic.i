FUNCTION MoveToStatic RETURN LOGICAL
        (INPUT ihttTable      AS HANDLE,
         INPUT ihTargetBuffer AS HANDLE):

  /* Move dynamic TT to static */

  DEF VAR httTableBuffer AS HANDLE NO-UNDO.
  DEF VAR httTableQuery  AS HANDLE NO-UNDO.
  
  
  httTableBuffer = ihttTable:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY httTableQuery NO-ERROR.
  httTableQuery:SET-BUFFERS(httTableBuffer) NO-ERROR.
  httTableQuery:QUERY-PREPARE("FOR EACH " + httTableBuffer:NAME).
  httTableQuery:QUERY-OPEN.
  httTableQuery:GET-FIRST().
  REPEAT WHILE NOT httTableQuery:QUERY-OFF-END:
    ihTargetBuffer:BUFFER-CREATE().
    ihTargetBuffer:BUFFER-COPY(httTableBuffer).
    httTableQuery:GET-NEXT().
  END.

END FUNCTION.
