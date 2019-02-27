DEF INPUT PARAM icSessionId AS CHAR NO-UNDO.
DEF INPUT PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT PARAM TABLE-HANDLE httForeignKey.
DEF OUTPUT PARAM ocReturn   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocStatus   AS CHAR NO-UNDO.

DEF VAR hBuffer      AS HANDLE NO-UNDO.

{incl/validatesession.i}

{incl/ttfksource.i NEW}
{incl/movetostatic.i}

hBuffer = BUFFER ttForeignKey:HANDLE.
MoveToStatic(httForeignKey,hBuffer).

DO:
  FOR EACH JBoxQForeignKey EXCLUSIVE-LOCK:
    DELETE JBoxQForeignKey. 
  END.
  FOR EACH ttForeignKey:
    IF ttForeignKey.cRelDBtable NE "" OR ttForeignKey.cViewFields NE "" THEN DO:
      CREATE JBoxQForeignKey.
      BUFFER-COPY ttForeignKey TO JBoxQForeignKey.
    END.
  END.
END.

