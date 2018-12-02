DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF INPUT  PARAM icDbName     AS CHAR NO-UNDO.
DEF INPUT  PARAM icTableName AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocFields     AS CHAR NO-UNDO.

DEF NEW SHARED VAR cFields AS CHAR NO-UNDO.
DEF VAR ocReturn   AS CHAR   NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR iy         AS INT    NO-UNDO.

{incl/validatesession.i}

IF NUM-ENTRIES(icTableName,".") = 2 THEN
  ASSIGN icDbName     = ENTRY(1,icTableName,".")
         icTableName = ENTRY(2,icTableName,".")
         .

REPEAT ix = 1 TO NUM-DBS:
  IF icDbName = "" OR icDbName = LDBNAME(ix) THEN DO:
    DELETE ALIAS dictdb.
    CREATE ALIAS dictdb FOR DATABASE VALUE (LDBNAME(ix)).
    RUN jbserv_getfieldsfortable_db.p (LDBNAME(ix),icTableName).
    iy = iy + 1.
  END.  
END.
IF iy > 1 THEN DO:
  DELETE ALIAS dictdb.
  CREATE ALIAS dictdb FOR DATABASE VALUE (LDBNAME(1)).
END.

ocFields = cFields.