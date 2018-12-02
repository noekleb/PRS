/* Purpose: Fill a complete cache table with all _field info + table and dbname 
   Created 20.may.2016 by brynjar@chemistry.no
*/
DEF INPUT  PARAM icDbName    AS CHAR NO-UNDO.

DEF VAR ix           AS INT NO-UNDO.

{jukebox/tt_field.i}


IF icDbName NE "" THEN DO:
    
  DEFINE QUERY qDb FOR DICTDB._db   FIELDS(), 
                       DICTDB._file FIELDS(_File-Name), 
                       DICTDB._field. 

  OPEN QUERY qDB FOR 
      EACH DICTDB._db  
           NO-LOCK,
      EACH DICTDB._file OF DICTDB._db 
           WHERE LOOKUP(DICTDB._FILE._OWNER,"PUB,_FOREIGN":U) > 0 AND DICTDB._File._Tbl-Type = "T"
           NO-LOCK,
      EACH DICTDB._field OF DICTDB._file 
           NO-LOCK.
  GET FIRST qDB.
  REPEAT WHILE NOT QUERY qDB:QUERY-OFF-END:
    CREATE tt_field.
    BUFFER-COPY DICTDB._field TO tt_field
           ASSIGN tt_field.cDbName = icDbName
                  tt_field.cTableName = DICTDB._file._File-Name
           NO-ERROR.
    GET NEXT qDB.
  END.
END.   

ELSE DO ix = 1 TO NUM-DBS:
  CREATE ALIAS "DICTDB":u FOR DATABASE VALUE(SDBNAME(ix)).
  RUN "JukeBox/fillTt_field.p" (LDBNAME(ix)).
END.


