/* ttfksource.i Foreign key source table */

DEF {1} SHARED TEMP-TABLE ttSource
  FIELD cDBtable AS CHAR
  FIELD cDBfield AS CHAR
  FIELD cDesc    AS CHAR.

DEF {1} SHARED TEMP-TABLE ttForeignKey
  FIELD cName       AS CHAR 
  FIELD cDBtable    AS CHAR
  FIELD cDBfield    AS CHAR
  FIELD cRelDBtable AS CHAR
  FIELD cRelDBfield AS CHAR
  FIELD cRelation   AS CHAR
  FIELD cViewFields AS CHAR
  FIELD iEntityId   AS INT
  FIELD cDesc       AS CHAR.
