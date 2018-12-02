/* Search JBoxDocMeta for keyword 
   and optional filename, create and mod.date (for file)
   
   Created: 27.11.14  By BHa
---------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cKeyword    AS CHAR NO-UNDO.
DEF VAR iMaxCount   AS INT  NO-UNDO INIT 1000000.
DEF VAR cFileName   AS CHAR NO-UNDO.
DEF VAR dtModified  AS DATETIME NO-UNDO.
DEF VAR dtCreated   AS DATETIME NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("startASlib" IN SOURCE-PROCEDURE).

cKeyword = ENTRY(1,icParam,CHR(1)).
IF NUM-ENTRIES(icParam,CHR(1)) > 1 THEN DO:
  icParam = ENTRY(2,icParam,CHR(1)).
  iMaxCount = INT(ENTRY(1,icParam,"|")).
  IF NUM-ENTRIES(icParam,"|") > 1 THEN
    cFileName = ENTRY(2,icParam,"|").
  IF NUM-ENTRIES(icParam,"|") > 2 THEN
    dtModified = DATETIME(ENTRY(3,icParam,"|")).
  IF NUM-ENTRIES(icParam,"|") > 3 THEN
    dtCreated = DATETIME(ENTRY(4,icParam,"|")).
END.

DO ON ERROR UNDO,THROW:
  IF cKeyword NE "" THEN DO:
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(BUFFER JBoxDocMeta:HANDLE).
    hQuery:QUERY-PREPARE("FOR EACH JBoxDocMeta NO-LOCK "
                       + "WHERE JBoxDocMeta.cKeyword CONTAINS '" + cKeyword + "' "
                       + "AND JBoxDocMeta.iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      ihBuffer:BUFFER-CREATE().
      ihBuffer:BUFFER-COPY(BUFFER JBoxDocMeta:HANDLE).
      hQuery:GET-NEXT().
      ix = ix + 1.
      IF ix > iMaxCount THEN LEAVE.
    END.
  END.

  IF ix > iMaxCount THEN 
    ocReturn = IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                 "OBS! Resultat ikke komplett - søket ga flere enn 1000 treff"
               ELSE
                 "OBS! Result not complete - more that 1000 documents matches criteria".
  
  obOK = ocReturn = "".
END.

CATCH eSysError AS Progress.Lang.SysError:    
  ASSIGN ocReturn = IF eSysError:GetMessageNum(1) = 4686 AND DYNAMIC-FUNCTION("Scandinavian") THEN
                      "* kan bare brukes i slutten av et ord (hvis søk på flere ord skilles de med mellomrom)."
                    ELSE eSysError:GetMessage(1)
         obOk = NO.
END CATCH.

CATCH eProError AS Progress.Lang.ProError:    
  ASSIGN ocReturn = eProError:GetMessage(1)
         obOk = NO.
END CATCH.

FINALLY:
  DELETE OBJECT hQuery NO-ERROR.
END.
