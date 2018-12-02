/*------------------------------------------------------------------------------
  Purpose: Set font-label and indicator for direction 
    Notes: Caller: setSortLabel function in JBoxObjLib.p
           Supports new browse features added from 10.1B 
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse     AS HANDLE NO-UNDO.
DEF INPUT PARAM icSortColumn AS CHAR   NO-UNDO.
DEF INPUT PARAM ibDesc       AS LOG    NO-UNDO.

DEF VAR hColumn        AS HANDLE NO-UNDO.
DEF VAR ix             AS INT NO-UNDO.
DEF VAR iy             AS INT NO-UNDO.
DEF VAR cSortMap       AS CHAR NO-UNDO.
DEF VAR cColumnName    AS CHAR NO-UNDO.

ihBrowse:CLEAR-SORT-ARROWS().
IF NUM-ENTRIES(icSortColumn,"") > 1 THEN DO:
  cSortMap = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"sortmap").
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    hColumn = ihBrowse:GET-BROWSE-COLUMN(ix).
  
    hColumn:LABEL-FONT = IF ihBrowse:PARENT:PARENT:FONT NE ? THEN 
                           ihBrowse:PARENT:PARENT:FONT
                         ELSE ?.
    hColumn:LABEL-BGCOLOR = IF ihBrowse:PARENT:PARENT:BGCOLOR NE ? THEN 
                              ihBrowse:PARENT:PARENT:BGCOLOR
                            ELSE ?.

    cColumnName = hColumn:NAME.
    IF SUBSTR(cColumnName,LENGTH(cColumnName)) = "]" THEN
      cColumnName = "jbextent_" + RIGHT-TRIM(SUBSTR(cColumnName,R-INDEX(cColumnName,"[") + 1),"]") + "_" + SUBSTR(cColumnName,1,R-INDEX(cColumnName,"[") - 1).  
    
    IF cColumnName = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"1stSortColumn") THEN 
      ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"1stSortColumnDesc") = "desc" THEN NO ELSE YES,1).
    ELSE IF cColumnName = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"2ndSortColumn") THEN
      ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"2ndSortColumnDesc") = "desc" THEN NO ELSE YES,2).
    ELSE IF cColumnName = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"3rdSortColumn") THEN
      ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"3rdSortColumnDesc") = "desc" THEN NO ELSE YES,3).
    ELSE IF cColumnName = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"4thSortColumn") THEN
      ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"4thSortColumnDesc") = "desc" THEN NO ELSE YES,4).
    ELSE IF cSortMap NE "" THEN
      DO iy = 1 TO NUM-ENTRIES(cSortMap):
        IF hColumn:NAME = ENTRY(1,ENTRY(iy,cSortMap),";") THEN DO:
          IF ENTRY(2,ENTRY(iy,cSortMap),";") = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"1stSortColumn") THEN
            ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"1stSortColumnDesc") = "desc" THEN NO ELSE YES,1).
          ELSE IF ENTRY(2,ENTRY(iy,cSortMap),";") = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"2ndSortColumn") THEN
            ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"2ndSortColumnDesc") = "desc" THEN NO ELSE YES,2).
          ELSE IF ENTRY(2,ENTRY(iy,cSortMap),";") = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"3rdSortColumn") THEN
            ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"3rdSortColumnDesc") = "desc" THEN NO ELSE YES,3).
          ELSE IF ENTRY(2,ENTRY(iy,cSortMap),";") = DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"4thSortColumn") THEN
            ihBrowse:SET-SORT-ARROW(ix,IF DYNAMIC-FUNCTION("getAttribute" IN SOURCE-PROCEDURE,ihBrowse,"4thSortColumnDesc") = "desc" THEN NO ELSE YES,4).
         LEAVE.
       END.
     END.
  END.
END.

ELSE DO ix = 1 TO ihBrowse:NUM-COLUMNS:
  hColumn = ihBrowse:GET-BROWSE-COLUMN(ix).

/*   hColumn:LABEL-FONT = IF ihBrowse:PARENT:PARENT:FONT NE ? THEN       */
/*                          ihBrowse:PARENT:PARENT:FONT                  */
/*                        ELSE ?.                                        */
/*   hColumn:LABEL-BGCOLOR = IF ihBrowse:PARENT:PARENT:BGCOLOR NE ? THEN */
/*                             ihBrowse:PARENT:PARENT:BGCOLOR            */
/*                           ELSE ?.                                     */

  IF hColumn:NAME = icSortColumn THEN DO:
    ihBrowse:SET-SORT-ARROW(ix,NOT ibDesc).
    DYNAMIC-FUNCTION("setSearchFieldLinkInfo",ihBrowse,icSortColumn).
  END.
END.



