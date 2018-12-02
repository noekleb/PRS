
DEF INPUT PARAM ihBrowse           AS HANDLE NO-UNDO.
DEF INPUT PARAM iBatchSize         AS INT    NO-UNDO.
DEF INPUT PARAM iStartRow          AS INT    NO-UNDO.
DEF INPUT PARAM icBuffersAndFields AS CHAR   NO-UNDO.
DEF INPUT PARAM icQueryCriteria    AS CHAR   NO-UNDO.
DEF INPUT PARAM icSortExpr       AS CHAR   NO-UNDO.
DEF INPUT PARAM ibDesc             AS LOG    NO-UNDO.
DEF INPUT PARAM ihCurrSourceProc   AS HANDLE NO-UNDO.
DEF OUTPUT PARAM iMaxCount         AS INT    NO-UNDO.

DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR rBuffer           AS ROWID  NO-UNDO.
DEF VAR cFindString       AS CHAR   NO-UNDO INIT "WHERE ".
DEF VAR iNumRowIds        AS INT    NO-UNDO.
DEF VAR iTotCount         AS INT    NO-UNDO.
DEF VAR iCount            AS INT    NO-UNDO.
DEF VAR cJoin             AS CHAR   NO-UNDO.
DEF VAR hField            AS HANDLE NO-UNDO.
DEF VAR cSkipUniqueRows   AS CHAR   NO-UNDO.
DEF VAR cSortMap          AS CHAR   NO-UNDO.
DEF VAR cLastRowid        AS CHAR   NO-UNDO.
DEF VAR cFirstRowid       AS CHAR   NO-UNDO.
DEF VAR iNumServerRec     AS INT    NO-UNDO.
DEF VAR iTmpStartRow      AS INT    NO-UNDO.
DEF VAR bWhereFound       AS LOG    NO-UNDO.
DEF VAR iy                AS INT    NO-UNDO.
DEF VAR hTmp              AS HANDLE NO-UNDO.
DEF VAR hWin              AS HANDLE NO-UNDO.
DEF VAR cCheckedRowIds    AS CHAR   NO-UNDO.
DEF VAR cDistinctColumns  AS CHAR   NO-UNDO.
DEF VAR cAccumFields      AS CHAR   NO-UNDO.
DEF VAR cStatValues       AS CHAR   NO-UNDO.
DEF VAR cDistBufFldList   AS CHAR   NO-UNDO.
DEF VAR cAccBufFldList    AS CHAR   NO-UNDO.
DEF VAR cCurrBuffer       AS CHAR   NO-UNDO.
                                    
DEF VAR cTmp                AS CHAR   NO-UNDO.
DEF VAR cTemp               AS CHAR   NO-UNDO.
DEF VAR cTemp2              AS CHAR   NO-UNDO.
DEF VAR cFieldDef           AS CHAR   NO-UNDO.
DEF VAR bCalcField          AS LOG    NO-UNDO.
DEF VAR cMatchQuery         AS CHAR   NO-UNDO.
DEF VAR hTmpQuery           AS HANDLE NO-UNDO.
DEF VAR hUseAppServer       AS HANDLE NO-UNDO.
DEF VAR hAltExtentFields    AS HANDLE NO-UNDO EXTENT 50.
DEF VAR hExtentFields       AS HANDLE NO-UNDO EXTENT 50.
DEF VAR cExtentFields       AS CHAR   NO-UNDO.
DEF VAR cAltExtFieldHandles AS CHAR   NO-UNDO.
DEF VAR iCntExtFields       AS INT    NO-UNDO.
DEF VAR cExcludeLocalFields AS CHAR   NO-UNDO.
DEF VAR cExcludedFields     AS CHAR   NO-UNDO. 

DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR httTable            AS HANDLE NO-UNDO.
DEF VAR httTableBuffer      AS HANDLE NO-UNDO.
DEF VAR httTableQuery       AS HANDLE NO-UNDO.
DEF VAR bBufferSwap         AS LOG    NO-UNDO.
DEF VAR cCompanyTag         AS CHAR   NO-UNDO.
DEF VAR cQueryWhere         AS CHAR   NO-UNDO.
DEF VAR cSortString         AS CHAR   NO-UNDO.
DEF VAR bAccumDistinct      AS LOG    NO-UNDO.
DEF VAR cMyQuerySort        AS CHAR   NO-UNDO.
DEF VAR cMyLocalSort        AS CHAR   NO-UNDO.
DEF VAR cModQuerySort       AS CHAR   NO-UNDO.
DEF VAR cModLocalSort       AS CHAR   NO-UNDO.

{JBoxObjectDef.i}
DEF BUFFER bbttObjectLink FOR ttObjectLink.

{incl/methodlog.i ihBrowse:NAME}

IF icQueryCriteria = "" THEN DO:
  PUBLISH "ControlBufferSwap" (ihBrowse,OUTPUT bBufferSwap).
  IF (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryWhere") = "" 
     AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryFilter") = "" 
     AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanquerywhere") = ""
     AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanqueryfilter") = ""
     AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"uselocaldata") NE "yes" 
     AND NOT CAN-DO(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"allcalcfields"),ENTRY(1,DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querysort")," "))
     ) OR bBufferSwap
     THEN DO:
    IF CAN-DO(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"altPrimaryBufferList"),DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortBuffer")) THEN
      DYNAMIC-FUNCTION("ChangePrimarySearchBuffer",ihBrowse,DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortBuffer"),
                                IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"preScanQuery" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortBuffer")) NE "" THEN
                                  "," + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"preScanQuery" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortBuffer"))
                                ELSE "WHERE true,EACH " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"basetable") + " NO-LOCK OF " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortBuffer")
                                ).
    ELSE DYNAMIC-FUNCTION("ChangePrimarySearchBuffer",ihBrowse,"","").
  END.
  
  ASSIGN icBuffersAndFields = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"buffersandfields")
         icQueryCriteria    = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"basequery") + 
                              DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryfilter") + 
                              DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querywhere") + 
                             (IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"use-index_" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumn")) NE "" THEN
                                " USE-INDEX " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"use-index_" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumn"))
                              ELSE IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"use-index") NE "" THEN
                                " USE-INDEX " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"use-index")
                              ELSE "") +
                              DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryjoin")
         icSortExpr       = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querysort")
         .
END.

ASSIGN cTemp = REPLACE(icBuffersAndFields,"!","")
       cTemp = REPLACE(cTemp,";RowCount","")
       cTemp = REPLACE(cTemp,";RowIdent1","")
       cTemp = REPLACE(cTemp,";RowIdent2","")
       cTemp = REPLACE(cTemp,";RowIdent3","")
       cTemp = REPLACE(cTemp,";RowIdent4","")
       cTemp = REPLACE(cTemp,";RowIdent5","")
       cTemp = REPLACE(cTemp,";RowIdent6","")
       cTemp = REPLACE(cTemp,";RowIdent7","")
       cTemp = REPLACE(cTemp,";RowIdent8","")
       cTemp = REPLACE(cTemp,";RowIdent9","")
       cTemp = REPLACE(cTemp,";distinct ",";")
       cTemp = REPLACE(cTemp,"+distinct ","+")
       cTemp = REPLACE(cTemp,";accum ",";")
       cTemp = REPLACE(cTemp,"+accum ","+")
       icQueryCriteria = DYNAMIC-FUNCTION("FixQuery",icQueryCriteria)
       .

IF icQueryCriteria MATCHES "*(*Company)" OR icQueryCriteria MATCHES "*(Codemaster)" THEN DO:
  ASSIGN cCompanyTag     = SUBSTR(icQueryCriteria,R-INDEX(icQueryCriteria,"("))
         icQueryCriteria = SUBSTR(icQueryCriteria,1,R-INDEX(icQueryCriteria,"(") - 1)
         cQueryWhere     = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryWhere").
  IF cQueryWhere MATCHES "*(*Company)" OR cQueryWhere MATCHES "*(Codemaster)" THEN
    cQueryWhere = SUBSTR(cQueryWhere,1,R-INDEX(cQueryWhere,"(") - 1).
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"queryWhere",cQueryWhere).
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"CompanyTag",cCompanyTag).
END.

hWin = ihBrowse:WINDOW NO-ERROR.
IF VALID-HANDLE(hWin) THEN
  DYNAMIC-FUNCTION("DoLockWindow",ihBrowse:WINDOW).

IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryjoin") = "" THEN DO:
  DO ix = 1 TO LENGTH(icQueryCriteria):
    IF SUBSTR(icQueryCriteria,ix,6) = "first " OR
       SUBSTR(icQueryCriteria,ix,5) = "last " OR
       SUBSTR(icQueryCriteria,ix,5) = "each " 
       THEN DO:
      DO iy = ix TO ix - 10 BY -1:
        IF SUBSTR(icQueryCriteria,iy,1) = "," THEN LEAVE.
      END.
      LEAVE.
    END.
  END.
  IF iy > 0 THEN 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"queryjoin",SUBSTR(icQueryCriteria,iy)).
END.

IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"enableondblclick") = "yes" THEN 
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"enableupdate","no").

cDistinctColumns = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"distinctcolumns").
cAccumFields = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"accumfields").


IF cDistinctColumns NE "" OR cAccumFields NE "" THEN DO:
  DO ix = 1 TO ihBrowse:NUM-COLUMNS:
    IF CAN-DO(cDistinctColumns,ihBrowse:GET-BROWSE-COLUMN(ix):NAME) THEN
      cDistBufFldList = cDistBufFldList + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"fieldbuffer" + ihBrowse:GET-BROWSE-COLUMN(ix):NAME) 
                      + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"orgdbfield" + ihBrowse:GET-BROWSE-COLUMN(ix):NAME) + ",".
    ELSE IF CAN-DO(cAccumFields,ihBrowse:GET-BROWSE-COLUMN(ix):NAME) THEN
      cAccBufFldList = cAccBufFldList + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"fieldbuffer" + ihBrowse:GET-BROWSE-COLUMN(ix):NAME) 
                      + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"orgdbfield" + ihBrowse:GET-BROWSE-COLUMN(ix):NAME) + ",".
  END.
  ASSIGN cDistBufFldList = TRIM(cDistBufFldList,",")
         cAccBufFldList  = TRIM(cAccBufFldList,",").
  DO ix = 1 TO NUM-ENTRIES(cTemp):
    ASSIGN cTemp2 = cTemp2 + (IF cTemp2 NE "" THEN "," ELSE "") + ENTRY(1,ENTRY(ix,cTemp),";") + ";"
           cCurrBuffer = ENTRY(1,ENTRY(ix,cTemp),";"). 
    DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,cTemp),";"):
      ASSIGN cFieldDef = TRIM(ENTRY(1,ENTRY(iy,ENTRY(ix,cTemp),";"),"|"),"+")
             bCalcField = ENTRY(iy,ENTRY(ix,cTemp),";") BEGINS "+".
      IF cDistinctColumns NE "" AND 
         CAN-DO(cDistBufFldList,cCurrBuffer + cFieldDef) AND
         (CAN-DO(cDistinctColumns,cFieldDef) OR
          CAN-DO(cDistinctColumns,cFieldDef + STRING(ix))) THEN
        cTemp2 = cTemp2 + (IF bCalcField THEN "+" ELSE "") + "distinct " + TRIM(ENTRY(iy,ENTRY(ix,cTemp),";"),"+") + ";".
      ELSE IF cAccumFields NE "" AND 
         CAN-DO(cAccBufFldList,cCurrBuffer + cFieldDef) AND
         (CAN-DO(cAccumFields,cFieldDef) OR
          CAN-DO(cAccumFields,cFieldDef + STRING(ix))) THEN
        cTemp2 = cTemp2 + (IF bCalcField THEN "+" ELSE "") + "accum " + TRIM(ENTRY(iy,ENTRY(ix,cTemp),";"),"+") + ";".
      ELSE
        cTemp2 = cTemp2 + ENTRY(iy,ENTRY(ix,cTemp),";") + ";".
    END.
    cTemp2 = TRIM(cTemp2,";").
  END.
  cTemp = cTemp2.
END.
  
DYNAMIC-FUNCTION("setAttribute",ihBrowse,"rowsadded","0").
DYNAMIC-FUNCTION("setAttribute",ihBrowse,"rowsdeleted","0").

bAccumDistinct = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"distinctColumns") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"accumFields") NE "".

IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"uselocaldata") = "yes" AND NOT bAccumDistinct THEN DO:
  IF VALID-HANDLE(ihCurrSourceProc) AND CAN-DO(ihCurrSourceProc:INTERNAL-ENTRIES,"FillLocalBrowse") THEN
    iMaxCount = DYNAMIC-FUNCTION("FillLocalBrowse" IN ihCurrSourceProc,ihBrowse,cTemp,icQueryCriteria,icSortExpr,ibDesc).
  ELSE
    iMaxCount = DYNAMIC-FUNCTION("FillLocalBrowse",ihBrowse,cTemp,icQueryCriteria,icSortExpr,ibDesc).
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"recordcount",STRING(iMaxCount)).
END.
ELSE DO:
  /* If we for some reason have a local copy, purge it now since we are getting a fresh load: */
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"originaltemptable") NE "" THEN DO:
    hTmp = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"originaltemptable")) NO-ERROR.
    IF VALID-HANDLE(hTmp) THEN DO:
      DYNAMIC-FUNCTION("DeleteObject",hTmp).
      DELETE OBJECT hTmp NO-ERROR.
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"originaltemptable","").
    END.
  END.

  cTemp = DYNAMIC-FUNCTION("AddCalcParam",cTemp,ihBrowse).
  
  hBuffer = ihBrowse:QUERY:GET-BUFFER-HANDLE(1).
  iTmpStartRow = iStartRow.
  
  IF iStartRow = 0 AND ihBrowse:QUERY:IS-OPEN THEN 
    hBuffer:EMPTY-TEMP-TABLE().
  ELSE IF iStartRow > 0 THEN DO:
    ihBrowse:SET-REPOSITIONED-ROW(ihBrowse:DOWN,"Conditional").
    iTotCount   = INT(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"totalcount")).
    cFirstRowId = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"firstrowid").
  END.
  
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"keepsearchvalue") = "yes" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanqueryfilter") NE "" AND
     DYNAMIC-FUNCTION("getAttribute",ihBrowse,"basetable") = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortbuffer") 
     THEN DO:
    FIND FIRST bbttObjectLink
         WHERE bbttObjectLink.hFromObject = ihBrowse
           AND bbttObjectLink.cLinkType   = "browse-search-field"
         NO-ERROR.
    IF AVAIL bbttObjectLink AND 
       DYNAMIC-FUNCTION("getBufferFieldDataType",ihBrowse:QUERY:GET-BUFFER-HANDLE(1),bbttObjectLink.cLinkInfo) = "character" AND
       INDEX(bbttObjectLink.hToObject:INPUT-VALUE,"*") > 0 THEN
      cMatchQuery = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortbuffer") + " WHERE " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortbuffer") + "." + bbttObjectLink.cLinkInfo
                  + " MATCHES '" + bbttObjectLink.hToObject:INPUT-VALUE + "'".
  END.
  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanBaseQuery") NE "" OR 
     DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanQueryFilter") NE "" OR 
     DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanQueryWhere") NE "" THEN
    DYNAMIC-FUNCTION("setPreScanQuery",TRIM(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanBaseQuery") + "|" + 
                                            DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanqueryfilter") + "|" + 
                                            DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanquerywhere") + "|" +
                                            cMatchQuery
                                            ,"|")).
  ELSE IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanqueryfilter") NE "" THEN
    DYNAMIC-FUNCTION("setPreScanQuery",cMatchQuery).

  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanlimit") NE "" THEN
    DYNAMIC-FUNCTION("setPreScanLimit",INT(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"prescanlimit"))).
  DYNAMIC-FUNCTION("setCalcFieldProc",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"calcfieldproc")).
  DYNAMIC-FUNCTION("setCalcFieldFilter",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"calcfieldfilter")
                   + (IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"calcfieldfilter") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"calcfieldwhere") NE "" THEN "|" ELSE "")
                   + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"calcfieldwhere")
                   ).
  DYNAMIC-FUNCTION("setNoExistBuffers",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"noexistbuffers")).

  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"useAppserver") NE "" THEN DO:
    hUseAppServer = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",SESSION,DYNAMIC-FUNCTION("getAttribute",ihBrowse,"useAppserver"))) NO-ERROR.
    IF VALID-HANDLE(hUseAppServer) THEN
      DYNAMIC-FUNCTION("setUseAppserver",hUseAppServer).
  END.

  DYNAMIC-FUNCTION("setUniqueBuffer",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"uniqueBuffer")).

  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"NoMandatoryCalcFields") NE "" THEN 
    DYNAMIC-FUNCTION("setSkipCalcFields",ihBrowse).

  IF icSortExpr = "" THEN icSortExpr = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort").
  
  IF DYNAMIC-FUNCTION("getAttributeList",ihBrowse,"myQuerySort*","",NO) NE "|" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(icSortExpr," "):
      cTmp = ENTRY(ix,icSortExpr," ").
      cTmp = ENTRY(NUM-ENTRIES(cTmp,"."),cTmp,".").
    
      IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"myQuerySort" + cTmp) NE "" THEN DO:
        cMyQuerySort = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"myQuerySort" + cTmp).
        cMyQuerySort = REPLACE(cMyQuerySort," <desc> ",IF ibDesc OR (ix < NUM-ENTRIES(icSortExpr," ") AND ENTRY(ix + 1,icSortExpr," ") BEGINS "desc") THEN " desc " ELSE " ").
        IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"myLocalSort" + cTmp) NE "" THEN
          ASSIGN cMyLocalSort = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"myLocalSort" + cTmp)
                 cMyLocalSort = REPLACE(cMyLocalSort," <desc> ",IF ibDesc THEN " desc " ELSE " ").
        ELSE
          DO iy = 1 TO NUM-ENTRIES(cMyQuerySort," "):   
            cTemp2 = ENTRY(iy,cMyQuerySort," ").  
            cMyLocalSort = cMyLocalSort + (IF cMyLocalSort NE "" THEN " " ELSE "") + ENTRY(NUM-ENTRIES(cTemp2,"."),cTemp2,".").
          END.  
        ASSIGN cModQuerySort = cModQuerySort + cMyQuerySort + " "
               cModLocalSort = cModLocalSort + cMyLocalSort + " "
               .    
      END.
      ELSE ASSIGN cModQuerySort = cModQuerySort + ENTRY(ix,icSortExpr," ") + " "
                  cModLocalSort = cModLocalSort + cTmp + " "
                  .    
    END. 
    icSortExpr = cModQuerySort. 
  END.

  httTable = DYNAMIC-FUNCTION("getTempTableJoin",iBatchSize,iStartRow,DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir"),
                              cTemp,
                              icQueryCriteria 
                            + (IF icSortExpr NE "" THEN
                                " BY " + icSortExpr + (IF ibDesc THEN " DESC" ELSE "")
                               ELSE "")
                            + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"CompanyTag")
                              ).
  
  httTableBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  CREATE QUERY httTableQuery NO-ERROR.
  httTableQuery:SET-BUFFERS(httTableBuffer) NO-ERROR.
  httTableQuery:QUERY-PREPARE("FOR EACH " + httTableBuffer:NAME).
  httTableQuery:QUERY-OPEN.
  cJoin = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"queryjoin").
  iNumRowIds = NUM-ENTRIES(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"buffersandfields")).
  
  cSkipUniqueRows = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"SkipUniqueRows").
  DO ix = 1 TO NUM-ENTRIES(cSkipUniqueRows):
    hField = hBuffer:BUFFER-FIELD("RowIdent" + STRING(ix)) NO-ERROR.
    IF NOT VALID-HANDLE(hField) THEN LEAVE.
    ELSE cCheckedRowIds = cCheckedRowIds + ENTRY(ix,cSkipUniqueRows) + ",".
  END.
  cSkipUniqueRows = TRIM(cCheckedRowIds,",").
  
  ASSIGN cExtentFields       = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"extentFields")
         cAltExtFieldHandles = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"AltExtentFieldHandles")
         cExcludeLocalFields = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localcalcfields").
  DO ix = 1 TO NUM-ENTRIES(cExtentFields):
    ASSIGN hAltExtentFields[ix] = WIDGET-HANDLE(ENTRY(ix,cAltExtFieldHandles))
/*            hExtentFields[ix]    = httTableBuffer:BUFFER-FIELD(ENTRY(ix,cExtentFields)) */
           iCntExtFields        = iCntExtFields + 1.
    hExtentFields[ix]    = httTableBuffer:BUFFER-FIELD(ENTRY(ix,cExtentFields)) NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
      cFieldDef = ENTRY(ix,cExtentFields).
      hAltExtentFields[ix] = hBuffer:BUFFER-FIELD(cFieldDef).
      cFieldDef = SUBSTR(cFieldDef,1,R-INDEX(cFieldDef,"_") - 1) + "[" + SUBSTR(cFieldDef,R-INDEX(cFieldDef,"_") + 1) + "]".
      hExtentFields[ix] = httTableBuffer:BUFFER-FIELD(cFieldDef).
    END.
  END.
  DO ix = 1 TO NUM-ENTRIES(cExcludeLocalFields):
    IF ENTRY(ix,cExcludeLocalFields) MATCHES "*]" THEN DO:
      IF NOT CAN-DO(cExcludedFields,SUBSTR(ENTRY(ix,cExcludeLocalFields),1,R-INDEX(ENTRY(ix,cExcludeLocalFields),"[") - 1)) THEN
        cExcludedFields = cExcludedFields + (IF cExcludedFields NE "" THEN "," ELSE "") + SUBSTR(ENTRY(ix,cExcludeLocalFields),1,R-INDEX(ENTRY(ix,cExcludeLocalFields),"[") - 1).
    END.
    ELSE cExcludedFields = cExcludedFields + (IF cExcludedFields NE "" THEN "," ELSE "") + ENTRY(ix,cExcludeLocalFields).
  END.

  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "" THEN
    httTableQuery:GET-FIRST().
  ELSE
    httTableQuery:GET-LAST().
  REPEAT WHILE NOT httTableQuery:QUERY-OFF-END:
    ASSIGN cFindString   = "WHERE "
           iNumServerRec = iNumServerRec + 1
           .
    IF cSkipUniqueRows NE "" THEN
      DO ix = 1 TO NUM-ENTRIES(cSkipUniqueRows):
        cFindString = cFindString + "RowIdent" + ENTRY(ix,cSkipUniqueRows) + " = '" + httTableBuffer:BUFFER-FIELD("RowIdent" + ENTRY(ix,cSkipUniqueRows)):BUFFER-VALUE + "' AND ".
      END.
    ELSE
      DO ix = 1 TO iNumRowIds:
        cFindString = cFindString + "RowIdent" + STRING(ix) + " = '" + httTableBuffer:BUFFER-FIELD("RowIdent" + STRING(ix)):BUFFER-VALUE + "' AND ".
      END.
    cFindString = RIGHT-TRIM(cFindString," AND ").
    bOk = hBuffer:FIND-UNIQUE(cFindString) NO-ERROR.
    IF NOT bOk THEN DO:
      hBuffer:BUFFER-CREATE().
      hBuffer:BUFFER-COPY(httTableBuffer).
      ASSIGN iTotCount   = iTotCount + 1
             iCount      = iCount    + 1
             cFirstRowid = IF cFirstRowid = "" THEN STRING(hBuffer:ROWID) ELSE cFirstRowid
             .
    END.
    ELSE hBuffer:BUFFER-COPY(httTableBuffer,cExcludedFields).  

    DO ix = 1 TO iCntExtFields:
      hAltExtentFields[ix]:BUFFER-VALUE = hExtentFields[ix]:BUFFER-VALUE.
    END.
  
    cLastRowid  = STRING(hBuffer:ROWID).
  
    IF iStartRow > 0 THEN DO:
      ihBrowse:SET-REPOSITIONED-ROW(ihBrowse:DOWN,"Conditional").
      rBuffer = hBuffer:ROWID.
      iStartRow = 0.
    END.
    iMaxCount = IF httTableBuffer:BUFFER-FIELD("RowCount"):BUFFER-VALUE > iMaxCount THEN httTableBuffer:BUFFER-FIELD("RowCount"):BUFFER-VALUE ELSE iMaxCount.
  
    IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "" THEN
      httTableQuery:GET-NEXT().
    ELSE 
      httTableQuery:GET-PREV().
  END.
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"totalcount",STRING(iTotCount)).
  
  IF iCount > 0 THEN DO:
  
    /* If we go from the start or we go from the end and retrieve all records, set firstrowid to cFirstRowid: */ 
    IF (iNumServerRec < INT(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"rowstobatch")) AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "") 
       OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "" AND iTmpStartRow = 0) THEN
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"firstrowid",cFirstRowid).
    ELSE IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "" THEN
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"firstrowid","").
  
    /* If we go from the end or we go from the start and retrieve all records, set lastrowid to cLastRowid: */ 
    IF iNumServerRec < INT(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"rowstobatch")) 
        OR DYNAMIC-FUNCTION("getAttribute",ihBrowse,"rowstobatch") = "0"
        OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "" AND iTmpStartRow = 0) THEN
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"lastrowid",cLastRowid).
    ELSE
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"lastrowid","").

    IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" THEN icSortExpr = DYNAMIC-FUNCTION("FixSortString",ihBrowse).
    
    IF cModLocalSort NE "" THEN 
      icSortExpr = cModLocalSort.
    
    ihBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBuffer:NAME  
                               + (IF icSortExpr NE ""
                                    OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"lastrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "")
                                    OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"firstrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "")
                                    THEN
                                   " BY " + icSortExpr
                                   + (IF ibDesc THEN " DESC" ELSE "")
                                  ELSE "")
                               + " BY RowCount"
                                 ) NO-ERROR.

    IF ERROR-STATUS:GET-NUMBER(1) = 7328 THEN
      ihBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " BY RowCount").

    hTmpQuery = ihBrowse:QUERY NO-ERROR.

    IF VALID-HANDLE(hTmpQuery) THEN
      bOk = hTmpQuery:QUERY-OPEN() NO-ERROR.
    ELSE DO: 
      IF DYNAMIC-FUNCTION("getASlibBeaviour","QueryLogFile") NE "" THEN DO:
        MESSAGE PROGRAM-NAME(1) SKIP
                "Invalid query handle: " hTmpQuery SKIP
                VIEW-AS ALERT-BOX ERROR.
        iMaxCount = 0.
        RETURN.
      END.
    END.

    IF NOT bOk AND ERROR-STATUS:GET-NUMBER(1) = 141 THEN DO:
      IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stDbSortColumn") NE "" THEN DO:
        IF DYNAMIC-FUNCTION("getFieldDataType",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stDbSortColumn")) = "CHARACTER" THEN
          cSortString = " BY SUBSTR(" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumn") + ",1,30)".
        ELSE
          cSortString = " BY " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumn").
        cSortString = cSortString + " " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"1stSortColumndDesc").
      END.
      IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndDbSortColumn") NE "" THEN DO:
        IF DYNAMIC-FUNCTION("getFieldDataType",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndDbSortColumn")) = "CHARACTER" THEN
          cSortString = cSortString + " BY SUBSTR(" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndSortColumn") + ",1,30)".
        ELSE
          cSortString = cSortString + " BY " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndSortColumn").
        cSortString = cSortString + " " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"2ndSortColumndDesc").
      END.
      IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdDbSortColumn") NE "" THEN DO:
        IF DYNAMIC-FUNCTION("getFieldDataType",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdDbSortColumn")) = "CHARACTER" THEN
          cSortString = cSortString + " BY SUBSTR(" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdSortColumn") + ",1,30)".
        ELSE
          cSortString = cSortString + " BY " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdSortColumn").
        cSortString = cSortString + " " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"3rdSortColumndDesc").
      END.
      IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thDbSortColumn") NE "" THEN DO:
        IF DYNAMIC-FUNCTION("getFieldDataType",DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thDbSortColumn")) = "CHARACTER" THEN
          cSortString = cSortString + " BY SUBSTR(" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thSortColumn") + ",1,30)".
        ELSE
          cSortString = cSortString + " BY " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thSortColumn").
        cSortString = cSortString + " " + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"4thSortColumndDesc").
      END.
      IF cSortString = "" THEN DO:
        ASSIGN cSortString  = "SUBSTR("
               icSortExpr = " BY " + icSortExpr.
        DO ix = 1 TO LENGTH(icSortExpr):
          cSortString = cSortString + IF SUBSTR(icSortExpr,ix,4) = " BY " THEN " BY SUBSTR("
                                      ELSE IF SUBSTR(icSortExpr,ix,1) = " " AND SUBSTR(icSortExpr,ix - 3,4) NE " BY " THEN ",1,50) "
                                      ELSE IF SUBSTR(icSortExpr,ix - 1,4) NE " BY " AND SUBSTR(icSortExpr,ix - 2,4) NE " BY " THEN SUBSTR(icSortExpr,ix,1)
                                      ELSE "".
        END.
        IF R-INDEX(icSortExpr,"DESC") = 0 OR R-INDEX(cSortString,"(") > R-INDEX(cSortString,")") THEN
          cSortString = cSortString + ",1,50) ".
      END.

      ihBrowse:QUERY:QUERY-PREPARE("FOR EACH " + hBuffer:NAME 
                                 + (IF icSortExpr NE ""
                                      OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"lastrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "")
                                      OR (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"firstrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "")
                                      THEN
                                    cSortString 
/*                                      " BY " + (IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" THEN "SUBSTR(" + DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") + ",1,50)" ELSE icSortExpr) */
/*                                      + (IF ibDesc THEN " DESC" ELSE "")                                                                                                                                            */
                                    ELSE "")
                                 + " BY RowCount"
                                   ).
      bOK = ihBrowse:QUERY:QUERY-OPEN() NO-ERROR.
    END.
    
    IF bSetSortLabel THEN DO:
      cSortMap = DYNAMIC-FUNCTION("getAttribute",ihBrowse,"sortmap").
      DO ix = 1 TO NUM-ENTRIES(cSortMap):
        IF ENTRY(2,ENTRY(ix,cSortMap),";") = icSortExpr THEN 
          icSortExpr = ENTRY(1,ENTRY(ix,cSortMap),";").
      END.
      IF icSortExpr = "" AND NOT (
           (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"lastrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") = "") OR
           (DYNAMIC-FUNCTION("getAttribute",ihBrowse,"firstrowid") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" AND DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "")) THEN
        DYNAMIC-FUNCTION("setAttribute",ihBrowse,"localsort","").
      DYNAMIC-FUNCTION("setSortLabel",ihBrowse,(IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") NE "" THEN DYNAMIC-FUNCTION("getAttribute",ihBrowse,"localsort") ELSE icSortExpr),ibDesc).
    END.
  
    IF rBuffer NE ? THEN
      ihBrowse:QUERY:REPOSITION-TO-ROWID(rBuffer).
    ELSE IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"querydir") NE "" THEN DO: 
      ihBrowse:QUERY:GET-LAST().
      IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
        DYNAMIC-FUNCTION("setAttribute",ihBrowse,"lastrowid",STRING(ihBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID)).
    END.
    ELSE DO:
      ihBrowse:QUERY:GET-FIRST().
      IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
        DYNAMIC-FUNCTION("setAttribute",ihBrowse,"firstrowid",STRING(ihBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID)).
    END.
  END.
  
  DELETE OBJECT httTableQuery.
  DELETE OBJECT httTable.
  
  IF VALID-HANDLE(hWin) THEN
    DYNAMIC-FUNCTION("DoLockWindow",?).

  IF DYNAMIC-FUNCTION("getQueryStatFieldValues") NE "" THEN 
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"recordcount",ENTRY(1,ENTRY(2,DYNAMIC-FUNCTION("getQueryStatFieldValues"),"|"),";")).
  ELSE DO:
    IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"getrecordcount") = "" THEN DYNAMIC-FUNCTION("setAttribute",ihBrowse,"recordcount","").
    IF NOT PROGRAM-NAME(2) BEGINS "StartSearch" AND NOT PROGRAM-NAME(2) BEGINS("MultiSortBrowse") THEN 
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"currentcount",STRING((IF cSkipUniqueRows NE "" THEN iTotCount ELSE iMaxCount))).
  END.

  IF INDEX(DYNAMIC-FUNCTION("getQueryStatFieldValues"),";") > 0 THEN DO:
    cStatValues = DYNAMIC-FUNCTION("getQueryStatFieldValues").
    DO ix = 2 TO NUM-ENTRIES(cStatValues,";"):
      DYNAMIC-FUNCTION("setAttribute",ihBrowse,"statvalue" + ENTRY(1,ENTRY(ix,cStatValues,";"),"|"),ENTRY(2,ENTRY(ix,cStatValues,";"),"|")).
    END.
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"QueryStatFieldValues",SUBSTR(DYNAMIC-FUNCTION("getQueryStatFieldValues"),INDEX(DYNAMIC-FUNCTION("getQueryStatFieldValues"),";") + 1)).
  END.

  IF DYNAMIC-FUNCTION("getAttribute",ihBrowse,"GoLocalWhenSmallDistinctCount") = "yes" AND 
     INT(DYNAMIC-FUNCTION("getAttribute",ihBrowse,"recordcount")) < 3000 AND
     DYNAMIC-FUNCTION("getAttribute",ihBrowse,"distinctcolumns") NE "" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"uselocaldata","yes").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"firstrowid","").
    DYNAMIC-FUNCTION("setAttribute",ihBrowse,"lastrowid","").
  END.
END.

IF VALID-HANDLE(ihCurrSourceProc) AND CAN-DO(ihCurrSourceProc:INTERNAL-ENTRIES,"ViewRecordCount") THEN
  DYNAMIC-FUNCTION("ViewRecordCount" IN ihCurrSourceProc,ihBrowse).
ELSE
  DYNAMIC-FUNCTION("ViewRecordCount",ihBrowse).

IF VALID-HANDLE(ihCurrSourceProc) AND CAN-DO(ihCurrSourceProc:INTERNAL-ENTRIES,"ViewQueryStat") THEN
  DYNAMIC-FUNCTION("ViewQueryStat" IN ihCurrSourceProc,ihBrowse).


