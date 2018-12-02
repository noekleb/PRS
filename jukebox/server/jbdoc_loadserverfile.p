/* Load ascii-file(s) produced on the server into a temp-table or the database.
   If icContext <> "" the file(s) are loaded into the database.
   If icContext = "" the file(s) are NOT loaded into the database.
   
   Created: 13.06.06 By Brynjar Hasle   
---------------------------------------------------------------------------*/
DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
DEF INPUT  PARAM icFileNames     AS CHAR NO-UNDO.
DEF INPUT  PARAM icContext       AS CHAR NO-UNDO. 
DEF INPUT  PARAM icEntityId      AS CHAR NO-UNDO.
DEF INPUT  PARAM icDescription   AS CHAR NO-UNDO.
DEF INPUT  PARAM ibCompressFiles AS LOG  NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE    httDoc.
DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK            AS LOG NO-UNDO INIT TRUE.

DEF VAR cTemp             AS CHAR NO-UNDO.
DEF VAR crBilde           AS CHAR NO-UNDO.
DEF VAR cFileName         AS CHAR NO-UNDO.
DEF VAR bCompress         AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR cNoCompressTypes  AS CHAR NO-UNDO.
DEF VAR cGZIP             AS CHAR NO-UNDO INIT ?.

DEF TEMP-TABLE ttDoc NO-UNDO
    FIELD cFileName       AS CHAR
    FIELD cFullPathName   AS CHAR
    FIELD cFileType       AS CHAR
    FIELD cDescription    AS CHAR
    FIELD iDocSize        AS INT
    FIELD blDocument      AS BLOB
    FIELD dFileCreateDate AS DATE
    FIELD iFileCreateTime AS INT
    FIELD dFileModDate    AS DATE
    FIELD iFileModTime    AS INT
    FIELD cCreatedBy      AS CHAR
    FIELD dCreated        AS DATE
    FIELD cContext        AS CHAR
    FIELD cEntityId       AS CHAR
    .
DEF VAR cDocLoadParam  AS CHAR NO-UNDO.

httDoc = BUFFER ttDoc:HANDLE:TABLE-HANDLE.

DEF STREAM strmDoc.

icFileNames = TRIM(icFileNames,";").
DO ix = 1 TO NUM-ENTRIES(icFileNames,";"):
  FILE-INFO:FILE-NAME = ENTRY(ix,icFileNames,";").

  CREATE ttDoc.
  ASSIGN ttDoc.cFileName       = SUBSTR(ENTRY(ix,icFileNames,";"),R-INDEX(ENTRY(ix,icFileNames,";"),"\") + 1)
         ttDoc.cFullPathName   = FILE-INFO:FULL-PATHNAME
         ttDoc.cFileType       = SUBSTR(ENTRY(ix,icFileNames,";"),R-INDEX(ENTRY(ix,icFileNames,";"),".") + 1)
         ttDoc.cDescription    = icDescription
         ttDoc.iDocSize        = FILE-INFO:FILE-SIZE
         ttDoc.dFileCreateDate = FILE-INFO:FILE-CREATE-DATE
         ttDoc.iFileCreateTime = FILE-INFO:FILE-CREATE-TIME
         ttDoc.dFileModDate    = FILE-INFO:FILE-MOD-DATE
         ttDoc.iFileModTime    = FILE-INFO:FILE-MOD-TIME
         ttDoc.cCreatedBy      = DYNAMIC-FUNCTION("getASuserId")
         ttDoc.dCreated        = TODAY
                               
         ttDoc.cContext        = icContext
         ttDoc.cEntityId       = icEntityId
         bCompress             = FALSE.
         .

  IF ibCompressFiles AND cGZIP NE ? AND NOT CAN-DO(cNoCompressTypes,SUBSTR(ENTRY(ix,icFileNames,";"),R-INDEX(ENTRY(ix,icFileNames,";"),".") + 1)) THEN DO:
    cFileName = SESSION:TEMP-DIR + ttDoc.cFileName.
    OS-COPY VALUE(FILE-INFO:FULL-PATHNAME) VALUE(cFileName).
    OS-COMMAND SILENT VALUE(SEARCH("gzip.exe") + ' "' + cFileName + '"').
    cFileName = cFileName + ".gz".
    IF SEARCH(cFileName) NE ? THEN DO:
      FILE-INFO:FILE-NAME = cFileName.
      ASSIGN bCompress = TRUE
             ttDoc.cFileName = ttDoc.cFileName + ".gz"
             ttDoc.iDocSize  = FILE-INFO:FILE-SIZE
             .
    END.
    ELSE cFileName = FILE-INFO:FULL-PATHNAME.
  END.
  ELSE cFileName = FILE-INFO:FULL-PATHNAME.

  COPY-LOB FROM FILE cFileName TO OBJECT ttDoc.blDocument.
  IF bCompress THEN
    OS-DELETE VALUE(cFileName).
END.

RUN jbdoc_savedoc.p
   (icSessionId,
    TABLE-HANDLE httDoc,
    cDocLoadParam,
    OUTPUT obOk,
    OUTPUT ocReturn)
    .


