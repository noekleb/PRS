DEF TEMP-TABLE ttXML
    FIELD cDocName   AS CHAR
    FIELD iLevel     AS INT 
    FIELD cElement   AS CHAR 
    FIELD cNodeValue AS CHAR
    FIELD iNodeIdx   AS INT
    .

DEFINE VAR httXML AS HANDLE NO-UNDO.
httXML = BUFFER ttXML:HANDLE.
