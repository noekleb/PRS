CURRENT-WINDOW:WIDTH-CHARS = 200.

DEF VAR cVariable       AS CHAR  NO-UNDO.
DEF VAR cSource         AS CHAR  NO-UNDO.
DEF VAR cPart1          AS CHAR  NO-UNDO.
DEF VAR cPart2          AS CHAR  NO-UNDO.
DEF VAR cPart3          AS CHAR  NO-UNDO.
DEF VAR bNextEntry      AS LOG   NO-UNDO.
DEF VAR ix              AS INT   NO-UNDO.
DEF VAR iy              AS INT   NO-UNDO.
DEF VAR iNumButtons     AS INT   NO-UNDO.
DEF VAR bReadNext       AS LOG   NO-UNDO.
DEF VAR bAppendFromHere AS LOG   NO-UNDO.
DEF VAR rUpdate         AS ROWID NO-UNDO.
DEF VAR iMaxLine        AS INT   NO-UNDO.
DEF VAR cConvClip       AS CHAR  NO-UNDO.

DEF TEMP-TABLE ttClipText
    FIELD iLineNum  AS INT
    FIELD cClipLine AS CHAR
    FIELD cVarName  AS CHAR
    FIELD cBtnName  AS CHAR
    FIELD bBtnAdded AS LOG
    FIELD fVarSize  AS DEC
    FIELD bSkipLine AS LOG
    .

DEF BUFFER bttClipText  FOR ttClipText.
DEF BUFFER bbttClipText FOR ttClipText.

OUTPUT TO VALUE(SESSION:TEMP-DIR + "jbClip.txt").
PUT UNFORMATTE CLIPBOARD:VALUE.
OUTPUT CLOSE.

INPUT FROM VALUE(SESSION:TEMP-DIR + "jbClip.txt").
REPEAT :
  ix = ix + 100.
  CREATE ttClipText.
  IMPORT UNFORMATTED ttClipText.cClipLine.
  ttClipText.iLineNum = ix.
END.
iMaxLine = ix.
INPUT CLOSE.

/* Find the size of the vars (FK) that should have the lookup button defined so we can place the button in the right column: */
FOR EACH ttClipText BY ttClipText.iLineNum:
  IF ttClipText.cClipLine BEGINS "DEFINE VARIABLE" THEN
    ASSIGN ttClipText.cVarName  = ENTRY(3,ttClipText.cClipLine," ")
           ttClipText.bSkipLine = YES
           rUpdate              = ROWID(ttClipText)
           bReadNext            = YES.

  IF bReadNext THEN DO:
    ttClipText.bSkipLine = YES.
    IF ttClipText.cClipLine BEGINS "     SIZE" THEN DO:
      FIND bttClipText
           WHERE ROWID(bttClipText) = rUpdate.
      cPart1 = ENTRY(2,TRIM(ttClipText.cClipLine)," ").
      IF SESSION:NUMERIC-DECIMAL-POINT = "," THEN
        cPart1 = REPLACE(cPart1,".",",").
      ASSIGN bttClipText.fVarSize = DEC(cPart1)
             bReadNext            = NO
             .
    END.
  END.
END.

/* REPLACE the VARIABLE (FK) in the frame definition with the button: */
bReadNext = NO.
FOR EACH ttClipText BY ttClipText.iLineNum:
  IF ttClipText.cClipLine BEGINS "FORM" THEN
    bReadNext            = YES.
  ELSE IF bReadNext THEN DO:
    IF ttClipText.cClipLine MATCHES "*AT ROW*" THEN DO:
      ASSIGN cVariable = ENTRY(1,TRIM(ttClipText.cClipLine)," ")
             ttClipText.cBtnName  = "btn" + cVariable
             ttClipText.cClipLine = SUBSTR(ttClipText.cClipLine,1,5) + "btn" + SUBSTR(ttClipText.cClipLine,6)
             .
      FIND bttClipText
           WHERE bttClipText.cVarName = cVariable.
      ASSIGN cSource     = TRIM(ttClipText.cClipLine)
             cPart1      = ""
             cPart2      = ""
             bNextEntry  = NO 
             .
      DO iy = 1 TO NUM-ENTRIES(cSource," "):
        IF NOT bNextEntry THEN
          cPart1 = cPart1 + ENTRY(iy,cSource," ") + " ".
        IF ENTRY(iy,cSource," ") = "COL" THEN 
          bNextEntry = YES.  
        IF bNextEntry AND ENTRY(iy,cSource," ") NE "COL" AND cPart2 = "" THEN DO:
          cPart2 = REPLACE(STRING(DEC(ENTRY(iy,cSource," ")) + bttClipText.fVarSize + 2),",",".") + " ".
          LEAVE.
        END.
      END.
      ttClipText.cClipLine = "    " + cPart1 + cPart2 + cPart3.
    END.
    ELSE IF NOT ttClipText.cClipLine BEGINS "    WITH" THEN
      ttClipText.bSkipLine = YES.
    ELSE
      bReadNext = NO.
  END.
END.

/* Add new buttons to definition of enabled objects: */
ASSIGN bReadNext       = NO
       bAppendFromHere = NO.
FOR EACH ttClipText BY ttClipText.iLineNum:
  IF ttClipText.cClipLine BEGINS "&Scoped-Define ENABLED-OBJECTS" THEN DO:
    IF SUBSTR(ttClipText.cClipLine,LENGTH(ttClipText.cClipLine)) = CHR(126) THEN
      bReadNext = YES.
    ELSE 
      bAppendFromHere = YES.
  END.
  IF ttClipText.cClipLine BEGINS "&Scoped-Define DISPLAYED-OBJECTS" THEN 
    ASSIGN bReadNext = NO
           bAppendFromHere = NO.
      
  IF bReadNext AND SUBSTR(ttClipText.cClipLine,LENGTH(ttClipText.cClipLine)) NE CHR(126) THEN 
    bAppendFromHere = YES.
  
  IF bAppendFromHere THEN DO:
    FIND bbttClipText WHERE ROWID(bbttClipText) = ROWID(ttClipText).
    ASSIGN ix = 0
           iy = 0.
    FOR EACH bttClipText
        WHERE bttClipText.cBtnName NE "":
      iNumButtons = iNumButtons + 1.
    END.
    FOR EACH bttClipText
        WHERE bttClipText.cBtnName NE "":
      iy = iy + 1.
      IF LENGTH(bbttClipText.cClipLine + " " + bttClipText.cBtnName) LT 71  THEN
        bbttClipText.cClipLine = bbttClipText.cClipLine + " " + bttClipText.cBtnName.
      ELSE IF iy LE iNumButtons THEN DO:
        bbttClipText.cClipLine = bbttClipText.cClipLine + " " + CHR(126).
        CREATE bbttClipText.
        ASSIGN bbttClipText.iLineNum  = ttClipText.iLineNum + iy * 10
               bbttClipText.cClipLine = bttClipText.cBtnName.
      END.
    END.
  END.
END.

/* Add definitions of new buttons and remove VARIABLE definitions: */
bReadNext = NO.
FOR EACH ttClipText BY ttClipText.iLineNum:
  IF ttClipText.cClipLine BEGINS "/* Definitions of the field level widgets" THEN DO:
    bReadNext = YES.
    ix = 1.
    FOR EACH bttClipText
        WHERE bttClipText.cBtnName NE "":
      CREATE bbttClipText.
      ASSIGN bbttClipText.cClipLine = "DEFINE BUTTON " + bttClipText.cBtnName
             bbttClipText.iLineNum  = ttClipText.iLineNum + ix
             ix = ix + 1.
      CREATE bbttClipText.
      ASSIGN bbttClipText.cClipLine = '     LABEL "..."'
             bbttClipText.iLineNum  = ttClipText.iLineNum + ix
             ix = ix + 1.
      CREATE bbttClipText.
      ASSIGN bbttClipText.cClipLine = "     SIZE 4 BY 1."
             bbttClipText.iLineNum  = ttClipText.iLineNum + ix
             ix = ix + 1.
      CREATE bbttClipText.
      ASSIGN bbttClipText.cClipLine = " "
             bbttClipText.iLineNum  = ttClipText.iLineNum + ix
             ix = ix + 1.
    END.

  END.
  ELSE IF bReadNext THEN DO:
    IF ttClipText.cClipLine BEGINS "/* ************************  Frame Definitions" THEN 
      bReadNext = NO.
    ELSE 
      ttClipText.bSkipLine = YES.
  END.
END.

/* Add trigger code for buttons: */
FIND FIRST ttClipText
     WHERE ttClipText.cClipLine BEGINS "/* ************************  Control Triggers  ************************ */"
     NO-ERROR.
IF NOT AVAIL ttClipText THEN DO:
  CREATE ttClipText.
  ASSIGN ix = iMaxLine + 1
         ttClipText.iLineNum  = ix
         ttClipText.cClipLine = "/* ************************  Control Triggers  ************************ */".
  CREATE ttClipText.
  ASSIGN ix = ix + 1
         ttClipText.iLineNum  = ix
         ttClipText.cClipLine = " ".
END.
ELSE ix = ttClipText.iLineNum + 1.

iy = 0.
FOR EACH ttClipText
    WHERE ttClipText.cBtnName NE "":
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = " "
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "&Scoped-define SELF-NAME " + ttClipText.cBtnName
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL " + ttClipText.cBtnName
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "ON CHOOSE OF " + ttClipText.cBtnName + " IN FRAME DEFAULT-FRAME"
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "DO:"
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "  DEF VAR cReturnValues AS CHAR NO-UNDO."
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "  DEF VAR bOk           AS LOG  NO-UNDO."
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = " "
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE."
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "  RUN JBoxLookup.w (THIS-PROCEDURE,100,"
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = '                    "Customer"'
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = '                    + ";CustNum;Name"'
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = '                   ,"WHERE TRUE"'
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = '                    ,""'
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = '                    ,"CustNum,Name",   /* <- return values for these fields */ '
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "                    OUTPUT cReturnValues,"
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "                    OUTPUT bOK)."
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE."
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = " "
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = '  IF bOk AND cReturnValues NE "" THEN DO:'
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "    ASSIGN " + LEFT-TRIM(ttClipText.cBtnName,"btn") + ':SCREEN-VALUE = STRING(ENTRY(1,cReturnValues,"|"))'
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "          ."
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = '    APPLY "ANY-PRINTABLE" TO ' + LEFT-TRIM(ttClipText.cBtnName,"btn") + "."
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "  END."
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "END."
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "  "
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "/* _UIB-CODE-BLOCK-END */"
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "&ANALYZE-RESUME"
         ix = ix + 1.
  CREATE bttClipText.
  ASSIGN bttClipText.iLineNum = ix
         bttClipText.cClipLine = "  "
         ix = ix + 1.

  iy = iy + 1.
  IF iy = iNumButtons THEN DO:
    CREATE bttClipText.
    ASSIGN bttClipText.iLineNum = ix
           bttClipText.cClipLine = "&UNDEFINE SELF-NAME"
           ix = ix + 1.
  END.
END.
                                 
ix = 0.
/* OUTPUT TO c:\temp\jbModClip.txt. */
FOR EACH ttClipText 
    WHERE NOT ttClipText.bSkipLine
    BY ttClipText.iLineNum:
  ix = ix + 1.
  IF ix = 1 AND ttClipText.cClipLine = "" THEN NEXT.
  cConvClip = cConvClip + ttClipText.cClipLine + CHR(10).

/*   PUT UNFORMATTED ttClipText.cClipLine SKIP. */
/*   IF ttClipText.cClipLine = "" THEN  */
/*     PUT " " SKIP. */
END.
/* OUTPUT CLOSE. */

CLIPBOARD:VALUE = cConvClip.
