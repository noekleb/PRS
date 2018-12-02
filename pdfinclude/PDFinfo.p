/******************************************************************************

  Program:      PDFinfo.p
  
  Written By:   Gordon Campbell
  Written On:   January 2004
  
  Description:  returns information about PDF document
  
                Returned information includes:
                
                 PDF Version
                 Pages
                 Encrypted
                 Author
                 Creation Date
                 Creation Time
                 Producer
                 Creator
                 Subject
                 Title
                 Keywords
                 ModDate
                 ModTime
                 Font List

  Notes:        - Encrypted PDF files are not supported at this time

                - ReadLine is used instead of IMPORT UNFORMATTED due to 
                  potential differences in each of line characters.  Some PDF
                  documents use CHR(13), some use CHR(13) + CHR(10), and others
                  use CHR(10).  This causes quite the headache for the IMPORT
                  UNFORMATTED statement.  Of course, reading byte-by-byte slows
                  down the process but what do you do? 

******************************************************************************/

{ pdfinfo.i }

DEFINE INPUT PARAMETER pFileName  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR TT_Info.
DEFINE OUTPUT PARAMETER TABLE FOR TT_Font.

DEFINE TEMP-TABLE TT_Object NO-UNDO
  FIELD obj_ptr     AS INTEGER
  FIELD obj_id      AS INTEGER
  FIELD gen_id      AS INTEGER
  FIELD obj_seq     AS INTEGER
  FIELD obj_type    AS CHARACTER
  FIELD page_id     AS INTEGER
INDEX obj_id AS PRIMARY
      obj_id
INDEX gen_id
      gen_id
INDEX obj_seq
      obj_seq
INDEX obj_type
      obj_type.

DEFINE VARIABLE Text-Line   AS CHARACTER NO-UNDO.

/* variables used for seeking points in the document */
DEFINE VARIABLE obj-ctr   AS INTEGER NO-UNDO.
DEFINE VARIABLE root-obj  AS INTEGER NO-UNDO.
DEFINE VARIABLE root-gen  AS INTEGER NO-UNDO.
DEFINE VARIABLE info-obj  AS INTEGER NO-UNDO.
DEFINE VARIABLE info-gen  AS INTEGER NO-UNDO.
DEFINE VARIABLE pages-obj AS INTEGER NO-UNDO.
DEFINE VARIABLE pages-gen AS INTEGER NO-UNDO.
DEFINE VARIABLE row-ctr   AS INTEGER NO-UNDO.
DEFINE VARIABLE seek-ptr  AS INTEGER NO-UNDO.
DEFINE VARIABLE xref-ptr  AS INTEGER NO-UNDO.

/* Variables used for storage of document information */
DEFINE VARIABLE doc-version   AS CHARACTER NO-UNDO.
DEFINE VARIABLE doc-pages     AS INTEGER   NO-UNDO.
DEFINE VARIABLE doc-encrypted AS LOGICAL   NO-UNDO.
DEFINE VARIABLE doc-author    AS CHARACTER NO-UNDO.
DEFINE VARIABLE doc-createdon AS DATE      NO-UNDO.
DEFINE VARIABLE doc-createdat AS CHARACTER NO-UNDO.
DEFINE VARIABLE doc-producer  AS CHARACTER NO-UNDO.
DEFINE VARIABLE doc-creator   AS CHARACTER NO-UNDO.
DEFINE VARIABLE doc-subject   AS CHARACTER NO-UNDO.
DEFINE VARIABLE doc-title     AS CHARACTER NO-UNDO.
DEFINE VARIABLE doc-keywords  AS CHARACTER NO-UNDO.
DEFINE VARIABLE doc-moddate   AS DATE      NO-UNDO.
DEFINE VARIABLE doc-modtime   AS CHARACTER NO-UNDO.
DEFINE VARIABLE doc-bookmarks AS INTEGER NO-UNDO.
DEFINE VARIABLE doc-annots    AS INTEGER NO-UNDO.

FUNCTION ReadLine RETURNS CHARACTER():
  DEFINE VARIABLE L_Byte  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_Line  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Seek  AS INTEGER NO-UNDO.

  L_Seek = SEEK(INPUT).
  SET-SIZE(L_Byte) = 1.
  Read-Loop:
  DO WHILE TRUE:
    SEEK INPUT TO L_Seek.
    IMPORT L_Byte. 
    IF (GET-BYTE(L_Byte,1) = 10 OR GET-BYTE(L_Byte,1) = 13) THEN DO:
      IF GET-BYTE(L_Byte,1) = 13 THEN DO:
        IMPORT L_Byte. 
        IF GET-BYTE(L_Byte,1) = 10 THEN
          L_Seek = L_Seek + 1.
      END.

      L_Seek = L_Seek + 1.
      IMPORT L_Byte.
      IF (GET-BYTE(L_Byte,1) = 10) THEN
        L_Seek = L_Seek + 1.

      SEEK INPUT TO L_Seek.
      LEAVE Read-Loop.
    END.

    ASSIGN L_Seek = L_Seek + 1.
           L_Line = L_Line + CHR(GET-BYTE(L_Byte,1)).
  END.

  SET-SIZE(L_Byte) = 0.

  RETURN TRIM(L_Line).
END FUNCTION. /* ReadLine */

FUNCTION UpLine RETURNS LOGICAL ():
  DEFINE VARIABLE L_Byte  AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_Byte2 AS MEMPTR NO-UNDO.
  DEFINE VARIABLE L_Seek  AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Found AS LOGICAL NO-UNDO.

  L_Seek = SEEK(INPUT) - 1.
  SET-SIZE(L_Byte) = 1.
  SET-SIZE(L_Byte2) = 1.
  Read-Loop:
  DO WHILE TRUE:
    SEEK INPUT TO L_Seek.
    IMPORT L_Byte. 

    /* Find the first linefeed */
    IF  NOT L_Found
    AND ((GET-BYTE(L_Byte,1) = 10) 
    OR (GET-BYTE(L_Byte,1) = 13)) THEN DO:
      
      /* If Chr(10) found then determine if a CHR(13) appears before it.
         If it does then skip past it .. if not we need to reset our postion */
      IF GET-BYTE(L_Byte,1) = 10 THEN DO:
        L_Seek = SEEK(INPUT).
        SEEK INPUT TO L_Seek - 2.
        IMPORT L_Byte.
        IF GET-BYTE(L_Byte,1) = 13 THEN DO:
          L_Seek = L_Seek - 2.
        END.
        ELSE
          L_Seek = L_Seek - 1.
      END.

      L_Found = TRUE.
    END.

    ELSE IF L_Found
        AND ((GET-BYTE(L_Byte,1) = 10) 
    OR (GET-BYTE(L_Byte,1) = 13)) THEN DO:
      L_seek = SEEK(INPUT).
      LEAVE Read-Loop.
    END.
    
    ASSIGN L_Seek = L_Seek - 1.
  END. /* Read Loop */

  SEEK INPUT TO L_Seek.

  SET-SIZE(L_Byte2) = 0.
  SET-SIZE(L_Byte) = 0.

  RETURN TRUE.
END FUNCTION. /* UpLine */

etime(yes).

INPUT FROM VALUE(pFileName) BINARY NO-ECHO NO-CONVERT.
  
  /* Determine Version 
     - this should be on first line of a well-formed PDF document */
  text-line = ReadLine().
  doc-version = REPLACE(text-line,"%PDF-","").

  /* Go To End of File */
  SEEK INPUT TO END.
  seek-ptr = SEEK(input).

  UpLine().  /* Get the %%EOF line */
  UpLine().  /* Before the XREF pointer line */
  xref-ptr = INT(ReadLine()).  /* Get the XREF Pointer line */

  IF xref-ptr = 0 THEN DO:
    message "Incorrect starting point --- Why? I need to Figure that out!!"
            VIEW-AS ALERT-BOX.
    RETURN.
  END.

  RUN LoadObjectPointers(xref-ptr).
  IF doc-encrypted THEN DO:
    RUN CreateInfo("ERROR", "Document is Encrypted").
    RETURN.
  END.

  RUN ProcessObjectPointers.

INPUT CLOSE.

RUN CreateInfo("Pages",STRING(doc-pages)).
RUN CreateInfo("Process Time", STRING(etime / 1000) + " seconds").

/* ----------------------- INTERNAL PROCEDURES ------------------------ */
PROCEDURE LoadObjectPointers:
  DEFINE INPUT PARAMETER pPointer AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.
  DEFINE VARIABLE prev-ptr  AS INTEGER NO-UNDO.
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_Object.

  SEEK INPUT TO pPointer.

  DO WHILE TRUE:
    text-ptr = readline(). 

    IF LENGTH(text-ptr) > 0 
    AND SUBSTR(text-ptr,LENGTH(text-ptr),1) = "n" THEN DO:
      CREATE TT_Object.
      ASSIGN TT_Object.obj_ptr = INT(ENTRY(1,text-ptr," "))
             TT_Object.obj_seq = obj-ctr
             obj-ctr           = obj-ctr + 1.
    END.

    IF INDEX(text-ptr,"~/Root") > 0 THEN DO:
      text-ptr = TRIM(SUBSTR(text-ptr, INDEX(text-ptr,"~/Root") + 5)).

      ASSIGN root-obj = INT(ENTRY(1,text-ptr," "))
             root-gen = INT(ENTRY(2,text-ptr," ")).
    END.

    ELSE IF INDEX(text-ptr,"~/Info") > 0 THEN
      ASSIGN info-obj = INT(ENTRY(2,text-ptr," "))
             info-gen = INT(ENTRY(3,text-ptr," ")).

    ELSE IF INDEX(text-ptr,"~/Encrypt") > 0 THEN DO:
      doc-encrypted = TRUE.
      LEAVE.
    END.

    ELSE IF INDEX(text-ptr,"~/Prev") > 0 THEN DO:
      prev-ptr = INT(ENTRY(2,text-ptr," ")).

      curr-ptr = SEEK(input).
      RUN LoadObjectPointers(prev-ptr). 
      SEEK INPUT TO curr-ptr.
    END.

    IF INDEX(text-ptr,">>") > 0 THEN LEAVE.
  END.

END. /* LoadObjectPointers */

PROCEDURE ProcessObjectPointers:
  
  DEFINE VARIABLE text-line AS CHARACTER NO-UNDO.

  /* Determine Object and Generation Number */
  FOR EACH TT_Object.
    SEEK INPUT TO TT_object.obj_ptr.
    /* text-line  = ReadLine(). */
    IMPORT UNFORMATTED text-line.

    TT_Object.obj_id = INT(ENTRY(1,text-line," ")).
    TT_object.gen_id = INT(ENTRY(2,text-line," ")).

    IF  TT_object.obj_id = root-obj 
    AND TT_object.gen_id = root-gen THEN
      TT_Object.obj_type = "~/Root".

    ELSE IF  TT_object.obj_id = info-obj 
        AND TT_object.gen_id  = info-gen THEN
      TT_Object.obj_type = "~/Info".
  END.
  
  /* Read the Info Dictionary and Determine Document Info */
  FOR EACH TT_Object WHERE TT_object.obj_type = "~/Info"
      BREAK BY obj_id 
            BY gen_id 
            BY obj_seq.

    IF FIRST-OF( TT_Object.gen_id) THEN DO:
      RUN ProcessInfoDictionary.
    END. /* First-Of /Root */

  END. /* Info Dictionary */

  /* Read the Root Dictionary and determine the Page Objects */
  FOR EACH TT_Object WHERE TT_object.obj_type = "/Root"
      BREAK BY obj_id 
            BY gen_id 
            BY obj_seq.

    IF FIRST-OF( TT_Object.gen_id) THEN DO:
      RUN ProcessRootDictionary.
    END. /* First-Of /Root */

  END. /* Root Dictionary */

END. /* ProcessObjectPointers */

PROCEDURE ProcessInfoDictionary:
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.

  SEEK INPUT TO TT_Object.obj_ptr.
  text-line = Readline().

  DO WHILE TRUE:
    text-ptr = readline(). 
    
    IF INDEX(text-ptr,"~/Author") > 0 THEN DO:
      RUN ParseText (text-ptr, "~/Author", OUTPUT doc-author).
      RUN CreateInfo ("Author",doc-author).
    END.

    ELSE IF INDEX(text-ptr,"/Producer") > 0 THEN DO:
      RUN ParseText (text-ptr, "~/Producer", OUTPUT doc-producer).
      RUN CreateInfo ("Producer",doc-producer).
    END.

    ELSE IF INDEX(text-ptr,"/Creator") > 0 THEN DO:
      RUN ParseText (text-ptr, "~/Creator", OUTPUT doc-creator).
      RUN CreateInfo ("Creator",doc-creator).
    END.

    ELSE IF INDEX(text-ptr,"~/Title") > 0 THEN DO:
      RUN ParseText (text-ptr, "~/Title", OUTPUT doc-title).
      RUN CreateInfo ("Title",doc-title).
    END.

    ELSE IF INDEX(text-ptr,"~/Subject") > 0 THEN DO:
      RUN ParseText (text-ptr, "~/Subject", OUTPUT doc-subject).
      RUN CreateInfo ("Subject",doc-subject).
    END.
    
    ELSE IF INDEX(text-ptr,"~/Keywords") > 0 THEN DO:
      RUN ParseText (text-ptr, "~/Keywords", OUTPUT doc-keywords).
      RUN CreateInfo ("Keywords",doc-keywords).
    END.

    ELSE IF INDEX(text-ptr,"~/ModDate") > 0 THEN DO:
      RUN ParseText (text-ptr, "~/ModDate", OUTPUT text-ptr).

      doc-moddate = DATE(INT(SUBSTR(text-ptr,7,2)),
                         INT(SUBSTR(text-ptr,9,2)),
                         INT(SUBSTR(text-ptr,3,4))).
      RUN CreateInfo ("ModDate",STRING(doc-moddate)).

      doc-modtime = SUBSTR(text-ptr,11,2) + ":"
                  + SUBSTR(text-ptr,13,2) + ":"
                  + SUBSTR(text-ptr,15,2).
      RUN CreateInfo ("ModTime",doc-modtime).
    END.

    ELSE IF INDEX(text-ptr,"~/CreationDate") > 0 THEN DO:
      RUN ParseText (text-ptr, "~/CreationDate", OUTPUT text-ptr).

      doc-createdon = DATE(INT(SUBSTR(text-ptr,7,2)),
                           INT(SUBSTR(text-ptr,9,2)),
                           INT(SUBSTR(text-ptr,3,4))).
      RUN CreateInfo ("CreationDate",STRING(doc-createdon)).

      doc-createdat = SUBSTR(text-ptr,11,2) + ":"
                    + SUBSTR(text-ptr,13,2) + ":"
                    + SUBSTR(text-ptr,15,2).
      RUN CreateInfo ("CreationTime",doc-createdat).
    END.

    IF INDEX(text-ptr,">>") > 0 THEN LEAVE.
  END.

END. /* ProcessInfoDictionary */

PROCEDURE ParseText:
  DEFINE INPUT  PARAMETER pIn-Text  AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pReplace  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pOut-Text AS CHARACTER NO-UNDO.

  /* This removes any characters previous to the Replace text */
  pIn-Text = SUBSTR(pIn-Text, INDEX(pIn-Text,pReplace) + LENGTH(pReplace)).

  ASSIGN pIn-Text = REPLACE(pIn-Text,"~\(","&paraL;")
         pIn-Text = REPLACE(pIn-Text,"~\)","&paraR;")
         pOut-Text = TRIM(REPLACE(pIn-Text,pReplace,""))
         pOut-Text = REPLACE(pOut-Text,"(","")
         pOut-Text = REPLACE(pOut-Text,")","")
         pOut-Text = REPLACE(pOut-Text,"&paraL;","(")
         pOut-Text = REPLACE(pOut-Text,"&paraR;",")").

END. /* ParseText */

PROCEDURE CreateInfo:
  DEFINE INPUT PARAMETER pInfo  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pValue AS CHARACTER NO-UNDO.

  IF pValue = "" OR pValue = ? THEN RETURN.

  CREATE TT_Info.
  ASSIGN TT_info.info_name  = pInfo
         TT_info.info_value = pValue.

END. /* CreateInfo */

PROCEDURE ProcessRootDictionary:
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.

  SEEK INPUT TO TT_Object.obj_ptr.
  text-line = Readline().

  DO WHILE TRUE:
    text-ptr = readline().
    IF INDEX(text-ptr,"~/Pages") > 0 THEN DO:
      pages-obj = INT( ENTRY(2, text-ptr, " ") ).
      pages-gen = INT( ENTRY(3, text-ptr, " ") ).

      RUN ProcessPagesDictionary (pages-obj, pages-gen).

      LEAVE.
    END.

    IF INDEX(text-ptr,">>") > 0 THEN LEAVE.
  END.

END. /* ProcessRootDictionary */

PROCEDURE ProcessPagesDictionary:
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_InKids  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE L_Kids    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen     AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.
  
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_object.obj_id = pObject
                         AND B_TT_object.gen_id = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      SEEK INPUT TO B_TT_Object.obj_ptr.
      RUN ProcessKids.

    END. /* FIRST */
  END. /* each Pages Dictionary */

END. /* ProcessPagesDictionary */

PROCEDURE ProcessKids:
  DEFINE VARIABLE L_InKids  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE L_Kids    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_ptr     AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.

  text-ptr = Readline().

  DO WHILE TRUE:
    text-ptr = readline().

    IF INDEX(text-ptr,"~/Kids") > 0 THEN DO:
      L_InKids = TRUE.

      L_Kids = REPLACE(text-ptr,"[","").

      IF INDEX(text-ptr,"]") > 0 THEN DO:
        L_InKids = FALSE.
        L_Kids = REPLACE(L_Kids,"]","").
      END.

    END.

    ELSE IF L_InKids THEN DO:
      IF INDEX(text-ptr,"]") > 0 THEN DO:
        L_Kids = L_Kids + " " + SUBSTR(text-ptr,1,INDEX(text-ptr,"]") - 1).
        L_InKids = FALSE.
        LEAVE.
      END.
      ELSE
        L_Kids = L_Kids + " " + text-ptr.

    END.
    IF INDEX(text-ptr,">>") > 0 THEN LEAVE.
  END.

  L_Kids = TRIM(REPLACE(L_Kids,"~/Kids"," ")).

  DO L_Loop = 1 TO NUM-ENTRIES(L_Kids," ") BY 3:
    ASSIGN L_Obj = INT(ENTRY(L_Loop, L_Kids, " "))
           L_Gen = INT(ENTRY(L_Loop + 1, L_Kids, " ")).

    RUN ProcessPageDictionary (L_Obj, L_Gen).
  END.

END. /* ProcessKids */

PROCEDURE ProcessPAGEDictionary:
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.
  
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE obj-type  AS CHARACTER NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_object.obj_id = pObject
                         AND B_TT_object.gen_id = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      SEEK INPUT TO B_TT_Object.obj_ptr.
      text-ptr = Readline().

      DO WHILE TRUE:
        text-ptr = readline().

        IF INDEX(text-ptr,"~/Type") > 0  THEN DO:
          obj-type = TRIM(ENTRY(3, text-ptr, "~/")).
          B_TT_object.obj_type = obj-type.
        END.

        CASE obj-type:
          WHEN "Pages" THEN DO:
            RUN ProcessPagesDictionary (pObject, pGen).
            LEAVE.
          END.

          WHEN "Page" THEN DO:
            doc-pages = doc-pages + 1.
            B_TT_object.page_id = doc-pages.
            RUN ProcessPageObject (pObject, pGen, doc-pages).
            LEAVE.
          END.
        END CASE.

        IF INDEX(text-ptr,">>") > 0 THEN LEAVE.
      END.

    END. /* FIRST */
  END. /* each Pages Dictionary */

END. /* ProcessPAGEDictionary */

PROCEDURE ProcessPageObject:
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pPage    AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.
  
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Fonts   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.
  DEFINE VARIABLE res-obj   AS INTEGER NO-UNDO.
  DEFINE VARIABLE res-gen   AS INTEGER NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_object.obj_id = pObject
                         AND B_TT_object.gen_id = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      B_TT_Object.page_id = pPage.

      SEEK INPUT TO B_TT_Object.obj_ptr.
      text-ptr = Readline().

      DO WHILE TRUE:
        text-ptr = readline().

        IF INDEX(text-ptr,"~/Resources") > 0  THEN DO:
          ASSIGN res-obj = INT(ENTRY(2, text-ptr, " "))
                 res-gen = INT(ENTRY(3, text-ptr, " ")) NO-ERROR.

          IF ERROR-STATUS:ERROR THEN NEXT.

          curr-ptr = SEEK(INPUT).
          RUN ProcessResourceDictionary (res-obj, res-gen, pPage).
          SEEK INPUT TO curr-ptr.
        END.

        ELSE IF INDEX(text-ptr,"~/Font") > 0  THEN DO:
          curr-ptr = SEEK(INPUT).
          RUN ProcessFonts (text-ptr, OUTPUT L_Fonts).

          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,">>") > 0 THEN LEAVE.
      END.

    END. /* FIRST */
  END. /* each Pages Dictionary */

END. /* ProcessPageObject */

PROCEDURE ProcessResourceDictionary:

  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pPage    AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_InFonts AS LOGICAL NO-UNDO.
  DEFINE VARIABLE L_Fonts   AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.
  
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_ptr     AS INTEGER NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_object.obj_id = pObject
                         AND B_TT_object.gen_id = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      ASSIGN B_TT_Object.obj_type = "~/Resource"
             B_TT_Object.page_id  = pPage.

      SEEK INPUT TO B_TT_Object.obj_ptr.
      text-ptr = Readline().

      DO WHILE TRUE:
        text-ptr = readline().

        IF INDEX(text-ptr,"~/Font") > 0 THEN DO:
          RUN ProcessFonts (INPUT text-ptr, OUTPUT L_Fonts).
          LEAVE.
        END.

        /*
        IF INDEX(text-ptr,"~/Font") > 0 THEN DO:
          RUN ProcessFonts (OUTPUT L_Fonts).
        
          L_InFonts = TRUE.
 
          L_Fonts = REPLACE(text-ptr,"[","").
          L_Fonts = REPLACE(text-ptr,"<<","").

          IF INDEX(text-ptr,"]") > 0 
          OR INDEX(text-ptr,">>") > 0 THEN DO:
            L_InFonts = FALSE.
            L_Fonts = REPLACE(L_Fonts,"]","").
            L_Fonts = REPLACE(L_Fonts,">>","").
          END.
        END.

        ELSE IF L_InFonts THEN DO:
          IF INDEX(text-ptr,"]") > 0 
          OR INDEX(text-ptr,">>") > 0 THEN DO:
            IF INDEX(text-ptr,"]") > 0 THEN
              L_Fonts = L_Fonts + " " + SUBSTR(text-ptr,1,INDEX(text-ptr,"]") - 1).
            ELSE
              L_Fonts = L_Fonts + " " + SUBSTR(text-ptr,1,INDEX(text-ptr,">>") - 1).

            L_InFonts = FALSE.
            LEAVE.
          END.
          ELSE
            L_Fonts = L_Fonts + " " + text-ptr.

        END.
        */

        IF INDEX(text-ptr,">>") > 0 THEN LEAVE.

      END. /* While True */

    END. /* FIRST */
  END. /* each Pages Dictionary */

  /***
  L_Fonts = REPLACE(L_Fonts,"~/Font ","").
  L_Fonts = REPLACE(L_Fonts,"<<","").
  L_Fonts = TRIM(REPLACE(L_Fonts,">>","")).

  MESSAGE L_Fonts view-as alert-box.

  DO L_Loop = 1 TO NUM-ENTRIES(L_Fonts," ") BY 4:
    ASSIGN L_Obj = INT(ENTRY(L_Loop + 1, L_Fonts, " "))
           L_Gen = INT(ENTRY(L_Loop + 2, L_Fonts, " ")).

    RUN ProcessFontDictionary (L_Obj, L_Gen).
  END.
  **/

END. /* ProcressResourceDictionary */

PROCEDURE ProcessFonts:
  DEFINE INPUT  PARAMETER pLine   AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pFonts  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop  AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen   AS INTEGER NO-UNDO.

  DO WHILE TRUE:
    IF INDEX(pLine,"]") > 0 
    OR INDEX(pLine,">>") > 0 THEN DO:
      IF INDEX(pLine,"]") > 0 THEN
        pFonts = pFonts + " " + SUBSTR(pLine,1,INDEX(pLine,"]") - 1).
      ELSE
        pFonts = pFonts + " " + SUBSTR(pLine,1,INDEX(pLine,">>") + 1).

      LEAVE.
    END.
    ELSE
      pFonts = pFonts + " " + pLine.
    
    pLine = readline().

    IF INDEX(pLine,">>") > 0 THEN LEAVE.

  END. /* While True */

  /* Remove stuff after the >> indicator */
  IF INDEX(Pfonts,">>") > 0 THEN
    pFonts = ENTRY(1,pFonts,">>").

  pFonts = REPLACE(pFonts,"~/Font ","").
  pFonts = REPLACE(pFonts,"<<","").
  pFonts = TRIM(REPLACE(pFonts,">>","")).


  DO L_Loop = 1 TO NUM-ENTRIES(pFonts," ") BY 4:
    ASSIGN L_Obj = INT(ENTRY(L_Loop + 1, pFonts, " "))
           L_Gen = INT(ENTRY(L_Loop + 2, pFonts, " ")).

    RUN ProcessFontDictionary (L_Obj, L_Gen).
  END.

END. /* ProcessFonts */

PROCEDURE ProcessFontDictionary:
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-ptr    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE base-font   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE descriptor  AS LOGICAL NO-UNDO.

  DEFINE VARIABLE obj-ptr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE gen-ptr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_object FOR TT_object.

  /* If the Font has already been processed then don't do it again */
  IF CAN-FIND(FIRST B_TT_Object
              WHERE B_TT_Object.obj_id = pObject
                AND B_TT_Object.gen_id = pGen
                AND B_TT_Object.obj_type = "~/Font" NO-LOCK)
  THEN RETURN.

  FOR EACH B_TT_Object WHERE B_TT_object.obj_id = pObject
                         AND B_TT_object.gen_id = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      B_TT_Object.obj_type = "~/Font".

      SEEK INPUT TO B_TT_Object.obj_ptr.
      text-line = Readline().

      DO WHILE TRUE:
        text-ptr = readline().
        
        IF INDEX(text-ptr,"~/FontDescriptor") > 0 THEN DO:
          ASSIGN obj-ptr = INT(ENTRY(2, text-ptr, " "))
                 gen-ptr = INT(ENTRY(3, text-ptr, " ")).

          descriptor = TRUE.
          curr-ptr = SEEK(INPUT).
          RUN ProcessFontDescriptorDictionary (obj-ptr, gen-ptr).
          SEEK INPUT TO curr-ptr.
        END.

        ELSE IF INDEX(text-ptr,"~/BaseFont") > 0 THEN DO:
          base-font = ENTRY(3, text-ptr, "/") + " [Base]".
        END.

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
      END. /* True */
    END. /* First */
  END. /* each */

  IF NOT descriptor THEN DO:
    CREATE TT_Font.
    TT_Font.font_name = base-font.
  END.

END. /* ProcessFontDictionary */

PROCEDURE ProcessFontDescriptorDictionary:
  
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_Object.

  FOR EACH B_TT_Object WHERE B_TT_object.obj_id = pObject
                         AND B_TT_object.gen_id = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      SEEK INPUT TO B_TT_Object.obj_ptr.
      text-ptr = Readline().

      DO WHILE TRUE:
        text-ptr = readline().

        IF INDEX(text-ptr,"~/FontName") > 0  THEN DO:
          CREATE TT_Font.
          TT_Font.font_name = ENTRY(3,text-ptr,"~/") + " [Embedded]".
        END.

        IF INDEX(text-ptr,">>") > 0 THEN LEAVE.
      END.

    END. /* FIRST */
  END. /* for each  */

END. /* ProcessFontDescriptorDictionary */


/* end of PDFinfo,gif */
