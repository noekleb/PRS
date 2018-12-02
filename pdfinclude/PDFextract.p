/******************************************************************************

  Program:      pdfextract.p
  
  Written By:   Gordon Campbell
  Written On:   January 2004
  
  Description:  Allows you to extract a Page from an existing PDF document

  Notes:        - Encrypted PDF files are not supported at this time
                - Only works with very simple PDF documents created by
                  Adobe Distiller (do not use Ghostscript generated PDFs)

  History:
  
  05/31/04  G Campbell  Fixed issue with Array processing in ProcessEntry
  
  06/03/04  G Campbell  Add code to handle XObject resource.  This allows us
                        to extract a PDF page that has JPEG and GIF images on
                        it.
  
  06/08/04  G Campbell  Reworked logic to allow for Adobe 6 Distilled documents
                        (the buggers at Adobe changed the output format).
                        
  06/09/04  G Campbell  Added code to handle Shading (Gradient Fill) in PDF
                        Documents (for v5 -- v6 handlies it differently)
  
  06/15/04  G Campbell  Added variable Entry4 and use it in place of 
                        ENTRY(4 ... since we can use the NO-ERROR when 
                        assigning the Entry4 variable.  This was changed 
                        because it was causing an issue with parsing some of
                        the PDF information (on some files).
                        
  06/16/04  G Campbell  Added code to handle /Encoding operator in the Font
                        Dictionary and also the FontFile3 operator
                        
                        Plus added NO-ERROR when using DATE statement (just in
                        case the info is not found)
                        
  06/23/04  G Campbell  Added code to cleanup h_Zlib and h_Zlibbind
  
  07/07/04  G Campbell  Added code to extract the Text Form Fields from a PDF
                        document.  This change was in conjunction with changes
                        made to pdf_inc.p that allow you to use Form fields
                        to place data in .. see the pdf_fill_text procedure 
                        in pdf_inc.p or the FormFill.p sample in the samples
                        directory.

  07/30/04  G Campbell  Updated to allow for usage of multi-page PDF template
                        documents with Form Fields.  Basically, checked for
                        "/P" in the field dictionary to determine which
                        page it belonged to and then assigned the widget it's
                        appropriate page number.

  08/11/04  G Campbell  Added procedure GetListOfFonts to accommodate how
                        OpenOffice defines it's fonts.  Instead of doing:

                        /Fonts <</TTF1 10 0 R>>    like in Distiller

                        it does the following:

                        /Fonts 10 0 R

                        Where 10 0 is a reference to a list of Fonts in another
                        object.

  09/13/04  G Campbell  Added code to handle landscape PDF templates.

                        Plus added code to handle Base Fonts that are defined
                        in the PDF template but are not embedded.

  10/07/04  G Campbell  Added code to set res_old and res_text for an XObject.
                        This was required because if merging Xobjects from more
                        than one document they had to be unique names.

  10/07/04  G Campbell  Added code to determine the MediaBox for Pages or Page.

  10/12/04  G Campbell  Added code to load the Zlib procedure since the call 
                        was removed from zlib.i.

  01/35/05  G Campbell  Added code to handle ToUnicode and DescendantFonts
                        dictionaries -- INCOMPLETE

  04/14/05  G Campbell  Added code to determine the CropBox for Page.

  07/06/05  G Campbell  Removed zlib.i.  Functions etc now defined in 
                        pdf_func.i and pdf_inc.p
                        
  09/22/05  G Campbell  Added code to handle Content when an array object

  10/21/05  G Campbell  Peter Kiss identified an issue regarding a Font not
                        being included in a multi-page template.  Basically
                        what happened was, I added a check in previous version
                        to ensure that multiple instances of the same resource
                        weren't added to the new document.  The find of the
                        resource was missing the Page ID field so it wouldn't 
                        include the same resource onto multiple pages.  This
                        was fixed in the ProcessEntry procedure. See 10/21/05.

  01/06/06  G Campbell  Added TT_Font.font_base to store the BaseFont name
                        which is (potentiall) different from the Font name (or
                        tag).

  02/15/06  G Campbell  Added code to handle CropBox and MediaBox which are
                        defined as objects in the the Pages Dictionary (as per
                        samples sent in by Simon Murrey)
                        
******************************************************************************/

{ pdfglobal.i "SHARED" }
{ pdf_func.i h_PDFinc}

DEFINE INPUT PARAMETER pStream    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pFileName  AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER pID        AS CHARACTER NO-UNDO.

DEFINE VARIABLE Text-Line   AS CHARACTER NO-UNDO.
DEFINE VARIABLE Entry4      AS CHARACTER NO-UNDO.

/* variables used for seeking points in the document */
DEFINE VARIABLE obj-ctr   AS INTEGER NO-UNDO.
DEFINE VARIABLE root-obj  AS INTEGER NO-UNDO.
DEFINE VARIABLE root-gen  AS INTEGER NO-UNDO.
DEFINE VARIABLE info-obj  AS INTEGER NO-UNDO.
DEFINE VARIABLE info-gen  AS INTEGER NO-UNDO.
DEFINE VARIABLE pages-obj AS INTEGER NO-UNDO.
DEFINE VARIABLE pages-gen AS INTEGER NO-UNDO.
DEFINE VARIABLE form-obj  AS INTEGER NO-UNDO.
DEFINE VARIABLE form-gen  AS INTEGER NO-UNDO.
DEFINE VARIABLE row-ctr   AS INTEGER NO-UNDO.
DEFINE VARIABLE seek-ptr  AS INTEGER NO-UNDO.
DEFINE VARIABLE xref-ptr  AS INTEGER NO-UNDO.

/* variables used to Store Page MediaBox info */
DEFINE VARIABLE decDefMedia1 AS DECIMAL DECIMALS 5 NO-UNDO.
DEFINE VARIABLE decDefMedia2 AS DECIMAL DECIMALS 5 NO-UNDO.
DEFINE VARIABLE decDefMedia3 AS DECIMAL DECIMALS 5 NO-UNDO.
DEFINE VARIABLE decDefMedia4 AS DECIMAL DECIMALS 5 NO-UNDO.

DEFINE VARIABLE decMedia1    AS DECIMAL DECIMALS 5 NO-UNDO.
DEFINE VARIABLE decMedia2    AS DECIMAL DECIMALS 5 NO-UNDO.
DEFINE VARIABLE decMedia3    AS DECIMAL DECIMALS 5 NO-UNDO.
DEFINE VARIABLE decMedia4    AS DECIMAL DECIMALS 5 NO-UNDO.

DEFINE VARIABLE decCrop1     AS DECIMAL DECIMALS 5 NO-UNDO.
DEFINE VARIABLE decCrop2     AS DECIMAL DECIMALS 5 NO-UNDO.
DEFINE VARIABLE decCrop3     AS DECIMAL DECIMALS 5 NO-UNDO.
DEFINE VARIABLE decCrop4     AS DECIMAL DECIMALS 5 NO-UNDO.

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


/* ----------------------- INTERNAL PROCEDURES ------------------------ */
PROCEDURE LoadObjectPointers:
  DEFINE INPUT PARAMETER pPointer AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.
  DEFINE VARIABLE prev-ptr  AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_Object.

  SEEK INPUT TO pPointer.

  DO WHILE TRUE:
    text-ptr = readline(). 

    IF LENGTH(text-ptr) > 0 
    AND SUBSTR(text-ptr,LENGTH(text-ptr),1) = "n" THEN DO:
      CREATE TT_Object.
      ASSIGN TT_Object.obj_stream = pStream
             TT_Object.pdf_id     = pID
             TT_Object.obj_ptr    = INT(ENTRY(1,text-ptr," "))
             TT_Object.obj_seq    = obj-ctr
             obj-ctr              = obj-ctr + 1.
    END.

    IF INDEX(text-ptr,"~/Root") > 0 THEN DO:
      text-temp = TRIM(SUBSTR(text-ptr, INDEX(text-ptr,"~/Root") + 5)).

      ASSIGN root-obj = INT(ENTRY(1,text-temp," "))
             root-gen = INT(ENTRY(2,text-temp," ")).
    END.

    IF INDEX(text-ptr,"~/Info") > 0 THEN DO:
      text-temp = TRIM(SUBSTR(text-ptr, INDEX(text-ptr,"~/Info") + 5)).

      ASSIGN info-obj = INT(ENTRY(1,text-temp," "))
             info-gen = INT(ENTRY(2,text-temp," ")).
    END.

    IF INDEX(text-ptr,"~/Encrypt") > 0 THEN DO:
      doc-encrypted = TRUE.
      LEAVE.
    END.

    IF INDEX(text-ptr,"~/Prev") > 0 THEN DO:
      text-temp = TRIM(SUBSTR(text-ptr, INDEX(text-ptr,"~/Prev") + 5)).
      text-temp = REPLACE(text-temp,"~/"," ").
      prev-ptr = INT(ENTRY(1,text-temp," ")).

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
  FOR EACH TT_Object WHERE TT_Object.obj_stream = pStream
                       AND TT_Object.pdf_id     = pID.

    SEEK INPUT TO TT_object.obj_ptr.
    text-line  = ReadLine().

    TT_Object.obj_id = INT(ENTRY(1,text-line," ")).
    TT_object.gen_id = INT(ENTRY(2,text-line," ")) NO-ERROR.

    IF  TT_object.obj_id = root-obj 
    AND TT_object.gen_id = root-gen THEN
      TT_Object.obj_type = "~/Root".

    ELSE IF  TT_object.obj_id = info-obj 
        AND TT_object.gen_id  = info-gen THEN
      TT_Object.obj_type = "~/Info".
  END.
  
  /* Read the Info Dictionary and Determine Document Info */
  FOR EACH TT_Object WHERE TT_Object.obj_stream = pStream
                       AND TT_Object.pdf_id     = pID
                       AND TT_object.obj_type   = "~/Info"
      BREAK BY obj_id 
            BY gen_id 
            BY obj_seq.

    IF FIRST-OF( TT_Object.gen_id) THEN DO:
      RUN ProcessInfoDictionary.
    END. /* First-Of /Root */

  END. /* Info Dictionary */

  /* Read the Root Dictionary and determine the Page Objects */
  FOR EACH TT_Object WHERE TT_Object.obj_stream = pStream
                       AND TT_Object.pdf_id     = pID
                       AND TT_object.obj_type   = "/Root"
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
                         INT(SUBSTR(text-ptr,3,4))) NO-ERROR.
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
                           INT(SUBSTR(text-ptr,3,4))) NO-ERROR.
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
  ASSIGN TT_Info.obj_stream = pStream
         TT_info.pdf_id     = pID
         TT_info.info_name  = pInfo
         TT_info.info_value = pValue.

END. /* CreateInfo */

PROCEDURE ExtraInfo:
  DEFINE INPUT PARAMETER pInfo  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pValue AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pExtra AS CHARACTER NO-UNDO.
  
  IF pValue = "" OR pValue = ? THEN RETURN.

  FIND TT_info WHERE TT_info.pdf_id = Pid
                 AND TT_info.info_name = pInfo
                 AND TT_info.info_Value = pValue NO-ERROR.
  IF NOT AVAIL TT_info THEN DO:
    CREATE TT_Info.
    ASSIGN TT_Info.obj_stream = pStream
           TT_info.pdf_id     = pID
           TT_info.info_name  = pInfo
           TT_info.info_value = pValue.
  END.

  TT_info.info_extra = pExtra.

END. /* ExtraInfo */

PROCEDURE ProcessRootDictionary:
  DEFINE VARIABLE text-ptr   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE temp-text  AS CHARACTER NO-UNDO.

  SEEK INPUT TO TT_Object.obj_ptr.
  /* text-line = Readline(). */

  DO WHILE TRUE:
    text-ptr = readline().

    IF INDEX(text-ptr,"~/Pages") > 0 THEN DO:
      temp-text = SUBSTR(text-ptr,INDEX(text-ptr,"~/Pages")).
      pages-obj = INT( ENTRY(2, temp-text, " ") ).
      pages-gen = INT( ENTRY(3, temp-text, " ") ).
   
      RUN ProcessPagesDictionary (pages-obj, pages-gen).

    END.

    IF INDEX(text-ptr,"~/AcroForm") > 0 THEN DO:
      temp-text = SUBSTR(text-ptr,INDEX(text-ptr,"~/AcroForm")).
      form-obj = INT( ENTRY(2, temp-text, " ") ).
      form-gen = INT( ENTRY(3, temp-text, " ") ).
      
      RUN ProcessAcroFormDictionary (form-obj, form-gen).
    END.

    IF INDEX(text-ptr,">>") > 0 THEN LEAVE.
  END.

END. /* ProcessRootDictionary */

PROCEDURE ProcessAcroFormDictionary:
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_InFields  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE L_Fields    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen     AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Curr    AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.
  
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.

  L_Curr = SEEK(INPUT).

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pId
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      SEEK INPUT TO B_TT_Object.obj_ptr.
      RUN ProcessFields.

    END. /* FIRST */
  END. /* each Pages Dictionary */

  SEEK INPUT TO L_Curr.

END. /* ProcessAcroFormDictionary */

PROCEDURE ProcessFields:
  DEFINE VARIABLE L_InFields  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE L_Fields    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_ptr     AS INTEGER NO-UNDO.

  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  DO WHILE TRUE:
    text-ptr = readline().

    IF INDEX(text-ptr,"~/Fields") > 0 THEN DO:
      L_InFields = TRUE.

      L_Fields = SUBSTR(text-ptr,INDEX(text-ptr,"[") + 1).

      IF INDEX(text-ptr,"]") > 0 THEN DO:
        L_InFields = FALSE.
        L_Fields = SUBSTR(L_Fields,1,INDEX(L_Fields,"]") - 1).
        /* L_Fields = REPLACE(L_Fields,"]",""). */
      END.

    END. /* Fields */

    ELSE IF L_InFields THEN DO:
      IF INDEX(text-ptr,"]") > 0 THEN DO:
        L_Fields = L_Fields + " " + SUBSTR(text-ptr,1,INDEX(text-ptr,"]") - 1).
        L_InFields = FALSE.
      END.
      ELSE
        L_Fields = L_Fields + " " + text-ptr.

    END.

    IF INDEX(text-ptr,"~/DR") > 0  THEN DO:
       RUN ProcessDR (text-ptr).
     END.

    IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
  END.

  L_Fields = TRIM(REPLACE(L_Fields,"~/Fields"," ")).

  IF INDEX(L_Fields,"~/") > 0 THEN
    L_Fields = SUBSTR(L_Fields,1,INDEX(L_Fields,"~/") - 1).

  DO L_Loop = 1 TO NUM-ENTRIES(L_Fields," ") BY 3:
    ASSIGN L_Obj = INT(ENTRY(L_Loop, L_Fields, " "))
           L_Gen = INT(ENTRY(L_Loop + 1, L_Fields, " ")).

    RUN ProcessFieldDictionary (L_Obj, L_Gen, "", "", "").
  END.

END. /* ProcessFields */

PROCEDURE ProcessFieldDictionary:
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER obj-name AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER obj-type AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER obj-disp AS CHARACTER NO-UNDO.

  DEFINE VARIABLE par-obj   AS INTEGER NO-UNDO.
  DEFINE VARIABLE par-gen   AS INTEGER NO-UNDO.
  DEFINE VARIABLE par-page  AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.
  DEFINE BUFFER B_Par_Object FOR TT_object.
  
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  DEFINE VARIABLE obj-rect  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_HasKids AS LOGICAL NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      SEEK INPUT TO B_TT_Object.obj_ptr.

      DO WHILE TRUE:
        text-ptr = readline().

        IF INDEX(text-ptr,"~/FT") > 0  THEN DO:
          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/FT")).
          obj-type = TRIM(ENTRY(3, text-temp, "~/")).

          B_TT_object.obj_type = "Widget".
        END.

        IF INDEX(text-ptr,"~/Rect") > 0  THEN DO:
          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/Rect") + 5).
          text-temp = SUBSTR(text-temp,1,INDEX(text-temp,"]")).

          obj-rect = REPLACE(text-temp,"[","").
          obj-rect = TRIM(REPLACE(obj-rect,"]","")).
        END.

        IF INDEX(text-ptr,"~/T ") > 0 THEN DO:
          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/T ") + 3).
          text-temp = SUBSTR(text-temp,1,INDEX(text-temp,")")).

          obj-name = REPLACE(text-temp,"(","").
          obj-name = TRIM(REPLACE(obj-name,")","")).
        END.

        IF INDEX(text-ptr,"~/P ") > 0 THEN DO:
          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/P ") + 3).

          ASSIGN par-obj = INT(ENTRY(1,text-temp," "))
                 par-gen = INT(ENTRY(2,text-temp," ")) NO-ERROR.

          FIND FIRST B_Par_object 
               WHERE B_par_object.obj_Stream = pStream
                 AND B_par_object.pdf_id     = pID
                 AND B_par_object.obj_id     = par-obj
                 AND B_par_object.gen_id     = par-gen NO-ERROR.
          IF AVAIL B_par_object THEN DO:
            /* Bit of a hack to ensure that the Field ID is set to the current
              document page - don't have time to figure out why the parent
              would be set as Page 0 (since it only happens sometimes) */
            IF b_Par_object.page_id = 0 THEN
              b_Par_Object.page_id = doc-pages.
            par-page = B_par_object.page_id.
          END.
          ELSE
            par-page = 1.
        END.

        IF INDEX(text-ptr,"~/T(") > 0 THEN DO:
          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/T(") + 3).
          text-temp = SUBSTR(text-temp,1,INDEX(text-temp,")")).

          obj-name = REPLACE(text-temp,"(","").
          obj-name = TRIM(REPLACE(obj-name,")","")).
        END.

        IF INDEX(text-ptr,"~/DA") > 0  THEN DO:
          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/DA") + 3).
          text-temp = SUBSTR(text-temp,1,INDEX(text-temp,")")).

          obj-disp = REPLACE(text-temp,"(","").
          obj-disp = TRIM(REPLACE(obj-disp,")","")).
        END.

        IF INDEX(text-ptr,"~/DR") > 0  THEN DO:
          RUN ProcessDR (text-ptr).
        END.

        IF INDEX(text-ptr,"~/Kids") > 0  THEN DO:
          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/Kids") + 5).
          text-temp = SUBSTR(text-temp,1,INDEX(text-temp,"]")).
          text-temp = REPLACE(text-temp,"[","").
          text-temp = TRIM(REPLACE(text-temp,"]","")).

          RUN ProcessFieldKids (text-temp, obj-name, obj-type, obj-disp).
          L_HasKids = TRUE.
        END.

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
      END. /* While True */

      /* A text form field was found */
      IF NOT l_HasKids AND obj-type = "TX" THEN DO:

        CREATE TT_Widget.
        ASSIGN TT_widget.obj_stream  = pStream
               TT_widget.pdf_id      = pID
               TT_Widget.widget_name = obj-name
               TT_Widget.widget_type = obj-type
               TT_Widget.widget_rect = obj-rect
               TT_Widget.widget_disp = obj-disp
               TT_Widget.widget_page = par-page.

        /* used for debugging
        message "NAME=" obj-name SKIP 
                "RECT=" obj-rect SKIP
                "DISP=" obj-disp view-as alert-box.
        */
      END.

    END. /* FIRST */
  END. /* each Field Dictionary */

END. /* ProcessFIELDDictionary */

PROCEDURE ProcessFieldKids:
  DEFINE INPUT PARAMETER text-ptr AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER obj-name AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER obj-type AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER obj-disp AS CHARACTER NO-UNDO.


  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen     AS INTEGER NO-UNDO.

  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  curr-ptr = SEEK(INPUT).

  DO L_Loop = 1 TO NUM-ENTRIES(text-ptr," ") BY 3:
    ASSIGN L_Obj = INT(ENTRY(L_Loop, text-ptr, " "))
           L_Gen = INT(ENTRY(L_Loop + 1, text-ptr, " ")).

    RUN ProcessFieldDictionary (L_Obj, L_Gen, obj-name, obj-type, obj-disp).
  END.

  SEEK INPUT TO curr-ptr.

END. /* ProcessFieldKids */

PROCEDURE ProcessDR:
  DEFINE INPUT PARAMETER text-ptr AS CHARACTER NO-UNDO.

  DEFINE VARIABLE text-tmp  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Entries AS CHARACTER NO-UNDO.

  DEFINE VARIABLE curr-pos  AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_FontNbr AS INTEGER NO-UNDO INIT 1.

  DEFINE VARIABLE L_InFonts AS LOGICAL NO-UNDO.

  curr-pos = SEEK(INPUT).

  DO WHILE TRUE:

    IF NOT L_InFonts AND INDEX(text-ptr,"~/Font") > 0 THEN DO:
      text-tmp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Font"))).
      text-ptr = text-tmp.

      IF INDEX(text-ptr,">>") > 0 THEN DO:
        text-tmp = SUBSTR(text-tmp,1,INDEX(text-tmp,">>") - 1).
        L_InFonts = FALSE.
      END.

      ELSE
        L_InFonts = TRUE.
    END.
    
    ELSE IF L_InFonts THEN DO:

      IF INDEX(text-ptr,">>") > 0 THEN DO:
        text-ptr = SUBSTR(text-ptr,1,INDEX(text-ptr,">>") - 1).
        L_InFonts = FALSE.
      END.

      text-tmp = text-tmp + " " + text-ptr.

    END.

    text-ptr = ReadLine().

    IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
  END.

  /*
    RUN ProcessEntry (INPUT  999,
                      INPUT  999,
                      INPUT  "Font", 
                      INPUT  0, 
                      INPUT  text-tmp, 
                      OUTPUT L_Entries).
  */
  
  text-tmp = TRIM(REPLACE(text-tmp,"~/Font"," ")).
  text-tmp = TRIM(REPLACE(text-tmp,"<<","")).
  text-tmp = TRIM(REPLACE(text-tmp,">>","")).
  text-tmp = TRIM(REPLACE(text-tmp,"~/"," ~/")).
  text-tmp = TRIM(REPLACE(text-tmp,"  ~/"," ~/")).

  DO L_Loop = 1 TO NUM-ENTRIES(text-tmp," ") BY 4:
    ASSIGN L_Obj = INT(ENTRY(L_Loop + 1, text-tmp, " "))
           L_Gen = INT(ENTRY(L_Loop + 2, text-tmp, " ")) NO-ERROR.

    RUN OutputObject(L_Obj,L_Gen).

    CREATE TT_Resource.
    ASSIGN TT_Resource.obj_stream = pStream
           TT_Resource.pdf_id     = pID
           TT_Resource.par_Obj    = 0
           TT_Resource.par_gen    = 0
           TT_Resource.page_id    = 0
           TT_Resource.res_type   = "Font"
           TT_Resource.res_obj    = L_Obj
           TT_Resource.res_gen    = L_Gen
           TT_Resource.res_text   = "~/xTT" + STRING(L_FontNbr).
  
    L_FontNbr = L_FontNbr + 1.
    
    RUN ProcessFontDictionary (0, L_Obj, L_Gen, TT_Resource.res_Text). 

  END.
  
  SEEK INPUT TO curr-pos.

END. /* ProcessDR */

PROCEDURE ProcessPagesDictionary:
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_InKids  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE L_Kids    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen     AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Curr    AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.
  
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.

  L_Curr = SEEK(INPUT).

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pId
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      SEEK INPUT TO B_TT_Object.obj_ptr.
      RUN ProcessKids.

    END. /* FIRST */
  END. /* each Pages Dictionary */

  SEEK INPUT TO L_Curr.

END. /* ProcessPagesDictionary */

PROCEDURE ProcessKids:
  DEFINE VARIABLE L_InKids  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE L_Kids    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_ptr     AS INTEGER NO-UNDO.

  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  /* text-ptr = Readline(). */

  DO WHILE TRUE:
    text-ptr = readline().

    IF INDEX(text-ptr,"~/Kids") > 0 THEN DO:
      L_InKids = TRUE.

      /* L_Kids = REPLACE(text-ptr,"[",""). */
      L_Kids = SUBSTR(text-ptr,INDEX(text-ptr,"[") + 1).

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

    IF INDEX(text-ptr,"~/MediaBox") > 0 THEN DO:
      text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/MediaBox")).
      text-temp = REPLACE(text-temp,"/MediaBox","").
      text-temp = REPLACE(text-temp,"[","").
      text-temp = TRIM(REPLACE(text-temp,"]","")).

      /* If MediaBox is an Object then find the object and return the 
         MediaBox directives */
      IF ENTRY(3,text-temp," ") = "R" THEN DO:
        curr-ptr = SEEK(INPUT).
        RUN ProcessMediaBox (INPUT INT(ENTRY(1,text-temp," ") ),
                             INPUT INT(ENTRY(2,text-temp," ") ),
                             OUTPUT text-temp).
        SEEK INPUT TO curr-ptr.
      END.

      ASSIGN decDefMedia1 = DEC(ENTRY(1,text-temp, " "))
             decDefMedia2 = DEC(ENTRY(2,text-temp, " "))
             decDefMedia3 = DEC(ENTRY(3,text-temp, " "))
             decDefMedia4 = DEC(ENTRY(4,text-temp, " ")).

    END. /* MediaBox */

    IF INDEX(text-ptr,"~/CropBox") > 0 THEN DO:
      text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/CropBox")).
      text-temp = REPLACE(text-temp,"/CropBox","").
      text-temp = REPLACE(text-temp,"[","").
      text-temp = TRIM(REPLACE(text-temp,"]","")).

      /* If MediaBox is an Object then find the object and return the 
         MediaBox directives */
      IF ENTRY(3,text-temp," ") = "R" THEN DO:
        curr-ptr = SEEK(INPUT).
        RUN ProcessCropBox (INPUT INT(ENTRY(1,text-temp," ") ),
                            INPUT INT(ENTRY(2,text-temp," ") ),
                            OUTPUT text-temp).
        SEEK INPUT TO curr-ptr.
      END.

      ASSIGN decCrop1 = DEC(ENTRY(1,text-temp, " "))
             decCrop2 = DEC(ENTRY(2,text-temp, " "))
             decCrop3 = DEC(ENTRY(3,text-temp, " "))
             decCrop4 = DEC(ENTRY(4,text-temp, " ")).

    END. /* CropBox */

    IF INDEX(text-ptr,"~/Resources") > 0 THEN DO:
      text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/Resources")).
      ASSIGN L_obj = INT(ENTRY(2, text-temp, " "))
             L_gen = INT(ENTRY(3, text-temp, " ")) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN NEXT.

      curr-ptr = SEEK(INPUT).
      RUN ProcessResourceDictionary (L_obj, L_gen, 0).
      SEEK INPUT TO curr-ptr.

    END.

    IF INDEX(text-ptr,">>") > 0 THEN LEAVE.
  END.

  L_Kids = TRIM(REPLACE(L_Kids,"~/Kids"," ")).

  IF INDEX(L_Kids,"~/") > 0 THEN
    L_Kids = SUBSTR(L_Kids,1,INDEX(L_Kids,"~/") - 1).

  DO L_Loop = 1 TO NUM-ENTRIES(L_Kids," ") BY 3:
    ASSIGN L_Obj = INT(ENTRY(L_Loop, L_Kids, " "))
           L_Gen = INT(ENTRY(L_Loop + 1, L_Kids, " ")).

    RUN ProcessPageDictionary (L_Obj, L_Gen).
  END.

END. /* ProcessKids */

PROCEDURE ProcessMediaBox:
  DEFINE INPUT  PARAMETER pObject   AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pGen      AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pMediaBox AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.

  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      SEEK INPUT TO B_TT_Object.obj_ptr.
      DO WHILE TRUE:
        text-ptr = readline().
        IF TRIM(text-ptr) BEGINS "[" THEN
          ASSIGN pMediaBox = REPLACE( text-ptr, "[", "")
                 pMediaBox = REPLACE( pMediaBox, "]", "")
                 pMediaBox = TRIM(pMediaBox).

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
      END.
    END.
  END.

END.  /* ProcessMediaBox */

PROCEDURE ProcessCropBox:
  DEFINE INPUT  PARAMETER pObject   AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pGen      AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pCropBox  AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.

  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      SEEK INPUT TO B_TT_Object.obj_ptr.
      DO WHILE TRUE:
        text-ptr = readline().
        IF TRIM(text-ptr) BEGINS "[" THEN
          ASSIGN pCropBox = REPLACE( text-ptr, "[", "")
                 pCropBox = REPLACE( pCropBox, "]", "")
                 pCropBox = TRIM(pCropBox).

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
      END.
    END.
  END.

END.  /* ProcessCropBox */

PROCEDURE ProcessPAGEDictionary:
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.
  
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.
  DEFINE VARIABLE obj-type  AS CHARACTER NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      SEEK INPUT TO B_TT_Object.obj_ptr.
      /* text-ptr = Readline(). */

      DO WHILE TRUE:
        text-ptr = readline().

        IF INDEX(text-ptr,"~/Type") > 0  THEN DO:
          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/Type")).
          obj-type = TRIM(ENTRY(3, text-temp, "~/")).
          obj-type = REPLACE(obj-type,">>","").

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
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Entries AS CHARACTER NO-UNDO.
  DEFINE VARIABLE Contents  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.
  DEFINE VARIABLE iRotate   AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE obj-cnt   AS INTEGER NO-UNDO.
  DEFINE VARIABLE res-obj   AS INTEGER NO-UNDO. /* Resources */
  DEFINE VARIABLE res-gen   AS INTEGER NO-UNDO.
  DEFINE VARIABLE con-obj   AS INTEGER NO-UNDO. /* Contents */
  DEFINE VARIABLE con-gen   AS INTEGER NO-UNDO.

  ASSIGN decMedia1 = 0
         decMedia2 = 0
         decMedia3 = 0
         decMedia4 = 0
        
         decCrop1  = 0
         decCrop2  = 0
         decCrop3  = 0
         decCrop4  = 0.

  FOR EACH B_TT_Object WHERE B_TT_object.obj_stream = pStream
                         AND B_TT_object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      B_TT_Object.page_id = pPage.

      SEEK INPUT TO B_TT_Object.obj_ptr.
      /* text-ptr = Readline(). */

      DO WHILE TRUE:
        text-ptr = readline().

        IF INDEX(text-ptr,"~/MediaBox") > 0 THEN DO:
          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/MediaBox")).
          text-temp = REPLACE(text-temp,"/MediaBox","").
          text-temp = REPLACE(text-temp,"[","").
          text-temp = TRIM(REPLACE(text-temp,"]","")).

          /* If MediaBox is an Object then find the object and return the 
             MediaBox directives */
          IF ENTRY(3,text-temp," ") = "R" THEN DO:
            curr-ptr = SEEK(INPUT).
            RUN ProcessMediaBox (INPUT INT(ENTRY(1,text-temp," ") ),
                                 INPUT INT(ENTRY(2,text-temp," ") ),
                                 OUTPUT text-temp).
            SEEK INPUT TO curr-ptr.
          END.

          ASSIGN decMedia1 = DEC(ENTRY(1,text-temp, " "))
                 decMedia2 = DEC(ENTRY(2,text-temp, " "))
                 decMedia3 = DEC(ENTRY(3,text-temp, " "))
                 decMedia4 = DEC(ENTRY(1,REPLACE(ENTRY(4,text-temp, " "),"/"," ")," ")).

        END. /* MediaBox */

        IF INDEX(text-ptr,"~/CropBox") > 0 THEN DO:
          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/CropBox")).
          text-temp = REPLACE(text-temp,"/CropBox","").
          text-temp = REPLACE(text-temp,"[","").
          text-temp = TRIM(REPLACE(text-temp,"]","")).

          /* If CropBox is an Object then find the object and return the 
             CropBox directives */
          IF ENTRY(3,text-temp," ") = "R" THEN DO:
            curr-ptr = SEEK(INPUT).
            RUN ProcessCropBox (INPUT INT(ENTRY(1,text-temp," ") ),
                                INPUT INT(ENTRY(2,text-temp," ") ),
                                OUTPUT text-temp).
            SEEK INPUT TO curr-ptr.
          END.

          ASSIGN decCrop1 = DEC(ENTRY(1,text-temp, " "))
                 decCrop2 = DEC(ENTRY(2,text-temp, " "))
                 decCrop3 = DEC(ENTRY(3,text-temp, " "))
                 decCrop4 = DEC(ENTRY(1,REPLACE(ENTRY(4,text-temp, " "),"/"," ")," ")).

        END. /* CropBox */

        IF INDEX(text-ptr,"~/Rotate") > 0  THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Rotate"))).
          text-temp = TRIM(REPLACE(text-temp,"~/Rotate","")).

          IF INDEX(text-temp,"~/") > 0 THEN
            iRotate = INT(TRIM(SUBSTR(text-temp,1,INDEX(text-temp,"~/") - 1))).
          ELSE
            iRotate = INT(text-temp) NO-ERROR.

          B_TT_Object.Rotate = iRotate.
        END.

        IF INDEX(text-ptr,"~/Font") > 0  THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Font"))).
          curr-ptr = SEEK(INPUT).
          RUN ProcessEntry (INPUT pObject,
                            INPUT pGen,
                            INPUT "Font", 
                            INPUT pPage, 
                            INPUT text-temp, 
                            OUTPUT L_Entries).

          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,"~/Resources") > 0  THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Resources"))).

          IF INDEX(text-temp, "~/Resources<<") = 0 
          AND INDEX(text-temp, "~/Resources <<") = 0 THEN DO:
            ASSIGN res-obj = INT(ENTRY(2, text-temp, " "))
                   res-gen = INT(ENTRY(3, text-temp, " ")) NO-ERROR.

            IF ERROR-STATUS:ERROR THEN NEXT.

            curr-ptr = SEEK(INPUT).
            RUN ProcessResourceDictionary (res-obj, res-gen, pPage).
            SEEK INPUT TO curr-ptr.
          END.
        END.

        IF INDEX(text-ptr,"~/Contents") > 0  THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Contents"))).
          contents = REPLACE(text-temp,"~/Contents","").
          IF INDEX(contents,"~/") > 0 THEN
            contents = SUBSTR(contents,1,INDEX(contents,"~/") - 1).

          contents = REPLACE(contents,"[","").
          contents = REPLACE(contents,"]","").
          contents = TRIM(contents).

          /* This is a temporary holder for the page content */
          OS-DELETE VALUE(SESSION:TEMP-DIR + "filter.txt").

          DO obj-cnt = 1 TO NUM-ENTRIES(contents," ") BY 3:

            ASSIGN con-obj = INT(ENTRY(obj-cnt, contents, " "))
                   con-gen = INT(ENTRY(obj-cnt + 1, contents, " ")) NO-ERROR.

            IF ERROR-STATUS:ERROR THEN NEXT.

            curr-ptr = SEEK(INPUT).
            RUN ProcessPageContent (obj-cnt, con-obj, con-gen, pPage).
            SEEK INPUT TO curr-ptr.

          END.

          /* Again, we don't need it so make sure it's gone */
          OS-DELETE VALUE(SESSION:TEMP-DIR + "filter.txt").
        END.

        IF INDEX(text-ptr,"~/Xobject") > 0  THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/XObject"))).
          curr-ptr = SEEK(INPUT).
          RUN ProcessEntry (INPUT pObject,
                            INPUT pGen,
                            INPUT "XObject", 
                            INPUT pPage, 
                            INPUT text-temp, 
                            OUTPUT L_Entries).

          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,"~/ExtGState") > 0  THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/ExtGState"))).
          curr-ptr = SEEK(INPUT).
          RUN ProcessEntry (INPUT pObject,
                            INPUT pGen,
                            INPUT "ExtGState", 
                            INPUT pPage, 
                            INPUT text-temp, 
                            OUTPUT L_Entries).

          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,"~/ColorSpace") > 0  THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/ColorSpace"))).

          curr-ptr = SEEK(INPUT).
          RUN ProcessEntry (INPUT pObject,
                            INPUT pGen,
                            INPUT "ColorSpace", 
                            INPUT pPage, 
                            INPUT text-temp, 
                            OUTPUT L_Entries).

          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,"~/Shading") > 0  THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Shading"))).
          curr-ptr = SEEK(INPUT).
          RUN ProcessEntry (INPUT pObject,
                            INPUT pGen,
                            INPUT "Shading", 
                            INPUT pPage, 
                            INPUT text-temp, 
                            OUTPUT L_Entries).

          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.

      END.
      
      IF decMedia1 = 0 AND decMedia2 = 0 AND decMedia3 = 0 AND decMedia4 = 0
      THEN
        ASSIGN B_TT_Object.obj_media1 = decDefMedia1
               B_TT_Object.obj_media2 = decDefMedia2
               B_TT_Object.obj_media3 = decDefMedia3
               B_TT_Object.obj_media4 = decDefMedia4.
      ELSE
        ASSIGN B_TT_Object.obj_media1 = decMedia1
               B_TT_Object.obj_media2 = decMedia2
               B_TT_Object.obj_media3 = decMedia3
               B_TT_Object.obj_media4 = decMedia4.

      ASSIGN B_TT_Object.obj_crop1 = decCrop1
             B_TT_Object.obj_crop2 = decCrop2
             B_TT_Object.obj_crop3 = decCrop3
             B_TT_Object.obj_crop4 = decCrop4.


    END. /* FIRST */
  END. /* each Pages Dictionary */

END. /* ProcessPageObject */

PROCEDURE ProcessResourceDictionary:

  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pPage    AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Array   AS LOGICAL NO-UNDO INIT FALSE.
  DEFINE VARIABLE L_InFonts AS LOGICAL NO-UNDO.
  DEFINE VARIABLE L_Entries AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.
  
  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen     AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_ptr     AS INTEGER NO-UNDO.
  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      ASSIGN B_TT_Object.obj_type = "~/Resource"
             B_TT_Object.page_id  = pPage.

      SEEK INPUT TO B_TT_Object.obj_ptr.
      /* text-ptr = Readline(). */

      DO WHILE TRUE:
        text-ptr = readline().

        IF INDEX(text-ptr,"~/Font") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Font"))).
          curr-ptr = SEEK(INPUT).
          RUN ProcessEntry (INPUT pObject,
                            INPUT pGen,
                            INPUT "Font", 
                            INPUT pPage, 
                            INPUT text-temp, 
                            OUTPUT L_Entries).
          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,"~/XObject") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/XObject"))).
          curr-ptr = SEEK(INPUT).
          RUN ProcessEntry (INPUT pObject,
                            INPUT pGen,
                            INPUT "XObject", 
                            INPUT pPage, 
                            INPUT text-temp, 
                            OUTPUT L_Entries).
          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,"~/ExtGState") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/ExtGState"))).
          curr-ptr = SEEK(INPUT).
          RUN ProcessEntry (INPUT pObject,
                            INPUT pGen,
                            INPUT "ExtGState", 
                            INPUT pPage, 
                            INPUT text-temp, 
                            OUTPUT L_Entries).
          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,"~/ColorSpace") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/ColorSpace"))).
          curr-ptr = SEEK(INPUT).
          
          RUN ProcessEntry (INPUT pObject,
                            INPUT pGen,
                            INPUT "ColorSpace", 
                            INPUT pPage, 
                            INPUT text-temp, 
                            OUTPUT L_Entries).
          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,"~/Shading") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Shading"))).
          curr-ptr = SEEK(INPUT).
          RUN ProcessEntry (INPUT pObject,
                            INPUT pGen,
                            INPUT "Shading", 
                            INPUT pPage, 
                            INPUT text-temp, 
                            OUTPUT L_Entries).
          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.

      END. /* While True */

    END. /* FIRST */
  END. /* each Pages Dictionary */

END. /* ProcressResourceDictionary */

PROCEDURE ProcessEntry:
  DEFINE INPUT  PARAMETER pObject    AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pGen       AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pType      AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pPage      AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pLine      AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pEntry     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Loop   AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Obj    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Gen    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Incr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE orig-ptr AS INTEGER NO-UNDO.
  DEFINE VARIABLE l_Length AS INTEGER NO-UNDO.

  DEFINE VARIABLE l_Array  AS LOGICAL NO-UNDO INIT FALSE.

  orig-ptr = SEEK(INPUT).

  pLine = SUBSTR(pLine,INDEX(pLine,"~/" + pType)).
  pLine = REPLACE(pLine,"[","").
  pLine = TRIM(REPLACE(pLine,"~/" + pType,"")).

  DO WHILE TRUE:
    IF INDEX(pLine,"]") > 0 
    OR INDEX(pLine,">>") > 0 
    OR INDEX(pLine,"<<") > 0 THEN DO:
      l_Array = TRUE.
      IF INDEX(pLine,"]") > 0 THEN
        pEntry = pEntry + " " + SUBSTR(pLine,1,INDEX(pLine,"]") - 1).
      ELSE IF INDEX(pLine,">>") > 9 THEN
        pEntry = pEntry + " " + SUBSTR(pLine,1,INDEX(pLine,">>") + 1).
      ELSE
        pEntry = pEntry + " " + pLine.

      /* LEAVE. */
    END.
    ELSE
      pEntry = pEntry + " " + pLine.
    
    IF NOT L_Array AND INDEX(pLine,"~/") > 0
    OR INDEX(pLine,">>") > 0 THEN DO:
     LEAVE.
    END.

    pLine = readline().

  END. /* While True */

  /* Remove stuff after the >> indicator */
  IF INDEX(pEntry,">>") > 0 THEN
    pEntry = ENTRY(1,pEntry,">>").

  Entry4 = "".  /* Clear this variable before we re-use it */

  pEntry = REPLACE(pEntry,"<<","").
  pEntry = TRIM(REPLACE(pEntry,">>","")).
  pEntry = TRIM(REPLACE(pEntry,"~/" + pType,"")).

  /* At this point, check to see if they are using a Font object or the a 
     reference to a font dictionary.

      eg: /TTF1 9 0 R      or
          9 0 R

      If '9 0 R' then they are referencing a font dictionary so we need to 
      get the list of fonts from there */
  IF SUBSTR(TRIM(pEntry),1,1) <> "~/" THEN DO:
    ASSIGN L_Obj = INT(ENTRY(1, pEntry, " "))
           L_Gen = INT(ENTRY(2, pEntry, " ")) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      RUN GetListOfFonts(L_Obj, L_Gen, OUTPUT pEntry).
  END.

  pEntry = TRIM(REPLACE(pEntry,"~/"," ")).
  pEntry = REPLACE(pEntry,"  "," ").  /* This removes double-spaces */

  IF L_Array THEN DO L_Loop = 1 TO NUM-ENTRIES(pEntry," ") BY 4:

    ASSIGN L_Obj = INT(ENTRY(L_Loop + 1, pEntry, " "))
           L_Gen = INT(ENTRY(L_Loop + 2, pEntry, " ")) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN NEXT.

    /* Skip resources that have already been added */
    IF CAN-FIND(FIRST tt_Resource
                WHERE tt_Resource.obj_stream = pStream
                  AND tt_Resource.pdf_id     = pID
                  AND tt_Resource.page_id    = pPage /* igc - 10/21/05 */
                  AND tt_Resource.res_text   = (IF pType = "Xobject" THEN
                                      "~/" + pID + ENTRY(L_Loop, pEntry, " ")
                                    ELSE 
                                      "~/" + ENTRY(L_Loop, pEntry, " ") )
                  AND tt_Resource.res_type   = pType
                  AND tt_Resource.res_obj    = L_Obj
                  AND tt_Resource.res_gen    = L_Gen NO-LOCK) THEN NEXT.

    RUN OutputObject(L_Obj,L_Gen).

    CREATE TT_Resource.
    ASSIGN TT_Resource.obj_stream = pStream
           TT_Resource.pdf_id     = pID
           TT_Resource.par_Obj    = pObject
           TT_Resource.par_gen    = pGen
           TT_Resource.page_id    = pPage
           TT_Resource.res_type   = pType
           TT_Resource.res_obj    = L_Obj
           TT_Resource.res_gen    = L_Gen
           TT_Resource.res_text   = IF pType = "Xobject" THEN
                                      "~/" + pID + ENTRY(L_Loop, pEntry, " ")
                                    ELSE 
                                      "~/" + ENTRY(L_Loop, pEntry, " ")
           TT_Resource.res_old    = IF pType = "Xobject" THEN
                                      "~/" + ENTRY(L_Loop,pEntry, " ")
                                    ELSE "".

    CASE pType:
      WHEN "Font" THEN
        RUN ProcessFontDictionary (pPage, L_Obj, L_Gen, TT_Resource.res_text).
      WHEN "ColorSpace" THEN
        RUN ProcessColorSpaceDictionary (pPage, L_Obj, L_Gen).
      WHEN "Shading" THEN
        RUN ProcessShadingDictionary (pPage, L_Obj, L_Gen).
      WHEN "XObject" THEN
        RUN ProcessXobjectDictionary (pPage, pObject, pGen, L_Obj, L_Gen, OUTPUT L_Length).
      WHEN "ExtGState" THEN
        RUN ProcessExtGStateDictionary (pPage, L_Obj, L_Gen).
    END CASE.

    TT_Resource.res_len = l_Length.
  END.
  ELSE DO:
    ASSIGN L_Obj = INT(ENTRY(1, pEntry, " "))
           L_Gen = INT(ENTRY(2, pEntry, " ")).

    CREATE TT_Resource.
    ASSIGN TT_Resource.obj_stream = pStream
           TT_Resource.pdf_id     = pID
           TT_Resource.par_Obj    = pObject
           TT_Resource.par_gen    = pGen
           TT_Resource.page_id    = pPage
           TT_Resource.res_type   = pType
           TT_Resource.res_obj    = L_Obj
           TT_Resource.res_gen    = L_Gen
           TT_Resource.res_text   = "".

    CASE pType:
      WHEN "Font" THEN
        RUN ProcessFontDictionary (pPage, L_Obj, L_Gen,TT_Resource.res_text).
      WHEN "ColorSpace" THEN
        RUN ProcessColorSpaceDictionary (pPage, L_Obj, L_Gen).
      WHEN "Shading" THEN
        RUN ProcessShadingDictionary (pPage, L_Obj, L_Gen).
      WHEN "XObject" THEN
        RUN ProcessXObjectDictionary (pPage, pObject, pGen, L_Obj, L_Gen, OUTPUT L_Length).
    END CASE.

    TT_Resource.res_len = L_Length.
  END.

  SEEK INPUT TO orig-ptr.

END. /* ProcessEntry */

PROCEDURE GetListOfFonts:
  DEFINE INPUT  PARAMETER pObj AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pGen AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pFonts  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Array   AS LOGICAL NO-UNDO.

  curr-ptr = SEEK(INPUT).

  DEFINE BUFFER B_TT_object FOR TT_object.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObj
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:
      SEEK INPUT TO B_TT_Object.obj_ptr.
      /* text-ptr = Readline(). /* Skip object number */ */

      DO WHILE TRUE:
        text-ptr = ReadLine().

       IF INDEX(text-ptr,"]") > 0 
       OR INDEX(text-ptr,">>") > 0 
       OR INDEX(text-ptr,"<<") > 0 THEN DO:
         l_Array = TRUE.
         IF INDEX(text-ptr,"]") > 0 THEN
           pFonts = pFonts + " " + SUBSTR(text-ptr,1,INDEX(text-ptr,"]") - 1).
         ELSE IF INDEX(text-ptr,">>") > 9 THEN
           pFonts = pFonts + " " + SUBSTR(pFonts,1,INDEX(pFonts,">>") + 1).
         ELSE
           pFonts = pFonts + " " + text-ptr.

         /* LEAVE. */
       END.
       ELSE
         pFonts = pFonts + " " + text-ptr.
    
       IF NOT L_Array AND INDEX(text-ptr,"~/") > 0
       OR INDEX(text-ptr,">>") > 0 THEN DO:
        LEAVE.
       END.
     END. /* While True */

    END. /* First */
  END.

  SEEK INPUT TO curr-ptr.
END.

PROCEDURE ProcessFontDictionary:
  DEFINE INPUT PARAMETER pPage    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pTag     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Entries   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-ptr    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fonts       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE widths      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE base-font   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE font_name   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE descriptor  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE fontlist    AS LOGICAL NO-UNDO INIT FALSE.
  DEFINE VARIABLE i_Font      AS INTEGER NO-UNDO.

  DEFINE VARIABLE obj-ptr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE gen-ptr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE enc-obj   AS INTEGER NO-UNDO.
  DEFINE VARIABLE enc-gen   AS INTEGER NO-UNDO.
  DEFINE VARIABLE uni-obj   AS INTEGER NO-UNDO.
  DEFINE VARIABLE uni-gen   AS INTEGER NO-UNDO.
  DEFINE VARIABLE descend-obj   AS INTEGER NO-UNDO.
  DEFINE VARIABLE descend-gen   AS INTEGER NO-UNDO.
  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.

  DEFINE VARIABLE cEntry4     AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_object   FOR TT_object.
  DEFINE BUFFER B_TT_Resource FOR TT_Resource.

  /* igc - removed this code since PDFinclude wasn't getting Base14 fonts from
           imported PDF - don't know why though?

    If the Font has already been processed then don't do it again 
  IF CAN-FIND(FIRST B_TT_Object
              WHERE B_TT_Object.obj_stream = pStream
                AND B_TT_Object.pdf_id = pID
                AND B_TT_Object.obj_id = pObject
                AND B_TT_Object.gen_id = pGen
                AND B_TT_Object.obj_type = "~/Font" NO-LOCK)
  THEN RETURN.
  */

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      B_TT_Object.obj_type = "~/Font".

      SEEK INPUT TO B_TT_Object.obj_ptr.
      /* text-ptr = Readline(). /* Skip object number */ */

      DO WHILE TRUE:
        text-ptr = readline().
        
        IF INDEX(text-ptr,"~/FontDescriptor") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/FontDescriptor"))).

          ASSIGN obj-ptr = INT(ENTRY(2, text-temp, " "))
                 gen-ptr = INT(ENTRY(3, text-temp, " ")) NO-ERROR.

          descriptor = TRUE.
          curr-ptr = SEEK(INPUT).
          RUN ProcessFontDescriptorDictionary (pTag, pObject, pGen, pPage, obj-ptr, gen-ptr, enc-obj, enc-gen, OUTPUT base-font).
          SEEK INPUT TO curr-ptr.
        END.

        IF INDEX(text-ptr,"~/ToUnicode") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/ToUnicode"))).

          ASSIGN uni-obj = INT(ENTRY(2, text-temp, " "))
                 uni-gen = INT(ENTRY(3, text-temp, " ")) NO-ERROR.

          IF uni-obj > 0 THEN DO:
            curr-ptr = SEEK(INPUT).
            RUN ProcessDictionary (uni-obj, uni-gen).
            SEEK INPUT TO curr-ptr.
          END.
        END.

        IF INDEX(text-ptr,"~/DescendantFonts") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/DescendantFonts"))).
          text-temp = REPLACE(text-temp,"~/DescendantFonts","").
          text-temp = REPLACE(text-temp,"["," ").
          text-temp = TRIM(text-temp).

          ASSIGN descend-obj = INT(ENTRY(1, text-temp, " "))
                 descend-gen = INT(ENTRY(2, text-temp, " ")) NO-ERROR.

          IF descend-obj > 0 THEN DO:
            curr-ptr = SEEK(INPUT).
            RUN OutputObject(descend-obj,descend-gen).

            CREATE TT_Resource.
            ASSIGN TT_Resource.obj_stream = pStream
                   TT_Resource.pdf_id     = pID
                   TT_Resource.par_Obj    = pObject
                   TT_Resource.par_gen    = pGen
                   TT_Resource.page_id    = pPage
                   TT_Resource.res_type   = "DescendantFont"
                   TT_Resource.res_obj    = descend-obj
                   TT_Resource.res_gen    = descend-gen
                   TT_Resource.res_text   = pTag.
            
            RUN ProcessFontDictionary (pPage, descend-obj, descend-gen, pTag). 

            SEEK INPUT TO curr-ptr.
          END.
        END.

        IF INDEX(text-ptr,"~/Encoding") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Encoding"))).

          ASSIGN enc-obj = INT(ENTRY(2, text-temp, " "))
                 enc-gen = INT(ENTRY(3, text-temp, " ")) NO-ERROR.

          IF enc-obj <> 0 THEN
            RUN OutputObject(enc-obj,enc-gen).
        END.

        IF INDEX(text-ptr,"~/Widths") > 0 THEN DO:

          RUN ProcessFontWidths (text-ptr, OUTPUT Widths).
        END.

        IF base-font = "" 
        AND INDEX(text-ptr,"~/BaseFont") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/BaseFont"))).

          cEntry4 = ENTRY(4, text-temp, " ") NO-ERROR.
          IF cEntry4 = "R" THEN DO:
            curr-ptr = SEEK(INPUT).
            RUN ProcessBaseFont( INPUT  INT( ENTRY(2, text-temp, " ") ),
                                 INPUT  INT( ENTRY(3, text-temp, " ") ),
                                 OUTPUT base-font ).
            SEEK INPUT TO curr-ptr.
          END.
          ELSE
            base-font = ENTRY(3, text-temp, "/").
        END.

        IF INDEX(text-ptr,"~/Name") > 0  THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Name"))).

          font_name = ENTRY(3,text-temp,"~/").
        END.

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
      END. /* True */
    END. /* First */
  END. /* each */

  IF NOT descriptor THEN DO: 
    CREATE TT_Font.
    ASSIGN TT_Font.obj_stream  = pStream
           TT_Font.pdf_id      = pID
           TT_Font.obj_id      = pObject
           TT_Font.gen_id      = pGen
           TT_Font.page_id     = pPage
           TT_Font.font_name   = font_name
           TT_Font.font_base   = base-font
           TT_Font.font_tag    = pTag
           TT_Font.enc_obj     = enc-obj
           TT_Font.enc_gen     = enc-gen
           TT_Font.uni_obj     = uni-obj
           TT_Font.uni_gen     = uni-gen
           TT_Font.descend_obj = descend-obj
           TT_Font.descend_gen = descend-gen
           TT_Font.font_width  = Widths.

  END.
  ELSE DO:
    FIND FIRST TT_Font WHERE TT_Font.obj_stream = pStream
                         AND TT_Font.pdf_id     = pID
                         AND TT_Font.font_name  = font_name 
                         AND TT_Font.page_id    = pPage NO-ERROR.
    IF AVAIL TT_Font THEN
      ASSIGN TT_Font.font_width   = Widths. /*
             TT_Font.uni_obj      = IF uni-obj <> 0 THEN uni-obj ELSE TT_Font.uni_obj
             TT_Font.uni_gen      = uni-gen
             TT_Font.descend_obj  = IF descend-obj <> 0 THEN descend-obj ELSE TT_Font.descend_obj
             TT_Font.descend_gen  = descend-gen. */
  END.
  
END. /* ProcessFontDictionary */

PROCEDURE ProcessBaseFont:
  DEFINE INPUT  PARAMETER pObject   AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pGen      AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pBaseFont AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.

  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      SEEK INPUT TO B_TT_Object.obj_ptr.

      DO WHILE TRUE:
        text-ptr = readline().
       IF TRIM(text-ptr) BEGINS "~/" THEN
          ASSIGN pBaseFont = SUBSTR(text-ptr,2).

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
      END.
    END.
  END.

END.  /* ProcessBaseFont */

PROCEDURE ProcessFontWidths:
  DEFINE INPUT  PARAMETER text-ptr AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER widths   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE curr-pos  AS INTEGER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_InWidths  AS LOGICAL NO-UNDO.

  curr-pos = SEEK(INPUT).

  DO WHILE TRUE:

    IF NOT L_InWidths AND INDEX(text-ptr,"~/Widths") > 0 THEN DO:
      Widths = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Widths"))).
      text-ptr = Widths.

      IF INDEX(text-ptr,"]") > 0 THEN DO:
        Widths = SUBSTR(Widths,1,INDEX(Widths,"]") - 1).
        L_InWidths = FALSE.
      END.

      ELSE
        L_InWidths = TRUE.
    END.
    
    ELSE IF L_InWidths THEN DO:

      IF INDEX(text-ptr,"]") > 0 THEN DO:
        text-ptr = SUBSTR(text-ptr,1,INDEX(text-ptr,"]") - 1).
        L_InWidths = FALSE.
      END.

      Widths = Widths + " " + text-ptr.

    END.

    text-ptr = ReadLine().

    IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
  END.

  Widths = REPLACE(Widths,"~/Widths","").
  Widths = TRIM(REPLACE(Widths,"[","")).

  SEEK INPUT TO curr-pos.
END.

PROCEDURE ProcessFontDescriptorDictionary:
  
  DEFINE INPUT  PARAMETER pTag        AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pParObj     AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pParGen     AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pParPage    AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pObject     AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pGen        AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pEncObject  AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pEncGen     AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pFontName   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_Object.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      /* This was originally created when the /FontName was found but in some
         instances the FontFile came before the FontName therefore this was
         causing issues when trying to update the Font_file indicators */
      CREATE TT_Font.
      ASSIGN TT_Font.obj_stream = pStream
             TT_Font.pdf_id     = pID
             TT_Font.obj_id     = pParObj
             TT_Font.gen_id     = pParGen
             TT_Font.page_id    = pParPage
             TT_Font.desc_obj   = pObject
             TT_Font.desc_gen   = pGen
             TT_Font.font_tag   = pTag
             TT_Font.enc_obj    = pEncObject
             TT_Font.enc_gen    = PEncGen.

      SEEK INPUT TO B_TT_Object.obj_ptr.
      /* text-ptr = Readline(). */

      DO WHILE TRUE:
        text-ptr = readline().

        IF INDEX(text-ptr,"~/FontName") > 0  THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/FontName"))).

          IF ENTRY(4, text-temp, " ") = "R" THEN DO:
            curr-ptr = SEEK(INPUT).

            RUN ProcessFontName (INPUT  INT( ENTRY(2,text-temp," ") ),
                                 INPUT  INT( ENTRY(3,text-temp," ") ),
                                 OUTPUT tt_Font.Font_name ).
            SEEK INPUT TO curr-ptr.
          END.

          ELSE 
            TT_Font.font_name  = ENTRY(3,text-temp,"~/").

           pFontName = TT_Font.Font_name.

            RUN OutputObject(pObject,pGen).
        END.

        IF INDEX(text-ptr,"~/FontFile2") > 0 THEN DO:

          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/FontFile2")).

          RUN OutputFile(ENTRY(2,text-temp," "),
                         ENTRY(3,text-temp," ")).

          ASSIGN TT_Font.file2_obj = INT(ENTRY(2,text-temp," "))
                 TT_Font.file2_gen = INT(ENTRY(3,text-temp," ")).
        END.

        IF INDEX(text-ptr,"~/FontFile3") > 0 THEN DO:

          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"~/FontFile3")).

          RUN OutputFile(ENTRY(2,text-temp," "),
                         ENTRY(3,text-temp," ")).

          ASSIGN TT_Font.file3_obj = INT(ENTRY(2,text-temp," "))
                 TT_Font.file3_gen = INT(ENTRY(3,text-temp," ")).
        END.

        IF INDEX(text-ptr,">>") > 0 THEN LEAVE.
      END.

    END. /* FIRST */
  END. /* for each  */

END. /* ProcessFontDescriptorDictionary */

PROCEDURE ProcessFontName:
  DEFINE INPUT  PARAMETER pObject   AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pGen      AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pFontName AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.

  DEFINE VARIABLE text-ptr  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp AS CHARACTER NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      SEEK INPUT TO B_TT_Object.obj_ptr.

      DO WHILE TRUE:
        text-ptr = readline().
       IF TRIM(text-ptr) BEGINS "~/" THEN
          ASSIGN pFontName = SUBSTR(text-ptr,2).

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
      END.
    END.
  END.

END.  /* ProcessFontName */

PROCEDURE ProcessDictionary:
  
  DEFINE INPUT  PARAMETER pObject     AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pGen        AS INTEGER NO-UNDO.

  RUN OutputFile(pObject,
                 pGen).

END. /* ProcessDictionary */

PROCEDURE ProcessColorSpaceDictionary:
  DEFINE INPUT PARAMETER pPage    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-ptr    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE base-font   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Entries   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE descriptor  AS LOGICAL NO-UNDO.

  DEFINE VARIABLE obj-ptr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE gen-ptr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_object FOR TT_object.

  /* If the ColorSpace has already been processed then don't do it again */
  IF CAN-FIND(FIRST B_TT_Object
              WHERE B_TT_Object.obj_stream = pStream
                AND B_TT_Object.pdf_id     = pID
                AND B_TT_Object.obj_id     = pObject
                AND B_TT_Object.gen_id     = pGen
                AND B_TT_Object.obj_type   = "~/ColorSpace" NO-LOCK)
  THEN RETURN.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      B_TT_Object.obj_type = "~/ColorSpace".

      SEEK INPUT TO B_TT_Object.obj_ptr.
      /* text-line = Readline(). */

      DO WHILE TRUE:
        text-ptr = readline().

        IF INDEX(text-ptr,"~/Separation") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Separation"))).

          /* Output Separation ColorSpace */
          ASSIGN obj-ptr = INT(ENTRY(3, text-temp, " "))
                 gen-ptr = INT(ENTRY(4, text-temp, " ")) NO-ERROR.

          IF NOT ERROR-STATUS:ERROR THEN DO:
            /* Skip resources that have already been added */
            IF NOT CAN-FIND(FIRST tt_Resource
                            WHERE tt_Resource.obj_stream = pStream
                              AND tt_Resource.pdf_id     = pID
                              AND tt_Resource.page_id    = pPage 
                              AND tt_Resource.res_text   = ""
                              AND tt_Resource.res_type   = "ColorSpace"
                              AND tt_Resource.res_obj    = obj-ptr
                              AND tt_Resource.res_gen    = gen-ptr NO-LOCK) 
            THEN DO:

              RUN OutputFile(obj-ptr,
                             gen-ptr).

              CREATE TT_Resource.
              ASSIGN TT_Resource.obj_stream = pStream
                     TT_Resource.pdf_id     = pID
                     TT_Resource.par_Obj    = pObject
                     TT_Resource.par_gen    = pGen
                     TT_Resource.page_id    = pPage
                     TT_Resource.res_type   = "SeparationColorSpace"
                     TT_Resource.res_obj    = obj-ptr
                     TT_Resource.res_gen    = gen-ptr
                     TT_Resource.res_text   = "".
            END.
          
            /* Output Separation ColorSpace */
            ASSIGN obj-ptr = INT(ENTRY(6, text-temp, " "))
                   gen-ptr = INT(ENTRY(7, text-temp, " ")).

            RUN OutputFile(obj-ptr,
                           gen-ptr).

            CREATE TT_Resource.
            ASSIGN TT_Resource.obj_stream = pStream
                   TT_Resource.pdf_id     = pID
                   TT_Resource.par_Obj    = pObject
                   TT_Resource.par_gen    = pGen
                   TT_Resource.page_id    = pPage
                   TT_Resource.res_type   = "SeparationColorSpace"
                   TT_Resource.res_obj    = obj-ptr
                   TT_Resource.res_gen    = gen-ptr
                   TT_Resource.res_text   = "".

          END. /* Not Error */

        END. /* Separation */

        IF INDEX(text-ptr,"~/ICCBased") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/ICCBased"))).
          ASSIGN obj-ptr = INT(ENTRY(2, text-temp, " "))
                 gen-ptr = INT(ENTRY(3, text-temp, " ")).

          RUN OutputFile(obj-ptr,
                         gen-ptr).

          CREATE TT_Resource.
          ASSIGN TT_Resource.obj_stream = pStream
                 TT_Resource.pdf_id     = pID
                 TT_Resource.par_Obj    = pObject
                 TT_Resource.par_gen    = pGen
                 TT_Resource.page_id    = pPage
                 TT_Resource.res_type   = "ICCBased"
                 TT_Resource.res_obj    = obj-ptr
                 TT_Resource.res_gen    = gen-ptr
                 TT_Resource.res_text   = "".

        END.

        IF INDEX(text-ptr,"~/DeviceCMYK") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/DeviceCMYK"))).
          ASSIGN obj-ptr = INT(ENTRY(2, text-temp, " "))
                 gen-ptr = INT(ENTRY(3, text-temp, " ")).

          RUN OutputFile(obj-ptr,
                         gen-ptr).

          CREATE TT_Resource.
          ASSIGN TT_Resource.obj_stream = pStream
                 TT_Resource.pdf_id     = pID
                 TT_Resource.par_Obj    = pObject
                 TT_Resource.par_gen    = pGen
                 TT_Resource.page_id    = pPage
                 TT_Resource.res_type   = "DeviceCMYK"
                 TT_Resource.res_obj    = obj-ptr
                 TT_Resource.res_gen    = gen-ptr
                 TT_Resource.res_text   = "".

        END.

        IF INDEX(text-ptr,"~/R11") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/R11"))).
          ASSIGN obj-ptr = INT(ENTRY(2, text-temp, " "))
                 gen-ptr = INT(ENTRY(3, text-temp, " ")).

          RUN OutputFile(obj-ptr,
                         gen-ptr).

          CREATE TT_Resource.
          ASSIGN TT_Resource.obj_stream = pStream
                 TT_Resource.pdf_id     = pID
                 TT_Resource.par_Obj    = pObject
                 TT_Resource.par_gen    = pGen
                 TT_Resource.page_id    = pPage
                 TT_Resource.res_type   = "R11"
                 TT_Resource.res_obj    = obj-ptr
                 TT_Resource.res_gen    = gen-ptr
                 TT_Resource.res_text   = "".

        END.

        IF INDEX(text-ptr,"~/Indexed") > 0 THEN DO:
          text-ptr = TRIM(text-ptr).
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Indexed"))).

          
          ASSIGN obj-ptr = INT(ENTRY(2, text-temp, " "))
                 gen-ptr = INT(ENTRY(3, text-temp, " ")).

          RUN OutputFile(obj-ptr,
                         gen-ptr).

          CREATE TT_Resource.
          ASSIGN TT_Resource.obj_stream = pStream
                 TT_Resource.pdf_id     = pID
                 TT_Resource.par_Obj    = pObject
                 TT_Resource.par_gen    = pGen
                 TT_Resource.page_id    = pPage
                 TT_Resource.res_type   = "Indexed"
                 TT_Resource.res_obj    = obj-ptr
                 TT_Resource.res_gen    = gen-ptr
                 TT_Resource.res_text   = "".


          /* Process Lookup */
          ASSIGN obj-ptr = INT(ENTRY(6, text-temp, " "))
                 gen-ptr = INT(ENTRY(7, text-temp, " ")).

          RUN OutputFile(obj-ptr,
                         gen-ptr).

          CREATE TT_Resource.
          ASSIGN TT_Resource.obj_stream = pStream
                 TT_Resource.pdf_id     = pID
                 TT_Resource.par_Obj    = pObject
                 TT_Resource.par_gen    = pGen
                 TT_Resource.page_id    = pPage
                 TT_Resource.res_type   = "Indexed"
                 TT_Resource.res_obj    = obj-ptr
                 TT_Resource.res_gen    = gen-ptr
                 TT_Resource.res_text   = "".

        END.

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
      END. /* True */

    END. /* First */
  END. /* each */

END. /* ProcessColorSpaceDictionary */

PROCEDURE ProcessShadingDictionary:
  DEFINE INPUT PARAMETER pPage    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-ptr    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE base-font   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Entries   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE descriptor  AS LOGICAL NO-UNDO.

  DEFINE VARIABLE obj-ptr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE gen-ptr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_object FOR TT_object.

  /* If the Shading has already been processed then don't do it again */
  IF CAN-FIND(FIRST B_TT_Object
              WHERE B_TT_Object.obj_stream = pStream
                AND B_TT_Object.pdf_id     = pID
                AND B_TT_Object.obj_id     = pObject
                AND B_TT_Object.gen_id     = pGen
                AND B_TT_Object.obj_type   = "~/Shading" NO-LOCK)
  THEN RETURN.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      B_TT_Object.obj_type = "~/Shading".

      SEEK INPUT TO B_TT_Object.obj_ptr.
      /* text-line = Readline(). */

      DO WHILE TRUE:
        text-ptr = readline().
        
        IF INDEX(text-ptr,"~/Function") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/Function"))).
          ASSIGN obj-ptr = INT(ENTRY(2, text-temp, " "))
                 gen-ptr = INT(ENTRY(3, text-temp, " ")).

          RUN OutputObject(obj-ptr,
                           gen-ptr).

          CREATE TT_Resource.
          ASSIGN TT_Resource.obj_stream = pStream
                 TT_Resource.pdf_id     = pID
                 TT_Resource.par_Obj    = pObject
                 TT_Resource.par_gen    = pGen
                 TT_Resource.page_id    = pPage
                 TT_Resource.res_type   = "Function"
                 TT_Resource.res_obj    = obj-ptr
                 TT_Resource.res_gen    = gen-ptr
                 TT_Resource.res_text   = "".

        END.

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
      END. /* True */

    END. /* First */
  END. /* each */

END. /* ProcessShadingDictionary */

PROCEDURE ProcessXObjectDictionary:

  DEFINE INPUT PARAMETER pPage        AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pParObject   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pParGen      AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pObject      AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen         AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pLen        AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.
  
  DEFINE VARIABLE m_contents  AS MEMPTR NO-UNDO.

  DEFINE VARIABLE text-ptr    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-sub    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE L_Length    AS INTEGER NO-UNDO.

  DEFINE VARIABLE l_Filter    AS LOGICAL NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      ASSIGN B_TT_Object.obj_type = "~/XObject"
             B_TT_Object.page_id  = pPage.

      SEEK INPUT TO B_TT_Object.obj_ptr.

      /* Find Length */
      DO WHILE TRUE:

        text-ptr = readline().

        IF INDEX(text-ptr,"~/Length") > 0 THEN DO:
          text-sub = SUBSTR(text-ptr,INDEX(text-ptr,"~/Length")).
          text-sub = TRIM(REPLACE(text-sub,"~/"," ")).
          Entry4 = ENTRY(4, text-sub, " ") NO-ERROR.

          IF NOT Entry4 BEGINS "R" THEN
            l_Length = INT(ENTRY(2, text-sub, " ")) NO-ERROR.
          ELSE DO:
            RUN DetermineLength (INPUT  INT(ENTRY(2, text-sub, " ")),
                                 INPUT  INT(ENTRY(3, text-sub, " ")),
                                 INPUT  pPage,
                                 OUTPUT l_Length).
          END.

          /* LEAVE. */
        END. /* Length */

        IF INDEX(text-ptr,">>") > 0 
        OR INDEX(text-ptr,"endstream") > 0 THEN LEAVE.

      END. /* While True */

      IF l_Length > 0 THEN DO:
        SET-SIZE(m_contents) = l_Length.
        SEEK INPUT TO B_TT_Object.obj_ptr.

        RUN OutputFile(B_TT_object.obj_id,
                       B_TT_Object.gen_id).

        pLen = l_Length.

      END. /* Length is > 0 */
      
      SET-SIZE(m_contents) = 0.
      OUTPUT CLOSE.

    END. /* FIRST */
  END. /* each Pages Dictionary */
  
END. /* ProcessXObjectDictionary */

PROCEDURE ProcessExtGStateDictionary:

  DEFINE INPUT PARAMETER pPage    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-ptr    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE base-font   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE L_Entries   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE descriptor  AS LOGICAL NO-UNDO.

  DEFINE VARIABLE obj-ptr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE gen-ptr   AS INTEGER NO-UNDO.
  DEFINE VARIABLE curr-ptr  AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_object FOR TT_object.

  /* If the Shading has already been processed then don't do it again */
  IF CAN-FIND(FIRST B_TT_Object
              WHERE B_TT_Object.obj_stream = pStream
                AND B_TT_Object.pdf_id     = pID
                AND B_TT_Object.obj_id     = pObject
                AND B_TT_Object.gen_id     = pGen
                AND B_TT_Object.obj_type   = "~/ExtGState" NO-LOCK)
  THEN RETURN.

  FOR EACH B_TT_Object WHERE B_TT_Object.obj_stream = pStream
                         AND B_TT_Object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      B_TT_Object.obj_type = "~/ExtGState".

      SEEK INPUT TO B_TT_Object.obj_ptr.
      /* text-line = Readline(). */

      DO WHILE TRUE:
        text-ptr = readline().
        
        IF INDEX(text-ptr,"~/HT ") > 0 THEN DO:
          text-temp = TRIM(SUBSTR(text-ptr,INDEX(text-ptr,"~/HT "))).
          ASSIGN obj-ptr = INT(ENTRY(2, text-temp, " "))
                 gen-ptr = INT(ENTRY(3, text-temp, " ")) NO-ERROR.

          /*
          RUN OutputObject(obj-ptr,
                           gen-ptr).

          CREATE TT_Resource.
          ASSIGN TT_Resource.obj_stream = pStream
                 TT_Resource.pdf_id     = pID
                 TT_Resource.par_Obj    = pObject
                 TT_Resource.par_gen    = pGen
                 TT_Resource.page_id    = pPage
                 TT_Resource.res_type   = "HalfTone"
                 TT_Resource.res_obj    = obj-ptr
                 TT_Resource.res_gen    = gen-ptr
                 TT_Resource.res_text   = "".
          */
        END.

        IF INDEX(text-ptr,"endobj") > 0 THEN LEAVE.
      END. /* True */

    END. /* First */
  END. /* each */
  
END. /* ProcessExtGStateDictionary */

PROCEDURE ProcessPageContent:

  DEFINE INPUT PARAMETER pCounter AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pObject  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen     AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pPage    AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.
  
  DEFINE VARIABLE m_contents  AS MEMPTR NO-UNDO.

  DEFINE VARIABLE text-ptr    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-sub    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE text-temp   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE obj-cnt     AS INTEGER NO-UNDO.
  DEFINE VARIABLE con-obj     AS INTEGER NO-UNDO.
  DEFINE VARIABLE con-gen     AS INTEGER NO-UNDO.
  DEFINE VARIABLE curr-ptr    AS INTEGER NO-UNDO.

  DEFINE VARIABLE L_Length    AS INTEGER NO-UNDO.
  DEFINE VARIABLE L_Ptr       AS INTEGER NO-UNDO.

  DEFINE VARIABLE l_Filter    AS LOGICAL NO-UNDO.

  FOR EACH B_TT_Object WHERE B_TT_object.obj_stream = pStream
                         AND B_TT_object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObject
                         AND B_TT_object.gen_id     = pGen
    BREAK BY B_TT_object.obj_seq:
    IF FIRST(B_TT_object.obj_Seq) THEN DO:

      IF pCounter = 1 THEN DO:
        OS-DELETE VALUE(SESSION:TEMP-DIR + pID + STRING(pPage) + ".txt") NO-ERROR.
        RUN CreateInfo("Page",pPage).
      END.

      ASSIGN B_TT_Object.obj_type = "~/Content"
             B_TT_Object.page_id  = pPage.

      SEEK INPUT TO B_TT_Object.obj_ptr.

      /* Find Length */
      DO WHILE TRUE:

        text-ptr = readline().

        /* If the Content was passed as an array of object pointers then we
           need to handle the actual contents here */
        IF INDEX(text-ptr,"[") > 0 THEN DO:
          text-temp = SUBSTR(text-ptr,INDEX(text-ptr,"[") + 1).

          IF INDEX(text-temp,"]") > 0 THEN 
            text-temp = SUBSTR(text-temp,1,INDEX(text-temp,"]") - 1).

          text-temp = TRIM(text-temp).

          /* This is a temporary holder for the page content */
          OS-DELETE VALUE(SESSION:TEMP-DIR + "filter.txt").

          DO obj-cnt = 1 TO NUM-ENTRIES(text-temp," ") BY 3:

            ASSIGN con-obj = INT(ENTRY(obj-cnt, text-temp, " "))
                   con-gen = INT(ENTRY(obj-cnt + 1, text-temp, " ")) NO-ERROR.

            IF ERROR-STATUS:ERROR THEN NEXT.

            curr-ptr = SEEK(INPUT).
            RUN ProcessPageContent (obj-cnt, con-obj, con-gen, pPage).
            SEEK INPUT TO curr-ptr.

          END.

          /* Again, we don't need it so make sure it's gone */
          OS-DELETE VALUE(SESSION:TEMP-DIR + "filter.txt").

          RETURN.
        END.

        IF INDEX(text-ptr,"~/Length") > 0 THEN DO:
          text-sub = SUBSTR(text-ptr,INDEX(text-ptr,"~/Length")).
          text-sub = TRIM(REPLACE(text-sub,"~/"," ")).

          Entry4 = ENTRY(4, text-sub, " ") NO-ERROR.
          IF NOT Entry4 BEGINS "R" THEN
            l_Length = INT(ENTRY(2, text-sub, " ")) NO-ERROR.
          ELSE
            RUN DetermineLength (INPUT  INT(ENTRY(2, text-sub, " ")),
                                 INPUT  INT(ENTRY(3, text-sub, " ")),
                                 INPUT  pPage,
                                 OUTPUT l_Length).

          /* LEAVE. */
        END. /* Length */
        
        IF INDEX(text-ptr,"~/Filter") > 0 THEN DO:
          text-sub = SUBSTR(text-ptr,INDEX(text-ptr,"~/Filter")).
          text-sub = REPLACE(text-sub,"~/"," ").
          RUN ExtraInfo("Page",pPage,"~/Filter " + ENTRY(2,text-sub, " ")).
          l_Filter = TRUE.
        END. /* Filter */

        IF INDEX(text-ptr,">>") > 0 
        OR INDEX(text-ptr,"endstream") > 0 THEN LEAVE.

      END. /* While True */

      SET-SIZE(m_Contents) = 0.

      IF l_Length > 0 THEN DO:
        SET-SIZE(m_contents) = l_Length.
        SEEK INPUT TO B_TT_Object.obj_ptr.

        /* Get Content */
        CONTENTS:
        DO WHILE TRUE:
          L_Ptr = SEEK(INPUT).
          text-ptr = readline().

          IF INDEX(text-ptr,"stream") > 0 THEN DO:
            /* SEEK INPUT TO L_Ptr + LENGTH(text-ptr) - 1. */
            SEEK INPUT TO SEEK(INPUT).
            IMPORT m_Contents.
            LEAVE CONTENTS.
          END.

          /* Somehow we misssed the content? */
          IF INDEX(text-ptr,"endstream") > 0 THEN DO:
            l_Length = 0.
            LEAVE CONTENTS.
          END.
        END. /* While True */

        IF NOT L_Filter THEN DO:
          OUTPUT TO VALUE(SESSION:TEMP-DIR + pID + STRING(pPage) + ".txt") BINARY NO-MAP NO-CONVERT APPEND.
            EXPORT m_Contents.
          OUTPUT CLOSE.
        END. /* No Filter */

        ELSE DO:

          /* Export the individual Object */
          OUTPUT TO VALUE(SESSION:TEMP-DIR + pID + STRING(pPage) + "-" + STRING(B_TT_Object.obj_id) + ".txt") BINARY NO-MAP NO-CONVERT.
            EXPORT m_Contents.
          OUTPUT CLOSE.

          /* Now Decompress the File */
          decompressfile ( SESSION:TEMP-DIR + pID + STRING(pPage) + "-" + STRING(B_TT_Object.obj_id) + ".txt",
                           SESSION:TEMP-DIR + "decompressed.txt").

          /* Remove the individual Object file */
          OS-DELETE VALUE(SESSION:TEMP-DIR + pID + STRING(pPage) + "-" + STRING(B_TT_Object.obj_id) + ".txt").

          /* For debugging purposes 
          OS-COPY VALUE(SESSION:TEMP-DIR + "decompressed.txt")
                  VALUE(SESSION:TEMP-DIR + "Copy-" + pID + STRING(pPage) + "-" + STRING(B_TT_Object.obj_id) + ".txt").
          */

          /* Now re-import the decompressed file */
          SET-SIZE(m_Contents) = 0.
          FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + "decompressed.txt".
          IF FILE-INFO:FILE-size <> ? THEN DO:
            SET-SIZE(m_Contents) = FILE-INFO:FILE-SIZE.
            INPUT FROM VALUE(FILE-INFO:FILE-NAME) BINARY NO-MAP NO-CONVERT NO-ECHO.
              IMPORT m_Contents.
            INPUT CLOSE.

            /* Then output to Page File */
            OUTPUT TO VALUE(SESSION:TEMP-DIR + "filter.txt") BINARY NO-MAP NO-CONVERT APPEND.
              EXPORT m_Contents.
            OUTPUT CLOSE.

            OS-DELETE VALUE(SESSION:TEMP-DIR + "decompressed.txt").
          END.
        END. /* Filtered */

      END. /* Length is > 0 */

      SET-SIZE(m_contents) = 0.
      OUTPUT CLOSE.
    END. /* FIRST */
  END. /* each Pages Dictionary */
  
  /* If a filter was applied to original stream then we need to compress the
     contents of the page */
  IF l_Filter THEN DO:

    /* Now don't recompress the file --- it's up to the calling program
       to determine whether they want the contents compressed or not via
       the COMPRESS parameter */
    OS-COPY VALUE(SESSION:TEMP-DIR + "filter.txt")
            VALUE(SESSION:TEMP-DIR + pID + STRING(pPage) + ".txt").

    /*
    compressfile(SESSION:TEMP-DIR + "filter.txt",
                 SESSION:TEMP-DIR + pID + STRING(pPage) + ".txt").
    */
  END.
END. /* ProcressPageContent */

PROCEDURE DetermineLength:
  DEFINE INPUT  PARAMETER pObj    AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pGen    AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER pPage   AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pLength AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.

  DEFINE VARIABLE orig-ptr    AS INTEGER NO-UNDO.

  DEFINE VARIABLE text-ptr    AS CHARACTER NO-UNDO.

  orig-ptr = SEEK(INPUT).

  FOR EACH B_TT_Object WHERE B_TT_object.obj_stream = pStream
                         AND B_TT_object.pdf_id     = pID
                         AND B_TT_object.obj_id     = pObj
                         AND B_TT_object.gen_id     = pGen
                         BREAK BY B_TT_object.obj_seq:
    IF FIRST-OF(B_TT_Object.obj_seq) THEN DO:

      ASSIGN B_TT_Object.obj_type = "~/Length"
             B_TT_Object.page_id  = pPage.

      SEEK INPUT TO B_TT_Object.obj_ptr.

      /* Find Length */
      text-ptr = readline().
      pLength = INT(readline()) NO-ERROR.

    END.
  END.

  SEEK INPUT TO orig-ptr.

END.

PROCEDURE OutputObject:
  DEFINE INPUT PARAMETER pObject AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen    AS INTEGER NO-UNDO.

  DEFINE VARIABLE orig-ptr  AS INTEGER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.

  orig-ptr = SEEK(INPUT).

  FIND FIRST B_TT_Object 
       WHERE B_TT_Object.obj_stream = pStream
         AND B_TT_Object.pdf_id     = pID
         AND B_TT_Object.obj_id     = pObject
         AND B_TT_Object.gen_id     = pGen NO-LOCK NO-ERROR.

  SEEK INPUT TO B_TT_Object.obj_ptr.

  /* text-line = readline().  /* Skip the Object Header */ */

  OUTPUT TO VALUE(SESSION:TEMP-DIR + pID + "-" 
                 + STRING(pObject) + "-" + STRING(pGen) + ".txt") 
                 BINARY NO-MAP NO-CONVERT.
   REPEAT:
     text-line = readline().

     /* Remove the Object Header info */
     IF INDEX(text-line, STRING(pObject) + " " + STRING(pGen) + " obj") > 0 THEN
       text-line = REPLACE(text-line,STRING(pObject) + " " + STRING(pGen) + " obj", "").

     IF INDEX(text-line,"endobj") > 0 THEN LEAVE.

     PUT UNFORMATTED text-line SKIP.

   END.
 OUTPUT CLOSE.

 SEEK INPUT TO orig-ptr.

END. /* OutputObject */

PROCEDURE OutputFile:
  DEFINE INPUT PARAMETER pObject AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pGen    AS INTEGER NO-UNDO.

  DEFINE VARIABLE orig-ptr    AS INTEGER NO-UNDO.
  DEFINE VARIABLE obj-length  AS INTEGER NO-UNDO.
  DEFINE VARIABLE length-obj  AS INTEGER  NO-UNDO.
  DEFINE VARIABLE length-gen  AS INTEGER NO-UNDO.

  DEFINE VARIABLE file-content  AS MEMPTR NO-UNDO.

  DEFINE VARIABLE got-length  AS LOGICAL NO-UNDO.
  DEFINE VARIABLE length-dict AS LOGICAL NO-UNDO.

  DEFINE VARIABLE text-ptr    AS CHARACTER NO-UNDO.

  DEFINE BUFFER B_TT_Object FOR TT_object.

  orig-ptr = SEEK(INPUT).

  FIND FIRST B_TT_Object 
       WHERE B_TT_Object.obj_stream = pStream
         AND B_TT_Object.pdf_id     = pID
         AND B_TT_Object.obj_id     = pObject
         AND B_TT_Object.gen_id     = pGen NO-ERROR.

  SEEK INPUT TO B_TT_Object.obj_ptr.

  /* text-ptr = readline().  /* Skip the Object Header */ */

  /* Now find the Length of the File */
  REPEAT:
    text-ptr = readline().

    /* Remove the Object Header info */
    IF INDEX(text-ptr, STRING(pObject) + " " + STRING(pGen) + " obj") > 0 THEN
      text-ptr = REPLACE(text-ptr,STRING(pObject) + " " + STRING(pGen) + " obj", "").

    IF INDEX(text-ptr,"~/Length ") > 0 THEN DO:
      ASSIGN got-length  = TRUE
             length-dict = FALSE.
      
      text-ptr = SUBSTR(text-ptr,INDEX(text-ptr,"~/Length")).
      text-ptr = TRIM(REPLACE(text-ptr,"~/"," ")).

      Entry4 = ENTRY(4, text-ptr, " ") NO-ERROR.
      IF NOT Entry4 BEGINS "R" THEN
        obj-length = INT(ENTRY(2, text-ptr, " ")) NO-ERROR.
      ELSE DO:
        ASSIGN length-obj = INT(ENTRY(2, text-ptr, " "))
               length-gen = INT(ENTRY(3, text-ptr, " ")).

        RUN DetermineLength (INPUT  length-obj,
                             INPUT  length-gen,
                             INPUT  0,
                             OUTPUT obj-length).

        length-dict = TRUE.
      END.

      LEAVE.
    END.

    IF INDEX(text-ptr,"stream") > 0 THEN LEAVE.
  END.

  IF NOT got-length THEN RETURN.

  SET-SIZE(file-content) = obj-length.
  SEEK INPUT TO B_TT_Object.obj_ptr.

  /* text-ptr = readline().  /* Skip the Object Header */ */

  OUTPUT TO VALUE(SESSION:TEMP-DIR + pID + "-" 
                 + STRING(pObject) + "-" + STRING(pGen) + ".txt") 
                 BINARY NO-MAP NO-CONVERT.
   REPEAT:
     text-ptr = readline().

     IF INDEX(text-ptr, STRING(pObject) + " " + STRING(pGen) + " obj") > 0 THEN
       text-ptr = REPLACE(text-ptr,STRING(pObject) + " " + STRING(pGen) + " obj", "").

     /* This replaces the length dictionary entry with the actual length */
     IF length-dict THEN
       text-ptr = REPLACE(text-ptr,
                          STRING(length-obj) + " " + STRING(length-gen) + " R",
                          STRING(obj-length)).

     PUT UNFORMATTED text-ptr SKIP.

     IF INDEX(text-ptr,"stream") > 0 THEN DO:
       IMPORT file-content.
       EXPORT file-content.
       LEAVE.
     END.

   END.
 OUTPUT CLOSE.

 SET-SIZE(file-content) = 0.

 SEEK INPUT TO orig-ptr.
                       
END. /* OutputFile */

/* end of pdfextract,gif */
