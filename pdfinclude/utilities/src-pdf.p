/* =====================================================================
   file    : src-pdf.p
   purpose : read Progress Source file and add syntax-coloring plus output
             it to a PDF document.

   note    : Most code re-used from Jurjen Dykstra's src-html.p routine
   
   ====================================================================== */

DEFINE INPUT PARAMETER p-origdir  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER p-newdir   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER p-filename AS CHAR NO-UNDO.

&GLOBAL-DEFINE TAG_DEFAULT RUN pdf_text_color("sPDF",0.0,0.0,0.0).

/* comments */
&GLOBAL-DEFINE TAG_START_COMMENT RUN pdf_text_color("sPDF",0.0,0.7,0.0).
/* progress 4GL keywords */
&GLOBAL-DEFINE TAG_START_KEYWORD RUN pdf_text_color("sPDF",0.0,0.0,0.7).
/* literal integers and decimals */ 
&GLOBAL-DEFINE TAG_START_NUMBER RUN pdf_text_color("sPDF",0.7,0.0,0.0).
/* literal strings */ 
&GLOBAL-DEFINE TAG_START_CHAR RUN pdf_text_color("sPDF",0.0,0.7,0.7).
/* preprocessors (things between {} or starting with &) */   
&GLOBAL-DEFINE TAG_START_PREP RUN pdf_text_color("sPDF",1.0,0.0,1.0).

                                                   
DEF STREAM oldfile.

DEF VAR v-line         AS CHAR    NO-UNDO.
DEF VAR v-commentdepth AS INTEGER NO-UNDO INITIAL 0.
DEF VAR v-insource     AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR v-pdffile      AS CHAR    NO-UNDO.
DEF VAR v-prevtag      AS CHAR    NO-UNDO INITIAL "".
DEF VAR v-prevposition AS INTEGER NO-UNDO INITIAL 0.

v-pdffile = SUBSTR(p-filename,1,INDEX(p-filename,".":U) - 1) + ".pdf":U.

{ pdf_inc.i }
RUN pdf_new("Spdf",p-newdir + "\" + v-pdffile).
RUN pdf_set_BottomMargin("Spdf", 25).

pdf_PageHeader("Spdf",
               THIS-PROCEDURE:HANDLE,
               "ipPageHeader").

pdf_PageFooter("Spdf",
               THIS-PROCEDURE:HANDLE,
               "ipPageFooter").

RUN pdf_new_page("Spdf").

INPUT STREAM oldfile FROM value(p-origdir + '\' + p-filename).

REPEAT :
     import stream oldfile unformatted v-line no-error.
     RUN ParseLine.

     RUN pdf_text("Spdf",v-line).
     RUN pdf_skip("Spdf").
END.

RUN pdf_close("Spdf").

PROCEDURE ipPageHeader:
  DEFINE VARIABLE cFont       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE decFontSize AS DECIMAL NO-UNDO.

  /* Draw a page border */
  RUN pdf_rect2 ("Spdf",
                 5,
                 5,
                 pdf_PageWidth("Spdf") - 10,
                 pdf_PageHeight("Spdf") - 10,
                 1.0).

  DEFINE VARIABLE decRed      AS DECIMAL NO-UNDO.
  DEFINE VARIABLE decGreen    AS DECIMAL NO-UNDO.
  DEFINE VARIABLE decBlue     AS DECIMAL NO-UNDO.

  decRed   = pdf_TextRed("Spdf").
  decGreen = pdf_TextGreen("Spdf").
  decBlue  = pdf_TextBlue("Spdf").

  /* This ensures that the last color set is re-used on the next page.  Useful
     if the tags (say for comments) span multiple pages */
  RUN pdf_text_color("Spdf",
                     decRed,
                     decGreen,
                     decBlue).

END.

PROCEDURE ipPageFooter:
  DEFINE VARIABLE cFont       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE decFontSize AS DECIMAL NO-UNDO.

  DEFINE VARIABLE decRed      AS DECIMAL NO-UNDO.
  DEFINE VARIABLE decGreen    AS DECIMAL NO-UNDO.
  DEFINE VARIABLE decBlue     AS DECIMAL NO-UNDO.

  decRed   = pdf_TextRed("Spdf").
  decGreen = pdf_TextGreen("Spdf").
  decBlue  = pdf_TextBlue("Spdf").

  decFontSize = pdf_PointSize("Spdf").
  cFont       = pdf_Font("Spdf").
  RUN pdf_set_font("Spdf",cFont,6.0).

  RUN pdf_text_color("Spdf",0.0,0.0,0.0).
  RUN pdf_text_xy("Spdf","Produced by PDFinclude",10,10).

  RUN pdf_set_font("Spdf",cFont,10.0).

  /* This ensures that the last color set is re-used on the next page.  Useful
     if the tags (say for comments) span multiple pages */
  RUN pdf_text_color("Spdf",
                     decRed,
                     decGreen,
                     decBlue).


END.

PROCEDURE ParseLine :

    DEF VAR indoublestring AS LOGICAL NO-UNDO INITIAL FALSE.
    DEF VAR insinglestring AS LOGICAL NO-UNDO INITIAL FALSE.
    DEF VAR v-token        AS CHAR    NO-UNDO INITIAL "".
    DEF VAR v-number       AS DECIMAL NO-UNDO.
    DEF VAR inhtml         AS LOGICAL NO-UNDO INITIAL FALSE.
    DEF VAR stringed       AS LOGICAL NO-UNDO INITIAL FALSE.

    DEF VAR v-save         AS CHARACTER NO-UNDO.

   /* step 0: reset previous tag and previous position */

   ASSIGN v-prevtag      = ""
          v-prevposition = 0.

   /* step 1: find (nested) comments */
   DEF VAR i AS INTEGER NO-UNDO.
   DEF VAR vc AS CHAR NO-UNDO.
   
   i = 0.
   loop_chars:
   DO WHILE i < LENGTH(v-line) :
       i = i + 1.
       stringed = FALSE.

       vc = SUBSTRING(v-line, i , 2).

       IF vc = "~/*":U  THEN DO:
          v-commentdepth = v-commentdepth + 1.
          IF v-commentdepth=1 THEN DO:
              v-save = v-line.
              v-line = SUBSTRING(v-save, 1, i - 1).
              RUN pdf_text("Spdf",v-line).
              {&TAG_START_COMMENT}

              RUN pdf_text("Spdf","~/*"). 

              v-line = SUBSTRING(v-save, i + 2).
              i = 1.
              ASSIGN v-prevtag      = ""
                     v-prevposition = 0.
          END.
          NEXT loop_chars.
       END.

       IF vc = "*~/":U  THEN DO:
          v-commentdepth = v-commentdepth - 1.
          IF v-commentdepth=0 THEN DO:
              v-save = v-line.
              v-line = SUBSTRING(v-line, 1, i + 1).
              RUN pdf_text("Spdf",v-line).

              {&TAG_DEFAULT}

              v-line = SUBSTRING(v-save, i + 2).
              i = 1.
              ASSIGN v-prevtag      = ""
                     v-prevposition = 0.
          END.
          NEXT loop_chars.
       END.
   
      IF v-commentdepth>0 THEN NEXT loop_chars.

      /* step 2a: find string literals ("" - double quotes) */
      IF NOT insinglestring THEN DO:
          vc = SUBSTRING(v-line, i,1).
          IF vc='"' THEN DO:
             IF NOT indoublestring THEN DO:
                 v-save = v-line.
                 v-line = SUBSTRING(v-line, 1, i - 1).
                 RUN pdf_text("Spdf",v-line).

                 {&TAG_START_CHAR}

                 v-line = SUBSTRING(v-save, i).
                 i = 1.
                 indoublestring = TRUE.
                 stringed = TRUE.
             END.
             ELSE DO:
                 v-save = v-line.
                 v-line = SUBSTRING(v-line, 1, i).
                 RUN pdf_text("Spdf",v-line).

                 {&TAG_DEFAULT}

                 v-line = SUBSTRING(v-save, i + 1).
                 i = 1.
                 indoublestring = false.
                 stringed = false.
             END.

             ASSIGN v-prevtag      = ""
                    v-prevposition = 0.
          END.
      END.

      /* step 2b: find string literals ('' - single quotes) */
      IF NOT indoublestring THEN DO:
          vc = SUBSTRING(v-line, i,1).
          IF vc="'" THEN DO:
             IF NOT insinglestring THEN DO:
                 v-save = v-line.
                 v-line = SUBSTRING(v-line, 1, i - 1).
                 RUN pdf_Text("Spdf",v-line).

                 {&TAG_START_CHAR}
                 v-line = SUBSTRING(v-save, i).
                 i = 1.
             END.
             ELSE DO:
                 v-save = v-line.
                 v-line = SUBSTRING(v-line, 1, i).
                 RUN pdf_text("Spdf",v-line).

                 {&TAG_DEFAULT}
                 v-line = SUBSTRING(v-save, i + 1).
                 i = 1.
             END.
             insinglestring = NOT insinglestring.
             stringed = TRUE.
             ASSIGN v-prevtag      = ""
                    v-prevposition = 0.
          END.
      END.

      /* step 3: find preprocessors and include directives */
      vc = SUBSTRING(v-line, i,1).
      IF vc="~{" THEN DO:
          v-save = v-line.
          v-line = SUBSTRING(v-line, 1, i - 1).
          RUN pdf_text("Spdf",v-line).

          {&TAG_START_PREP}
          v-line = SUBSTRING(v-save, i).
          i = 1.
          ASSIGN v-prevtag      = ""
                 v-prevposition = 0.
      END.
      IF vc="~}" THEN DO:
          v-save = v-line.
          v-line = SUBSTRING(v-line, 1, i).

          {&TAG_START_PREP}
          RUN pdf_text("Spdf",v-line).

          {&TAG_DEFAULT}
          v-line = SUBSTRING(v-save, i + 1).
          i = 1.
          ASSIGN v-prevtag      = ""
                 v-prevposition = 0.
      END.

      /* step 4: find words (could be keywords or numbers) */
      IF NOT (indoublestring OR insinglestring) THEN DO:
          vc = SUBSTRING(v-line, i,1).
          IF INDEX(" (),/+.:=" + CHR(9), vc)>0 OR stringed THEN DO:
             IF KEYWORD-ALL(v-token) NE ? THEN
                 RUN AddKeywordTags(INPUT-OUTPUT i, v-token).
             ELSE IF v-token MATCHES "&*" THEN
                 RUN AddPreprocessorTags(INPUT-OUTPUT i, v-token).
             ELSE IF v-token<>'' THEN DO:
                v-number = ?.
                ASSIGN v-number=DECIMAL(v-token) NO-ERROR.
                IF v-number<>? THEN
                   RUN AddNumberTags(INPUT-OUTPUT i, v-token).
                ELSE 
                   RUN AddStandardTags(INPUT-OUTPUT i, v-token).
             END.
             v-token = ''.

             IF (NOT stringed) AND (vc<>" ") THEN DO:
                /* the word-separator itself has to be colored too */
                 RUN AddStandardTags(INPUT-OUTPUT i, vc).
             END.

          END.
          ELSE 
             v-token = v-token + vc.
      END.

   END.

   /* step 5: remaining token at end of line */
   i = LENGTH(v-line) + 1.
   IF (NOT (indoublestring OR insinglestring)) AND TRIM(v-token)<>'' THEN DO:
          IF KEYWORD-ALL(v-token) NE ? THEN
             RUN AddKeywordTags (INPUT-OUTPUT i, v-token).
          ELSE IF v-token MATCHES "&*" THEN
              RUN AddPreprocessorTags(INPUT-OUTPUT i, v-token).
          ELSE DO:
             v-number = ?.
             ASSIGN v-number=DECIMAL(v-token) NO-ERROR.
             IF v-number<>? THEN
                 RUN AddNumberTags(INPUT-OUTPUT i, v-token).
          END.
   END.

   ASSIGN v-prevtag      = ""
          v-prevposition = 0.

END PROCEDURE.

PROCEDURE AddStandardTags :
    DEFINE INPUT-OUTPUT PARAMETER i       AS INTEGER NO-UNDO.
    DEFINE INPUT        PARAMETER v-token AS CHAR    NO-UNDO.
    
    v-prevtag      = "".
    v-prevposition = 0.
END PROCEDURE.


PROCEDURE AddNumberTags :
    DEFINE INPUT-OUTPUT PARAMETER i       AS INTEGER NO-UNDO.
    DEFINE INPUT        PARAMETER v-token AS CHAR    NO-UNDO.

    RUN AddSyntaxTags (INPUT-OUTPUT i, v-token, "~{~&TAG_START_NUMBER~}", "~{~&TAG_END_NUMBER~}").

END PROCEDURE.

PROCEDURE AddPreprocessorTags :
    DEFINE INPUT-OUTPUT PARAMETER i       AS INTEGER NO-UNDO.
    DEFINE INPUT        PARAMETER v-token AS CHAR    NO-UNDO.

    IF v-token MATCHES "&lt;*" OR v-token MATCHES "&gt;*" THEN DO:
        RUN AddKeywordTags(INPUT-OUTPUT i, v-token).
        RETURN.
    END.

    RUN AddSyntaxTags (INPUT-OUTPUT i, v-token, "~{~&TAG_START_PREP~}", "~{~&TAG_END_PREP~}").
END PROCEDURE.


PROCEDURE AddKeywordTags :
    DEFINE INPUT-OUTPUT PARAMETER i       AS INTEGER NO-UNDO.
    DEFINE INPUT        PARAMETER v-token AS CHAR    NO-UNDO.

    IF v-token="?" THEN DO:
       RUN AddNumberTags(INPUT-OUTPUT i, v-token).
       RETURN.
    END.

    RUN AddSyntaxTags (INPUT-OUTPUT i, v-token, "~{~&TAG_START_KEYWORD~}", "~{~&TAG_END_KEYWORD~}").
    
END PROCEDURE.

PROCEDURE AddSyntaxTags :
    DEFINE INPUT-OUTPUT PARAMETER i       AS INTEGER NO-UNDO.
    DEFINE INPUT        PARAMETER v-token AS CHAR    NO-UNDO.
    DEFINE INPUT        PARAMETER v-starttag  AS CHAR    NO-UNDO.
    DEFINE INPUT        PARAMETER v-endtag  AS CHAR    NO-UNDO.

    DEFINE VARIABLE v-save  AS CHARACTER NO-UNDO.

    IF v-prevtag=v-endtag THEN DO:
      /* This determines what color to start */
      CASE v-starttag:
        WHEN "~{~&TAG_START_KEYWORD~}" THEN
          {&TAG_START_KEYWORD}
        WHEN "~{~&TAG_START_NUMBER~}" THEN
          {&TAG_START_NUMBER}
        WHEN "~{~&TAG_START_CHAR~}" THEN
          {&TAG_START_CHAR}
        WHEN "~{~&TAG_START_PREP~}" THEN
          {&TAG_START_PREP}
      END CASE.

       v-save = v-line.
       v-line = SUBSTRING(v-line, 1, i - 1 - LENGTH(v-token))
                      + UPPER(v-token).

       RUN pdf_text("Spdf",v-line).
    END.
    ELSE DO:
       v-save = v-line.
       v-line = SUBSTRING(v-line, 1, i - 1 - LENGTH(v-token)).

       RUN pdf_text("Spdf",v-line).

       /* This determines what color to start */
       CASE v-starttag:
         WHEN "~{~&TAG_START_KEYWORD~}" THEN
           {&TAG_START_KEYWORD}
         WHEN "~{~&TAG_START_NUMBER~}" THEN
           {&TAG_START_NUMBER}
         WHEN "~{~&TAG_START_CHAR~}" THEN
           {&TAG_START_CHAR}
         WHEN "~{~&TAG_START_PREP~}" THEN
           {&TAG_START_PREP}
       END CASE.
       
       RUN pdf_text("Spdf",UPPER(v-token)).

    END.

    {&TAG_DEFAULT}

    v-line         = SUBSTR(v-save,i).
    i = 1.

    v-prevtag      = v-endtag.
    v-prevposition = i.

END PROCEDURE.
  
