/* =====================================================================
   file    : src-htm.p
   purpose : read HTML file from apisite and add syntax-coloring 
             to its sourcecode-examples 
   ====================================================================== */

DEFINE INPUT PARAMETER p-origdir  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER p-newdir   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER p-filename AS CHAR NO-UNDO.

/* define the HTML-tags you want to insert. Actual styles will be defined in stylesheet */

/* comments */
&GLOBAL-DEFINE TAG_START_COMMENT '<span class="src-comment">'
&GLOBAL-DEFINE TAG_END_COMMENT '</span src-comment>'
/* progress 4GL keywords */
&GLOBAL-DEFINE TAG_START_KEYWORD '<span class="src-keyword">'
&GLOBAL-DEFINE TAG_END_KEYWORD '</span src-keyword>'
/* literal integers and decimals */ 
&GLOBAL-DEFINE TAG_START_NUMBER '<span class="src-number">'
&GLOBAL-DEFINE TAG_END_NUMBER '</span src-number>'
/* literal strings */ 
&GLOBAL-DEFINE TAG_START_CHAR '<span class="src-char">'
&GLOBAL-DEFINE TAG_END_CHAR '</span src-char>'
/* preprocessors (things between {} or starting with &) */   
&GLOBAL-DEFINE TAG_START_PREP '<span class="src-prep">'
&GLOBAL-DEFINE TAG_END_PREP '</span src-prep>'

         
                                                   
DEF STREAM oldfile.
DEF STREAM newfile.
DEF VAR v-line         AS CHAR    NO-UNDO.
DEF VAR v-commentdepth AS INTEGER NO-UNDO INITIAL 0.
DEF VAR v-insource     AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR v-prevtag      AS CHAR    NO-UNDO INITIAL "".
DEF VAR v-prevposition AS INTEGER NO-UNDO INITIAL 0.

INPUT STREAM oldfile FROM value(p-origdir + '\' + p-filename).
OUTPUT STREAM newfile TO value(p-newdir + '\' + p-filename).

REPEAT :
     import stream oldfile unformatted v-line no-error.
     IF (INDEX(v-line, "</TABLE>")>0) AND v-insource THEN 
        v-insource = FALSE.
     IF v-insource THEN
        RUN ParseLine.
     IF INDEX(v-line,"<TABLE")>0 AND INDEX(v-line,'class="voorbeeld"')>0 THEN
        v-insource = TRUE.
     IF v-line="" THEN v-line=" ".
     PUT STREAM newfile UNFORMATTED v-line SKIP.
END.

PROCEDURE ParseLine :

    DEF VAR indoublestring AS LOGICAL NO-UNDO INITIAL FALSE.
    DEF VAR insinglestring AS LOGICAL NO-UNDO INITIAL FALSE.
    DEF VAR v-token        AS CHAR    NO-UNDO INITIAL "".
    DEF VAR v-number       AS DECIMAL NO-UNDO.
    DEF VAR inhtml         AS LOGICAL NO-UNDO INITIAL FALSE.
    DEF VAR stringed       AS LOGICAL NO-UNDO INITIAL FALSE.

   /* step 0: remove old html tags */
    v-line = REPLACE(v-line, {&TAG_START_COMMENT}, "").
    v-line = REPLACE(v-line, {&TAG_END_COMMENT}, "").
    v-line = REPLACE(v-line, {&TAG_START_CHAR}, "").
    v-line = REPLACE(v-line, {&TAG_END_CHAR}, "").
    v-line = REPLACE(v-line, {&TAG_START_PREP}, "").
    v-line = REPLACE(v-line, {&TAG_END_PREP}, "").
    v-line = REPLACE(v-line, {&TAG_START_KEYWORD}, "").
    v-line = REPLACE(v-line, {&TAG_END_KEYWORD}, "").
    v-line = REPLACE(v-line, {&TAG_START_NUMBER}, "").
    v-line = REPLACE(v-line, {&TAG_END_NUMBER}, "").

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

       IF vc = "/*":U  THEN DO:
          v-commentdepth = v-commentdepth + 1.
          IF v-commentdepth=1 THEN DO:
              v-line = SUBSTRING(v-line, 1, i - 1)
                          + {&TAG_START_COMMENT}
                          + SUBSTRING(v-line, i). 
              i = i + LENGTH({&TAG_START_COMMENT}).
              ASSIGN v-prevtag      = ""
                     v-prevposition = 0.
          END.
          NEXT loop_chars.
       END.

       IF vc = "*/":U  THEN DO:
          v-commentdepth = v-commentdepth - 1.
          IF v-commentdepth=0 THEN DO:
              v-line = SUBSTRING(v-line, 1, i + 1)
                          + {&TAG_END_COMMENT}
                          + SUBSTRING(v-line, i + 2). 
              i = i + LENGTH({&TAG_END_COMMENT}).
              ASSIGN v-prevtag      = ""
                     v-prevposition = 0.
          END.
          NEXT loop_chars.
       END.
   
      IF v-commentdepth>0 THEN NEXT loop_chars.

      /* ignore existing HTML tags */
      vc = SUBSTRING(v-line, i,1).
      IF vc="<" THEN DO: inhtml = TRUE.
                         NEXT loop_chars.
                     END.
      IF vc=">" THEN DO: inhtml = FALSE.
                         NEXT loop_chars.
                     END.
      IF inhtml THEN NEXT loop_chars.

      /* step 2a: find string literals ("" - double quotes) */
      IF NOT insinglestring THEN DO:
          vc = SUBSTRING(v-line, i,1).
          IF vc='"' THEN DO:
             IF NOT indoublestring THEN DO:
                 v-line = SUBSTRING(v-line, 1, i - 1)
                                    + {&TAG_START_CHAR}
                                    + SUBSTRING(v-line, i). 
                 i = i + LENGTH({&TAG_START_CHAR}).
             END.
             ELSE DO:
                 v-line = SUBSTRING(v-line, 1, i)
                                    + {&TAG_END_CHAR}
                                    + SUBSTRING(v-line, i + 1). 
                 i = i + LENGTH({&TAG_END_CHAR}) + 1.
             END.
             indoublestring = NOT indoublestring.
             stringed = TRUE.
             ASSIGN v-prevtag      = ""
                    v-prevposition = 0.
          END.
      END.

      /* step 2b: find string literals ('' - single quotes) */
      IF NOT indoublestring THEN DO:
          vc = SUBSTRING(v-line, i,1).
          IF vc="'" THEN DO:
             IF NOT insinglestring THEN DO:
                 v-line = SUBSTRING(v-line, 1, i - 1)
                                    + {&TAG_START_CHAR}
                                    + SUBSTRING(v-line, i). 
                 i = i + LENGTH({&TAG_START_CHAR}).
             END.
             ELSE DO:
                 v-line = SUBSTRING(v-line, 1, i)
                                    + {&TAG_END_CHAR}
                                    + SUBSTRING(v-line, i + 1). 
                 i = i + LENGTH({&TAG_END_CHAR}).
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
          v-line = SUBSTRING(v-line, 1, i - 1)
                             + {&TAG_START_PREP}
                             + SUBSTRING(v-line, i). 
          i = i + LENGTH({&TAG_START_PREP}).
          ASSIGN v-prevtag      = ""
                 v-prevposition = 0.
      END.
      IF vc="~}" THEN DO:
          v-line = SUBSTRING(v-line, 1, i)
                             + {&TAG_END_PREP}
                             + SUBSTRING(v-line, i + 1). 
          i = i + LENGTH({&TAG_END_PREP}).
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

    RUN AddSyntaxTags (INPUT-OUTPUT i, v-token, {&TAG_START_NUMBER}, {&TAG_END_NUMBER}).

END PROCEDURE.

PROCEDURE AddPreprocessorTags :
    DEFINE INPUT-OUTPUT PARAMETER i       AS INTEGER NO-UNDO.
    DEFINE INPUT        PARAMETER v-token AS CHAR    NO-UNDO.

    IF v-token MATCHES "&lt;*" OR v-token MATCHES "&gt;*" THEN DO:
        RUN AddKeywordTags(INPUT-OUTPUT i, v-token).
        RETURN.
    END.

    RUN AddSyntaxTags (INPUT-OUTPUT i, v-token, {&TAG_START_PREP}, {&TAG_END_PREP}).
END PROCEDURE.


PROCEDURE AddKeywordTags :
    DEFINE INPUT-OUTPUT PARAMETER i       AS INTEGER NO-UNDO.
    DEFINE INPUT        PARAMETER v-token AS CHAR    NO-UNDO.

    IF v-token="?" THEN DO:
       RUN AddNumberTags(INPUT-OUTPUT i, v-token).
       RETURN.
    END.

    RUN AddSyntaxTags (INPUT-OUTPUT i, v-token, {&TAG_START_KEYWORD}, {&TAG_END_KEYWORD}).
    
END PROCEDURE.

PROCEDURE AddSyntaxTags :
    DEFINE INPUT-OUTPUT PARAMETER i       AS INTEGER NO-UNDO.
    DEFINE INPUT        PARAMETER v-token AS CHAR    NO-UNDO.
    DEFINE INPUT        PARAMETER v-starttag  AS CHAR    NO-UNDO.
    DEFINE INPUT        PARAMETER v-endtag  AS CHAR    NO-UNDO.

    IF v-prevtag=v-endtag THEN DO:
       SUBSTRING(v-line, v-prevposition - LENGTH(v-endtag), LENGTH(v-endtag)) = "".
       i = i - LENGTH(v-endtag).
       v-line = SUBSTRING(v-line, 1, i - 1 - LENGTH(v-token))
                      + UPPER(v-token)
                      + v-endtag
                      + SUBSTRING(v-line, i).
       i = i + LENGTH(v-endtag).
    END.
    ELSE DO:
        v-line = SUBSTRING(v-line, 1, i - 1 - LENGTH(v-token))
                       + v-starttag
                       + UPPER(v-token)
                       + v-endtag
                       + SUBSTRING(v-line, i).
        i = i + LENGTH(v-starttag) + LENGTH(v-endtag).
    END.

    v-prevtag      = v-endtag.
    v-prevposition = i.

END PROCEDURE.
  
