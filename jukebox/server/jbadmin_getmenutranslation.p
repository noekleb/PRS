/* jbadmin_getmenutranslation.p
   Retrieve translations for the main menu for the translation program, JBoxMenuTranslation.w 
   
   Created 18.08.10 by brynjar@chemistry.no
--------------------------------------------------------------------------*/   
DEF INPUT  PARAM icParam         AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer        AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOk            AS LOG    NO-UNDO.

DEF VAR hQuery AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND FIRST JBoxMenuTranslation NO-LOCK
       WHERE JBoxMenuTranslation.iJBoxMenuId = INT(ihBuffer:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)
         AND JBoxMenuTranslation.cLanguage   = icParam
       NO-ERROR.
  IF AVAIL JBoxMenuTranslation THEN
    ASSIGN ihBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE   = JBoxMenuTranslation.cMenuLabel
           ihBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE = JBoxMenuTranslation.cAccelerator
           ihBuffer:BUFFER-FIELD("cMenuTooltip"):BUFFER-VALUE = JBoxMenuTranslation.cMenuTooltip
           .
  ELSE
    ASSIGN ihBuffer:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE   = ""
           ihBuffer:BUFFER-FIELD("cAccelerator"):BUFFER-VALUE = ""
           ihBuffer:BUFFER-FIELD("cMenuTooltip"):BUFFER-VALUE = ""
           .
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.

obOk = ocReturn = "".
   

