DEF VAR ocReturn AS CHAR NO-UNDO.
DEF VAR obOk AS LOG NO-UNDO.
DEF VAR ihBuffer AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttBokforingsbilag NO-UNDO
  FIELD lBokforingsId AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9"
  FIELD TYPE AS INTEGER 
  FIELD SeqNr AS INTEGER 
  FIELD PdfFil AS BLOB 
  . 

ihbuffer = BUFFER ttBokforingsbilag:HANDLE.

RUN Bokforingsbilag_getBlob.p ('62020000004',
                               ihbuffer,
                               '',
                               OUTPUT ocReturn,
                               OUTPUT obOk
                              ).
MESSAGE ocReturn SKIP
    obOk
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
