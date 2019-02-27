/* Send eMail */
DEF VAR bOk AS LOG NO-UNDO.
DEF VAR cMessage AS CHAR NO-UNDO.

RUN SendSMS.w ('',
               '',
               cMessage,
               OUTPUT bOK,
               OUTPUT cMessage).

