DEF VAR obOk AS LOG NO-UNDO.

RUN cls\dintero\asSendLinkViaSMSTilDinSide.p ('41365436', OUTPUT obOk).

MESSAGE obOk
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
/*                                 
DEF VAR ocReturn AS CHAR NO-UNDO.
DEF VAR obOk AS LOG NO-UNDO.
DEF VAR cURLMinside AS CHAR NO-UNDO.

DEF VAR cMobilNr AS CHAR NO-UNDO.
DEF VAR cMsgs    AS CHAR NO-UNDO.
DEF VAR cEmne    AS CHAR NO-UNDO.

{syspara.i 14 303 6 cURLMinside}

ASSIGN 
    cEmne    = 'GANT Exclusive'
    cMsgs    = 'Legg inn dine data via GANT Exclusive registrerings side' + CHR(10) + 
               cURLMinside
    cMobilNr = '+4748004066' /* Are */
    cMobilNr = '+4741223320' /* Ken1 */
    cMobilNr = '+4741365436' /* Tom */
    .

RUN sendSMSTilMobil.p (cEmne + '|' +
               cMsgs + '|' +
               cMobilNr,
               ?,
               '',
               OUTPUT ocReturn,
               OUTPUT obOk).

*/
