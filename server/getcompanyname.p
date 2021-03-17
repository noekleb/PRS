DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
DEF INPUT  PARAM iiCompanyId   AS INT NO-UNDO.
DEF OUTPUT PARAM ocCompanyName AS CHAR NO-UNDO.
DEF OUTPUT PARAM bOk           AS LOG NO-UNDO.

ASSIGN ocCompanyName = "Polygon"
       bOk           = TRUE.

/* FIND Company WHERE Company.iCompanyId = iiCompanyId NO-LOCK NO-ERROR.                            */
/* IF AVAIL Company THEN DO:                                                                        */
/*   ocCompanyName = Company.cCompanyName.                                                          */
/*   FIND JBoxLoginSession WHERE JBoxLoginSession.cSessionId = icSessionId EXCLUSIVE-LOCK NO-ERROR. */
/*   IF AVAIL JBoxLoginSession THEN                                                                 */
/*     ASSIGN JBoxLoginSession.iCompanyId = Company.iCompanyId                                      */
/*            bOK                         = TRUE                                                    */
/*            .                                                                                     */
/* END.                                                                                             */


