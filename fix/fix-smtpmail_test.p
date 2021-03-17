def var lMailOK as logi no-undo.
def var cMessage as char no-undo.
        RUN prssmtpmailv5_7a.p (
    /*mailhub    */   "mail.polygonsoftware.no",
    /*EmailTo    */   "tomn@polygonsoftware.no",
    /*EmailFrom  */   "tomn@polygonsoftware.no",
    /*EmailCC    */   "",
    /*Attachments*/   "",
    /*LocalFiles */   "",
    /*Subject    */   "Polygon prssmtpmailv5_7a",
    /*Body       */   "tz = blank",
    /*MIMEHeader */   "",
    /*BodyType   */   "",
    /*Importance */   0,
    /*L_DoAUTH   */   TRUE,
    /*C_AuthType */   "base64",
    /*C_User     */   "tomn@polygonsoftware.no",
    /*C_Password */   "tomno",
    /*oSuccessful*/  OUTPUT lMailOK,
    /*vMessage   */  OUTPUT cMessage) NO-ERROR.
    message lMailOk skip
    cMessage view-as alert-box.
/*     IF cFilNavn <> "" THEN         */
/*         OS-DELETE VALUE(cFilNavn). */
