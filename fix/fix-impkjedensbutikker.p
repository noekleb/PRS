
/* 21/09/04 COPY assignment */
DEF VAR pcFil AS CHAR NO-UNDO.

DEF VAR pcButNr    AS CHAR NO-UNDO.
DEF VAR piButNr    AS INT  NO-UNDO.
DEF VAR pcButNavn  AS CHAR NO-UNDO.
DEF VAR pcKontakt  AS CHAR NO-UNDO.
DEF VAR pcAdresse  AS CHAR NO-UNDO.
DEF VAR pcTelefon  AS CHAR NO-UNDO.
DEF VAR pcTelefaks AS CHAR NO-UNDO.
DEF VAR pcPostNr   AS CHAR NO-UNDO.

pcFil = "C:\DB\mxold\Dump20092004\MXMEDL_UTEN_MAIL_XLS.csv".

DEF BUFFER bKjedensButikker FOR KjedensButikker.

FIND bKjedensButikker WHERE 
    bKjedensButikker.ButikkNR = 176.

DEF STREAM inn.

INPUT STREAM Inn FROM VALUE(pcFil) NO-ECHO.
  REPEAT:
    IMPORT STREAM inn DELIMITER ";"
    pcButNr   
    pcButNavn 
    pcKontakt
    pcAdresse 
    pcPostNr
    pcTelefon 
    pcTelefaks
    .
    ASSIGN
      piButNr = int(pcButNr) + 200
      pcPostNr = ENTRY(1,pcPostNr," ")
      NO-ERROR.
/*     IF ERROR-STATUS:ERROR THEN         */
/*         MESSAGE                        */
/*         piButNr                        */
/*         pcButNr                        */
/*         pcButNavn                      */
/*         pcAdresse                      */
/*         pcTelefon                      */
/*         pcTelefaks                     */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    FIND Kjedensbutikker WHERE
         KjedensButikker.ButikkNr = piButNr NO-ERROR.
    IF NOT AVAILABLE KjedensButikker THEN
        CREATE KjedensButikker.
    DO:
        BUFFER-COPY bKjedensButikker TO KjedensButikker
            ASSIGN
            KjedensButikker.ButikkNr   = piButNr
            KjedensButikker.ButikkNavn = pcButNavn
            KjedensButikker.Adresse1   = pcAdresse
            KjedensButikker.Telefon    = pcTelefon
            KjedensButikker.Telefaks   = pcTelefaks
            Kjedensbutikker.KontaktPerson = ""
            KjedensButikker.Firmanavn     = ""
            KjedensButikker.Kontaktperson = pcKontakt
            KjedensButikker.PostNr        = pcPostNr
            .
    END.
  END.

INPUT STREAM inn CLOSE.
