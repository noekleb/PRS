  USING System.Data.SqlClient.*.
USING System.Data.*.


  DEF VAR cSQL      AS CHAR          NO-UNDO.
   DEF VAR ConString AS CHAR          NO-UNDO.
   DEF VAR Conn      AS SqlConnection NO-UNDO.
   DEF VAR Cmd       AS SqlCommand    NO-UNDO.
   DEF VAR Rdr       AS SqlDataReader NO-UNDO.
   DEF VAR SqlCred   AS SqlCredential NO-UNDO.
   DEF VAR SeqString AS System.Security.SecureString NO-UNDO.
   DEF VAR ix        AS INT NO-UNDO.
   DEF VAR ocReturn  AS CHAR NO-UNDO.
   DEF VAR icAvdNr   AS CHAR NO-UNDO INIT "24".

   FIND FIRST SysReg NO-LOCK
        WHERE SysReg.RegNr = 553
        NO-ERROR.
   IF NOT AVAIL SysReg THEN DO:
     ocReturn = ?.
     RETURN.
   END.

/* passord -> sec.string */
   SeqString = NEW System.Security.SecureString().
   DO ix = 1 TO LENGTH(SysReg.Beskrivelse):
     SeqString:AppendChar(SUBSTR(SysReg.Beskrivelse,ix,1)).
   END.
   SeqString:MakeReadOnly().

   FIND FIRST SysReg NO-LOCK
        WHERE SysReg.RegNr = 552
        NO-ERROR.
   IF NOT AVAIL SysReg THEN DO:
     ocReturn = ?.
     RETURN.
   END.

/* brukernavn, pwd */
   SqlCred = NEW SqlCredential(SysReg.Beskrivelse,SeqString).

   FIND FIRST SysReg NO-LOCK
        WHERE SysReg.RegNr = 545
        NO-ERROR.
   IF NOT AVAIL SysReg THEN DO:
     ocReturn = ?.
     RETURN.
   END.

   ConString = "Server=" + SysReg.Verdi + ",1433".

   FIND FIRST SysReg NO-LOCK
        WHERE SysReg.RegNr = 546
        NO-ERROR.
   IF NOT AVAIL SysReg THEN DO:
     ocReturn = ?.
     RETURN.
   END.

   ConString = ConString + ";Database=" + SysReg.Verdi + "GlobalData".

   Conn = NEW SqlConnection(ConString,SqlCred).

   FIND FIRST SysReg NO-LOCK
        WHERE SysReg.RegNr = 569
        NO-ERROR.
   IF NOT AVAIL SysReg THEN DO:
     ocReturn = ?.
     RETURN.
   END.

   cSQL =
REPLACE(REPLACE(SysReg.Beskrivelse,"%fromcustomerno%",STRING((INT(icAvdNr) +
100)) + "00000"),"%tocustomerno%",STRING((INT(icAvdNr) + 100)) + "99999").

   Conn:Open() NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
     ocReturn = ?.
     MESSAGE ERROR-STATUS:GET-MESSAGE(1)
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     RETURN.
   END.

   DYNAMIC-FUNCTION("LogThis" IN hSourceProc,"Start Vismasql",cSQL,1).
   Cmd = NEW SqlCommand(cSql, Conn).

   Rdr = Cmd:ExecuteReader().

   ix = 0.
   DO WHILE Rdr:Read():
     CREATE t-Utestaaende.
     ASSIGN t-Utestaaende.VGkundenr       = Rdr["CustomerNo"]
            t-Utestaaende.Navn            = Rdr["Name"]
            t-Utestaaende.Belop           = Rdr["RestAmount"]
            t-Utestaaende.Avdeling        = Rdr["DepNo"]
            ix = ix + 1.
   END.


