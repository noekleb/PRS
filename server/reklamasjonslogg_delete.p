/*
   Opprettet: 29.09.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEF VAR fReklamVerdi    AS DEC NO-UNDO.
DEF VAR fReklamTotal    AS DEC NO-UNDO.
DEF VAR fReklamUtgift   AS DEC NO-UNDO.
DEF VAR iReklamasjonsNr AS INT NO-UNDO.

iReklamasjonsNr = INT(ENTRY(1,icParam,';')).
DO TRANSACTION:
 FIND FIRST Reklamasjonslogg WHERE Reklamasjonslogg.ReklamasjonsNr = iReklamasjonsNr
                             NO-LOCK NO-ERROR.

 IF AVAIL reklamasjonslogg THEN
 DO:
   FOR EACH Reklamasjonslinje WHERE Reklamasjonslinje.Reklamasjonsnr = iReklamasjonsnr EXCLUSIVE-LOCK:
     DELETE Reklamasjonslinje.
   END.                              
 END.
END. /*Transaction*/
  
