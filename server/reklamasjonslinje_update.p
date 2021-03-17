/*
   Opprettet: 29.09.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
 DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
 DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
 DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
 DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
 DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

 DEF VAR fReklamVerdi    AS DEC NO-UNDO.
 DEF VAR fReklamTotal    AS DEC NO-UNDO.
 DEF VAR fReklamUtgift   AS DEC NO-UNDO.
 DEF VAR iReklamasjonsNr AS INT NO-UNDO.
 DEF VAR iVg             AS INT NO-UNDO.
 DEF VAR iLopNr          AS INT NO-UNDO.
 DEF VAR fArtikkelNr     AS DEC NO-UNDO.

 DEF BUFFER bReklamasjonslinje FOR Reklamasjonslinje.

 DO TRANSACTION:
   FIND Reklamasjonslinje WHERE ROWID(Reklamasjonslinje) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.
  
   IF NOT AVAIL Reklamasjonslinje THEN
   DO:
     ocReturn = 'Posten for Reklamasjonslinje ble ikke funnet'.
     RETURN.
   END.

   ELSE DO:
       FIND ArtBas NO-LOCK WHERE
           ArtBas.ArtikkelNr = DEC(ENTRY(LOOKUP('ArtikkelNr',icFields),icValues,'|'))
           NO-ERROR.
       IF AVAILABLE ArtBas THEN
       DO:
           IF ArtBas.Vg <> INT(ENTRY(LOOKUP('Vg',icFields),icValues,'|')) or
               ArtBas.LopNr <> int(ENTRY(LOOKUP('LopNr',icFields),icValues,'|')) THEN
           DO:
               ocReturn =  "Ugyldig artikkelreferanse." + CHR(13) + 
                           "Sjekk artikkelnr og/eller vg/løpnr.".
               RETURN.
           END.
       END.
       ELSE DO:
           ocReturn =  "Ugyldig artikkelnummer.".
           RETURN.
       END.
/*        MESSAGE                                */
/*            Reklamasjonslinje.ArtikkelNr       */
/*            Reklamasjonslinje.Beskr            */
/*            Reklamasjonslinje.LevKod           */
/*            Reklamasjonslinje.Vg               */
/*            Reklamasjonslinje.LopNr SKIP       */
/*            icFields SKIP                      */
/*            icValues                           */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK. */
   END.
 END. /* TRANSACTION */
