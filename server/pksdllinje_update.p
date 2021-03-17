/* To invoke: 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"CustomUpdateValProc","=update_validation.p").
   Procedure name must be prefixed by = or +
   =:   Dynamic validation is suppressed
   +:   The validation proc is run in addition to dynamic validation
   Dynamic (automatic) validation is done before custom val (ref doc/html/fieldMap.html for rules).
   
   If there's no fieldmap (viewer) set the attribute on the browse object
-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

DEF VAR fArtikkelnr AS DEC  NO-UNDO.
DEF VAR iLnr        AS INT  NO-UNDO INIT 1.
DEF VAR cStorl      AS CHAR NO-UNDO.
DEF VAR iStr        AS INT  NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.
DEFINE VARIABLE iCL AS INTEGER NO-UNDO.

DEF BUFFER bPkSdlLinje FOR PkSdlLinje.
DEFINE BUFFER clButiker FOR Butiker.

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAIL clButiker THEN DO:
  ocReturn = "Finner ikke sentrallager " + STRING(iCL) + ".".
  RETURN.
END.
  
FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL PkSdlLinje AND 
   DYNAMIC-FUNCTION("getCurrentAction" IN SOURCE-PROCEDURE) = "Create" THEN 
DO:
  fArtikkelnr = DEC(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"ArtikkelNr")).  
  IF fArtikkelnr = ? THEN RETURN.
  cStorl = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Storl").

  FIND FIRST PkSdlHode NO-LOCK 
       OF PkSdlLinje
       NO-ERROR.
  IF NOT AVAIL PkSdlHode THEN DO:
    ocReturn = "Finner ikke pakkseddelhode".
    RETURN.
  END.

  FIND FIRST Butiker NO-LOCK
     WHERE Butiker.Butik = PksdlLinje.ButikkNr
     NO-ERROR.
  
  IF PkSdlHode.CL <> clButiker.Butik THEN DO:
    ocReturn = "Pakkseddelhode mangler eller har feil referanse til sentrallager".
    RETURN.
  END.

  FIND FIRST ArtBas NO-LOCK
       WHERE ArtBas.ArtikkelNr = fArtikkelnr
       NO-ERROR.
  IF NOT AVAIL ArtBas THEN
    ocReturn = "Ugyldig artikkelnr".
  ELSE DO:
    FOR EACH bPkSdlLinje NO-LOCK
        WHERE bPkSdlLinje.PkSdlId = PkSdlHode.PkSdlId
        BY bPkSdlLinje.Linjenr DESC:
      iLnr = bPkSdlLinje.Linjenr + 1.
      LEAVE.
    END.

    FIND FIRST StrType NO-LOCK 
         OF ArtBas
         NO-ERROR.
    IF NOT AVAIL StrType THEN DO:
      ocReturn = "Artikkel mangler størrelsestype".
      RETURN.
    END.

    FIND StrKonv NO-LOCK WHERE
      StrKonv.Storl = cStorl NO-ERROR.
    IF AVAILABLE StrKonv THEN 
      iStr = StrKonv.StrKode.
    ELSE DO:
      ocReturn = "Ukjent størrelse. " + cStorl.
      RETURN.
    END.
    /*iStr = INTEGER(ENTRY(LOOKUP(cStorl,StrType.AlfaFordeling),StrType.Fordeling)) NO-ERROR.*/
            
    BUFFER-COPY ArtBas TO PkSdlLinje.

    ASSIGN PkSdlLinje.StrKode = iStr
           PkSdlLinje.Linjenr = iLnr
           .

    FIND FIRST PkSdlPris NO-LOCK OF PkSdlHode WHERE 
               PkSdlPris.ArtikkelNr = fArtikkelnr
               NO-ERROR.              
    /* Finnes ikke kalkylen, eller kalkylen er tom, skal den opprettes/oppdateres */
    IF NOT AVAIL PkSdlPris OR 
       (PkSdlPris.Pris + PkSdlPris.InnkjopsPris) = 0 THEN 
    DO:
      /* Pris på butikkens profil. */
      IF AVAILABLE Butiker THEN 
          FIND FIRST ArtPris NO-LOCK OF ArtBas
               WHERE ArtPris.ProfilNr = Butiker.ProfilNr
               NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK OF ArtBas
               WHERE ArtPris.ProfilNr = clButiker.ProfilNr
               NO-ERROR.
      IF NOT AVAIL ArtPris THEN DO:
        ocReturn = "Artpris mangler for butikkens profilnr".
        RETURN.
      END.

      CREATE PkSdlPris.
      BUFFER-COPY PkSdlHode  TO PkSdlPris.
      BUFFER-COPY PkSdlLinje TO PkSdlPris.
      
      ASSIGN
          PkSdlPris.InnkjopsPris    = ArtPris.InnkjopsPris[1]
          PkSdlPris.Rab1%           = ArtPris.Rab1%[1]
          PkSdlPris.Db%             = ArtPris.Db%[1]
          PkSdlPris.Pris            = ArtPris.Pris[1]
          PkSdlPris.NyPris          = ArtPris.Pris[1]
          PkSdlPris.NyVarekost      = ArtPris.Varekost[1]
          PkSdlPris.Varekost        = ArtPris.Varekost[1]
          PkSdlPris.NyRab1%         = ArtPris.Rab1%[1]
          PkSdlPris.NyDb%           = ArtPris.Db%[1]
          PkSdlPris.NyInnkjopsPris  = ArtPris.InnkjopsPris[1]
          PkSdlPris.Frakt           = ArtPris.Frakt[1]
          PkSdlPris.NyFrakt         = ArtPris.Frakt[1]
          .
      /* Legg til verdier fra artpris */
    END.

    /* Opprette bestilling, osv */ 
  END.
END.


