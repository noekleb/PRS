/*
  TN 7/10-03
  rutine som importerer varestruktur fra StorePoint.
*/

DEF VAR cLinje  AS CHAR NO-UNDO.
DEF VAR cFelt1  AS CHAR NO-UNDO.
DEF VAR cFelt2  AS CHAR NO-UNDO.
DEF VAR cFelt3  AS CHAR NO-UNDO.
DEF VAR cFelt4  AS CHAR NO-UNDO.
DEF VAR cFelt5  AS CHAR NO-UNDO.
DEF VAR cFelt6  AS CHAR NO-UNDO.
DEF VAR cFelt7  AS CHAR NO-UNDO.
DEF VAR cFelt8  AS CHAR NO-UNDO.
DEF VAR cFelt9  AS CHAR NO-UNDO.
DEF VAR cFelt10 AS CHAR NO-UNDO.

DEF VAR piLoop    AS INT NO-UNDO.
DEF VAR piMvaKode AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 200.

DEF STREAM InnFil.
DEF STREAM UtFil.

/* Renser alle varegrupper */
FOR EACH VarGr:
    FOR EACH VgKat OF VarGr:
        DELETE VgKat.
    END.
    DELETE VarGr.
END.
/* Renser alle hovedgrupper */
FOR EACH HuvGr:
    DELETE HuvGr.
END.
/* Renser alle avdelinger */
FOR EACH Avdeling:
    DELETE Avdeling.
END.

INPUT STREAM InnFil FROM VALUE("c:\home\lindbak\ankommet\varehierarki.sdv") NO-ECHO.
LESFRAFIL:
REPEAT :
  IMPORT STREAM InnFil DELIMITER ";" 
      cFelt1 
      cFelt2 
      cFelt3 
      cFelt4 
      cFelt5 
      cFelt6 
      cFelt7 
      cFelt8 
      cFelt9 
      cFelt10 
      .
  ASSIGN
      cFelt2 = TRIM(cFelt2)
      cFelt4 = TRIM(cFelt4) 
      cFelt6 = TRIM(cFelt6) 
      .
  /* Omforming av MvaKode    */
  /* iVat1Id = Moms 25%  = 1 */
  /* iVat2Id = Moms 12%  = 6 */
  /* iVat3Id = Moms 6%   = 7 */
  /* iVat4Id = Moms 0%   = 4 */
  IF (cFelt7) = "1" THEN
      piMvaKode = 1.
  ELSE IF (cFelt8) = "1" THEN
      piMvaKode = 6.
  ELSE IF (cFelt9) = "1" THEN
      piMvaKode = 7.
  ELSE IF (cFelt10) = "1" THEN
      piMvaKode = 4.

  /* Sjekk på gyldig avdeling */
  IF INT(cFelt5) = 0 THEN
  DO:
      MESSAGE 
          cFelt1 
          cFelt2 
          cFelt3 
          cFelt4 
          cFelt5 
          cFelt6 
          cFelt7 
          cFelt8 
          cFelt9 
          cFelt10 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      NEXT LESFRAFIL.
  END.
  /* Oppretter Avdeling */
  FIND Avdeling EXCLUSIVE-LOCK WHERE
      Avdeling.AvdelingNr = INT(cFelt5) NO-ERROR.
  IF NOT AVAILABLE Avdeling THEN
  DO:
      CREATE Avdeling.
      ASSIGN
          Avdeling.AvdelingNr   = INT(cFelt5)
          .
  END.
  ASSIGN
      Avdeling.AvdelingNavn = cFelt6
      .
  /* Oppretter hovedgrupper */
  FIND HuvGr EXCLUSIVE-LOCK WHERE
      HuvGr.Hg = INT(cFelt3) NO-ERROR.
  IF NOT AVAILABLE HuvGr THEN
  DO:
      CREATE HuvGr.
      ASSIGN
          HuvGr.Hg         = INT(cFelt3)
          .
  END.
  ASSIGN
      HuvGr.HgBeskr    = cFelt4
      HuvGr.AvdelingNr = INT(cFelt5)
      .
  /* Oppretter varegrupper */
  FIND VarGr EXCLUSIVE-LOCK WHERE
      VarGr.Vg = INT(cFelt1) NO-ERROR.
  IF NOT AVAILABLE VarGr THEN
  DO:
      CREATE VarGr.
      ASSIGN
          VarGr.Vg          = INT(cFelt1)
          .
      piLoop = 0.
      FOR EACH Kategori NO-LOCK:
          piLoop = piLoop + 1.
          FIND VgKat NO-LOCK WHERE
              VgKat.Vg    = VarGr.Vg AND
              VgKat.VgKat = piLoop AND
              VgKat.KatNr = Kategori.KatNr NO-ERROR.
          IF NOT AVAILABLE VgKat THEN
          DO:
              CREATE VgKat.
              ASSIGN
                  VgKat.Vg    = VarGr.Vg 
                  VgKat.VgKat = piLoop 
                  VgKat.KatNr = Kategori.KatNr 
                  .
          END.
      END.
  END.

  ASSIGN
      VarGr.VgBeskr     = cFelt2
      VarGr.MomsKod     = piMvaKode
      VarGr.Hg          = INT(cFelt3)
      VarGr.Kost_Proc   = 65.0
      .
END. /* LESFRAFIL */

OUTPUT STREAM UtFil CLOSE.
INPUT STREAM InnFil CLOSE.
