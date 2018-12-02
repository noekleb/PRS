CURRENT-WINDOW:WIDTH = 300.

DEF VAR cStorl AS CHAR NO-UNDO.
DEF VAR cNyStorl AS CHAR NO-UNDO.
DEF VAR ocReturn AS CHAR NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.

DEF BUFFER bufStrTStr FOR StrTStr. 

FOR EACH ArtBAs WHERE ArtBAs.LopNr = ?:
  FOR EACH BestHode OF ArtBas,
      EACH BestSort OF BestHode,
      EACH BestStr OF BestHode:

      ASSIGN
          cStorl = TRIM(BestSTr.Storl).
      ASSIGN 
          cNyStorl = ENTRY(LOOKUP(cStorl, BestSort.Storrelser, ' ') - 2, BestSort.Storrelser, ' ').

      /* Korrigerer størrelse på bestilling */
      /* VIKTIG: størrelsene skal IKKE formateres med ledende space */
      BestStr.Storl = cNyStorl.
  END.
END.
