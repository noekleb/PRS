/* bibl_journalfiladmin.p            */
/* Rutinen slår sammen journalfiler. */

  DEFINE VARIABLE iCount        AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFil          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFilPath      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFilMask      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lFirstRad     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFilRad       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cBkupDir      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAppendFil    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dDag          AS DATE       NO-UNDO.
  DEFINE VARIABLE cKatalogListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKasse        AS CHARACTER  NO-UNDO.

  DEFINE STREAM sLesKatalog.
  DEFINE STREAM sLesFil.
  DEFINE STREAM sAppendFil.
    
  RUN bibl_getKatalogListeKasse.p (OUTPUT cKatalogListe).
  
  DO iCount = 1 TO NUM-ENTRIES(cKatalogListe):
      IF TRIM(ENTRY(iCount,cKatalogListe)) = "" THEN
          NEXT.
      ASSIGN cFilMask = ENTRY(iCount,cKatalogListe) + "\" + "OPPD-journal*"
             cBkupDir = ENTRY(iCount,cKatalogListe) + "\" + "bku".
      
      OS-CREATE-DIR VALUE(cBkupDir).

      INPUT STREAM sLesKatalog FROM OS-DIR(ENTRY(iCount,cKatalogListe)) NO-ATTR-LIST.
      REPEAT:
          IMPORT STREAM sLesKatalog
            cFil  
            cFilPath.
          IF cFil BEGINS "OPPD-JOURNAL" THEN 
          DO:
              ASSIGN lFirstRad = FALSE.
              INPUT STREAM sLesFil FROM VALUE(cFilPath).
              REPEAT:
                  IMPORT STREAM sLesFil UNFORMATTED cFilRad.
                  IF NOT NUM-ENTRIES(cFilRad,";") > 3 THEN
                      NEXT.
                  ASSIGN dDag =  DATE(ENTRY(3,cFilRad,";")).
                  IF lFirstRad = FALSE THEN DO:
                             cAppendFil = cBkupDir + "\" + "journal." + 
                             STRING(YEAR(dDag)) + STRING(MONTH(dDag),"99") + STRING(DAY(dDag),"99")
                             + "." + ENTRY(1,cFilRad,";").
                      OUTPUT STREAM sAppendFil TO VALUE(cAppendFil) APPEND.
                      ASSIGN lFirstRad = TRUE.
                  END.
                  PUT STREAM sAppendFil UNFORMATTED cFilRad SKIP.
              END.
              INPUT STREAM sLesFil CLOSE.
              OUTPUT STREAM sAppendFil CLOSE.
              OS-DELETE VALUE(cFilPath).
          END.
          ELSE IF cFil BEGINS 'oppd-PRSjournal' THEN 
          DO:
              ASSIGN lFirstRad = FALSE
                     cKasse    = ENTRY(2,cFil,'_').
              INPUT STREAM sLesFil FROM VALUE(cFilPath).
              REPEAT:
                  IMPORT STREAM sLesFil UNFORMATTED cFilRad.
                  IF NOT NUM-ENTRIES(cFilRad,";") > 3 THEN
                      NEXT.
                  IF lFirstRad = FALSE THEN DO:
                             cAppendFil = cBkupDir + "\" + "PRSPosJournal_" + cKasse + "_" + 
                             ENTRY(5,cFilRad,";")
                             + "." + ENTRY(2,cFil,'.').
                      OUTPUT STREAM sAppendFil TO VALUE(cAppendFil) APPEND.
                      ASSIGN lFirstRad = TRUE.
                  END.
                  PUT STREAM sAppendFil UNFORMATTED cFilRad SKIP.
              END.
              INPUT STREAM sLesFil CLOSE.
              OUTPUT STREAM sAppendFil CLOSE.
              OS-DELETE VALUE(cFilPath).
          END.
      END.
      INPUT STREAM sLesKatalog CLOSE.
  END.
