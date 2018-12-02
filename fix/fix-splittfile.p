/* fix-splitfile.p */

DEF VAR cInnFil  AS CHAR NO-UNDO.
DEF VAR cUtFil   AS CHAR NO-UNDO.
DEF VAR cKatalog AS CHAR NO-UNDO.
DEF VAR cLinje   AS CHAR NO-UNDO.
DEF VAR iAntRec  AS INT  NO-UNDO.
DEF VAR iantall  AS INT  NO-UNDO.
DEF VAR iSeqNr   AS INT  NO-UNDO.
DEF VAR lFlagg   AS LOG  NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.

ASSIGN
    cKatalog = 'C:\ArkivDokument\Kunder\Anton\Cognito\'
    cInnFil  = 'Kredk.csv'
    cUtFil   = cInnFil
    iAntRec  = 1000
    iSeqNr   = 1
    lFlagg   = TRUE
    .

INPUT STREAM inn FROM VALUE(cKatalog + cInnfil) NO-ECHO.
INNLES:
REPEAT:
  IMPORT STREAM Inn UNFORMATTED cLinje NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
  DO:
      MESSAGE 'Feil i import. Import avbrutt.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      LEAVE INNLES.
  END.
  ELSE iAntall = iAntall + 1.

  IF lflagg THEN DO:
      OUTPUT STREAM Ut TO VALUE(cKatalog + ENTRY(1,cUtFil,'.') + 
                                 '_' + STRING(iSeqNr) + '.' +
                                 ENTRY(2,cUtFil,'.')
                                 ) APPEND.
      ASSIGN lFlagg  = FALSE.
  END.
  
  IF (iAntall MODULO iAntRec = 0) THEN
  DO:
      IF lFlagg = FALSE THEN OUTPUT STREAM Ut CLOSE.
      ASSIGN
        iAntall = 0
        iSeqNr  = iSeqNr + 1
        .
      OUTPUT STREAM Ut TO VALUE(cKatalog + ENTRY(1,cUtFil,'.') + 
                                '_' + STRING(iSeqNr) + '.' +
                                ENTRY(2,cUtFil,'.')
                                ) APPEND.
  END.

  PUT STREAM Ut UNFORMATTED cLinje SKIP.

END. /* INNLES */
OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.
