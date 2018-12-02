DEF VAR cInnFil AS CHAR NO-UNDO.
DEF VAR cUtFil AS CHAR NO-UNDO.

DEF VAR iStrKode AS INT NO-UNDO.
DEF VAR cStorl   AS CHAR NO-UNDO.
DEF VAR iSport1StrKode AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.

DEF STREAM Inn.
DEF STREAM Ut.

ASSIGN
    cInnFil = 'Anton_Storrelser.csv'
    cUtFil  = 'Anton_Storrelser_sport1.csv'
    .


INPUT STREAM Inn FROM VALUE(cInnFil).
OUTPUT STREAM Ut TO VALUE(cUtFil).

REPEAT:
  IMPORT STREAM Inn DELIMITER ';' 
      iStrKode
      cStorl
      .

  FIND StrKonv NO-LOCK WHERE
      StrKonv.Storl = cStorl NO-ERROR.
  IF AVAILABLE StrKonv THEN
      iSport1StrKode = StrKonv.StrKode.
  ELSE
      iSport1StrKode = 0.

  DISPLAY iStrKode cStorl iSport1StrKode
      WITH WIDTH 250.

  PUT STREAM Ut UNFORMATTED
      iStrKode ";"
      cStorl ";"
      iSport1StrKode
      SKIP.
END.

OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.

