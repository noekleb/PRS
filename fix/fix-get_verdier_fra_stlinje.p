
DEF VAR pcPerId AS CHAR NO-UNDO.
DEF VAR pcStTypeId AS CHAR NO-UNDO.
DEF VAR pdFraDato AS DATE NO-UNDO.
DEF VAR pdTilDato AS DATE NO-UNDO.
DEF VAR piFraAarPerLinNr AS INT NO-UNDO.
DEF VAR piTilAarPerLinNr AS INT NO-UNDO.
DEF VAR piButik AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 300.

ASSIGN
  pdFraDato  = 12/24/2011
  pdtilDato  = pdFraDato
  pibutik    = 716
  pcPerId    = 'DAG'
  pcStTypeId = 'AVDELING'
  piFraAarPerLinNr = INT(
                         STRING(YEAR(pdFraDato),"9999") + 
                         STRING(pdFraDato - DATE(12 , 31, YEAR(pdFraDato) - 1),"999")
                        )
  piTilAarPerLinNr = INT(
                         STRING(YEAR(pdTilDato),"9999") + 
                         STRING(pdTilDato - DATE(12 , 31, YEAR(pdTilDato) - 1),"999")
                        )
  .

MESSAGE piButik SKIP
  pcStTypeId SKIP
  pcPerId SKIP
  piFraAarPerLinNr SKIP
  piFraAarPerLinNr SKIP
  pdFraDato pdtilDato
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

FOR EACH StLinje NO-LOCK WHERE
  StLinje.Butik    = piButik AND
  StLinje.StTypeId = pcStTypeId AND
  StLinje.PerId    = pcPerId AND
  StLinje.AarPerLinNr >= piFraAarPerLinNr AND
  StLinje.AarPerLinNr <= piTilAarPerLinNr 
  :

  DISPLAY
    StLinje.DataObjekt
    StLinje.Butik
    StLinje.StTypeId
    StLinje.PerId
    StLinje.AarPerLinNr
    (StLinje.VerdiSolgt - StLinje.VVareKost) COLUMN-LABEL 'DbKr' (TOTAL)
    StLinje.VerdiSolgt (TOTAL)
    StLinje.MvaVerdi (TOTAL)
    StLinje.VVareKost (TOTAL)
    WITH WIDTH 300.

END.


