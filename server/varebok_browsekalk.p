/* Samling av alle prosedyrer som kalkulerer verdier for varebok
   Opprettet: 03.01.06 av BHa
-------------------------------------------------------------------*/   
{incl/weeknum.i}
                                 
PROCEDURE vareboklinje_levuke1:
  DEF INPUT PARAM irVarebokLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  DEF VAR oiWeek AS INT NO-UNDO.
  DEFINE VARIABLE dStart AS DATE NO-UNDO.

  FIND VarebokLinje WHERE ROWID(VarebokLinje) = irVarebokLinje NO-LOCK NO-ERROR.
  IF AVAIL VarebokLinje THEN DO:

    RUN weeknum (VarebokLinje.LevDato1,OUTPUT oiWeek).
    ocValue = STRING(oiWeek).
  END.

  IF ocValue = ? THEN ocValue = "".
END PROCEDURE.

PROCEDURE vareboklinje_levuke2:
  DEF INPUT PARAM irVarebokLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  DEF VAR oiWeek AS INT NO-UNDO.
  DEFINE VARIABLE dStart AS DATE NO-UNDO.

  FIND VarebokLinje WHERE ROWID(VarebokLinje) = irVarebokLinje NO-LOCK NO-ERROR.
  IF AVAIL VarebokLinje THEN DO:

    RUN weeknum (VarebokLinje.LevDato2,OUTPUT oiWeek).
    ocValue = STRING(oiWeek).
  END.

  IF ocValue = ? THEN ocValue = "".
END PROCEDURE.

PROCEDURE vareboklinje_levuke3:
  DEF INPUT PARAM irVarebokLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  DEF VAR oiWeek AS INT NO-UNDO.
  DEFINE VARIABLE dStart AS DATE NO-UNDO.

  FIND VarebokLinje WHERE ROWID(VarebokLinje) = irVarebokLinje NO-LOCK NO-ERROR.
  IF AVAIL VarebokLinje THEN DO:

    RUN weeknum (VarebokLinje.LevDato3,OUTPUT oiWeek).
    ocValue = STRING(oiWeek).
  END.

  IF ocValue = ? THEN ocValue = "".
END PROCEDURE.

PROCEDURE vareboklinje_levuke4:
  DEF INPUT PARAM irVarebokLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

  DEF VAR oiWeek AS INT NO-UNDO.
  DEFINE VARIABLE dStart AS DATE NO-UNDO.

  FIND VarebokLinje WHERE ROWID(VarebokLinje) = irVarebokLinje NO-LOCK NO-ERROR.
  IF AVAIL VarebokLinje THEN DO:

    RUN weeknum (VarebokLinje.LevDato4,OUTPUT oiWeek).
    ocValue = STRING(oiWeek).
  END.

  IF ocValue = ? THEN ocValue = "".
END PROCEDURE.


PROCEDURE varebok_tilgjsort:
  DEF INPUT  PARAM irVarebokLinje  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

  DEF VAR fArtikkelnr AS DEC NO-UNDO.
  DEF VAR fVareboknr  AS DEC NO-UNDO.
  DEF VAR fMessenr    AS DEC NO-UNDO.

  FOR EACH VarebokLinje FIELDS() NO-LOCK
      WHERE ROWID(VarebokLinje) = irVarebokLinje
      ,FIRST ArtBas FIELDS() OF VarebokLinje NO-LOCK
      ,FIRST LevSort WHERE LevSort.LevNr GE 0 AND LevSort.StrTypeId = ArtBas.StrTypeId NO-LOCK:
    ocReturn = "*".
  END.

END PROCEDURE.

PROCEDURE varebok_valgtsort:
  DEF INPUT  PARAM irVarebokLinje  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

  DEF VAR fArtikkelnr AS DEC NO-UNDO.
  DEF VAR fVareboknr  AS DEC NO-UNDO.
  DEF VAR fMessenr    AS DEC NO-UNDO.

  FOR EACH VarebokLinje FIELDS() NO-LOCK
      WHERE ROWID(VarebokLinje) = irVarebokLinje
      ,FIRST ArtSort WHERE ArtSort.ArtikkelNr = VarebokLinje.ArtikkelNr NO-LOCK:
    ocReturn = "*".
  END.
END PROCEDURE.

PROCEDURE vareboklinje_endrettid:
  DEF INPUT  PARAM iiTime         AS INT  NO-UNDO.
  DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue        AS CHAR NO-UNDO.

  ocValue = STRING(iiTime,"HH:MM").
END PROCEDURE.

PROCEDURE artikkel_varebok_messe:
  DEF INPUT  PARAM irVarebokLinje  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

  DEF VAR fArtikkelnr AS DEC NO-UNDO.
  DEF VAR fVareboknr  AS DEC NO-UNDO.
  DEF VAR fMessenr    AS DEC NO-UNDO.

  DEF BUFFER bVarebokLinje FOR VarebokLinje.

  FIND bVarebokLinje WHERE ROWID(bVarebokLinje) = irVarebokLinje NO-LOCK NO-ERROR.
  IF NOT AVAIL bVarebokLinje THEN RETURN.

  FIND FIRST VarebokHode OF bVarebokLinje NO-LOCK NO-ERROR.
  IF NOT AVAIL VarebokHode THEN RETURN.

  ASSIGN fArtikkelnr = bVarebokLinje.ArtikkelNr
         fVarebokNr  = bVarebokLinje.VarebokNr
         fMessenr    = VarebokHode.MesseNr
         .

  FOR EACH VareBokHode FIELDS() NO-LOCK
      WHERE VareBokHode.MesseNr   = fMessenr
        AND VareBokHode.VareBokNr NE fVareBokNr,
      FIRST VareBokLinje FIELDS(VareBokNr) OF VareBokHode NO-LOCK
            WHERE VareBokLinje.ArtikkelNr = fArtikkelNr
      :
    ocReturn = ocReturn + STRING(VareBokHode.VareBokNr) + ",".
  END.
  ocReturn = TRIM(ocReturn,",").
END PROCEDURE.

PROCEDURE artbas_varefaktaind:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

  FIND ArtBas NO-LOCK 
       WHERE ROWID(ArtBas) = irArtBas
       NO-ERROR.
  IF AVAIL ArtBas AND ArtBas.VareFakta NE "" THEN
    ocReturn = "*".
END PROCEDURE.

PROCEDURE vareboklinje_rgb:
  DEF INPUT  PARAM irVarebokLinje  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

  DEF VAR iColorEntry AS INT NO-UNDO.

  FIND VarebokLinje WHERE ROWID(VarebokLinje) = irVarebokLinje NO-LOCK NO-ERROR.

  IF AVAIL VarebokLinje THEN DO:
    FIND FIRST VarebokHode OF VarebokLinje NO-LOCK.
    FIND FIRST Messe OF VarebokHode NO-LOCK NO-ERROR.
    IF AVAIL Messe THEN DO:
      iColorEntry = LOOKUP(VarebokLinje.LinjeMerknad,Messe.Oppmerking,"¤").
      IF iColorEntry NE 0 AND NUM-ENTRIES(Messe.Fargekoder) GE iColorEntry THEN
        ocReturn = ENTRY(iColorEntry,Fargekoder).
    END.
  END.

END PROCEDURE.
