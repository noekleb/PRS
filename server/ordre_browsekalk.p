/* Samling av alle prosedyrer som kalkulerer verdier for ordre under vareh.bok supplering 
   Opprettet: 08.05.06 av BHa
-------------------------------------------------------------------*/   
PROCEDURE ordre_tot_antall:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  DEF VAR fTotAnt AS DEC NO-UNDO.

  FOR FIRST Ordre FIELDS(OrdreNr) NO-LOCK
      WHERE ROWID(Ordre) = irBuffer
      ,EACH BestHode FIELDS(TotAntPar) NO-LOCK
            OF Ordre:
    fTotAnt = fTotAnt + TotAntPar.
  END.

  ocReturn = STRING(fTotAnt).
END PROCEDURE.

PROCEDURE ordre_tot_pris:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  DEF VAR fTotAnt AS DEC NO-UNDO.

  FOR FIRST Ordre FIELDS(OrdreNr) NO-LOCK
      WHERE ROWID(Ordre) = irBuffer
      ,EACH BestHode FIELDS(TotInnkjVerdi) NO-LOCK
            OF Ordre:
    fTotAnt = fTotAnt + TotInnkjVerdi.
  END.

  ocReturn = STRING(fTotAnt).
END PROCEDURE.

PROCEDURE ordre_tot_dbkr:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  DEF VAR fTotAnt AS DEC NO-UNDO.

  FOR FIRST Ordre FIELDS(OrdreNr) NO-LOCK
      WHERE ROWID(Ordre) = irBuffer
      ,EACH BestHode FIELDS(TotDbKr) NO-LOCK
            OF Ordre:
    fTotAnt = fTotAnt + TotDbKr.
  END.

  ocReturn = STRING(fTotAnt).

END PROCEDURE.

PROCEDURE ordre_tot_levert:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  DEF VAR fTotAnt AS DEC NO-UNDO.

  FOR FIRST Ordre FIELDS(OrdreNr) NO-LOCK
      WHERE ROWID(Ordre) = irBuffer
      ,EACH BestHode FIELDS(TotInnLev) NO-LOCK
            OF Ordre:
    fTotAnt = fTotAnt + TotInnLev.
  END.

  ocReturn = STRING(fTotAnt).
END PROCEDURE.

PROCEDURE ordre_tot_rest:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  DEF VAR fTotAnt AS DEC NO-UNDO.

  FOR FIRST Ordre FIELDS(OrdreNr) NO-LOCK
      WHERE ROWID(Ordre) = irBuffer
      ,EACH BestHode FIELDS(TotAntPar TotInnLev TotMakulert) NO-LOCK
            OF Ordre:
    fTotAnt = fTotAnt + TotAntPar - TotInnLev - TotMakulert.
  END.

  ocReturn = STRING(fTotAnt).
END PROCEDURE.

PROCEDURE ordre_for_butikk:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

  DEF VAR bMatch AS LOG NO-UNDO.

  DEF VAR iBestStat    AS INT  NO-UNDO.
  DEF VAR cButikkListe AS CHAR NO-UNDO.

  IF icParam = "¤*" THEN RETURN.   /* Ikke statusfilter, alle butikker */
  ELSE 
    ASSIGN iBestStat    = INT(ENTRY(1,icParam,"¤"))
           cButikkListe = REPLACE(ENTRY(2,icParam,"¤"),CHR(1),",").

  FIND Ordre WHERE ROWID(Ordre) = irBuffer NO-LOCK NO-ERROR.
  IF AVAIL Ordre THEN DO:
    FOR EACH BestHode NO-LOCK OF Ordre
        WHERE (IF iBestStat = 4 THEN BestHode.BestStat = iBestStat AND BestHode.BekreftetDato = ?
               ELSE IF iBestStat = 44 THEN BestHode.BestStat = 4 AND BestHode.BekreftetDato NE ?
               ELSE IF iBestStat > 0 THEN BestHode.BestStat = iBestStat
               ELSE TRUE)
       ,EACH BestLinje OF BestHode NO-LOCK
             WHERE CAN-DO(cButikkListe,STRING(BestLinje.Butik)):
        bMatch = YES.
        LEAVE.
    END.
  END.
  IF NOT bMatch THEN ocReturn = "skiprow".
END PROCEDURE.

PROCEDURE kjedevare_filter:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

  DEF VAR bMatch AS LOG NO-UNDO.

  IF icParam = "" THEN RETURN.

  FIND Ordre WHERE ROWID(Ordre) = irBuffer NO-LOCK NO-ERROR.
  IF AVAIL Ordre THEN DO:
    FOR EACH BestHode NO-LOCK OF Ordre
       ,FIRST VarebehLinje NO-LOCK
              WHERE VareBehLinje.VareBehNr = Ordre.VareBehNr
                AND VareBehLinje.ArtikkelNr = BestHode.ArtikkelNr
                AND VareBehLinje.KjedeVare  = LOGICAL(icParam):
      bMatch = YES.
      LEAVE.
    END.
  END.
  IF NOT bMatch THEN ocReturn = "skiprow".
END PROCEDURE.

PROCEDURE gjennomfaktureres_filter:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

  DEF VAR bMatch AS LOG NO-UNDO.

  IF icParam = "" THEN RETURN.

  FIND Ordre WHERE ROWID(Ordre) = irBuffer NO-LOCK NO-ERROR.
  IF AVAIL Ordre THEN DO:
    FOR EACH BestHode NO-LOCK OF Ordre
       ,FIRST VarebehLinje NO-LOCK
              WHERE VareBehLinje.VareBehNr = Ordre.VareBehNr
                AND VareBehLinje.ArtikkelNr = BestHode.ArtikkelNr
                AND VareBehLinje.Gjennomfaktureres = LOGICAL(icParam):
      bMatch = YES.
      LEAVE.
    END.
  END.
  IF NOT bMatch THEN ocReturn = "skiprow".
END PROCEDURE.
