/* Samling av alle prosedyrer som kalkulerer verdier for BestHode under Ordre 
   Opprettet: 30.03.07 av BHa
-------------------------------------------------------------------*/   
PROCEDURE best_tot_rest:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  FOR FIRST BestHode FIELDS(TotAntPar TotInnLev TotMakulert) NO-LOCK
      WHERE ROWID(BestHode) = irBuffer:
    ocReturn = STRING(TotAntPar - TotInnLev - TotMakulert).
  END.
END PROCEDURE.

PROCEDURE kjedevare:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  FOR FIRST BestHode FIELDS() NO-LOCK
      WHERE ROWID(BestHode) = irBuffer
     ,FIRST VareBehLinje NO-LOCK
            WHERE VareBehLinje.VareBehNr = BestHode.VareBehNr
              AND VareBehLinje.ArtikkelNr = BestHode.ArtikkelNr:
    ocReturn = STRING(VareBehLinje.KjedeVare).
  END.
END PROCEDURE.

PROCEDURE gjennomfaktureres:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

  FOR FIRST BestHode FIELDS() NO-LOCK
      WHERE ROWID(BestHode) = irBuffer
     ,FIRST VareBehLinje NO-LOCK
            WHERE VareBehLinje.VareBehNr = BestHode.VareBehNr
              AND VareBehLinje.ArtikkelNr = BestHode.ArtikkelNr:
    ocReturn = STRING(VareBehLinje.Gjennomfaktureres).
  END.
END PROCEDURE.

