DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icAction    AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR fAntBest     AS DEC NO-UNDO.
DEF VAR fAntLevert   AS DEC NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) <> "AntLevert" THEN
  ASSIGN
    ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("AntLevert"):BUFFER-VALUE. 

ihBuffer:BUFFER-FIELD("AntRest"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE     
                                              - ihBuffer:BUFFER-FIELD("AntLevert"):BUFFER-VALUE. 

/*
FIND PkSdlLinje EXCLUSIVE-LOCK WHERE
    PkSdlLinje.PkSdlId      = dec(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) AND
    PkSdlLinje.PkSdlLinjeId = int(ihBuffer:BUFFER-FIELD("PkSdlLinjeId"):BUFFER-VALUE) NO-ERROR. 
IF AVAIL PkSdlLinje THEN DO:
  FIND FIRST StrKonv NO-LOCK
       WHERE StrKonv.StrKode = PkSdlLinje.StrKode
       NO-ERROR.
  IF AVAIL StrKonv THEN DO:
    /* Antall levert  tidligere */
    FOR EACH BestLevert NO-LOCK
        WHERE BestLevert.BestNr = PkSdlLinje.BestNr
          AND BestLevert.Butik  = PkSdlLinje.ButikkNr
          AND TRIM(BestLevert.Storl) = TRIM(StrKonv.Storl)
        :
      ASSIGN
          fAntLevert = fAntLevert + BestLevert.Levert.    
    END.

    /* Antall bestillt */
    FOR EACH BestStr NO-LOCK
        WHERE BestStr.BestNr = PkSdlLinje.BestNr
          AND BestStr.Butik  = PkSdlLinje.ButikkNr
          AND TRIM(BestStr.Storl) = TRIM(StrKonv.Storl)
        BY BestStr.BestStat DESC
        :
      fAntBest = fAntBest + BestStr.Bestilt.
      LEAVE.
    END.
  END.

  /* Beregner restantall. */
  ihBuffer:BUFFER-FIELD("AntRest"):BUFFER-VALUE = fAntBest 
                                                - fAntLevert
                                                - ihBuffer:BUFFER-FIELD("AntLevert"):BUFFER-VALUE.
END.
*/

