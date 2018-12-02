DEF INPUT  PARAM ifReskontro_id   AS DEC  NO-UNDO.
DEF INPUT  PARAM icSessionId      AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue          AS CHAR NO-UNDO.

DEF VAR fSaldo  AS DEC NO-UNDO.

FOR EACH KundeResKobling NO-LOCK
    WHERE DReskontro_id = ifReskontro_id 
      AND RegistrertDato GE TODAY - 1
    ,FIRST Kundereskontr NO-LOCK
          WHERE Kundereskontr.Reskontro_id = KundeResKobling.KReskontro_id
            AND Kundereskontr.BilagsType = 3
     BY KundeResKobling.Dato:
  ocValue = STRING(KundeResKobling.Dato).
END.

