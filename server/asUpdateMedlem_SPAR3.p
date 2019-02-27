DEFINE INPUT  PARAMETER cPersonNr AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cMobil    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cEmail    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cMelding  AS CHARACTER   NO-UNDO.

cMelding = "Finner inte medlem".

FOR EACH medlem WHERE Medlem.PersonNr = cPersonNr:
    cMelding = "Medlem uppdaterad".
    ASSIGN Medlem.ePostAdresse = cEmail
           Medlem.MobilTlf     = cMobil NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        cMelding = "Uppdatering misslyckades".
END.
