TRIGGER PROCEDURE FOR WRITE OF TelleHode OLD BUFFER OLDTelleHode.

IF TelleHode.TBId = 0 THEN
    TelleHode.TBId = 1.

ASSIGN
    TelleHode.LokasjonsId = ''.
