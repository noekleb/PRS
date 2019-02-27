TRIGGER PROCEDURE FOR CREATE OF StrekKode.

DEF BUFFER trgStrekkode FOR Strekkode.

ASSIGN
  StrekKode.RegistrertDato = TODAY
  StrekKode.RegistrertTid  = TIME
  StrekKode.RegistrertAV   = USERID("skotex")
  .
DO FOR trgStrekkode:
    /* Henter ERPNr fra annen strekkodepost på samme størrelse hvis det finnes fra før. */
    FIND FIRST trgSTrekkode NO-LOCK WHERE
      trgStrekkode.ArtikkelNr = Strekkode.ArtikkelNr AND
      trgSTrekkode.StrKode    = Strekkode.StrKode AND
      RECID(trgStrekkode)    <> recid(Strekkode) NO-ERROR.
    IF AVAILABLE trgStrekkode AND
      TRIM(trgSTrekkode.ERPNr) <> '' AND
      Strekkode.ERPNr = '' THEN
      ASSIGN Strekkode.ERPNr = trgSTrekkode.ERPNr.  
    IF AVAILABLE trgStrekkode THEN RELEASE trgStrekkode.
END.
  
/* Initierer ERPNr hvis det er blankt. */ 
/* 
if (Strekkode.StrKode <> 0 and 
    Strekkode.ArtikkelNr <> 0 and 
    Strekkode.ERPNr = '') then
  assign Strekkode.ERPNr = string(STrekkode.ArtikkelNr) + trim(string(Strekkode.StrKode,">>999")).
*/


