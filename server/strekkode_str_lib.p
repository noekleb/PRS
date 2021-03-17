/* strekkode_str_lib.p
   Funksjoner for å finne rett strekkkode, størrelse,
  
   Opprettet 03.05.07 av brynjar@chemistry.no
------------------------------------------------------------------------------*/
                                       
FUNCTION getGyldigStrekkode RETURNS CHARACTER 
         (INPUT ifArtikkelnr AS DEC,
          INPUT iiStrKode    AS INT,
          INPUT icExtra      AS CHAR):

DEF VAR cStrekkode  AS CHAR NO-UNDO.

FIND StrekKode NO-LOCK
     WHERE StrekKode.ArtikkelNr = ifArtikkelNr
       AND StrekKode.StrKode    = iiStrKode
     NO-ERROR.
IF AMBIGUOUS Strekkode THEN DO:
  FOR EACH StrekKode NO-LOCK
      WHERE StrekKode.ArtikkelNr = ifArtikkelNr
        AND StrekKode.StrKode    = iiStrKode
        AND NOT StrekKode.Kode   BEGINS "02"
      BY DEC(StrekKode.Kode):
    cStrekkode = StrekKode.Kode.
  END.
  IF cStrekkode = "" THEN DO:
    FIND FIRST StrekKode NO-LOCK
         WHERE StrekKode.ArtikkelNr = ifArtikkelNr
           AND StrekKode.StrKode    = iiStrKode
           AND StrekKode.Kode       BEGINS "02"
           AND LENGTH(StrekKode.Kode) = 13
         NO-ERROR.
    IF AVAIL StrekKode THEN
      cStrekkode = StrekKode.Kode.
  END.
END.
ELSE IF AVAIL strekkode THEN DO: 
    IF (StrekKode.Kode BEGINS "02" AND LENGTH(StrekKode.Kode) = 13) OR NOT StrekKode.Kode BEGINS "02" THEN
      cStrekkode = StrekKode.Kode.
END.

RETURN cStrekkode.

END FUNCTION.
