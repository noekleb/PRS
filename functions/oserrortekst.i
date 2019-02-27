FUNCTION OsErrorTekst RETURNS CHARACTER
  ( INPUT wErrNum AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  Returnerer feiltekst fra OS-ERROR
    Notes:  
------------------------------------------------------------------------------*/
      CASE wErrNum:
         WHEN 0   THEN RETURN "Ingen feil (0)".
         WHEN 1   THEN RETURN "Ikke eier (1)".
         WHEN 2   THEN RETURN "Ingen slik fil eller mappe (2)".
         WHEN 3   THEN RETURN "Avbrutt systemkall (3)".
         WHEN 4   THEN RETURN "I/O feil (4)".
         WHEN 5   THEN RETURN "Feil filnummer (5)".
         WHEN 6   THEN RETURN "Ingen prosesser igjen (6)".
         WHEN 7   THEN RETURN "Ikke nok kjerneminne (7)".
         WHEN 8   THEN RETURN "Adgang nektet (8)".
         WHEN 9   THEN RETURN "Feil adresse (9)".
         WHEN 10  THEN RETURN "Filen finnes (10)".
         WHEN 11  THEN RETURN "Ugyldig enhet (11)".
         WHEN 12  THEN RETURN "Ikke en mappe (12)".
         WHEN 13  THEN RETURN "Er en mappe (13)".
         WHEN 14  THEN RETURN "Filtabell overfylt (14)".
         WHEN 15  THEN RETURN "For mange åpne filer (15)".
         WHEN 16  THEN RETURN "Filen er for stor (16)".
         WHEN 17  THEN RETURN "Ikke mer plass igjen på enheten (17)".
         WHEN 18  THEN RETURN "Mappen er ikke tom (18)".
         WHEN 999 THEN RETURN "Ukjent feil (999)".
         OTHERWISE RETURN "".
      END CASE.

END FUNCTION.
