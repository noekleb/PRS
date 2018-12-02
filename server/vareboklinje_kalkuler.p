/* varebok_kalkuler.p: Kalkuler en vareboklinje
   Kalles fra: - newvareboklinje.p  
               - update_vareboklinje.p
               - vareboklinje_justermange.p
                
   Opprettet 02.09.04 av BHa     
   Endret    16.03.07 av BHa: Lagt tilbake oppdatering av andre varebøker for messe
   Endret    17.09.07 av BHa: Nå skal også merknadskoden kopieres fra varebok til varebok
-----------------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBuffVarebokLinje AS HANDLE NO-UNDO.
DEF INPUT PARAM icChangedField    AS CHAR NO-UNDO.

DEF VAR fPrisExMVA     AS DEC NO-UNDO.
DEF VAR fMesseNr       AS DEC NO-UNDO.
DEF VAR bUpdateVarebeh AS LOG NO-UNDO INIT TRUE.
DEF VAR ocReturn       AS CHAR NO-UNDO.
DEF VAR obOK           AS LOG NO-UNDO.

DEF BUFFER bVarebokLinje FOR VarebokLinje.

FIND VarebokLinje WHERE ROWID(VarebokLinje) = ihBuffVarebokLinje:ROWID EXCLUSIVE-LOCK.

fPrisExMVA     = Pris / (1 + Mva% / 100).

CASE icChangedField:
  WHEN "InnkjopsPris" THEN 
    ASSIGN Varekost       = InnkjopsPris - InnkjopsPris * forhRab% / 100
           supVarekost    = InnkjopsPris - InnkjopsPris * supRab% / 100
           KjedeInnkPris  = InnkjopsPris - InnkjopsPris * KjedeRab% / 100
           .
  WHEN "Varekost"         THEN forhRab%         = (InnkjopsPris - Varekost) / InnkjopsPris * 100.
  WHEN "supVarekost"      THEN supRab%          = (InnkjopsPris - supVarekost) / InnkjopsPris * 100.
  WHEN "forhRab%"         THEN Varekost         = InnkjopsPris  - InnkjopsPris * forhRab% / 100.
  WHEN "supRab%"          THEN supVarekost      = InnkjopsPris  - InnkjopsPris * supRab% / 100.           
  WHEN "KjedeRab%"        THEN KjedeInnkPris    = InnkjopsPris  - InnkjopsPris * KjedeRab% / 100.           
  WHEN "KjedeInnkPris"    THEN KjedeRab%        = (InnkjopsPris - KjedeInnkPris) / InnkjopsPris * 100.
  WHEN "KjedeSupRab%"     THEN KjedeSupInnkPris = InnkjopsPris  - InnkjopsPris * KjedeSupRab% / 100.           
  WHEN "KjedeSupInnkPris" THEN KjedeSupRab%     = (InnkjopsPris - KjedeSupInnkPris) / InnkjopsPris * 100.
END CASE.

IF icChangedField NE "LinjeMerknad" THEN
  ASSIGN forhKalkyle    = Pris / VareKost
         supKalkyle     = Pris / SupVareKost
  
         DBkr           = fPrisExMVA - VareKost
         supDBkr        = fPrisExMVA - supVareKost
  
         DB%            = DBkr / fPrisExMVA * 100
         supDB%         = supDBkr / fPrisExMVA * 100
         .

FIND FIRST VarebokHode OF VarebokLinje NO-LOCK.
fMesseNr = VarebokHode.MesseNr.

FOR EACH VarebokHode  
    WHERE VarebokHode.MesseNr   = fMesseNr
      AND VarebokHode.VarebokNr NE VarebokLinje.VarebokNr,
    FIRST bVarebokLinje OF VarebokHode
          WHERE bVarebokLinje.ArtikkelNr = VarebokLinje.ArtikkelNr
          EXCLUSIVE-LOCK:

  BUFFER-COPY VarebokLinje 
         EXCEPT VarebokNr ArtikkelNr /* LinjeMerknad */
         TO bVarebokLinje. 

  IF icChangedField = "LinjeMerknad" THEN
    bVarebokLinje.LinjeMerknad = VarebokLinje.LinjeMerknad.

  IF bUpdateVarebeh THEN DO:
    FIND FIRST VarebehHode 
         WHERE VarebehHode.kilde = VarebokHode.VarebokNr
         NO-LOCK NO-ERROR.
    IF AVAIL VarebehHode THEN DO:
      RUN vareboklinje_kopier_pris_tilvarebeh.p
          (STRING(VarebokHode.VarebokNr) + "; AND Artikkelnr = " + STRING(VarebokLinje.Artikkelnr) + ";" + STRING(VarebehHode.VarebehNr),
           ?,
           "",
           OUTPUT ocReturn,
           OUTPUT obOK).
      bUpdateVarebeh = FALSE.
    END.
  END.
END.

IF bUpdateVarebeh THEN DO:
  FIND VarebokLinje WHERE ROWID(VarebokLinje) = ihBuffVarebokLinje:ROWID NO-LOCK.
  FIND FIRST VarebokHode OF VarebokLinje NO-LOCK.
  FIND FIRST VarebehHode 
       WHERE VarebehHode.kilde = VarebokHode.VarebokNr
       NO-LOCK NO-ERROR.
  IF AVAIL VarebehHode THEN DO:
    RUN vareboklinje_kopier_pris_tilvarebeh.p
        (STRING(VarebokHode.VarebokNr) + "; AND Artikkelnr = " + STRING(VarebokLinje.Artikkelnr) + ";" + STRING(VarebehHode.VarebehNr),
         ?,
         "",
         OUTPUT ocReturn,
         OUTPUT obOK).
    bUpdateVarebeh = FALSE.
  END.
END.

