/* Varebeh_kalkuler.p: Kalkuler en Varebehlinje
   Kalles fra: - newVarebehlinje.p  
               - update_Varebehlinje.p
               - Varebehlinje_justermange.p
                
   Opprettet 02.09.04 av BHa     
-----------------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBuffVarebehLinje AS HANDLE NO-UNDO.
DEF INPUT PARAM icChangedField    AS CHAR NO-UNDO.

DEF VAR fPrisExMVA     AS DEC NO-UNDO.
DEF VAR fMesseNr       AS DEC NO-UNDO.

DEF BUFFER bVarebehLinje FOR VarebehLinje.

FIND VarebehLinje WHERE ROWID(VarebehLinje) = ihBuffVarebehLinje:ROWID EXCLUSIVE-LOCK.

fPrisExMVA     = Pris / (1 + Mva% / 100).

CASE icChangedField:
  WHEN "Varekost"    THEN forhRab%    = (InnkjopsPris - Varekost) / InnkjopsPris * 100.
  WHEN "supVarekost" THEN supRab%     = (InnkjopsPris - supVarekost) / InnkjopsPris * 100.
  WHEN "forhRab%"    THEN Varekost    = InnkjopsPris - InnkjopsPris * forhRab% / 100.
  WHEN "supRab%"     THEN supVarekost = InnkjopsPris - InnkjopsPris * supRab% / 100.           
END CASE.

IF icChangedField NE "LinjeMerknad" THEN
  ASSIGN forhKalkyle    = Pris / VareKost
         supKalkyle     = Pris / SupVareKost
  
         DBkr           = fPrisExMVA - VareKost
         supDBkr        = fPrisExMVA - supVareKost
  
         DB%            = DBkr / fPrisExMVA * 100
         supDB%         = supDBkr / fPrisExMVA * 100
         .

FIND FIRST VarebehHode OF VarebehLinje NO-LOCK.
fMesseNr = VarebehHode.MesseNr.

FOR EACH VarebehHode  
    WHERE VarebehHode.MesseNr   = fMesseNr
      AND VarebehHode.VarebehNr NE VarebehLinje.VarebehNr,
    FIRST bVarebehLinje OF VarebehHode
          WHERE bVarebehLinje.ArtikkelNr = VarebehLinje.ArtikkelNr
          EXCLUSIVE-LOCK:
  ASSIGN bVarebehLinje.Varekost     = VarebehLinje.Varekost
         bVarebehLinje.supVarekost  = VarebehLinje.supVarekost
         bVarebehLinje.forhRab%     = VarebehLinje.forhRab%
         bVarebehLinje.supRab%      = VarebehLinje.supRab%
         bVarebehLinje.forhKalkyle  = VarebehLinje.forhKalkyle
         bVarebehLinje.supKalkyle   = VarebehLinje.supKalkyle
         bVarebehLinje.DBkr         = VarebehLinje.DBkr
         bVarebehLinje.DB%          = VarebehLinje.DB%
         bVarebehLinje.supDBkr      = VarebehLinje.supDBkr
         bVarebehLinje.supDB%       = VarebehLinje.supDB%
         .
  IF icChangedField = "LinjeMerknad" THEN
    bVarebehLinje.LinjeMerknad = VarebehLinje.LinjeMerknad.
END.

