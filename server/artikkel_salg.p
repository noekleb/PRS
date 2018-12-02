/* Finn salgstall for artikkel 
   Parametere: Artikkelnr;komma-sep.butikkliste
   
               Returnerer streng beregnet på presentasjon i grid:
              =Butikk|Mnd|Mnd|..
              ¤Butnr |Ant|Ant|..
               
              Legg merke til skilletegn mellom label og verdier samt = foran første label
               
   Opprettet:  22.12.05 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cArtikkelNr   AS CHAR NO-UNDO.
DEF VAR cButikkListe  AS CHAR NO-UNDO.
DEF VAR ix            AS INT  NO-UNDO.
DEF VAR iy            AS INT  NO-UNDO.
DEF VAR iTotButLev    AS INT  NO-UNDO.
DEF VAR cPerLabel     AS CHAR NO-UNDO.
DEF VAR cButPer       AS CHAR NO-UNDO.
DEF VAR iButBest      AS INT  NO-UNDO.
DEF VAR cButSalg      AS CHAR NO-UNDO.
DEF VAR iTotButSalg   AS INT  NO-UNDO.

ASSIGN cArtikkelNr  = FILL("0",13 - LENGTH(ENTRY(1,icParam,";"))) + ENTRY(1,icParam,";")
       cButikkListe = ENTRY(2,icParam,";")
       .

DO ix = 1 TO NUM-ENTRIES(cButikkListe):
  ASSIGN iTotButLev  = 0
         cButPer     = ""
         iTotButSalg = 0.

  DO iy = MONTH(TODAY) TO 1 BY -1:
    IF ix = 1 THEN
      cPerLabel = cPerLabel + SUBSTR(STRING(YEAR(TODAY)),3) + STRING(iy,"99") + "|".

    FIND FIRST StLinje NO-LOCK
         WHERE StLinje.StTypeId    = "artikkel"
           AND StLinje.DataObjekt  = cArtikkelNr
           AND StLinje.PerId       = "maned"
           AND StLinje.Aar         = YEAR(TODAY) 
           AND Stlinje.butik       = INT(ENTRY(ix,cButikkListe))
           AND StLinje.PerLinNr    = iy
         NO-ERROR.
    IF AVAIL StLinje THEN
      ASSIGN cButPer     = cButPer + STRING(StLinje.AntSolgt)
             iTotButSalg = iTotButSalg + StLinje.AntSolgt. 
    ELSE 
      cButPer = cButPer + "0".

    cButPer = cButPer + "|".
  END.

  IF MONTH(TODAY) < 12 THEN
    DO iy = 12 TO MONTH(TODAY) + 1 BY -1:
      IF ix = 1 THEN
        cPerLabel = cPerLabel + SUBSTR(STRING(YEAR(TODAY) - 1),3) + STRING(iy,"99") + "|".
  
      FIND FIRST StLinje NO-LOCK
           WHERE StLinje.StTypeId    = "artikkel"
             AND StLinje.DataObjekt  = cArtikkelNr
             AND StLinje.PerId       = "maned"
             AND StLinje.Aar         = YEAR(TODAY) - 1
             AND Stlinje.butik       = INT(ENTRY(ix,cButikkListe))
             AND StLinje.PerLinNr    = iy
           NO-ERROR.
      IF AVAIL StLinje THEN
        ASSIGN cButPer     = cButPer + STRING(StLinje.AntSolgt)
               iTotButSalg = iTotButSalg + StLinje.AntSolgt. 
      ELSE 
        cButPer = cButPer + "0".
  
      cButPer = cButPer + "|".
    END.

  ASSIGN cButPer   = ENTRY(ix,cButikkListe) + "|" + 
                     TRIM(cButPer,"|")
         cButSalg = cButSalg + cButPer + ";".
END.

ocReturn = "=Butikk|" + TRIM(cPerLabel,"|") + "¤" 
         + TRIM(cButSalg,";").

obOk = TRUE.

