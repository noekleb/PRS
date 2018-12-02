/* Finn lagerstatus for artikkel 
   Parametere: Artikkelnr;komma-sep.butikkliste
   
               Returnerer streng beregnet på presentasjon i grid:
              =Butikk|Totalt|Str1|Str2|..
              ¤Butnr |Tot.ant|Ant.str1|Ant.str2|..
               
              Legg merke til skilletegn mellom label og verdier samt = foran første label
               
   Opprettet:  13.12.05 av BHa              
   Endret:     20.12.05 av BHa
               - Sum bestillinger pr str vises sammen med lager
               19.03.07 av BHa
               - Fjernet test på at neste ankomstdato er større enn dagens dato
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fArtikkelNr   AS DEC  NO-UNDO.
DEF VAR cButikkListe  AS CHAR NO-UNDO.
DEF VAR ix            AS INT  NO-UNDO.
DEF VAR iy            AS INT  NO-UNDO.
DEF VAR iTotButLev    AS INT  NO-UNDO.
DEF VAR cButStr       AS CHAR NO-UNDO.
DEF VAR cButLager     AS CHAR NO-UNDO.
DEF VAR cLevLager     AS CHAR NO-UNDO INIT "|".
DEF VAR cLevBest      AS CHAR NO-UNDO.
DEF VAR cLevNavn      AS CHAR NO-UNDO.
DEF VAR dFirstLev     AS DATE NO-UNDO INIT 01/01/2099.
DEF VAR iButBest      AS INT  NO-UNDO.
DEF VAR iTotButBest   AS INT  NO-UNDO.

ASSIGN fArtikkelNr  = DEC(ENTRY(1,icParam,";"))
       cButikkListe = ENTRY(2,icParam,";")
       NO-ERROR.
/* Hvis ikke artikkelnr kan omformateres til decimal, skal det bare returneres. */
IF ERROR-STATUS:ERROR THEN DO:
  obOK = TRUE.
  RETURN.
END.

FOR FIRST ArtBas NO-LOCK
    WHERE ArtBas.ArtikkelNr = fArtikkelNr
   ,FIRST StrType OF ArtBas NO-LOCK:

  DO ix = 1 TO NUM-ENTRIES(cButikkListe):
    ASSIGN iTotButLev  = 0
           cButStr     = ""
           iTotButBest = 0.
    DO iy = 1 TO NUM-ENTRIES(StrType.AlfaFordeling):
      FIND FIRST ArtLag NO-LOCK
           WHERE ArtLag.ArtikkelNr  = fArtikkelNr
             AND ArtLag.butik       = INT(ENTRY(ix,cButikkListe))
             AND TRIM(ArtLag.storl) = TRIM(ENTRY(iy,StrType.AlfaFordeling))
           NO-ERROR.
      IF AVAIL ArtLag THEN
        ASSIGN cButStr    = cButStr + STRING(ArtLag.lagant) 
               iTotButLev = iTotButLev + ArtLag.lagant.    
      ELSE 
        cButStr = cButStr + "0".

      iButBest = 0.
      FOR EACH BestHode FIELDS(BestNr) NO-LOCK
          WHERE BestHode.ArtikkelNr =  fArtikkelNr
            AND BestHode.BestStat   LE 3
          ,EACH BestStr NO-LOCK OF BestHode
                WHERE TRIM(BestStr.Storl) = TRIM(ENTRY(iy,StrType.AlfaFordeling))
                  AND BestStr.Butik       = INT(ENTRY(ix,cButikkListe)):
        iButBest = iButBest + BestStr.Bestilt.
      END.
      IF iButBest > 0 THEN
        ASSIGN cButStr = cButStr + "/" + STRING(iButBest)
               iTotButBest = iTotButBest + iButBest.

      cButStr = cButStr + "|".
    END.
    ASSIGN cButStr   = ENTRY(ix,cButikkListe) + "|" + 
                       STRING(iTotButLev) +  
                       (IF iTotButBest NE 0 THEN "/" + STRING(iTotButBest) ELSE "") + "|" + 
                       TRIM(cButStr,"|")
           cButLager = cButLager + cButStr + ";".
  END.

  DO ix = 1 TO NUM-ENTRIES(StrType.Fordeling):
    FIND FIRST LevLager NO-LOCK
         WHERE LevLager.ArtikkelNr   = fArtikkelNr
           AND LevLager.LevNr        = ArtBas.LevNr
           AND LevLager.StrKode      = INT(ENTRY(ix,StrType.Fordeling))
/*            AND LevLager.NesteAnkDato GT TODAY  */
         NO-ERROR.
    IF AVAIL LevLager THEN DO:
      ASSIGN cLevLager  = cLevLager + LevLager.cLager + "|"
             cLevBest   = cLevBest  + (IF LevLager.iBestilling NE ? THEN 
                                         STRING(LevLager.iBestilling)
                                       ELSE "") + "|".
      IF cLevNavn = "" THEN DO:
        FIND FIRST LevBas OF LevLager NO-LOCK NO-ERROR.
        IF AVAIL LevBas THEN
          cLevNavn = SUBSTR(LevBas.LevNamn,1,10).
      END.
      IF LevLager.NesteAnkDato NE ? AND LevLager.NesteAnkDato < dFirstLev THEN
        dFirstLev = LevLager.NesteAnkDato.
    END.
    ELSE DO:
      ASSIGN cLevLager  = cLevLager + "0|"
             cLevBest   = cLevBest   + "|".
    END.
  END.

  ocReturn = "=Lev/But|Total|" + REPLACE(TRIM(StrType.AlfaFordeling,","),",","|") + "¤" 
           + (IF cLevNavn NE "" THEN
                cLevNavn + "|Lager:|" + TRIM(cLevLager,"|") + ";"
              + "Ank " 
                + (IF dFirstLev NE 01/01/2099 THEN
                     STRING(DAY(dFirstLev)) + "/" + STRING(MONTH(dFirstLev))
                   ELSE "?") + "|Bestilt:|"
              + TRIM(cLevBest,"|") + "¤"
             ELSE "")
           + TRIM(cButLager,";").
END.

obOk = TRUE.

/*                                                                   */
/* FOR EACH BestHode                                                 */
/* END.                                                              */
/*                                                                   */
/* DO ix = 1 TO NUM-ENTRIES(cOrdreNrList) TRANSACTION:               */
/*   FIND Ordre EXCLUSIVE-LOCK                                       */
/*        WHERE Ordre.OrdreNr = INT(ENTRY(ix,cOrdreNrList))          */
/*        NO-ERROR.                                                  */
/*   IF AVAIL Ordre THEN DO:                                         */
/*     IF cStatus = "sendt" AND Ordre.OrdreStatus = 1 THEN DO:       */
/*       for each BestHode no-lock where                             */
/*         BestHode.OrdreNr  = Ordre.OrdreNr and                     */
/*         BestHode.BestStat = 3: /* På odre --> Ordre sendt */      */
/*                                                                   */
/*         run bytbeststatus.p (recid(BestHode),"+",Ordre.OrdreNr).  */
/*       end. /* SET_RADER_SOM_SENDT */                              */
/*       ASSIGN Ordre.OrdreStatus = 2                                */
/*              Ordre.SendtDato   = TODAY.                           */
/*     END.                                                          */
/*   END.                                                            */
/*   ELSE DO:                                                        */
/*     ocReturn = "Ordre ikke tilgjengelig for oppdatering".         */
/*     UNDO, LEAVE.                                                  */
/*   END.                                                            */
/* END.                                                              */
/*                                                                   */
/* IF ocReturn = "" THEN                                             */
/*   obOk = TRUE.                                                    */

