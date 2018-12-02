TRIGGER PROCEDURE FOR DELETE OF SkoTex.ArtBas.

DEFINE BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
DEFINE BUFFER trgStrekkode FOR Strekkode.
DEFINE BUFFER trgArtBas FOR ArtBas.
DEFINE BUFFER trgELogg FOR ELogg.

ASSIGN
  SkoTex.ArtBas.EDato      = TODAY
  SkoTex.ArtBas.ETid       = TIME
  SkoTex.ArtBas.BrukerId   = USERID("skotex").
/* Vi gør ingenting hær nær vi tar bort en artbas    */
/* sletting tas om hand av deletetrigger i strekkode */
/* och deletetrigger i pakkelinje                    */

/* Förutom */
FOR EACH KonvReg WHERE KonvReg.EDB-System = "RESTPAR" AND
                       KonvReg.Tabell     = ""        AND
                       KonvReg.EkstId     = STRING(ArtBas.Artikkelnr):
    DELETE KonvReg.
END.
FOR EACH KonvReg WHERE KonvReg.EDB-System = "RESTPAR" AND
                       KonvReg.Tabell     = ""        AND
                       KonvReg.InterntId  = STRING(ArtBas.Artikkelnr):
    DELETE KonvReg.
END.

/*
ERP-LOGG:
DO:
    FIND trgELogg WHERE 
         trgELogg.TabellNavn     = "ArtBas" AND
         trgELogg.EksterntSystem = "ERP"    AND
         trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR.
    IF NOT AVAIL trgELogg THEN DO:
        CREATE trgELogg.
        ASSIGN trgELogg.TabellNavn     = "ArtBas"
               trgELogg.EksterntSystem = "ERP"   
               trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
    END.
    ASSIGN trgELogg.EndringsType = 3 
           trgELogg.Behandlet    = FALSE.
    RELEASE trgELogg.
END. /* ERP-LOGG */
*/

SLETT-LOGG:
DO FOR trgELogg:
    FIND trgELogg WHERE 
         trgELogg.TabellNavn     = "ArtBas" AND
         trgELogg.EksterntSystem = "POS"    AND
         trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR NO-WAIT.
    IF LOCKED trgELogg THEN LEAVE SLETT-LOGG. 
    IF NOT AVAIL trgELogg THEN DO:
        CREATE trgELogg.
        ASSIGN trgELogg.TabellNavn     = "ArtBas"
               trgELogg.EksterntSystem = "POS"   
               trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
    END.
    ASSIGN trgELogg.EndringsType = 3 
           trgELogg.Behandlet    = FALSE.
    IF AVAILABLE trgELogg THEN RELEASE trgELogg.
END. /* SLETT-LOGG */

FIND FIRST trgEkstEDBSystem WHERE 
    trgEkstEDBSystem.DataType = "WEBBUT" AND 
    trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem THEN
WEBBUTIKK:
DO FOR trgELogg:
    /* Artikkel skal slettes fra */
    IF ArtBas.WebButikkArtikkel   = TRUE THEN DO:
        FIND trgELogg WHERE 
             trgELogg.TabellNavn     = "ArtBas" AND
             trgELogg.EksterntSystem = "WEBBUT"    AND
             trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR NO-WAIT.
        IF LOCKED trgELogg THEN LEAVE WEBBUTIKK.
        IF NOT AVAIL trgELogg THEN DO:
            CREATE trgELogg.
            ASSIGN trgELogg.TabellNavn     = "ArtBas"
                   trgELogg.EksterntSystem = "WEBBUT"   
                   trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
        END.
        ASSIGN trgELogg.EndringsType = 3 
               trgELogg.Behandlet    = FALSE.
        RELEASE trgELogg.
    END.
    IF AVAILABLE trgELogg THEN RELEASE trgELogg.
END. /* WEBBUTIKK */

FOR EACH Strekkode OF ArtBas EXCLUSIVE-LOCK:
  DELETE Strekkode.
END.
FOR EACH ArtPris OF ArtBas EXCLUSIVE-LOCK:
  DELETE ArtPris.
END.
FOR EACH PrisKo OF ArtBAs EXCLUSIVE-LOCK:
    DELETE Prisko.
END.
FOR EACH HPrisko OF ArtBAs EXCLUSIVE-LOCK:
    DELETE HPrisko.
END.
FOR EACH ArtBasKarakteristikk OF ArtBas:
    DELETE ArtBasKarakteristikk.
END.

/* Utlegg til ferskvarevekt */
IF CAN-FIND (FIRST SysPara WHERE SysPara.SysHId = 23 AND SysPara.SysGr = 1 AND SysPara.Parameter1 > '0') AND 
  ArtBas.ArtSlag = 1 THEN 
BLOKK2:
DO FOR trgStrekkode:
  FOR EACH trgStrekkode OF ArtBas NO-LOCK WHERE 
    LENGTH(trgStrekkode.Kode) = 13:
    FIND trgELogg WHERE 
         trgELogg.TabellNavn     = "ArtBas" AND
         trgELogg.EksterntSystem = "FVEKT"    AND
         trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr) + CHR(1) + STRING(trgStrekkode.Kode) NO-ERROR NO-WAIT.
    IF LOCKED trgELogg THEN LEAVE BLOKK2.
    IF NOT AVAIL trgELogg THEN DO:
        CREATE trgELogg.
        ASSIGN trgELogg.TabellNavn     = "ArtBas"
               trgELogg.EksterntSystem = "FVEKT"   
               trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr) + CHR(1) + STRING(trgStrekkode.Kode).
    END.
    ASSIGN trgELogg.EndringsType = 3 
           trgELogg.Behandlet    = FALSE.
    RELEASE trgELogg.
  END.
  IF AVAILABLE trgStrekkode THEN RELEASE trgStrekkode.
END. /* BLOKK2 */

