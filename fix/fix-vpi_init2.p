current-window:width = 300.

def var iAnt as int no-undo.
def var iAntStr as int no-undo.
def var cStrLst as char no-undo.
def var iStrTypeId as int no-undo.

def buffer bufVPIArtBas for VPIArtBAs.

DISPLAY 'Størrelsestype i ArtBAs'.
PAUSE 0.

FOR EACH ArtBas WHERE Artbas.strtypeid = 0 EXCLUSIVE-LOCK:
     RUN bibl_opprettStrtypeForModell.p (ArtBas.ArtikkelNr, OUTPUT ArtBas.StrTypeID).
END.
FOR EACH ArtBas WHERE NOT CAN-FIND(StrType OF ArtBas) EXCLUSIVE-LOCK:
     RUN bibl_opprettStrtypeForModell.p (ArtBas.ArtikkelNr, OUTPUT ArtBas.StrTypeID).
END.

DISPLAY 'Størrelsestype i VPIArtBAs'.
PAUSE 0.

BURRE:
FOR EACH VPIArtBas WHERE 
  VPIArtBas.StrTypeId = 0 EXCLUSIVE-LOCK:
  RUN bibl_opprettVPIStrtypeForModell.p (VPIArtBas.ArtikkelNr, VPIArtBas.EkstVPILEvNr , OUTPUT VPIArtBas.StrTypeID).
END.
FOR EACH VPIArtBas WHERE NOT can-find(StrType OF VPIArtBAs) EXCLUSIVE-LOCK:
  RUN bibl_opprettVPIStrtypeForModell.p (VPIArtBas.ArtikkelNr, VPIArtBas.EkstVPILEvNr , OUTPUT VPIArtBas.StrTypeID).
END.
