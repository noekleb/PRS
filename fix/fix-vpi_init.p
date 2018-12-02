DISPLAY 'Sletter artikkel 0'.
PAUSE 0.
FOR EACH VPIArtBas WHERE VPIArtBas.ArtikkelNr = 0:
    DELETE VPIArtBAs.
END.
FOR EACH Strekkode WHERE
    Strekkode.ArtikkelNR = 0:
    DELETE strekkode.
END.
FOR EACH ArtPris WHERE 
    ArtPris.ArtikkelNR = 0:
    DELETE ArtPris.
END.
FOR EACH ArtBas WHERE ArtikkelNR = 0:
    DELETE ArtBas.
END.

DISPLAY 'Init ArtBas.Utvidetsok'.
PAUSE 0.
FOR EACH ArtBAs EXCLUSIVE-LOCK WHERE
    ArtBas.UtvidetSok = '':
    ArtBas.ETid = TIME.
END.

DISPLAY 'Renser VPI filhode'.
PAUSE 0.
FOR EACH VPIFilHOde WHERE Dato >= 06/19/2012:
  FOR EACH VPIFillinje OF VPIFilHode:
    DELETE VPIFillinje.
  END.
  DELETE VPIFilHode.
END. 
  
DISPLAY 'Størrelsestype i ArtBAs'.
PAUSE 0.
FOR EACH ArtBas WHERE NOT CAN-FIND(StrType OF ArtBAs):
     RUN bibl_opprettStrtypeForModell.p (ArtBas.ArtikkelNr, OUTPUT ArtBas.StrTypeID).
END.

DISPLAY 'Størrelsestype i VPIArtBAs'.
PAUSE 0.
FOR EACH VPIArtBas WHERE NOT CAN-FIND(StrType OF VPIArtBAs):
     RUN bibl_opprettStrtypeForModell.p (VPIArtBas.ArtikkelNr, OUTPUT VPIArtBas.StrTypeID).
END.

DEFINE VARIABLE cFilnamn AS CHARACTER   NO-UNDO.
INPUT FROM OS-DIR("c:\home\lindbak\ankommet\bku\") NO-ATTR-LIST.
REPEAT:
    IMPORT cFilnamn.
    IF cFilnamn BEGINS "HKVPI" THEN DO:
        FILE-INFO:FILE-NAME = "c:\home\lindbak\ankommet\bku\" + cFilnamn.
        IF FILE-INFO:FILE-CREATE-DATE >= DATE(6,19,2012) THEN
            OS-RENAME VALUE(FILE-INFO:FILE-NAME)
                      VALUE("c:\home\lindbak\ankommet\" + ENTRY(1,cFilnamn,".") + "." + ENTRY(2,cFilnamn,".")).
    END.
END.

