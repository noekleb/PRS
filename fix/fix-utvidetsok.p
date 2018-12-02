/* fix-vareslag.p */

PUBLISH 'infoDisp' ("Initierer utvidetsøk...").

RUN initUtvidetsok.  /* ArtBas.    */ 
RUN initUtvidetSok2. /* VPIArtBas. */ 

PUBLISH 'infoDisp' ("..").

PROCEDURE initUtvidetsok:
/*IF CAN-FIND(FIRST ArtBas WHERE
            ArtBas.Utvidetsok = "") THEN*/
  DO:
    FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
        ArtBas.UtvidetSok = ".":
        {w_artbas.i}
    END.
  END.

  /*IF CAN-FIND(FIRST VareBokLinje WHERE
            VareBokLinje.Utvidetsok = "") THEN */
  DO:
    FOR EACH VareBokLinje EXCLUSIVE-LOCK:
        VarebokLinje.UtvidetSok = ".".
    END.
    FOR EACH VareBehLinje EXCLUSIVE-LOCK:
        VareBehLinje.UtvidetSok = ".".
    END.
  END.
END PROCEDURE.

PROCEDURE initUtvidetsok2:
  /*IF CAN-FIND(FIRST VPIArtBas WHERE
            VPIArtBas.EkstVPILevNr > 0 AND
            VPIArtBas.Utvidetsok = "") THEN */
  DO:
    FOR EACH vpiArtBas EXCLUSIVE-LOCK WHERE
        vpiArtBas.EkstVPILevNr > 0 AND
        vpiArtBas.UtvidetSok = ".":
        {w_artbas.i &vpi = "vpi"}
    END.
  END.
END PROCEDURE.


