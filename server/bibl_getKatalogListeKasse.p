/* bibl_getKatalogListeKasse.p */
/* Rutine skaper en liste over alle kataloger som kassen forventer å finne data i. */

  DEF OUTPUT PARAMETER pcKataloger AS CHAR NO-UNDO.

  ASSIGN
    pcKataloger = ""
    .
  FOR EACH Kasse NO-LOCK WHERE
      Kasse.Aktiv = TRUE:
      IF Kasse.ElJournalAktiv AND Kasse.ElJournalKatalog <> "" 
         AND NOT CAN-DO(pcKataloger,Kasse.ElJournalKatalog) THEN
        ASSIGN
          pcKataloger = pcKataloger + 
                        (IF pcKataloger = ""
                           THEN ""
                           ELSE ",") + 
                        Kasse.ElJournalKatalog
                        .
      IF Kasse.KvitteringAktiv AND Kasse.KvitteringKatalog <> "" 
         AND NOT CAN-DO(pcKataloger,Kasse.KvitteringKatalog) THEN
        ASSIGN
          pcKataloger = pcKataloger + 
                        (IF pcKataloger = ""
                           THEN ""
                           ELSE ",") + 
                        Kasse.KvitteringKatalog
                        .
      IF Kasse.UtskriftskopiAktiv AND Kasse.UtskriftskopiKatalog <> "" 
         AND NOT CAN-DO(pcKataloger,Kasse.UtskriftskopiKatalog) THEN
        ASSIGN
          pcKataloger = pcKataloger + 
                        (IF pcKataloger = ""
                           THEN ""
                           ELSE ",") + 
                        Kasse.UtskriftskopiKatalog
                        .
      IF Kasse.KassererOppgjAktiv AND Kasse.KassererOppgjKatalog <> "" 
         AND NOT CAN-DO(pcKataloger,Kasse.KassererOppgjKatalog) THEN
        ASSIGN
          pcKataloger = pcKataloger + 
                        (IF pcKataloger = ""
                           THEN ""
                           ELSE ",") + 
                        Kasse.KassererOppgjKatalog
                        .
      IF Kasse.DagsOppgjAktiv AND Kasse.DagsOppgjKatalog <> "" 
         AND NOT CAN-DO(pcKataloger,Kasse.DagsOppgjKatalog) THEN
        ASSIGN
          pcKataloger = pcKataloger + 
                        (IF pcKataloger = ""
                           THEN ""
                           ELSE ",") + 
                        Kasse.DagsOppgjKatalog
                        .
  END.

