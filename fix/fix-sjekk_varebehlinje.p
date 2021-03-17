
DEF VAR X AS dec NO-UNDO.

/*                                     
  CASE ENTRY(4,icParam):
    WHEN "1" THEN cKjedeLevGjFakt = " AND VareBehLinje.KjedeVare".
    WHEN "2" THEN cKjedeLevGjFakt = " AND VareBehLinje.Gjennomfaktureres".
    WHEN "3" THEN cKjedeLevGjFakt = " AND (VareBehLinje.Gjennomfaktureres OR VareBehLinje.KjedeVare)".
  END CASE.
*/

FOR EACH VareBehLinje NO-LOCK 
         WHERE VareBehLinje.VarebehNr = 9000006
         AND VareBehLinje.levnr = 10
         AND (VareBehLinje.Gjennomfaktureres OR VareBehLinje.KjedeVare)
         ,EACH VarebehlinjeTrans OF VarebehLinje NO-LOCK
          WHERE VarebehLinjeTrans.ArtikkelNr > 0
             AND VareBehLinjeTrans.ButikkNr = 81              
             AND RegistrertBestilling
             AND GodkjentBestilling
             
         ,FIRST StrKonv OF VareBehLinjeTrans NO-LOCK
         /*
         ,FIRST ArtBas OF VarebehLinje NO-LOCK
         ,FIRST Butiker WHERE Butiker.Butik = VareBehLinjeTrans.ButikkNr NO-LOCK
         */
         BY VarebehLinjeTrans.ButikkNr BY VarebehLinje.LevKod:
         
    X = X + VarebehLinjeTrans.Bestilt1.

    IF NOT CAN-FIND(FIRST ArtBas OF VarebehLinje) THEN
        MESSAGE VareBehLinjeTrans.ArtikkelNr
           VarebehLinje.ArtikkelNr
           VareBehLinje.LevKod
           VareBehLinje.Beskr
           VareBehLinje.LEvFargKod
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.

MESSAGE X
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
