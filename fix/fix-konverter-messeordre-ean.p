/* Fiks/konvertering av messeordre - EANn utgår. */
FOR EACH VareBehLinjeTrans
   ,FIRST strekkode NO-LOCK WHERE
    Strekkode.Kode = ENTRY(1,VareBehLinjeTrans.Kode,";"):
  ASSIGN VareBehLinjeTrans.StrKode = strekkode.StrKode.
         VareBehLinjeTrans.Kode    = "000000" +  
  STRING(VareBehLinjeTrans.ArtikkelNr) + STRING(StrekKode.StrKode,"999"). 
END.
