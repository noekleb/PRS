FIND KordrEHode WHERE KOrdrEHode.KORdre_Id = 1190000016.

DISPLAY
  KORdreHode.KORdre_Id
  KOrdreHode.RefKOrdre_Id
  KOrdreHode.LevStatus   
  KOrdreHode.VerkstedMerknad FORMAT "x(20)"
  KOrdreHode.SendingsNr 
  KOrdreHode.EkstOrdreNr
  KOrdreHode.DatoTidOpprettet
WITH SIDE-LABELS 2 COLUMNS.
