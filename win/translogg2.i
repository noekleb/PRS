/************************************************************
    Program:  translogg.i
    Created:  TN    7 Jan 1999
Description:  Assign av record i TransLogg.
              For kopiering av record.

Last change:  TN    2 Dec 99    7:47 pm
************************************************************/

DO:
  ASSIGN
    {1}.Butik          = {2}.Butik
    {1}.TransNr        = {2}.TransNr
    {1}.SeqNr          = {2}.SeqNr{3}.

  ASSIGN
    {1}.BatchNr        = {2}.BatchNr
    {1}.ForsNr         = {2}.ForsNr
    {1}.KundNr         = {2}.KundNr
    {1}.TTId           = {2}.TTId
    {1}.TBId           = {2}.TBId
    {1}.ArtikkelNr     = {2}.ArtikkelNr
    {1}.LevNr          = {2}.LevNr
    /*
    {1}.RegistrertDato = {2}.RegistrertDato
    {1}.RegistrertTid  = {2}.RegistrertTid
    {1}.RegistrertAv   = {2}.RegistrertAv
    */
    {1}.BongId         = {2}.BongId
    {1}.BongLinjeNr    = {2}.BongLinjeNr
    {1}.KassaNr        = {2}.KassaNr
    {1}.Vg             = {2}.Vg
    {1}.LopNr          = {2}.LopNr
    {1}.Storl          = {2}.Storl
    {1}.TilStorl       = {2}.TilStorl
    {1}.Antall         = {2}.Antall
    {1}.Pris           = {2}.Pris
    {1}.VVArekost      = {2}.VVarekost
    {1}.SattVVareKost  = TRUE
    .

  ASSIGN
    {1}.RabKr          = {2}.RabKr
    {1}.Mva            = {2}.Mva
    {1}.Plukket        = {4}   /* {2}.Plukket     */
    {1}.Dato           = {2}.Dato        
    {1}.Tid            = {2}.Tid         
    {1}.Postert        = FALSE /* {2}.Postert */    
    {1}.PostertDato    = ?     /* {2}.PostertDato */
    {1}.PostertTid     = 0     /* {2}.PostertTid  */
    {1}.BestNr         = {2}.BestNr
    {1}.OvButik        = {2}.OvButik
    {1}.OvTransNr      = {2}.OvTransNr.
END.
