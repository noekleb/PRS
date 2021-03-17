/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER piRecType    AS INT NO-UNDO.
DEF INPUT  PARAMETER piAntRecord  AS INT NO-UNDO.
DEF OUTPUT PARAMETER piAntLinjer  AS INT NO-UNDO.

DEF VAR piLoop AS INT NO-UNDO.

{tmptt_VPIArtBas.i &SHARED=SHARED}

LOOPEN:
DO piLoop = 1 TO piAntRecord:
  CREATE tmpTT_VPIArtBas.
  ASSIGN
    piAntLinjer = piAntLinjer + 1
    tmpTT_VPIArtBas.RecType = piRecType
    .
  IMPORT STREAM InnFil
    tmpTT_VPIArtBas.EkstVPILevNr      
    tmpTT_VPIArtBas.VareNr            
    tmpTT_VPIArtBas.Hg                
    tmpTT_VPIArtBas.Vg                
    tmpTT_VPIArtBas.LopNr             
    tmpTT_VPIArtBas.SaSong            
    tmpTT_VPIArtBas.Farg              
    tmpTT_VPIArtBas.Klack             
    tmpTT_VPIArtBas.MatKod            
    tmpTT_VPIArtBas.BildNr            
    tmpTT_VPIArtBas.Beskr             
    tmpTT_VPIArtBas.LevNr             
    tmpTT_VPIArtBas.LevKod            
    tmpTT_VPIArtBas.Tilv-Land         
    tmpTT_VPIArtBas.Kommentar         
    tmpTT_VPIArtBas.Ov-Id             
    tmpTT_VPIArtBas.Last-Id           
    tmpTT_VPIArtBas.Foder-Id          
    tmpTT_VPIArtBas.Inner-Id          
    tmpTT_VPIArtBas.Slit-Id           
    tmpTT_VPIArtBas.Anv-Id            
    tmpTT_VPIArtBas.RabKod            
    tmpTT_VPIArtBas.ProvKod           
    tmpTT_VPIArtBas.ValKod            
    tmpTT_VPIArtBas.Lager             
    tmpTT_VPIArtBas.VMId              
    tmpTT_VPIArtBas.LevFargKod        
    tmpTT_VPIArtBas.Notat             
    tmpTT_VPIArtBas.BongTekst         
    tmpTT_VPIArtBas.AnonseArtikkel    
    tmpTT_VPIArtBas.VgKat             
    tmpTT_VPIArtBas.StrTypeID         
    tmpTT_VPIArtBas.ProdNr            
    tmpTT_VPIArtBas.EDato             
    tmpTT_VPIArtBas.ETid              
    tmpTT_VPIArtBas.BrukerID          
    tmpTT_VPIArtBas.RegistrertDato    
    tmpTT_VPIArtBas.RegistrertTid     
    tmpTT_VPIArtBas.RegistrertAv      
    tmpTT_VPIArtBas.ArtikkelNr        
    tmpTT_VPIArtBas.Storrelser        
    tmpTT_VPIArtBas.LevDato1          
    tmpTT_VPIArtBas.LevDato2          
    tmpTT_VPIArtBas.DivInfo[ 1]       
    tmpTT_VPIArtBas.DivInfo[ 2]       
    tmpTT_VPIArtBas.DivInfo[ 3]       
    tmpTT_VPIArtBas.DivInfo[ 4]       
    tmpTT_VPIArtBas.DivInfo[ 5]       
    tmpTT_VPIArtBas.DivInfo[ 6]       
    tmpTT_VPIArtBas.DivInfo[ 7]       
    tmpTT_VPIArtBas.DivInfo[ 8]       
    tmpTT_VPIArtBas.DivInfo[ 9]       
    tmpTT_VPIArtBas.DivInfo[10]       
    tmpTT_VPIArtBas.DivInfo[11]       
    tmpTT_VPIArtBas.DivInfo[12]       
    tmpTT_VPIArtBas.DivInfo[13]       
    tmpTT_VPIArtBas.DivInfo[14]       
    tmpTT_VPIArtBas.DivInfo[15]       
    tmpTT_VPIArtBas.DivInfo[16]       
    tmpTT_VPIArtBas.DivInfo[17]       
    tmpTT_VPIArtBas.DivInfo[18]       
    tmpTT_VPIArtBas.DivInfo[19]       
    tmpTT_VPIArtBas.DivInfo[20]       
    tmpTT_VPIArtBas.VisDivInfo[ 1]    
    tmpTT_VPIArtBas.VisDivInfo[ 2]    
    tmpTT_VPIArtBas.VisDivInfo[ 3]    
    tmpTT_VPIArtBas.VisDivInfo[ 4]    
    tmpTT_VPIArtBas.VisDivInfo[ 5]    
    tmpTT_VPIArtBas.VisDivInfo[ 6]    
    tmpTT_VPIArtBas.VisDivInfo[ 7]    
    tmpTT_VPIArtBas.VisDivInfo[ 8]    
    tmpTT_VPIArtBas.VisDivInfo[ 9]    
    tmpTT_VPIArtBas.VisDivInfo[10]    
    tmpTT_VPIArtBas.VisDivInfo[11]    
    tmpTT_VPIArtBas.VisDivInfo[12]    
    tmpTT_VPIArtBas.VisDivInfo[13]    
    tmpTT_VPIArtBas.VisDivInfo[14]    
    tmpTT_VPIArtBas.VisDivInfo[15]    
    tmpTT_VPIArtBas.VisDivInfo[16]    
    tmpTT_VPIArtBas.VisDivInfo[17]    
    tmpTT_VPIArtBas.VisDivInfo[18]    
    tmpTT_VPIArtBas.VisDivInfo[19]    
    tmpTT_VPIArtBas.VisDivInfo[20]    
    tmpTT_VPIArtBas.SattPaKampanje    
    tmpTT_VPIArtBas.OPris             
    tmpTT_VPIArtBas.OLLager           
    tmpTT_VPIArtBas.BildeIKasse       
    tmpTT_VPIArtBas.Pakke             
    tmpTT_VPIArtBas.Alder             
    tmpTT_VPIArtBas.HkStyrt           
    tmpTT_VPIArtBas.LokPris           
    tmpTT_VPIArtBas.IKasse            
    tmpTT_VPIArtBas.HKVareId          
    tmpTT_VPIArtBas.KjentPaHK         
    tmpTT_VPIArtBas.BehKode           
    tmpTT_VPIArtBas.Pakkenr           
    tmpTT_VPIArtBas.HandKode          
    tmpTT_VPIArtBas.AnbefaltPris      
    tmpTT_VPIArtBas.KundeRabatt       
    tmpTT_VPIArtBas.Etikett           
    tmpTT_VPIArtBas.SalgsEnhet        
    tmpTT_VPIArtBas.Oppdatert         
    tmpTT_VPIArtBas.LokArtikkelNr     
    tmpTT_VPIArtBas.ModellFarge       
    tmpTT_VPIArtBas.SentralBestilling 
    tmpTT_VPIArtBas.PrisGrpNr         
    tmpTT_VPIArtBas.HovedModellFarge  
    tmpTT_VPIArtBas.StrKode1  
    tmpTT_VPIArtBas.StrKode2  
    tmpTT_VPIArtBas.LevVareTekst  
    tmpTT_VPIArtBas.Gjennomfaktureres
    tmpTT_VPIArtBas.KjedeVare
    tmpTT_VPIArtBas.KorrArtikkelNr
    tmpTT_VPIArtBas.UtvidetSok
    tmpTT_VPIArtBas.Etikettekst1
    tmpTT_VPIArtBas.Etikettekst2
    /* Nye felt 17/4-08 */
    tmpTT_VPIArtBas.forhRab%
    tmpTT_VPIArtBas.suppRab%
    tmpTT_VPIArtBas.KatalogPris
    tmpTT_VPIArtBas.Linjemerknad
    tmpTT_VPIArtBas.LevDato3
    tmpTT_VPIArtBas.LevDato4
    tmpTT_VPIArtBas.VPIDato
    tmpTT_VPIArtBas.VPIBildeKode
    tmpTT_VPIArtBas.LinkVareNr
    tmpTT_VPIArtBas.Mengde
    tmpTT_VPIArtBas.ManRabIKas
    tmpTT_VPIArtBas.ArtSlag
    tmpTT_VPIArtBas.IndividType
    tmpTT_VPIArtBas.Pant
    tmpTT_VPIArtBas.BestForslag
    tmpTT_VPIArtBas.GarantiKl
    tmpTT_VPIArtBas.AntIPkn
    tmpTT_VPIArtBas.BehStatus
    tmpTT_VPIArtBas.ArtStatus
    tmpTT_VPIArtBas.KorrStatus
    tmpTT_VPIArtBas.Lokasjon
    tmpTT_VPIArtBas.RAvdNr
    tmptt_VPIArtBas.EkstStrTypeNavn
    .
  /* Løpenummer skal alltid settes lokalt. */
  ASSIGN
      tmpTT_VPIArtBas.LopNr = ?.

  FIND TT_VPIArtBas WHERE 
      TT_VPIArtBas.EkstVPILevNr = tmpTT_VPIArtBas.EkstVPILevNr AND
      TT_VPIArtBas.VareNr       = tmpTT_VPIArtBas.VareNr
      NO-ERROR.
  IF NOT AVAILABLE TT_VPIArtBas THEN
      CREATE TT_VPIArtBas.
  BUFFER-COPY tmpTT_VPIArtBas TO TT_VPIArtBas.

  RELEASE TT_VPIArtBas.
  DELETE tmpTT_VPIArtBas.
END. /* LOOPEN */


