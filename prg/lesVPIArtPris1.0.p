/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER piRecType    AS INT NO-UNDO.
DEF INPUT  PARAMETER piAntRecord  AS INT NO-UNDO.
DEF OUTPUT PARAMETER piAntLinjer  AS INT NO-UNDO.

DEF VAR piLoop AS INT NO-UNDO.

{tmptt_VPIArtPris.i &SHARED=SHARED}

LOOPEN:
DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_VPIArtPris.
    ASSIGN
      piAntLinjer = piAntLinjer + 1
      tmpTT_VPIArtPris.RecType = piRecType
      .
    IMPORT STREAM InnFil
      tmpTT_VPIArtPris.EkstVPILevNr    
      tmpTT_VPIArtPris.VareNr          
      tmpTT_VPIArtPris.EDato           
      tmpTT_VPIArtPris.ETid            
      tmpTT_VPIArtPris.BrukerID        
      tmpTT_VPIArtPris.RegistrertDato  
      tmpTT_VPIArtPris.RegistrertTid   
      tmpTT_VPIArtPris.RegistrertAv    
      tmpTT_VPIArtPris.ProfilNr        
      tmpTT_VPIArtPris.ArtikkelNr      
      tmpTT_VPIArtPris.Tilbud          
      tmpTT_VPIArtPris.VareKost[1]     
      tmpTT_VPIArtPris.VareKost[2]     
      tmpTT_VPIArtPris.MvaKr[1]        
      tmpTT_VPIArtPris.MvaKr[2]        
      tmpTT_VPIArtPris.LevNr           
      tmpTT_VPIArtPris.EuroManuel      
      tmpTT_VPIArtPris.ValPris[1]      
      tmpTT_VPIArtPris.ValPris[2]      
      tmpTT_VPIArtPris.Rab1Kr[1]       
      tmpTT_VPIArtPris.Rab1Kr[2]       
      tmpTT_VPIArtPris.Rab1%[1]        
      tmpTT_VPIArtPris.Rab1%[2]        
      tmpTT_VPIArtPris.Rab2Kr[1]       
      tmpTT_VPIArtPris.Rab2Kr[2]       
      tmpTT_VPIArtPris.Rab2%[1]        
      tmpTT_VPIArtPris.Rab2%[2]        
      tmpTT_VPIArtPris.Frakt[1]        
      tmpTT_VPIArtPris.Frakt[2]        
      tmpTT_VPIArtPris.Frakt%[1]       
      tmpTT_VPIArtPris.Frakt%[2]       
      tmpTT_VPIArtPris.DivKostKr[1]    
      tmpTT_VPIArtPris.DivKostKr[2]    
      tmpTT_VPIArtPris.DivKost%[1]     
      tmpTT_VPIArtPris.DivKost%[2]     
      tmpTT_VPIArtPris.Rab3Kr[1]       
      tmpTT_VPIArtPris.Rab3Kr[2]       
      tmpTT_VPIArtPris.Rab3%[1]        
      tmpTT_VPIArtPris.Rab3%[2]        
      tmpTT_VPIArtPris.DBKr[1]         
      tmpTT_VPIArtPris.DBKr[2]         
      tmpTT_VPIArtPris.DB%[1]          
      tmpTT_VPIArtPris.DB%[2]          
      tmpTT_VPIArtPris.EuroPris[1]     
      tmpTT_VPIArtPris.EuroPris[2]     
      tmpTT_VPIArtPris.InnkjopsPris[1] 
      tmpTT_VPIArtPris.InnkjopsPris[2] 
      tmpTT_VPIArtPris.Mva%[1]         
      tmpTT_VPIArtPris.Mva%[2]         
      tmpTT_VPIArtPris.Pris[1]         
      tmpTT_VPIArtPris.Pris[2]         
      tmpTT_VPIArtPris.AktivFraDato    
      tmpTT_VPIArtPris.AktivFraTid     
      tmpTT_VPIArtPris.TilbudFraDato   
      tmpTT_VPIArtPris.TilbudTilDato   
      tmpTT_VPIArtPris.TilbudFraTid    
      tmpTT_VPIArtPris.TilbudTilTid    
      tmpTT_VPIArtPris.TilbudTimeStyrt 
      .
    FIND TT_VPIArtPris WHERE 
        TT_VPIArtPris.EkstVPILevNr = tmpTT_VPIArtPris.EkstVPILevNr AND
        TT_VPIArtPris.VareNr       = tmpTT_VPIArtPris.VareNr AND
        TT_VPIArtPris.ProfilNr     = tmpTT_VPIArtPris.ProfilNr
        NO-ERROR.
    IF NOT AVAILABLE TT_VPIArtPris THEN
        CREATE TT_VPIArtPris.
    BUFFER-COPY tmpTT_VPIArtPris TO TT_VPIArtPris.

    RELEASE TT_VPIArtPris.
    DELETE tmpTT_VPIArtPris.
END. /* LOOPEN */


