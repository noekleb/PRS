
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
DEF INPUT  PARAMETER piRecType   AS INT NO-UNDO.
DEF INPUT  PARAMETER piAntRecord AS INT NO-UNDO.
DEF OUTPUT PARAMETER piAntLinjer AS INT NO-UNDO.

{tmptt_LevBas.i &SHARED=SHARED}

DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_LevBas.
    ASSIGN
      tmpTT_LevBas.RecType = piRecType
      piAntLinjer = piAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_LevBas.levnr         
      tmpTT_LevBas.levnamn       
      tmpTT_LevBas.levadr        
      tmpTT_LevBas.levponr       
      tmpTT_LevBas.levpadr       
      tmpTT_LevBas.levland       
      tmpTT_LevBas.levtel        
      tmpTT_LevBas.levkon        
      tmpTT_LevBas.levsal        
      tmpTT_LevBas.telefax       
      tmpTT_LevBas.telex         
      tmpTT_LevBas.kommentar[1]  
      tmpTT_LevBas.kommentar[2]  
      tmpTT_LevBas.kommentar[3]  
      tmpTT_LevBas.kommentar[4]  
      tmpTT_LevBas.valkod        
      tmpTT_LevBas.koadr         
      tmpTT_LevBas.koponr        
      tmpTT_LevBas.kopadr        
      tmpTT_LevBas.koland        
      tmpTT_LevBas.kotel         
      tmpTT_LevBas.kotelefax     
      tmpTT_LevBas.kotelex       
      tmpTT_LevBas.betant        
      tmpTT_LevBas.EDato         
      tmpTT_LevBas.ETid          
      tmpTT_LevBas.BrukerID      
      tmpTT_LevBas.BildNr        
      tmpTT_LevBas.RegistrertDato
      tmpTT_LevBas.RegistrertTid 
      tmpTT_LevBas.RegistrertAv  
      tmpTT_LevBas.Notat         
      tmpTT_LevBas.VisDivInfo[ 1]
      tmpTT_LevBas.VisDivInfo[ 2]
      tmpTT_LevBas.VisDivInfo[ 3]
      tmpTT_LevBas.VisDivInfo[ 4]
      tmpTT_LevBas.VisDivInfo[ 5]
      tmpTT_LevBas.VisDivInfo[ 6]
      tmpTT_LevBas.VisDivInfo[ 7]
      tmpTT_LevBas.VisDivInfo[ 8]
      tmpTT_LevBas.VisDivInfo[ 9]
      tmpTT_LevBas.VisDivInfo[10]
      tmpTT_LevBas.VisDivInfo[11]
      tmpTT_LevBas.VisDivInfo[12]
      tmpTT_LevBas.VisDivInfo[13]
      tmpTT_LevBas.VisDivInfo[14]
      tmpTT_LevBas.VisDivInfo[15]
      tmpTT_LevBas.VisDivInfo[16]
      tmpTT_LevBas.VisDivInfo[17]
      tmpTT_LevBas.VisDivInfo[18]
      tmpTT_LevBas.VisDivInfo[19]
      tmpTT_LevBas.VisDivInfo[20]
      tmpTT_LevBas.Lng           
      tmpTT_LevBas.E_MailKontakt 
      tmpTT_LevBas.E_MailLev
      tmpTT_LevBas.KjedeAvtale
      tmpTT_LevBas.ReklAdresse1      
      tmpTT_LevBas.ReklAdresse2      
      tmpTT_LevBas.ReklPostNr        
      tmpTT_LevBas.ReklPostBoks      
      tmpTT_LevBas.Rab1%             
      tmpTT_LevBas.Rab2%             
      tmpTT_LevBas.Frakt%            
      tmpTT_LevBas.DivKost%          
      tmpTT_LevBas.Rab3%             
      tmpTT_LevBas.EgetKundeNrHosLev 
      tmpTT_LevBas.supRab1%          
      tmpTT_LevBas.supRab2%          
      tmpTT_LevBas.supDivKost%       
      tmpTT_LevBas.supRab3%          
      .
    FIND TT_LevBas WHERE 
        TT_LevBas.LevNr = tmpTT_LevBas.LevNr NO-ERROR.
    IF NOT AVAILABLE TT_LevBas THEN
        CREATE TT_LevBas.
    BUFFER-COPY tmpTT_LevBas TO TT_LevBas.

    RELEASE TT_LevBas.
    DELETE tmpTT_LevBas.
END. /* LOOPEN */

