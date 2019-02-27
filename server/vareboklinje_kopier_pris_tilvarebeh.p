/* Kopier priser fra varebok eksisterende artikler i varehåndteringsbok
   Parametere:  Vareboknr;liste over artikler;VarebehNr
   
   Opprettet: 28.01.05 av BHa 
   Endret:    18.02.05 av BHa: Tar med alle vareh.bøker for samme messe
              16.03.07 av BHa: Oppdaterer også flagg for Kjedevare og Gjennomfaktureres
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR hBuffVarebokLinje AS HANDLE NO-UNDO.
DEF VAR fArtikkelNr       AS DEC    NO-UNDO.
DEF VAR bOK               AS LOG    NO-UNDO.

DEF BUFFER bVarebehHode FOR VarebehHode.

CREATE BUFFER hBuffVarebokLinje FOR TABLE "VarebokLinje".

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffVarebokLinje).
obOk = hQuery:QUERY-PREPARE("FOR EACH VarebokLinje NO-LOCK WHERE VarebokNr = " 
                           + ENTRY(1,icParam,";")  /* Vareboknr */
                           + ENTRY(2,icParam,";")  /* Liste over artikler (can-do) */
                           ) NO-ERROR.
IF NOT obOk THEN DO:
  ocReturn = "Denne spørringen blir ikke godtatt: " + CHR(10) + "FOR EACH VarebokLinje NO-LOCK WHERE VarebokNr = " + ENTRY(1,icParam,";") + " BY " + ENTRY(3,icParam,";").
  RETURN.
END.

FIND FIRST bVarebehHode
     WHERE bVarebehHode.VarebehNr = DEC(ENTRY(3,icParam,";"))
     NO-LOCK NO-ERROR.

IF AVAIL bVarebehHode THEN DO TRANSACTION:
  hQuery:QUERY-OPEN().

  FOR EACH VarebehHode 
      WHERE VarebehHode.MesseNr     = bVarebehHode.MesseNr
        AND VarebehHode.VarebehType = bVarebehHode.VarebehType
      NO-LOCK:

    hQuery:GET-FIRST().
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      fArtikkelNr = DEC(hBuffVarebokLinje:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
      FIND FIRST VarebehLinje
           WHERE VarebehLinje.VarebehNr  = VarebehHode.VarebehNr
             AND VarebehLinje.Artikkelnr = fArtikkelNr 
           EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAIL VarebehLinje THEN 
        ASSIGN 
          VareBehLinje.AnbefaltPris      = hBuffVarebokLinje:BUFFER-FIELD("AnbefaltPris"):BUFFER-VALUE   
          VareBehLinje.DB%               = hBuffVarebokLinje:BUFFER-FIELD("DB%"):BUFFER-VALUE            
          VareBehLinje.DBKr              = hBuffVarebokLinje:BUFFER-FIELD("DBKr"):BUFFER-VALUE           
          VareBehLinje.forhKalkyle       = hBuffVarebokLinje:BUFFER-FIELD("forhKalkyle"):BUFFER-VALUE    
          VareBehLinje.forhRab%          = hBuffVarebokLinje:BUFFER-FIELD("forhRab%"):BUFFER-VALUE       
          VareBehLinje.InnkjopsPris      = hBuffVarebokLinje:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE   
          VareBehLinje.KampanjePris      = hBuffVarebokLinje:BUFFER-FIELD("KampanjePris"):BUFFER-VALUE   
          VareBehLinje.KatalogPris       = hBuffVarebokLinje:BUFFER-FIELD("KatalogPris"):BUFFER-VALUE    
          VareBehLinje.KjedeInnkPris     = hBuffVarebokLinje:BUFFER-FIELD("KjedeInnkPris"):BUFFER-VALUE  
          VareBehLinje.KjedeRab%         = hBuffVarebokLinje:BUFFER-FIELD("KjedeRab%"):BUFFER-VALUE      
          VareBehLinje.Mva%              = hBuffVarebokLinje:BUFFER-FIELD("Mva%"):BUFFER-VALUE           
          VareBehLinje.Pris              = hBuffVarebokLinje:BUFFER-FIELD("Pris"):BUFFER-VALUE           
          VareBehLinje.Rab1Kr            = hBuffVarebokLinje:BUFFER-FIELD("Rab1Kr"):BUFFER-VALUE         
          VareBehLinje.supDB%            = hBuffVarebokLinje:BUFFER-FIELD("supDB%"):BUFFER-VALUE         
          VareBehLinje.supDBKr           = hBuffVarebokLinje:BUFFER-FIELD("supDBKr"):BUFFER-VALUE        
          VareBehLinje.supInnkjopsPris   = hBuffVarebokLinje:BUFFER-FIELD("supInnkjopsPris"):BUFFER-VALUE
          VareBehLinje.supKalkyle        = hBuffVarebokLinje:BUFFER-FIELD("supKalkyle"):BUFFER-VALUE     
          VareBehLinje.supPris           = hBuffVarebokLinje:BUFFER-FIELD("supPris"):BUFFER-VALUE        
          VareBehLinje.supRab%           = hBuffVarebokLinje:BUFFER-FIELD("supRab%"):BUFFER-VALUE        
          VareBehLinje.supRab1Kr         = hBuffVarebokLinje:BUFFER-FIELD("supRab1Kr"):BUFFER-VALUE      
          VareBehLinje.supVareKost       = hBuffVarebokLinje:BUFFER-FIELD("supVareKost"):BUFFER-VALUE    
          VareBehLinje.VareKost          = hBuffVarebokLinje:BUFFER-FIELD("VareKost"):BUFFER-VALUE   
          
          VareBehLinje.LinjeMerknad      = hBuffVarebokLinje:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE  
        
          VareBehLinje.Gjennomfaktureres = hBuffVarebokLinje:BUFFER-FIELD("Gjennomfaktureres"):BUFFER-VALUE  
          VareBehLinje.Kjedevare         = hBuffVarebokLinje:BUFFER-FIELD("Kjedevare"):BUFFER-VALUE  

          /* Også artikkelinformasjonen skal synkroniseres. */
          VareBehLinje.LevNr                = hBuffVarebokLinje:BUFFER-FIELD("LevNr"):BUFFER-VALUE  
          VareBehLinje.LevNamn              = hBuffVarebokLinje:BUFFER-FIELD("LevNamn"):BUFFER-VALUE  
          VareBehLinje.LevKod               = hBuffVarebokLinje:BUFFER-FIELD("LevKod"):BUFFER-VALUE  
          VareBehLinje.LevFargKod           = hBuffVarebokLinje:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE  
          VareBehLinje.Hg                   = hBuffVarebokLinje:BUFFER-FIELD("Hg"):BUFFER-VALUE  
          VareBehLinje.HgBeskr              = hBuffVarebokLinje:BUFFER-FIELD("HgBeskr"):BUFFER-VALUE  
          VareBehLinje.Vg                   = hBuffVarebokLinje:BUFFER-FIELD("Vg"):BUFFER-VALUE  
          VareBehLinje.VgBeskr              = hBuffVarebokLinje:BUFFER-FIELD("VgBeskr"):BUFFER-VALUE  
          VareBehLinje.AvdelingNr           = hBuffVarebokLinje:BUFFER-FIELD("AvdelingNr"):BUFFER-VALUE  
          VareBehLinje.AvdelingNavn         = hBuffVarebokLinje:BUFFER-FIELD("AvdelingNavn"):BUFFER-VALUE  
          VareBehLinje.ModellFarge          = hBuffVarebokLinje:BUFFER-FIELD("ModellFarge"):BUFFER-VALUE  
          VareBehLinje.Beskr                = hBuffVarebokLinje:BUFFER-FIELD("Beskr"):BUFFER-VALUE  
          VareBehLinje.ProdNr               = hBuffVarebokLinje:BUFFER-FIELD("ProdNr"):BUFFER-VALUE 
          VareBehLinje.ProdusentBeskrivelse = hBuffVarebokLinje:BUFFER-FIELD("ProdusentBeskrivelse"):BUFFER-VALUE 
          

          .
      hQuery:GET-NEXT().
    END.
  END.
END.
ELSE ocReturn = "Finner ikke varebok " + ENTRY(1,icParam,";").

DELETE OBJECT hQuery.
DELETE OBJECT hBuffVarebokLinje.

IF ocReturn = "" THEN obOk = TRUE.

