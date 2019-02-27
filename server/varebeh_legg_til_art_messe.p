  /* Kompletterer registreringsunderlag for kombinasjon Varebeh.bok og liste med nye artikkelnumre
   Parametere:  Varebehnr,pipe-separert liste over nye artikkelnumre
   OBS:         Alle artikler som legges til vil få kalkyle fra artikkelregister
   Opprettet: 27.01.05 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix                     AS INT NO-UNDO.
DEF VAR iy                     AS INT NO-UNDO.
DEF VAR bOK                    AS LOG NO-UNDO.
DEF VAR oiWeek1                AS INT NO-UNDO.
DEF VAR oiWeek2                AS INT NO-UNDO.
DEF VAR oiWeek3                AS INT NO-UNDO.
DEF VAR oiWeek4                AS INT NO-UNDO.
DEF VAR bUse02kode             AS LOG NO-UNDO.

DEF VAR hBuffVarebehLinjeTrans AS HANDLE NO-UNDO.

FIND VarebehHode
     WHERE VarebehHode.VarebehNr = DEC(ENTRY(1,icParam))
     NO-LOCK NO-ERROR.
IF NOT AVAIL VarebehHode THEN DO:
  ocReturn = "Programmet " + PROGRAM-NAME(1) + " finner ingen varehåndteringsbok.".
  RETURN.
END.

CREATE BUFFER hBuffVarebehLinjeTrans FOR TABLE "VarebehLinjeTrans".

DO ix = 1 TO NUM-ENTRIES(ENTRY(2,icParam),"|"):
  FIND FIRST VarebehLinje
       WHERE VarebehLinje.VarebehNr  = VarebehHode.VarebehNr
         AND VarebehLinje.ArtikkelNr = DEC(ENTRY(ix,ENTRY(2,icParam),"|"))
       NO-LOCK NO-ERROR.
  IF NOT AVAIL VarebehLinje THEN DO:
    FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(ix,ENTRY(2,icParam),"|")) NO-LOCK NO-ERROR.
    FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
    FIND FIRST LevBas  OF ArtBas NO-LOCK NO-ERROR.
    FIND FIRST VarGr   OF ArtBas NO-LOCK NO-ERROR.
    FIND FIRST HuvGr   OF ArtBas NO-LOCK NO-ERROR.
    FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
    IF AVAIL ArtBas AND AVAIL ArtPris THEN DO:
      CREATE VarebehLinje.
      BUFFER-COPY ArtBas  TO VarebehLinje.
      BUFFER-COPY LevBas  TO VarebehLinje.
      BUFFER-COPY VarGr   TO VarebehLinje.
      BUFFER-COPY HuvGr   TO VarebehLinje.
      ASSIGN 
             VareBehLinje.DB%              = ArtPris.DB%[1]         
             VareBehLinje.DBKr             = ArtPris.DBKr[1]        
             VareBehLinje.forhKalkyle      = 0    
             VareBehLinje.forhRab%         = 0   
             VareBehLinje.InnkjopsPris     = ArtPris.InnkjopsPris[1]    
             VareBehLinje.KampanjePris     = 0    
             VareBehLinje.KatalogPris      = 0    
             VareBehLinje.KjedeInnkPris    = 0
             VareBehLinje.KjedeRab%        = 0   
             VareBehLinje.Mva%             = ArtPris.Mva%[1]        
             VareBehLinje.Pris             = ArtPris.Pris[1]       
             VareBehLinje.Rab1Kr           = ArtPris.Rab1Kr[1]        
             VareBehLinje.supDB%           = 0       
             VareBehLinje.supDBKr          = 0      
             VareBehLinje.supInnkjopsPris  = 0      
             VareBehLinje.supKalkyle       = 0      
             VareBehLinje.supPris          = 0       
             VareBehLinje.supRab%          = 0
             VareBehLinje.supRab1Kr        = 0
             VareBehLinje.supVareKost      = 0  
             VareBehLinje.VareKost         = ArtPris.VareKost[1]

             VarebehLinje.VarebehNr        = VarebehHode.VarebehNr
             .

      IF artbas.levdato1 NE ? THEN RUN weeknum.p (artbas.levdato1,OUTPUT oiWeek1).
      ELSE oiWeek1 = 0.
      IF artbas.levdato2 NE ? THEN RUN weeknum.p (artbas.levdato2,OUTPUT oiWeek2).
      ELSE oiWeek2 = 0.
      IF artbas.levdato3 NE ? THEN RUN weeknum.p (artbas.levdato3,OUTPUT oiWeek3).
      ELSE oiWeek3 = 0.
      IF artbas.levdato4 NE ? THEN RUN weeknum.p (artbas.levdato4,OUTPUT oiWeek4).
      ELSE oiWeek4 = 0.

      bUse02kode = NOT CAN-FIND(FIRST StrekKode OF ArtBas WHERE NOT StrekKode.Kode BEGINS "02").

      FIND FIRST StrType OF ArtBas NO-LOCK NO-ERROR.

      IF AVAIL StrType THEN 
         FOR EACH VarebehLinjeThode NO-LOCK
             WHERE VarebehLinjeThode.VarebehNr = VarebehHode.VarebehNr:

          DO iy = 1 TO NUM-ENTRIES(StrType.fordeling):
            FOR EACH StrekKode OF ArtBas NO-LOCK
                WHERE Strekkode.kode > ""
                  AND StrekKode.StrKode = INT(ENTRY(iy,StrType.fordeling))
              , FIRST StrKonv OF StrekKode NO-LOCK
                BREAK BY StrekKode.StrKode:
  
              IF NOT bUse02kode AND StrekKode.kode BEGINS "02" THEN NEXT.
  
              IF LAST-OF(StrekKode.StrKode) THEN DO:
                hBuffVarebehLinjeTrans:BUFFER-CREATE().
                ASSIGN hBuffVarebehLinjeTrans:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE  = DEC(ENTRY(1,icParam))
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE   = VarebehLinjeThode.ButikkNr
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = VarebehLinje.ArtikkelNr
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("Kode"):BUFFER-VALUE       = strekkode.kode
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("SeqNr"):BUFFER-VALUE      = iy
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato1"):BUFFER-VALUE   = ArtBas.LevDato1
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato2"):BUFFER-VALUE   = ArtBas.LevDato2
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato3"):BUFFER-VALUE   = ArtBas.LevDato3
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("LevDato4"):BUFFER-VALUE   = ArtBas.LevDato4
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke1"):BUFFER-VALUE    = oiWeek1
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke2"):BUFFER-VALUE    = oiWeek2
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke3"):BUFFER-VALUE    = oiWeek3
                       hBuffVarebehLinjeTrans:BUFFER-FIELD("Levuke4"):BUFFER-VALUE    = oiWeek4
                       .
              END.
            END.
          END.

      END.
    END.
  END.
  ELSE ocReturn = ocReturn + "Artikkel " + ENTRY(ix,ENTRY(2,icParam),"|") + " finnes allerede" + CHR(10).
END.

DELETE OBJECT hBuffVarebehLinjeTrans.
IF ocReturn = "" THEN obOk = TRUE.


 

