if (select name from sysobjects 
    where name = 'detaljer' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table detaljer
go
CREATE TABLE detaljer (
  jobbnr decimal(15,2) null,
  char1 varchar (80) null,
  char2 varchar (80) null,
  char3 varchar (80) null,
  seqnr integer null,
  defchar##1 varchar (4) null,
  defchar##2 varchar (4) null,
  defchar##3 varchar (4) null,
  defchar##4 varchar (4) null,
  defchar##5 varchar (4) null,
  defchar##6 varchar (4) null,
  defchar##7 varchar (4) null,
  defchar##8 varchar (4) null,
  defchar##9 varchar (4) null,
  defchar##10 varchar (4) null,
  defchar##11 varchar (4) null,
  defchar##12 varchar (4) null,
  defchar##13 varchar (4) null,
  defchar##14 varchar (4) null,
  defchar##15 varchar (4) null,
  defchar##16 varchar (4) null,
  defchar##17 varchar (4) null,
  defchar##18 varchar (4) null,
  defchar##19 varchar (4) null,
  defchar##20 varchar (4) null,
  defchar##21 varchar (4) null,
  defchar##22 varchar (4) null,
  defchar##23 varchar (4) null,
  defchar##24 varchar (4) null,
  defchar##25 varchar (4) null,
  defchar##26 varchar (4) null,
  defchar##27 varchar (4) null,
  defchar##28 varchar (4) null,
  defchar##29 varchar (4) null,
  defchar##30 varchar (4) null,
  defchar##31 varchar (4) null,
  defchar##32 varchar (4) null,
  defchar##33 varchar (4) null,
  defchar##34 varchar (4) null,
  defchar##35 varchar (4) null,
  defchar##36 varchar (4) null,
  defchar##37 varchar (4) null,
  defchar##38 varchar (4) null,
  defchar##39 varchar (4) null,
  defchar##40 varchar (4) null,
  defchar##41 varchar (4) null,
  defchar##42 varchar (4) null,
  defchar##43 varchar (4) null,
  defchar##44 varchar (4) null,
  defchar##45 varchar (4) null,
  defchar##46 varchar (4) null,
  defchar##47 varchar (4) null,
  defchar##48 varchar (4) null,
  defchar##49 varchar (4) null,
  defchar##50 varchar (4) null,
  defdec##1 decimal(7,2) null,
  defdec##2 decimal(7,2) null,
  defdec##3 decimal(7,2) null,
  defdec##4 decimal(7,2) null,
  defdec##5 decimal(7,2) null,
  defdec##6 decimal(7,2) null,
  defdec##7 decimal(7,2) null,
  defdec##8 decimal(7,2) null,
  defdec##9 decimal(7,2) null,
  defdec##10 decimal(7,2) null,
  defdec##11 decimal(7,2) null,
  defdec##12 decimal(7,2) null,
  defdec##13 decimal(7,2) null,
  defdec##14 decimal(7,2) null,
  defdec##15 decimal(7,2) null,
  defdec##16 decimal(7,2) null,
  defdec##17 decimal(7,2) null,
  defdec##18 decimal(7,2) null,
  defdec##19 decimal(7,2) null,
  defdec##20 decimal(7,2) null,
  defdec##21 decimal(7,2) null,
  defdec##22 decimal(7,2) null,
  defdec##23 decimal(7,2) null,
  defdec##24 decimal(7,2) null,
  defdec##25 decimal(7,2) null,
  defdec##26 decimal(7,2) null,
  defdec##27 decimal(7,2) null,
  defdec##28 decimal(7,2) null,
  defdec##29 decimal(7,2) null,
  defdec##30 decimal(7,2) null,
  defdec##31 decimal(7,2) null,
  defdec##32 decimal(7,2) null,
  defdec##33 decimal(7,2) null,
  defdec##34 decimal(7,2) null,
  defdec##35 decimal(7,2) null,
  defdec##36 decimal(7,2) null,
  defdec##37 decimal(7,2) null,
  defdec##38 decimal(7,2) null,
  defdec##39 decimal(7,2) null,
  defdec##40 decimal(7,2) null,
  defdec##41 decimal(7,2) null,
  defdec##42 decimal(7,2) null,
  defdec##43 decimal(7,2) null,
  defdec##44 decimal(7,2) null,
  defdec##45 decimal(7,2) null,
  defdec##46 decimal(7,2) null,
  defdec##47 decimal(7,2) null,
  defdec##48 decimal(7,2) null,
  defdec##49 decimal(7,2) null,
  defdec##50 decimal(7,2) null,
  divchar##1 varchar (40) null,
  divchar##2 varchar (40) null,
  divchar##3 varchar (40) null,
  divchar##4 varchar (40) null,
  divchar##5 varchar (40) null,
  divchar##6 varchar (40) null,
  divchar##7 varchar (40) null,
  divchar##8 varchar (40) null,
  divchar##9 varchar (40) null,
  divchar##10 varchar (40) null,
  divchar##11 varchar (40) null,
  divchar##12 varchar (40) null,
  divchar##13 varchar (40) null,
  divchar##14 varchar (40) null,
  divchar##15 varchar (40) null,
  divchar##16 varchar (40) null,
  divchar##17 varchar (40) null,
  divchar##18 varchar (40) null,
  divchar##19 varchar (40) null,
  divchar##20 varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_detaljer ON detaljer for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from detaljer t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX detaljer#_#progress_recid ON detaljer (PROGRESS_RECID)
go
CREATE UNIQUE INDEX detaljer#_#progress_recid_ident_ ON detaljer (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX detaljer##detaljer ON detaljer (jobbnr, char1, char2, char3, seqnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ht_filhode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ht_filhode
go
CREATE TABLE ht_filhode (
  htfilid integer null,
  filnavn varchar (20) null,
  filekst varchar (30) null,
  fildato datetime null,
  filtid integer null,
  innlestfra varchar (30) null,
  filstorrelse varchar (30) null,
  antlinjer integer null,
  annulertdato datetime null,
  annulertav varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (30) null,
  typeid integer null,
  tellenr integer null,
  butik integer null,
  oppdatert datetime null,
  oppdaterttid integer null,
  oppdatertav varchar (30) null,
  feilifil tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ht_filhode ON ht_filhode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ht_filhode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ht_filhode#_#progress_recid ON ht_filhode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ht_filhode#_#progress_recid_ident_ ON ht_filhode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX ht_filhode##butikk ON ht_filhode (butik, htfilid DESC, PROGRESS_RECID)
go
CREATE INDEX ht_filhode##fildato ON ht_filhode (tellenr, fildato, filtid, filnavn, filekst, PROGRESS_RECID)
go
CREATE INDEX ht_filhode##filnavn ON ht_filhode (tellenr, filnavn, filekst, fildato, filtid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX ht_filhode##htfilid ON ht_filhode (htfilid)
go
CREATE INDEX ht_filhode##telldato ON ht_filhode (tellenr, fildato, filtid, filnavn, filekst, PROGRESS_RECID)
go
CREATE INDEX ht_filhode##tellenrhtfilid ON ht_filhode (tellenr, htfilid DESC, PROGRESS_RECID)
go
CREATE INDEX ht_filhode##telling ON ht_filhode (tellenr, butik, filnavn, filekst, fildato, filtid DESC, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ht_fillinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ht_fillinje
go
CREATE TABLE ht_fillinje (
  htfilid integer null,
  linjeid integer null,
  linjedata varchar (50) null,
  ok tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ht_fillinje ON ht_fillinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ht_fillinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ht_fillinje#_#progress_recid ON ht_fillinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ht_fillinje#_#progress_recid_ident_ ON ht_fillinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX ht_fillinje##feil ON ht_fillinje (htfilid, ok, linjedata, PROGRESS_RECID)
go
CREATE UNIQUE INDEX ht_fillinje##fillinje ON ht_fillinje (htfilid, linjeid)
go
CREATE INDEX ht_fillinje##linjedata ON ht_fillinje (htfilid, linjedata, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'importdetalj' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table importdetalj
go
CREATE TABLE importdetalj (
  batchnr integer null,
  linjenr integer null,
  feil varchar (25) null,
  felt##1 varchar (12) null,
  felt##2 varchar (12) null,
  felt##3 varchar (12) null,
  felt##4 varchar (12) null,
  felt##5 varchar (12) null,
  felt##6 varchar (12) null,
  felt##7 varchar (12) null,
  felt##8 varchar (12) null,
  felt##9 varchar (12) null,
  felt##10 varchar (12) null,
  felt##11 varchar (12) null,
  felt##12 varchar (12) null,
  felt##13 varchar (12) null,
  felt##14 varchar (12) null,
  felt##15 varchar (12) null,
  felt##16 varchar (12) null,
  felt##17 varchar (12) null,
  felt##18 varchar (12) null,
  felt##19 varchar (12) null,
  felt##20 varchar (12) null,
  felt##21 varchar (12) null,
  felt##22 varchar (12) null,
  felt##23 varchar (12) null,
  felt##24 varchar (12) null,
  felt##25 varchar (12) null,
  felt##26 varchar (12) null,
  felt##27 varchar (12) null,
  felt##28 varchar (12) null,
  felt##29 varchar (12) null,
  felt##30 varchar (12) null,
  felt##31 varchar (12) null,
  felt##32 varchar (12) null,
  felt##33 varchar (12) null,
  felt##34 varchar (12) null,
  felt##35 varchar (12) null,
  felt##36 varchar (12) null,
  felt##37 varchar (12) null,
  felt##38 varchar (12) null,
  felt##39 varchar (12) null,
  felt##40 varchar (12) null,
  felt##41 varchar (12) null,
  felt##42 varchar (12) null,
  felt##43 varchar (12) null,
  felt##44 varchar (12) null,
  felt##45 varchar (12) null,
  felt##46 varchar (12) null,
  felt##47 varchar (12) null,
  felt##48 varchar (12) null,
  felt##49 varchar (12) null,
  felt##50 varchar (12) null,
  felt##51 varchar (12) null,
  felt##52 varchar (12) null,
  felt##53 varchar (12) null,
  felt##54 varchar (12) null,
  felt##55 varchar (12) null,
  felt##56 varchar (12) null,
  felt##57 varchar (12) null,
  felt##58 varchar (12) null,
  felt##59 varchar (12) null,
  felt##60 varchar (12) null,
  felt##61 varchar (12) null,
  felt##62 varchar (12) null,
  felt##63 varchar (12) null,
  felt##64 varchar (12) null,
  felt##65 varchar (12) null,
  felt##66 varchar (12) null,
  felt##67 varchar (12) null,
  felt##68 varchar (12) null,
  felt##69 varchar (12) null,
  felt##70 varchar (12) null,
  felt##71 varchar (12) null,
  felt##72 varchar (12) null,
  felt##73 varchar (12) null,
  felt##74 varchar (12) null,
  felt##75 varchar (12) null,
  felt##76 varchar (12) null,
  felt##77 varchar (12) null,
  felt##78 varchar (12) null,
  felt##79 varchar (12) null,
  felt##80 varchar (12) null,
  tabell varchar (15) null,
  type varchar (30) null,
  seqnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_importdetalj ON importdetalj for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from importdetalj t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX importdetalj#_#progress_recid ON importdetalj (PROGRESS_RECID)
go
CREATE UNIQUE INDEX importdetalj#_#progress_recid_ident_ ON importdetalj (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX importdetalj##importdetalj ON importdetalj (batchnr, tabell, type, linjenr, seqnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'importhode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table importhode
go
CREATE TABLE importhode (
  batchnr integer null,
  beskrivelse varchar (40) null,
  kundekode varchar (15) null,
  bunt integer null,
  importdato datetime null,
  merknad varchar (30) null,
  importtid integer null,
  oppdatertdato datetime null,
  oppdaterttid datetime null,
  oppdatertav varchar (15) null,
  importkatalog varchar (40) null,
  bildekatalog varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_importhode ON importhode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from importhode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX importhode#_#progress_recid ON importhode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX importhode#_#progress_recid_ident_ ON importhode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX importhode##beskrivelse ON importhode (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX importhode##importhode ON importhode (batchnr)
go
if (select name from sysobjects 
    where name = 'importlinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table importlinje
go
CREATE TABLE importlinje (
  batchnr integer null,
  linjenr integer null,
  feil varchar (25) null,
  felt##1 varchar (12) null,
  felt##2 varchar (12) null,
  felt##3 varchar (12) null,
  felt##4 varchar (12) null,
  felt##5 varchar (12) null,
  felt##6 varchar (12) null,
  felt##7 varchar (12) null,
  felt##8 varchar (12) null,
  felt##9 varchar (12) null,
  felt##10 varchar (12) null,
  felt##11 varchar (12) null,
  felt##12 varchar (12) null,
  felt##13 varchar (12) null,
  felt##14 varchar (12) null,
  felt##15 varchar (12) null,
  felt##16 varchar (12) null,
  felt##17 varchar (12) null,
  felt##18 varchar (12) null,
  felt##19 varchar (12) null,
  felt##20 varchar (12) null,
  felt##21 varchar (12) null,
  felt##22 varchar (12) null,
  felt##23 varchar (12) null,
  felt##24 varchar (12) null,
  felt##25 varchar (12) null,
  felt##26 varchar (12) null,
  felt##27 varchar (12) null,
  felt##28 varchar (12) null,
  felt##29 varchar (12) null,
  felt##30 varchar (12) null,
  felt##31 varchar (12) null,
  felt##32 varchar (12) null,
  felt##33 varchar (12) null,
  felt##34 varchar (12) null,
  felt##35 varchar (12) null,
  felt##36 varchar (12) null,
  felt##37 varchar (12) null,
  felt##38 varchar (12) null,
  felt##39 varchar (12) null,
  felt##40 varchar (12) null,
  felt##41 varchar (12) null,
  felt##42 varchar (12) null,
  felt##43 varchar (12) null,
  felt##44 varchar (12) null,
  felt##45 varchar (12) null,
  felt##46 varchar (12) null,
  felt##47 varchar (12) null,
  felt##48 varchar (12) null,
  felt##49 varchar (12) null,
  felt##50 varchar (12) null,
  felt##51 varchar (12) null,
  felt##52 varchar (12) null,
  felt##53 varchar (12) null,
  felt##54 varchar (12) null,
  felt##55 varchar (12) null,
  felt##56 varchar (12) null,
  felt##57 varchar (12) null,
  felt##58 varchar (12) null,
  felt##59 varchar (12) null,
  felt##60 varchar (12) null,
  felt##61 varchar (12) null,
  felt##62 varchar (12) null,
  felt##63 varchar (12) null,
  felt##64 varchar (12) null,
  felt##65 varchar (12) null,
  felt##66 varchar (12) null,
  felt##67 varchar (12) null,
  felt##68 varchar (12) null,
  felt##69 varchar (12) null,
  felt##70 varchar (12) null,
  felt##71 varchar (12) null,
  felt##72 varchar (12) null,
  felt##73 varchar (12) null,
  felt##74 varchar (12) null,
  felt##75 varchar (12) null,
  felt##76 varchar (12) null,
  felt##77 varchar (12) null,
  felt##78 varchar (12) null,
  felt##79 varchar (12) null,
  felt##80 varchar (12) null,
  tabell varchar (15) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_importlinje ON importlinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from importlinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX importlinje#_#progress_recid ON importlinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX importlinje#_#progress_recid_ident_ ON importlinje (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX importlinje##importlinje ON importlinje (batchnr, tabell, linjenr)
go
if (select name from sysobjects 
    where name = 'jobblinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jobblinje
go
CREATE TABLE jobblinje (
  jobbnr decimal(15,2) null,
  char1 varchar (80) null,
  char2 varchar (80) null,
  char3 varchar (80) null,
  divx##1 varchar (80) null,
  divx##2 varchar (80) null,
  divx##3 varchar (80) null,
  divx##4 varchar (80) null,
  divx##5 varchar (80) null,
  divx##6 varchar (80) null,
  divx##7 varchar (80) null,
  divx##8 varchar (80) null,
  divx##9 varchar (80) null,
  divx##10 varchar (80) null,
  divx##11 varchar (80) null,
  divx##12 varchar (80) null,
  divx##13 varchar (80) null,
  divx##14 varchar (80) null,
  divx##15 varchar (80) null,
  divx##16 varchar (80) null,
  divx##17 varchar (80) null,
  divx##18 varchar (80) null,
  divx##19 varchar (80) null,
  divx##20 varchar (80) null,
  divx##21 varchar (80) null,
  divx##22 varchar (80) null,
  divx##23 varchar (80) null,
  divx##24 varchar (80) null,
  divx##25 varchar (80) null,
  divx##26 varchar (80) null,
  divx##27 varchar (80) null,
  divx##28 varchar (80) null,
  divx##29 varchar (80) null,
  divx##30 varchar (80) null,
  divx##31 varchar (80) null,
  divx##32 varchar (80) null,
  divx##33 varchar (80) null,
  divx##34 varchar (80) null,
  divx##35 varchar (80) null,
  divx##36 varchar (80) null,
  divx##37 varchar (80) null,
  divx##38 varchar (80) null,
  divx##39 varchar (80) null,
  divx##40 varchar (80) null,
  divx##41 varchar (80) null,
  divx##42 varchar (80) null,
  divx##43 varchar (80) null,
  divx##44 varchar (80) null,
  divx##45 varchar (80) null,
  divx##46 varchar (80) null,
  divx##47 varchar (80) null,
  divx##48 varchar (80) null,
  divx##49 varchar (80) null,
  divx##50 varchar (80) null,
  decx##1 decimal(7,2) null,
  decx##2 decimal(7,2) null,
  decx##3 decimal(7,2) null,
  decx##4 decimal(7,2) null,
  decx##5 decimal(7,2) null,
  decx##6 decimal(7,2) null,
  decx##7 decimal(7,2) null,
  decx##8 decimal(7,2) null,
  decx##9 decimal(7,2) null,
  decx##10 decimal(7,2) null,
  decx##11 decimal(7,2) null,
  decx##12 decimal(7,2) null,
  decx##13 decimal(7,2) null,
  decx##14 decimal(7,2) null,
  decx##15 decimal(7,2) null,
  decx##16 decimal(7,2) null,
  decx##17 decimal(7,2) null,
  decx##18 decimal(7,2) null,
  decx##19 decimal(7,2) null,
  decx##20 decimal(7,2) null,
  decx##21 decimal(7,2) null,
  decx##22 decimal(7,2) null,
  decx##23 decimal(7,2) null,
  decx##24 decimal(7,2) null,
  decx##25 decimal(7,2) null,
  decx##26 decimal(7,2) null,
  decx##27 decimal(7,2) null,
  decx##28 decimal(7,2) null,
  decx##29 decimal(7,2) null,
  decx##30 decimal(7,2) null,
  decx##31 decimal(7,2) null,
  decx##32 decimal(7,2) null,
  decx##33 decimal(7,2) null,
  decx##34 decimal(7,2) null,
  decx##35 decimal(7,2) null,
  decx##36 decimal(7,2) null,
  decx##37 decimal(7,2) null,
  decx##38 decimal(7,2) null,
  decx##39 decimal(7,2) null,
  decx##40 decimal(7,2) null,
  decx##41 decimal(7,2) null,
  decx##42 decimal(7,2) null,
  decx##43 decimal(7,2) null,
  decx##44 decimal(7,2) null,
  decx##45 decimal(7,2) null,
  decx##46 decimal(7,2) null,
  decx##47 decimal(7,2) null,
  decx##48 decimal(7,2) null,
  decx##49 decimal(7,2) null,
  decx##50 decimal(7,2) null,
  div2x##1 varchar (80) null,
  div2x##2 varchar (80) null,
  div2x##3 varchar (80) null,
  div2x##4 varchar (80) null,
  div2x##5 varchar (80) null,
  div2x##6 varchar (80) null,
  div2x##7 varchar (80) null,
  div2x##8 varchar (80) null,
  div2x##9 varchar (80) null,
  div2x##10 varchar (80) null,
  div2x##11 varchar (80) null,
  div2x##12 varchar (80) null,
  div2x##13 varchar (80) null,
  div2x##14 varchar (80) null,
  div2x##15 varchar (80) null,
  div2x##16 varchar (80) null,
  div2x##17 varchar (80) null,
  div2x##18 varchar (80) null,
  div2x##19 varchar (80) null,
  div2x##20 varchar (80) null,
  div2x##21 varchar (80) null,
  div2x##22 varchar (80) null,
  div2x##23 varchar (80) null,
  div2x##24 varchar (80) null,
  div2x##25 varchar (80) null,
  div2x##26 varchar (80) null,
  div2x##27 varchar (80) null,
  div2x##28 varchar (80) null,
  div2x##29 varchar (80) null,
  div2x##30 varchar (80) null,
  div2x##31 varchar (80) null,
  div2x##32 varchar (80) null,
  div2x##33 varchar (80) null,
  div2x##34 varchar (80) null,
  div2x##35 varchar (80) null,
  div2x##36 varchar (80) null,
  div2x##37 varchar (80) null,
  div2x##38 varchar (80) null,
  div2x##39 varchar (80) null,
  div2x##40 varchar (80) null,
  div2x##41 varchar (80) null,
  div2x##42 varchar (80) null,
  div2x##43 varchar (80) null,
  div2x##44 varchar (80) null,
  div2x##45 varchar (80) null,
  div2x##46 varchar (80) null,
  div2x##47 varchar (80) null,
  div2x##48 varchar (80) null,
  div2x##49 varchar (80) null,
  div2x##50 varchar (80) null,
  dec2x##1 decimal(7,2) null,
  dec2x##2 decimal(7,2) null,
  dec2x##3 decimal(7,2) null,
  dec2x##4 decimal(7,2) null,
  dec2x##5 decimal(7,2) null,
  dec2x##6 decimal(7,2) null,
  dec2x##7 decimal(7,2) null,
  dec2x##8 decimal(7,2) null,
  dec2x##9 decimal(7,2) null,
  dec2x##10 decimal(7,2) null,
  dec2x##11 decimal(7,2) null,
  dec2x##12 decimal(7,2) null,
  dec2x##13 decimal(7,2) null,
  dec2x##14 decimal(7,2) null,
  dec2x##15 decimal(7,2) null,
  dec2x##16 decimal(7,2) null,
  dec2x##17 decimal(7,2) null,
  dec2x##18 decimal(7,2) null,
  dec2x##19 decimal(7,2) null,
  dec2x##20 decimal(7,2) null,
  dec2x##21 decimal(7,2) null,
  dec2x##22 decimal(7,2) null,
  dec2x##23 decimal(7,2) null,
  dec2x##24 decimal(7,2) null,
  dec2x##25 decimal(7,2) null,
  dec2x##26 decimal(7,2) null,
  dec2x##27 decimal(7,2) null,
  dec2x##28 decimal(7,2) null,
  dec2x##29 decimal(7,2) null,
  dec2x##30 decimal(7,2) null,
  dec2x##31 decimal(7,2) null,
  dec2x##32 decimal(7,2) null,
  dec2x##33 decimal(7,2) null,
  dec2x##34 decimal(7,2) null,
  dec2x##35 decimal(7,2) null,
  dec2x##36 decimal(7,2) null,
  dec2x##37 decimal(7,2) null,
  dec2x##38 decimal(7,2) null,
  dec2x##39 decimal(7,2) null,
  dec2x##40 decimal(7,2) null,
  dec2x##41 decimal(7,2) null,
  dec2x##42 decimal(7,2) null,
  dec2x##43 decimal(7,2) null,
  dec2x##44 decimal(7,2) null,
  dec2x##45 decimal(7,2) null,
  dec2x##46 decimal(7,2) null,
  dec2x##47 decimal(7,2) null,
  dec2x##48 decimal(7,2) null,
  dec2x##49 decimal(7,2) null,
  dec2x##50 decimal(7,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jobblinje ON jobblinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jobblinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jobblinje#_#progress_recid ON jobblinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jobblinje#_#progress_recid_ident_ ON jobblinje (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jobblinje##jobblinje ON jobblinje (jobbnr, char1, char2, char3)
go
exit
