if (select name from sysobjects 
    where name = 'aktivitet' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table aktivitet
go
CREATE TABLE aktivitet (
  aktnr integer null,
  beskrivelse varchar (30) null,
  merknad varchar (50) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_aktivitet ON aktivitet for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from aktivitet t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX aktivitet#_#progress_recid ON aktivitet (PROGRESS_RECID)
go
CREATE UNIQUE INDEX aktivitet#_#progress_recid_ident_ ON aktivitet (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX aktivitet##aktnr ON aktivitet (aktnr)
go
CREATE INDEX aktivitet##beskrivelse ON aktivitet (beskrivelse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'akt_mal' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table akt_mal
go
CREATE TABLE akt_mal (
  start_tid##1 integer null,
  start_tid##2 integer null,
  start_tid##3 integer null,
  start_tid##4 integer null,
  start_tid##5 integer null,
  start_tid##6 integer null,
  start_tid##7 integer null,
  start_tid##8 integer null,
  start_tid##9 integer null,
  start_tid##10 integer null,
  start_tid##11 integer null,
  start_tid##12 integer null,
  start_tid##13 integer null,
  start_tid##14 integer null,
  label_tid##1 varchar (6) null,
  label_tid##2 varchar (6) null,
  label_tid##3 varchar (6) null,
  label_tid##4 varchar (6) null,
  label_tid##5 varchar (6) null,
  label_tid##6 varchar (6) null,
  label_tid##7 varchar (6) null,
  label_tid##8 varchar (6) null,
  label_tid##9 varchar (6) null,
  label_tid##10 varchar (6) null,
  label_tid##11 varchar (6) null,
  label_tid##12 varchar (6) null,
  label_tid##13 varchar (6) null,
  label_tid##14 varchar (6) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_akt_mal ON akt_mal for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from akt_mal t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX akt_mal#_#progress_recid ON akt_mal (PROGRESS_RECID)
go
CREATE UNIQUE INDEX akt_mal#_#progress_recid_ident_ ON akt_mal (PROGRESS_RECID_IDENT_ )
go
if (select name from sysobjects 
    where name = 'akt_rapp' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table akt_rapp
go
CREATE TABLE akt_rapp (
  dato datetime null,
  uke_dag integer null,
  uke_nr integer null,
  mnd integer null,
  butik integer null,
  kasse integer null,
  tid integer null,
  tid_txt varchar (5) null,
  oms_ant decimal(11,4) null,
  oms_verd decimal(9,2) null,
  ant_kunder integer null,
  svk decimal(9,2) null,
  ant_ret integer null,
  verd_ret decimal(11,2) null,
  ant_kvitto integer null,
  mva_kr decimal(9,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_akt_rapp ON akt_rapp for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from akt_rapp t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX akt_rapp#_#progress_recid ON akt_rapp (PROGRESS_RECID)
go
CREATE UNIQUE INDEX akt_rapp#_#progress_recid_ident_ ON akt_rapp (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX akt_rapp##akt_rap ON akt_rapp (dato, butik, kasse, tid)
go
CREATE INDEX akt_rapp##akt_rap3 ON akt_rapp (butik, kasse, tid, mnd, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'alarmlog' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table alarmlog
go
CREATE TABLE alarmlog (
  alarmlogid decimal(15,2) null,
  alarmlogbeskrivelse varchar (40) null,
  alarmdato datetime null,
  alarmtid integer null,
  alarmbrukerid varchar (20) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  alarmok tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_alarmlog ON alarmlog for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from alarmlog t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX alarmlog#_#progress_recid ON alarmlog (PROGRESS_RECID)
go
CREATE UNIQUE INDEX alarmlog#_#progress_recid_ident_ ON alarmlog (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX alarmlog##alarmdatotid ON alarmlog (alarmdato, alarmtid, PROGRESS_RECID)
go
CREATE INDEX alarmlog##alarmlogbeskrivelse ON alarmlog (alarmlogbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX alarmlog##alarmlogid ON alarmlog (alarmlogid)
go
CREATE INDEX alarmlog##alarmok ON alarmlog (alarmok, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'alarmtype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table alarmtype
go
CREATE TABLE alarmtype (
  alarmtypeid integer null,
  atypebeksr varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_alarmtype ON alarmtype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from alarmtype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX alarmtype#_#progress_recid ON alarmtype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX alarmtype#_#progress_recid_ident_ ON alarmtype (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX alarmtype##alarmtype ON alarmtype (alarmtypeid)
go
CREATE INDEX alarmtype##beskrivelse ON alarmtype (atypebeksr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'alfalandkode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table alfalandkode
go
CREATE TABLE alfalandkode (
  alfakode2 varchar (2) null,
  alfakode3 varchar (3) null,
  numlandkode integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_alfalandkode ON alfalandkode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from alfalandkode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX alfalandkode#_#progress_recid ON alfalandkode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX alfalandkode#_#progress_recid_ident_ ON alfalandkode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX alfalandkode##alfakode2 ON alfalandkode (alfakode2, PROGRESS_RECID)
go
CREATE UNIQUE INDEX alfalandkode##alfalandkode ON alfalandkode (alfakode3)
go
CREATE INDEX alfalandkode##numlandkode ON alfalandkode (numlandkode, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'altlevbas' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table altlevbas
go
CREATE TABLE altlevbas (
  artikkelnr decimal(13,0) null,
  levnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_altlevbas ON altlevbas for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from altlevbas t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX altlevbas#_#progress_recid ON altlevbas (PROGRESS_RECID)
go
CREATE UNIQUE INDEX altlevbas#_#progress_recid_ident_ ON altlevbas (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX altlevbas##artbas ON altlevbas (artikkelnr, levnr)
go
CREATE INDEX altlevbas##levbas ON altlevbas (levnr, artikkelnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'anv_kod' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table anv_kod
go
CREATE TABLE anv_kod (
  anv_id integer null,
  anvbeskr varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_anv_kod ON anv_kod for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from anv_kod t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX anv_kod#_#progress_recid ON anv_kod (PROGRESS_RECID)
go
CREATE UNIQUE INDEX anv_kod#_#progress_recid_ident_ ON anv_kod (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX anv_kod##anv_beskr ON anv_kod (anvbeskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX anv_kod##anv_kod ON anv_kod (anv_id)
go
if (select name from sysobjects 
    where name = 'apnskjema' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table apnskjema
go
CREATE TABLE apnskjema (
  butikknr integer null,
  ar integer null,
  ukelengde integer null,
  beskrivelse varchar (30) null,
  openclosed varchar (40) null,
  notat varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_apnskjema ON apnskjema for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from apnskjema t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX apnskjema#_#progress_recid ON apnskjema (PROGRESS_RECID)
go
CREATE UNIQUE INDEX apnskjema#_#progress_recid_ident_ ON apnskjema (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX apnskjema##apnskjema ON apnskjema (butikknr, ar)
go
CREATE INDEX apnskjema##ar ON apnskjema (ar, butikknr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'artbas' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table artbas
go
CREATE TABLE artbas (
  hg integer null,
  vg integer null,
  lopnr integer null,
  sasong integer null,
  farg integer null,
  klack integer null,
  matkod integer null,
  bildnr integer null,
  beskr varchar (20) null,
  levnr integer null,
  levkod varchar (20) null,
  tilv_land varchar (20) null,
  kommentar varchar (64) null,
  ov_id integer null,
  last_id integer null,
  foder_id integer null,
  inner_id integer null,
  slit_id integer null,
  anv_id integer null,
  rabkod integer null,
  provkod integer null,
  valkod varchar (3) null,
  ny_dato datetime null,
  inn_dato datetime null,
  lager tinyint null,
  vmid integer null,
  levfargkod varchar (15) null,
  notat varchar (40) null,
  bongtekst varchar (30) null,
  anonseartikkel tinyint null,
  vgkat integer null,
  strtypeid integer null,
  prodnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  artikkelnr decimal(13,0) null,
  aktivdato datetime null,
  aktivav varchar (15) null,
  aktivert tinyint null,
  storrelser tinyint null,
  laptop tinyint null,
  levdato1 datetime null,
  levdato2 datetime null,
  divinfo##1 varchar (30) null,
  divinfo##2 varchar (30) null,
  divinfo##3 varchar (30) null,
  divinfo##4 varchar (30) null,
  divinfo##5 varchar (30) null,
  divinfo##6 varchar (30) null,
  divinfo##7 varchar (30) null,
  divinfo##8 varchar (30) null,
  divinfo##9 varchar (30) null,
  divinfo##10 varchar (30) null,
  divinfo##11 varchar (30) null,
  divinfo##12 varchar (30) null,
  divinfo##13 varchar (30) null,
  divinfo##14 varchar (30) null,
  divinfo##15 varchar (30) null,
  divinfo##16 varchar (30) null,
  divinfo##17 varchar (30) null,
  divinfo##18 varchar (30) null,
  divinfo##19 varchar (30) null,
  divinfo##20 varchar (30) null,
  visdivinfo##1 tinyint null,
  visdivinfo##2 tinyint null,
  visdivinfo##3 tinyint null,
  visdivinfo##4 tinyint null,
  visdivinfo##5 tinyint null,
  visdivinfo##6 tinyint null,
  visdivinfo##7 tinyint null,
  visdivinfo##8 tinyint null,
  visdivinfo##9 tinyint null,
  visdivinfo##10 tinyint null,
  visdivinfo##11 tinyint null,
  visdivinfo##12 tinyint null,
  visdivinfo##13 tinyint null,
  visdivinfo##14 tinyint null,
  visdivinfo##15 tinyint null,
  visdivinfo##16 tinyint null,
  visdivinfo##17 tinyint null,
  visdivinfo##18 tinyint null,
  visdivinfo##19 tinyint null,
  visdivinfo##20 tinyint null,
  sattpakampanje datetime null,
  opris tinyint null,
  ollager tinyint null,
  bildeikasse tinyint null,
  pakke tinyint null,
  alder integer null,
  hkstyrt tinyint null,
  lokpris tinyint null,
  ikasse tinyint null,
  hkvareid integer null,
  kjentpahk tinyint null,
  behkode integer null,
  pakkenr integer null,
  anbefaltpris decimal(8,2) null,
  kunderabatt tinyint null,
  etikett integer null,
  salgsenhet varchar (10) null,
  slasket tinyint null,
  slaskartikkelnr decimal(15,2) null,
  modellfarge decimal(15,2) not null,
  sentralbestilling tinyint null,
  prisgrpnr integer not null,
  hkartikkelnr decimal(13,0) null,
  hovedmodellfarge tinyint null,
  dato1gsendthk datetime null,
  etikettekst1 varchar (30) null,
  etilayout integer null,
  linkvarenr decimal(13,0) null,
  mengde decimal(6,3) null,
  manrabikas tinyint null,
  artslag integer null,
  individtype integer null,
  pant tinyint null,
  bestforslag tinyint null,
  garantikl integer not null,
  levdato3 datetime null,
  levdato4 datetime null,
  linjemerknad varchar (40) null,
  katalogpris decimal(8,2) null,
  forhrab_ decimal(5,2) null,
  suprab_ decimal(5,2) null,
  vpidato datetime null,
  kjedevare tinyint null,
  vpibildekode varchar (30) null,
  strkode1 integer null,
  strkode2 integer null,
  levvaretekst varchar (30) null,
  antipakn integer null,
  fritttillegg tinyint null,
  varefakta varchar (40) null,
  lokasjon varchar (20) null,
  konvfaktetikett decimal(5,2) null,
  gjennomfaktureres tinyint null,
  kjederab_ decimal(4,2) null,
  kjedeinnkpris decimal(7,2) null,
  depositum decimal(7,2) null,
  medlemsutbytte tinyint null,
  utvidetsok varchar (100) null,
  hoylavmva tinyint null,
  etikettekst2 varchar (30) null,
  webbutikkartikkel tinyint null,
  ravdnr integer null,
  sanertdato datetime null,
  anbrekk tinyint null,
  inkranbrekk integer null,
  kjedevalutapris varchar (15) null,
  kjedeprodusent varchar (30) null,
  manueltopprettet tinyint null,
  postvekt decimal(8,3) null,
  postlengde integer null,
  posthoyde integer null,
  postbredde integer null,
  webminlager decimal(8,2) null,
  etikettanthylleplasser integer null,
  kampanjekode varchar (20) null,
  webleveringstid integer null,
  varetype integer null,
  leveringstid integer null,
  salgsenhetstype integer null,
  jamforenhet varchar (4) null,
  tilgjengeligfralev datetime null,
  levdatostopp1 datetime null,
  levdatostopp2 datetime null,
  levdatostopp3 datetime null,
  levdatostopp4 datetime null,
  utgatt tinyint null,
  utgattdato datetime null,
  avdelingnr integer null,
  avdelingnavn varchar (30) null,
  behstatus integer null,
  ekststrtypenavn varchar (30) null,
  kjedesuprab_ decimal(4,2) null,
  kjedesupinnkpris decimal(7,2) null,
  non_sale tinyint null,
  sortimentkoder varchar (40) null,
  kampanjeuker varchar (30) null,
  kampanjestotte varchar (30) null,
  lagerkoder varchar (30) null,
  negvare tinyint null,
  vekt tinyint null,
  grunnsortiment tinyint null,
  bonus_givende tinyint null,
  publiserinettbutikk tinyint null,
  link_til_nettside varchar (40) null,
  telefonkort tinyint null,
  mengderabatt tinyint null,
  hovedkatnr integer null,
  kjokkenskriver integer null,
  linkvareant integer null,
  salgsstopp integer null,
  alfakode2 varchar (4) null,
  onlinelevnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_artbas ON artbas for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from artbas t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX artbas#_#progress_recid ON artbas (PROGRESS_RECID)
go
CREATE UNIQUE INDEX artbas#_#progress_recid_ident_ ON artbas (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX artbas##aktiv ON artbas (aktivert, PROGRESS_RECID)
go
CREATE INDEX artbas##aktivert ON artbas (aktivert, vg, lopnr, PROGRESS_RECID)
go
CREATE INDEX artbas##alder ON artbas (alder, PROGRESS_RECID)
go
CREATE INDEX artbas##alfakode2 ON artbas (alfakode2, PROGRESS_RECID)
go
CREATE INDEX artbas##anbefaltpris ON artbas (anbefaltpris, PROGRESS_RECID)
go
CREATE INDEX artbas##annonse ON artbas (anonseartikkel, PROGRESS_RECID)
go
CREATE INDEX artbas##artbeskr ON artbas (beskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX artbas##artikkelnr ON artbas (artikkelnr)
go
CREATE UNIQUE INDEX artbas##artin ON artbas (vg, lopnr)
go
CREATE INDEX artbas##artslag ON artbas (artslag, PROGRESS_RECID)
go
CREATE INDEX artbas##behkode ON artbas (behkode, PROGRESS_RECID)
go
CREATE INDEX artbas##behstatus ON artbas (behstatus, PROGRESS_RECID)
go
CREATE INDEX artbas##bestforslag ON artbas (bestforslag, PROGRESS_RECID)
go
CREATE INDEX artbas##bildeikasse ON artbas (bildeikasse, PROGRESS_RECID)
go
CREATE INDEX artbas##bilder ON artbas (bildnr, vg, lopnr, PROGRESS_RECID)
go
CREATE INDEX artbas##bongtekst ON artbas (bongtekst, PROGRESS_RECID)
go
CREATE INDEX artbas##bonus_givende ON artbas (bonus_givende, PROGRESS_RECID)
go
CREATE INDEX artbas##depositum ON artbas (depositum, PROGRESS_RECID)
go
CREATE INDEX artbas##endretdato ON artbas (edato, PROGRESS_RECID)
go
CREATE INDEX artbas##etikett ON artbas (etikett, PROGRESS_RECID)
go
CREATE INDEX artbas##farge ON artbas (farg, PROGRESS_RECID)
go
CREATE INDEX artbas##fritttillegg ON artbas (fritttillegg, PROGRESS_RECID)
go
CREATE INDEX artbas##garantikl ON artbas (garantikl, PROGRESS_RECID)
go
CREATE INDEX artbas##gjennomgaktureres ON artbas (gjennomfaktureres, PROGRESS_RECID)
go
CREATE INDEX artbas##grunnsortiment ON artbas (grunnsortiment, PROGRESS_RECID)
go
CREATE INDEX artbas##hkartikkelnr ON artbas (hkartikkelnr, PROGRESS_RECID)
go
CREATE INDEX artbas##hkstyrt ON artbas (hkstyrt, PROGRESS_RECID)
go
CREATE INDEX artbas##hkvareid ON artbas (hkvareid, PROGRESS_RECID)
go
CREATE INDEX artbas##hovedkategori ON artbas (hovedkatnr, PROGRESS_RECID)
go
CREATE INDEX artbas##hovedmodellfarge ON artbas (hovedmodellfarge, PROGRESS_RECID)
go
CREATE INDEX artbas##hoylavmva ON artbas (hoylavmva, PROGRESS_RECID)
go
CREATE INDEX artbas##huvgr ON artbas (hg, PROGRESS_RECID)
go
CREATE INDEX artbas##ikasse ON artbas (ikasse, PROGRESS_RECID)
go
CREATE INDEX artbas##individtype ON artbas (individtype, PROGRESS_RECID)
go
CREATE INDEX artbas##kampanjekode ON artbas (kampanjekode, PROGRESS_RECID)
go
CREATE INDEX artbas##kjedevare ON artbas (kjedevare, PROGRESS_RECID)
go
CREATE INDEX artbas##kjentpahk ON artbas (kjentpahk, PROGRESS_RECID)
go
CREATE INDEX artbas##kjokkenskriver ON artbas (kjokkenskriver, PROGRESS_RECID)
go
CREATE INDEX artbas##klack ON artbas (klack, PROGRESS_RECID)
go
CREATE INDEX artbas##kunderabatt ON artbas (kunderabatt, PROGRESS_RECID)
go
CREATE INDEX artbas##lager ON artbas (lager, PROGRESS_RECID)
go
CREATE INDEX artbas##laptop ON artbas (laptop, artikkelnr, PROGRESS_RECID)
go
CREATE INDEX artbas##levdato1 ON artbas (levdato1, PROGRESS_RECID)
go
CREATE INDEX artbas##levdato2 ON artbas (levdato2, PROGRESS_RECID)
go
CREATE INDEX artbas##leverandor ON artbas (levnr, PROGRESS_RECID)
go
CREATE INDEX artbas##levfargkod ON artbas (levfargkod, PROGRESS_RECID)
go
CREATE INDEX artbas##levkod ON artbas (levkod, vg, lopnr, PROGRESS_RECID)
go
CREATE INDEX artbas##levkod2 ON artbas (levnr, levkod, PROGRESS_RECID)
go
CREATE INDEX artbas##levnrbeskr ON artbas (levnr, beskr, PROGRESS_RECID)
go
CREATE INDEX artbas##levnrlevkod ON artbas (levnr, levkod, vg, beskr, PROGRESS_RECID)
go
CREATE INDEX artbas##levnrvglopnr ON artbas (levnr, vg, lopnr, PROGRESS_RECID)
go
CREATE INDEX artbas##levvaretekst ON artbas (levvaretekst, PROGRESS_RECID)
go
CREATE INDEX artbas##linjemerknad ON artbas (linjemerknad, PROGRESS_RECID)
go
CREATE INDEX artbas##linkvarenr ON artbas (linkvarenr, PROGRESS_RECID)
go
CREATE INDEX artbas##lokalpris ON artbas (lokpris, PROGRESS_RECID)
go
CREATE INDEX artbas##lokasjon ON artbas (lokasjon, PROGRESS_RECID)
go
CREATE INDEX artbas##lokpris ON artbas (lokpris, PROGRESS_RECID)
go
CREATE INDEX artbas##manrabikas ON artbas (manrabikas, PROGRESS_RECID)
go
CREATE INDEX artbas##manueltopprettet ON artbas (manueltopprettet, PROGRESS_RECID)
go
CREATE INDEX artbas##material ON artbas (matkod, PROGRESS_RECID)
go
CREATE INDEX artbas##medlemsutbytte ON artbas (medlemsutbytte, PROGRESS_RECID)
go
CREATE INDEX artbas##modellfarge ON artbas (modellfarge, PROGRESS_RECID)
go
CREATE INDEX artbas##negvare ON artbas (negvare, PROGRESS_RECID)
go
CREATE INDEX artbas##non_sale ON artbas (non_sale, PROGRESS_RECID)
go
CREATE INDEX artbas##onlinelevnr ON artbas (onlinelevnr, PROGRESS_RECID)
go
CREATE INDEX artbas##opris ON artbas (opris, PROGRESS_RECID)
go
CREATE INDEX artbas##pakke ON artbas (pakke, PROGRESS_RECID)
go
CREATE INDEX artbas##pakkenr ON artbas (pakkenr, PROGRESS_RECID)
go
CREATE INDEX artbas##pant ON artbas (pant, PROGRESS_RECID)
go
CREATE INDEX artbas##plu ON artbas (vg, opris, PROGRESS_RECID)
go
CREATE INDEX artbas##prisgruppe ON artbas (prisgrpnr, PROGRESS_RECID)
go
CREATE INDEX artbas##produsent ON artbas (prodnr, PROGRESS_RECID)
go
CREATE INDEX artbas##ravdnr ON artbas (ravdnr, PROGRESS_RECID)
go
CREATE INDEX artbas##registrertdato ON artbas (registrertdato, PROGRESS_RECID)
go
CREATE INDEX artbas##salgsenhetstype ON artbas (salgsenhetstype, PROGRESS_RECID)
go
CREATE INDEX artbas##salgsstopp ON artbas (salgsstopp, PROGRESS_RECID)
go
CREATE INDEX artbas##sanertdato ON artbas (sanertdato, PROGRESS_RECID)
go
CREATE INDEX artbas##sasong ON artbas (sasong, PROGRESS_RECID)
go
CREATE INDEX artbas##sentralbestilling ON artbas (sentralbestilling, PROGRESS_RECID)
go
CREATE INDEX artbas##setartikkelnrfravpi ON artbas (levkod, beskr, levfargkod, PROGRESS_RECID)
go
CREATE INDEX artbas##slaskartikkelnr ON artbas (slaskartikkelnr, PROGRESS_RECID)
go
CREATE INDEX artbas##slasket ON artbas (slasket, PROGRESS_RECID)
go
CREATE INDEX artbas##storrelser ON artbas (storrelser, PROGRESS_RECID)
go
CREATE INDEX artbas##strtype ON artbas (strtypeid, PROGRESS_RECID)
go
CREATE INDEX artbas##telefonkort ON artbas (telefonkort, PROGRESS_RECID)
go
CREATE INDEX artbas##utgatt ON artbas (utgatt, PROGRESS_RECID)
go
CREATE INDEX artbas##valuta ON artbas (valkod, PROGRESS_RECID)
go
CREATE INDEX artbas##varegruppe ON artbas (vg, PROGRESS_RECID)
go
CREATE INDEX artbas##varemerke ON artbas (vmid, PROGRESS_RECID)
go
CREATE INDEX artbas##varetype ON artbas (varetype, PROGRESS_RECID)
go
CREATE INDEX artbas##vekt ON artbas (vekt, PROGRESS_RECID)
go
CREATE INDEX artbas##vgbeskr ON artbas (vg, beskr, levnr, levkod, PROGRESS_RECID)
go
CREATE INDEX artbas##vgkat ON artbas (vg, vgkat, PROGRESS_RECID)
go
CREATE INDEX artbas##vglevkod ON artbas (vg, levnr, levkod, beskr, PROGRESS_RECID)
go
CREATE INDEX artbas##vglopnr ON artbas (vg, lopnr, beskr, levnr, PROGRESS_RECID)
go
CREATE INDEX artbas##vpibildekode ON artbas (vpibildekode, PROGRESS_RECID)
go
CREATE INDEX artbas##vpidato ON artbas (vpidato, PROGRESS_RECID)
go
CREATE INDEX artbas##webbutikk ON artbas (webbutikkartikkel, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'artbaskarakteristikk' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table artbaskarakteristikk
go
CREATE TABLE artbaskarakteristikk (
  artikkelnr decimal(15,2) null,
  karakteristikkid varchar (4) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_artbaskarakteristikk ON artbaskarakteristikk for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from artbaskarakteristikk t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX artbaskarakteristikk#_#progress_recid ON artbaskarakteristikk (PROGRESS_RECID)
go
CREATE UNIQUE INDEX artbaskarakteristikk#_#progress_recid_ident_ ON artbaskarakteristikk (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX artbaskarakteristikk##artikkel ON artbaskarakteristikk (artikkelnr, karakteristikkid)
go
CREATE INDEX artbaskarakteristikk##karakteristikkid ON artbaskarakteristikk (karakteristikkid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'artbasunderkategori' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table artbasunderkategori
go
CREATE TABLE artbasunderkategori (
  artikkelnr decimal(13,0) null,
  underkatnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_artbasunderkategori ON artbasunderkategori for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from artbasunderkategori t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX artbasunderkategori#_#progress_recid ON artbasunderkategori (PROGRESS_RECID)
go
CREATE UNIQUE INDEX artbasunderkategori#_#progress_recid_ident_ ON artbasunderkategori (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX artbasunderkategori##artikkelunderkategori ON artbasunderkategori (artikkelnr, underkatnr)
go
CREATE INDEX artbasunderkategori##underkategoriartikkel ON artbasunderkategori (underkatnr, artikkelnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'artbestpkt' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table artbestpkt
go
CREATE TABLE artbestpkt (
  artikkelnr decimal(13,0) null,
  butikknr integer not null,
  strkode integer null,
  maksant decimal(12,4) null,
  minant decimal(12,4) null,
  bestant decimal(12,4) null,
  tillatbruttpk tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_artbestpkt ON artbestpkt for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from artbestpkt t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX artbestpkt#_#progress_recid ON artbestpkt (PROGRESS_RECID)
go
CREATE UNIQUE INDEX artbestpkt#_#progress_recid_ident_ ON artbestpkt (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX artbestpkt##artbestpkt ON artbestpkt (artikkelnr, butikknr, strkode)
go
CREATE INDEX artbestpkt##strkode ON artbestpkt (artikkelnr, strkode, butikknr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'artbut' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table artbut
go
CREATE TABLE artbut (
  artikkelnr decimal(13,0) null,
  butik integer null,
  deleted tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_artbut ON artbut for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from artbut t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX artbut#_#progress_recid ON artbut (PROGRESS_RECID)
go
CREATE UNIQUE INDEX artbut#_#progress_recid_ident_ ON artbut (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX artbut##artbut ON artbut (artikkelnr, butik)
go
CREATE INDEX artbut##butart ON artbut (butik, artikkelnr, PROGRESS_RECID)
go
CREATE INDEX artbut##deleted ON artbut (butik, deleted, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'artikkelnrserie' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table artikkelnrserie
go
CREATE TABLE artikkelnrserie (
  artikkelnr decimal(13,0) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_artikkelnrserie ON artikkelnrserie for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from artikkelnrserie t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX artikkelnrserie#_#progress_recid ON artikkelnrserie (PROGRESS_RECID)
go
CREATE UNIQUE INDEX artikkelnrserie#_#progress_recid_ident_ ON artikkelnrserie (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX artikkelnrserie##idxartikkelnr ON artikkelnrserie (artikkelnr)
go
if (select name from sysobjects 
    where name = 'artlag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table artlag
go
CREATE TABLE artlag (
  vg integer null,
  lopnr integer null,
  storl varchar (10) null,
  butik integer null,
  lagant decimal(10,4) null,
  retant decimal(10,4) null,
  lager tinyint null,
  artikkelnr decimal(13,0) null,
  antsolgt decimal(12,4) null,
  brekkant decimal(9,4) null,
  intant decimal(9,4) null,
  reklant decimal(9,4) null,
  rekllant decimal(9,4) null,
  gjenkjopant decimal(9,4) null,
  retlant decimal(9,4) null,
  kjopant decimal(9,4) null,
  ovant decimal(9,4) null,
  justant decimal(9,4) null,
  justverdi decimal(10,2) null,
  svinnant decimal(9,4) null,
  svinnverdi decimal(10,2) null,
  nedant decimal(9,4) null,
  nedverdi decimal(10,2) null,
  antrab decimal(10,2) null,
  strkode integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_artlag ON artlag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from artlag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX artlag#_#progress_recid ON artlag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX artlag#_#progress_recid_ident_ ON artlag (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX artlag##artbut ON artlag (butik, vg, lopnr, storl)
go
CREATE INDEX artlag##artlag ON artlag (artikkelnr, storl, butik, PROGRESS_RECID)
go
CREATE UNIQUE INDEX artlag##artlagin ON artlag (vg, lopnr, storl, butik)
go
CREATE UNIQUE INDEX artlag##strkonv ON artlag (artikkelnr, butik, strkode)
go
if (select name from sysobjects 
    where name = 'artlok' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table artlok
go
CREATE TABLE artlok (
  artikkelnr decimal(13,0) null,
  butikknr integer null,
  lokasjonsnr varchar (4) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_artlok ON artlok for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from artlok t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX artlok#_#progress_recid ON artlok (PROGRESS_RECID)
go
CREATE UNIQUE INDEX artlok#_#progress_recid_ident_ ON artlok (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX artlok##artlokasjon ON artlok (artikkelnr, butikknr, lokasjonsnr)
go
if (select name from sysobjects 
    where name = 'artpris' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table artpris
go
CREATE TABLE artpris (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  profilnr integer null,
  artikkelnr decimal(13,0) null,
  tilbud tinyint null,
  varekost##1 decimal(7,2) null,
  varekost##2 decimal(7,2) null,
  mvakr##1 decimal(7,2) null,
  mvakr##2 decimal(7,2) null,
  levnr integer null,
  euromanuel tinyint null,
  valpris##1 decimal(11,2) null,
  valpris##2 decimal(11,2) null,
  rab1kr##1 decimal(7,2) null,
  rab1kr##2 decimal(7,2) null,
  rab1_##1 decimal(5,2) null,
  rab1_##2 decimal(5,2) null,
  rab2kr##1 decimal(7,2) null,
  rab2kr##2 decimal(7,2) null,
  rab2_##1 decimal(7,2) null,
  rab2_##2 decimal(7,2) null,
  frakt##1 decimal(7,2) null,
  frakt##2 decimal(7,2) null,
  frakt_##1 decimal(7,2) null,
  frakt_##2 decimal(7,2) null,
  divkostkr##1 decimal(7,2) null,
  divkostkr##2 decimal(7,2) null,
  divkost_##1 decimal(7,2) null,
  divkost_##2 decimal(7,2) null,
  rab3kr##1 decimal(7,2) null,
  rab3kr##2 decimal(7,2) null,
  rab3_##1 decimal(7,2) null,
  rab3_##2 decimal(7,2) null,
  dbkr##1 decimal(7,2) null,
  dbkr##2 decimal(7,2) null,
  db_##1 decimal(7,2) null,
  db_##2 decimal(7,2) null,
  europris##1 decimal(9,2) null,
  europris##2 decimal(9,2) null,
  innkjopspris##1 decimal(7,2) null,
  innkjopspris##2 decimal(7,2) null,
  mva_##1 decimal(7,2) null,
  mva_##2 decimal(7,2) null,
  pris##1 decimal(9,2) null,
  pris##2 decimal(9,2) null,
  aktivfradato datetime null,
  aktivfratid integer null,
  tilbudfradato datetime null,
  tilbudtildato datetime null,
  tilbudfratid integer null,
  tilbudtiltid integer null,
  tilbudtimestyrt tinyint null,
  momskod##1 integer null,
  momskod##2 integer null,
  mengderabantall integer null,
  mengderabpris decimal(9,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_artpris ON artpris for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from artpris t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX artpris#_#progress_recid ON artpris (PROGRESS_RECID)
go
CREATE UNIQUE INDEX artpris#_#progress_recid_ident_ ON artpris (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX artpris##artikkelnr ON artpris (artikkelnr, profilnr)
go
CREATE INDEX artpris##profil ON artpris (profilnr, artikkelnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'artsort' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table artsort
go
CREATE TABLE artsort (
  artikkelnr decimal(13,0) null,
  sortid varchar (12) null,
  levnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_artsort ON artsort for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from artsort t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX artsort#_#progress_recid ON artsort (PROGRESS_RECID)
go
CREATE UNIQUE INDEX artsort#_#progress_recid_ident_ ON artsort (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX artsort##idxartsort ON artsort (artikkelnr, sortid, levnr)
go
CREATE INDEX artsort##idxsortid ON artsort (sortid, levnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'avdeling' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table avdeling
go
CREATE TABLE avdeling (
  avdelingnr integer null,
  avdelingnavn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_avdeling ON avdeling for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from avdeling t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX avdeling#_#progress_recid ON avdeling (PROGRESS_RECID)
go
CREATE UNIQUE INDEX avdeling#_#progress_recid_ident_ ON avdeling (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX avdeling##avdeling ON avdeling (avdelingnr)
go
CREATE INDEX avdeling##avdelingsnavn ON avdeling (avdelingnavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'batchlogg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table batchlogg
go
CREATE TABLE batchlogg (
  batchnr integer null,
  bestnr integer null,
  registrertdato datetime null,
  registrerttid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertav varchar (10) null,
  merknad varchar (30) null,
  beskrivelse varchar (50) null,
  opphav varchar (20) null,
  oppdstatus integer null,
  statusoppdatert datetime null,
  tidoppdatert integer null,
  oppdatertav varchar (12) null,
  tekst1 varchar (20) null,
  tekst2 varchar (20) null,
  tekst3 varchar (20) null,
  tekst4 varchar (20) null,
  pksdlnr varchar (15) null,
  ekstid varchar (15) null,
  ordrenr integer null,
  pksdlid decimal(13,0) not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_batchlogg ON batchlogg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from batchlogg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX batchlogg#_#progress_recid ON batchlogg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX batchlogg#_#progress_recid_ident_ ON batchlogg (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX batchlogg##batchlogg ON batchlogg (batchnr)
go
CREATE INDEX batchlogg##batchstatus ON batchlogg (oppdstatus, batchnr, PROGRESS_RECID)
go
CREATE INDEX batchlogg##beskrivelse ON batchlogg (beskrivelse, PROGRESS_RECID)
go
CREATE INDEX batchlogg##datobatch ON batchlogg (registrertdato, batchnr, PROGRESS_RECID)
go
CREATE INDEX batchlogg##datobeskrivelse ON batchlogg (registrertdato, beskrivelse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'behandlingskode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table behandlingskode
go
CREATE TABLE behandlingskode (
  behkode integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  beskrivelse varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_behandlingskode ON behandlingskode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from behandlingskode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX behandlingskode#_#progress_recid ON behandlingskode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX behandlingskode#_#progress_recid_ident_ ON behandlingskode (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX behandlingskode##behandlingskode ON behandlingskode (behkode)
go
CREATE INDEX behandlingskode##beskrivelse ON behandlingskode (beskrivelse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'beliggenhet' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table beliggenhet
go
CREATE TABLE beliggenhet (
  beliggenhetid integer null,
  beliggenhetnavn varchar (30) null,
  beliggenhetnotat varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_beliggenhet ON beliggenhet for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from beliggenhet t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX beliggenhet#_#progress_recid ON beliggenhet (PROGRESS_RECID)
go
CREATE UNIQUE INDEX beliggenhet#_#progress_recid_ident_ ON beliggenhet (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX beliggenhet##beliggenhet ON beliggenhet (beliggenhetid)
go
CREATE INDEX beliggenhet##beliggenhetnavn ON beliggenhet (beliggenhetnavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'besthlev' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table besthlev
go
CREATE TABLE besthlev (
  bestnr integer null,
  leveringsnr integer null,
  levertdato datetime null,
  levtidspunkt integer null,
  levertav varchar (15) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_besthlev ON besthlev for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from besthlev t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX besthlev#_#progress_recid ON besthlev (PROGRESS_RECID)
go
CREATE UNIQUE INDEX besthlev#_#progress_recid_ident_ ON besthlev (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX besthlev##bestnrleveringsnr ON besthlev (bestnr, leveringsnr)
go
if (select name from sysobjects 
    where name = 'besthode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table besthode
go
CREATE TABLE besthode (
  bestnr integer null,
  levnr integer not null,
  bestillingsdato datetime null,
  beststat integer null,
  registrertdato datetime null,
  registrerttid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertav varchar (10) null,
  merknad varchar (30) null,
  beskrivelse varchar (50) null,
  direktelev tinyint null,
  levtid varchar (6) null,
  strtypeid integer null,
  anonseartikkel tinyint null,
  artikkelnr decimal(13,0) null,
  levkod varchar (30) not null,
  levfargkod varchar (15) null,
  ordrenr integer null,
  besttype integer not null,
  totantpar decimal(7,2) null,
  totinnkjverdi decimal(9,2) null,
  totsalgsverdi decimal(9,2) null,
  totdbkr decimal(9,2) null,
  ekstid varchar (15) null,
  laptop tinyint null,
  levdato datetime null,
  sendtdato datetime null,
  sendttid integer null,
  sendtav varchar (10) not null,
  totinnlev decimal(7,2) null,
  totoverlev decimal(7,2) null,
  totmakulert decimal(7,2) null,
  cl integer null,
  teamnr integer null,
  kjedeavtale tinyint null,
  ekstordrenr integer null,
  hkordre tinyint null,
  varebehnr decimal(15,2) null,
  bekreftetordre tinyint null,
  bekreftetdato datetime null,
  bekreftetav varchar (15) null,
  fraerp tinyint null,
  sendtbutikkflagg tinyint null,
  sendtbutikkdato datetime null,
  sendtbutikktid integer null,
  kordre_id decimal(15,2) not null,
  ulevnr integer null,
  opphav varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_besthode ON besthode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from besthode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX besthode#_#progress_recid ON besthode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX besthode#_#progress_recid_ident_ ON besthode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX besthode##artikkelnr ON besthode (artikkelnr, PROGRESS_RECID)
go
CREATE INDEX besthode##bekreftet ON besthode (bekreftetordre, PROGRESS_RECID)
go
CREATE INDEX besthode##beskrivelse ON besthode (beskrivelse, bestnr, PROGRESS_RECID)
go
CREATE INDEX besthode##bestdato ON besthode (bestillingsdato, bestnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX besthode##bestnr ON besthode (bestnr)
go
CREATE INDEX besthode##ekstid ON besthode (ekstid, PROGRESS_RECID)
go
CREATE INDEX besthode##ekstordrenr ON besthode (ekstordrenr, bestnr, PROGRESS_RECID)
go
CREATE INDEX besthode##fraerp ON besthode (fraerp, PROGRESS_RECID)
go
CREATE INDEX besthode##hkordre ON besthode (hkordre, PROGRESS_RECID)
go
CREATE INDEX besthode##kolleksjon ON besthode (beststat, bestillingsdato, levtid, PROGRESS_RECID)
go
CREATE INDEX besthode##kordre_id ON besthode (kordre_id, PROGRESS_RECID)
go
CREATE INDEX besthode##laptop ON besthode (laptop, bestnr, PROGRESS_RECID)
go
CREATE INDEX besthode##levartikkelnr ON besthode (levnr, artikkelnr, PROGRESS_RECID)
go
CREATE INDEX besthode##levdato ON besthode (levdato, PROGRESS_RECID)
go
CREATE INDEX besthode##levekstid ON besthode (levnr, ekstid, PROGRESS_RECID)
go
CREATE INDEX besthode##levkod ON besthode (levkod, PROGRESS_RECID)
go
CREATE INDEX besthode##levnrbestdato ON besthode (levnr, bestillingsdato, PROGRESS_RECID)
go
CREATE INDEX besthode##levnrbestnr ON besthode (levnr, bestnr, PROGRESS_RECID)
go
CREATE INDEX besthode##levnrlevtid ON besthode (levnr, levtid, bestnr, PROGRESS_RECID)
go
CREATE INDEX besthode##opphav ON besthode (opphav, PROGRESS_RECID)
go
CREATE INDEX besthode##opprettet ON besthode (registrertdato, registrerttid, PROGRESS_RECID)
go
CREATE INDEX besthode##ordrenr ON besthode (ordrenr, bestnr, PROGRESS_RECID)
go
CREATE INDEX besthode##sendtbutikkdato ON besthode (sendtbutikkdato, PROGRESS_RECID)
go
CREATE INDEX besthode##sendtbutikkflagg ON besthode (sendtbutikkflagg, PROGRESS_RECID)
go
CREATE INDEX besthode##sentrallager ON besthode (cl, PROGRESS_RECID)
go
CREATE INDEX besthode##strtypeid ON besthode (strtypeid, PROGRESS_RECID)
go
CREATE INDEX besthode##teamnr ON besthode (teamnr, PROGRESS_RECID)
go
CREATE INDEX besthode##varebehnr ON besthode (varebehnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'bestkasse' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bestkasse
go
CREATE TABLE bestkasse (
  bestnr integer null,
  sortid varchar (12) null,
  butik integer null,
  antal integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bestkasse ON bestkasse for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bestkasse t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bestkasse#_#progress_recid ON bestkasse (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bestkasse#_#progress_recid_ident_ ON bestkasse (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bestkasse##bestkasse ON bestkasse (bestnr, sortid, butik)
go
if (select name from sysobjects 
    where name = 'bestlevert' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bestlevert
go
CREATE TABLE bestlevert (
  bestnr integer null,
  storl varchar (10) null,
  butik integer null,
  levert decimal(7,2) null,
  rest decimal(7,2) null,
  leveringsnr integer null,
  levertdato datetime null,
  levertav varchar (10) null,
  avskrevet tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bestlevert ON bestlevert for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bestlevert t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bestlevert#_#progress_recid ON bestlevert (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bestlevert#_#progress_recid_ident_ ON bestlevert (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bestlevert##bestlevert ON bestlevert (bestnr, butik, storl, leveringsnr)
go
CREATE INDEX bestlevert##levertdato ON bestlevert (levertdato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'bestlevindivid' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bestlevindivid
go
CREATE TABLE bestlevindivid (
  bestnr integer null,
  storl varchar (10) null,
  butik integer null,
  levertdato datetime null,
  levertav varchar (10) null,
  individnr decimal(12,0) not null,
  leveringsnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bestlevindivid ON bestlevindivid for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bestlevindivid t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bestlevindivid#_#progress_recid ON bestlevindivid (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bestlevindivid#_#progress_recid_ident_ ON bestlevindivid (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bestlevindivid##bestlevindivid ON bestlevindivid (bestnr, butik, storl, leveringsnr, individnr)
go
if (select name from sysobjects 
    where name = 'bestlinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bestlinje
go
CREATE TABLE bestlinje (
  bestnr integer null,
  butik integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bestlinje ON bestlinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bestlinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bestlinje#_#progress_recid ON bestlinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bestlinje#_#progress_recid_ident_ ON bestlinje (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bestlinje##bestlinje ON bestlinje (bestnr, butik)
go
if (select name from sysobjects 
    where name = 'bestpris' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bestpris
go
CREATE TABLE bestpris (
  bestnr integer null,
  beststat integer null,
  valpris decimal(11,2) null,
  innkjopspris decimal(7,2) null,
  rab1kr decimal(7,2) null,
  rab1_ decimal(5,2) null,
  rab2kr decimal(7,2) null,
  rab2_ decimal(5,2) null,
  frakt decimal(8,2) null,
  frakt_ decimal(5,2) null,
  divkostkr decimal(7,2) null,
  divkost_ decimal(5,2) null,
  rab3kr decimal(7,2) null,
  rab3_ decimal(5,2) null,
  dbkr decimal(7,2) null,
  db_ decimal(7,2) null,
  pris decimal(7,2) null,
  europris decimal(7,2) null,
  euromanuel tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  profilnr integer null,
  artikkelnr decimal(13,0) null,
  varekost decimal(9,2) null,
  mvakr decimal(9,2) null,
  mva_ decimal(5,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bestpris ON bestpris for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bestpris t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bestpris#_#progress_recid ON bestpris (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bestpris#_#progress_recid_ident_ ON bestpris (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bestpris##bestpris ON bestpris (bestnr, beststat, profilnr)
go
if (select name from sysobjects 
    where name = 'bestsort' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bestsort
go
CREATE TABLE bestsort (
  bestnr integer null,
  sortid varchar (12) null,
  fri tinyint null,
  antsort integer null,
  antall integer null,
  strinterval varchar (11) null,
  fordeling varchar (30) null,
  storrelser varchar (33) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bestsort ON bestsort for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bestsort t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bestsort#_#progress_recid ON bestsort (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bestsort#_#progress_recid_ident_ ON bestsort (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bestsort##bestsort ON bestsort (bestnr, sortid)
go
if (select name from sysobjects 
    where name = 'beststr' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table beststr
go
CREATE TABLE beststr (
  bestnr integer null,
  storl varchar (10) null,
  butik integer null,
  beststat integer null,
  bestilt decimal(7,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_beststr ON beststr for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from beststr t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX beststr#_#progress_recid ON beststr (PROGRESS_RECID)
go
CREATE UNIQUE INDEX beststr#_#progress_recid_ident_ ON beststr (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX beststr##beststr ON beststr (bestnr, butik, storl, beststat)
go
if (select name from sysobjects 
    where name = 'betalingsbetingelser' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table betalingsbetingelser
go
CREATE TABLE betalingsbetingelser (
  betbet integer null,
  bettekst varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  antkredittdager integer null,
  frilevmnd integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_betalingsbetingelser ON betalingsbetingelser for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from betalingsbetingelser t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX betalingsbetingelser#_#progress_recid ON betalingsbetingelser (PROGRESS_RECID)
go
CREATE UNIQUE INDEX betalingsbetingelser#_#progress_recid_ident_ ON betalingsbetingelser (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX betalingsbetingelser##betalingsbetingelse ON betalingsbetingelser (betbet)
go
CREATE INDEX betalingsbetingelser##tekst ON betalingsbetingelser (bettekst, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'bilagsart' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bilagsart
go
CREATE TABLE bilagsart (
  bartnr integer null,
  barttekst varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kontonr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bilagsart ON bilagsart for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bilagsart t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bilagsart#_#progress_recid ON bilagsart (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bilagsart#_#progress_recid_ident_ ON bilagsart (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX bilagsart##beskrivelse ON bilagsart (barttekst, PROGRESS_RECID)
go
CREATE UNIQUE INDEX bilagsart##bilagsart ON bilagsart (bartnr)
go
CREATE INDEX bilagsart##kontonr ON bilagsart (kontonr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'bilagstype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bilagstype
go
CREATE TABLE bilagstype (
  bilagstype integer null,
  bttekst varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  bruk integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bilagstype ON bilagstype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bilagstype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bilagstype#_#progress_recid ON bilagstype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bilagstype#_#progress_recid_ident_ ON bilagstype (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bilagstype##bilagstype ON bilagstype (bilagstype)
go
CREATE INDEX bilagstype##bruk ON bilagstype (bruk, PROGRESS_RECID)
go
CREATE INDEX bilagstype##bttekst ON bilagstype (bttekst, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'bildedata' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bildedata
go
CREATE TABLE bildedata (
  bildnr integer null,
  teller integer null,
  rawdata text null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bildedata ON bildedata for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bildedata t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bildedata#_#progress_recid ON bildedata (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bildedata#_#progress_recid_ident_ ON bildedata (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bildedata##bildedata ON bildedata (bildnr, teller)
go
if (select name from sysobjects 
    where name = 'bilderegister' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bilderegister
go
CREATE TABLE bilderegister (
  bildnr integer null,
  merknad varchar (40) null,
  tekst varchar (30) null,
  filnavn varchar (50) null,
  registrertdato datetime null,
  dato datetime null,
  notat varchar (30) null,
  levartnr varchar (20) null,
  levnr integer null,
  registrerttid integer null,
  tid integer null,
  sted varchar (20) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  eksterntid varchar (15) null,
  registrertav varchar (10) null,
  dokumentnr integer null,
  bytes integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bilderegister ON bilderegister for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bilderegister t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bilderegister#_#progress_recid ON bilderegister (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bilderegister#_#progress_recid_ident_ ON bilderegister (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bilderegister##bilde ON bilderegister (bildnr)
go
CREATE INDEX bilderegister##dato ON bilderegister (dato, bildnr, PROGRESS_RECID)
go
CREATE INDEX bilderegister##dokumentnr ON bilderegister (dokumentnr, PROGRESS_RECID)
go
CREATE INDEX bilderegister##leverandor ON bilderegister (levnr, bildnr, PROGRESS_RECID)
go
CREATE INDEX bilderegister##leverandor2 ON bilderegister (levnr, levartnr, PROGRESS_RECID)
go
CREATE INDEX bilderegister##merknad ON bilderegister (dato, merknad, PROGRESS_RECID)
go
CREATE INDEX bilderegister##rmerknad ON bilderegister (registrertdato, merknad, PROGRESS_RECID)
go
CREATE INDEX bilderegister##tekst ON bilderegister (tekst, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'bokforingsbilag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bokforingsbilag
go
CREATE TABLE bokforingsbilag (
  butikknr integer null,
  bokforingsnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  sendtdato datetime null,
  sendav varchar (15) null,
  sendttid integer null,
  omsetningsdato datetime null,
  sendtregnskap tinyint null,
  aar integer null,
  godkjentdato datetime null,
  godkjenttid integer null,
  godkjentav varchar (15) null,
  godkjentflagg tinyint null,
  eoddato datetime null,
  eodmottatt tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bokforingsbilag ON bokforingsbilag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bokforingsbilag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bokforingsbilag#_#progress_recid ON bokforingsbilag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bokforingsbilag#_#progress_recid_ident_ ON bokforingsbilag (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bokforingsbilag##bokforingsbilag ON bokforingsbilag (butikknr, aar, bokforingsnr)
go
CREATE INDEX bokforingsbilag##eodmottatt ON bokforingsbilag (eodmottatt, PROGRESS_RECID)
go
CREATE INDEX bokforingsbilag##godkjentdato ON bokforingsbilag (godkjentdato, PROGRESS_RECID)
go
CREATE INDEX bokforingsbilag##godkjentflagg ON bokforingsbilag (godkjentflagg, PROGRESS_RECID)
go
CREATE INDEX bokforingsbilag##omsatt ON bokforingsbilag (omsetningsdato, PROGRESS_RECID)
go
CREATE INDEX bokforingsbilag##sendt ON bokforingsbilag (sendtregnskap, PROGRESS_RECID)
go
CREATE INDEX bokforingsbilag##sendtdato ON bokforingsbilag (sendtdato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'bruker' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bruker
go
CREATE TABLE bruker (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) not null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  brgrpnr integer not null,
  navn varchar (40) null,
  ebrukerid varchar (12) null,
  lng varchar (3) null,
  grad integer not null,
  butikknr integer null,
  brukertype integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bruker ON bruker for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bruker t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bruker#_#progress_recid ON bruker (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bruker#_#progress_recid_ident_ ON bruker (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bruker##bruker ON bruker (brukerid)
go
CREATE INDEX bruker##brukertype ON bruker (brukertype, PROGRESS_RECID)
go
CREATE INDEX bruker##butikk ON bruker (butikknr, PROGRESS_RECID)
go
CREATE INDEX bruker##grad ON bruker (grad, PROGRESS_RECID)
go
CREATE INDEX bruker##lng ON bruker (lng, brukerid, PROGRESS_RECID)
go
CREATE INDEX bruker##navn ON bruker (navn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'brukergrp' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table brukergrp
go
CREATE TABLE brukergrp (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  brgrpnr integer not null,
  beskrivelse varchar (40) null,
  navn varchar (40) not null,
  notat varchar (40) null,
  knappeliste varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_brukergrp ON brukergrp for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from brukergrp t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX brukergrp#_#progress_recid ON brukergrp (PROGRESS_RECID)
go
CREATE UNIQUE INDEX brukergrp#_#progress_recid_ident_ ON brukergrp (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX brukergrp##beskrivelse ON brukergrp (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX brukergrp##brukergrp ON brukergrp (brgrpnr)
go
if (select name from sysobjects 
    where name = 'brukerlev' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table brukerlev
go
CREATE TABLE brukerlev (
  brukerid varchar (10) not null,
  levnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_brukerlev ON brukerlev for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from brukerlev t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX brukerlev#_#progress_recid ON brukerlev (PROGRESS_RECID)
go
CREATE UNIQUE INDEX brukerlev#_#progress_recid_ident_ ON brukerlev (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX brukerlev##brukerlev ON brukerlev (brukerid, levnr)
go
CREATE INDEX brukerlev##levbruker ON brukerlev (levnr, brukerid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'butiker' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table butiker
go
CREATE TABLE butiker (
  butik integer null,
  butnamn varchar (20) null,
  buadr varchar (20) null,
  buponr varchar (6) null,
  bupadr varchar (20) null,
  bukon varchar (20) null,
  butel varchar (20) null,
  levadresse1 varchar (40) null,
  levadresse2 varchar (40) null,
  levpostnr varchar (10) null,
  levpostboks varchar (40) null,
  levtelefon varchar (15) null,
  levkontakt varchar (30) null,
  levmerknad varchar (50) null,
  registrertdato datetime null,
  registrerttid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertav varchar (10) null,
  profilnr integer null,
  kortnavn varchar (30) null,
  organisasjonsnr varchar (30) null,
  lanbutikk tinyint null,
  sentrallager tinyint null,
  apningsdato datetime null,
  nedlagtdato datetime null,
  segmentkode integer null,
  stdveksel decimal(7,2) null,
  fakturagebyr decimal(7,2) null,
  clbutikknr integer null,
  plukkbutikk integer null,
  vaarref varchar (30) null,
  bankkonto varchar (20) null,
  postgiro varchar (20) null,
  butland varchar (30) null,
  urladresse varchar (40) null,
  epostadresse varchar (40) null,
  telefaks varchar (15) null,
  purregebyr decimal(7,2) null,
  fgmomskod integer null,
  pgmomskod integer null,
  dirfakturautskrift tinyint null,
  fakturaskriver varchar (30) null,
  fakturakopi integer null,
  fakturalayout integer null,
  fakttekstnr integer null,
  minusbutikk tinyint null,
  beprinter varchar (40) null,
  belayout integer null,
  beterminalklient tinyint null,
  beaktiv tinyint null,
  kundenr decimal(15,2) null,
  intfaktoverforing tinyint null,
  rapprinter varchar (30) null,
  fakturaadresse1 varchar (40) null,
  fakturaadresse2 varchar (40) null,
  fakturapostnr varchar (10) null,
  fakturapostboks varchar (40) null,
  harbutikksystem tinyint null,
  eodrapporter tinyint null,
  eodfinansrapport tinyint null,
  eodbokforingsbilag tinyint null,
  edojournal tinyint null,
  faktkopirappskriver tinyint null,
  kassenr integer null,
  etikettprinterkordre varchar (30) null,
  falckmedlnr varchar (4) null,
  kommisjonsdatostart datetime null,
  kampanje integer null,
  vpi integer null,
  statistikkoppdatering tinyint null,
  prioplukket integer null,
  katalogvaretellingsfil varchar (40) null,
  postetikettprinter varchar (30) null,
  butfirmanavn varchar (40) null,
  eksterntid varchar (15) null,
  rigalnr varchar (30) null,
  rigalsekvnr integer null,
  rigalfilkatalog varchar (30) null,
  rigalipadresse varchar (20) null,
  rigalmottakskatalog varchar (30) null,
  siebutikknr integer null,
  nettbutikk tinyint null,
  bloboverforing tinyint null,
  glnnr varchar (20) null,
  appserverparam varchar (30) null,
  dbconparam varchar (30) null,
  webbutik tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_butiker ON butiker for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from butiker t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX butiker#_#progress_recid ON butiker (PROGRESS_RECID)
go
CREATE UNIQUE INDEX butiker#_#progress_recid_ident_ ON butiker (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX butiker##butikin ON butiker (butik)
go
CREATE INDEX butiker##butnamn ON butiker (butnamn, PROGRESS_RECID)
go
CREATE INDEX butiker##eksterntid ON butiker (eksterntid, PROGRESS_RECID)
go
CREATE INDEX butiker##falkmedlnr ON butiker (falckmedlnr, PROGRESS_RECID)
go
CREATE INDEX butiker##glnnr ON butiker (glnnr, PROGRESS_RECID)
go
CREATE INDEX butiker##harbutikksystem ON butiker (harbutikksystem, PROGRESS_RECID)
go
CREATE INDEX butiker##kampanje ON butiker (kampanje, PROGRESS_RECID)
go
CREATE INDEX butiker##kortnavn ON butiker (kortnavn, PROGRESS_RECID)
go
CREATE INDEX butiker##lan ON butiker (lanbutikk, PROGRESS_RECID)
go
CREATE INDEX butiker##organisasjonsnr ON butiker (organisasjonsnr, PROGRESS_RECID)
go
CREATE INDEX butiker##postnrbutik ON butiker (buponr, butik, PROGRESS_RECID)
go
CREATE INDEX butiker##profilnr ON butiker (profilnr, PROGRESS_RECID)
go
CREATE INDEX butiker##segmentkode ON butiker (segmentkode, PROGRESS_RECID)
go
CREATE INDEX butiker##siebutikknr ON butiker (siebutikknr, PROGRESS_RECID)
go
CREATE INDEX butiker##vpi ON butiker (vpi, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'butikkforsalj' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table butikkforsalj
go
CREATE TABLE butikkforsalj (
  butik integer null,
  forsnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kassererid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_butikkforsalj ON butikkforsalj for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from butikkforsalj t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX butikkforsalj#_#progress_recid ON butikkforsalj (PROGRESS_RECID)
go
CREATE UNIQUE INDEX butikkforsalj#_#progress_recid_ident_ ON butikkforsalj (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX butikkforsalj##butikkforskassererid ON butikkforsalj (butik, forsnr, kassererid)
go
CREATE INDEX butikkforsalj##forsbutkassererid ON butikkforsalj (forsnr, butik, kassererid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'butikkkobling' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table butikkkobling
go
CREATE TABLE butikkkobling (
  teamtypeid integer not null,
  teamnr integer not null,
  butik integer not null,
  brgrpnr integer not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_butikkkobling ON butikkkobling for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from butikkkobling t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX butikkkobling#_#progress_recid ON butikkkobling (PROGRESS_RECID)
go
CREATE UNIQUE INDEX butikkkobling#_#progress_recid_ident_ ON butikkkobling (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX butikkkobling##butikkkobling ON butikkkobling (brgrpnr, teamtypeid, teamnr, butik)
go
if (select name from sysobjects 
    where name = 'butikkselger' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table butikkselger
go
CREATE TABLE butikkselger (
  selgernr decimal(15,2) null,
  butikknr integer null,
  selgerid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_butikkselger ON butikkselger for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from butikkselger t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX butikkselger#_#progress_recid ON butikkselger (PROGRESS_RECID)
go
CREATE UNIQUE INDEX butikkselger#_#progress_recid_ident_ ON butikkselger (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX butikkselger##butikkselgerselgerid ON butikkselger (butikknr, selgernr, selgerid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX butikkselger##selgernrbutikkselgerid ON butikkselger (selgernr, butikknr, selgerid)
go
if (select name from sysobjects 
    where name = 'butikkteam' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table butikkteam
go
CREATE TABLE butikkteam (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  teamtypeid integer not null,
  teamnr integer not null,
  beskrivelse varchar (40) null,
  notat varchar (40) null,
  brgrpnr integer not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_butikkteam ON butikkteam for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from butikkteam t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX butikkteam#_#progress_recid ON butikkteam (PROGRESS_RECID)
go
CREATE UNIQUE INDEX butikkteam#_#progress_recid_ident_ ON butikkteam (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX butikkteam##beskivelse ON butikkteam (teamtypeid, beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX butikkteam##butikkteam ON butikkteam (brgrpnr, teamtypeid, teamnr)
go
if (select name from sysobjects 
    where name = 'butikktilgang' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table butikktilgang
go
CREATE TABLE butikktilgang (
  brgrpnr integer not null,
  butik integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_butikktilgang ON butikktilgang for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from butikktilgang t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX butikktilgang#_#progress_recid ON butikktilgang (PROGRESS_RECID)
go
CREATE UNIQUE INDEX butikktilgang#_#progress_recid_ident_ ON butikktilgang (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX butikktilgang##brgrpnr ON butikktilgang (brgrpnr, butik)
go
if (select name from sysobjects 
    where name = 'butlok' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table butlok
go
CREATE TABLE butlok (
  butikknr integer null,
  lokasjonsnr varchar (4) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_butlok ON butlok for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from butlok t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX butlok#_#progress_recid ON butlok (PROGRESS_RECID)
go
CREATE UNIQUE INDEX butlok#_#progress_recid_ident_ ON butlok (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX butlok##butlok ON butlok (butikknr, lokasjonsnr)
go
if (select name from sysobjects 
    where name = 'dags_lbl' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table dags_lbl
go
CREATE TABLE dags_lbl (
  navn##1 varchar (20) null,
  navn##2 varchar (20) null,
  navn##3 varchar (20) null,
  navn##4 varchar (20) null,
  navn##5 varchar (20) null,
  navn##6 varchar (20) null,
  navn##7 varchar (20) null,
  navn##8 varchar (20) null,
  navn##9 varchar (20) null,
  navn##10 varchar (20) null,
  navn##11 varchar (20) null,
  navn##12 varchar (20) null,
  navn##13 varchar (20) null,
  navn##14 varchar (20) null,
  navn##15 varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_dags_lbl ON dags_lbl for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from dags_lbl t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX dags_lbl#_#progress_recid ON dags_lbl (PROGRESS_RECID)
go
CREATE UNIQUE INDEX dags_lbl#_#progress_recid_ident_ ON dags_lbl (PROGRESS_RECID_IDENT_ )
go
if (select name from sysobjects 
    where name = 'dags_rap' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table dags_rap
go
CREATE TABLE dags_rap (
  butikk integer null,
  dato datetime null,
  mnd integer null,
  hg1_oms decimal(11,2) null,
  hg2_oms decimal(11,2) null,
  hg3_oms decimal(11,2) null,
  hg4_oms decimal(11,2) null,
  hg5_oms decimal(11,2) null,
  hg6_oms decimal(11,2) null,
  retur_korr decimal(9,2) null,
  tb1 decimal(5,2) null,
  tb2 decimal(5,2) null,
  tb3 decimal(5,2) null,
  tb4 decimal(5,2) null,
  tb5 decimal(5,2) null,
  tb6 decimal(5,2) null,
  aar integer null,
  mvaverdi decimal(10,2) null,
  hg7_oms decimal(11,2) null,
  hg8_oms decimal(11,2) null,
  hg9_oms decimal(11,2) null,
  hg10_oms decimal(11,2) null,
  tb7 decimal(5,2) null,
  tb8 decimal(5,2) null,
  tb9 decimal(5,2) null,
  tb10 decimal(5,2) null,
  reklamasjon_korr decimal(9,2) null,
  retur_korr_andre decimal(9,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_dags_rap ON dags_rap for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from dags_rap t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX dags_rap#_#progress_recid ON dags_rap (PROGRESS_RECID)
go
CREATE UNIQUE INDEX dags_rap#_#progress_recid_ident_ ON dags_rap (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX dags_rap##dags_rap ON dags_rap (butikk, dato)
go
CREATE INDEX dags_rap##mnd_rap ON dags_rap (butikk, aar, mnd, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'defaultlevdato' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table defaultlevdato
go
CREATE TABLE defaultlevdato (
  levnr integer null,
  butikknr integer null,
  ukedag integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  tid integer null,
  hastertid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_defaultlevdato ON defaultlevdato for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from defaultlevdato t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX defaultlevdato#_#progress_recid ON defaultlevdato (PROGRESS_RECID)
go
CREATE UNIQUE INDEX defaultlevdato#_#progress_recid_ident_ ON defaultlevdato (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX defaultlevdato##defaultlevdato ON defaultlevdato (butikknr, levnr)
go
if (select name from sysobjects 
    where name = 'distributefile' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table distributefile
go
CREATE TABLE distributefile (
  id varchar (12) null,
  date_ datetime null,
  butikknr integer null,
  filename varchar (30) null,
  fileobject varbinary(max) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_distributefile ON distributefile for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from distributefile t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX distributefile#_#progress_recid ON distributefile (PROGRESS_RECID)
go
CREATE UNIQUE INDEX distributefile#_#progress_recid_ident_ ON distributefile (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX distributefile##datebutikknr ON distributefile (date_, butikknr)
go
CREATE INDEX distributefile##filename ON distributefile (filename, PROGRESS_RECID)
go
CREATE INDEX distributefile##id ON distributefile (id, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'distributefilereceiver' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table distributefilereceiver
go
CREATE TABLE distributefilereceiver (
  id varchar (12) null,
  distributefileid varchar (12) null,
  kassenr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_distributefilereceiver ON distributefilereceiver for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from distributefilereceiver t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX distributefilereceiver#_#progress_recid ON distributefilereceiver (PROGRESS_RECID)
go
CREATE UNIQUE INDEX distributefilereceiver#_#progress_recid_ident_ ON distributefilereceiver (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX distributefilereceiver##distributefileid ON distributefilereceiver (distributefileid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX distributefilereceiver##id ON distributefilereceiver (id)
go
CREATE UNIQUE INDEX distributefilereceiver##idxidkassenr ON distributefilereceiver (distributefileid, kassenr)
go
CREATE INDEX distributefilereceiver##kassenr ON distributefilereceiver (kassenr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'driftsform' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table driftsform
go
CREATE TABLE driftsform (
  driftsformid integer null,
  driftsformnavn varchar (30) null,
  driftsformnotat varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_driftsform ON driftsform for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from driftsform t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX driftsform#_#progress_recid ON driftsform (PROGRESS_RECID)
go
CREATE UNIQUE INDEX driftsform#_#progress_recid_ident_ ON driftsform (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX driftsform##driftsformid ON driftsform (driftsformid)
go
CREATE INDEX driftsform##driftsformnavn ON driftsform (driftsformnavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'driftstype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table driftstype
go
CREATE TABLE driftstype (
  driftsformid integer null,
  driftstypeid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  driftstypenavn varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_driftstype ON driftstype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from driftstype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX driftstype#_#progress_recid ON driftstype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX driftstype#_#progress_recid_ident_ ON driftstype (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX driftstype##driftstype ON driftstype (driftsformid, driftstypeid)
go
CREATE INDEX driftstype##driftstypenavn ON driftstype (driftsformid, driftstypenavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'eannrliste' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table eannrliste
go
CREATE TABLE eannrliste (
  eanserieid integer null,
  eankode varchar (20) null,
  artikkelnr decimal(15,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_eannrliste ON eannrliste for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from eannrliste t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX eannrliste#_#progress_recid ON eannrliste (PROGRESS_RECID)
go
CREATE UNIQUE INDEX eannrliste#_#progress_recid_ident_ ON eannrliste (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX eannrliste##idxartikkelnr ON eannrliste (artikkelnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX eannrliste##idxeankode ON eannrliste (eankode)
go
CREATE INDEX eannrliste##idxeanserieid ON eannrliste (eanserieid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'eannrserie' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table eannrserie
go
CREATE TABLE eannrserie (
  eanserieid integer null,
  eanbeskrivelse varchar (30) null,
  eantype integer null,
  antsifferilevnr integer null,
  eanlevnr integer null,
  fraeanartikkelnr integer null,
  tileanartikkelnr integer null,
  eanlandkode integer null,
  cl integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  eanserieaktiv tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_eannrserie ON eannrserie for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from eannrserie t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX eannrserie#_#progress_recid ON eannrserie (PROGRESS_RECID)
go
CREATE UNIQUE INDEX eannrserie#_#progress_recid_ident_ ON eannrserie (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX eannrserie##idxaktiv ON eannrserie (eanserieaktiv, PROGRESS_RECID)
go
CREATE INDEX eannrserie##idxcl ON eannrserie (cl, PROGRESS_RECID)
go
CREATE INDEX eannrserie##idxeanbeskrivelse ON eannrserie (eanbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX eannrserie##idxeannrserie ON eannrserie (eanserieid)
go
if (select name from sysobjects 
    where name = 'edbdatatype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table edbdatatype
go
CREATE TABLE edbdatatype (
  datatype varchar (30) null,
  typebeskrivelse varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_edbdatatype ON edbdatatype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from edbdatatype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX edbdatatype#_#progress_recid ON edbdatatype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX edbdatatype#_#progress_recid_ident_ ON edbdatatype (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX edbdatatype##datatype ON edbdatatype (datatype)
go
CREATE INDEX edbdatatype##typebeskrivelse ON edbdatatype (typebeskrivelse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ekstbutiker' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ekstbutiker
go
CREATE TABLE ekstbutiker (
  butik integer null,
  butnamn varchar (20) null,
  buadr varchar (20) null,
  buponr varchar (6) null,
  bupadr varchar (20) null,
  bukon varchar (20) null,
  butel varchar (20) null,
  registrertdato datetime null,
  registrerttid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertav varchar (10) null,
  kortnavn varchar (30) null,
  organisasjonsnr varchar (30) null,
  epostadresse varchar (40) null,
  kundenr decimal(15,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ekstbutiker ON ekstbutiker for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ekstbutiker t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ekstbutiker#_#progress_recid ON ekstbutiker (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ekstbutiker#_#progress_recid_ident_ ON ekstbutiker (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX ekstbutiker##butik ON ekstbutiker (butik, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ekstedbsystem' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ekstedbsystem
go
CREATE TABLE ekstedbsystem (
  edbsystem varchar (30) null,
  sysbeskrivelse varchar (30) null,
  datatype varchar (30) null,
  filkatalog varchar (40) null,
  filprefix varchar (30) null,
  filekstent varchar (30) null,
  seqnr integer null,
  maksseq integer null,
  ekspfrekvens integer null,
  starttid integer null,
  stopptid integer null,
  tidsintervall integer null,
  aktiv tinyint null,
  seqvaktiv tinyint null,
  eksportrutine varchar (40) null,
  edb_system varchar (12) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ekstedbsystem ON ekstedbsystem for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ekstedbsystem t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ekstedbsystem#_#progress_recid ON ekstedbsystem (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ekstedbsystem#_#progress_recid_ident_ ON ekstedbsystem (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX ekstedbsystem##datatype ON ekstedbsystem (datatype, PROGRESS_RECID)
go
CREATE INDEX ekstedbsystem##eb_system ON ekstedbsystem (edb_system, PROGRESS_RECID)
go
CREATE UNIQUE INDEX ekstedbsystem##edbsystem ON ekstedbsystem (edbsystem)
go
CREATE INDEX ekstedbsystem##sysbeskrivelse ON ekstedbsystem (sysbeskrivelse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ekstvpifil' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ekstvpifil
go
CREATE TABLE ekstvpifil (
  ekstvpilevnr integer null,
  vpifilnr integer null,
  vpifilnavn varchar (30) null,
  vpiekst varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  vpikatalog varchar (30) null,
  vpifilmaske varchar (20) null,
  vpiinnlesningsrutine varchar (30) null,
  vpiutpakkingsrutine varchar (30) null,
  vpioppdateringsrutine varchar (30) null,
  vpifilaktiv tinyint null,
  vpifilbeskrivelse varchar (40) null,
  vpifiltypenr integer null,
  vpioperator integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ekstvpifil ON ekstvpifil for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ekstvpifil t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ekstvpifil#_#progress_recid ON ekstvpifil (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ekstvpifil#_#progress_recid_ident_ ON ekstvpifil (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX ekstvpifil##ekstvpifil ON ekstvpifil (ekstvpilevnr, vpifilnr)
go
CREATE INDEX ekstvpifil##vpifilbeskrivelse ON ekstvpifil (vpifilbeskrivelse, PROGRESS_RECID)
go
CREATE INDEX ekstvpifil##vpifilnavn ON ekstvpifil (ekstvpilevnr, vpifilnavn, PROGRESS_RECID)
go
CREATE INDEX ekstvpifil##vpifiltypenr ON ekstvpifil (vpifiltypenr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ekstvpilev' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ekstvpilev
go
CREATE TABLE ekstvpilev (
  ekstvpilevnr integer null,
  kortnavn varchar (10) null,
  navn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  oppdatmaskevpi varchar (40) null,
  aktivlev tinyint null,
  prionr integer null,
  levnr integer null,
  edb_system varchar (12) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ekstvpilev ON ekstvpilev for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ekstvpilev t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ekstvpilev#_#progress_recid ON ekstvpilev (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ekstvpilev#_#progress_recid_ident_ ON ekstvpilev (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX ekstvpilev##edb_system ON ekstvpilev (edb_system, PROGRESS_RECID)
go
CREATE UNIQUE INDEX ekstvpilev##ekstvpilev ON ekstvpilev (ekstvpilevnr)
go
CREATE INDEX ekstvpilev##kortnavn ON ekstvpilev (kortnavn, PROGRESS_RECID)
go
CREATE INDEX ekstvpilev##leverandor ON ekstvpilev (levnr, PROGRESS_RECID)
go
CREATE INDEX ekstvpilev##navn ON ekstvpilev (navn, PROGRESS_RECID)
go
CREATE INDEX ekstvpilev##prionr ON ekstvpilev (prionr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ekstvpitabell' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ekstvpitabell
go
CREATE TABLE ekstvpitabell (
  ekstvpilevnr integer null,
  tabellnavn varchar (30) null,
  tabellnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ekstvpitabell ON ekstvpitabell for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ekstvpitabell t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ekstvpitabell#_#progress_recid ON ekstvpitabell (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ekstvpitabell#_#progress_recid_ident_ ON ekstvpitabell (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX ekstvpitabell##ekstvpitabell ON ekstvpitabell (ekstvpilevnr, tabellnr)
go
CREATE INDEX ekstvpitabell##navn ON ekstvpitabell (ekstvpilevnr, tabellnavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'elogg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table elogg
go
CREATE TABLE elogg (
  tabellnavn varchar (20) null,
  eksterntsystem varchar (20) null,
  endringstype integer null,
  behandlet tinyint null,
  verdier varchar (30) null,
  opprettet decimal(18,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_elogg ON elogg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from elogg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX elogg#_#progress_recid ON elogg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX elogg#_#progress_recid_ident_ ON elogg (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX elogg##behandlet ON elogg (behandlet, PROGRESS_RECID)
go
CREATE INDEX elogg##eksterntsystem ON elogg (eksterntsystem, PROGRESS_RECID)
go
CREATE INDEX elogg##endringstype ON elogg (endringstype, PROGRESS_RECID)
go
CREATE UNIQUE INDEX elogg##endrlogg ON elogg (tabellnavn, eksterntsystem, verdier)
go
CREATE INDEX elogg##opprettet ON elogg (opprettet, PROGRESS_RECID)
go
CREATE INDEX elogg##opprettetdato ON elogg (registrertdato, PROGRESS_RECID)
go
CREATE INDEX elogg##postbehandlet ON elogg (tabellnavn, eksterntsystem, behandlet, PROGRESS_RECID)
go
CREATE INDEX elogg##verdier ON elogg (verdier, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'eodkasse' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table eodkasse
go
CREATE TABLE eodkasse (
  butikknr integer not null,
  gruppenr integer not null,
  kassenr integer not null,
  eoddato datetime null,
  totalsum decimal(11,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_eodkasse ON eodkasse for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from eodkasse t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX eodkasse#_#progress_recid ON eodkasse (PROGRESS_RECID)
go
CREATE UNIQUE INDEX eodkasse#_#progress_recid_ident_ ON eodkasse (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX eodkasse##eodkasse ON eodkasse (butikknr, gruppenr, kassenr, eoddato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'erstattningsvare' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table erstattningsvare
go
CREATE TABLE erstattningsvare (
  erstattid integer null,
  artikkelnr decimal(13,0) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_erstattningsvare ON erstattningsvare for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from erstattningsvare t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX erstattningsvare#_#progress_recid ON erstattningsvare (PROGRESS_RECID)
go
CREATE UNIQUE INDEX erstattningsvare#_#progress_recid_ident_ ON erstattningsvare (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX erstattningsvare##artikkelnr ON erstattningsvare (artikkelnr)
go
CREATE INDEX erstattningsvare##erstattid ON erstattningsvare (erstattid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'etikett' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table etikett
go
CREATE TABLE etikett (
  vg integer null,
  lopnr integer null,
  storlek varchar (10) null,
  pris decimal(8,2) null,
  texten varchar (10) null,
  antal integer null,
  butik integer null,
  levinnr integer null,
  rad integer null,
  kode varchar (20) null,
  artikkelnr decimal(13,0) null,
  strkode integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  etikettekst1 varchar (30) null,
  etilayout integer null,
  etikettekst2 varchar (30) null,
  pris2 decimal(7,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_etikett ON etikett for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from etikett t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX etikett#_#progress_recid ON etikett (PROGRESS_RECID)
go
CREATE UNIQUE INDEX etikett#_#progress_recid_ident_ ON etikett (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX etikett##ekstra ON etikett (vg, lopnr, storlek, butik, PROGRESS_RECID)
go
CREATE UNIQUE INDEX etikett##etikett ON etikett (butik, vg, lopnr, storlek)
go
CREATE INDEX etikett##ordre ON etikett (levinnr, rad, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'etikettko' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table etikettko
go
CREATE TABLE etikettko (
  artikkelnr decimal(13,0) null,
  strkode integer null,
  kode varchar (20) null,
  butikknr integer null,
  etikettanthylleplasser integer null,
  etikettekst1 varchar (30) null,
  etikettekst2 varchar (30) null,
  etilayout integer null,
  pris##1 decimal(9,2) null,
  pris##2 decimal(9,2) null,
  mengde decimal(6,3) null,
  jamforenhet varchar (4) null,
  utskriftsnr integer null,
  utskriftsdato datetime null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_etikettko ON etikettko for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from etikettko t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX etikettko#_#progress_recid ON etikettko (PROGRESS_RECID)
go
CREATE UNIQUE INDEX etikettko#_#progress_recid_ident_ ON etikettko (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX etikettko##idxartikkelnr ON etikettko (artikkelnr, PROGRESS_RECID)
go
CREATE INDEX etikettko##idxbutikknr ON etikettko (butikknr, PROGRESS_RECID)
go
CREATE INDEX etikettko##idxkode ON etikettko (kode, PROGRESS_RECID)
go
CREATE INDEX etikettko##idxstrkode ON etikettko (strkode, PROGRESS_RECID)
go
CREATE INDEX etikettko##idxutskriftsdato ON etikettko (utskriftsdato, PROGRESS_RECID)
go
CREATE INDEX etikettko##idxutskriftsnr ON etikettko (utskriftsnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'extbutikk' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table extbutikk
go
CREATE TABLE extbutikk (
  butikknr integer not null,
  navn varchar (30) null,
  kortnavn varchar (30) null,
  optchar varchar (20) null,
  optint integer null,
  optdec decimal(7,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_extbutikk ON extbutikk for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from extbutikk t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX extbutikk#_#progress_recid ON extbutikk (PROGRESS_RECID)
go
CREATE UNIQUE INDEX extbutikk#_#progress_recid_ident_ ON extbutikk (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX extbutikk##but ON extbutikk (butikknr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'fakturahode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table fakturahode
go
CREATE TABLE fakturahode (
  faktura_id decimal(15,2) not null,
  fakturanr decimal(13,0) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kundenr decimal(15,2) null,
  navn varchar (40) null,
  adresse1 varchar (40) null,
  adresse2 varchar (40) null,
  postnr varchar (10) null,
  telefon varchar (15) null,
  kontnavn varchar (40) null,
  butikknr integer null,
  avgplsalg decimal(10,2) null,
  avgfrisalg decimal(10,2) null,
  mva decimal(10,2) null,
  totalt decimal(11,2) null,
  dato datetime null,
  antdager integer null,
  telefaks varchar (15) null,
  faktadresse1 varchar (30) null,
  faktadresse2 varchar (30) null,
  faktpostnr varchar (15) null,
  konttelefon varchar (15) null,
  fakttekstnr integer null,
  totalrabatt_ decimal(4,2) null,
  totalrabattkr decimal(8,2) null,
  mvakr decimal(8,2) null,
  fakturertdato datetime null,
  fakturerttid integer null,
  fakturertav varchar (20) null,
  forfallsdato datetime null,
  produksjonsdato datetime null,
  levfnr integer null,
  betbet integer null,
  kprosjektnr integer null,
  sendingsnr varchar (30) null,
  varespesifikasjon varchar (30) null,
  embalage varchar (30) null,
  fraktbrevtekst varchar (30) null,
  godsmerking varchar (30) null,
  utsendelsesdato datetime null,
  antkolli integer null,
  bruttovekt decimal(7,2) null,
  totaltvolum decimal(7,2) null,
  valkod varchar (3) null,
  vaarref varchar (30) null,
  deresref varchar (30) null,
  referanse varchar (30) null,
  avrundingtype integer null,
  land varchar (30) null,
  samlefaktura tinyint null,
  poststed varchar (30) null,
  faktpoststed varchar (30) null,
  faktland varchar (30) null,
  firmanavn varchar (30) null,
  firmaadresse1 varchar (30) null,
  firmaadresse2 varchar (30) null,
  firmapostnr varchar (10) null,
  firmapoststed varchar (30) null,
  firmatelefon varchar (15) null,
  firmatelefaks varchar (15) null,
  firmaepost varchar (40) null,
  firmaorganisasjonsnr varchar (12) null,
  firmabankkonto varchar (20) null,
  firmapostgiro varchar (20) null,
  kordre_id decimal(15,2) not null,
  firmaland varchar (30) null,
  firmaurladresse varchar (40) null,
  butikksalg tinyint null,
  purretrinn integer null,
  leveringsdato datetime null,
  bilagstype integer null,
  fnotat varchar (40) null,
  nettopris decimal(9,2) null,
  avrundingkr decimal(5,2) null,
  valkurs decimal(8,6) null,
  bettekst varchar (30) null,
  kid decimal(27,2) null,
  levformbeskrivelse varchar (30) null,
  levformmetode varchar (20) null,
  fakturagebyr tinyint null,
  bttekst varchar (30) null,
  forskuddsbetalt decimal(11,2) null,
  eksportertdato datetime null,
  eksportertav varchar (15) null,
  levadresse1 varchar (30) null,
  levadresse2 varchar (30) null,
  levpostnr varchar (10) null,
  levland varchar (30) null,
  levpoststed varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_fakturahode ON fakturahode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from fakturahode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX fakturahode#_#progress_recid ON fakturahode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX fakturahode#_#progress_recid_ident_ ON fakturahode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX fakturahode##bilagstype ON fakturahode (bilagstype, PROGRESS_RECID)
go
CREATE INDEX fakturahode##butikknr ON fakturahode (butikknr, samlefaktura, fakturanr, faktura_id, PROGRESS_RECID)
go
CREATE INDEX fakturahode##butikksalg ON fakturahode (butikksalg, PROGRESS_RECID)
go
CREATE INDEX fakturahode##butkid ON fakturahode (butikknr, kid, PROGRESS_RECID)
go
CREATE INDEX fakturahode##eksportert ON fakturahode (eksportertdato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX fakturahode##fakthode ON fakturahode (faktura_id)
go
CREATE INDEX fakturahode##fakttekst ON fakturahode (fakttekstnr, PROGRESS_RECID)
go
CREATE INDEX fakturahode##fakturanr ON fakturahode (bilagstype, fakturanr, PROGRESS_RECID)
go
CREATE INDEX fakturahode##kid ON fakturahode (kid, PROGRESS_RECID)
go
CREATE INDEX fakturahode##kordrenr ON fakturahode (kordre_id, PROGRESS_RECID)
go
CREATE INDEX fakturahode##kunde ON fakturahode (kundenr, fakturanr, PROGRESS_RECID)
go
CREATE INDEX fakturahode##purregebyr ON fakturahode (fakturagebyr, PROGRESS_RECID)
go
CREATE INDEX fakturahode##purretrinn ON fakturahode (purretrinn, PROGRESS_RECID)
go
CREATE INDEX fakturahode##samlefaktura ON fakturahode (samlefaktura, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'fakturalinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table fakturalinje
go
CREATE TABLE fakturalinje (
  faktura_id decimal(15,2) not null,
  fakturalinjenr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  opphav integer null,
  varenr varchar (20) null,
  varetekst varchar (30) null,
  antall decimal(7,2) null,
  linjerab_ decimal(7,2) null,
  linjerabattkr decimal(8,2) null,
  totalrabattkr decimal(8,2) null,
  nettopris decimal(8,2) null,
  momskod integer null,
  mvakr decimal(8,2) null,
  nettolinjesum decimal(8,2) null,
  notat varchar (40) null,
  leveringsdato datetime null,
  db_ decimal(7,2) null,
  dbkr decimal(8,2) null,
  storl varchar (10) null,
  ekstrefid varchar (30) null,
  ekstreftekst varchar (30) null,
  mva_ decimal(7,2) null,
  linjesum decimal(8,2) null,
  ttid integer not null,
  tbid integer null,
  totrab_ decimal(7,2) null,
  b_id decimal(15,2) null,
  bonglinjenr integer null,
  levfargkod varchar (15) null,
  artikkelnr decimal(13,0) null,
  pris decimal(8,2) null,
  arbeidsbeskr varchar (40) null,
  varespesifikasjon varchar (30) null,
  normalpris decimal(8,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_fakturalinje ON fakturalinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from fakturalinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX fakturalinje#_#progress_recid ON fakturalinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX fakturalinje#_#progress_recid_ident_ ON fakturalinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX fakturalinje##bong ON fakturalinje (b_id, bonglinjenr, PROGRESS_RECID)
go
CREATE INDEX fakturalinje##ekstref ON fakturalinje (ekstrefid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX fakturalinje##faktlinje ON fakturalinje (faktura_id, fakturalinjenr)
go
CREATE INDEX fakturalinje##opphav ON fakturalinje (opphav, varenr, PROGRESS_RECID)
go
CREATE INDEX fakturalinje##varetekst ON fakturalinje (faktura_id, varetekst, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'fakturatekst' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table fakturatekst
go
CREATE TABLE fakturatekst (
  fakttekstnr integer null,
  fakttekst varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_fakturatekst ON fakturatekst for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from fakturatekst t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX fakturatekst#_#progress_recid ON fakturatekst (PROGRESS_RECID)
go
CREATE UNIQUE INDEX fakturatekst#_#progress_recid_ident_ ON fakturatekst (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX fakturatekst##tekst ON fakturatekst (fakttekst, PROGRESS_RECID)
go
CREATE UNIQUE INDEX fakturatekst##tekstnr ON fakturatekst (fakttekstnr)
go
if (select name from sysobjects 
    where name = 'falckeksport' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table falckeksport
go
CREATE TABLE falckeksport (
  eksportid decimal(11,2) null,
  butikknr integer null,
  eksportdato datetime null,
  eksporttid integer null,
  merknad varchar (30) null,
  notat varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_falckeksport ON falckeksport for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from falckeksport t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX falckeksport#_#progress_recid ON falckeksport (PROGRESS_RECID)
go
CREATE UNIQUE INDEX falckeksport#_#progress_recid_ident_ ON falckeksport (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX falckeksport##edato ON falckeksport (edato, PROGRESS_RECID)
go
CREATE INDEX falckeksport##eksportbutdato ON falckeksport (butikknr, eksportdato, PROGRESS_RECID)
go
CREATE INDEX falckeksport##eksportdatobut ON falckeksport (eksportdato, butikknr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX falckeksport##eksportid ON falckeksport (eksportid)
go
CREATE INDEX falckeksport##registrertdato ON falckeksport (registrertdato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'falck_sykkelregister' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table falck_sykkelregister
go
CREATE TABLE falck_sykkelregister (
  butikknr integer null,
  bulk_kode varchar (16) null,
  eier_fornavn varchar (50) null,
  eier_etternavn varchar (50) null,
  eier_adresse varchar (50) null,
  eier_postnr varchar (10) null,
  eier_poststed varchar (30) null,
  eier_epost varchar (100) null,
  eier_telefon varchar (16) null,
  eier_mobil varchar (16) null,
  eier_fodt datetime null,
  fabrikat varchar (30) null,
  sykkeltype varchar (30) null,
  dato_solgt datetime null,
  rammenummer varchar (30) null,
  arsmodell varchar (4) null,
  pris decimal(8,2) null,
  kampanjekode varchar (4) null,
  dbsindividnummer varchar (9) null,
  dbsnokkelnummer varchar (10) null,
  dbsartikkelnummer varchar (15) null,
  dbskundenummer varchar (10) null,
  kampanjekode2 varchar (4) null,
  forhandlerreklame tinyint null,
  bildereferanse varchar (32) null,
  sec_nr varchar (16) null,
  klarforsending tinyint null,
  sendtdato datetime null,
  sendttid integer null,
  eksportertdato datetime null,
  eksporterttid integer null,
  forsnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  transsaksjonsnumer decimal(10,0) null,
  foresatt_fornavn varchar (30) null,
  foresatt_etternavn varchar (30) null,
  foresatt_fodt datetime null,
  eksportid decimal(11,2) null,
  kjonn integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_falck_sykkelregister ON falck_sykkelregister for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from falck_sykkelregister t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX falck_sykkelregister#_#progress_recid ON falck_sykkelregister (PROGRESS_RECID)
go
CREATE UNIQUE INDEX falck_sykkelregister#_#progress_recid_ident_ ON falck_sykkelregister (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX falck_sykkelregister##butikktrans ON falck_sykkelregister (butikknr, transsaksjonsnumer, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##eier_adresse ON falck_sykkelregister (eier_adresse, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##eier_etternavn ON falck_sykkelregister (eier_etternavn, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##eier_fornavn ON falck_sykkelregister (eier_fornavn, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##eier_mobil ON falck_sykkelregister (eier_mobil, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##eier_postnr ON falck_sykkelregister (eier_postnr, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##eier_poststed ON falck_sykkelregister (eier_poststed, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##eier_telefon ON falck_sykkelregister (eier_telefon, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##eksportid ON falck_sykkelregister (eksportid, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##fabrikat ON falck_sykkelregister (fabrikat, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##foresatt_etternavn ON falck_sykkelregister (foresatt_etternavn, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##foresatt_fodt ON falck_sykkelregister (foresatt_fodt, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##foresatt_fornavn ON falck_sykkelregister (foresatt_fornavn, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##kampanjekode ON falck_sykkelregister (kampanjekode, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##kampanjekode2 ON falck_sykkelregister (kampanjekode2, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##kjonn ON falck_sykkelregister (kjonn, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##klarforsending ON falck_sykkelregister (klarforsending, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##rammenummer ON falck_sykkelregister (rammenummer, PROGRESS_RECID)
go
CREATE INDEX falck_sykkelregister##sykkeltype ON falck_sykkelregister (sykkeltype, PROGRESS_RECID)
go
CREATE UNIQUE INDEX falck_sykkelregister##transaksjonsnummer ON falck_sykkelregister (transsaksjonsnumer)
go
if (select name from sysobjects 
    where name = 'farg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table farg
go
CREATE TABLE farg (
  farg integer null,
  farbeskr varchar (30) null,
  kfarge varchar (5) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_farg ON farg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from farg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX farg#_#progress_recid ON farg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX farg#_#progress_recid_ident_ ON farg (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX farg##farbeskr ON farg (farbeskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX farg##fargin ON farg (farg)
go
CREATE INDEX farg##kfarge ON farg (kfarge, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'feilkode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table feilkode
go
CREATE TABLE feilkode (
  feilkode integer null,
  beskrivelse varchar (40) null,
  notat varchar (30) null,
  belastes integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  transkode integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_feilkode ON feilkode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from feilkode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX feilkode#_#progress_recid ON feilkode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX feilkode#_#progress_recid_ident_ ON feilkode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX feilkode##beskrivelse ON feilkode (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX feilkode##feilkode ON feilkode (feilkode)
go
if (select name from sysobjects 
    where name = 'filarkiv' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table filarkiv
go
CREATE TABLE filarkiv (
  katalog varchar (15) null,
  filnavn varchar (20) null,
  dato datetime null,
  datoopprettet datetime null,
  tid varchar (10) null,
  storlek varchar (12) null,
  dokumentnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_filarkiv ON filarkiv for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from filarkiv t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX filarkiv#_#progress_recid ON filarkiv (PROGRESS_RECID)
go
CREATE UNIQUE INDEX filarkiv#_#progress_recid_ident_ ON filarkiv (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX filarkiv##dato ON filarkiv (katalog, dato, filnavn)
go
CREATE INDEX filarkiv##dokumentnr ON filarkiv (dokumentnr, PROGRESS_RECID)
go
CREATE INDEX filarkiv##filnavn ON filarkiv (katalog, filnavn, dato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'foder' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table foder
go
CREATE TABLE foder (
  foder_id integer null,
  beskrivning varchar (20) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_foder ON foder for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from foder t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX foder#_#progress_recid ON foder (PROGRESS_RECID)
go
CREATE UNIQUE INDEX foder#_#progress_recid_ident_ ON foder (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX foder##foder ON foder (foder_id)
go
CREATE INDEX foder##foder_beskr ON foder (beskrivning, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'forsalj' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table forsalj
go
CREATE TABLE forsalj (
  forsnr integer null,
  foanstnr integer null,
  fonamn varchar (30) null,
  foadr varchar (30) null,
  foponr varchar (6) null,
  fopadr varchar (30) null,
  fotel varchar (15) null,
  fopersnr decimal(11,0) null,
  levnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  ansattnr varchar (15) null,
  rabatt integer null,
  prisendring integer null,
  retur integer null,
  sletttidligere integer null,
  slettbong integer null,
  sletteforste integer null,
  fodtdato datetime null,
  navnikasse varchar (15) null,
  passord integer null,
  forsaljaktiv tinyint null,
  brukerid2 varchar (10) null,
  butikknr integer null,
  brukeridprs varchar (15) null,
  fofornavn varchar (30) null,
  lonnprofil varchar (4) null,
  arbeidsprosent decimal(4,2) null,
  timelonn decimal(7,2) null,
  fastlonn decimal(8,2) null,
  ansattdato datetime null,
  sluttetdato datetime null,
  jobbtittel varchar (30) null,
  foadr2 varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_forsalj ON forsalj for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from forsalj t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX forsalj#_#progress_recid ON forsalj (PROGRESS_RECID)
go
CREATE UNIQUE INDEX forsalj#_#progress_recid_ident_ ON forsalj (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX forsalj##brukerid2 ON forsalj (brukerid2, PROGRESS_RECID)
go
CREATE INDEX forsalj##butikknr ON forsalj (butikknr, PROGRESS_RECID)
go
CREATE INDEX forsalj##fonamn ON forsalj (fonamn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX forsalj##forsalin ON forsalj (forsnr)
go
CREATE INDEX forsalj##forsaljaktiv ON forsalj (forsaljaktiv, PROGRESS_RECID)
go
CREATE INDEX forsalj##prsbrukerid ON forsalj (brukeridprs, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'fributik' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table fributik
go
CREATE TABLE fributik (
  bestnr integer null,
  butik integer null,
  beststat integer null,
  friantal##1 integer null,
  friantal##2 integer null,
  friantal##3 integer null,
  friantal##4 integer null,
  friantal##5 integer null,
  friantal##6 integer null,
  friantal##7 integer null,
  friantal##8 integer null,
  friantal##9 integer null,
  friantal##10 integer null,
  friantal##11 integer null,
  friantal##12 integer null,
  friantal##13 integer null,
  friantal##14 integer null,
  friantal##15 integer null,
  friantal##16 integer null,
  friantal##17 integer null,
  friantal##18 integer null,
  friantal##19 integer null,
  friantal##20 integer null,
  friantal##21 integer null,
  friantal##22 integer null,
  friantal##23 integer null,
  friantal##24 integer null,
  friantal##25 integer null,
  friantal##26 integer null,
  friantal##27 integer null,
  friantal##28 integer null,
  friantal##29 integer null,
  friantal##30 integer null,
  friantal##31 integer null,
  friantal##32 integer null,
  friantal##33 integer null,
  friantal##34 integer null,
  friantal##35 integer null,
  friantal##36 integer null,
  friantal##37 integer null,
  friantal##38 integer null,
  friantal##39 integer null,
  friantal##40 integer null,
  friantal##41 integer null,
  friantal##42 integer null,
  friantal##43 integer null,
  friantal##44 integer null,
  friantal##45 integer null,
  friantal##46 integer null,
  friantal##47 integer null,
  friantal##48 integer null,
  friantal##49 integer null,
  friantal##50 integer null,
  totantal integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_fributik ON fributik for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from fributik t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX fributik#_#progress_recid ON fributik (PROGRESS_RECID)
go
CREATE UNIQUE INDEX fributik#_#progress_recid_ident_ ON fributik (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX fributik##fributik ON fributik (bestnr, butik, beststat, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'fvektartikkel' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table fvektartikkel
go
CREATE TABLE fvektartikkel (
  butikknr integer null,
  fvektnr integer null,
  artikkelnr decimal(15,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_fvektartikkel ON fvektartikkel for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from fvektartikkel t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX fvektartikkel#_#progress_recid ON fvektartikkel (PROGRESS_RECID)
go
CREATE UNIQUE INDEX fvektartikkel#_#progress_recid_ident_ ON fvektartikkel (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX fvektartikkel##fvektartikkel ON fvektartikkel (butikknr, fvektnr, artikkelnr)
go
if (select name from sysobjects 
    where name = 'fvektreg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table fvektreg
go
CREATE TABLE fvektreg (
  fvektnr integer null,
  fvektnavn varchar (30) null,
  butikknr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  fvektfilkatalog varchar (30) null,
  fvektfilprefiks varchar (30) null,
  fvektfilsufiks varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_fvektreg ON fvektreg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from fvektreg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX fvektreg#_#progress_recid ON fvektreg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX fvektreg#_#progress_recid_ident_ ON fvektreg (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX fvektreg##fvektnavn ON fvektreg (fvektnavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX fvektreg##fvektreg ON fvektreg (butikknr, fvektnr)
go
if (select name from sysobjects 
    where name = 'fvekttype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table fvekttype
go
CREATE TABLE fvekttype (
  fvekttype integer null,
  fvtypenavn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_fvekttype ON fvekttype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from fvekttype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX fvekttype#_#progress_recid ON fvekttype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX fvekttype#_#progress_recid_ident_ ON fvekttype (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX fvekttype##fvekttype ON fvekttype (fvekttype)
go
CREATE INDEX fvekttype##fvtypenavn ON fvekttype (fvtypenavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'fylke' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table fylke
go
CREATE TABLE fylke (
  fylkesnr varchar (30) null,
  beskrivelse varchar (30) null,
  merknad varchar (50) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_fylke ON fylke for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from fylke t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX fylke#_#progress_recid ON fylke (PROGRESS_RECID)
go
CREATE UNIQUE INDEX fylke#_#progress_recid_ident_ ON fylke (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX fylke##beskrivelse ON fylke (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX fylke##fylkesnr ON fylke (fylkesnr)
go
if (select name from sysobjects 
    where name = 'garanti' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table garanti
go
CREATE TABLE garanti (
  garantikl integer not null,
  garantitekst varchar (30) null,
  mndant integer not null,
  bonga5 tinyint not null,
  fritekst varchar (1000) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_garanti ON garanti for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from garanti t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX garanti#_#progress_recid ON garanti (PROGRESS_RECID)
go
CREATE UNIQUE INDEX garanti#_#progress_recid_ident_ ON garanti (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX garanti##garantiidx1 ON garanti (garantikl)
go
CREATE INDEX garanti##garantiidx2 ON garanti (garantitekst, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'gavekort' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table gavekort
go
CREATE TABLE gavekort (
  butnr integer not null,
  identnr varchar (20) null,
  identtype integer not null,
  modus integer not null,
  dato datetime not null,
  tid integer not null,
  kassenr integer not null,
  kassnr integer not null,
  bongnr integer not null,
  gyldigdato datetime null,
  belop decimal(10,2) not null,
  bruktdato datetime null,
  brukttid integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  frab_id decimal(15,2) null,
  bruktb_id decimal(15,2) null,
  bruktbutnr integer null,
  bruktbongnr integer null,
  medlemsnr decimal(15,2) null,
  mfornavn varchar (40) null,
  madresse1 varchar (40) null,
  mpostnr varchar (10) null,
  mtelefon varchar (15) null,
  metternavn varchar (40) null,
  kundenr decimal(15,2) null,
  knavn varchar (40) null,
  kadresse1 varchar (40) null,
  kpostnr varchar (10) null,
  ktelefon varchar (15) null,
  selgernr decimal(15,2) null,
  bruktkassnr integer not null,
  bruktselgernr decimal(15,2) null,
  bruktkassenr integer not null,
  utgatt tinyint null,
  utgattdato datetime null,
  utgatttid integer null,
  utgattregav varchar (10) null,
  eget tinyint null,
  rabkr decimal(7,2) null,
  fakturert tinyint null,
  fakturertdato datetime null,
  fakturanr decimal(15,2) null,
  faktura_id decimal(15,2) not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_gavekort ON gavekort for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from gavekort t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX gavekort#_#progress_recid ON gavekort (PROGRESS_RECID)
go
CREATE UNIQUE INDEX gavekort#_#progress_recid_ident_ ON gavekort (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX gavekort##eget ON gavekort (eget, PROGRESS_RECID)
go
CREATE INDEX gavekort##faktdato ON gavekort (fakturertdato, PROGRESS_RECID)
go
CREATE INDEX gavekort##faktnr ON gavekort (fakturanr, PROGRESS_RECID)
go
CREATE INDEX gavekort##faktura_id ON gavekort (faktura_id, PROGRESS_RECID)
go
CREATE INDEX gavekort##fakturert ON gavekort (fakturert, PROGRESS_RECID)
go
CREATE UNIQUE INDEX gavekort##gavekortidx1 ON gavekort (butnr, identnr)
go
CREATE INDEX gavekort##gavekortidx2 ON gavekort (dato, tid, PROGRESS_RECID)
go
CREATE INDEX gavekort##gavekortidx3 ON gavekort (gyldigdato, PROGRESS_RECID)
go
CREATE INDEX gavekort##gavekortidx4 ON gavekort (bruktdato, brukttid, PROGRESS_RECID)
go
CREATE INDEX gavekort##gavekortidx6 ON gavekort (identtype, PROGRESS_RECID)
go
CREATE INDEX gavekort##identnr ON gavekort (identnr, PROGRESS_RECID)
go
CREATE INDEX gavekort##kadresse ON gavekort (kadresse1, PROGRESS_RECID)
go
CREATE INDEX gavekort##knavn ON gavekort (knavn, PROGRESS_RECID)
go
CREATE INDEX gavekort##ktelefon ON gavekort (ktelefon, PROGRESS_RECID)
go
CREATE INDEX gavekort##kundenr ON gavekort (kundenr, PROGRESS_RECID)
go
CREATE INDEX gavekort##madresse ON gavekort (madresse1, PROGRESS_RECID)
go
CREATE INDEX gavekort##medlemsnr ON gavekort (medlemsnr, PROGRESS_RECID)
go
CREATE INDEX gavekort##metternavn ON gavekort (metternavn, PROGRESS_RECID)
go
CREATE INDEX gavekort##mtelefon ON gavekort (mtelefon, PROGRESS_RECID)
go
CREATE INDEX gavekort##rabatt ON gavekort (rabkr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'gavektype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table gavektype
go
CREATE TABLE gavektype (
  identtype integer not null,
  gktbeskrivelse varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_gavektype ON gavektype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from gavektype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX gavektype#_#progress_recid ON gavektype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX gavektype#_#progress_recid_ident_ ON gavektype (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX gavektype##gavektype ON gavektype (identtype)
go
CREATE INDEX gavektype##gktbeskrivelse ON gavektype (gktbeskrivelse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'gruppe' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table gruppe
go
CREATE TABLE gruppe (
  butikknr integer not null,
  gruppenr integer not null,
  navn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_gruppe ON gruppe for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from gruppe t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX gruppe#_#progress_recid ON gruppe (PROGRESS_RECID)
go
CREATE UNIQUE INDEX gruppe#_#progress_recid_ident_ ON gruppe (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX gruppe##gruppe ON gruppe (butikknr, gruppenr)
go
CREATE INDEX gruppe##navn ON gruppe (butikknr, navn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'handtering' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table handtering
go
CREATE TABLE handtering (
  handkode integer null,
  beskrivelse varchar (40) null,
  notat varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_handtering ON handtering for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from handtering t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX handtering#_#progress_recid ON handtering (PROGRESS_RECID)
go
CREATE UNIQUE INDEX handtering#_#progress_recid_ident_ ON handtering (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX handtering##beskrivelse ON handtering (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX handtering##handtering ON handtering (handkode)
go
if (select name from sysobjects 
    where name = 'happyhourhode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table happyhourhode
go
CREATE TABLE happyhourhode (
  haphourid integer null,
  haphournavn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  haphournotat varchar (60) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_happyhourhode ON happyhourhode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from happyhourhode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX happyhourhode#_#progress_recid ON happyhourhode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX happyhourhode#_#progress_recid_ident_ ON happyhourhode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX happyhourhode##haphounavn ON happyhourhode (haphournavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX happyhourhode##happyhourhode ON happyhourhode (haphourid)
go
if (select name from sysobjects 
    where name = 'happyhourperiode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table happyhourperiode
go
CREATE TABLE happyhourperiode (
  haphourid integer null,
  haphourperid integer null,
  haphourperstarttid integer null,
  haphourperslutttid integer null,
  haphourperukedagliste varchar (15) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_happyhourperiode ON happyhourperiode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from happyhourperiode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX happyhourperiode#_#progress_recid ON happyhourperiode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX happyhourperiode#_#progress_recid_ident_ ON happyhourperiode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX happyhourperiode##haphourperid ON happyhourperiode (haphourperid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX happyhourperiode##happyhourperiode ON happyhourperiode (haphourid, haphourperid)
go
if (select name from sysobjects 
    where name = 'hjelpmap' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table hjelpmap
go
CREATE TABLE hjelpmap (
  ldnavn varchar (30) null,
  id varchar (15) null,
  beskrivelse varchar (40) null,
  contextnr varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_hjelpmap ON hjelpmap for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from hjelpmap t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX hjelpmap#_#progress_recid ON hjelpmap (PROGRESS_RECID)
go
CREATE UNIQUE INDEX hjelpmap#_#progress_recid_ident_ ON hjelpmap (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX hjelpmap##beskrivelse ON hjelpmap (ldnavn, PROGRESS_RECID)
go
CREATE INDEX hjelpmap##contextnr ON hjelpmap (ldnavn, contextnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX hjelpmap##hjelpmap ON hjelpmap (ldnavn, id)
go
if (select name from sysobjects 
    where name = 'hovedkategori' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table hovedkategori
go
CREATE TABLE hovedkategori (
  hovedkatnr integer null,
  hovedkattekst varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_hovedkategori ON hovedkategori for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from hovedkategori t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX hovedkategori#_#progress_recid ON hovedkategori (PROGRESS_RECID)
go
CREATE UNIQUE INDEX hovedkategori#_#progress_recid_ident_ ON hovedkategori (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX hovedkategori##hovedkategori ON hovedkategori (hovedkatnr)
go
CREATE INDEX hovedkategori##hovedkattekst ON hovedkategori (hovedkattekst, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'hprisko' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table hprisko
go
CREATE TABLE hprisko (
  endringsnr integer null,
  artikkelnr decimal(13,0) null,
  levnr integer null,
  valpris decimal(11,2) null,
  innkjopspris decimal(7,2) null,
  rab1kr decimal(7,2) null,
  rab1_ decimal(5,2) null,
  rab2kr decimal(7,2) null,
  rab2_ decimal(5,2) null,
  frakt decimal(8,2) null,
  frakt_ decimal(5,2) null,
  divkostkr decimal(7,2) null,
  divkost_ decimal(5,2) null,
  rab3kr decimal(7,2) null,
  rab3_ decimal(5,2) null,
  dbkr decimal(7,2) null,
  db_ decimal(7,2) null,
  pris decimal(7,2) null,
  europris decimal(7,2) null,
  euromanuel tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  tilbud tinyint null,
  aktiveresdato datetime null,
  gyldigtildato datetime null,
  aktiverestid integer null,
  gyldigtiltid integer null,
  timestyrt tinyint null,
  aktivert tinyint null,
  profilnr integer null,
  type integer not null,
  varekost decimal(9,2) null,
  mvakr decimal(9,2) null,
  mva_ decimal(5,2) null,
  momskod integer null,
  kampid decimal(13,0) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_hprisko ON hprisko for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from hprisko t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX hprisko#_#progress_recid ON hprisko (PROGRESS_RECID)
go
CREATE UNIQUE INDEX hprisko#_#progress_recid_ident_ ON hprisko (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX hprisko##hprisko ON hprisko (artikkelnr, profilnr, endringsnr DESC)
go
CREATE INDEX hprisko##kampid ON hprisko (kampid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ht_type' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ht_type
go
CREATE TABLE ht_type (
  typeid integer null,
  betegnelse varchar (30) null,
  notat varchar (30) null,
  importkatalog varchar (30) null,
  filprefix varchar (30) null,
  filekstent varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  eksportkatalog varchar (30) null,
  ekspfilprefix varchar (30) null,
  ekspfilekstent varchar (30) null,
  importprog varchar (20) null,
  eksportprog varchar (20) null,
  htaktiv tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ht_type ON ht_type for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ht_type t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ht_type#_#progress_recid ON ht_type (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ht_type#_#progress_recid_ident_ ON ht_type (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX ht_type##betegnelse ON ht_type (betegnelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX ht_type##ht_type ON ht_type (typeid)
go
CREATE INDEX ht_type##htaktiv ON ht_type (htaktiv, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'huvgr' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table huvgr
go
CREATE TABLE huvgr (
  hg integer null,
  hgbeskr varchar (30) null,
  avdelingnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_huvgr ON huvgr for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from huvgr t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX huvgr#_#progress_recid ON huvgr (PROGRESS_RECID)
go
CREATE UNIQUE INDEX huvgr#_#progress_recid_ident_ ON huvgr (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX huvgr##avdelingnr ON huvgr (avdelingnr, PROGRESS_RECID)
go
CREATE INDEX huvgr##hgbeskr ON huvgr (hgbeskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX huvgr##huvgrin ON huvgr (hg)
go
if (select name from sysobjects 
    where name = 'imphode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table imphode
go
CREATE TABLE imphode (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  edb_system varchar (12) null,
  systembeskrivelse varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_imphode ON imphode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from imphode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX imphode#_#progress_recid ON imphode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX imphode#_#progress_recid_ident_ ON imphode (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX imphode##imphode ON imphode (edb_system)
go
CREATE INDEX imphode##systembeskrivelse ON imphode (systembeskrivelse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'impkonv' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table impkonv
go
CREATE TABLE impkonv (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  edb_system varchar (12) null,
  tabell varchar (12) null,
  interntid varchar (12) null,
  eksterntid varchar (12) null,
  merknad varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_impkonv ON impkonv for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from impkonv t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX impkonv#_#progress_recid ON impkonv (PROGRESS_RECID)
go
CREATE UNIQUE INDEX impkonv#_#progress_recid_ident_ ON impkonv (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX impkonv##eksterntid ON impkonv (edb_system, tabell, eksterntid, PROGRESS_RECID)
go
CREATE INDEX impkonv##impkonv ON impkonv (edb_system, tabell, interntid, PROGRESS_RECID)
go
CREATE INDEX impkonv##merknad ON impkonv (edb_system, tabell, merknad, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'individ' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table individ
go
CREATE TABLE individ (
  butnr integer not null,
  ean decimal(13,0) not null,
  dato datetime not null,
  individnr decimal(12,0) not null,
  serienr varchar (20) null,
  kordnr integer not null,
  kradnr integer not null,
  kundenr integer not null,
  salgdato datetime null,
  feilvare tinyint not null,
  individtekst varchar (1000) null,
  rapportert tinyint not null,
  levnr integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  levnamn varchar (30) null,
  kkundenr decimal(15,2) null,
  adresse1 varchar (40) null,
  adresse2 varchar (40) null,
  postnr varchar (10) null,
  telefon varchar (15) null,
  mobiltlf varchar (15) null,
  nyvare tinyint null,
  sadresse2 varchar (40) null,
  spostnr varchar (10) null,
  stelefon varchar (15) null,
  smobiltlf varchar (15) null,
  navn varchar (40) null,
  snavn varchar (40) null,
  vapenkort varchar (25) null,
  jegerkort varchar (25) null,
  kjoptdato datetime null,
  selgernr decimal(15,2) null,
  forsnr integer null,
  persorgnr varchar (25) null,
  vmid integer null,
  vmbeskrivelse varchar (30) null,
  kaliber varchar (12) null,
  garantinummer varchar (25) null,
  vvarekost decimal(11,4) null,
  pris decimal(7,2) null,
  varekost decimal(9,2) null,
  dbkr decimal(7,2) null,
  db_ decimal(7,2) null,
  vdbkr decimal(7,2) null,
  vdb_ decimal(7,2) null,
  poststed varchar (30) null,
  spoststed varchar (30) null,
  sadresse1 varchar (40) null,
  storl varchar (10) null,
  bruktvarekost decimal(9,2) null,
  individtype integer null,
  hg integer null,
  vg integer null,
  artikkelnr decimal(13,0) null,
  avdelingnr integer null,
  beskr varchar (20) null,
  strkode integer null,
  epostadresse varchar (40) null,
  seqnr decimal(11,2) null,
  b_id decimal(15,2) null,
  batchnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_individ ON individ for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from individ t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX individ#_#progress_recid ON individ (PROGRESS_RECID)
go
CREATE UNIQUE INDEX individ#_#progress_recid_ident_ ON individ (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX individ##adresse ON individ (adresse1, PROGRESS_RECID)
go
CREATE INDEX individ##artikkelnr ON individ (artikkelnr, PROGRESS_RECID)
go
CREATE INDEX individ##avdeling ON individ (avdelingnr, PROGRESS_RECID)
go
CREATE INDEX individ##batchnr ON individ (batchnr, PROGRESS_RECID)
go
CREATE INDEX individ##beskr ON individ (beskr, PROGRESS_RECID)
go
CREATE INDEX individ##garantinummer ON individ (garantinummer, PROGRESS_RECID)
go
CREATE INDEX individ##hg ON individ (hg, PROGRESS_RECID)
go
CREATE UNIQUE INDEX individ##individidx1 ON individ (individnr)
go
CREATE INDEX individ##individidx4 ON individ (serienr, PROGRESS_RECID)
go
CREATE INDEX individ##individidx5 ON individ (dato, PROGRESS_RECID)
go
CREATE INDEX individ##individidx6 ON individ (kordnr, kradnr, PROGRESS_RECID)
go
CREATE INDEX individ##individidx7 ON individ (kundenr, PROGRESS_RECID)
go
CREATE INDEX individ##individidx8 ON individ (levnr, PROGRESS_RECID)
go
CREATE INDEX individ##individtype ON individ (individtype, PROGRESS_RECID)
go
CREATE INDEX individ##jegerkort ON individ (jegerkort, PROGRESS_RECID)
go
CREATE INDEX individ##kasseoppslag ON individ (artikkelnr, individnr, strkode, PROGRESS_RECID)
go
CREATE INDEX individ##kkundenr ON individ (kkundenr, PROGRESS_RECID)
go
CREATE INDEX individ##mobilnr ON individ (mobiltlf, PROGRESS_RECID)
go
CREATE INDEX individ##navn ON individ (navn, PROGRESS_RECID)
go
CREATE INDEX individ##sadresse ON individ (sadresse1, PROGRESS_RECID)
go
CREATE UNIQUE INDEX individ##seqnr ON individ (butnr, seqnr)
go
CREATE INDEX individ##smobil ON individ (smobiltlf, PROGRESS_RECID)
go
CREATE INDEX individ##snavn ON individ (snavn, PROGRESS_RECID)
go
CREATE INDEX individ##stelefon ON individ (stelefon, PROGRESS_RECID)
go
CREATE INDEX individ##telefon ON individ (telefon, PROGRESS_RECID)
go
CREATE INDEX individ##vapenkort ON individ (vapenkort, PROGRESS_RECID)
go
CREATE INDEX individ##vg ON individ (vg, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'indtype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table indtype
go
CREATE TABLE indtype (
  individtype integer null,
  individbeskr varchar (30) null,
  serienrreg integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_indtype ON indtype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from indtype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX indtype#_#progress_recid ON indtype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX indtype#_#progress_recid_ident_ ON indtype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX indtype##beskrivelse ON indtype (individbeskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX indtype##indtype ON indtype (individtype)
go
if (select name from sysobjects 
    where name = 'innbettype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table innbettype
go
CREATE TABLE innbettype (
  innbettid integer null,
  innbbeskrivelse varchar (30) null,
  kontonr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_innbettype ON innbettype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from innbettype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX innbettype#_#progress_recid ON innbettype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX innbettype#_#progress_recid_ident_ ON innbettype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX innbettype##innbbeskrivelse ON innbettype (innbbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX innbettype##innbettype ON innbettype (innbettid)
go
CREATE INDEX innbettype##kontonr ON innbettype (kontonr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'innersula' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table innersula
go
CREATE TABLE innersula (
  inner_id integer null,
  innerbeskr varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_innersula ON innersula for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from innersula t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX innersula#_#progress_recid ON innersula (PROGRESS_RECID)
go
CREATE UNIQUE INDEX innersula#_#progress_recid_ident_ ON innersula (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX innersula##inners_beskr ON innersula (innerbeskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX innersula##innersula ON innersula (inner_id)
go
if (select name from sysobjects 
    where name = 'innkjopsgrupper' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table innkjopsgrupper
go
CREATE TABLE innkjopsgrupper (
  innkjkode integer null,
  innkjbeskr varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_innkjopsgrupper ON innkjopsgrupper for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from innkjopsgrupper t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX innkjopsgrupper#_#progress_recid ON innkjopsgrupper (PROGRESS_RECID)
go
CREATE UNIQUE INDEX innkjopsgrupper#_#progress_recid_ident_ ON innkjopsgrupper (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX innkjopsgrupper##innkjbeskr ON innkjopsgrupper (innkjbeskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX innkjopsgrupper##innkjkode ON innkjopsgrupper (innkjkode)
go
if (select name from sysobjects 
    where name = 'innloser' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table innloser
go
CREATE TABLE innloser (
  innloserid varchar (10) null,
  innlosernavn varchar (30) null,
  spesifiseres tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  royalty_ decimal(7,2) null,
  kontonr integer null,
  kortnavn varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_innloser ON innloser for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from innloser t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX innloser#_#progress_recid ON innloser (PROGRESS_RECID)
go
CREATE UNIQUE INDEX innloser#_#progress_recid_ident_ ON innloser (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX innloser##innloserid ON innloser (innloserid)
go
CREATE INDEX innloser##innlosernavn ON innloser (innlosernavn, PROGRESS_RECID)
go
CREATE INDEX innloser##kontonr ON innloser (kontonr, PROGRESS_RECID)
go
CREATE INDEX innloser##kortnavn ON innloser (kortnavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jamforenhet' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jamforenhet
go
CREATE TABLE jamforenhet (
  jamforenhid integer null,
  jamforenhet varchar (4) null,
  jamforenhtekst varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jamforenhet ON jamforenhet for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jamforenhet t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jamforenhet#_#progress_recid ON jamforenhet (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jamforenhet#_#progress_recid_ident_ ON jamforenhet (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jamforenhet##jamforenhet ON jamforenhet (jamforenhet, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jamforenhet##jamforenhid ON jamforenhet (jamforenhid)
go
CREATE INDEX jamforenhet##jamforenhtekst ON jamforenhet (jamforenhtekst, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxappmodule' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxappmodule
go
CREATE TABLE jboxappmodule (
  ijboxappmoduleid integer null,
  cmodulename varchar (40) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxappmodule ON jboxappmodule for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxappmodule t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxappmodule#_#progress_recid ON jboxappmodule (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxappmodule#_#progress_recid_ident_ ON jboxappmodule (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxappmodule##idxappmoduleid ON jboxappmodule (ijboxappmoduleid)
go
if (select name from sysobjects 
    where name = 'jboxappmodulecompany' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxappmodulecompany
go
CREATE TABLE jboxappmodulecompany (
  ijboxappmoduleid integer null,
  ijboxcompanyid integer null,
  cconfigparam1 varchar (80) null,
  cconfigparam2 varchar (80) null,
  cconfigparam3 varchar (80) null,
  cconfigparam4 varchar (80) null,
  cconfigparam5 varchar (80) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxappmodulecompany ON jboxappmodulecompany for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxappmodulecompany t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxappmodulecompany#_#progress_recid ON jboxappmodulecompany (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxappmodulecompany#_#progress_recid_ident_ ON jboxappmodulecompany (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxappmodulecompany##idxappmodulecomp ON jboxappmodulecompany (ijboxappmoduleid, ijboxcompanyid)
go
CREATE INDEX jboxappmodulecompany##idxcompany ON jboxappmodulecompany (ijboxcompanyid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxappmoduleitem' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxappmoduleitem
go
CREATE TABLE jboxappmoduleitem (
  ijboxappmoduleitemid integer null,
  ijboxappmoduleid integer null,
  ijboxmenuid integer null,
  ijboxappprogramid integer null,
  ijboxcompanyid integer null,
  cconfigparam1 varchar (80) null,
  cconfigparam2 varchar (80) null,
  cconfigparam3 varchar (80) null,
  cconfigparam4 varchar (80) null,
  cconfigparam5 varchar (80) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxappmoduleitem ON jboxappmoduleitem for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxappmoduleitem t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxappmoduleitem#_#progress_recid ON jboxappmoduleitem (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxappmoduleitem#_#progress_recid_ident_ ON jboxappmoduleitem (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxappmoduleitem##idxappmodule ON jboxappmoduleitem (ijboxappmoduleid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxappmoduleitem##idxappmoduleitem ON jboxappmoduleitem (ijboxappmoduleitemid)
go
CREATE INDEX jboxappmoduleitem##idxappprogramid ON jboxappmoduleitem (ijboxappprogramid, PROGRESS_RECID)
go
CREATE INDEX jboxappmoduleitem##idxcompany ON jboxappmoduleitem (ijboxcompanyid, PROGRESS_RECID)
go
CREATE INDEX jboxappmoduleitem##idxmenu ON jboxappmoduleitem (ijboxmenuid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxappprogram' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxappprogram
go
CREATE TABLE jboxappprogram (
  ijboxappprogramid integer null,
  cfilename varchar (40) null,
  cfiletype varchar (15) null,
  ialtappprogramid integer null,
  cdescription varchar (60) null,
  cconfigparam1 varchar (80) null,
  cconfigparam2 varchar (80) null,
  cconfigparam3 varchar (80) null,
  cconfigparam4 varchar (80) null,
  cconfigparam5 varchar (80) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxappprogram ON jboxappprogram for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxappprogram t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxappprogram#_#progress_recid ON jboxappprogram (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxappprogram#_#progress_recid_ident_ ON jboxappprogram (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxappprogram##idxfilename ON jboxappprogram (cfilename, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxappprogram##seqprogramid ON jboxappprogram (ijboxappprogramid)
go
if (select name from sysobjects 
    where name = 'jboxcompany' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxcompany
go
CREATE TABLE jboxcompany (
  ijboxcompanyid integer null,
  ccompanyname varchar (40) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  cemail varchar (60) null,
  cemail2 varchar (60) null,
  ctlfmob varchar (15) null,
  ctlfhome varchar (15) null,
  ctlfwrk varchar (15) null,
  cfax varchar (15) null,
  curl varchar (40) null,
  curl2 varchar (40) null,
  caddress1 varchar (50) null,
  caddress2 varchar (40) null,
  ccity varchar (40) null,
  cstate varchar (20) null,
  cpostalcode varchar (30) null,
  ccountry varchar (40) null,
  cl integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxcompany ON jboxcompany for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxcompany t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxcompany#_#progress_recid ON jboxcompany (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxcompany#_#progress_recid_ident_ ON jboxcompany (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxcompany##idxcl ON jboxcompany (cl, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxcompany##idxcompanyid ON jboxcompany (ijboxcompanyid)
go
if (select name from sysobjects 
    where name = 'jboxcompanyuser' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxcompanyuser
go
CREATE TABLE jboxcompanyuser (
  ijboxcompanyid integer null,
  cjboxuserid varchar (40) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  bsuperusercompany tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxcompanyuser ON jboxcompanyuser for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxcompanyuser t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxcompanyuser#_#progress_recid ON jboxcompanyuser (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxcompanyuser#_#progress_recid_ident_ ON jboxcompanyuser (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxcompanyuser##idxjboxcompanyuser ON jboxcompanyuser (ijboxcompanyid, cjboxuserid)
go
CREATE INDEX jboxcompanyuser##idxuser ON jboxcompanyuser (cjboxuserid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxeventcategory' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxeventcategory
go
CREATE TABLE jboxeventcategory (
  eventcategoryobj decimal(12,2) null,
  eventcategoryname varchar (50) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxeventcategory ON jboxeventcategory for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxeventcategory t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxeventcategory#_#progress_recid ON jboxeventcategory (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxeventcategory#_#progress_recid_ident_ ON jboxeventcategory (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxeventcategory##eventcategoryobj ON jboxeventcategory (eventcategoryobj)
go
if (select name from sysobjects 
    where name = 'jboxeventgroup' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxeventgroup
go
CREATE TABLE jboxeventgroup (
  eventgroupobj decimal(12,2) null,
  eventgroupname varchar (50) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxeventgroup ON jboxeventgroup for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxeventgroup t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxeventgroup#_#progress_recid ON jboxeventgroup (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxeventgroup#_#progress_recid_ident_ ON jboxeventgroup (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxeventgroup##eventgroupobj ON jboxeventgroup (eventgroupobj)
go
if (select name from sysobjects 
    where name = 'jboxeventline' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxeventline
go
CREATE TABLE jboxeventline (
  eventlineobj decimal(12,2) null,
  eventlinedescription varchar (50) null,
  eventlinedate datetime null,
  eventlinetime integer null,
  eventlineid integer null,
  eventcategoryobj decimal(12,2) null,
  eventgroupobj decimal(12,2) null,
  eventsourceobj decimal(12,2) null,
  eventtypeobj decimal(12,2) null,
  ijboxcompanyid integer null,
  cjboxuserid varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxeventline ON jboxeventline for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxeventline t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxeventline#_#progress_recid ON jboxeventline (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxeventline#_#progress_recid_ident_ ON jboxeventline (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxeventline##cjboxuserid ON jboxeventline (cjboxuserid, PROGRESS_RECID)
go
CREATE INDEX jboxeventline##eventcategoryobj ON jboxeventline (eventcategoryobj, PROGRESS_RECID)
go
CREATE INDEX jboxeventline##eventgroupobj ON jboxeventline (eventgroupobj, PROGRESS_RECID)
go
CREATE INDEX jboxeventline##eventlinedate ON jboxeventline (eventlinedate, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxeventline##eventlineobj ON jboxeventline (eventlineobj)
go
CREATE INDEX jboxeventline##eventlinetime ON jboxeventline (eventlinetime, PROGRESS_RECID)
go
CREATE INDEX jboxeventline##eventsourceobj ON jboxeventline (eventsourceobj, PROGRESS_RECID)
go
CREATE INDEX jboxeventline##eventtypeobj ON jboxeventline (eventtypeobj, PROGRESS_RECID)
go
CREATE INDEX jboxeventline##ijboxcompanyid ON jboxeventline (ijboxcompanyid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxeventsource' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxeventsource
go
CREATE TABLE jboxeventsource (
  eventsourceobj decimal(12,2) null,
  eventsourcename varchar (50) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxeventsource ON jboxeventsource for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxeventsource t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxeventsource#_#progress_recid ON jboxeventsource (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxeventsource#_#progress_recid_ident_ ON jboxeventsource (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxeventsource##eventsrouceobj ON jboxeventsource (eventsourceobj)
go
if (select name from sysobjects 
    where name = 'jboxeventtype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxeventtype
go
CREATE TABLE jboxeventtype (
  eventtypeobj decimal(12,2) null,
  eventtypename varchar (50) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxeventtype ON jboxeventtype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxeventtype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxeventtype#_#progress_recid ON jboxeventtype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxeventtype#_#progress_recid_ident_ ON jboxeventtype (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxeventtype##eventtypeobj ON jboxeventtype (eventtypeobj)
go
if (select name from sysobjects 
    where name = 'jboxfunction' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxfunction
go
CREATE TABLE jboxfunction (
  ijboxfunctionid integer null,
  cclientfilename varchar (40) null,
  cserverfilename varchar (40) null,
  cobjectname varchar (30) null,
  caction varchar (30) null,
  bmenu tinyint null,
  cdescription varchar (50) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxfunction ON jboxfunction for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxfunction t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxfunction#_#progress_recid ON jboxfunction (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxfunction#_#progress_recid_ident_ ON jboxfunction (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxfunction##idxfunctionid ON jboxfunction (ijboxfunctionid)
go
if (select name from sysobjects 
    where name = 'jboxfunctionaccess' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxfunctionaccess
go
CREATE TABLE jboxfunctionaccess (
  ijboxfunctionaccessid integer null,
  ijboxfunctionid integer null,
  ijboxusergroupid integer null,
  cjboxuserid varchar (40) null,
  ijboxcompanyid integer null,
  caccess varchar (12) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxfunctionaccess ON jboxfunctionaccess for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxfunctionaccess t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxfunctionaccess#_#progress_recid ON jboxfunctionaccess (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxfunctionaccess#_#progress_recid_ident_ ON jboxfunctionaccess (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxfunctionaccess##idxcompany ON jboxfunctionaccess (ijboxfunctionaccessid, PROGRESS_RECID)
go
CREATE INDEX jboxfunctionaccess##idxfunction ON jboxfunctionaccess (ijboxfunctionid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxfunctionaccess##idxfunctionacces ON jboxfunctionaccess (ijboxfunctionaccessid)
go
CREATE INDEX jboxfunctionaccess##idxusergroup ON jboxfunctionaccess (ijboxusergroupid, PROGRESS_RECID)
go
CREATE INDEX jboxfunctionaccess##idxuserid ON jboxfunctionaccess (cjboxuserid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxgencode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxgencode
go
CREATE TABLE jboxgencode (
  ccodevalue varchar (40) null,
  cdescription varchar (50) null,
  clanguage varchar (3) null,
  ccodetype varchar (30) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  ijboxcompanyid integer null,
  ijboxgencodeid integer null,
  cmisc1 varchar (20) null,
  cmisc2 varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxgencode ON jboxgencode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxgencode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxgencode#_#progress_recid ON jboxgencode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxgencode#_#progress_recid_ident_ ON jboxgencode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxgencode##idxcodetype ON jboxgencode (ccodetype, PROGRESS_RECID)
go
CREATE INDEX jboxgencode##idxcodevalue ON jboxgencode (ccodevalue, PROGRESS_RECID)
go
CREATE INDEX jboxgencode##idxcompanyid ON jboxgencode (ijboxcompanyid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxgencode##idxjboxgencodeid ON jboxgencode (ijboxgencodeid)
go
CREATE INDEX jboxgencode##idxlanguage ON jboxgencode (clanguage, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxgencodetype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxgencodetype
go
CREATE TABLE jboxgencodetype (
  ijboxgencodetypeid integer null,
  ccodetype varchar (30) null,
  cdescription varchar (50) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxgencodetype ON jboxgencodetype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxgencodetype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxgencodetype#_#progress_recid ON jboxgencodetype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxgencodetype#_#progress_recid_ident_ ON jboxgencodetype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxgencodetype##idxcodetype ON jboxgencodetype (ccodetype, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxgencodetype##idxjboxgencodetypeid ON jboxgencodetype (ijboxgencodetypeid)
go
if (select name from sysobjects 
    where name = 'jboxloginsession' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxloginsession
go
CREATE TABLE jboxloginsession (
  csessionid varchar (40) null,
  ijboxcompanyid integer null,
  ccontext varchar (40) null,
  cjboxuserid varchar (30) null,
  dcreated datetime null,
  cdateformat varchar (30) null,
  cnumformat varchar (12) null,
  clanguage varchar (2) null,
  ilogintime integer null,
  ilogouttime integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxloginsession ON jboxloginsession for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxloginsession t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxloginsession#_#progress_recid ON jboxloginsession (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxloginsession#_#progress_recid_ident_ ON jboxloginsession (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxloginsession##idxcreated ON jboxloginsession (dcreated, PROGRESS_RECID)
go
CREATE INDEX jboxloginsession##idxsessionid ON jboxloginsession (csessionid, PROGRESS_RECID)
go
CREATE INDEX jboxloginsession##idxuserid ON jboxloginsession (cjboxuserid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxmenu' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxmenu
go
CREATE TABLE jboxmenu (
  ijboxmenuid integer null,
  cmenutype varchar (12) null,
  cmenulabel varchar (40) null,
  ijboxprogramid integer null,
  claunch varchar (40) null,
  claunchtype varchar (20) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  caccelerator varchar (30) null,
  bconfigurable tinyint null,
  cimage varchar (30) null,
  cselimage varchar (30) null,
  cstateimage varchar (30) null,
  ctextcolor varchar (10) null,
  cfontstyle varchar (30) null,
  cmenutooltip varchar (40) null,
  cmenunumber varchar (3) null,
  itvnavbarstyle integer null,
  iimagesize integer null,
  iselimagesize integer null,
  istateimagesize integer null,
  cparameter varchar (20) null,
  blogexec tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxmenu ON jboxmenu for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxmenu t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxmenu#_#progress_recid ON jboxmenu (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxmenu#_#progress_recid_ident_ ON jboxmenu (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxmenu##idxmenuid ON jboxmenu (ijboxmenuid)
go
CREATE INDEX jboxmenu##idxprogramid ON jboxmenu (ijboxprogramid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxmenufavorites' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxmenufavorites
go
CREATE TABLE jboxmenufavorites (
  ijboxmenufavoritesid integer null,
  ijboxmenuid integer null,
  iseq integer null,
  cjboxuserid varchar (40) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  ctype varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxmenufavorites ON jboxmenufavorites for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxmenufavorites t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxmenufavorites#_#progress_recid ON jboxmenufavorites (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxmenufavorites#_#progress_recid_ident_ ON jboxmenufavorites (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxmenufavorites##idxmenufavouritesid ON jboxmenufavorites (ijboxmenufavoritesid)
go
CREATE INDEX jboxmenufavorites##idxmenuuser ON jboxmenufavorites (ijboxmenuid, cjboxuserid, PROGRESS_RECID)
go
CREATE INDEX jboxmenufavorites##idxuserseq ON jboxmenufavorites (cjboxuserid, iseq, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxmenutomenu' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxmenutomenu
go
CREATE TABLE jboxmenutomenu (
  itomenuid integer null,
  ifrommenuid integer null,
  iseq integer null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  cjboxuserid varchar (30) null,
  ijboxusergroupid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxmenutomenu ON jboxmenutomenu for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxmenutomenu t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxmenutomenu#_#progress_recid ON jboxmenutomenu (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxmenutomenu#_#progress_recid_ident_ ON jboxmenutomenu (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxmenutomenu##idxfrommenu ON jboxmenutomenu (ifrommenuid, PROGRESS_RECID)
go
CREATE INDEX jboxmenutomenu##idxseq ON jboxmenutomenu (itomenuid, iseq, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxmenutomenu##idxtofrommenu ON jboxmenutomenu (itomenuid, ifrommenuid)
go
if (select name from sysobjects 
    where name = 'jboxmenutranslation' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxmenutranslation
go
CREATE TABLE jboxmenutranslation (
  ijboxmenuid integer null,
  clanguage varchar (2) null,
  cmenulabel varchar (40) null,
  cmenutooltip varchar (80) null,
  caccelerator varchar (30) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxmenutranslation ON jboxmenutranslation for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxmenutranslation t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxmenutranslation#_#progress_recid ON jboxmenutranslation (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxmenutranslation#_#progress_recid_ident_ ON jboxmenutranslation (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxmenutranslation##idxlanguagemenuid ON jboxmenutranslation (clanguage, ijboxmenuid)
go
CREATE INDEX jboxmenutranslation##idxmenuid ON jboxmenutranslation (ijboxmenuid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxqcriteria' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxqcriteria
go
CREATE TABLE jboxqcriteria (
  ientityid integer not null,
  isequence integer null,
  candor varchar (30) null,
  cleftpar varchar (30) null,
  coper varchar (30) not null,
  cvalue varchar (40) not null,
  crightpar varchar (30) null,
  fvalue decimal(10,2) null,
  dvalue datetime null,
  bvalue tinyint null,
  ivalue integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxqcriteria ON jboxqcriteria for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxqcriteria t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxqcriteria#_#progress_recid ON jboxqcriteria (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxqcriteria#_#progress_recid_ident_ ON jboxqcriteria (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxqcriteria##idxcriteria ON jboxqcriteria (ientityid, isequence)
go
if (select name from sysobjects 
    where name = 'jboxqentity' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxqentity
go
CREATE TABLE jboxqentity (
  icompanyid integer null,
  ientityid integer not null,
  cname varchar (20) null,
  cdesc varchar (60) null,
  ctype varchar (30) null,
  cquerycat varchar (30) null,
  cdbtable varchar (30) null,
  cdbfield varchar (30) null,
  caccess varchar (240) null,
  cowner varchar (20) null,
  cdatatype varchar (9) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxqentity ON jboxqentity for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxqentity t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxqentity#_#progress_recid ON jboxqentity (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxqentity#_#progress_recid_ident_ ON jboxqentity (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxqentity##idxcompanyid ON jboxqentity (icompanyid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxqentity##idxentityid ON jboxqentity (ientityid)
go
if (select name from sysobjects 
    where name = 'jboxqforeignkey' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxqforeignkey
go
CREATE TABLE jboxqforeignkey (
  cname varchar (20) null,
  cdbtable varchar (30) null,
  cdbfield varchar (30) null,
  creldbtable varchar (30) null,
  creldbfield varchar (30) null,
  crelation varchar (240) null,
  cviewfields varchar (100) null,
  ientityid integer not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxqforeignkey ON jboxqforeignkey for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxqforeignkey t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxqforeignkey#_#progress_recid ON jboxqforeignkey (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxqforeignkey#_#progress_recid_ident_ ON jboxqforeignkey (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxqforeignkey##idxforeignkey ON jboxqforeignkey (cdbtable, cdbfield, ientityid)
go
if (select name from sysobjects 
    where name = 'jboxqlog' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxqlog
go
CREATE TABLE jboxqlog (
  ientityid integer not null,
  ilogid integer not null,
  drundate datetime not null,
  cuserid varchar (20) null,
  itime integer not null,
  inumsec integer null,
  icountrec integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxqlog ON jboxqlog for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxqlog t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxqlog#_#progress_recid ON jboxqlog (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxqlog#_#progress_recid_ident_ ON jboxqlog (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxqlog##idxqlog ON jboxqlog (ientityid, ilogid DESC)
go
if (select name from sysobjects 
    where name = 'jboxqrelation' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxqrelation
go
CREATE TABLE jboxqrelation (
  ientityid integer not null,
  ientityin integer not null,
  crelation varchar (240) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxqrelation ON jboxqrelation for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxqrelation t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxqrelation#_#progress_recid ON jboxqrelation (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxqrelation#_#progress_recid_ident_ ON jboxqrelation (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxqrelation##idxrelation ON jboxqrelation (ientityid, ientityin)
go
if (select name from sysobjects 
    where name = 'jboxtranslation' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxtranslation
go
CREATE TABLE jboxtranslation (
  ijboxtranslationid integer null,
  cobjectname varchar (40) null,
  cobjecttype varchar (40) null,
  clabel varchar (40) null,
  ccolumnlabel varchar (40) null,
  ctooltip varchar (100) null,
  chelptext varchar (100) null,
  cinitvalue varchar (100) null,
  chelptag varchar (60) null,
  clanguage varchar (2) null,
  cfilename varchar (40) null,
  imsgno integer null,
  cmessage varchar (320) null,
  imsgdatapos integer null,
  ctitle varchar (100) null,
  imsgbuttontype integer null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxtranslation ON jboxtranslation for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxtranslation t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxtranslation#_#progress_recid ON jboxtranslation (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxtranslation#_#progress_recid_ident_ ON jboxtranslation (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxtranslation##idxfilename ON jboxtranslation (cfilename, PROGRESS_RECID)
go
CREATE INDEX jboxtranslation##idxlanguage ON jboxtranslation (clanguage, PROGRESS_RECID)
go
CREATE INDEX jboxtranslation##idxmsgno ON jboxtranslation (imsgno, PROGRESS_RECID)
go
CREATE INDEX jboxtranslation##idxobjectname ON jboxtranslation (cobjectname, PROGRESS_RECID)
go
CREATE INDEX jboxtranslation##idxobjecttype ON jboxtranslation (cobjecttype, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxtranslation##idxtranslationid ON jboxtranslation (ijboxtranslationid)
go
if (select name from sysobjects 
    where name = 'jboxuser' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxuser
go
CREATE TABLE jboxuser (
  cjboxuserid varchar (40) null,
  cusername varchar (40) null,
  bsuperuser tinyint null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  cemail varchar (40) null,
  cwrkphone varchar (15) null,
  ccellphone varchar (15) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxuser ON jboxuser for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxuser t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxuser#_#progress_recid ON jboxuser (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxuser#_#progress_recid_ident_ ON jboxuser (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxuser##idxuserid ON jboxuser (cjboxuserid)
go
if (select name from sysobjects 
    where name = 'jboxusergroup' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxusergroup
go
CREATE TABLE jboxusergroup (
  ijboxusergroupid integer null,
  cusergroupname varchar (30) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxusergroup ON jboxusergroup for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxusergroup t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxusergroup#_#progress_recid ON jboxusergroup (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxusergroup#_#progress_recid_ident_ ON jboxusergroup (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxusergroup##idxusergroupid ON jboxusergroup (ijboxusergroupid)
go
if (select name from sysobjects 
    where name = 'jboxusergroupmembers' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxusergroupmembers
go
CREATE TABLE jboxusergroupmembers (
  ijboxusergroupmembersid integer null,
  ijboxusergroupid integer null,
  cjboxuserid varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxusergroupmembers ON jboxusergroupmembers for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxusergroupmembers t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxusergroupmembers#_#progress_recid ON jboxusergroupmembers (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxusergroupmembers#_#progress_recid_ident_ ON jboxusergroupmembers (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jboxusergroupmembers##idxusergroupmemberid ON jboxusergroupmembers (ijboxusergroupmembersid)
go
CREATE UNIQUE INDEX jboxusergroupmembers##idxusergroupmembers ON jboxusergroupmembers (ijboxusergroupid, cjboxuserid)
go
CREATE INDEX jboxusergroupmembers##idxuserid ON jboxusergroupmembers (cjboxuserid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jboxusermenu' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxusermenu
go
CREATE TABLE jboxusermenu (
  ijboxmenuid integer null,
  ijboxusergroupid integer null,
  cjboxuserid varchar (40) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  ijboxusermenuid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxusermenu ON jboxusermenu for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxusermenu t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxusermenu#_#progress_recid ON jboxusermenu (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxusermenu#_#progress_recid_ident_ ON jboxusermenu (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxusermenu##idxmenuid ON jboxusermenu (ijboxmenuid, PROGRESS_RECID)
go
CREATE INDEX jboxusermenu##idxusergroupid ON jboxusermenu (ijboxusergroupid, PROGRESS_RECID)
go
CREATE INDEX jboxusermenu##idxuserid ON jboxusermenu (cjboxuserid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxusermenu##idxusermenuid ON jboxusermenu (ijboxusermenuid)
go
if (select name from sysobjects 
    where name = 'jboxusersetting' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jboxusersetting
go
CREATE TABLE jboxusersetting (
  ijboxusersettingid integer null,
  cjboxuserid varchar (30) null,
  csourcefile varchar (30) null,
  cobjectname varchar (30) null,
  ccontext varchar (30) null,
  csettingname varchar (30) null,
  csetting varchar (50) null,
  dcreated datetime null,
  ccreatedby varchar (30) null,
  dmodified datetime null,
  cmodifiedby varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jboxusersetting ON jboxusersetting for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jboxusersetting t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jboxusersetting#_#progress_recid ON jboxusersetting (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxusersetting#_#progress_recid_ident_ ON jboxusersetting (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jboxusersetting##idxsetting ON jboxusersetting (csourcefile, cobjectname, ccontext, csettingname, cjboxuserid, PROGRESS_RECID)
go
CREATE INDEX jboxusersetting##idxuserid ON jboxusersetting (cjboxuserid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jboxusersetting##idxusersettingid ON jboxusersetting (ijboxusersettingid)
go
if (select name from sysobjects 
    where name = 'jobb' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jobb
go
CREATE TABLE jobb (
  jobbnr integer not null,
  bestiltav varchar (20) null,
  bestillingsdato datetime null,
  bestillingstid integer null,
  startdato datetime null,
  starttid integer null,
  ferdigdato datetime null,
  ferdigtid integer null,
  antstart integer null,
  jobbstatus varchar (30) null,
  startprogram varchar (15) null,
  eksekprogram varchar (15) null,
  startesav varchar (1) null,
  kriterier varchar (60) null,
  merknad varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jobb ON jobb for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jobb t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jobb#_#progress_recid ON jobb (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jobb#_#progress_recid_ident_ ON jobb (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX jobb##brukerdatojobb ON jobb (bestiltav, bestillingsdato, jobbnr, PROGRESS_RECID)
go
CREATE INDEX jobb##brukerjobb ON jobb (bestiltav, jobbnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX jobb##jobbnr ON jobb (jobbnr)
go
if (select name from sysobjects 
    where name = 'kampanjebutikker' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kampanjebutikker
go
CREATE TABLE kampanjebutikker (
  kampid decimal(15,2) null,
  butik integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  mottattdato datetime null,
  mottattkl varchar (10) null,
  resultat varchar (10) null,
  melding varchar (50) null,
  sendtdato datetime null,
  sendttid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kampanjebutikker ON kampanjebutikker for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kampanjebutikker t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kampanjebutikker#_#progress_recid ON kampanjebutikker (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjebutikker#_#progress_recid_ident_ ON kampanjebutikker (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kampanjebutikker##kampanjebutikker ON kampanjebutikker (kampid, butik)
go
if (select name from sysobjects 
    where name = 'kampanjebutkobling' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kampanjebutkobling
go
CREATE TABLE kampanjebutkobling (
  kampanjeid bigint null,
  butikknr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kampanjebutkobling ON kampanjebutkobling for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kampanjebutkobling t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kampanjebutkobling#_#progress_recid ON kampanjebutkobling (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjebutkobling#_#progress_recid_ident_ ON kampanjebutkobling (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kampanjebutkobling##kampanjebutkobling ON kampanjebutkobling (kampanjeid, butikknr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kampanjebutmottak' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kampanjebutmottak
go
CREATE TABLE kampanjebutmottak (
  kampid decimal(15,2) null,
  butik integer null,
  sekvnr integer null,
  filnavn varchar (30) null,
  resultat varchar (30) null,
  okmottatt tinyint null,
  mottattdato datetime null,
  mottatttid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kampanjebutmottak ON kampanjebutmottak for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kampanjebutmottak t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kampanjebutmottak#_#progress_recid ON kampanjebutmottak (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjebutmottak#_#progress_recid_ident_ ON kampanjebutmottak (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kampanjebutmottak##kom ON kampanjebutmottak (butik, sekvnr, PROGRESS_RECID)
go
CREATE INDEX kampanjebutmottak##mottatt ON kampanjebutmottak (kampid, butik, sekvnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kampanjeeier' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kampanjeeier
go
CREATE TABLE kampanjeeier (
  kampeierid integer null,
  kampeiernavn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  butikknr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kampanjeeier ON kampanjeeier for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kampanjeeier t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kampanjeeier#_#progress_recid ON kampanjeeier (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjeeier#_#progress_recid_ident_ ON kampanjeeier (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kampanjeeier##butikknr ON kampanjeeier (butikknr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjeeier##kampanjeeier ON kampanjeeier (kampeierid)
go
CREATE INDEX kampanjeeier##kampeiernavn ON kampanjeeier (kampeiernavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kampanjehode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kampanjehode
go
CREATE TABLE kampanjehode (
  kampanjeid integer null,
  beskrivelse varchar (40) null,
  notat varchar (256) null,
  startdato datetime null,
  sluttdato datetime null,
  aktivert tinyint null,
  registrertdato datetime null,
  registrerttid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertav varchar (10) null,
  profilnr integer null,
  aktiverestid integer null,
  gyldigtiltid integer null,
  komplett tinyint null,
  normalpris tinyint null,
  kamp_ decimal(4,2) null,
  kampanjepris decimal(8,2) null,
  avslagtype integer null,
  setannonse tinyint null,
  kampid decimal(15,2) null,
  leverandorkampanje tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kampanjehode ON kampanjehode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kampanjehode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kampanjehode#_#progress_recid ON kampanjehode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjehode#_#progress_recid_ident_ ON kampanjehode (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kampanjehode##kampanjeid ON kampanjehode (kampanjeid)
go
CREATE INDEX kampanjehode##leverandorkampanje ON kampanjehode (leverandorkampanje, PROGRESS_RECID)
go
CREATE INDEX kampanjehode##linkmotkampanjemodul ON kampanjehode (kampid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kampanjelinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kampanjelinje
go
CREATE TABLE kampanjelinje (
  kampanjeid integer null,
  vg integer null,
  lopnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  artikkelnr decimal(13,0) null,
  pris##1 decimal(9,2) null,
  pris##2 decimal(9,2) null,
  profilnr integer null,
  feilkode varchar (30) null,
  behandlet tinyint null,
  varekost decimal(9,2) null,
  mvakr decimal(9,2) null,
  mva_ decimal(5,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kampanjelinje ON kampanjelinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kampanjelinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kampanjelinje#_#progress_recid ON kampanjelinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjelinje#_#progress_recid_ident_ ON kampanjelinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kampanjelinje##artikkelnr ON kampanjelinje (artikkelnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjelinje##idvglopnr ON kampanjelinje (kampanjeid DESC, vg, lopnr)
go
if (select name from sysobjects 
    where name = 'kampanjemixmatch' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kampanjemixmatch
go
CREATE TABLE kampanjemixmatch (
  kampid decimal(15,2) null,
  kampeierid integer null,
  kampklar tinyint null,
  kampnavn varchar (30) null,
  kampsendtdato datetime null,
  kampsendttid integer null,
  kampsluttdato datetime null,
  kampslutttid integer null,
  kampstartdato datetime null,
  kampstarttid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kampanjenotat varchar (60) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kampanjemixmatch ON kampanjemixmatch for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kampanjemixmatch t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kampanjemixmatch#_#progress_recid ON kampanjemixmatch (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjemixmatch#_#progress_recid_ident_ ON kampanjemixmatch (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kampanjemixmatch##kampanjemixmatch ON kampanjemixmatch (kampid)
go
CREATE INDEX kampanjemixmatch##kampeier ON kampanjemixmatch (kampeierid, PROGRESS_RECID)
go
CREATE INDEX kampanjemixmatch##kampnavn ON kampanjemixmatch (kampnavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kampanjetilbartikkel' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kampanjetilbartikkel
go
CREATE TABLE kampanjetilbartikkel (
  kampid decimal(15,2) null,
  kamptilbid integer null,
  kamptilbartseq decimal(10,2) null,
  kamptilbartid decimal(13,0) null,
  kamptilbartbelop decimal(8,2) null,
  kamptilbartminantall decimal(7,2) null,
  kamprabatttypeid decimal(11,2) null,
  prodfamid decimal(9,0) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kupongid decimal(15,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kampanjetilbartikkel ON kampanjetilbartikkel for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kampanjetilbartikkel t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kampanjetilbartikkel#_#progress_recid ON kampanjetilbartikkel (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjetilbartikkel#_#progress_recid_ident_ ON kampanjetilbartikkel (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kampanjetilbartikkel##kamptilbartid ON kampanjetilbartikkel (kamptilbartid, PROGRESS_RECID)
go
CREATE INDEX kampanjetilbartikkel##kamptilbartikkeldec ON kampanjetilbartikkel (kampid, kamptilbid, kamptilbartid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjetilbartikkel##kamptilbartseq ON kampanjetilbartikkel (kamptilbartseq)
go
CREATE INDEX kampanjetilbartikkel##kupongid ON kampanjetilbartikkel (kupongid, PROGRESS_RECID)
go
CREATE INDEX kampanjetilbartikkel##prodfamid ON kampanjetilbartikkel (prodfamid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kampanjetilbtype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kampanjetilbtype
go
CREATE TABLE kampanjetilbtype (
  kamptilbtypeid integer null,
  kamptilbtypenavn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kampanjetilbtype ON kampanjetilbtype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kampanjetilbtype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kampanjetilbtype#_#progress_recid ON kampanjetilbtype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjetilbtype#_#progress_recid_ident_ ON kampanjetilbtype (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kampanjetilbtype##kampanjetilbtype ON kampanjetilbtype (kamptilbtypeid)
go
CREATE INDEX kampanjetilbtype##kamptilbtypenavn ON kampanjetilbtype (kamptilbtypenavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kampanjetilbud' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kampanjetilbud
go
CREATE TABLE kampanjetilbud (
  kampid decimal(15,2) null,
  kamptilbid integer null,
  kamptilbtypeid integer null,
  haphourid integer null,
  kamptilbbelop decimal(8,2) null,
  kamptilbkvitteringstekst varchar (30) null,
  kamptilbpopuptekstbruk tinyint null,
  kamptilbpopuptekst varchar (30) null,
  kamptilbpropbetalfor tinyint null,
  kamptilbokning tinyint null,
  kamptilbgrenseantallbruk tinyint null,
  kamptilbgrenseantall decimal(7,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kamptilbnavn varchar (30) null,
  kamptilbnotat varchar (60) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kampanjetilbud ON kampanjetilbud for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kampanjetilbud t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kampanjetilbud#_#progress_recid ON kampanjetilbud (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjetilbud#_#progress_recid_ident_ ON kampanjetilbud (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kampanjetilbud##haphourid ON kampanjetilbud (haphourid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kampanjetilbud##kamptilbid ON kampanjetilbud (kampid, kamptilbid)
go
CREATE INDEX kampanjetilbud##kamptilbid2 ON kampanjetilbud (kamptilbid, PROGRESS_RECID)
go
CREATE INDEX kampanjetilbud##kamptilbnavn ON kampanjetilbud (kamptilbnavn, PROGRESS_RECID)
go
CREATE INDEX kampanjetilbud##kamptilbtypeid ON kampanjetilbud (kamptilbtypeid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kamprabatttype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kamprabatttype
go
CREATE TABLE kamprabatttype (
  kamprabatttypeid decimal(11,2) null,
  kamprabatttypenavn varchar (50) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kamprabatttype ON kamprabatttype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kamprabatttype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kamprabatttype#_#progress_recid ON kamprabatttype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kamprabatttype#_#progress_recid_ident_ ON kamprabatttype (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kamprabatttype##kamprabatttypeid ON kamprabatttype (kamprabatttypeid)
go
CREATE INDEX kamprabatttype##kamprabatttypenavn ON kamprabatttype (kamprabatttypenavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kamptilbtemplate' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kamptilbtemplate
go
CREATE TABLE kamptilbtemplate (
  kamptilbtempnr integer null,
  kamptilbtempnavn varchar (50) null,
  kampid decimal(15,2) null,
  kamptilbid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kamptilbtemplate ON kamptilbtemplate for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kamptilbtemplate t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kamptilbtemplate#_#progress_recid ON kamptilbtemplate (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kamptilbtemplate#_#progress_recid_ident_ ON kamptilbtemplate (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kamptilbtemplate##kampid ON kamptilbtemplate (kampid, kamptilbid)
go
CREATE INDEX kamptilbtemplate##kamptilbtemplatenavn ON kamptilbtemplate (kamptilbtempnavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kamptilbtemplate##kamptilbtemplatenr ON kamptilbtemplate (kamptilbtempnr)
go
if (select name from sysobjects 
    where name = 'karakteristikk' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table karakteristikk
go
CREATE TABLE karakteristikk (
  karakteristikkid varchar (4) null,
  kbeskrivelse varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_karakteristikk ON karakteristikk for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from karakteristikk t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX karakteristikk#_#progress_recid ON karakteristikk (PROGRESS_RECID)
go
CREATE UNIQUE INDEX karakteristikk#_#progress_recid_ident_ ON karakteristikk (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX karakteristikk##karakteristikk ON karakteristikk (karakteristikkid)
go
CREATE INDEX karakteristikk##kbeskrivelse ON karakteristikk (kbeskrivelse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kasse' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kasse
go
CREATE TABLE kasse (
  butikknr integer not null,
  gruppenr integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  navn varchar (30) null,
  kassenr integer not null,
  layoutnr integer null,
  teksthode##1 varchar (60) null,
  teksthode##2 varchar (60) null,
  teksthode##3 varchar (60) null,
  teksthode##4 varchar (60) null,
  teksthode##5 varchar (60) null,
  teksthode##6 varchar (60) null,
  teksthode##7 varchar (60) null,
  teksthode##8 varchar (60) null,
  teksthode##9 varchar (60) null,
  teksthode##10 varchar (60) null,
  tekstslutt##1 varchar (60) null,
  tekstslutt##2 varchar (60) null,
  tekstslutt##3 varchar (60) null,
  tekstslutt##4 varchar (60) null,
  tekstslutt##5 varchar (60) null,
  tekstslutt##6 varchar (60) null,
  tekstslutt##7 varchar (60) null,
  tekstslutt##8 varchar (60) null,
  tekstslutt##9 varchar (60) null,
  tekstslutt##10 varchar (60) null,
  teksthstil##1 integer null,
  teksthstil##2 integer null,
  teksthstil##3 integer null,
  teksthstil##4 integer null,
  teksthstil##5 integer null,
  teksthstil##6 integer null,
  teksthstil##7 integer null,
  teksthstil##8 integer null,
  teksthstil##9 integer null,
  teksthstil##10 integer null,
  tekstsstil##1 integer null,
  tekstsstil##2 integer null,
  tekstsstil##3 integer null,
  tekstsstil##4 integer null,
  tekstsstil##5 integer null,
  tekstsstil##6 integer null,
  tekstsstil##7 integer null,
  tekstsstil##8 integer null,
  tekstsstil##9 integer null,
  tekstsstil##10 integer null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  aktiv tinyint null,
  eljournal##1 varchar (30) null,
  eljournal##2 varchar (30) null,
  kvittering##1 varchar (30) null,
  kvittering##2 varchar (30) null,
  utskriftskopi##1 varchar (30) null,
  utskriftskopi##2 varchar (30) null,
  kassereropgj##1 varchar (30) null,
  kassereropgj##2 varchar (30) null,
  dagsopgj##1 varchar (30) null,
  dagsopgj##2 varchar (30) null,
  eljournalid varchar (30) not null,
  kvitteringid varchar (30) not null,
  utskriftskopiid varchar (30) not null,
  kassereroppgjid varchar (30) not null,
  dagsoppgj varchar (30) not null,
  eljournalaktiv tinyint null,
  kvitteringaktiv tinyint null,
  utskriftskopiaktiv tinyint null,
  kassereroppgjaktiv tinyint null,
  dagsoppgjaktiv tinyint null,
  eljournalkatalog varchar (30) null,
  kvitteringkatalog varchar (30) null,
  utskriftskopikatalog varchar (30) null,
  kassereroppgjkatalog varchar (30) null,
  dagsoppgjkatalog varchar (30) null,
  eljournalkonv tinyint null,
  kvitteringkonv tinyint null,
  utskriftskopikonv tinyint null,
  kassereroppgjkonv tinyint null,
  dagsoppgjkonv tinyint null,
  dagsoppgjid varchar (30) not null,
  eljournaloperand integer null,
  kvitteringoperand integer null,
  utskriftskopioperand integer null,
  kassereroppgjoperand integer null,
  dagsoppgjoperand integer null,
  eljournalinnles varchar (20) null,
  kvitteringinnles varchar (20) null,
  utskriftskopiinnles varchar (20) null,
  kassereroppgjinnles varchar (20) null,
  dagsoppgjinnles varchar (20) null,
  eljournalbehandle varchar (20) null,
  kvitteringbehandle varchar (20) null,
  utskriftskopibehandle varchar (20) null,
  kassereroppgjbehandle varchar (20) null,
  dagsoppgjbehandle varchar (20) null,
  modellnr integer null,
  fakturaskriver varchar (30) null,
  fakturakopi integer null,
  fakturalayout integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kasse ON kasse for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kasse t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kasse#_#progress_recid ON kasse (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kasse#_#progress_recid_ident_ ON kasse (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kasse##dagsoppgjid ON kasse (dagsoppgj, PROGRESS_RECID)
go
CREATE INDEX kasse##eljournalid ON kasse (eljournalid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kasse##kasse ON kasse (butikknr, gruppenr, kassenr)
go
CREATE INDEX kasse##kassereroppgjid ON kasse (kassereroppgjid, PROGRESS_RECID)
go
CREATE INDEX kasse##kvitteringid ON kasse (kvitteringid, PROGRESS_RECID)
go
CREATE INDEX kasse##navn ON kasse (butikknr, gruppenr, navn, PROGRESS_RECID)
go
CREATE INDEX kasse##utskriftskopiid ON kasse (utskriftskopiid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kassererbilag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kassererbilag
go
CREATE TABLE kassererbilag (
  butikknr integer not null,
  dato datetime null,
  kasserernr integer null,
  z_nummer integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  bilagsnr integer null,
  meknad varchar (40) null,
  belop decimal(9,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kassererbilag ON kassererbilag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kassererbilag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kassererbilag#_#progress_recid ON kassererbilag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kassererbilag#_#progress_recid_ident_ ON kassererbilag (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kassererbilag##kassererbilag ON kassererbilag (butikknr, dato, kasserernr, z_nummer, bilagsnr)
go
if (select name from sysobjects 
    where name = 'kassererdag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kassererdag
go
CREATE TABLE kassererdag (
  butikknr integer not null,
  dato datetime not null,
  kasserernr integer not null,
  nettokr decimal(10,2) not null,
  kontokr decimal(10,2) not null,
  kontoinnkr decimal(10,2) not null,
  kontoinnant integer not null,
  vareant integer not null,
  kundeant integer not null,
  innbetkr decimal(10,2) not null,
  innbetant integer not null,
  utbetkr decimal(10,2) not null,
  utbetant integer not null,
  vekslekr decimal(10,2) not null,
  veksleant integer not null,
  levertkr decimal(10,2) not null,
  levertant integer not null,
  kontantkr decimal(10,2) not null,
  kontantant integer not null,
  annetkr decimal(10,2) not null,
  annetant integer not null,
  behkontkr decimal(10,2) not null,
  kreditkr decimal(10,2) not null,
  kreditant integer not null,
  media3kr decimal(10,2) not null,
  media3ant integer not null,
  media4kr decimal(10,2) not null,
  media4ant integer not null,
  bankkr decimal(10,2) not null,
  bankant integer not null,
  sjekkr decimal(10,2) not null,
  sjekkant integer not null,
  kronerabkr decimal(10,2) not null,
  kronerabant integer not null,
  returkr decimal(10,2) not null,
  returant integer not null,
  pantkr decimal(10,2) not null,
  pantant integer not null,
  feil1kr decimal(10,2) not null,
  feil1ant integer not null,
  feil2kr decimal(10,2) not null,
  feil2ant integer not null,
  nullant integer not null,
  signtid integer not null,
  regtid integer not null,
  opentid integer not null,
  tastant integer not null,
  feilant integer not null,
  scannant integer not null,
  signontid integer not null,
  signofftid integer not null,
  tellkontkr decimal(10,2) not null,
  tellsjekkr decimal(10,2) not null,
  tellmed3kr decimal(10,2) not null,
  tellmed4kr decimal(10,2) not null,
  tellbilagkr decimal(10,2) not null,
  tellbankkr decimal(10,2) not null,
  internkr decimal(10,2) not null,
  internant integer not null,
  medinnkr decimal(10,2) not null,
  medinnant integer not null,
  medutkr decimal(10,2) not null,
  medutant integer not null,
  depkr decimal(10,2) not null,
  depant integer not null,
  medkr##1 decimal(11,2) not null,
  medkr##2 decimal(11,2) not null,
  medant##1 integer not null,
  medant##2 integer not null,
  grutbyttekr##1 decimal(11,2) not null,
  grutbyttekr##2 decimal(11,2) not null,
  grutbytteant##1 integer not null,
  grutbytteant##2 integer not null,
  kortkr##1 decimal(11,2) not null,
  kortkr##2 decimal(11,2) not null,
  kortkr##3 decimal(11,2) not null,
  kortkr##4 decimal(11,2) not null,
  kortkr##5 decimal(11,2) not null,
  kortkr##6 decimal(11,2) not null,
  kortkr##7 decimal(11,2) not null,
  kortkr##8 decimal(11,2) not null,
  kortkr##9 decimal(11,2) not null,
  kortkr##10 decimal(11,2) not null,
  kortkr##11 decimal(11,2) not null,
  kortkr##12 decimal(11,2) not null,
  kortkr##13 decimal(11,2) not null,
  kortkr##14 decimal(11,2) not null,
  kortkr##15 decimal(11,2) not null,
  kortkr##16 decimal(11,2) not null,
  kortkr##17 decimal(11,2) not null,
  kortkr##18 decimal(11,2) not null,
  kortkr##19 decimal(11,2) not null,
  kortkr##20 decimal(11,2) not null,
  kortkr##21 decimal(11,2) not null,
  kortkr##22 decimal(11,2) not null,
  kortkr##23 decimal(11,2) not null,
  kortkr##24 decimal(11,2) not null,
  kortkr##25 decimal(11,2) not null,
  kortkr##26 decimal(11,2) not null,
  kortkr##27 decimal(11,2) not null,
  kortkr##28 decimal(11,2) not null,
  kortkr##29 decimal(11,2) not null,
  kortkr##30 decimal(11,2) not null,
  kortant##1 integer not null,
  kortant##2 integer not null,
  kortant##3 integer not null,
  kortant##4 integer not null,
  kortant##5 integer not null,
  kortant##6 integer not null,
  kortant##7 integer not null,
  kortant##8 integer not null,
  kortant##9 integer not null,
  kortant##10 integer not null,
  kortant##11 integer not null,
  kortant##12 integer not null,
  kortant##13 integer not null,
  kortant##14 integer not null,
  kortant##15 integer not null,
  kortant##16 integer not null,
  kortant##17 integer not null,
  kortant##18 integer not null,
  kortant##19 integer not null,
  kortant##20 integer not null,
  kortant##21 integer not null,
  kortant##22 integer not null,
  kortant##23 integer not null,
  kortant##24 integer not null,
  kortant##25 integer not null,
  kortant##26 integer not null,
  kortant##27 integer not null,
  kortant##28 integer not null,
  kortant##29 integer not null,
  kortant##30 integer not null,
  mvagrlag##1 decimal(11,2) not null,
  mvagrlag##2 decimal(11,2) not null,
  mvagrlag##3 decimal(11,2) not null,
  mvagrlag##4 decimal(11,2) not null,
  mvagrlag##5 decimal(11,2) not null,
  mvagrlag##6 decimal(11,2) not null,
  mvagrlag##7 decimal(11,2) not null,
  mvagrlag##8 decimal(11,2) not null,
  mvagrlag##9 decimal(11,2) not null,
  mvagrlag##10 decimal(11,2) not null,
  mvakr##1 decimal(11,2) not null,
  mvakr##2 decimal(11,2) not null,
  mvakr##3 decimal(11,2) not null,
  mvakr##4 decimal(11,2) not null,
  mvakr##5 decimal(11,2) not null,
  mvakr##6 decimal(11,2) not null,
  mvakr##7 decimal(11,2) not null,
  mvakr##8 decimal(11,2) not null,
  mvakr##9 decimal(11,2) not null,
  mvakr##10 decimal(11,2) not null,
  rabattkr##1 decimal(11,2) not null,
  rabattkr##2 decimal(11,2) not null,
  rabattkr##3 decimal(11,2) not null,
  rabattkr##4 decimal(11,2) not null,
  rabattkr##5 decimal(11,2) not null,
  rabattant##1 integer not null,
  rabattant##2 integer not null,
  rabattant##3 integer not null,
  rabattant##4 integer not null,
  rabattant##5 integer not null,
  posenr varchar (10) null,
  sendtbankkr decimal(11,2) not null,
  tilgodeinnkr decimal(11,2) not null,
  tilgodeinnant integer not null,
  tilgodeutkr decimal(11,2) not null,
  tilgodeutant integer not null,
  telltilgodekr decimal(11,2) not null,
  behtilgodekr decimal(11,2) not null,
  pantutkr decimal(11,2) not null,
  pantutant integer not null,
  ordrekr decimal(11,2) not null,
  ordreant integer not null,
  medmva##1 decimal(11,2) not null,
  medmva##2 decimal(11,2) not null,
  grutbyttemva##1 decimal(11,2) not null,
  grutbyttemva##2 decimal(11,2) not null,
  gavekortutkr decimal(11,2) not null,
  gavekortutant integer not null,
  reklamertkr decimal(11,2) not null,
  reklamertant integer not null,
  makbongkr decimal(11,2) not null,
  makbongant integer not null,
  spillkr decimal(11,2) not null,
  postkr decimal(11,2) not null,
  tellspillkr decimal(11,2) not null,
  tellpostkr decimal(11,2) not null,
  behkredkr decimal(11,2) not null,
  behmed3kr decimal(11,2) not null,
  behmed4kr decimal(11,2) not null,
  behbankkr decimal(11,2) not null,
  behsjekkr decimal(11,2) not null,
  tilgodeinn2kr decimal(11,2) not null,
  tilgodeinn2ant integer not null,
  telltilgodeinn2kr decimal(11,2) not null,
  behtilgode2kr decimal(11,2) not null,
  telltilgodeutkr decimal(11,2) not null,
  gavekortinn2kr decimal(11,2) not null,
  gavekortinn2ant integer not null,
  gavekortut2kr decimal(11,2) not null,
  gavekortut2ant integer not null,
  tellgavekortinn2kr decimal(11,2) not null,
  tellgavekortut2kr decimal(11,2) not null,
  behgavekort2kr decimal(11,2) not null,
  tellgavekortutkr decimal(11,2) not null,
  finanskr decimal(11,2) not null,
  finansant integer not null,
  tellfinanskr decimal(11,2) not null,
  behfinanskr decimal(11,2) not null,
  pluant integer not null,
  mobilkr decimal(11,2) not null,
  mobilant integer not null,
  tellmobilkr decimal(11,2) not null,
  behmobilkr decimal(11,2) not null,
  tellpantkr decimal(11,2) not null,
  tellreturkr decimal(11,2) not null,
  tellmakbongkr decimal(11,2) not null,
  tellkredkr decimal(11,2) not null,
  de1 decimal(11,2) not null,
  de2 decimal(11,2) not null,
  de3 decimal(11,2) not null,
  de4 decimal(11,2) not null,
  de5 decimal(11,2) not null,
  de6 decimal(11,2) not null,
  de7 decimal(11,2) not null,
  de8 decimal(11,2) not null,
  de9 decimal(11,2) not null,
  de10 decimal(11,2) not null,
  i1 integer not null,
  i2 integer not null,
  i3 integer not null,
  i4 integer not null,
  i5 integer not null,
  i6 integer not null,
  i7 integer not null,
  i8 integer not null,
  i9 integer not null,
  i10 integer not null,
  z_nummer integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kassererdag ON kassererdag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kassererdag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kassererdag#_#progress_recid ON kassererdag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kassererdag#_#progress_recid_ident_ ON kassererdag (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kassererdag##dato2 ON kassererdag (dato, butikknr, kasserernr, PROGRESS_RECID)
go
CREATE INDEX kassererdag##kasserer ON kassererdag (butikknr, kasserernr, dato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kassererdag##kassererdag ON kassererdag (butikknr, dato, kasserernr, z_nummer)
go
if (select name from sysobjects 
    where name = 'kassererdat' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kassererdat
go
CREATE TABLE kassererdat (
  butikknr integer not null,
  dato datetime not null,
  kasserernr integer not null,
  nettokr decimal(10,2) not null,
  kontokr decimal(10,2) not null,
  kontoinnkr decimal(10,2) not null,
  kontoinnant integer not null,
  vareant integer not null,
  kundeant integer not null,
  innbetkr decimal(10,2) not null,
  innbetant integer not null,
  utbetkr decimal(10,2) not null,
  utbetant integer not null,
  vekslekr decimal(10,2) not null,
  veksleant integer not null,
  levertkr decimal(10,2) not null,
  levertant integer not null,
  kontantkr decimal(10,2) not null,
  kontantant integer not null,
  annetkr decimal(10,2) not null,
  annetant integer not null,
  behkontkr decimal(10,2) not null,
  kreditkr decimal(10,2) not null,
  kreditant integer not null,
  media3kr decimal(10,2) not null,
  media3ant integer not null,
  media4kr decimal(10,2) not null,
  media4ant integer not null,
  bankkr decimal(10,2) not null,
  bankant integer not null,
  sjekkr decimal(10,2) not null,
  sjekkant integer not null,
  kronerabkr decimal(10,2) not null,
  kronerabant integer not null,
  returkr decimal(10,2) not null,
  returant integer not null,
  pantkr decimal(10,2) not null,
  pantant integer not null,
  feil1kr decimal(10,2) not null,
  feil1ant integer not null,
  feil2kr decimal(10,2) not null,
  feil2ant integer not null,
  nullant integer not null,
  signtid integer not null,
  regtid integer not null,
  opentid integer not null,
  tastant integer not null,
  feilant integer not null,
  scannant integer not null,
  signontid integer not null,
  signofftid integer not null,
  tellkontkr decimal(10,2) not null,
  tellsjekkr decimal(10,2) not null,
  tellmed3kr decimal(10,2) not null,
  tellmed4kr decimal(10,2) not null,
  tellbilagkr decimal(10,2) not null,
  tellbankkr decimal(10,2) not null,
  internkr decimal(10,2) not null,
  internant integer not null,
  medinnkr decimal(10,2) not null,
  medinnant integer not null,
  medutkr decimal(10,2) not null,
  medutant integer not null,
  depkr decimal(10,2) not null,
  depant integer not null,
  medkr##1 decimal(11,2) not null,
  medkr##2 decimal(11,2) not null,
  medant##1 integer not null,
  medant##2 integer not null,
  grutbyttekr##1 decimal(11,2) not null,
  grutbyttekr##2 decimal(11,2) not null,
  grutbytteant##1 integer not null,
  grutbytteant##2 integer not null,
  kortkr##1 decimal(11,2) not null,
  kortkr##2 decimal(11,2) not null,
  kortkr##3 decimal(11,2) not null,
  kortkr##4 decimal(11,2) not null,
  kortkr##5 decimal(11,2) not null,
  kortkr##6 decimal(11,2) not null,
  kortkr##7 decimal(11,2) not null,
  kortkr##8 decimal(11,2) not null,
  kortkr##9 decimal(11,2) not null,
  kortkr##10 decimal(11,2) not null,
  kortkr##11 decimal(11,2) not null,
  kortkr##12 decimal(11,2) not null,
  kortkr##13 decimal(11,2) not null,
  kortkr##14 decimal(11,2) not null,
  kortkr##15 decimal(11,2) not null,
  kortkr##16 decimal(11,2) not null,
  kortkr##17 decimal(11,2) not null,
  kortkr##18 decimal(11,2) not null,
  kortkr##19 decimal(11,2) not null,
  kortkr##20 decimal(11,2) not null,
  kortkr##21 decimal(11,2) not null,
  kortkr##22 decimal(11,2) not null,
  kortkr##23 decimal(11,2) not null,
  kortkr##24 decimal(11,2) not null,
  kortkr##25 decimal(11,2) not null,
  kortkr##26 decimal(11,2) not null,
  kortkr##27 decimal(11,2) not null,
  kortkr##28 decimal(11,2) not null,
  kortkr##29 decimal(11,2) not null,
  kortkr##30 decimal(11,2) not null,
  kortant##1 integer not null,
  kortant##2 integer not null,
  kortant##3 integer not null,
  kortant##4 integer not null,
  kortant##5 integer not null,
  kortant##6 integer not null,
  kortant##7 integer not null,
  kortant##8 integer not null,
  kortant##9 integer not null,
  kortant##10 integer not null,
  kortant##11 integer not null,
  kortant##12 integer not null,
  kortant##13 integer not null,
  kortant##14 integer not null,
  kortant##15 integer not null,
  kortant##16 integer not null,
  kortant##17 integer not null,
  kortant##18 integer not null,
  kortant##19 integer not null,
  kortant##20 integer not null,
  kortant##21 integer not null,
  kortant##22 integer not null,
  kortant##23 integer not null,
  kortant##24 integer not null,
  kortant##25 integer not null,
  kortant##26 integer not null,
  kortant##27 integer not null,
  kortant##28 integer not null,
  kortant##29 integer not null,
  kortant##30 integer not null,
  mvagrlag##1 decimal(11,2) not null,
  mvagrlag##2 decimal(11,2) not null,
  mvagrlag##3 decimal(11,2) not null,
  mvagrlag##4 decimal(11,2) not null,
  mvagrlag##5 decimal(11,2) not null,
  mvagrlag##6 decimal(11,2) not null,
  mvagrlag##7 decimal(11,2) not null,
  mvagrlag##8 decimal(11,2) not null,
  mvagrlag##9 decimal(11,2) not null,
  mvagrlag##10 decimal(11,2) not null,
  mvakr##1 decimal(11,2) not null,
  mvakr##2 decimal(11,2) not null,
  mvakr##3 decimal(11,2) not null,
  mvakr##4 decimal(11,2) not null,
  mvakr##5 decimal(11,2) not null,
  mvakr##6 decimal(11,2) not null,
  mvakr##7 decimal(11,2) not null,
  mvakr##8 decimal(11,2) not null,
  mvakr##9 decimal(11,2) not null,
  mvakr##10 decimal(11,2) not null,
  rabattkr##1 decimal(11,2) not null,
  rabattkr##2 decimal(11,2) not null,
  rabattkr##3 decimal(11,2) not null,
  rabattkr##4 decimal(11,2) not null,
  rabattkr##5 decimal(11,2) not null,
  rabattant##1 integer not null,
  rabattant##2 integer not null,
  rabattant##3 integer not null,
  rabattant##4 integer not null,
  rabattant##5 integer not null,
  posenr varchar (10) null,
  sendtbankkr decimal(11,2) not null,
  tilgodeinnkr decimal(11,2) not null,
  tilgodeinnant integer not null,
  tilgodeutkr decimal(11,2) not null,
  tilgodeutant integer not null,
  telltilgodekr decimal(11,2) not null,
  behtilgodekr decimal(11,2) not null,
  pantutkr decimal(11,2) not null,
  pantutant integer not null,
  ordrekr decimal(11,2) not null,
  ordreant integer not null,
  medmva##1 decimal(11,2) not null,
  medmva##2 decimal(11,2) not null,
  grutbyttemva##1 decimal(11,2) not null,
  grutbyttemva##2 decimal(11,2) not null,
  gavekortutkr decimal(11,2) not null,
  gavekortutant integer not null,
  reklamertkr decimal(11,2) not null,
  reklamertant integer not null,
  makbongkr decimal(11,2) not null,
  makbongant integer not null,
  spillkr decimal(11,2) not null,
  postkr decimal(11,2) not null,
  tellspillkr decimal(11,2) not null,
  tellpostkr decimal(11,2) not null,
  behkredkr decimal(11,2) not null,
  behmed3kr decimal(11,2) not null,
  behmed4kr decimal(11,2) not null,
  behbankkr decimal(11,2) not null,
  behsjekkr decimal(11,2) not null,
  tilgodeinn2kr decimal(11,2) not null,
  tilgodeinn2ant integer not null,
  telltilgodeinn2kr decimal(11,2) not null,
  behtilgode2kr decimal(11,2) not null,
  telltilgodeutkr decimal(11,2) not null,
  gavekortinn2kr decimal(11,2) not null,
  gavekortinn2ant integer not null,
  gavekortut2kr decimal(11,2) not null,
  gavekortut2ant integer not null,
  tellgavekortinn2kr decimal(11,2) not null,
  tellgavekortut2kr decimal(11,2) not null,
  behgavekort2kr decimal(11,2) not null,
  tellgavekortutkr decimal(11,2) not null,
  finanskr decimal(11,2) not null,
  finansant integer not null,
  tellfinanskr decimal(11,2) not null,
  behfinanskr decimal(11,2) not null,
  pluant integer not null,
  mobilkr decimal(11,2) not null,
  mobilant integer not null,
  tellmobilkr decimal(11,2) not null,
  behmobilkr decimal(11,2) not null,
  tellpantkr decimal(11,2) not null,
  tellreturkr decimal(11,2) not null,
  tellmakbongkr decimal(11,2) not null,
  tellkredkr decimal(11,2) not null,
  de1 decimal(11,2) not null,
  de2 decimal(11,2) not null,
  de3 decimal(11,2) not null,
  de4 decimal(11,2) not null,
  de5 decimal(11,2) not null,
  de6 decimal(11,2) not null,
  de7 decimal(11,2) not null,
  de8 decimal(11,2) not null,
  de9 decimal(11,2) not null,
  de10 decimal(11,2) not null,
  i1 integer not null,
  i2 integer not null,
  i3 integer not null,
  i4 integer not null,
  i5 integer not null,
  i6 integer not null,
  i7 integer not null,
  i8 integer not null,
  i9 integer not null,
  i10 integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kassererdat ON kassererdat for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kassererdat t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kassererdat#_#progress_recid ON kassererdat (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kassererdat#_#progress_recid_ident_ ON kassererdat (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kassererdat##kassdagidx1 ON kassererdat (butikknr, kasserernr, dato)
go
CREATE INDEX kassererdat##kassererbutikk ON kassererdat (butikknr, PROGRESS_RECID)
go
CREATE INDEX kassererdat##kassererdato ON kassererdat (dato, PROGRESS_RECID)
go
CREATE INDEX kassererdat##kassererkasserernr ON kassererdat (kasserernr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kassererkontanter' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kassererkontanter
go
CREATE TABLE kassererkontanter (
  butikknr integer not null,
  dato datetime null,
  kasserernr integer null,
  z_nummer integer null,
  belop##1 decimal(9,2) null,
  belop##2 decimal(9,2) null,
  belop##3 decimal(9,2) null,
  belop##4 decimal(9,2) null,
  belop##5 decimal(9,2) null,
  belop##6 decimal(9,2) null,
  belop##7 decimal(9,2) null,
  belop##8 decimal(9,2) null,
  belop##9 decimal(9,2) null,
  belop##10 decimal(9,2) null,
  belop##11 decimal(9,2) null,
  belop##12 decimal(9,2) null,
  belop##13 decimal(9,2) null,
  belop##14 decimal(9,2) null,
  belop##15 decimal(9,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  antallvalor##1 integer null,
  antallvalor##2 integer null,
  antallvalor##3 integer null,
  antallvalor##4 integer null,
  antallvalor##5 integer null,
  antallvalor##6 integer null,
  antallvalor##7 integer null,
  antallvalor##8 integer null,
  antallvalor##9 integer null,
  antallvalor##10 integer null,
  antallvalor##11 integer null,
  antallvalor##12 integer null,
  antallvalor##13 integer null,
  antallvalor##14 integer null,
  antallvalor##15 integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kassererkontanter ON kassererkontanter for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kassererkontanter t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kassererkontanter#_#progress_recid ON kassererkontanter (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kassererkontanter#_#progress_recid_ident_ ON kassererkontanter (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kassererkontanter##kasserer ON kassererkontanter (butikknr, kasserernr, dato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kassererkontanter##kassererkontanter ON kassererkontanter (butikknr, dato, kasserernr, z_nummer)
go
if (select name from sysobjects 
    where name = 'kassereroppgj' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kassereroppgj
go
CREATE TABLE kassereroppgj (
  butikknr integer not null,
  dato datetime null,
  kasserernr integer null,
  z_nummer integer null,
  opptaltveksel decimal(9,2) null,
  opptaltkontanter decimal(9,2) null,
  opptaltsjekk decimal(9,2) null,
  opptaltreserve decimal(9,2) null,
  opptaltgavekort decimal(9,2) null,
  opptalttilgode decimal(9,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  opptaltgavekortandre decimal(9,2) null,
  opptaltgavekortutlevert decimal(9,2) null,
  opptalttilgodeandre decimal(9,2) null,
  opptalttilgodeutlevert decimal(9,2) null,
  opptaltinnveksel decimal(9,2) null,
  opptaltvaluta decimal(9,2) null,
  opptaltlevertbank decimal(9,2) null,
  opptaltbilag decimal(9,2) null,
  posenr varchar (20) null,
  opptaltfinansiering decimal(9,2) null,
  opptaltutbetalt decimal(9,2) null,
  opptaltkupong decimal(9,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kassereroppgj ON kassereroppgj for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kassereroppgj t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kassereroppgj#_#progress_recid ON kassereroppgj (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kassereroppgj#_#progress_recid_ident_ ON kassereroppgj (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kassereroppgj##kasserer ON kassereroppgj (butikknr, kasserernr, dato, z_nummer, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kassereroppgj##kassereroppgj ON kassereroppgj (butikknr, dato, kasserernr, z_nummer)
go
if (select name from sysobjects 
    where name = 'kasserervaluta' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kasserervaluta
go
CREATE TABLE kasserervaluta (
  butikknr integer not null,
  kassenr integer not null,
  dato datetime null,
  kasserernr integer null,
  z_nummer integer null,
  valkod varchar (3) null,
  kassevalkurs decimal(5,2) null,
  valuta decimal(9,2) null,
  belop decimal(9,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kasserervaluta ON kasserervaluta for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kasserervaluta t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kasserervaluta#_#progress_recid ON kasserervaluta (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kasserervaluta#_#progress_recid_ident_ ON kasserervaluta (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kasserervaluta##kvaluta ON kasserervaluta (butikknr, kassenr, dato, kasserernr, z_nummer, valkod)
go
if (select name from sysobjects 
    where name = 'kassetrans' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kassetrans
go
CREATE TABLE kassetrans (
  butikk integer null,
  dato datetime null,
  kasse integer null,
  belop decimal(9,2) null,
  antall decimal(10,3) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  posttype integer null,
  ttid integer not null,
  tbid integer null,
  korttype varchar (15) null,
  transbeskr varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kassetrans ON kassetrans for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kassetrans t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kassetrans#_#progress_recid ON kassetrans (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kassetrans#_#progress_recid_ident_ ON kassetrans (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kassetrans##butikkkassedato ON kassetrans (butikk, kasse, dato, ttid, tbid, korttype, posttype, PROGRESS_RECID)
go
CREATE INDEX kassetrans##datobutikkkasse ON kassetrans (dato, butikk, kasse, ttid, tbid, korttype, posttype, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kasvaluta' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kasvaluta
go
CREATE TABLE kasvaluta (
  valkod varchar (3) null,
  valkurs decimal(9,6) null,
  valland varchar (15) null,
  valdatum datetime null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  valnr integer null,
  valnavn varchar (30) null,
  indeks integer null,
  retur decimal(6,3) null,
  kassevalkurs decimal(5,2) null,
  valaktiv tinyint null,
  egenvaluta tinyint null,
  profilnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kasvaluta ON kasvaluta for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kasvaluta t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kasvaluta#_#progress_recid ON kasvaluta (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kasvaluta#_#progress_recid_ident_ ON kasvaluta (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kasvaluta##profilogvaluta ON kasvaluta (profilnr, valkod)
go
CREATE INDEX kasvaluta##valland ON kasvaluta (valland, PROGRESS_RECID)
go
CREATE INDEX kasvaluta##valytaidx ON kasvaluta (valkod, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kas_konter' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kas_konter
go
CREATE TABLE kas_konter (
  butikk integer null,
  dato datetime null,
  p_konto_1_nr integer null,
  p_konto_1_v decimal(9,2) null,
  p_konto_2_nr integer null,
  p_konto_2_v decimal(9,2) null,
  p_konto_3_nr integer null,
  p_konto_3_v decimal(9,2) null,
  p_konto_4_nr integer null,
  p_konto_4_v decimal(9,2) null,
  p_konto_5_nr integer null,
  p_konto_5_v decimal(9,2) null,
  p_konto_6_nr integer null,
  p_konto_6_v decimal(9,2) null,
  p_konto_7_nr integer null,
  p_konto_7_v decimal(9,2) null,
  p_konto_8_nr integer null,
  p_konto_8_v decimal(9,2) null,
  p_konto_9_nr integer null,
  p_konto_9_v decimal(9,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kas_konter ON kas_konter for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kas_konter t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kas_konter#_#progress_recid ON kas_konter (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kas_konter#_#progress_recid_ident_ ON kas_konter (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kas_konter##kas_kont ON kas_konter (dato, butikk)
go
if (select name from sysobjects 
    where name = 'kas_logg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kas_logg
go
CREATE TABLE kas_logg (
  butikk integer null,
  kasse integer null,
  z_nummer integer null,
  dato datetime null,
  kv_nr1 integer null,
  kv_nr2 integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kas_logg ON kas_logg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kas_logg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kas_logg#_#progress_recid ON kas_logg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kas_logg#_#progress_recid_ident_ ON kas_logg (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kas_logg##kas_log ON kas_logg (dato, butikk, kasse, z_nummer, kv_nr1)
go
if (select name from sysobjects 
    where name = 'kas_rap' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kas_rap
go
CREATE TABLE kas_rap (
  dato datetime null,
  butikk integer null,
  kasse integer null,
  z_nummer integer null,
  kontant decimal(9,2) null,
  sjekk decimal(9,2) null,
  kort decimal(9,2) null,
  kredit decimal(9,2) null,
  kupong1 decimal(9,2) null,
  kupong2 decimal(9,2) null,
  tilgode decimal(9,2) null,
  layaway_inn decimal(9,2) null,
  layaway_ut decimal(9,2) null,
  kont_inn decimal(9,2) null,
  kont_ut decimal(9,2) null,
  gavekort decimal(9,2) null,
  rekvisisasjon decimal(9,2) null,
  pant decimal(9,2) null,
  bank decimal(9,2) null,
  dropp decimal(9,2) null,
  overfort decimal(9,2) null,
  cashback decimal(9,2) null,
  veksel decimal(9,2) null,
  avrunding decimal(9,2) null,
  reklamasjon decimal(9,2) null,
  retur decimal(9,2) null,
  innbetaltkunde decimal(9,2) null,
  medlemssalg decimal(9,2) null,
  antcashback integer null,
  antmedlemssalg integer null,
  antinnbetaltkunde integer null,
  antretur integer null,
  antkontant integer null,
  antsjekk integer null,
  antkort integer null,
  antkredit integer null,
  antkupong1 integer null,
  antkupong2 integer null,
  anttilgode integer null,
  antbank integer null,
  antgavekort integer null,
  antrekvisisjon integer null,
  antveksel integer null,
  antavrunding integer null,
  antdropp integer null,
  antoverfort integer null,
  antkont_inn integer null,
  antkont_ut integer null,
  antlayaway_inn integer null,
  antlayaway_ut integer null,
  antreturer integer null,
  tilgodeinn decimal(9,2) null,
  tilgodeut decimal(9,2) null,
  anttilgodeinn integer null,
  anttilgodeut integer null,
  gavekortut decimal(9,2) null,
  gavekortinn decimal(9,2) null,
  antgavekortut integer null,
  antgavekortinn integer null,
  medlemsrabatt decimal(9,2) null,
  kunderabatt decimal(9,2) null,
  personalrabatt decimal(9,2) null,
  generellrabatt decimal(9,2) null,
  antpersonalrabatt integer null,
  antmedlemsrabatt integer null,
  antkunderabatt integer null,
  antgenerellrabatt integer null,
  overfortinn decimal(9,2) null,
  overfortut decimal(9,2) null,
  antoverfortinn integer null,
  antoverfortut integer null,
  mvagrp##1 integer null,
  mvagrp##2 integer null,
  mvagrp##3 integer null,
  mvagrp##4 integer null,
  mvagrp##5 integer null,
  mvagrp##6 integer null,
  mvagrp##7 integer null,
  mvagrp##8 integer null,
  mvagrp##9 integer null,
  mvagrp##10 integer null,
  mvagrunnlag##1 decimal(10,2) null,
  mvagrunnlag##2 decimal(10,2) null,
  mvagrunnlag##3 decimal(10,2) null,
  mvagrunnlag##4 decimal(10,2) null,
  mvagrunnlag##5 decimal(10,2) null,
  mvagrunnlag##6 decimal(10,2) null,
  mvagrunnlag##7 decimal(10,2) null,
  mvagrunnlag##8 decimal(10,2) null,
  mvagrunnlag##9 decimal(10,2) null,
  mvagrunnlag##10 decimal(10,2) null,
  mvabelop##1 decimal(10,2) null,
  mvabelop##2 decimal(10,2) null,
  mvabelop##3 decimal(10,2) null,
  mvabelop##4 decimal(10,2) null,
  mvabelop##5 decimal(10,2) null,
  mvabelop##6 decimal(10,2) null,
  mvabelop##7 decimal(10,2) null,
  mvabelop##8 decimal(10,2) null,
  mvabelop##9 decimal(10,2) null,
  mvabelop##10 decimal(10,2) null,
  antreklamasjoner integer null,
  vekselbeholdning decimal(9,2) null,
  kontantbeholdning decimal(9,2) null,
  sjekkbeholdning decimal(9,2) null,
  lagerjustering decimal(9,2) null,
  varemottak decimal(9,2) null,
  antlagerjustering integer null,
  antvaremottak integer null,
  brekkasje decimal(9,2) null,
  interntforbruk decimal(9,2) null,
  antbrekkasje integer null,
  antinterntforbruk integer null,
  reservelosning decimal(9,2) null,
  antreservelosning integer null,
  antpakkerabatt integer null,
  pakkerabatt decimal(9,2) null,
  tilgodeandre decimal(9,2) null,
  gavekortandreinn decimal(9,2) null,
  antgavekortandreinn integer null,
  gavekortrabatt decimal(9,2) null,
  antgavekortrabut integer null,
  anttilgodeandre integer null,
  mvakredgrp##1 integer null,
  mvakredgrp##2 integer null,
  mvakredgrp##3 integer null,
  mvakredgrp##4 integer null,
  mvakredgrp##5 integer null,
  mvakredgrp##6 integer null,
  mvakredgrp##7 integer null,
  mvakredgrp##8 integer null,
  mvakredgrp##9 integer null,
  mvakredgrp##10 integer null,
  mvakredgrunnlag##1 decimal(10,2) null,
  mvakredgrunnlag##2 decimal(10,2) null,
  mvakredgrunnlag##3 decimal(10,2) null,
  mvakredgrunnlag##4 decimal(10,2) null,
  mvakredgrunnlag##5 decimal(10,2) null,
  mvakredgrunnlag##6 decimal(10,2) null,
  mvakredgrunnlag##7 decimal(10,2) null,
  mvakredgrunnlag##8 decimal(10,2) null,
  mvakredgrunnlag##9 decimal(10,2) null,
  mvakredgrunnlag##10 decimal(10,2) null,
  mvakredbelop##1 decimal(10,2) null,
  mvakredbelop##2 decimal(10,2) null,
  mvakredbelop##3 decimal(10,2) null,
  mvakredbelop##4 decimal(10,2) null,
  mvakredbelop##5 decimal(10,2) null,
  mvakredbelop##6 decimal(10,2) null,
  mvakredbelop##7 decimal(10,2) null,
  mvakredbelop##8 decimal(10,2) null,
  mvakredbelop##9 decimal(10,2) null,
  mvakredbelop##10 decimal(10,2) null,
  non_salepos decimal(9,2) null,
  non_saleneg decimal(9,2) null,
  non_saleposant decimal(9,2) null,
  non_salenegant decimal(9,2) null,
  dato0 datetime null,
  butikk0 integer null,
  kasse0 integer null,
  kasserernr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kas_rap ON kas_rap for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kas_rap t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kas_rap#_#progress_recid ON kas_rap (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kas_rap#_#progress_recid_ident_ ON kas_rap (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kas_rap##kasserer ON kas_rap (dato, butikk, kasse, kasserernr, z_nummer)
go
CREATE INDEX kas_rap##kas_rap ON kas_rap (dato, butikk, kasse, z_nummer, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'katalogarkiv' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table katalogarkiv
go
CREATE TABLE katalogarkiv (
  katalog varchar (15) not null,
  fullpath varchar (30) null,
  datoopprettet datetime null,
  merknad varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_katalogarkiv ON katalogarkiv for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from katalogarkiv t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX katalogarkiv#_#progress_recid ON katalogarkiv (PROGRESS_RECID)
go
CREATE UNIQUE INDEX katalogarkiv#_#progress_recid_ident_ ON katalogarkiv (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX katalogarkiv##katnavn ON katalogarkiv (katalog)
go
CREATE INDEX katalogarkiv##merknad ON katalogarkiv (merknad, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kategori' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kategori
go
CREATE TABLE kategori (
  katnr integer null,
  beskrivelse varchar (30) null,
  merknad varchar (50) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kategori ON kategori for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kategori t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kategori#_#progress_recid ON kategori (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kategori#_#progress_recid_ident_ ON kategori (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kategori##beskrivelse ON kategori (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kategori##katnr ON kategori (katnr)
go
if (select name from sysobjects 
    where name = 'kjede' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kjede
go
CREATE TABLE kjede (
  kjedenr integer null,
  kjedenavn varchar (30) null,
  kjedeknavn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kjede ON kjede for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kjede t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kjede#_#progress_recid ON kjede (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kjede#_#progress_recid_ident_ ON kjede (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kjede##kjede ON kjede (kjedenr)
go
CREATE INDEX kjede##kjedeknav ON kjede (kjedeknavn, PROGRESS_RECID)
go
CREATE INDEX kjede##kjedenavn ON kjede (kjedenavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kjededistrikt' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kjededistrikt
go
CREATE TABLE kjededistrikt (
  kjedenr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  regionnr integer null,
  distriktnavn varchar (30) null,
  distriktknavn varchar (30) null,
  kontaktperson varchar (30) null,
  telefon varchar (20) null,
  mombil varchar (30) null,
  email varchar (30) null,
  distriktnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kjededistrikt ON kjededistrikt for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kjededistrikt t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kjededistrikt#_#progress_recid ON kjededistrikt (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kjededistrikt#_#progress_recid_ident_ ON kjededistrikt (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kjededistrikt##distriktknavn ON kjededistrikt (kjedenr, regionnr, distriktknavn, PROGRESS_RECID)
go
CREATE INDEX kjededistrikt##distriktnavn ON kjededistrikt (kjedenr, regionnr, distriktnavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kjededistrikt##kjededistrikt ON kjededistrikt (kjedenr, regionnr, distriktnr)
go
if (select name from sysobjects 
    where name = 'kjedensbutikker' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kjedensbutikker
go
CREATE TABLE kjedensbutikker (
  butikknr integer null,
  butikknavn varchar (30) null,
  kontaktperson varchar (30) null,
  e_mail varchar (30) null,
  telefon varchar (25) null,
  mobil varchar (25) null,
  telefaks varchar (25) null,
  postnr varchar (10) null,
  firmanavn varchar (30) null,
  dagligleder varchar (30) null,
  adresse1 varchar (30) null,
  adresse2 varchar (30) null,
  medlemsstatus integer null,
  kjedenr integer null,
  regionnr integer null,
  distriktnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  segmentkode integer null,
  organisasjonsnr varchar (12) null,
  oppstartbutikkdata datetime null,
  driftsformid integer null,
  driftstypeid integer null,
  beliggenhetid integer null,
  utmeldtdato datetime null,
  lgid integer null,
  fakturaadresse1 varchar (40) null,
  fakturaadresse2 varchar (40) null,
  fakturapostnr varchar (10) null,
  fakturapostboks varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kjedensbutikker ON kjedensbutikker for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kjedensbutikker t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kjedensbutikker#_#progress_recid ON kjedensbutikker (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kjedensbutikker#_#progress_recid_ident_ ON kjedensbutikker (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kjedensbutikker##adresse1 ON kjedensbutikker (adresse1, PROGRESS_RECID)
go
CREATE INDEX kjedensbutikker##butikknavn ON kjedensbutikker (butikknavn, PROGRESS_RECID)
go
CREATE INDEX kjedensbutikker##dagligleder ON kjedensbutikker (dagligleder, PROGRESS_RECID)
go
CREATE INDEX kjedensbutikker##firmanavn ON kjedensbutikker (firmanavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kjedensbutikker##kjedensbutikker ON kjedensbutikker (butikknr)
go
CREATE INDEX kjedensbutikker##kjedestruktur ON kjedensbutikker (kjedenr, regionnr, distriktnr, butikknr, PROGRESS_RECID)
go
CREATE INDEX kjedensbutikker##kontaktperson ON kjedensbutikker (kontaktperson, PROGRESS_RECID)
go
CREATE INDEX kjedensbutikker##lokalgruppering ON kjedensbutikker (lgid, PROGRESS_RECID)
go
CREATE INDEX kjedensbutikker##medlemsstatus ON kjedensbutikker (medlemsstatus, PROGRESS_RECID)
go
CREATE INDEX kjedensbutikker##segmentkode ON kjedensbutikker (segmentkode, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kjederegion' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kjederegion
go
CREATE TABLE kjederegion (
  kjedenr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  regionnr integer null,
  regionnavn varchar (30) null,
  regionknavn varchar (30) null,
  kontaktperson varchar (30) null,
  telefon varchar (20) null,
  mombil varchar (30) null,
  email varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kjederegion ON kjederegion for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kjederegion t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kjederegion#_#progress_recid ON kjederegion (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kjederegion#_#progress_recid_ident_ ON kjederegion (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kjederegion##kjedeknavn ON kjederegion (kjedenr, regionknavn, PROGRESS_RECID)
go
CREATE INDEX kjederegion##kjedenavn ON kjederegion (kjedenr, regionnavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kjederegion##kjederegion ON kjederegion (kjedenr, regionnr)
go
if (select name from sysobjects 
    where name = 'klack' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table klack
go
CREATE TABLE klack (
  beskrivning varchar (20) null,
  klack_id integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_klack ON klack for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from klack t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX klack#_#progress_recid ON klack (PROGRESS_RECID)
go
CREATE UNIQUE INDEX klack#_#progress_recid_ident_ ON klack (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX klack##klack ON klack (klack_id)
go
CREATE INDEX klack##klack_txt ON klack (beskrivning, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kommune' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kommune
go
CREATE TABLE kommune (
  kommnr varchar (30) null,
  beskrivelse varchar (30) null,
  merknad varchar (50) null,
  fylkesnr varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kommune ON kommune for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kommune t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kommune#_#progress_recid ON kommune (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kommune#_#progress_recid_ident_ ON kommune (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kommune##beskrivelse ON kommune (beskrivelse, PROGRESS_RECID)
go
CREATE INDEX kommune##fylkesnr ON kommune (fylkesnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kommune##kommnr ON kommune (kommnr)
go
if (select name from sysobjects 
    where name = 'konto' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table konto
go
CREATE TABLE konto (
  butikk integer null,
  dato datetime null,
  kontonummer integer null,
  vg integer null,
  lopnr integer null,
  storl varchar (10) null,
  pris decimal(7,2) null,
  antall integer null,
  kvitto integer null,
  kasse integer null,
  forsnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_konto ON konto for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from konto t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX konto#_#progress_recid ON konto (PROGRESS_RECID)
go
CREATE UNIQUE INDEX konto#_#progress_recid_ident_ ON konto (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX konto##konto ON konto (dato, butikk, kontonummer, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kontotabell' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kontotabell
go
CREATE TABLE kontotabell (
  kontonr integer null,
  kontonavn varchar (40) null,
  debkred integer null,
  notat varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kontotabell ON kontotabell for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kontotabell t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kontotabell#_#progress_recid ON kontotabell (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kontotabell#_#progress_recid_ident_ ON kontotabell (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kontotabell##kontonavn ON kontotabell (kontonavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kontotabell##kontonr ON kontotabell (kontonr)
go
if (select name from sysobjects 
    where name = 'kont_mal' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kont_mal
go
CREATE TABLE kont_mal (
  butikk integer null,
  k1 integer null,
  k2 integer null,
  k3 integer null,
  k4 integer null,
  k5 integer null,
  k6 integer null,
  k7 integer null,
  k8 integer null,
  k9 integer null,
  n1 varchar (15) null,
  n2 varchar (15) null,
  n3 varchar (15) null,
  n4 varchar (15) null,
  n5 varchar (15) null,
  n6 varchar (15) null,
  n7 varchar (15) null,
  n8 varchar (15) null,
  n9 varchar (15) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kont_mal ON kont_mal for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kont_mal t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kont_mal#_#progress_recid ON kont_mal (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kont_mal#_#progress_recid_ident_ ON kont_mal (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kont_mal##kont_mal ON kont_mal (butikk)
go
if (select name from sysobjects 
    where name = 'konvreg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table konvreg
go
CREATE TABLE konvreg (
  edb_system varchar (15) null,
  tabell varchar (15) null,
  ekstid varchar (30) null,
  interntid varchar (30) null,
  divtekst varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_konvreg ON konvreg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from konvreg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX konvreg#_#progress_recid ON konvreg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX konvreg#_#progress_recid_ident_ ON konvreg (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX konvreg##eksternt ON konvreg (edb_system, tabell, ekstid)
go
CREATE UNIQUE INDEX konvreg##interntid ON konvreg (edb_system, tabell, interntid)
go
if (select name from sysobjects 
    where name = 'kordrehode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kordrehode
go
CREATE TABLE kordrehode (
  kordre_id decimal(15,2) not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kontonr integer null,
  kundenr decimal(15,2) null,
  betaltdato datetime null,
  navn varchar (40) null,
  adresse1 varchar (40) null,
  adresse2 varchar (40) null,
  postnr varchar (10) null,
  telefon varchar (15) null,
  kontnavn varchar (40) null,
  butikknr integer null,
  avgplsalg decimal(10,2) null,
  avgfrisalg decimal(10,2) null,
  mva decimal(10,2) null,
  totalt decimal(11,2) null,
  avrund decimal(7,2) null,
  dato datetime null,
  antdager integer null,
  telefaks varchar (15) null,
  faktadresse1 varchar (30) null,
  faktadresse2 varchar (30) null,
  faktpostnr varchar (15) null,
  konttelefon varchar (15) null,
  fakttekstnr integer null,
  totalrabatt_ decimal(4,2) null,
  totalrabattkr decimal(8,2) null,
  mvakr decimal(8,2) null,
  fakturertdato datetime null,
  fakturerttid integer null,
  fakturertav varchar (20) null,
  forfallsdato datetime null,
  produksjonsdato datetime null,
  butik integer null,
  levfnr integer null,
  betbet integer null,
  kprosjektnr integer null,
  sendingsnr varchar (30) null,
  embalage varchar (30) null,
  fraktbrevtekst varchar (30) null,
  godsmerking varchar (30) null,
  utsendelsesdato datetime null,
  antkolli integer null,
  bruttovekt decimal(7,2) null,
  totaltvolum decimal(7,2) null,
  valkod varchar (3) null,
  vaarref varchar (30) null,
  deresref varchar (30) null,
  referanse varchar (30) null,
  avrundingtype integer null,
  avrundingkr integer null,
  levadresse1 varchar (40) null,
  levadresse2 varchar (40) null,
  levpostnr varchar (10) null,
  levland varchar (30) null,
  levpoststed varchar (30) null,
  poststed varchar (30) null,
  faktpoststed varchar (30) null,
  faktland varchar (30) null,
  firmanavn varchar (30) null,
  firmaadresse1 varchar (30) null,
  firmaadresse2 varchar (30) null,
  firmapostnr varchar (10) null,
  firmapoststed varchar (30) null,
  firmatelefon varchar (15) null,
  firmatelefaks varchar (15) null,
  firmaepost varchar (40) null,
  firmaorganisasjonsnr varchar (12) null,
  firmabankkonto varchar (20) null,
  firmapostgiro varchar (20) null,
  firmaland varchar (30) null,
  firmaurladresse varchar (40) null,
  leveringsdato datetime null,
  levstatus varchar (2) null,
  prodstatus varchar (1) null,
  verkstedordre tinyint null,
  epostadresse varchar (40) null,
  kassenr integer not null,
  ansvverksted varchar (30) null,
  verkstedmerknad varchar (80) null,
  bettekst varchar (30) null,
  forsnr integer null,
  selgernr decimal(15,2) null,
  ekstordrenr varchar (15) null,
  opphav integer null,
  faktura_id decimal(15,2) null,
  internmerknad varchar (40) null,
  kundemerknad varchar (40) null,
  mobiltlf varchar (15) null,
  svarfrist datetime null,
  avdelingnr integer null,
  refkordre_id decimal(15,2) not null,
  datotidopprettet datetime null,
  datotidendret datetime null,
  leveresdatotid datetime null,
  copt1 varchar (30) null,
  dopt1 decimal(7,2) null,
  iopt1 integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kordrehode ON kordrehode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kordrehode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kordrehode#_#progress_recid ON kordrehode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kordrehode#_#progress_recid_ident_ ON kordrehode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kordrehode##datotidendret ON kordrehode (datotidendret, PROGRESS_RECID)
go
CREATE INDEX kordrehode##datotidopprettet ON kordrehode (datotidopprettet, PROGRESS_RECID)
go
CREATE INDEX kordrehode##ekstordrenr ON kordrehode (registrertdato, ekstordrenr, PROGRESS_RECID)
go
CREATE INDEX kordrehode##fakttekst ON kordrehode (fakttekstnr, PROGRESS_RECID)
go
CREATE INDEX kordrehode##faktura_id ON kordrehode (faktura_id, PROGRESS_RECID)
go
CREATE INDEX kordrehode##kasserer ON kordrehode (forsnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kordrehode##kordrehode ON kordrehode (kordre_id)
go
CREATE INDEX kordrehode##kunde ON kordrehode (kundenr, kordre_id, PROGRESS_RECID)
go
CREATE INDEX kordrehode##leveresdatotid ON kordrehode (leveresdatotid, PROGRESS_RECID)
go
CREATE INDEX kordrehode##opphav ON kordrehode (opphav, PROGRESS_RECID)
go
CREATE INDEX kordrehode##refkordre ON kordrehode (refkordre_id, PROGRESS_RECID)
go
CREATE INDEX kordrehode##selger ON kordrehode (selgernr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kordrelinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kordrelinje
go
CREATE TABLE kordrelinje (
  kordre_id decimal(15,2) not null,
  kordrelinjenr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  varenr varchar (20) null,
  varetekst varchar (30) null,
  antall decimal(7,2) null,
  linjerab_ decimal(7,2) null,
  linjerabattkr decimal(8,2) null,
  ordrerabattkr decimal(8,2) null,
  nettopris decimal(8,2) null,
  momskod integer null,
  mvakr decimal(8,2) null,
  nettolinjesum decimal(8,2) null,
  notat varchar (40) null,
  leveringsdato datetime null,
  db_ decimal(7,2) null,
  dbkr decimal(8,2) null,
  faktura_id decimal(15,2) not null,
  storl varchar (10) null,
  mva_ decimal(7,2) null,
  strkode integer null,
  pakkeidx integer null,
  bruttopris decimal(8,2) null,
  arbeidsbeskr varchar (40) null,
  varespesifikasjon varchar (30) null,
  pris decimal(7,2) null,
  varekost decimal(9,2) null,
  depositum decimal(7,2) null,
  levfargkod varchar (15) null,
  refnr integer null,
  reftekst varchar (40) null,
  kunderab_ decimal(6,2) null,
  linjesum decimal(8,2) null,
  kunderabattkr decimal(8,2) null,
  tilbud tinyint null,
  bestillingsnummer varchar (25) null,
  varebehnr decimal(15,2) null,
  bestnr integer null,
  valkod varchar (3) null,
  plukkbutikk integer null,
  utleverbutikk integer null,
  plockstatus integer null,
  plockdatetime datetime null,
  datotidopprettet datetime null,
  kode varchar (20) null,
  returkodeid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kordrelinje ON kordrelinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kordrelinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kordrelinje#_#progress_recid ON kordrelinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kordrelinje#_#progress_recid_ident_ ON kordrelinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kordrelinje##bestnr ON kordrelinje (bestnr, PROGRESS_RECID)
go
CREATE INDEX kordrelinje##butikkplstatus ON kordrelinje (plukkbutikk, plockstatus, PROGRESS_RECID)
go
CREATE INDEX kordrelinje##datotidopprettet ON kordrelinje (datotidopprettet, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kordrelinje##faktlinje ON kordrelinje (kordre_id, kordrelinjenr)
go
CREATE INDEX kordrelinje##leveringsdato ON kordrelinje (leveringsdato, PROGRESS_RECID)
go
CREATE INDEX kordrelinje##pakkeidx ON kordrelinje (pakkeidx, PROGRESS_RECID)
go
CREATE INDEX kordrelinje##plockstatus ON kordrelinje (plockstatus, PROGRESS_RECID)
go
CREATE INDEX kordrelinje##plukkbutikk ON kordrelinje (kordre_id, plukkbutikk, PROGRESS_RECID)
go
CREATE INDEX kordrelinje##suppleringsordre ON kordrelinje (varebehnr, PROGRESS_RECID)
go
CREATE INDEX kordrelinje##utleverbutikk ON kordrelinje (kordre_id, utleverbutikk, PROGRESS_RECID)
go
CREATE INDEX kordrelinje##varetekst ON kordrelinje (kordre_id, varetekst, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kordrelinjerejectplock' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kordrelinjerejectplock
go
CREATE TABLE kordrelinjerejectplock (
  kordre_id decimal(15,2) not null,
  kordrelinjenr integer null,
  plukkbutikk integer null,
  storl varchar (10) null,
  plockstatus integer null,
  plockdatetime datetime null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kordrelinjerejectplock ON kordrelinjerejectplock for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kordrelinjerejectplock t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kordrelinjerejectplock#_#progress_recid ON kordrelinjerejectplock (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kordrelinjerejectplock#_#progress_recid_ident_ ON kordrelinjerejectplock (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kordrelinjerejectplock##butik ON kordrelinjerejectplock (kordre_id, kordrelinjenr, plukkbutikk, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kort_spes' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kort_spes
go
CREATE TABLE kort_spes (
  dato datetime null,
  butikk integer null,
  kasse integer null,
  z_nummer integer null,
  antkort integer null,
  korttype integer null,
  belop decimal(9,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kasserernr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kort_spes ON kort_spes for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kort_spes t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kort_spes#_#progress_recid ON kort_spes (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kort_spes#_#progress_recid_ident_ ON kort_spes (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kort_spes##kortspes ON kort_spes (dato, butikk, kasse, kasserernr, z_nummer, korttype)
go
CREATE INDEX kort_spes##spes ON kort_spes (dato, butikk, kasse, z_nummer, korttype, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kravkode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kravkode
go
CREATE TABLE kravkode (
  kravkode integer null,
  beskrivelse varchar (40) null,
  notat varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  transkode integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kravkode ON kravkode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kravkode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kravkode#_#progress_recid ON kravkode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kravkode#_#progress_recid_ident_ ON kravkode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kravkode##beskrivelse ON kravkode (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kravkode##kravkode ON kravkode (kravkode)
go
if (select name from sysobjects 
    where name = 'kunde' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kunde
go
CREATE TABLE kunde (
  kundenr decimal(15,2) null,
  navn varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  typeid integer null,
  gruppeid integer null,
  adresse1 varchar (40) null,
  adresse2 varchar (40) null,
  postnr varchar (10) null,
  telefon varchar (15) null,
  telefaks varchar (15) null,
  mobiltlf varchar (15) null,
  kontnavn varchar (40) null,
  konttelefon varchar (15) null,
  stilling varchar (30) null,
  konttelefaks varchar (15) null,
  kontmobiltlf varchar (15) null,
  levadresse1 varchar (30) null,
  levadresse2 varchar (30) null,
  levpostnr varchar (10) null,
  levland varchar (30) null,
  land varchar (30) null,
  makskredit decimal(9,2) null,
  kreditsperret tinyint null,
  opphort datetime null,
  butikknr integer null,
  bydelsnr varchar (30) null,
  epostadresse varchar (40) null,
  konte_post varchar (40) null,
  orgnr varchar (15) null,
  totalrabatt_ decimal(4,2) null,
  bankkonto varchar (20) null,
  postgiro varchar (20) null,
  betbet integer null,
  etablert datetime null,
  samlefaktura tinyint null,
  privattlf varchar (15) null,
  kjon integer null,
  fodtdato datetime null,
  alder integer null,
  faktadresse1 varchar (30) null,
  faktadresse2 varchar (30) null,
  faktpostnr varchar (15) null,
  fakttekstnr integer null,
  deresref varchar (30) null,
  privat tinyint null,
  faktland varchar (30) null,
  valkod varchar (3) null,
  bettype integer null,
  kundesaldo decimal(9,2) null,
  forstekjop datetime null,
  sistekjop datetime null,
  purregebyr tinyint null,
  fakturagebyr tinyint null,
  webkunde tinyint null,
  aktiv tinyint null,
  hovedkunde tinyint null,
  koblettilkunde decimal(15,2) null,
  faktureringsperiode integer null,
  kilde varchar (30) null,
  tilgkilde varchar (30) null,
  eksterntkundenr varchar (20) null,
  momskod integer null,
  bynavn varchar (30) null,
  avdeling varchar (30) null,
  tittel varchar (10) null,
  hilsen varchar (5) null,
  emailfirma varchar (40) null,
  telefonfirma varchar (15) null,
  banknavn varchar (30) null,
  bankkode varchar (30) null,
  webkansetteordre tinyint null,
  kommentar varchar (40) null,
  webkansendeemail tinyint null,
  urlfirma varchar (40) null,
  region varchar (30) null,
  fakturadeltajniva integer null,
  mvafri tinyint null,
  mottaemailutsendelser tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kunde ON kunde for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kunde t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kunde#_#progress_recid ON kunde (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kunde#_#progress_recid_ident_ ON kunde (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kunde##aktiv ON kunde (aktiv, PROGRESS_RECID)
go
CREATE INDEX kunde##betalignstype ON kunde (bettype, PROGRESS_RECID)
go
CREATE INDEX kunde##butikknr ON kunde (butikknr, PROGRESS_RECID)
go
CREATE INDEX kunde##eksterntkundenr ON kunde (eksterntkundenr, PROGRESS_RECID)
go
CREATE INDEX kunde##forstekjop ON kunde (forstekjop, PROGRESS_RECID)
go
CREATE INDEX kunde##gruppeid ON kunde (gruppeid, navn, PROGRESS_RECID)
go
CREATE INDEX kunde##hovedkunde ON kunde (koblettilkunde, PROGRESS_RECID)
go
CREATE INDEX kunde##koblettilhovedkunde ON kunde (koblettilkunde, PROGRESS_RECID)
go
CREATE INDEX kunde##kontakt ON kunde (kontnavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kunde##kunde ON kunde (kundenr)
go
CREATE INDEX kunde##momskod ON kunde (momskod, PROGRESS_RECID)
go
CREATE INDEX kunde##navn ON kunde (navn, PROGRESS_RECID)
go
CREATE INDEX kunde##postnr ON kunde (postnr, navn, PROGRESS_RECID)
go
CREATE INDEX kunde##privat ON kunde (privat, PROGRESS_RECID)
go
CREATE INDEX kunde##saldo ON kunde (kundesaldo, PROGRESS_RECID)
go
CREATE INDEX kunde##sistekjop ON kunde (sistekjop, PROGRESS_RECID)
go
CREATE INDEX kunde##totalrabatt_ ON kunde (totalrabatt_, PROGRESS_RECID)
go
CREATE INDEX kunde##typeid ON kunde (typeid, navn, PROGRESS_RECID)
go
CREATE INDEX kunde##valkod ON kunde (valkod, PROGRESS_RECID)
go
CREATE INDEX kunde##webkunde ON kunde (webkunde, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kundebettrans' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kundebettrans
go
CREATE TABLE kundebettrans (
  batchnr integer null,
  butik integer null,
  transnr integer null,
  ttid integer not null,
  tbid integer null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  bongid integer null,
  bonglinjenr integer null,
  kassanr integer null,
  belop decimal(10,2) null,
  dato datetime null,
  tid integer null,
  seqnr integer null,
  kundenr decimal(15,2) null,
  kortnr varchar (22) null,
  medlemsnr decimal(15,2) null,
  forsnr decimal(8,2) null,
  splittet tinyint null,
  orgbelop decimal(10,2) null,
  avregnet tinyint null,
  fakturanr decimal(11,2) null,
  betbutik integer null,
  betkassanr integer null,
  betbongid integer null,
  notat varchar (40) null,
  kontonr integer null,
  korttype integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  motpostert tinyint null,
  selgernr decimal(15,2) null,
  reftekst varchar (40) null,
  refnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kundebettrans ON kundebettrans for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kundebettrans t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kundebettrans#_#progress_recid ON kundebettrans (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundebettrans#_#progress_recid_ident_ ON kundebettrans (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kundebettrans##batchlogg ON kundebettrans (kundenr, batchnr, butik, transnr, seqnr, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##belop ON kundebettrans (kundenr, belop, dato, tid, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##betkvittering ON kundebettrans (betbutik, betkassanr, betbongid, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##butikkdato ON kundebettrans (butik, dato, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##datotid ON kundebettrans (kundenr, dato, tid, butik, ttid, tbid, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##forsnr ON kundebettrans (kundenr, forsnr, butik, dato, tid, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##kontonr ON kundebettrans (kundenr, kontonr, dato, tid, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##kortnr ON kundebettrans (kundenr, kortnr, dato, tid, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##kundebettrans ON kundebettrans (kundenr, butik, transnr, seqnr, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##kvittering ON kundebettrans (butik, kassanr, bongid, bonglinjenr, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##medlem ON kundebettrans (kundenr, medlemsnr, dato, tid, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##motpostert ON kundebettrans (kundenr, motpostert, PROGRESS_RECID)
go
CREATE INDEX kundebettrans##transtype ON kundebettrans (kundenr, ttid, tbid, dato, tid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kundegruppe' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kundegruppe
go
CREATE TABLE kundegruppe (
  gruppeid integer null,
  beskrivelse varchar (40) null,
  notat varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kundegruppe ON kundegruppe for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kundegruppe t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kundegruppe#_#progress_recid ON kundegruppe (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundegruppe#_#progress_recid_ident_ ON kundegruppe (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kundegruppe##beskrivelse ON kundegruppe (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundegruppe##kundegruppe ON kundegruppe (gruppeid)
go
if (select name from sysobjects 
    where name = 'kundekommentar' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kundekommentar
go
CREATE TABLE kundekommentar (
  kundenr decimal(15,2) null,
  kommentarid integer null,
  kundekommentar varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kundekommentar ON kundekommentar for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kundekommentar t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kundekommentar#_#progress_recid ON kundekommentar (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundekommentar#_#progress_recid_ident_ ON kundekommentar (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kundekommentar##kunde ON kundekommentar (kundenr, kommentarid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundekommentar##kundekommentar ON kundekommentar (kommentarid)
go
if (select name from sysobjects 
    where name = 'kundekort' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kundekort
go
CREATE TABLE kundekort (
  kundenr decimal(15,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kortnr varchar (22) null,
  merknad varchar (40) null,
  aktivertdato datetime null,
  utgardato datetime null,
  sperret tinyint null,
  innehaver varchar (30) null,
  medlemsnr decimal(15,2) null,
  interntkkortid decimal(15,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kundekort ON kundekort for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kundekort t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kundekort#_#progress_recid ON kundekort (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundekort#_#progress_recid_ident_ ON kundekort (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kundekort##innehaver ON kundekort (kundenr, innehaver, PROGRESS_RECID)
go
CREATE INDEX kundekort##interntkkortid ON kundekort (interntkkortid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundekort##kortidx ON kundekort (kortnr)
go
CREATE UNIQUE INDEX kundekort##kundekort ON kundekort (kundenr, kortnr)
go
CREATE INDEX kundekort##medlem_kunde ON kundekort (medlemsnr, kundenr, PROGRESS_RECID)
go
CREATE INDEX kundekort##merknad ON kundekort (kundenr, merknad, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kundeprosjekt' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kundeprosjekt
go
CREATE TABLE kundeprosjekt (
  kundenr decimal(15,2) null,
  kprosjektnr integer null,
  kpbeskrivelse varchar (30) null,
  kpstartdato datetime null,
  kpsluttdato datetime null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kundeprosjekt ON kundeprosjekt for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kundeprosjekt t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kundeprosjekt#_#progress_recid ON kundeprosjekt (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundeprosjekt#_#progress_recid_ident_ ON kundeprosjekt (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kundeprosjekt##beskrivelse ON kundeprosjekt (kundenr, kpbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundeprosjekt##kundeprosjekt ON kundeprosjekt (kundenr, kprosjektnr)
go
if (select name from sysobjects 
    where name = 'kundereskobling' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kundereskobling
go
CREATE TABLE kundereskobling (
  dreskontro_id decimal(15,2) null,
  kreskontro_id decimal(15,2) null,
  belop decimal(9,2) null,
  dato datetime null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kundereskobling ON kundereskobling for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kundereskobling t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kundereskobling#_#progress_recid ON kundereskobling (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundereskobling#_#progress_recid_ident_ ON kundereskobling (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kundereskobling##kredit ON kundereskobling (kreskontro_id, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundereskobling##kundereskobling ON kundereskobling (dreskontro_id, kreskontro_id)
go
if (select name from sysobjects 
    where name = 'kundereskontr' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kundereskontr
go
CREATE TABLE kundereskontr (
  reskontro_id decimal(15,2) null,
  bartnr integer not null,
  fakturanr decimal(13,0) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kundenr decimal(15,2) null,
  fakturertdato datetime null,
  forfallsdato datetime null,
  opprforfallsdato datetime null,
  purretrinn integer null,
  sistepurredato datetime null,
  reklamert datetime null,
  notat varchar (30) null,
  regnskapsperiode integer null,
  bilagsnr decimal(15,2) null,
  buntnr decimal(15,2) null,
  overfortregnskap datetime null,
  kid decimal(24,2) null,
  belop decimal(10,2) null,
  saldo decimal(10,2) null,
  ferdigbeh tinyint null,
  ferdigdato datetime null,
  bilagstype integer null,
  sendtinkasso datetime null,
  b_id decimal(15,2) null,
  bonglinjenr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kundereskontr ON kundereskontr for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kundereskontr t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kundereskontr#_#progress_recid ON kundereskontr (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundereskontr#_#progress_recid_ident_ ON kundereskontr (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kundereskontr##bong ON kundereskontr (b_id, bonglinjenr, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##bunt ON kundereskontr (buntnr, bilagsnr, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##fakturanr ON kundereskontr (bilagstype, fakturanr, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##fakturet ON kundereskontr (fakturertdato, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##ferdig ON kundereskontr (ferdigbeh, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##forfall ON kundereskontr (forfallsdato, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##inkasso ON kundereskontr (sendtinkasso, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##kid ON kundereskontr (kid, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##kunde ON kundereskontr (kundenr, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##kundebilagdato ON kundereskontr (kundenr, bilagstype, fakturertdato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundereskontr##kundereskontro ON kundereskontr (reskontro_id)
go
CREATE INDEX kundereskontr##overfregnskap ON kundereskontr (overfortregnskap, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##purretrinn ON kundereskontr (purretrinn, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##regnskapsperiode ON kundereskontr (regnskapsperiode, PROGRESS_RECID)
go
CREATE INDEX kundereskontr##reklamert ON kundereskontr (reklamert, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kundesaldo' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kundesaldo
go
CREATE TABLE kundesaldo (
  kundenr decimal(15,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  butikknr integer null,
  forstedato datetime null,
  datosiste datetime null,
  forstetid integer null,
  sistetid integer null,
  saldo decimal(11,2) null,
  totaltkjop decimal(11,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kundesaldo ON kundesaldo for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kundesaldo t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kundesaldo#_#progress_recid ON kundesaldo (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundesaldo#_#progress_recid_ident_ ON kundesaldo (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kundesaldo##forste ON kundesaldo (kundenr, forstedato, forstetid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundesaldo##kundesaldo ON kundesaldo (kundenr, butikknr)
go
CREATE INDEX kundesaldo##siste ON kundesaldo (kundenr, datosiste, sistetid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kundetrans' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kundetrans
go
CREATE TABLE kundetrans (
  batchnr integer null,
  butik integer null,
  transnr integer null,
  ttid integer not null,
  tbid integer null,
  artikkelnr decimal(13,0) null,
  levnr integer null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  bongid integer null,
  bonglinjenr integer null,
  kassanr integer null,
  vg integer null,
  lopnr integer null,
  storl varchar (10) null,
  antall decimal(10,2) null,
  pris decimal(10,2) null,
  rabkr decimal(9,2) null,
  mva decimal(9,2) null,
  dato datetime null,
  tid integer null,
  seqnr integer null,
  vvarekost decimal(11,4) null,
  sattvvarekost tinyint null,
  kundenr decimal(15,2) null,
  kortnr varchar (22) null,
  meldemsnr decimal(15,2) null,
  forsnr decimal(8,2) null,
  kontonr integer null,
  motpostert tinyint null,
  fakturanr decimal(11,2) null,
  selgernr decimal(15,2) null,
  subtotalrab decimal(9,2) null,
  reftekst varchar (40) null,
  refnr integer null,
  bongtekst varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kundetrans ON kundetrans for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kundetrans t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kundetrans#_#progress_recid ON kundetrans (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundetrans#_#progress_recid_ident_ ON kundetrans (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kundetrans##artbutstr ON kundetrans (kundenr, artikkelnr, butik, storl, PROGRESS_RECID)
go
CREATE INDEX kundetrans##artikkelnr ON kundetrans (artikkelnr, PROGRESS_RECID)
go
CREATE INDEX kundetrans##batchlogg ON kundetrans (kundenr, batchnr, butik, transnr, seqnr, PROGRESS_RECID)
go
CREATE INDEX kundetrans##bongtekst ON kundetrans (kundenr, bongtekst, PROGRESS_RECID)
go
CREATE INDEX kundetrans##butartstr ON kundetrans (kundenr, butik, artikkelnr, storl, PROGRESS_RECID)
go
CREATE INDEX kundetrans##butikkdato ON kundetrans (butik, dato, PROGRESS_RECID)
go
CREATE INDEX kundetrans##butvglopnrstrl ON kundetrans (kundenr, butik, vg, lopnr, storl, PROGRESS_RECID)
go
CREATE INDEX kundetrans##faktura ON kundetrans (fakturanr, PROGRESS_RECID)
go
CREATE INDEX kundetrans##kontonr ON kundetrans (kundenr, kontonr, dato, tid, PROGRESS_RECID)
go
CREATE INDEX kundetrans##kortnr ON kundetrans (kortnr, PROGRESS_RECID)
go
CREATE INDEX kundetrans##kundedato ON kundetrans (kundenr, dato, tid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundetrans##kundetrans ON kundetrans (kundenr, butik, transnr, seqnr)
go
CREATE INDEX kundetrans##motpostert ON kundetrans (kundenr, motpostert, PROGRESS_RECID)
go
CREATE INDEX kundetrans##oppslagdatotid ON kundetrans (kundenr, artikkelnr, dato, tid, butik, ttid, tbid, PROGRESS_RECID)
go
CREATE INDEX kundetrans##oppslagselger ON kundetrans (kundenr, forsnr, artikkelnr, butik, dato, tid, ttid, tbid, PROGRESS_RECID)
go
CREATE INDEX kundetrans##oppslagstr ON kundetrans (kundenr, artikkelnr, storl, butik, dato, tid, ttid, PROGRESS_RECID)
go
CREATE INDEX kundetrans##transtype ON kundetrans (ttid, PROGRESS_RECID)
go
CREATE INDEX kundetrans##vglopnrstrbut ON kundetrans (kundenr, vg, lopnr, storl, butik, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kundetype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kundetype
go
CREATE TABLE kundetype (
  typeid integer null,
  beskrivelse varchar (40) null,
  notat varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kundetype ON kundetype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kundetype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kundetype#_#progress_recid ON kundetype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundetype#_#progress_recid_ident_ ON kundetype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kundetype##beskrivelse ON kundetype (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kundetype##kundetype ON kundetype (typeid)
go
if (select name from sysobjects 
    where name = 'kupong' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kupong
go
CREATE TABLE kupong (
  kupongid decimal(15,2) null,
  kupbeskrivelse varchar (30) null,
  belop decimal(7,2) null,
  maksbelop decimal(7,2) null,
  minbelop decimal(7,2) null,
  eankode varchar (20) null,
  gyldigfra datetime null,
  gyldigtil datetime null,
  aktiv tinyint null,
  idkrav tinyint null,
  interleavekode varchar (20) null,
  ktypenr integer not null,
  rabattverdi decimal(7,2) null,
  sisteinnldato datetime null,
  tavarepakupong tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kupongnotat varchar (60) null,
  keiernr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kupong ON kupong for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kupong t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kupong#_#progress_recid ON kupong (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kupong#_#progress_recid_ident_ ON kupong (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kupong##aktiv ON kupong (aktiv, PROGRESS_RECID)
go
CREATE INDEX kupong##eankode ON kupong (eankode, PROGRESS_RECID)
go
CREATE INDEX kupong##gyldigfra ON kupong (gyldigfra, PROGRESS_RECID)
go
CREATE INDEX kupong##gyldigtil ON kupong (gyldigtil, PROGRESS_RECID)
go
CREATE INDEX kupong##interleavekode ON kupong (interleavekode, PROGRESS_RECID)
go
CREATE INDEX kupong##ktypenr ON kupong (ktypenr, PROGRESS_RECID)
go
CREATE INDEX kupong##kupbeskrivelse ON kupong (kupbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kupong##kupong ON kupong (kupongid)
go
if (select name from sysobjects 
    where name = 'kupongeier' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kupongeier
go
CREATE TABLE kupongeier (
  keiernr integer null,
  kenavn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kupongeier ON kupongeier for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kupongeier t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kupongeier#_#progress_recid ON kupongeier (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kupongeier#_#progress_recid_ident_ ON kupongeier (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kupongeier##kenavn ON kupongeier (kenavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kupongeier##kupongeier ON kupongeier (keiernr)
go
if (select name from sysobjects 
    where name = 'kupongtranslogg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kupongtranslogg
go
CREATE TABLE kupongtranslogg (
  ktloggid decimal(15,2) null,
  b_id decimal(15,2) null,
  salgsdato datetime null,
  avregnet datetime null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kupbeskrivelse varchar (30) null,
  belopknd decimal(8,2) null,
  belopbut decimal(8,2) null,
  kode varchar (20) null,
  interleave varchar (20) null,
  ktypenr integer null,
  bongtekst varchar (30) null,
  linjenr integer null,
  salgstid integer null,
  butikknr integer null,
  kassenr integer null,
  gruppenr integer null,
  bongnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kupongtranslogg ON kupongtranslogg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kupongtranslogg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kupongtranslogg#_#progress_recid ON kupongtranslogg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kupongtranslogg#_#progress_recid_ident_ ON kupongtranslogg (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kupongtranslogg##avregnet ON kupongtranslogg (avregnet, PROGRESS_RECID)
go
CREATE INDEX kupongtranslogg##b_id ON kupongtranslogg (b_id, PROGRESS_RECID)
go
CREATE INDEX kupongtranslogg##dagsoppgjor ON kupongtranslogg (butikknr, salgsdato, kassenr, gruppenr, bongnr, linjenr, PROGRESS_RECID)
go
CREATE INDEX kupongtranslogg##ktypenr ON kupongtranslogg (butikknr, salgsdato, ktypenr, kassenr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kupongtranslogg##kupongtranslogg ON kupongtranslogg (ktloggid)
go
CREATE INDEX kupongtranslogg##salgsdato ON kupongtranslogg (salgsdato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kupongtype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kupongtype
go
CREATE TABLE kupongtype (
  ktypenr integer not null,
  ktypenavn varchar (30) null,
  ktypebeskrivelse varchar (50) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  ktypekode varchar (2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kupongtype ON kupongtype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kupongtype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kupongtype#_#progress_recid ON kupongtype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kupongtype#_#progress_recid_ident_ ON kupongtype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX kupongtype##ktypenavn ON kupongtype (ktypenavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX kupongtype##kupongtype ON kupongtype (ktypenr)
go
CREATE INDEX kupongtype##kupongtypekode ON kupongtype (ktypekode, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'kupongvarekode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kupongvarekode
go
CREATE TABLE kupongvarekode (
  kupongid decimal(15,2) null,
  kupvarekode varchar (20) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_kupongvarekode ON kupongvarekode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kupongvarekode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kupongvarekode#_#progress_recid ON kupongvarekode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kupongvarekode#_#progress_recid_ident_ ON kupongvarekode (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kupongvarekode##kupongvarekode ON kupongvarekode (kupongid, kupvarekode)
go
if (select name from sysobjects 
    where name = 'lager' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table lager
go
CREATE TABLE lager (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  artikkelnr decimal(13,0) null,
  vvarekost decimal(11,4) null,
  lagant decimal(12,4) null,
  sistinnlevert datetime null,
  butik integer null,
  antsolgt decimal(12,4) null,
  brekkant decimal(9,4) null,
  intant decimal(9,4) null,
  reklant decimal(9,4) null,
  rekllant decimal(9,4) null,
  gjenkjopant decimal(9,4) null,
  retlant decimal(9,4) null,
  kjopant decimal(9,4) null,
  ovant decimal(9,4) null,
  justant decimal(9,4) null,
  justverdi decimal(10,2) null,
  svinnant decimal(9,4) null,
  svinnverdi decimal(10,2) null,
  nedant decimal(9,4) null,
  nedverdi decimal(10,2) null,
  verdisolgt decimal(10,2) null,
  kjopverdi decimal(10,2) null,
  brekkverdi decimal(10,2) null,
  intverdi decimal(10,2) null,
  reklverdi decimal(10,2) null,
  rekllverdi decimal(10,2) null,
  gjenkjopverdi decimal(10,2) null,
  ovverdi decimal(10,2) null,
  verdirabatt decimal(9,2) null,
  antrab decimal(12,4) null,
  svk decimal(10,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_lager ON lager for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from lager t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX lager#_#progress_recid ON lager (PROGRESS_RECID)
go
CREATE UNIQUE INDEX lager#_#progress_recid_ident_ ON lager (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX lager##butikk ON lager (butik, artikkelnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX lager##lager ON lager (artikkelnr, butik)
go
if (select name from sysobjects 
    where name = 'laptop' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table laptop
go
CREATE TABLE laptop (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  laptopnr integer not null,
  navn varchar (30) null,
  merknad varchar (30) null,
  lokdb_katalog varchar (30) null,
  lokdb_navn varchar (20) null,
  datosisoverf datetime null,
  tidsisoverf integer null,
  brukersisoverf varchar (15) null,
  datosistekopi datetime null,
  tidsistkopi integer null,
  brukersistkopi varchar (15) null,
  oppkoblparam varchar (30) null,
  ld_sentraldb varchar (15) null,
  serverdbkopi varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_laptop ON laptop for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from laptop t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX laptop#_#progress_recid ON laptop (PROGRESS_RECID)
go
CREATE UNIQUE INDEX laptop#_#progress_recid_ident_ ON laptop (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX laptop##laptop ON laptop (laptopnr)
go
CREATE INDEX laptop##navn ON laptop (navn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'last_sko' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table last_sko
go
CREATE TABLE last_sko (
  last_id integer null,
  lastbeskr varchar (20) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_last_sko ON last_sko for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from last_sko t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX last_sko#_#progress_recid ON last_sko (PROGRESS_RECID)
go
CREATE UNIQUE INDEX last_sko#_#progress_recid_ident_ ON last_sko (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX last_sko##last_sko ON last_sko (last_id)
go
CREATE INDEX last_sko##last_txt ON last_sko (lastbeskr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'levbas' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table levbas
go
CREATE TABLE levbas (
  levnr integer null,
  levnamn varchar (30) null,
  levadr varchar (40) null,
  levponr varchar (6) null,
  levpadr varchar (20) null,
  levland varchar (15) null,
  levtel varchar (20) null,
  levkon varchar (20) null,
  levsal decimal(8,2) null,
  telefax varchar (15) null,
  telex varchar (15) null,
  kommentar##1 varchar (60) null,
  kommentar##2 varchar (60) null,
  kommentar##3 varchar (60) null,
  kommentar##4 varchar (60) null,
  valkod varchar (3) null,
  koadr varchar (40) null,
  koponr varchar (6) null,
  kopadr varchar (20) null,
  koland varchar (15) null,
  kotel varchar (20) null,
  kotelefax varchar (15) null,
  kotelex varchar (15) null,
  betant integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  bildnr integer null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  notat varchar (30) null,
  visdivinfo##1 tinyint null,
  visdivinfo##2 tinyint null,
  visdivinfo##3 tinyint null,
  visdivinfo##4 tinyint null,
  visdivinfo##5 tinyint null,
  visdivinfo##6 tinyint null,
  visdivinfo##7 tinyint null,
  visdivinfo##8 tinyint null,
  visdivinfo##9 tinyint null,
  visdivinfo##10 tinyint null,
  visdivinfo##11 tinyint null,
  visdivinfo##12 tinyint null,
  visdivinfo##13 tinyint null,
  visdivinfo##14 tinyint null,
  visdivinfo##15 tinyint null,
  visdivinfo##16 tinyint null,
  visdivinfo##17 tinyint null,
  visdivinfo##18 tinyint null,
  visdivinfo##19 tinyint null,
  visdivinfo##20 tinyint null,
  lng varchar (3) null,
  e_mailkontakt varchar (40) null,
  e_maillev varchar (40) null,
  kjedeavtale tinyint null,
  rekladresse1 varchar (30) null,
  rekladresse2 varchar (30) null,
  reklpostnr varchar (10) null,
  reklpostboks varchar (30) null,
  rab1_ decimal(5,2) null,
  rab2_ decimal(5,2) null,
  frakt_ decimal(5,2) null,
  divkost_ decimal(5,2) null,
  rab3_ decimal(5,2) null,
  egetkundenrhoslev varchar (25) null,
  suprab1_ decimal(5,2) null,
  suprab2_ decimal(5,2) null,
  supdivkost_ decimal(5,2) null,
  suprab3_ decimal(5,2) null,
  sendtilerp tinyint null,
  kundenr decimal(15,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_levbas ON levbas for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from levbas t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX levbas#_#progress_recid ON levbas (PROGRESS_RECID)
go
CREATE UNIQUE INDEX levbas#_#progress_recid_ident_ ON levbas (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX levbas##e_mailkontakt ON levbas (e_mailkontakt, PROGRESS_RECID)
go
CREATE INDEX levbas##e_maillev ON levbas (e_maillev, PROGRESS_RECID)
go
CREATE INDEX levbas##kunde ON levbas (kundenr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX levbas##levin ON levbas (levnr)
go
CREATE INDEX levbas##levnamn ON levbas (levnamn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'leveringsform' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table leveringsform
go
CREATE TABLE leveringsform (
  levfnr integer null,
  levformbeskrivelse varchar (30) null,
  levformmetode varchar (20) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  gebyr decimal(7,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_leveringsform ON leveringsform for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from leveringsform t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX leveringsform#_#progress_recid ON leveringsform (PROGRESS_RECID)
go
CREATE UNIQUE INDEX leveringsform#_#progress_recid_ident_ ON leveringsform (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX leveringsform##leveringsform ON leveringsform (levfnr)
go
CREATE INDEX leveringsform##levformmetode ON leveringsform (levformmetode, PROGRESS_RECID)
go
CREATE INDEX leveringsform##levformtekst ON leveringsform (levformbeskrivelse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'levkontakt' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table levkontakt
go
CREATE TABLE levkontakt (
  levnr integer null,
  navn varchar (40) null,
  stilling varchar (30) null,
  telefon varchar (15) null,
  telefaks varchar (15) null,
  mobiltelefon varchar (15) null,
  merknad varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kontnr integer null,
  e_mailkontakt varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_levkontakt ON levkontakt for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from levkontakt t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX levkontakt#_#progress_recid ON levkontakt (PROGRESS_RECID)
go
CREATE UNIQUE INDEX levkontakt#_#progress_recid_ident_ ON levkontakt (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX levkontakt##levkontakt ON levkontakt (levnr, kontnr)
go
CREATE INDEX levkontakt##levkontnavn ON levkontakt (levnr, navn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'levlager' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table levlager
go
CREATE TABLE levlager (
  levnr integer null,
  ibestilling decimal(8,2) null,
  iordre decimal(8,2) null,
  ilager decimal(8,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  artikkelnr decimal(13,0) null,
  strkode integer null,
  nesteankdato datetime null,
  clager varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_levlager ON levlager for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from levlager t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX levlager#_#progress_recid ON levlager (PROGRESS_RECID)
go
CREATE UNIQUE INDEX levlager#_#progress_recid_ident_ ON levlager (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX levlager##levlager ON levlager (levnr, artikkelnr, strkode)
go
if (select name from sysobjects 
    where name = 'levpris' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table levpris
go
CREATE TABLE levpris (
  artikkelnr decimal(13,0) null,
  levnr integer null,
  valpris decimal(11,2) null,
  innkjopspris decimal(7,2) null,
  rab1kr decimal(7,2) null,
  rab1_ decimal(5,2) null,
  rab2kr decimal(7,2) null,
  rab2_ decimal(5,2) null,
  frakt decimal(8,2) null,
  frakt_ decimal(5,2) null,
  divkostkr decimal(7,2) null,
  divkost_ decimal(5,2) null,
  rab3kr decimal(7,2) null,
  rab3_ decimal(5,2) null,
  varekost##1 decimal(7,2) null,
  varekost##2 decimal(7,2) null,
  dbkr decimal(7,2) null,
  db_ decimal(7,2) null,
  pris decimal(7,2) null,
  europris decimal(7,2) null,
  euromanuel tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_levpris ON levpris for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from levpris t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX levpris#_#progress_recid ON levpris (PROGRESS_RECID)
go
CREATE UNIQUE INDEX levpris#_#progress_recid_ident_ ON levpris (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX levpris##levpris ON levpris (levnr, artikkelnr)
go
CREATE INDEX levpris##prislev ON levpris (artikkelnr, levnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'levsant' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table levsant
go
CREATE TABLE levsant (
  sostorl varchar (10) null,
  soant integer null,
  seqnr integer null,
  levnr integer null,
  sortid varchar (12) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_levsant ON levsant for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from levsant t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX levsant#_#progress_recid ON levsant (PROGRESS_RECID)
go
CREATE UNIQUE INDEX levsant#_#progress_recid_ident_ ON levsant (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX levsant##levsant ON levsant (levnr, sortid, seqnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX levsant##sostorl ON levsant (levnr, sortid, sostorl)
go
CREATE INDEX levsant##strtype ON levsant (sostorl, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'levsort' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table levsort
go
CREATE TABLE levsort (
  levnr integer null,
  strtypeid integer null,
  sortid varchar (12) null,
  beskrivelse varchar (30) null,
  merknad varchar (50) null,
  fri tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_levsort ON levsort for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from levsort t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX levsort#_#progress_recid ON levsort (PROGRESS_RECID)
go
CREATE UNIQUE INDEX levsort#_#progress_recid_ident_ ON levsort (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX levsort##beskrivelse ON levsort (levnr, beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX levsort##levsort ON levsort (levnr, sortid)
go
CREATE INDEX levsort##strtype ON levsort (levnr, strtypeid, sortid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'listelinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table listelinje
go
CREATE TABLE listelinje (
  dataobjekt varchar (20) null,
  listetype varchar (30) null,
  listenr integer null,
  div1 varchar (30) null,
  div2 varchar (30) null,
  div3 varchar (30) null,
  decx##1 varchar (30) null,
  decx##2 varchar (30) null,
  decx##3 varchar (30) null,
  decx##4 varchar (30) null,
  decx##5 varchar (30) null,
  decx##6 varchar (30) null,
  decx##7 varchar (30) null,
  decx##8 varchar (30) null,
  decx##9 varchar (30) null,
  decx##10 varchar (30) null,
  decx##11 varchar (30) null,
  decx##12 varchar (30) null,
  decx##13 varchar (30) null,
  decx##14 varchar (30) null,
  decx##15 varchar (30) null,
  decx##16 varchar (30) null,
  decx##17 varchar (30) null,
  decx##18 varchar (30) null,
  decx##19 varchar (30) null,
  decx##20 varchar (30) null,
  decx##21 varchar (30) null,
  decx##22 varchar (30) null,
  decx##23 varchar (30) null,
  decx##24 varchar (30) null,
  decx##25 varchar (30) null,
  decx##26 varchar (30) null,
  decx##27 varchar (30) null,
  decx##28 varchar (30) null,
  decx##29 varchar (30) null,
  decx##30 varchar (30) null,
  decx##31 varchar (30) null,
  decx##32 varchar (30) null,
  decx##33 varchar (30) null,
  decx##34 varchar (30) null,
  decx##35 varchar (30) null,
  decx##36 varchar (30) null,
  decx##37 varchar (30) null,
  decx##38 varchar (30) null,
  decx##39 varchar (30) null,
  decx##40 varchar (30) null,
  decx##41 varchar (30) null,
  decx##42 varchar (30) null,
  decx##43 varchar (30) null,
  decx##44 varchar (30) null,
  decx##45 varchar (30) null,
  decx##46 varchar (30) null,
  decx##47 varchar (30) null,
  decx##48 varchar (30) null,
  decx##49 varchar (30) null,
  decx##50 varchar (30) null,
  divx##1 varchar (30) null,
  divx##2 varchar (30) null,
  divx##3 varchar (30) null,
  divx##4 varchar (30) null,
  divx##5 varchar (30) null,
  divx##6 varchar (30) null,
  divx##7 varchar (30) null,
  divx##8 varchar (30) null,
  divx##9 varchar (30) null,
  divx##10 varchar (30) null,
  divx##11 varchar (30) null,
  divx##12 varchar (30) null,
  divx##13 varchar (30) null,
  divx##14 varchar (30) null,
  divx##15 varchar (30) null,
  divx##16 varchar (30) null,
  divx##17 varchar (30) null,
  divx##18 varchar (30) null,
  divx##19 varchar (30) null,
  divx##20 varchar (30) null,
  divx##21 varchar (30) null,
  divx##22 varchar (30) null,
  divx##23 varchar (30) null,
  divx##24 varchar (30) null,
  divx##25 varchar (30) null,
  divx##26 varchar (30) null,
  divx##27 varchar (30) null,
  divx##28 varchar (30) null,
  divx##29 varchar (30) null,
  divx##30 varchar (30) null,
  divx##31 varchar (30) null,
  divx##32 varchar (30) null,
  divx##33 varchar (30) null,
  divx##34 varchar (30) null,
  divx##35 varchar (30) null,
  divx##36 varchar (30) null,
  divx##37 varchar (30) null,
  divx##38 varchar (30) null,
  divx##39 varchar (30) null,
  divx##40 varchar (30) null,
  divx##41 varchar (30) null,
  divx##42 varchar (30) null,
  divx##43 varchar (30) null,
  divx##44 varchar (30) null,
  divx##45 varchar (30) null,
  divx##46 varchar (30) null,
  divx##47 varchar (30) null,
  divx##48 varchar (30) null,
  divx##49 varchar (30) null,
  divx##50 varchar (30) null,
  cellnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_listelinje ON listelinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from listelinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX listelinje#_#progress_recid ON listelinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX listelinje#_#progress_recid_ident_ ON listelinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX listelinje##listelinje ON listelinje (listetype, listenr, dataobjekt, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'lister' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table lister
go
CREATE TABLE lister (
  listetype varchar (30) null,
  listenr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  beskrivelse varchar (30) null,
  merknad varchar (50) null,
  eier varchar (15) null,
  kriterier##1 varchar (30) null,
  kriterier##2 varchar (30) null,
  kriterier##3 varchar (30) null,
  kriterier##4 varchar (30) null,
  kriterier##5 varchar (30) null,
  kriterier##6 varchar (30) null,
  kriterier##7 varchar (30) null,
  kriterier##8 varchar (30) null,
  kriterier##9 varchar (30) null,
  kriterier##10 varchar (30) null,
  kriterier##11 varchar (30) null,
  kriterier##12 varchar (30) null,
  kriterier##13 varchar (30) null,
  kriterier##14 varchar (30) null,
  kriterier##15 varchar (30) null,
  kriterier##16 varchar (30) null,
  kriterier##17 varchar (30) null,
  kriterier##18 varchar (30) null,
  kriterier##19 varchar (30) null,
  kriterier##20 varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_lister ON lister for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from lister t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX lister#_#progress_recid ON lister (PROGRESS_RECID)
go
CREATE UNIQUE INDEX lister#_#progress_recid_ident_ ON lister (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX lister##eier ON lister (listetype, eier, beskrivelse, PROGRESS_RECID)
go
CREATE INDEX lister##eier2 ON lister (listetype, eier, listenr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX lister##lister ON lister (listetype, listenr)
go
if (select name from sysobjects 
    where name = 'lng' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table lng
go
CREATE TABLE lng (
  lng varchar (3) null,
  prgnavn varchar (15) null,
  tekster varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_lng ON lng for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from lng t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX lng#_#progress_recid ON lng (PROGRESS_RECID)
go
CREATE UNIQUE INDEX lng#_#progress_recid_ident_ ON lng (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX lng##lng ON lng (lng, prgnavn)
go
CREATE INDEX lng##prgnavn ON lng (prgnavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'lokalgruppering' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table lokalgruppering
go
CREATE TABLE lokalgruppering (
  lgid integer null,
  lgbeskrivelse varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_lokalgruppering ON lokalgruppering for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from lokalgruppering t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX lokalgruppering#_#progress_recid ON lokalgruppering (PROGRESS_RECID)
go
CREATE UNIQUE INDEX lokalgruppering#_#progress_recid_ident_ ON lokalgruppering (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX lokalgruppering##beskrivelse ON lokalgruppering (lgbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX lokalgruppering##lokalgruppering ON lokalgruppering (lgid)
go
if (select name from sysobjects 
    where name = 'material' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table material
go
CREATE TABLE material (
  matkod integer null,
  matbeskr varchar (20) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_material ON material for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from material t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX material#_#progress_recid ON material (PROGRESS_RECID)
go
CREATE UNIQUE INDEX material#_#progress_recid_ident_ ON material (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX material##matbeskr ON material (matbeskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX material##matin ON material (matkod)
go
if (select name from sysobjects 
    where name = 'medkjop' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table medkjop
go
CREATE TABLE medkjop (
  butikknr integer null,
  medlemsnr decimal(15,2) null,
  kjopsdato datetime null,
  kjopstid integer null,
  kjopsbelop decimal(10,2) null,
  kjopsgrunnlag decimal(10,2) null,
  tildeltbelop decimal(10,2) null,
  saldo decimal(10,2) null,
  b_id decimal(14,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  notat varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_medkjop ON medkjop for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from medkjop t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX medkjop#_#progress_recid ON medkjop (PROGRESS_RECID)
go
CREATE UNIQUE INDEX medkjop#_#progress_recid_ident_ ON medkjop (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX medkjop##beregnrabsjekk ON medkjop (medlemsnr, butikknr, saldo, kjopsdato, kjopstid, PROGRESS_RECID)
go
CREATE INDEX medkjop##b_id ON medkjop (b_id, PROGRESS_RECID)
go
CREATE INDEX medkjop##medsalg ON medkjop (medlemsnr, butikknr, kjopsdato, kjopstid, PROGRESS_RECID)
go
CREATE INDEX medkjop##saldo ON medkjop (butikknr, saldo, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'medlem' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table medlem
go
CREATE TABLE medlem (
  medlemsnr decimal(15,2) null,
  fornavn varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  medtype integer null,
  medgruppe integer null,
  adresse1 varchar (40) null,
  adresse2 varchar (40) null,
  postnr varchar (10) null,
  telefon varchar (15) null,
  telefaks varchar (15) null,
  mobiltlf varchar (15) null,
  land varchar (30) null,
  opphort datetime null,
  butikknr integer null,
  bydelsnr varchar (30) null,
  epostadresse varchar (40) null,
  etternavn varchar (40) null,
  hovedmedlemflagg tinyint null,
  hovedmedlemsnr decimal(15,2) null,
  fodselsdato datetime null,
  fodtar integer null,
  kjonn tinyint null,
  regkode varchar (10) null,
  kundenr decimal(15,2) null,
  aktiv tinyint null,
  aktivertfraweb datetime null,
  webbrukerid varchar (15) null,
  webpassord varchar (15) null,
  kilde varchar (30) null,
  tilgkilde varchar (30) null,
  rabatt decimal(7,2) null,
  eksterntmedlemsnr varchar (20) null,
  bonus_berettiget tinyint null,
  bonus_forsendelse integer null,
  bonus_varsel integer null,
  medlemnotat varchar (30) null,
  medleminfo varchar (30) null,
  mottaemailutsendelser tinyint null,
  mklubbid integer null,
  personnr varchar (15) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_medlem ON medlem for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from medlem t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX medlem#_#progress_recid ON medlem (PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlem#_#progress_recid_ident_ ON medlem (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX medlem##aktivertfraweb ON medlem (aktivertfraweb, PROGRESS_RECID)
go
CREATE INDEX medlem##bonus_berettiget ON medlem (bonus_berettiget, PROGRESS_RECID)
go
CREATE INDEX medlem##butikknr ON medlem (butikknr, etternavn, PROGRESS_RECID)
go
CREATE INDEX medlem##eksterntmedlemsnr ON medlem (eksterntmedlemsnr, PROGRESS_RECID)
go
CREATE INDEX medlem##epostadresse ON medlem (epostadresse, etternavn, PROGRESS_RECID)
go
CREATE INDEX medlem##etternavn ON medlem (etternavn, PROGRESS_RECID)
go
CREATE INDEX medlem##fodtar ON medlem (fodtar, etternavn, PROGRESS_RECID)
go
CREATE INDEX medlem##fordselsdato ON medlem (fodselsdato, etternavn, PROGRESS_RECID)
go
CREATE INDEX medlem##fornavn ON medlem (fornavn, PROGRESS_RECID)
go
CREATE INDEX medlem##hovedmedlem ON medlem (hovedmedlemsnr, etternavn, PROGRESS_RECID)
go
CREATE INDEX medlem##kunde ON medlem (kundenr, etternavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlem##medlem ON medlem (medlemsnr)
go
CREATE INDEX medlem##medlemsklubb ON medlem (mklubbid, PROGRESS_RECID)
go
CREATE INDEX medlem##personnr ON medlem (personnr, PROGRESS_RECID)
go
CREATE INDEX medlem##postnr ON medlem (postnr, etternavn, PROGRESS_RECID)
go
CREATE INDEX medlem##region ON medlem (regkode, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'medlembettrans' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table medlembettrans
go
CREATE TABLE medlembettrans (
  batchnr integer null,
  butik integer null,
  transnr integer null,
  ttid integer not null,
  tbid integer null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  bongid integer null,
  bonglinjenr integer null,
  kassanr integer null,
  belop decimal(10,2) null,
  dato datetime null,
  tid integer null,
  seqnr integer null,
  kortnr varchar (22) null,
  medlemsnr decimal(15,2) null,
  forsnr decimal(8,2) null,
  splittet tinyint null,
  orgbelop decimal(10,2) null,
  avregnet tinyint null,
  fakturanr decimal(11,2) null,
  betbutik integer null,
  betkassanr integer null,
  betbongid integer null,
  notat varchar (40) null,
  kontonr integer null,
  korttype integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  motpostert tinyint null,
  selgernr decimal(15,2) null,
  reftekst varchar (40) null,
  refnr integer null,
  sendterp datetime null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_medlembettrans ON medlembettrans for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from medlembettrans t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX medlembettrans#_#progress_recid ON medlembettrans (PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlembettrans#_#progress_recid_ident_ ON medlembettrans (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX medlembettrans##batchlogg ON medlembettrans (medlemsnr, batchnr, butik, transnr, seqnr, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##belop ON medlembettrans (medlemsnr, belop, dato, tid, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##betkvittering ON medlembettrans (betbutik, betkassanr, betbongid, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##butikkdato ON medlembettrans (butik, dato, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##datotid ON medlembettrans (medlemsnr, dato, tid, butik, ttid, tbid, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##forsnr ON medlembettrans (medlemsnr, forsnr, butik, dato, tid, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##kontonr ON medlembettrans (medlemsnr, kontonr, dato, tid, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##kortnr ON medlembettrans (medlemsnr, kortnr, dato, tid, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##kunde ON medlembettrans (medlemsnr, motpostert, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##kvittering ON medlembettrans (butik, kassanr, bongid, bonglinjenr, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##medlembettrans ON medlembettrans (medlemsnr, butik, transnr, seqnr, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##sendterp ON medlembettrans (sendterp, PROGRESS_RECID)
go
CREATE INDEX medlembettrans##transtype ON medlembettrans (medlemsnr, ttid, tbid, dato, tid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'medlemsaldo' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table medlemsaldo
go
CREATE TABLE medlemsaldo (
  medlemsnr decimal(15,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  butikknr integer null,
  forstedato datetime null,
  datosiste datetime null,
  forstetid integer null,
  sistetid integer null,
  saldo decimal(11,2) null,
  totaltkjop decimal(11,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_medlemsaldo ON medlemsaldo for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from medlemsaldo t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX medlemsaldo#_#progress_recid ON medlemsaldo (PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlemsaldo#_#progress_recid_ident_ ON medlemsaldo (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX medlemsaldo##forste ON medlemsaldo (medlemsnr, forstedato, forstetid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlemsaldo##medlemssaldo ON medlemsaldo (medlemsnr, butikknr)
go
CREATE INDEX medlemsaldo##siste ON medlemsaldo (medlemsnr, datosiste, sistetid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'medlemsgruppe' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table medlemsgruppe
go
CREATE TABLE medlemsgruppe (
  medgruppe integer null,
  beskrivelse varchar (40) null,
  notat varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_medlemsgruppe ON medlemsgruppe for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from medlemsgruppe t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX medlemsgruppe#_#progress_recid ON medlemsgruppe (PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlemsgruppe#_#progress_recid_ident_ ON medlemsgruppe (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX medlemsgruppe##beskrivelse ON medlemsgruppe (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlemsgruppe##medlemsgruppe ON medlemsgruppe (medgruppe)
go
if (select name from sysobjects 
    where name = 'medlemsklubb' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table medlemsklubb
go
CREATE TABLE medlemsklubb (
  mklubbid integer null,
  mklubbbeskrivelse varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_medlemsklubb ON medlemsklubb for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from medlemsklubb t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX medlemsklubb#_#progress_recid ON medlemsklubb (PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlemsklubb#_#progress_recid_ident_ ON medlemsklubb (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX medlemsklubb##medlemsklubb ON medlemsklubb (mklubbid)
go
CREATE INDEX medlemsklubb##mklubbbeskrivelse ON medlemsklubb (mklubbbeskrivelse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'medlemskort' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table medlemskort
go
CREATE TABLE medlemskort (
  medlemsnr decimal(15,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kortnr varchar (22) null,
  merknad varchar (40) null,
  aktivertdato datetime null,
  utgardato datetime null,
  sperret tinyint null,
  innehaver varchar (30) null,
  kunderabattkort tinyint null,
  kundekortnr varchar (22) null,
  korttype integer null,
  interntkkortid decimal(15,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_medlemskort ON medlemskort for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from medlemskort t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX medlemskort#_#progress_recid ON medlemskort (PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlemskort#_#progress_recid_ident_ ON medlemskort (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX medlemskort##interntkkortid ON medlemskort (interntkkortid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlemskort##kortidx ON medlemskort (kortnr)
go
CREATE INDEX medlemskort##korttype ON medlemskort (korttype, PROGRESS_RECID)
go
CREATE INDEX medlemskort##kundekort ON medlemskort (kundekortnr, kortnr, PROGRESS_RECID)
go
CREATE INDEX medlemskort##kunderabattkort ON medlemskort (kunderabattkort, PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlemskort##medlemskort ON medlemskort (medlemsnr, kortnr)
go
CREATE INDEX medlemskort##merknad ON medlemskort (medlemsnr, merknad, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'medlemstype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table medlemstype
go
CREATE TABLE medlemstype (
  medtype integer null,
  beskrivelse varchar (40) null,
  notat varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_medlemstype ON medlemstype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from medlemstype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX medlemstype#_#progress_recid ON medlemstype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlemstype#_#progress_recid_ident_ ON medlemstype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX medlemstype##beskrivelse ON medlemstype (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX medlemstype##medlemstype ON medlemstype (medtype)
go
if (select name from sysobjects 
    where name = 'medrabreskontr' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table medrabreskontr
go
CREATE TABLE medrabreskontr (
  rabreskontrid decimal(15,2) null,
  medlemsnr decimal(15,2) null,
  rabattbelop decimal(7,2) null,
  dato datetime null,
  rabsjekkserienr decimal(7,0) null,
  butikknr integer null,
  belopbrukt decimal(7,2) null,
  saldo decimal(7,2) null,
  b_id decimal(14,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_medrabreskontr ON medrabreskontr for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from medrabreskontr t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX medrabreskontr#_#progress_recid ON medrabreskontr (PROGRESS_RECID)
go
CREATE UNIQUE INDEX medrabreskontr#_#progress_recid_ident_ ON medrabreskontr (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX medrabreskontr##b_id ON medrabreskontr (b_id, PROGRESS_RECID)
go
CREATE INDEX medrabreskontr##dato ON medrabreskontr (dato, PROGRESS_RECID)
go
CREATE INDEX medrabreskontr##medlemdato ON medrabreskontr (medlemsnr, butikknr, dato, PROGRESS_RECID)
go
CREATE INDEX medrabreskontr##medlemsaldo ON medrabreskontr (medlemsnr, butikknr, saldo, PROGRESS_RECID)
go
CREATE UNIQUE INDEX medrabreskontr##rabreskontrid ON medrabreskontr (rabreskontrid)
go
CREATE INDEX medrabreskontr##saldo ON medrabreskontr (butikknr, saldo, PROGRESS_RECID)
go
CREATE INDEX medrabreskontr##serienr ON medrabreskontr (butikknr, rabsjekkserienr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'medrabsjekk' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table medrabsjekk
go
CREATE TABLE medrabsjekk (
  butikknr integer null,
  rabsjekkid decimal(15,2) null,
  medlemsnr decimal(15,2) null,
  belop decimal(7,2) null,
  datoutstedt datetime null,
  tidutstedt integer null,
  datoskrevet datetime null,
  tidskrevet integer null,
  datogyldig datetime null,
  rabsjekkserienr decimal(7,0) null,
  rabsjekktypenr integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  datobrukt datetime null,
  bruktbutikknr integer null,
  b_id decimal(15,2) null,
  notat varchar (40) null,
  brukt tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_medrabsjekk ON medrabsjekk for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from medrabsjekk t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX medrabsjekk#_#progress_recid ON medrabsjekk (PROGRESS_RECID)
go
CREATE UNIQUE INDEX medrabsjekk#_#progress_recid_ident_ ON medrabsjekk (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX medrabsjekk##brukt ON medrabsjekk (brukt, PROGRESS_RECID)
go
CREATE INDEX medrabsjekk##butikk ON medrabsjekk (butikknr, medlemsnr, rabsjekkid, PROGRESS_RECID)
go
CREATE INDEX medrabsjekk##butrabsjekkserienr ON medrabsjekk (butikknr, rabsjekkserienr, PROGRESS_RECID)
go
CREATE INDEX medrabsjekk##b_id ON medrabsjekk (b_id, PROGRESS_RECID)
go
CREATE INDEX medrabsjekk##datobrukt ON medrabsjekk (datobrukt, PROGRESS_RECID)
go
CREATE INDEX medrabsjekk##datogyldig ON medrabsjekk (datogyldig, PROGRESS_RECID)
go
CREATE INDEX medrabsjekk##datoskrevet ON medrabsjekk (datoskrevet, tidskrevet, PROGRESS_RECID)
go
CREATE INDEX medrabsjekk##datoutstedt ON medrabsjekk (datoutstedt, tidutstedt, PROGRESS_RECID)
go
CREATE INDEX medrabsjekk##medlem ON medrabsjekk (medlemsnr, rabsjekkid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX medrabsjekk##rabsjekk ON medrabsjekk (rabsjekkid)
go
CREATE INDEX medrabsjekk##rabsjekktype ON medrabsjekk (rabsjekktypenr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'medtrans' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table medtrans
go
CREATE TABLE medtrans (
  batchnr integer null,
  butik integer null,
  transnr integer null,
  ttid integer not null,
  tbid integer null,
  artikkelnr decimal(13,0) null,
  levnr integer null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  bongid integer null,
  bonglinjenr integer null,
  kassanr integer null,
  vg integer null,
  lopnr integer null,
  storl varchar (10) null,
  antall decimal(10,2) null,
  pris decimal(10,2) null,
  rabkr decimal(9,2) null,
  mva decimal(9,2) null,
  dato datetime null,
  tid integer null,
  seqnr integer null,
  vvarekost decimal(11,4) null,
  sattvvarekost tinyint null,
  medlemsnr decimal(15,2) null,
  kortnr varchar (22) null,
  forsnr decimal(8,2) null,
  selgernr decimal(15,2) null,
  subtotalrab decimal(9,2) null,
  reftekst varchar (40) null,
  refnr integer null,
  bongtekst varchar (30) null,
  sendterp datetime null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_medtrans ON medtrans for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from medtrans t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX medtrans#_#progress_recid ON medtrans (PROGRESS_RECID)
go
CREATE UNIQUE INDEX medtrans#_#progress_recid_ident_ ON medtrans (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX medtrans##artbutstr ON medtrans (medlemsnr, artikkelnr, butik, storl, PROGRESS_RECID)
go
CREATE INDEX medtrans##artikkelnr ON medtrans (artikkelnr, PROGRESS_RECID)
go
CREATE INDEX medtrans##batchlogg ON medtrans (medlemsnr, batchnr, butik, transnr, seqnr, PROGRESS_RECID)
go
CREATE INDEX medtrans##bongtekst ON medtrans (medlemsnr, bongtekst, PROGRESS_RECID)
go
CREATE INDEX medtrans##butartstr ON medtrans (medlemsnr, butik, artikkelnr, storl, PROGRESS_RECID)
go
CREATE INDEX medtrans##butikkdato ON medtrans (butik, dato, PROGRESS_RECID)
go
CREATE INDEX medtrans##butvglopnrstrl ON medtrans (medlemsnr, butik, vg, lopnr, storl, PROGRESS_RECID)
go
CREATE INDEX medtrans##kortnr ON medtrans (kortnr, PROGRESS_RECID)
go
CREATE INDEX medtrans##meddato ON medtrans (medlemsnr, dato, tid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX medtrans##medtrans ON medtrans (medlemsnr, butik, transnr, seqnr)
go
CREATE INDEX medtrans##oppslagdatotid ON medtrans (medlemsnr, artikkelnr, dato, tid, butik, ttid, tbid, PROGRESS_RECID)
go
CREATE INDEX medtrans##oppslagselger ON medtrans (medlemsnr, forsnr, artikkelnr, butik, dato, tid, ttid, tbid, PROGRESS_RECID)
go
CREATE INDEX medtrans##oppslagstr ON medtrans (medlemsnr, artikkelnr, storl, butik, dato, tid, ttid, PROGRESS_RECID)
go
CREATE INDEX medtrans##sendterp ON medtrans (sendterp, PROGRESS_RECID)
go
CREATE INDEX medtrans##transtype ON medtrans (ttid, PROGRESS_RECID)
go
CREATE INDEX medtrans##vglopnrstrbut ON medtrans (medlemsnr, vg, lopnr, storl, butik, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'meny' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table meny
go
CREATE TABLE meny (
  navn varchar (40) not null,
  mdata varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_meny ON meny for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from meny t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX meny#_#progress_recid ON meny (PROGRESS_RECID)
go
CREATE UNIQUE INDEX meny#_#progress_recid_ident_ ON meny (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX meny##navn ON meny (navn)
go
if (select name from sysobjects 
    where name = 'messe' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table messe
go
CREATE TABLE messe (
  messenr decimal(10,2) null,
  messebeskrivelse varchar (40) null,
  fradato datetime null,
  fratid integer null,
  tildato datetime null,
  tiltid integer null,
  kontaktpers varchar (30) null,
  telefon varchar (20) null,
  mobiltlf varchar (20) null,
  telefaks varchar (20) null,
  adresse1 varchar (30) null,
  adresse2 varchar (30) null,
  postboks varchar (40) null,
  postnr varchar (10) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  oppmerking varchar (50) null,
  fraaar integer null,
  tilaar integer null,
  frauke integer null,
  tiluke integer null,
  messefradato datetime null,
  messetildato datetime null,
  publiserstartdato datetime null,
  publiserstoppdato datetime null,
  fargekoder varchar (40) null,
  messetype integer null,
  sortimentkoder varchar (40) null,
  kampanjeuker varchar (30) null,
  kampanjestotte varchar (30) null,
  lagerkoder varchar (30) null,
  sasong integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_messe ON messe for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from messe t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX messe#_#progress_recid ON messe (PROGRESS_RECID)
go
CREATE UNIQUE INDEX messe#_#progress_recid_ident_ ON messe (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX messe##fradato ON messe (fradato, PROGRESS_RECID)
go
CREATE INDEX messe##kontakt ON messe (kontaktpers, PROGRESS_RECID)
go
CREATE UNIQUE INDEX messe##messe ON messe (messenr)
go
CREATE INDEX messe##messebeskrivelse ON messe (messebeskrivelse, PROGRESS_RECID)
go
CREATE INDEX messe##messetype ON messe (messetype, PROGRESS_RECID)
go
CREATE INDEX messe##sesong ON messe (sasong, PROGRESS_RECID)
go
CREATE INDEX messe##tildato ON messe (tildato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'messeforbutikk' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table messeforbutikk
go
CREATE TABLE messeforbutikk (
  messenr decimal(10,2) null,
  butikknr integer null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_messeforbutikk ON messeforbutikk for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from messeforbutikk t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX messeforbutikk#_#progress_recid ON messeforbutikk (PROGRESS_RECID)
go
CREATE UNIQUE INDEX messeforbutikk#_#progress_recid_ident_ ON messeforbutikk (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX messeforbutikk##butikknr ON messeforbutikk (butikknr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX messeforbutikk##messeforbutikk ON messeforbutikk (messenr, butikknr)
go
if (select name from sysobjects 
    where name = 'mixmatchhode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table mixmatchhode
go
CREATE TABLE mixmatchhode (
  mixnr integer not null,
  mixtekst varchar (30) null,
  antall decimal(7,3) not null,
  utpris decimal(7,2) not null,
  fradato datetime null,
  tildato datetime null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  profilnr integer null,
  mixmatchtype integer null,
  mixaktiver tinyint null,
  klartilaktivering tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_mixmatchhode ON mixmatchhode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from mixmatchhode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX mixmatchhode#_#progress_recid ON mixmatchhode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX mixmatchhode#_#progress_recid_ident_ ON mixmatchhode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX mixmatchhode##aktiver ON mixmatchhode (mixaktiver, PROGRESS_RECID)
go
CREATE INDEX mixmatchhode##fradato ON mixmatchhode (fradato, PROGRESS_RECID)
go
CREATE INDEX mixmatchhode##klartilaktivering ON mixmatchhode (klartilaktivering, PROGRESS_RECID)
go
CREATE INDEX mixmatchhode##mixmatchtype ON mixmatchhode (mixmatchtype, PROGRESS_RECID)
go
CREATE UNIQUE INDEX mixmatchhode##mixnr ON mixmatchhode (mixnr)
go
CREATE INDEX mixmatchhode##mixtekst ON mixmatchhode (mixtekst, PROGRESS_RECID)
go
CREATE INDEX mixmatchhode##profilnr ON mixmatchhode (profilnr, PROGRESS_RECID)
go
CREATE INDEX mixmatchhode##tildato ON mixmatchhode (tildato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'mixmatchrad' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table mixmatchrad
go
CREATE TABLE mixmatchrad (
  mixnr integer not null,
  antall decimal(7,3) not null,
  kode varchar (20) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_mixmatchrad ON mixmatchrad for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from mixmatchrad t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX mixmatchrad#_#progress_recid ON mixmatchrad (PROGRESS_RECID)
go
CREATE UNIQUE INDEX mixmatchrad#_#progress_recid_ident_ ON mixmatchrad (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX mixmatchrad##kode ON mixmatchrad (kode, PROGRESS_RECID)
go
CREATE UNIQUE INDEX mixmatchrad##mixmatch ON mixmatchrad (mixnr, kode)
go
if (select name from sysobjects 
    where name = 'moms' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table moms
go
CREATE TABLE moms (
  momskod integer null,
  momsproc decimal(4,2) null,
  beskrivelse varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_moms ON moms for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from moms t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX moms#_#progress_recid ON moms (PROGRESS_RECID)
go
CREATE UNIQUE INDEX moms#_#progress_recid_ident_ ON moms (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX moms##beskrivelse ON moms (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX moms##momsin ON moms (momskod)
go
CREATE INDEX moms##mva_ ON moms (momsproc, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'nets' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table nets
go
CREATE TABLE nets (
  b_id decimal(24,2) null,
  ijboxcompanyid bigint null,
  butikknr integer null,
  dato datetime null,
  transactionid varchar (20) null,
  sendt tinyint null,
  sendtdato datetime null,
  sendttid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  datosendt datetime null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_nets ON nets for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from nets t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX nets#_#progress_recid ON nets (PROGRESS_RECID)
go
CREATE UNIQUE INDEX nets#_#progress_recid_ident_ ON nets (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX nets##bong ON nets (b_id, transactionid, PROGRESS_RECID)
go
CREATE INDEX nets##butikk ON nets (butikknr, PROGRESS_RECID)
go
CREATE INDEX nets##datosendt ON nets (datosendt, PROGRESS_RECID)
go
CREATE INDEX nets##endret ON nets (edato, etid, PROGRESS_RECID)
go
CREATE INDEX nets##registrert ON nets (registrertdato, registrerttid, PROGRESS_RECID)
go
CREATE INDEX nets##sendt ON nets (sendt, sendttid, PROGRESS_RECID)
go
CREATE INDEX nets##transactionid ON nets (transactionid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX nets##transactionidcompany ON nets (ijboxcompanyid, transactionid)
go
if (select name from sysobjects 
    where name = 'non_sale_spes' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table non_sale_spes
go
CREATE TABLE non_sale_spes (
  butikk integer null,
  kasse integer null,
  dato datetime null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  non_saleverdi decimal(9,2) null,
  non_saleantall integer null,
  kode varchar (25) null,
  non_sale_type integer null,
  kasserernr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_non_sale_spes ON non_sale_spes for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from non_sale_spes t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX non_sale_spes#_#progress_recid ON non_sale_spes (PROGRESS_RECID)
go
CREATE UNIQUE INDEX non_sale_spes#_#progress_recid_ident_ ON non_sale_spes (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX non_sale_spes##nonsale ON non_sale_spes (butikk, kasse, dato, kasserernr, non_sale_type, kode)
go
CREATE INDEX non_sale_spes##non_sale ON non_sale_spes (butikk, kasse, dato, non_sale_type, kode, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'notatkoder' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table notatkoder
go
CREATE TABLE notatkoder (
  notatkode integer null,
  beskrivelse varchar (40) null,
  notat varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_notatkoder ON notatkoder for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from notatkoder t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX notatkoder#_#progress_recid ON notatkoder (PROGRESS_RECID)
go
CREATE UNIQUE INDEX notatkoder#_#progress_recid_ident_ ON notatkoder (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX notatkoder##beskrivelse ON notatkoder (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX notatkoder##notatkoder ON notatkoder (notatkode)
go
if (select name from sysobjects 
    where name = 'numlandkode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table numlandkode
go
CREATE TABLE numlandkode (
  numlandkode integer null,
  land varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_numlandkode ON numlandkode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from numlandkode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX numlandkode#_#progress_recid ON numlandkode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX numlandkode#_#progress_recid_ident_ ON numlandkode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX numlandkode##land ON numlandkode (land, PROGRESS_RECID)
go
CREATE UNIQUE INDEX numlandkode##numlandkode ON numlandkode (numlandkode)
go
if (select name from sysobjects 
    where name = 'onlineleverandor' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table onlineleverandor
go
CREATE TABLE onlineleverandor (
  onlinelevnr integer null,
  onlinelevbeskr varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_onlineleverandor ON onlineleverandor for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from onlineleverandor t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX onlineleverandor#_#progress_recid ON onlineleverandor (PROGRESS_RECID)
go
CREATE UNIQUE INDEX onlineleverandor#_#progress_recid_ident_ ON onlineleverandor (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX onlineleverandor##onlinebeskrivelse ON onlineleverandor (onlinelevbeskr, PROGRESS_RECID)
go
CREATE INDEX onlineleverandor##onlinjelevnr ON onlineleverandor (onlinelevnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ordre' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ordre
go
CREATE TABLE ordre (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  ordrenr integer not null,
  sendtdato datetime null,
  merknad varchar (40) null,
  notat varchar (30) null,
  levadresse1 varchar (40) null,
  levadresse2 varchar (40) null,
  levpostnr varchar (10) null,
  levpostboks varchar (40) null,
  levtelefon varchar (15) null,
  levkontakt varchar (30) null,
  levmerknad varchar (50) null,
  levnr integer not null,
  ordrestatus integer not null,
  ekstid varchar (15) null,
  laptop tinyint null,
  hkordre tinyint null,
  varebehnr decimal(15,2) null,
  leveringsdato datetime null,
  bekreftetordre tinyint null,
  bekreftetdato datetime null,
  bekreftetav varchar (15) null,
  fraerp tinyint null,
  cl integer null,
  sendtbutikkflagg tinyint null,
  sendtbutikkdato datetime null,
  sendtbutikktid integer null,
  ulevnr integer null,
  hasteordre tinyint null,
  ordremottaker varchar (30) null,
  opphav varchar (30) null,
  pllisteid decimal(10,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ordre ON ordre for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ordre t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ordre#_#progress_recid ON ordre (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ordre#_#progress_recid_ident_ ON ordre (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX ordre##bekreftet ON ordre (bekreftetordre, PROGRESS_RECID)
go
CREATE INDEX ordre##cl ON ordre (cl, PROGRESS_RECID)
go
CREATE INDEX ordre##fraerp ON ordre (fraerp, PROGRESS_RECID)
go
CREATE INDEX ordre##hasteordre ON ordre (hasteordre, PROGRESS_RECID)
go
CREATE INDEX ordre##hkordre ON ordre (hkordre, PROGRESS_RECID)
go
CREATE INDEX ordre##laptop ON ordre (laptop, ordrenr, PROGRESS_RECID)
go
CREATE INDEX ordre##leveringsdato ON ordre (leveringsdato, PROGRESS_RECID)
go
CREATE INDEX ordre##levnrekstid ON ordre (levnr, ekstid, PROGRESS_RECID)
go
CREATE INDEX ordre##levordnr ON ordre (levnr, ordrenr, PROGRESS_RECID)
go
CREATE INDEX ordre##opphav ON ordre (opphav, PROGRESS_RECID)
go
CREATE UNIQUE INDEX ordre##ordrenr ON ordre (ordrenr)
go
CREATE INDEX ordre##pllisteid ON ordre (pllisteid, PROGRESS_RECID)
go
CREATE INDEX ordre##sendtbutikkdato ON ordre (sendtbutikkdato, PROGRESS_RECID)
go
CREATE INDEX ordre##sendtbutikkflagg ON ordre (sendtbutikkflagg, PROGRESS_RECID)
go
CREATE INDEX ordre##sendtdato ON ordre (sendtdato, PROGRESS_RECID)
go
CREATE INDEX ordre##varebehnr ON ordre (varebehnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ovandel' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ovandel
go
CREATE TABLE ovandel (
  ov_id integer null,
  ovbeskr varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ovandel ON ovandel for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ovandel t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ovandel#_#progress_recid ON ovandel (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ovandel#_#progress_recid_ident_ ON ovandel (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX ovandel##ov_txt ON ovandel (ovbeskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX ovandel##ovandel ON ovandel (ov_id)
go
if (select name from sysobjects 
    where name = 'ovart' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ovart
go
CREATE TABLE ovart (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  artikkelnr decimal(13,0) null,
  artnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ovart ON ovart for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ovart t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ovart#_#progress_recid ON ovart (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ovart#_#progress_recid_ident_ ON ovart (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX ovart##ovart ON ovart (artnr, artikkelnr)
go
if (select name from sysobjects 
    where name = 'ovbuffer' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ovbuffer
go
CREATE TABLE ovbuffer (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  linjenr decimal(12,2) null,
  artikkelnr decimal(13,0) null,
  vg integer null,
  lopnr integer null,
  butikknrfra integer null,
  butikknrtil integer null,
  antall decimal(7,2) null,
  storl varchar (10) null,
  tilstorl varchar (10) null,
  merknad varchar (40) null,
  buntnr integer null,
  varekost decimal(9,2) null,
  mvakr decimal(9,2) null,
  mva_ decimal(5,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ovbuffer ON ovbuffer for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ovbuffer t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ovbuffer#_#progress_recid ON ovbuffer (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ovbuffer#_#progress_recid_ident_ ON ovbuffer (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX ovbuffer##artikkel ON ovbuffer (artikkelnr, butikknrfra, butikknrtil, PROGRESS_RECID)
go
CREATE INDEX ovbuffer##buntartikkel ON ovbuffer (buntnr, artikkelnr, butikknrfra, butikknrtil, PROGRESS_RECID)
go
CREATE UNIQUE INDEX ovbuffer##buntlinjenr ON ovbuffer (buntnr, linjenr)
go
if (select name from sysobjects 
    where name = 'ovbunt' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ovbunt
go
CREATE TABLE ovbunt (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  buntnr integer null,
  merknad varchar (30) null,
  datooppdatert datetime null,
  tidoppdatert integer null,
  oppdatertav varchar (15) null,
  batchnr integer null,
  buntstatus integer null,
  opphav integer null,
  fakturertdato datetime null,
  fakturerttid integer null,
  fakturertav varchar (20) null,
  faktura_id decimal(15,2) not null,
  pllisteid decimal(10,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ovbunt ON ovbunt for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ovbunt t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ovbunt#_#progress_recid ON ovbunt (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ovbunt#_#progress_recid_ident_ ON ovbunt (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX ovbunt##buntnr ON ovbunt (buntnr)
go
CREATE INDEX ovbunt##datooppdatert ON ovbunt (datooppdatert, tidoppdatert, PROGRESS_RECID)
go
CREATE INDEX ovbunt##faktura_id ON ovbunt (faktura_id, PROGRESS_RECID)
go
CREATE INDEX ovbunt##fakturertdato ON ovbunt (fakturertdato, fakturerttid, PROGRESS_RECID)
go
CREATE INDEX ovbunt##merknad ON ovbunt (merknad, PROGRESS_RECID)
go
CREATE INDEX ovbunt##opphav ON ovbunt (opphav, datooppdatert, PROGRESS_RECID)
go
CREATE INDEX ovbunt##plliste ON ovbunt (pllisteid, PROGRESS_RECID)
go
CREATE INDEX ovbunt##registrertdato ON ovbunt (registrertdato, registrerttid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ovlinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ovlinje
go
CREATE TABLE ovlinje (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  ovordreid decimal(15,2) not null,
  linjenr integer null,
  artikkelnr decimal(13,0) null,
  frabutik integer null,
  tilbutik integer null,
  storl varchar (10) null,
  tilstorl varchar (10) null,
  forsnr integer null,
  antall integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ovlinje ON ovlinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ovlinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ovlinje#_#progress_recid ON ovlinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ovlinje#_#progress_recid_ident_ ON ovlinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX ovlinje##avsender ON ovlinje (ovordreid, artikkelnr, frabutik, tilbutik, storl, PROGRESS_RECID)
go
CREATE UNIQUE INDEX ovlinje##linje ON ovlinje (ovordreid, artikkelnr, linjenr)
go
CREATE INDEX ovlinje##mottager ON ovlinje (ovordreid, artikkelnr, tilbutik, frabutik, storl, PROGRESS_RECID)
go
CREATE INDEX ovlinje##selger ON ovlinje (ovordreid, artikkelnr, forsnr, linjenr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'ovlink' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ovlink
go
CREATE TABLE ovlink (
  buntnr integer null,
  ovordreid decimal(15,2) not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ovlink ON ovlink for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ovlink t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ovlink#_#progress_recid ON ovlink (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ovlink#_#progress_recid_ident_ ON ovlink (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX ovlink##ovlinke ON ovlink (buntnr, ovordreid)
go
if (select name from sysobjects 
    where name = 'ovordre' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table ovordre
go
CREATE TABLE ovordre (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  ovordreid decimal(15,2) not null,
  ovdato datetime null,
  bstat integer null,
  kasseid varchar (30) null,
  fovordre tinyint null,
  merknad varchar (40) null,
  notat varchar (40) null,
  mottatt tinyint null,
  ovtid integer null,
  ovav varchar (12) null,
  mottattdato datetime null,
  mottatttid integer null,
  mottattav varchar (15) null,
  overforesdato datetime null,
  kjortdato datetime null,
  kjorttid integer null,
  kjortav varchar (15) null,
  kjort tinyint null,
  overfort tinyint null,
  differanse tinyint null,
  diffant integer null,
  diffverdi decimal(10,2) null,
  ovantall integer null,
  mottattantall integer null,
  ovverdi decimal(9,2) null,
  mottattverdi decimal(10,2) null,
  diffantall integer null,
  frabutikk integer null,
  tilbutikk integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_ovordre ON ovordre for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from ovordre t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX ovordre#_#progress_recid ON ovordre (PROGRESS_RECID)
go
CREATE UNIQUE INDEX ovordre#_#progress_recid_ident_ ON ovordre (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX ovordre##kasseid ON ovordre (kasseid, PROGRESS_RECID)
go
CREATE INDEX ovordre##merknad ON ovordre (merknad, PROGRESS_RECID)
go
CREATE INDEX ovordre##mottatt ON ovordre (mottattdato, PROGRESS_RECID)
go
CREATE INDEX ovordre##overforesdato ON ovordre (overforesdato, PROGRESS_RECID)
go
CREATE INDEX ovordre##overfortdato ON ovordre (ovdato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX ovordre##ovordre ON ovordre (ovordreid)
go
if (select name from sysobjects 
    where name = 'pakkelinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pakkelinje
go
CREATE TABLE pakkelinje (
  artikkelnr decimal(13,0) null,
  pkartikkelnr decimal(13,0) null,
  antall decimal(8,3) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  pakkenr integer null,
  strkode integer null,
  varekost decimal(9,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_pakkelinje ON pakkelinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pakkelinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pakkelinje#_#progress_recid ON pakkelinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pakkelinje#_#progress_recid_ident_ ON pakkelinje (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX pakkelinje##pakkelinje ON pakkelinje (artikkelnr, pkartikkelnr, strkode)
go
CREATE INDEX pakkelinje##prlinjestrkode ON pakkelinje (pkartikkelnr, artikkelnr, strkode, PROGRESS_RECID)
go
CREATE INDEX pakkelinje##strkode ON pakkelinje (artikkelnr, strkode, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'periode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table periode
go
CREATE TABLE periode (
  perid varchar (10) not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  beskrivelse varchar (30) not null,
  fast tinyint null,
  aar integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_periode ON periode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from periode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX periode#_#progress_recid ON periode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX periode#_#progress_recid_ident_ ON periode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX periode##beskrivelse ON periode (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX periode##periode ON periode (perid)
go
if (select name from sysobjects 
    where name = 'perlin' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table perlin
go
CREATE TABLE perlin (
  perid varchar (10) not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  perlinnr integer not null,
  fradato datetime null,
  tildato datetime null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_perlin ON perlin for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from perlin t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX perlin#_#progress_recid ON perlin (PROGRESS_RECID)
go
CREATE UNIQUE INDEX perlin#_#progress_recid_ident_ ON perlin (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX perlin##finnlinje ON perlin (fradato, tildato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX perlin##periodelinje ON perlin (perid, perlinnr)
go
if (select name from sysobjects 
    where name = 'pksdlhode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pksdlhode
go
CREATE TABLE pksdlhode (
  pksdlid decimal(13,0) not null,
  pksdlnr varchar (15) null,
  pksdlopphav integer not null,
  cl integer not null,
  sendtdato datetime null,
  pksdlstatus integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  merknad varchar (30) null,
  meldingfralev varchar (30) null,
  ekstid varchar (15) null,
  sumfrakt decimal(7,2) null,
  levnr integer null,
  levnamn varchar (30) null,
  leveringsdato datetime null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_pksdlhode ON pksdlhode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pksdlhode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pksdlhode#_#progress_recid ON pksdlhode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pksdlhode#_#progress_recid_ident_ ON pksdlhode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pksdlhode##cl ON pksdlhode (cl, PROGRESS_RECID)
go
CREATE INDEX pksdlhode##ekstid ON pksdlhode (ekstid, PROGRESS_RECID)
go
CREATE INDEX pksdlhode##leveringsdato ON pksdlhode (leveringsdato, PROGRESS_RECID)
go
CREATE INDEX pksdlhode##levnamn ON pksdlhode (levnamn, PROGRESS_RECID)
go
CREATE INDEX pksdlhode##levnr ON pksdlhode (levnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pksdlhode##pksdlid ON pksdlhode (pksdlid)
go
CREATE INDEX pksdlhode##pksdlnr ON pksdlhode (pksdlnr, PROGRESS_RECID)
go
CREATE INDEX pksdlhode##pksdlstatus ON pksdlhode (pksdlstatus, PROGRESS_RECID)
go
CREATE INDEX pksdlhode##sendtdato ON pksdlhode (sendtdato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pksdllinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pksdllinje
go
CREATE TABLE pksdllinje (
  pksdlid decimal(13,0) not null,
  pksdllinjeid integer null,
  linjenr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  artikkelnr decimal(13,0) null,
  beskr varchar (30) null,
  levkod varchar (20) null,
  levfargkod varchar (15) null,
  strkode integer null,
  antall decimal(8,3) null,
  antrest decimal(7,2) null,
  antlevert decimal(7,2) null,
  mottaksid integer null,
  bestnr integer null,
  levnr integer not null,
  ordrenr integer null,
  salgsenhet varchar (10) null,
  butikknr integer null,
  pakkenr integer null,
  pakke tinyint null,
  kode varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_pksdllinje ON pksdllinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pksdllinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pksdllinje#_#progress_recid ON pksdllinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pksdllinje#_#progress_recid_ident_ ON pksdllinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pksdllinje##artikkellevert ON pksdllinje (artikkelnr, antlevert, PROGRESS_RECID)
go
CREATE INDEX pksdllinje##artikkelnr ON pksdllinje (pksdlid, artikkelnr, PROGRESS_RECID)
go
CREATE INDEX pksdllinje##beskr ON pksdllinje (pksdlid, beskr, PROGRESS_RECID)
go
CREATE INDEX pksdllinje##bestnr ON pksdllinje (bestnr, PROGRESS_RECID)
go
CREATE INDEX pksdllinje##butikknr ON pksdllinje (butikknr, PROGRESS_RECID)
go
CREATE INDEX pksdllinje##levfargkod ON pksdllinje (pksdlid, levfargkod, PROGRESS_RECID)
go
CREATE INDEX pksdllinje##levkod ON pksdllinje (pksdlid, levkod, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pksdllinje##pksdllinje ON pksdllinje (pksdlid, pksdllinjeid)
go
if (select name from sysobjects 
    where name = 'pksdlmottak' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pksdlmottak
go
CREATE TABLE pksdlmottak (
  pksdlid decimal(13,0) not null,
  mottaksid integer null,
  mottattdato datetime null,
  mottatttid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_pksdlmottak ON pksdlmottak for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pksdlmottak t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pksdlmottak#_#progress_recid ON pksdlmottak (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pksdlmottak#_#progress_recid_ident_ ON pksdlmottak (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX pksdlmottak##pksdlmottak ON pksdlmottak (pksdlid, mottaksid)
go
if (select name from sysobjects 
    where name = 'pksdlpris' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pksdlpris
go
CREATE TABLE pksdlpris (
  pksdlid decimal(13,0) not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  artikkelnr decimal(13,0) null,
  beskr varchar (30) null,
  levkod varchar (20) null,
  levfargkod varchar (15) null,
  innkjopspris decimal(7,2) null,
  rab1_ decimal(5,2) null,
  db_ decimal(7,2) null,
  pris decimal(7,2) null,
  overstyrpris tinyint null,
  nypris decimal(7,2) null,
  nyvarekost decimal(9,2) null,
  varekost decimal(9,2) null,
  nyrab1_ decimal(5,2) null,
  nydb_ decimal(7,2) null,
  nyinnkjopspris decimal(7,2) null,
  frakt decimal(8,2) null,
  nyfrakt decimal(8,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_pksdlpris ON pksdlpris for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pksdlpris t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pksdlpris#_#progress_recid ON pksdlpris (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pksdlpris#_#progress_recid_ident_ ON pksdlpris (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pksdlpris##beskr ON pksdlpris (pksdlid, beskr, PROGRESS_RECID)
go
CREATE INDEX pksdlpris##levfargkod ON pksdlpris (pksdlid, levfargkod, PROGRESS_RECID)
go
CREATE INDEX pksdlpris##levkod ON pksdlpris (pksdlid, levkod, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pksdlpris##pksdlpris ON pksdlpris (pksdlid, artikkelnr)
go
if (select name from sysobjects 
    where name = 'pllisteartikkel' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pllisteartikkel
go
CREATE TABLE pllisteartikkel (
  pllisteid decimal(10,2) null,
  modellfarge decimal(15,2) not null,
  artikkelnr decimal(13,0) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_pllisteartikkel ON pllisteartikkel for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pllisteartikkel t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pllisteartikkel#_#progress_recid ON pllisteartikkel (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pllisteartikkel#_#progress_recid_ident_ ON pllisteartikkel (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pllisteartikkel##pllisteartikkel ON pllisteartikkel (pllisteid, modellfarge, artikkelnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pllistehode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pllistehode
go
CREATE TABLE pllistehode (
  pllisteid decimal(10,2) null,
  frabutikknr integer null,
  tilbutikknr integer null,
  datoplukket datetime null,
  prioplukket integer null,
  sendtpda datetime null,
  tidplukket integer null,
  plmerknad varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  plnavn varchar (30) null,
  antallplukket decimal(8,3) null,
  antall decimal(8,3) null,
  overfortdato datetime null,
  buntnr integer null,
  plltype integer null,
  pllistestatus integer null,
  levnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_pllistehode ON pllistehode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pllistehode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pllistehode#_#progress_recid ON pllistehode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pllistehode#_#progress_recid_ident_ ON pllistehode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pllistehode##buntnr ON pllistehode (buntnr, PROGRESS_RECID)
go
CREATE INDEX pllistehode##datoplukket ON pllistehode (datoplukket, PROGRESS_RECID)
go
CREATE INDEX pllistehode##frabutikk ON pllistehode (frabutikknr, plltype, levnr, pllistestatus, pllisteid, PROGRESS_RECID)
go
CREATE INDEX pllistehode##frabutikknr ON pllistehode (frabutikknr, PROGRESS_RECID)
go
CREATE INDEX pllistehode##levnr ON pllistehode (levnr, PROGRESS_RECID)
go
CREATE INDEX pllistehode##overfortdato ON pllistehode (overfortdato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pllistehode##pllistehode ON pllistehode (pllisteid)
go
CREATE INDEX pllistehode##pllistestatus ON pllistehode (pllistestatus, PROGRESS_RECID)
go
CREATE INDEX pllistehode##plmerknad ON pllistehode (plmerknad, PROGRESS_RECID)
go
CREATE INDEX pllistehode##suppleringsordre ON pllistehode (plltype, pllisteid, frabutikknr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pllistelinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pllistelinje
go
CREATE TABLE pllistelinje (
  pllisteid decimal(10,2) null,
  artikkelnr decimal(15,2) null,
  levkod varchar (30) null,
  beskr varchar (40) null,
  levfargkod varchar (30) null,
  vargr integer null,
  strkode integer null,
  antall decimal(7,2) null,
  pllinjenr integer null,
  antallplukket decimal(8,3) null,
  lopnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_pllistelinje ON pllistelinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pllistelinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pllistelinje#_#progress_recid ON pllistelinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pllistelinje#_#progress_recid_ident_ ON pllistelinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pllistelinje##artikkelnr ON pllistelinje (pllisteid, artikkelnr, PROGRESS_RECID)
go
CREATE INDEX pllistelinje##beskr ON pllistelinje (pllisteid, beskr, PROGRESS_RECID)
go
CREATE INDEX pllistelinje##levfargkod ON pllistelinje (pllisteid, levfargkod, PROGRESS_RECID)
go
CREATE INDEX pllistelinje##levkod ON pllistelinje (pllisteid, levkod, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pllistelinje##pllistelinje ON pllistelinje (pllisteid, pllinjenr)
go
CREATE INDEX pllistelinje##vglopnr ON pllistelinje (pllisteid, vargr, lopnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pllistemodell' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pllistemodell
go
CREATE TABLE pllistemodell (
  pllisteid decimal(10,2) null,
  modellfarge decimal(15,2) not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_pllistemodell ON pllistemodell for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pllistemodell t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pllistemodell#_#progress_recid ON pllistemodell (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pllistemodell#_#progress_recid_ident_ ON pllistemodell (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pllistemodell##pllistemodell ON pllistemodell (pllisteid, modellfarge, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pllistetype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pllistetype
go
CREATE TABLE pllistetype (
  plltype integer null,
  plnavn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_pllistetype ON pllistetype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pllistetype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pllistetype#_#progress_recid ON pllistetype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pllistetype#_#progress_recid_ident_ ON pllistetype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pllistetype##navn ON pllistetype (plnavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pllistetype##pllistetype ON pllistetype (plltype)
go
if (select name from sysobjects 
    where name = 'post' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table post
go
CREATE TABLE post (
  postnr varchar (10) null,
  kommnr varchar (30) null,
  beskrivelse varchar (30) null,
  merknad varchar (50) null,
  fylkesnr varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_post ON post for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from post t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX post#_#progress_recid ON post (PROGRESS_RECID)
go
CREATE UNIQUE INDEX post#_#progress_recid_ident_ ON post (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX post##beskrivelse ON post (beskrivelse, PROGRESS_RECID)
go
CREATE INDEX post##fylkesnr ON post (fylkesnr, PROGRESS_RECID)
go
CREATE INDEX post##kommnr ON post (kommnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX post##postnr ON post (postnr)
go
if (select name from sysobjects 
    where name = 'printlogg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table printlogg
go
CREATE TABLE printlogg (
  loggnr decimal(15,2) null,
  loggtype varchar (20) null,
  numloggnokkel decimal(15,2) null,
  charloggnokkel varchar (15) null,
  edato datetime null,
  etid integer null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  brukerid varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_printlogg ON printlogg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from printlogg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX printlogg#_#progress_recid ON printlogg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX printlogg#_#progress_recid_ident_ ON printlogg (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX printlogg##loggnr ON printlogg (loggnr)
go
CREATE INDEX printlogg##loggtype ON printlogg (loggtype, PROGRESS_RECID)
go
CREATE INDEX printlogg##numloggnokkel ON printlogg (numloggnokkel, loggnr, PROGRESS_RECID)
go
CREATE INDEX printlogg##registrertdato ON printlogg (registrertdato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'printloggtype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table printloggtype
go
CREATE TABLE printloggtype (
  loggtype varchar (20) null,
  beskrivelse varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_printloggtype ON printloggtype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from printloggtype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX printloggtype#_#progress_recid ON printloggtype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX printloggtype#_#progress_recid_ident_ ON printloggtype (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX printloggtype##loggtype ON printloggtype (loggtype)
go
if (select name from sysobjects 
    where name = 'prisgruppe' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prisgruppe
go
CREATE TABLE prisgruppe (
  prisgrpnr integer null,
  beskrivelse varchar (30) null,
  frapris decimal(7,2) null,
  tilpris decimal(7,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prisgruppe ON prisgruppe for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prisgruppe t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prisgruppe#_#progress_recid ON prisgruppe (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prisgruppe#_#progress_recid_ident_ ON prisgruppe (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prisgruppe##beskrivelse ON prisgruppe (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX prisgruppe##prisgruppe ON prisgruppe (prisgrpnr)
go
if (select name from sysobjects 
    where name = 'prisko' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prisko
go
CREATE TABLE prisko (
  artikkelnr decimal(13,0) null,
  levnr integer null,
  valpris decimal(11,2) null,
  innkjopspris decimal(7,2) null,
  rab1kr decimal(7,2) null,
  rab1_ decimal(5,2) null,
  rab2kr decimal(7,2) null,
  rab2_ decimal(5,2) null,
  frakt decimal(8,2) null,
  frakt_ decimal(5,2) null,
  divkostkr decimal(7,2) null,
  divkost_ decimal(5,2) null,
  rab3kr decimal(7,2) null,
  rab3_ decimal(5,2) null,
  dbkr decimal(7,2) null,
  db_ decimal(7,2) null,
  pris decimal(7,2) null,
  europris decimal(7,2) null,
  euromanuel tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  tilbud tinyint null,
  aktiveresdato datetime null,
  gyldigtildato datetime null,
  aktiverestid integer null,
  gyldigtiltid integer null,
  timestyrt tinyint null,
  aktivert tinyint null,
  profilnr integer null,
  type integer not null,
  varekost decimal(9,2) null,
  mvakr decimal(9,2) null,
  mva_ decimal(5,2) null,
  endringstype integer null,
  konummer decimal(10,2) null,
  momskod integer null,
  kampid decimal(13,0) null,
  etikettstatus integer null,
  loggiprisko integer null,
  klargjorstatus integer null,
  opphav varchar (4) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prisko ON prisko for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prisko t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prisko#_#progress_recid ON prisko (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prisko#_#progress_recid_ident_ ON prisko (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX prisko##aktiveres ON prisko (artikkelnr, profilnr, aktiveresdato, aktiverestid, tilbud)
go
CREATE INDEX prisko##etikettstatus ON prisko (etikettstatus, PROGRESS_RECID)
go
CREATE INDEX prisko##gyldigtil ON prisko (artikkelnr, gyldigtildato, gyldigtiltid, tilbud, PROGRESS_RECID)
go
CREATE INDEX prisko##kampid ON prisko (kampid, PROGRESS_RECID)
go
CREATE INDEX prisko##klargjorstatus ON prisko (klargjorstatus, PROGRESS_RECID)
go
CREATE INDEX prisko##konummer ON prisko (konummer, PROGRESS_RECID)
go
CREATE INDEX prisko##loggiprisko ON prisko (loggiprisko, PROGRESS_RECID)
go
CREATE INDEX prisko##opphav ON prisko (opphav, PROGRESS_RECID)
go
CREATE INDEX prisko##profilnr ON prisko (profilnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prislistehode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prislistehode
go
CREATE TABLE prislistehode (
  prislisteid integer null,
  prislistebeskrivelse varchar (30) null,
  aktiv tinyint null,
  fradato datetime null,
  tildato datetime null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  prislistenotat varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prislistehode ON prislistehode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prislistehode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prislistehode#_#progress_recid ON prislistehode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prislistehode#_#progress_recid_ident_ ON prislistehode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prislistehode##aktiv ON prislistehode (aktiv, PROGRESS_RECID)
go
CREATE INDEX prislistehode##fradato ON prislistehode (fradato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX prislistehode##prislisteid ON prislistehode (prislisteid)
go
CREATE INDEX prislistehode##tildato ON prislistehode (tildato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prislistekunde' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prislistekunde
go
CREATE TABLE prislistekunde (
  prislisteid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kundenr decimal(15,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prislistekunde ON prislistekunde for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prislistekunde t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prislistekunde#_#progress_recid ON prislistekunde (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prislistekunde#_#progress_recid_ident_ ON prislistekunde (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX prislistekunde##prislisteknd ON prislistekunde (prislisteid, kundenr)
go
if (select name from sysobjects 
    where name = 'prislistelinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prislistelinje
go
CREATE TABLE prislistelinje (
  prislisteid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  rab1kr decimal(7,2) null,
  rab1_ decimal(5,2) null,
  pris decimal(7,2) null,
  rabattproc tinyint null,
  artikkelnr decimal(13,0) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prislistelinje ON prislistelinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prislistelinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prislistelinje#_#progress_recid ON prislistelinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prislistelinje#_#progress_recid_ident_ ON prislistelinje (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX prislistelinje##prislistelinje ON prislistelinje (prislisteid, artikkelnr)
go
CREATE INDEX prislistelinje##rabattproc ON prislistelinje (rabattproc, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prisprofil' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prisprofil
go
CREATE TABLE prisprofil (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  profilnr integer null,
  kortnavn varchar (15) null,
  beskrivelse varchar (30) null,
  merknad varchar (50) null,
  notat varchar (50) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prisprofil ON prisprofil for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prisprofil t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prisprofil#_#progress_recid ON prisprofil (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prisprofil#_#progress_recid_ident_ ON prisprofil (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prisprofil##beskrivelse ON prisprofil (beskrivelse, PROGRESS_RECID)
go
CREATE INDEX prisprofil##kortnavn ON prisprofil (kortnavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX prisprofil##profilnr ON prisprofil (profilnr)
go
if (select name from sysobjects 
    where name = 'produktfamilie' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table produktfamilie
go
CREATE TABLE produktfamilie (
  prodfamid decimal(9,0) null,
  prodfamnavn varchar (30) null,
  prodfamprislinje decimal(10,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  prodfamnotat varchar (60) null,
  prodfamautoreg tinyint null,
  prodfamaktiv tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_produktfamilie ON produktfamilie for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from produktfamilie t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX produktfamilie#_#progress_recid ON produktfamilie (PROGRESS_RECID)
go
CREATE UNIQUE INDEX produktfamilie#_#progress_recid_ident_ ON produktfamilie (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX produktfamilie##prodfamnavn ON produktfamilie (prodfamnavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX produktfamilie##produktfamilie ON produktfamilie (prodfamid)
go
if (select name from sysobjects 
    where name = 'produktfammedlem' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table produktfammedlem
go
CREATE TABLE produktfammedlem (
  prodfamid decimal(9,0) null,
  prodfamartikkelnr decimal(13,0) null,
  prodfamstrkode integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_produktfammedlem ON produktfammedlem for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from produktfammedlem t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX produktfammedlem#_#progress_recid ON produktfammedlem (PROGRESS_RECID)
go
CREATE UNIQUE INDEX produktfammedlem#_#progress_recid_ident_ ON produktfammedlem (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX produktfammedlem##produktfammedlem ON produktfammedlem (prodfamid, prodfamartikkelnr, prodfamstrkode)
go
if (select name from sysobjects 
    where name = 'produsent' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table produsent
go
CREATE TABLE produsent (
  registrertdato datetime null,
  registrerttid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertav varchar (10) null,
  merknad varchar (30) null,
  beskrivelse varchar (50) null,
  notat varchar (60) null,
  adresse1 varchar (40) null,
  adresse2 varchar (40) null,
  postnr varchar (10) null,
  postboks varchar (40) null,
  telefon varchar (15) null,
  kontakt varchar (30) null,
  land varchar (20) null,
  prodnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_produsent ON produsent for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from produsent t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX produsent#_#progress_recid ON produsent (PROGRESS_RECID)
go
CREATE UNIQUE INDEX produsent#_#progress_recid_ident_ ON produsent (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX produsent##beskrivelse ON produsent (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX produsent##prodnr ON produsent (prodnr)
go
if (select name from sysobjects 
    where name = 'progbrgrp' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table progbrgrp
go
CREATE TABLE progbrgrp (
  brgrpnr integer not null,
  prognavn varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_progbrgrp ON progbrgrp for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from progbrgrp t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX progbrgrp#_#progress_recid ON progbrgrp (PROGRESS_RECID)
go
CREATE UNIQUE INDEX progbrgrp#_#progress_recid_ident_ ON progbrgrp (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX progbrgrp##progbrgrp ON progbrgrp (brgrpnr, prognavn)
go
if (select name from sysobjects 
    where name = 'programliste' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table programliste
go
CREATE TABLE programliste (
  prognavn varchar (20) null,
  programbeskrivelse varchar (40) null,
  grad integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_programliste ON programliste for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from programliste t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX programliste#_#progress_recid ON programliste (PROGRESS_RECID)
go
CREATE UNIQUE INDEX programliste#_#progress_recid_ident_ ON programliste (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX programliste##programbeskrivelse ON programliste (programbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX programliste##programliste ON programliste (prognavn)
go
if (select name from sysobjects 
    where name = 'prov' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prov
go
CREATE TABLE prov (
  provkod integer null,
  provproc decimal(4,2) null,
  provbeskr varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prov ON prov for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prov t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prov#_#progress_recid ON prov (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prov#_#progress_recid_ident_ ON prov (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prov##provbeskr ON prov (provbeskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX prov##provin ON prov (provkod)
go
if (select name from sysobjects 
    where name = 'purretrinn' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table purretrinn
go
CREATE TABLE purretrinn (
  purretrinn integer null,
  purretekst varchar (30) null,
  purrenotat varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  dagerforfallt integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_purretrinn ON purretrinn for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from purretrinn t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX purretrinn#_#progress_recid ON purretrinn (PROGRESS_RECID)
go
CREATE UNIQUE INDEX purretrinn#_#progress_recid_ident_ ON purretrinn (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX purretrinn##purretekst ON purretrinn (purretekst, PROGRESS_RECID)
go
CREATE UNIQUE INDEX purretrinn##purring ON purretrinn (purretrinn)
go
if (select name from sysobjects 
    where name = 'rabatt' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table rabatt
go
CREATE TABLE rabatt (
  rabkod integer null,
  rabproc decimal(4,2) null,
  rabbeskr varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_rabatt ON rabatt for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from rabatt t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX rabatt#_#progress_recid ON rabatt (PROGRESS_RECID)
go
CREATE UNIQUE INDEX rabatt#_#progress_recid_ident_ ON rabatt (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX rabatt##rabattin ON rabatt (rabkod)
go
CREATE INDEX rabatt##rabbeskr ON rabatt (rabbeskr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'rabsjekktype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table rabsjekktype
go
CREATE TABLE rabsjekktype (
  rabsjekktypenr integer not null,
  rabsjekktypebeskrivelse varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  artikkelnr decimal(15,2) null,
  terskeltildeling decimal(8,2) null,
  verdipasjekk decimal(7,2) null,
  kommentar varchar (30) null,
  antalldagergyldig integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_rabsjekktype ON rabsjekktype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from rabsjekktype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX rabsjekktype#_#progress_recid ON rabsjekktype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX rabsjekktype#_#progress_recid_ident_ ON rabsjekktype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX rabsjekktype##artikkelnr ON rabsjekktype (artikkelnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX rabsjekktype##rabsjekktype ON rabsjekktype (rabsjekktypenr)
go
CREATE INDEX rabsjekktype##rabsjekktypebeskrivelse ON rabsjekktype (rabsjekktypebeskrivelse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'rapptype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table rapptype
go
CREATE TABLE rapptype (
  type integer null,
  typenavn varchar (30) null,
  kundenr integer null,
  feltoppdat varchar (60) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_rapptype ON rapptype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from rapptype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX rapptype#_#progress_recid ON rapptype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX rapptype#_#progress_recid_ident_ ON rapptype (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX rapptype##rapptypeidx1 ON rapptype (kundenr, type)
go
CREATE INDEX rapptype##rapptypeidx2 ON rapptype (kundenr, typenavn, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'region' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table region
go
CREATE TABLE region (
  regkode varchar (10) null,
  navn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_region ON region for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from region t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX region#_#progress_recid ON region (PROGRESS_RECID)
go
CREATE UNIQUE INDEX region#_#progress_recid_ident_ ON region (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX region##navn ON region (navn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX region##region ON region (regkode)
go
if (select name from sysobjects 
    where name = 'regnskapsavdeling' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table regnskapsavdeling
go
CREATE TABLE regnskapsavdeling (
  ravdnr integer null,
  ravdbeskrivelse varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  omradeansvarlig varchar (10) not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_regnskapsavdeling ON regnskapsavdeling for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from regnskapsavdeling t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX regnskapsavdeling#_#progress_recid ON regnskapsavdeling (PROGRESS_RECID)
go
CREATE UNIQUE INDEX regnskapsavdeling#_#progress_recid_ident_ ON regnskapsavdeling (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX regnskapsavdeling##ravdbeskrivelse ON regnskapsavdeling (ravdbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX regnskapsavdeling##ravdnr ON regnskapsavdeling (ravdnr)
go
if (select name from sysobjects 
    where name = 'reklamasjonslinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table reklamasjonslinje
go
CREATE TABLE reklamasjonslinje (
  reklamasjonsnr decimal(15,2) not null,
  linjenr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  butik integer null,
  artikkelnr decimal(13,0) null,
  bongid integer null,
  bonglinjenr integer null,
  kassanr integer null,
  vg integer null,
  lopnr integer null,
  storl varchar (10) null,
  antall decimal(10,2) null,
  pris decimal(10,2) null,
  rabkr decimal(9,2) null,
  mva decimal(9,2) null,
  dato datetime null,
  tid integer null,
  seqnr integer null,
  vvarekost decimal(11,4) null,
  forsnr decimal(8,2) null,
  selgernr decimal(15,2) null,
  subtotalrab decimal(9,2) null,
  solgtibutikk integer null,
  solgtbongid integer null,
  solgtforsnr decimal(15,2) null,
  solgtdato datetime null,
  feilkode integer null,
  reklamverdi decimal(9,2) null,
  varetekst varchar (30) null,
  levkod varchar (20) null,
  feilnotat varchar (40) null,
  akseptertverdi decimal(9,2) null,
  reklamutgifter decimal(9,2) null,
  reklamtotal decimal(9,2) null,
  transnr integer null,
  levfargkod varchar (15) null,
  beskr varchar (20) null,
  ttid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_reklamasjonslinje ON reklamasjonslinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from reklamasjonslinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX reklamasjonslinje#_#progress_recid ON reklamasjonslinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX reklamasjonslinje#_#progress_recid_ident_ ON reklamasjonslinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX reklamasjonslinje##artikkel ON reklamasjonslinje (artikkelnr, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##beskr ON reklamasjonslinje (beskr, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##bong ON reklamasjonslinje (butik, kassanr, bongid, bonglinjenr, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##dato ON reklamasjonslinje (dato, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##kasserer ON reklamasjonslinje (forsnr, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##levfargkod ON reklamasjonslinje (levfargkod, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##levkod ON reklamasjonslinje (levkod, PROGRESS_RECID)
go
CREATE UNIQUE INDEX reklamasjonslinje##reklamasjonslinje ON reklamasjonslinje (reklamasjonsnr, linjenr)
go
CREATE INDEX reklamasjonslinje##selgernr ON reklamasjonslinje (selgernr, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##solgtbong ON reklamasjonslinje (solgtibutikk, solgtdato, solgtbongid, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##solgtdato ON reklamasjonslinje (solgtdato, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##translogg ON reklamasjonslinje (butik, transnr, seqnr, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##ttid ON reklamasjonslinje (ttid, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##varetekst ON reklamasjonslinje (varetekst, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslinje##vglopnr ON reklamasjonslinje (vg, lopnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'reklamasjonslogg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table reklamasjonslogg
go
CREATE TABLE reklamasjonslogg (
  levnr integer null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  kundenr decimal(15,2) null,
  betalesav integer not null,
  reklamstatus integer not null,
  kundenavn varchar (40) null,
  kundeadresse varchar (40) null,
  postnr varchar (10) null,
  reklamasjonsnr decimal(15,2) not null,
  reklamverdi decimal(9,2) null,
  kundetelefon varchar (15) null,
  kundemobil varchar (15) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  akseptertkunde integer null,
  akseptertdato datetime null,
  akseptertav varchar (15) null,
  betalesdato datetime null,
  betalesbruker varchar (15) null,
  akseptertnotat varchar (40) null,
  betalesnotat varchar (40) null,
  akseptertverdi decimal(9,2) null,
  reklamutgifter decimal(9,2) null,
  reklamtotal decimal(9,2) null,
  betalesavgjort tinyint null,
  kundee_mail varchar (30) null,
  akseptertbesluttet tinyint null,
  sluttfortav varchar (15) null,
  sluttfortdato datetime null,
  sluttfortbesluttet tinyint null,
  artikkelnr decimal(13,0) null,
  beskr varchar (20) null,
  levkod varchar (20) null,
  solgtdato datetime null,
  frist_dato datetime null,
  forsnr decimal(8,2) null,
  oppdlager tinyint null,
  oppdlagerdato datetime null,
  oppdlagerav varchar (15) null,
  fakturert tinyint null,
  fakturertdato datetime null,
  fakturertav varchar (15) null,
  reklammerknad varchar (30) null,
  butikknr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_reklamasjonslogg ON reklamasjonslogg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from reklamasjonslogg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX reklamasjonslogg#_#progress_recid ON reklamasjonslogg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX reklamasjonslogg#_#progress_recid_ident_ ON reklamasjonslogg (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX reklamasjonslogg##akseptertbesluttet ON reklamasjonslogg (akseptertbesluttet, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##akseptertdato ON reklamasjonslogg (akseptertdato, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##akseptertkunde ON reklamasjonslogg (akseptertkunde, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##artikkelnr ON reklamasjonslogg (artikkelnr, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##besluttetsluttfort ON reklamasjonslogg (sluttfortbesluttet, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##betalesav ON reklamasjonslogg (betalesav, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##betalesavgjort ON reklamasjonslogg (betalesavgjort, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##betalesdato ON reklamasjonslogg (betalesdato, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##fakturert ON reklamasjonslogg (fakturert, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##forsnr ON reklamasjonslogg (forsnr, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##frist_dato ON reklamasjonslogg (frist_dato, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##levkod ON reklamasjonslogg (levkod, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##levnr ON reklamasjonslogg (levnr, reklamasjonsnr, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##oppdlager ON reklamasjonslogg (oppdlager, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##registreringsbutikk ON reklamasjonslogg (butikknr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX reklamasjonslogg##reklamasjonsnr ON reklamasjonslogg (reklamasjonsnr)
go
CREATE INDEX reklamasjonslogg##reklammerknad ON reklamasjonslogg (reklammerknad, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##reklamstatus ON reklamasjonslogg (reklamstatus, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##sluttfortdato ON reklamasjonslogg (sluttfortdato, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##solgtdato ON reklamasjonslogg (solgtdato, PROGRESS_RECID)
go
CREATE INDEX reklamasjonslogg##varetekst ON reklamasjonslogg (beskr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'returkoderegister' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table returkoderegister
go
CREATE TABLE returkoderegister (
  returkodeid integer null,
  returkodetekst varchar (30) null,
  visikasse tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_returkoderegister ON returkoderegister for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from returkoderegister t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX returkoderegister#_#progress_recid ON returkoderegister (PROGRESS_RECID)
go
CREATE UNIQUE INDEX returkoderegister#_#progress_recid_ident_ ON returkoderegister (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX returkoderegister##returkoderegsiter ON returkoderegister (returkodeid)
go
CREATE INDEX returkoderegister##returkodetekst ON returkoderegister (returkodetekst, PROGRESS_RECID)
go
CREATE INDEX returkoderegister##visikasse ON returkoderegister (visikasse, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'salgsenhet' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table salgsenhet
go
CREATE TABLE salgsenhet (
  salgsenhid integer null,
  salgsenhtekst varchar (20) null,
  brukerid varchar (10) null,
  edato datetime null,
  etid integer null,
  registrertav varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_salgsenhet ON salgsenhet for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from salgsenhet t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX salgsenhet#_#progress_recid ON salgsenhet (PROGRESS_RECID)
go
CREATE UNIQUE INDEX salgsenhet#_#progress_recid_ident_ ON salgsenhet (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX salgsenhet##salgsenhetid ON salgsenhet (salgsenhid, PROGRESS_RECID)
go
CREATE INDEX salgsenhet##salgsenhtekst ON salgsenhet (salgsenhtekst, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'sasong' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sasong
go
CREATE TABLE sasong (
  sasong integer null,
  sasbeskr varchar (20) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  startdato datetime null,
  sluttdato datetime null,
  aktiv tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sasong ON sasong for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sasong t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sasong#_#progress_recid ON sasong (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sasong#_#progress_recid_ident_ ON sasong (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX sasong##aktiv ON sasong (aktiv, PROGRESS_RECID)
go
CREATE INDEX sasong##sasbeskr ON sasong (sasbeskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX sasong##sasongin ON sasong (sasong)
go
if (select name from sysobjects 
    where name = 'sbuddag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sbuddag
go
CREATE TABLE sbuddag (
  sbudid integer null,
  aarmnd integer null,
  aarmnddag integer null,
  salgbudsjett decimal(11,2) null,
  salgprosent decimal(7,2) null,
  dbbudsjett decimal(11,2) null,
  dbprosent decimal(7,2) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  brukerid varchar (10) null,
  edato datetime null,
  etid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sbuddag ON sbuddag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sbuddag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sbuddag#_#progress_recid ON sbuddag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sbuddag#_#progress_recid_ident_ ON sbuddag (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX sbuddag##sbuddag ON sbuddag (sbudid, aarmnd, aarmnddag, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'sbudhode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sbudhode
go
CREATE TABLE sbudhode (
  sbudid integer null,
  sbudbeskrivelse varchar (30) null,
  sbudnotat varchar (40) null,
  butikknr integer null,
  aar integer null,
  malid integer null,
  salgbudsjett decimal(12,2) null,
  dbbudsjett decimal(11,2) null,
  aktiv tinyint null,
  aktivertdato datetime null,
  aktiverttid integer null,
  aktivertav varchar (20) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  brukerid varchar (10) null,
  edato datetime null,
  etid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sbudhode ON sbudhode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sbudhode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sbudhode#_#progress_recid ON sbudhode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sbudhode#_#progress_recid_ident_ ON sbudhode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX sbudhode##sbudaar ON sbudhode (aar, PROGRESS_RECID)
go
CREATE INDEX sbudhode##sbudbeskrivelse ON sbudhode (sbudbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX sbudhode##sbudid ON sbudhode (sbudid)
go
if (select name from sysobjects 
    where name = 'sbudmaldag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sbudmaldag
go
CREATE TABLE sbudmaldag (
  malid integer null,
  aarmnd integer null,
  aarmnddag integer null,
  prosent decimal(7,2) null,
  dbprosent decimal(7,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sbudmaldag ON sbudmaldag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sbudmaldag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sbudmaldag#_#progress_recid ON sbudmaldag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sbudmaldag#_#progress_recid_ident_ ON sbudmaldag (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX sbudmaldag##sbudmaldag ON sbudmaldag (malid, aarmnd, aarmnddag, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'sbudmalhode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sbudmalhode
go
CREATE TABLE sbudmalhode (
  malid integer null,
  butikknr integer null,
  aar integer null,
  malbeskrivelse varchar (30) null,
  malnotat varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sbudmalhode ON sbudmalhode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sbudmalhode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sbudmalhode#_#progress_recid ON sbudmalhode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sbudmalhode#_#progress_recid_ident_ ON sbudmalhode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX sbudmalhode##malbeskrivelse ON sbudmalhode (malbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX sbudmalhode##malid ON sbudmalhode (malid)
go
CREATE INDEX sbudmalhode##sbudmalhode ON sbudmalhode (malid, butikknr, aar, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'sbudmalmaned' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sbudmalmaned
go
CREATE TABLE sbudmalmaned (
  malid integer null,
  aarmnd integer null,
  prosent decimal(7,2) null,
  dbprosent decimal(7,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sbudmalmaned ON sbudmalmaned for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sbudmalmaned t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sbudmalmaned#_#progress_recid ON sbudmalmaned (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sbudmalmaned#_#progress_recid_ident_ ON sbudmalmaned (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX sbudmalmaned##sbudmaldag ON sbudmalmaned (malid, aarmnd, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'sbudmaned' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sbudmaned
go
CREATE TABLE sbudmaned (
  sbudid integer null,
  aarmnd integer null,
  salgbudsjett decimal(11,2) null,
  salgprosent decimal(7,2) null,
  dbbudsjett decimal(11,2) null,
  dbprosent decimal(7,2) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  brukerid varchar (10) null,
  edato datetime null,
  etid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sbudmaned ON sbudmaned for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sbudmaned t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sbudmaned#_#progress_recid ON sbudmaned (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sbudmaned#_#progress_recid_ident_ ON sbudmaned (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX sbudmaned##sbudmaned ON sbudmaned (sbudid, aarmnd)
go
if (select name from sysobjects 
    where name = 'segmentregister' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table segmentregister
go
CREATE TABLE segmentregister (
  segmentkode integer null,
  segmentknavn varchar (30) null,
  segmentbeskrivelse varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_segmentregister ON segmentregister for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from segmentregister t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX segmentregister#_#progress_recid ON segmentregister (PROGRESS_RECID)
go
CREATE UNIQUE INDEX segmentregister#_#progress_recid_ident_ ON segmentregister (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX segmentregister##beskrivelse ON segmentregister (segmentbeskrivelse, PROGRESS_RECID)
go
CREATE INDEX segmentregister##knavn ON segmentregister (segmentknavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX segmentregister##segmentkode ON segmentregister (segmentkode)
go
if (select name from sysobjects 
    where name = 'selger' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table selger
go
CREATE TABLE selger (
  selgernr decimal(15,2) null,
  navn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  ansattnr varchar (15) null,
  adresse1 varchar (30) null,
  telefon varchar (15) null,
  personnr decimal(11,0) null,
  mobiltelefon varchar (15) null,
  postnr varchar (10) null,
  adresse2 varchar (30) null,
  navnikasse varchar (15) null,
  butikknr integer null,
  brukeridprs varchar (15) null,
  fornavn varchar (30) null,
  lonnprofil varchar (4) null,
  arbeidsprosent decimal(4,2) null,
  timelonn decimal(7,2) null,
  fastlonn decimal(8,2) null,
  ansattdato datetime null,
  sluttetdato datetime null,
  jobtittel varchar (30) null,
  fodtdato datetime null,
  decipwd decimal(9,0) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_selger ON selger for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from selger t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX selger#_#progress_recid ON selger (PROGRESS_RECID)
go
CREATE UNIQUE INDEX selger#_#progress_recid_ident_ ON selger (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX selger##adresse1 ON selger (adresse1, PROGRESS_RECID)
go
CREATE INDEX selger##butikknr ON selger (butikknr, PROGRESS_RECID)
go
CREATE INDEX selger##navn ON selger (navn, PROGRESS_RECID)
go
CREATE INDEX selger##navnikasse ON selger (navnikasse, PROGRESS_RECID)
go
CREATE INDEX selger##postnr ON selger (personnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX selger##selger ON selger (selgernr)
go
if (select name from sysobjects 
    where name = 'sendtowoocomm' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sendtowoocomm
go
CREATE TABLE sendtowoocomm (
  batchnr integer null,
  typ varchar (30) null,
  blobdata varbinary(max) null,
  skapad datetime null,
  fetched tinyint null,
  fetcheddt datetime null,
  butikknr integer null,
  extraparam varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sendtowoocomm ON sendtowoocomm for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sendtowoocomm t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sendtowoocomm#_#progress_recid ON sendtowoocomm (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sendtowoocomm#_#progress_recid_ident_ ON sendtowoocomm (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX sendtowoocomm##batchnr ON sendtowoocomm (batchnr)
go
CREATE INDEX sendtowoocomm##fetched ON sendtowoocomm (batchnr, fetched, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'sieeksport' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sieeksport
go
CREATE TABLE sieeksport (
  sieeksportnr decimal(13,0) null,
  ekspdato datetime null,
  eksptid integer null,
  filnavn varchar (30) null,
  filinnhold varbinary(max) null,
  kvittertmottatt varchar (20) null,
  salgsdato datetime null,
  butikknr integer null,
  merknad varchar (30) null,
  notat varchar (40) null,
  edato datetime null,
  brukerid varchar (10) null,
  registrerttid integer null,
  etid integer null,
  registrertdato datetime null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sieeksport ON sieeksport for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sieeksport t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sieeksport#_#progress_recid ON sieeksport (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sieeksport#_#progress_recid_ident_ ON sieeksport (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX sieeksport##siebuteksportdato ON sieeksport (butikknr, ekspdato, eksptid, PROGRESS_RECID)
go
CREATE INDEX sieeksport##siebutsalgsdato ON sieeksport (butikknr, salgsdato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX sieeksport##sieeksportnr ON sieeksport (sieeksportnr)
go
if (select name from sysobjects 
    where name = 'siemoms' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table siemoms
go
CREATE TABLE siemoms (
  butikknr integer null,
  momskod integer null,
  beskrivelse varchar (30) null,
  kontonrmva integer null,
  kontonrvaresalg integer null,
  kontonrvarekost integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_siemoms ON siemoms for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from siemoms t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX siemoms#_#progress_recid ON siemoms (PROGRESS_RECID)
go
CREATE UNIQUE INDEX siemoms#_#progress_recid_ident_ ON siemoms (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX siemoms##beskrivelse ON siemoms (butikknr, beskrivelse, PROGRESS_RECID)
go
CREATE INDEX siemoms##siemoms ON siemoms (butikknr, momskod, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'sienon_saletbid' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sienon_saletbid
go
CREATE TABLE sienon_saletbid (
  artikkelnr decimal(13,0) null,
  tbid integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sienon_saletbid ON sienon_saletbid for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sienon_saletbid t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sienon_saletbid#_#progress_recid ON sienon_saletbid (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sienon_saletbid#_#progress_recid_ident_ ON sienon_saletbid (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX sienon_saletbid##artikkelnr ON sienon_saletbid (artikkelnr)
go
if (select name from sysobjects 
    where name = 'sietrans' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sietrans
go
CREATE TABLE sietrans (
  sietransid decimal(15,2) null,
  kontonr integer null,
  ttid integer not null,
  butikknr integer null,
  kassenr integer not null,
  dato datetime null,
  avdhgvg integer null,
  belop decimal(11,2) null,
  sieeksportnr decimal(11,0) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sietrans ON sietrans for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sietrans t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sietrans#_#progress_recid ON sietrans (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sietrans#_#progress_recid_ident_ ON sietrans (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX sietrans##akkumkasse1 ON sietrans (sieeksportnr, butikknr, dato, kassenr, kontonr, avdhgvg, PROGRESS_RECID)
go
CREATE INDEX sietrans##butikktrans1 ON sietrans (butikknr, kassenr, dato, kontonr, PROGRESS_RECID)
go
CREATE INDEX sietrans##kontonr ON sietrans (kontonr, PROGRESS_RECID)
go
CREATE INDEX sietrans##sieeksportnr ON sietrans (sieeksportnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX sietrans##sietransid ON sietrans (sietransid)
go
if (select name from sysobjects 
    where name = 'sietranstype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sietranstype
go
CREATE TABLE sietranstype (
  butikknr integer null,
  ttid integer not null,
  tbid integer null,
  beskrivelse varchar (30) not null,
  kontonr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sietranstype ON sietranstype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sietranstype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sietranstype#_#progress_recid ON sietranstype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sietranstype#_#progress_recid_ident_ ON sietranstype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX sietranstype##beskrivelse ON sietranstype (beskrivelse, PROGRESS_RECID)
go
CREATE INDEX sietranstype##butkontonr ON sietranstype (butikknr, kontonr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX sietranstype##sietranstype ON sietranstype (butikknr, ttid, tbid)
go
if (select name from sysobjects 
    where name = 'slitsula' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table slitsula
go
CREATE TABLE slitsula (
  slit_id integer null,
  slitbeskr varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_slitsula ON slitsula for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from slitsula t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX slitsula#_#progress_recid ON slitsula (PROGRESS_RECID)
go
CREATE UNIQUE INDEX slitsula#_#progress_recid_ident_ ON slitsula (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX slitsula##slit_beskr ON slitsula (slitbeskr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX slitsula##slitsula ON slitsula (slit_id)
go
if (select name from sysobjects 
    where name = 'sprak' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sprak
go
CREATE TABLE sprak (
  lng varchar (3) null,
  beskrivelse varchar (20) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sprak ON sprak for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sprak t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sprak#_#progress_recid ON sprak (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sprak#_#progress_recid_ident_ ON sprak (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX sprak##lng ON sprak (lng)
go
if (select name from sysobjects 
    where name = 'stdef' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table stdef
go
CREATE TABLE stdef (
  sttypeid varchar (10) not null,
  beskrivelse varchar (30) null,
  perid varchar (10) not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_stdef ON stdef for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from stdef t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX stdef#_#progress_recid ON stdef (PROGRESS_RECID)
go
CREATE UNIQUE INDEX stdef#_#progress_recid_ident_ ON stdef (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX stdef##stdef ON stdef (sttypeid, perid)
go
if (select name from sysobjects 
    where name = 'sthode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sthode
go
CREATE TABLE sthode (
  sttypeid varchar (10) not null,
  beskrivelse varchar (30) null,
  perid varchar (10) not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sthode ON sthode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sthode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sthode#_#progress_recid ON sthode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sthode#_#progress_recid_ident_ ON sthode (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX sthode##sthode ON sthode (sttypeid, perid)
go
if (select name from sysobjects 
    where name = 'stlager' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table stlager
go
CREATE TABLE stlager (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  vvarekost decimal(11,4) null,
  lagant decimal(12,4) null,
  sistinnlevert datetime null,
  butik integer null,
  antsolgt decimal(12,4) null,
  brekkant decimal(9,4) null,
  intant decimal(9,4) null,
  reklant decimal(9,4) null,
  rekllant decimal(9,4) null,
  gjenkjopant decimal(9,4) null,
  retlant decimal(9,4) null,
  kjopant decimal(9,4) null,
  ovant decimal(9,4) null,
  justant decimal(9,4) null,
  justverdi decimal(10,2) null,
  svinnant decimal(9,4) null,
  svinnverdi decimal(10,2) null,
  nedant decimal(9,4) null,
  nedverdi decimal(10,2) null,
  verdisolgt decimal(10,2) null,
  kjopverdi decimal(10,2) null,
  brekkverdi decimal(10,2) null,
  intverdi decimal(10,2) null,
  reklverdi decimal(10,2) null,
  rekllverdi decimal(10,2) null,
  gjenkjopverdi decimal(10,2) null,
  ovverdi decimal(10,2) null,
  verdirabatt decimal(9,2) null,
  antrab decimal(12,4) null,
  sttypeid varchar (10) not null,
  dataobjekt varchar (15) null,
  vsnittkostpris decimal(10,2) null,
  svk decimal(10,2) null,
  aarperlinnr integer null,
  oppdatertdato datetime null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_stlager ON stlager for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from stlager t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX stlager#_#progress_recid ON stlager (PROGRESS_RECID)
go
CREATE UNIQUE INDEX stlager#_#progress_recid_ident_ ON stlager (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX stlager##butikk ON stlager (sttypeid, butik, dataobjekt, PROGRESS_RECID)
go
CREATE INDEX stlager##lager ON stlager (sttypeid, dataobjekt, butik, PROGRESS_RECID)
go
CREATE UNIQUE INDEX stlager##stbutikk ON stlager (aarperlinnr, sttypeid, butik, dataobjekt)
go
CREATE INDEX stlager##stlager ON stlager (aarperlinnr, sttypeid, dataobjekt, butik, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'stlagerhist' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table stlagerhist
go
CREATE TABLE stlagerhist (
  dato datetime null,
  butikknr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_stlagerhist ON stlagerhist for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from stlagerhist t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX stlagerhist#_#progress_recid ON stlagerhist (PROGRESS_RECID)
go
CREATE UNIQUE INDEX stlagerhist#_#progress_recid_ident_ ON stlagerhist (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX stlagerhist##butikk2 ON stlagerhist (butikknr, dato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX stlagerhist##hist ON stlagerhist (dato, butikknr)
go
if (select name from sysobjects 
    where name = 'stlinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table stlinje
go
CREATE TABLE stlinje (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  vvarekost decimal(11,4) null,
  butik integer null,
  antsolgt decimal(12,4) null,
  brekkant decimal(9,4) null,
  intant decimal(9,4) null,
  reklant decimal(9,4) null,
  rekllant decimal(9,4) null,
  gjenkjopant decimal(9,4) null,
  kjopant decimal(9,4) null,
  ovant decimal(9,4) null,
  justant decimal(9,4) null,
  justverdi decimal(10,2) null,
  svinnant decimal(9,4) null,
  svinnverdi decimal(10,2) null,
  nedant decimal(9,4) null,
  nedverdi decimal(10,2) null,
  verdisolgt decimal(10,2) null,
  kjopverdi decimal(10,2) null,
  brekkverdi decimal(10,2) null,
  intverdi decimal(10,2) null,
  reklverdi decimal(10,2) null,
  rekllverdi decimal(10,2) null,
  gjenkjopverdi decimal(10,2) null,
  ovverdi decimal(10,2) null,
  dataobjekt varchar (15) null,
  sttypeid varchar (10) not null,
  beskrivelse varchar (30) null,
  perid varchar (10) not null,
  aar integer null,
  perlinnr integer null,
  mvaverdi decimal(10,2) null,
  diverse varchar (15) null,
  anttilbsolgt decimal(12,4) null,
  verditilbsolgt decimal(10,2) null,
  tilbvvarekost decimal(11,4) null,
  tilbmvaverdi decimal(10,2) null,
  antrabatt decimal(12,4) null,
  verdirabatt decimal(10,2) null,
  lagerant integer null,
  primoant integer null,
  omlhast decimal(7,2) null,
  hg integer null,
  visbut varchar (7) null,
  perlintxt varchar (20) null,
  dbkr decimal(10,2) null,
  db_ decimal(6,2) null,
  utsolgt_ decimal(6,2) null,
  lagerverdi decimal(11,2) null,
  primoverdi decimal(11,2) null,
  diverseant decimal(11,2) null,
  diverseverdi decimal(11,2) null,
  totalpost integer null,
  aarperlinnr integer null,
  artvg integer null,
  artsasong integer null,
  artfarg integer null,
  artmatkod integer null,
  art_beskr varchar (20) null,
  artlevnr integer null,
  artlevkod varchar (20) null,
  artvmid integer null,
  artlevfargkod varchar (15) null,
  artprodnr integer null,
  kunderabatt decimal(9,2) null,
  medlemsrabatt decimal(9,2) null,
  personalrabatt decimal(9,2) null,
  generellrabatt decimal(9,2) null,
  tilbudsrabatt decimal(9,2) null,
  mixmatchrabatt decimal(9,2) null,
  alternativprisrabatt decimal(9,2) null,
  manuelendretprisrabatt decimal(9,2) null,
  subtotalrabattpersonal decimal(9,2) null,
  linjerabattpersonal decimal(9,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_stlinje ON stlinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from stlinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX stlinje#_#progress_recid ON stlinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX stlinje#_#progress_recid_ident_ ON stlinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX stlinje##aarperlinnr ON stlinje (butik, sttypeid, perid, aarperlinnr, dataobjekt, PROGRESS_RECID)
go
CREATE INDEX stlinje##artikkelkort ON stlinje (dataobjekt, sttypeid, perid, aar, perlinnr, butik, PROGRESS_RECID)
go
CREATE UNIQUE INDEX stlinje##stlinje ON stlinje (sttypeid, perid, dataobjekt, diverse, butik, aar, perlinnr)
go
CREATE INDEX stlinje##tmpidx1 ON stlinje (dataobjekt, sttypeid, butik, aar, perid, perlinnr, PROGRESS_RECID)
go
CREATE INDEX stlinje##tmpidx2 ON stlinje (dataobjekt, sttypeid, perid, butik, perlinnr, aar, PROGRESS_RECID)
go
CREATE INDEX stlinje##tmpidx3 ON stlinje (dataobjekt, sttypeid, aar, perid, perlinnr, butik, PROGRESS_RECID)
go
CREATE INDEX stlinje##totalpost ON stlinje (totalpost, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'strekkode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table strekkode
go
CREATE TABLE strekkode (
  kode varchar (20) null,
  artikkelnr decimal(13,0) null,
  strkode integer null,
  kodetype integer null,
  vareid integer null,
  hovednr tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  bestillingsnummer varchar (25) null,
  ikasse tinyint null,
  erpnr varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_strekkode ON strekkode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from strekkode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX strekkode#_#progress_recid ON strekkode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX strekkode#_#progress_recid_ident_ ON strekkode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX strekkode##artikkel ON strekkode (artikkelnr, kode, PROGRESS_RECID)
go
CREATE INDEX strekkode##bestillingsnummer ON strekkode (bestillingsnummer, PROGRESS_RECID)
go
CREATE INDEX strekkode##erpnr ON strekkode (erpnr, PROGRESS_RECID)
go
CREATE INDEX strekkode##hovednr ON strekkode (artikkelnr, hovednr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX strekkode##strekkode ON strekkode (kode)
go
CREATE INDEX strekkode##strkode ON strekkode (strkode, PROGRESS_RECID)
go
CREATE INDEX strekkode##str_lib ON strekkode (artikkelnr, strkode, kode, PROGRESS_RECID)
go
CREATE INDEX strekkode##vareid ON strekkode (vareid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'strkonv' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table strkonv
go
CREATE TABLE strkonv (
  strkode integer null,
  storl varchar (10) null,
  merknad varchar (60) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  seqnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_strkonv ON strkonv for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from strkonv t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX strkonv#_#progress_recid ON strkonv (PROGRESS_RECID)
go
CREATE UNIQUE INDEX strkonv#_#progress_recid_ident_ ON strkonv (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX strkonv##storl ON strkonv (storl)
go
CREATE UNIQUE INDEX strkonv##strkode ON strkonv (strkode)
go
if (select name from sysobjects 
    where name = 'strtstr' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table strtstr
go
CREATE TABLE strtstr (
  strtypeid integer null,
  sostorl varchar (10) null,
  seqnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_strtstr ON strtstr for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from strtstr t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX strtstr#_#progress_recid ON strtstr (PROGRESS_RECID)
go
CREATE UNIQUE INDEX strtstr#_#progress_recid_ident_ ON strtstr (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX strtstr##sostorl ON strtstr (strtypeid, sostorl, PROGRESS_RECID)
go
CREATE INDEX strtstr##storl ON strtstr (sostorl, PROGRESS_RECID)
go
CREATE UNIQUE INDEX strtstr##strtstr ON strtstr (strtypeid, seqnr)
go
if (select name from sysobjects 
    where name = 'strtype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table strtype
go
CREATE TABLE strtype (
  strtypeid integer null,
  beskrivelse varchar (30) null,
  intervall varchar (12) null,
  fordeling varchar (40) null,
  kortnavn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  alfafordeling varchar (40) null,
  hg integer null,
  avdelingnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_strtype ON strtype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from strtype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX strtype#_#progress_recid ON strtype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX strtype#_#progress_recid_ident_ ON strtype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX strtype##avdeling ON strtype (avdelingnr, hg, PROGRESS_RECID)
go
CREATE INDEX strtype##beskrivelse ON strtype (beskrivelse, PROGRESS_RECID)
go
CREATE INDEX strtype##huvgr ON strtype (hg, PROGRESS_RECID)
go
CREATE INDEX strtype##kortnavn ON strtype (kortnavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX strtype##strtype ON strtype (strtypeid)
go
if (select name from sysobjects 
    where name = 'sttype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sttype
go
CREATE TABLE sttype (
  sttypeid varchar (10) not null,
  beskrivelse varchar (30) null,
  oppdaterstatistikk tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sttype ON sttype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sttype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sttype#_#progress_recid ON sttype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sttype#_#progress_recid_ident_ ON sttype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX sttype##beskrivelse ON sttype (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX sttype##sttypeid ON sttype (sttypeid)
go
if (select name from sysobjects 
    where name = 'sysgruppe' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table sysgruppe
go
CREATE TABLE sysgruppe (
  syshid integer null,
  beskrivelse varchar (30) null,
  hjelpetekst varchar (50) null,
  sysgr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_sysgruppe ON sysgruppe for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from sysgruppe t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX sysgruppe#_#progress_recid ON sysgruppe (PROGRESS_RECID)
go
CREATE UNIQUE INDEX sysgruppe#_#progress_recid_ident_ ON sysgruppe (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX sysgruppe##beskrivelse ON sysgruppe (syshid, beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX sysgruppe##sysgruppe ON sysgruppe (syshid, sysgr)
go
if (select name from sysobjects 
    where name = 'syshode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table syshode
go
CREATE TABLE syshode (
  syshid integer null,
  beskrivelse varchar (30) null,
  hjelpetekst varchar (50) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_syshode ON syshode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from syshode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX syshode#_#progress_recid ON syshode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX syshode#_#progress_recid_ident_ ON syshode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX syshode##beskrivelse ON syshode (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX syshode##syshode ON syshode (syshid)
go
if (select name from sysobjects 
    where name = 'syspara' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table syspara
go
CREATE TABLE syspara (
  syshid integer null,
  beskrivelse varchar (30) null,
  hjelpetekst1 varchar (50) null,
  sysgr integer null,
  paranr integer null,
  parameter1 varchar (50) null,
  hjelpetekst2 varchar (50) null,
  parameter2 varchar (50) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_syspara ON syspara for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from syspara t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX syspara#_#progress_recid ON syspara (PROGRESS_RECID)
go
CREATE UNIQUE INDEX syspara#_#progress_recid_ident_ ON syspara (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX syspara##beskrivelse ON syspara (syshid, sysgr, beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX syspara##syspara ON syspara (syshid, sysgr, paranr)
go
if (select name from sysobjects 
    where name = 'teamtype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table teamtype
go
CREATE TABLE teamtype (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  teamtypeid integer not null,
  beskrivelse varchar (40) null,
  notat varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_teamtype ON teamtype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from teamtype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX teamtype#_#progress_recid ON teamtype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX teamtype#_#progress_recid_ident_ ON teamtype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX teamtype##beskrivelse ON teamtype (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX teamtype##teamtypeid ON teamtype (teamtypeid)
go
if (select name from sysobjects 
    where name = 'tekst' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table tekst
go
CREATE TABLE tekst (
  prgnavn varchar (25) null,
  txtnr integer null,
  lng varchar (3) null,
  tekst varchar (60) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_tekst ON tekst for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from tekst t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX tekst#_#progress_recid ON tekst (PROGRESS_RECID)
go
CREATE UNIQUE INDEX tekst#_#progress_recid_ident_ ON tekst (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX tekst##hovedindeks ON tekst (prgnavn, txtnr, lng)
go
CREATE INDEX tekst##lng ON tekst (prgnavn, lng, txtnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'tellehode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table tellehode
go
CREATE TABLE tellehode (
  tellenr integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  beskrivelse varchar (30) not null,
  ttid integer not null,
  tbid integer null,
  notat varchar (60) null,
  startdato datetime null,
  oppdatert datetime null,
  antallpar decimal(9,2) null,
  antalltalt decimal(9,2) null,
  opptverdi decimal(10,2) null,
  verdidiff decimal(10,2) null,
  antalldiff decimal(10,3) null,
  opprverdi decimal(11,4) null,
  butikkliste varchar (40) null,
  antlinjer integer null,
  tilbutikk integer null,
  ordrenr decimal(9,2) null,
  pksdlnr decimal(9,2) null,
  batchnr integer null,
  telletype integer null,
  koblettiltellenr integer null,
  lokasjonsid varchar (15) null,
  brukeridpda varchar (15) null,
  fildatopda datetime null,
  filtidpda integer null,
  verdinegdiff decimal(10,2) null,
  verdiposdiff decimal(10,2) null,
  antallnegdiff decimal(11,3) null,
  antallposdiff decimal(11,3) null,
  filid decimal(15,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_tellehode ON tellehode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from tellehode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX tellehode#_#progress_recid ON tellehode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX tellehode#_#progress_recid_ident_ ON tellehode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX tellehode##batchnr ON tellehode (batchnr, PROGRESS_RECID)
go
CREATE INDEX tellehode##beskrivelse ON tellehode (beskrivelse, tellenr, PROGRESS_RECID)
go
CREATE INDEX tellehode##bruker ON tellehode (brukeridpda, PROGRESS_RECID)
go
CREATE INDEX tellehode##filid ON tellehode (filid, PROGRESS_RECID)
go
CREATE INDEX tellehode##koblettiltelleliste ON tellehode (koblettiltellenr, PROGRESS_RECID)
go
CREATE INDEX tellehode##lokasjonsid ON tellehode (lokasjonsid, PROGRESS_RECID)
go
CREATE INDEX tellehode##oppdatert ON tellehode (oppdatert, tellenr, PROGRESS_RECID)
go
CREATE INDEX tellehode##ordre ON tellehode (ordrenr, PROGRESS_RECID)
go
CREATE INDEX tellehode##pakkseddel ON tellehode (pksdlnr, PROGRESS_RECID)
go
CREATE INDEX tellehode##startdato ON tellehode (startdato, tellenr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX tellehode##tellehode ON tellehode (tellenr)
go
CREATE INDEX tellehode##telletype ON tellehode (telletype, PROGRESS_RECID)
go
CREATE INDEX tellehode##transtype ON tellehode (ttid, tbid, tellenr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'tellelinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table tellelinje
go
CREATE TABLE tellelinje (
  tellenr integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  merknad varchar (30) not null,
  oppdatert tinyint null,
  vg integer null,
  lopnr integer null,
  levkod varchar (20) null,
  antallpar decimal(9,2) null,
  antalltalt decimal(9,2) null,
  opprverdi decimal(11,4) null,
  opptverdi decimal(10,2) null,
  verdidiff decimal(10,2) null,
  antalldiff decimal(9,2) null,
  nedskrevet decimal(9,2) null,
  artikkelnr decimal(13,0) null,
  butik integer null,
  storl varchar (10) null,
  levnr integer null,
  farg integer null,
  sasong integer null,
  matkod integer null,
  vvarekost decimal(7,2) null,
  vglopnr varchar (10) null,
  rabkr decimal(9,2) null,
  kode varchar (20) null,
  beskr varchar (20) null,
  seqnr integer null,
  levfargkod varchar (15) null,
  opprantaltalt decimal(9,2) null,
  linjenr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_tellelinje ON tellelinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from tellelinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX tellelinje#_#progress_recid ON tellelinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX tellelinje#_#progress_recid_ident_ ON tellelinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX tellelinje##artikkelnr ON tellelinje (artikkelnr, PROGRESS_RECID)
go
CREATE INDEX tellelinje##beskr ON tellelinje (tellenr, beskr, PROGRESS_RECID)
go
CREATE INDEX tellelinje##butikk ON tellelinje (tellenr, butik, vg, lopnr, storl, PROGRESS_RECID)
go
CREATE INDEX tellelinje##levfargkod ON tellelinje (tellenr, levfargkod, PROGRESS_RECID)
go
CREATE INDEX tellelinje##levkod ON tellelinje (tellenr, butik, levkod, vg, lopnr, storl, PROGRESS_RECID)
go
CREATE INDEX tellelinje##linjenr ON tellelinje (tellenr, linjenr, PROGRESS_RECID)
go
CREATE INDEX tellelinje##seqnr ON tellelinje (tellenr, artikkelnr, butik, seqnr, PROGRESS_RECID)
go
CREATE INDEX tellelinje##sortvglopnrstorl ON tellelinje (tellenr, butik, vglopnr, storl, PROGRESS_RECID)
go
CREATE INDEX tellelinje##strekkode ON tellelinje (tellenr, butik, kode, PROGRESS_RECID)
go
CREATE UNIQUE INDEX tellelinje##tellelinje ON tellelinje (tellenr, artikkelnr, butik, storl)
go
CREATE INDEX tellelinje##vglopnr ON tellelinje (tellenr, vg, lopnr, butik, storl, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'tgemp' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table tgemp
go
CREATE TABLE tgemp (
  tgstore_id integer null,
  tgempid decimal(10,2) null,
  tgexportid decimal(11,2) null,
  tgemployeeid integer null,
  tgfirstname varchar (20) null,
  tgsurename varchar (30) null,
  tgsalaryprofile integer null,
  tgworkpercentage integer null,
  tghourlyrate decimal(7,2) null,
  tgfixedsalary decimal(10,2) null,
  tgstorename varchar (50) null,
  tgadress1 varchar (50) null,
  tgadress2 varchar (50) null,
  tgpostalcode varchar (10) null,
  tgpostalarea varchar (50) null,
  tgbirthdate datetime null,
  tgbegindate datetime null,
  tgleavedate datetime null,
  tgemployeetitle varchar (50) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_tgemp ON tgemp for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from tgemp t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX tgemp#_#progress_recid ON tgemp (PROGRESS_RECID)
go
CREATE UNIQUE INDEX tgemp#_#progress_recid_ident_ ON tgemp (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX tgemp##tgempid ON tgemp (tgstore_id, tgempid, PROGRESS_RECID)
go
CREATE INDEX tgemp##tgemployeeid ON tgemp (tgstore_id, tgemployeeid, PROGRESS_RECID)
go
CREATE INDEX tgemp##tgexportid1 ON tgemp (tgexportid, tgstore_id, PROGRESS_RECID)
go
CREATE UNIQUE INDEX tgemp##tgposteremp1 ON tgemp (tgexportid, tgstore_id, tgemployeeid)
go
if (select name from sysobjects 
    where name = 'tgexport' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table tgexport
go
CREATE TABLE tgexport (
  tgstore_id integer null,
  tgexportid decimal(11,2) null,
  tgexportdate datetime null,
  tgexporttime integer null,
  tgremark varchar (30) null,
  tgnote varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  tgsalesdate datetime null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_tgexport ON tgexport for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from tgexport t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX tgexport#_#progress_recid ON tgexport (PROGRESS_RECID)
go
CREATE UNIQUE INDEX tgexport#_#progress_recid_ident_ ON tgexport (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX tgexport##tgbutexportdate1 ON tgexport (tgstore_id, tgexportdate, tgexporttime, PROGRESS_RECID)
go
CREATE INDEX tgexport##tgbutopprettet ON tgexport (tgstore_id, registrertdato, registrerttid, PROGRESS_RECID)
go
CREATE INDEX tgexport##tgexportdate1 ON tgexport (tgexportdate, tgexporttime, PROGRESS_RECID)
go
CREATE UNIQUE INDEX tgexport##tgexportid1 ON tgexport (tgexportid)
go
CREATE INDEX tgexport##tgopprettet ON tgexport (registrertdato, registrerttid, PROGRESS_RECID)
go
CREATE INDEX tgexport##tgpostert ON tgexport (tgstore_id, tgexportdate, PROGRESS_RECID)
go
CREATE INDEX tgexport##tgsalesdate ON tgexport (tgstore_id, tgsalesdate, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'tgsales' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table tgsales
go
CREATE TABLE tgsales (
  tgstore_id integer null,
  tgsalesid decimal(10,2) null,
  tgexportid decimal(11,2) null,
  tgdate integer null,
  tgoperator_id integer null,
  tgtimeperiod integer null,
  tgturnover decimal(9,2) null,
  tgvat decimal(10,2) null,
  tgnocustomers integer null,
  tgnoitems decimal(11,3) null,
  tggrossprofit decimal(10,2) null,
  tgvendorid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_tgsales ON tgsales for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from tgsales t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX tgsales#_#progress_recid ON tgsales (PROGRESS_RECID)
go
CREATE UNIQUE INDEX tgsales#_#progress_recid_ident_ ON tgsales (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX tgsales##tgexportid1 ON tgsales (tgexportid, tgstore_id, PROGRESS_RECID)
go
CREATE UNIQUE INDEX tgsales##tgpostersales2 ON tgsales (tgexportid, tgstore_id, tgoperator_id, tgdate, tgtimeperiod, tgvendorid)
go
CREATE INDEX tgsales##tgsalesid ON tgsales (tgstore_id, tgsalesid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'tgsales_ext' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table tgsales_ext
go
CREATE TABLE tgsales_ext (
  tgstore_id integer null,
  tgsales_extid decimal(10,2) null,
  tgexportid decimal(11,2) null,
  tgdate integer null,
  tgtimeperiod integer null,
  tgdatatype varchar (30) null,
  tgvalue decimal(10,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_tgsales_ext ON tgsales_ext for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from tgsales_ext t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX tgsales_ext#_#progress_recid ON tgsales_ext (PROGRESS_RECID)
go
CREATE UNIQUE INDEX tgsales_ext#_#progress_recid_ident_ ON tgsales_ext (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX tgsales_ext##tgexportid1 ON tgsales_ext (tgexportid, tgstore_id, PROGRESS_RECID)
go
CREATE UNIQUE INDEX tgsales_ext##tgpostersales_ext1 ON tgsales_ext (tgexportid, tgstore_id, tgdatatype, tgdate, tgtimeperiod)
go
CREATE INDEX tgsales_ext##tgsales_ext ON tgsales_ext (tgstore_id, tgsales_extid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'tgtimestamp' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table tgtimestamp
go
CREATE TABLE tgtimestamp (
  tgstore_id integer null,
  tgtimestampid decimal(10,2) null,
  tgexportid decimal(11,2) null,
  tgdate integer null,
  tgtime integer null,
  tgoperator_id integer null,
  tgoperator_name varchar (50) null,
  tgoperator_id2 integer null,
  tgaction integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  tgcomment varchar (40) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_tgtimestamp ON tgtimestamp for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from tgtimestamp t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX tgtimestamp#_#progress_recid ON tgtimestamp (PROGRESS_RECID)
go
CREATE UNIQUE INDEX tgtimestamp#_#progress_recid_ident_ ON tgtimestamp (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX tgtimestamp##tgexport1 ON tgtimestamp (tgexportid, tgstore_id, PROGRESS_RECID)
go
CREATE UNIQUE INDEX tgtimestamp##tgpostertimestamp1 ON tgtimestamp (tgexportid, tgstore_id, tgoperator_id, tgdate, tgtime, tgaction)
go
CREATE INDEX tgtimestamp##tgtimestampid ON tgtimestamp (tgstore_id, tgtimestampid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'tilgode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table tilgode
go
CREATE TABLE tilgode (
  butnr integer not null,
  identnr varchar (20) null,
  identtype integer not null,
  modus integer not null,
  dato datetime not null,
  tid integer not null,
  kassenr integer not null,
  kassnr integer not null,
  bongnr integer not null,
  gyldigdato datetime null,
  belop decimal(10,2) not null,
  bruktdato datetime null,
  brukttid integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  frab_id decimal(15,2) null,
  bruktb_id decimal(15,2) null,
  bruktbutnr integer null,
  bruktbongnr integer null,
  medlemsnr decimal(15,2) null,
  mfornavn varchar (40) null,
  madresse1 varchar (40) null,
  mpostnr varchar (10) null,
  mtelefon varchar (15) null,
  metternavn varchar (40) null,
  kundenr decimal(15,2) null,
  knavn varchar (40) null,
  kadresse1 varchar (40) null,
  kpostnr varchar (10) null,
  ktelefon varchar (15) null,
  selgernr decimal(15,2) null,
  bruktkassnr integer not null,
  bruktselgernr decimal(15,2) null,
  bruktkassenr integer not null,
  utgatt tinyint null,
  utgattdato datetime null,
  utgatttid integer null,
  utgattregav varchar (10) null,
  eget tinyint null,
  rabkr decimal(7,2) null,
  fakturert tinyint null,
  fakturertdato datetime null,
  fakturanr decimal(15,2) null,
  faktura_id decimal(15,2) not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_tilgode ON tilgode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from tilgode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX tilgode#_#progress_recid ON tilgode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX tilgode#_#progress_recid_ident_ ON tilgode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX tilgode##bruktbutnr ON tilgode (bruktbutnr, PROGRESS_RECID)
go
CREATE INDEX tilgode##eget ON tilgode (eget, PROGRESS_RECID)
go
CREATE INDEX tilgode##faktdato ON tilgode (fakturertdato, PROGRESS_RECID)
go
CREATE INDEX tilgode##fakturanr ON tilgode (fakturanr, PROGRESS_RECID)
go
CREATE INDEX tilgode##faktura_id ON tilgode (faktura_id, PROGRESS_RECID)
go
CREATE INDEX tilgode##fakturert ON tilgode (fakturert, PROGRESS_RECID)
go
CREATE INDEX tilgode##identnr ON tilgode (identnr, PROGRESS_RECID)
go
CREATE INDEX tilgode##kadresse ON tilgode (madresse1, PROGRESS_RECID)
go
CREATE INDEX tilgode##ktelefon ON tilgode (ktelefon, PROGRESS_RECID)
go
CREATE INDEX tilgode##kunde ON tilgode (kundenr, PROGRESS_RECID)
go
CREATE INDEX tilgode##madresse ON tilgode (madresse1, PROGRESS_RECID)
go
CREATE INDEX tilgode##medlem ON tilgode (medlemsnr, PROGRESS_RECID)
go
CREATE INDEX tilgode##metternav ON tilgode (metternavn, PROGRESS_RECID)
go
CREATE INDEX tilgode##mknavn ON tilgode (knavn, PROGRESS_RECID)
go
CREATE INDEX tilgode##mtelefon ON tilgode (mtelefon, PROGRESS_RECID)
go
CREATE UNIQUE INDEX tilgode##tilgodeidx1 ON tilgode (butnr, identnr)
go
CREATE INDEX tilgode##tilgodeidx2 ON tilgode (dato, tid, PROGRESS_RECID)
go
CREATE INDEX tilgode##tilgodeidx3 ON tilgode (gyldigdato, PROGRESS_RECID)
go
CREATE INDEX tilgode##tilgodeidx4 ON tilgode (bruktdato, brukttid, PROGRESS_RECID)
go
CREATE INDEX tilgode##tilgodeidx6 ON tilgode (identtype, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'transbeskr' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table transbeskr
go
CREATE TABLE transbeskr (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  notat varchar (60) null,
  beskrivelse varchar (30) not null,
  ttid integer not null,
  tbid integer null,
  innloser varchar (10) null,
  royalty_ decimal(7,2) null,
  vispabokfbilag tinyint null,
  kontonr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_transbeskr ON transbeskr for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from transbeskr t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX transbeskr#_#progress_recid ON transbeskr (PROGRESS_RECID)
go
CREATE UNIQUE INDEX transbeskr#_#progress_recid_ident_ ON transbeskr (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX transbeskr##beskrivelse ON transbeskr (ttid, beskrivelse, PROGRESS_RECID)
go
CREATE INDEX transbeskr##innloser ON transbeskr (innloser, PROGRESS_RECID)
go
CREATE INDEX transbeskr##kortnavn ON transbeskr (innloser, PROGRESS_RECID)
go
CREATE UNIQUE INDEX transbeskr##transbeskr ON transbeskr (ttid, tbid)
go
CREATE INDEX transbeskr##ttidinnloser ON transbeskr (ttid, innloser, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'translogg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table translogg
go
CREATE TABLE translogg (
  batchnr integer null,
  butik integer null,
  transnr integer null,
  forsnr decimal(8,2) null,
  ttid integer not null,
  tbid integer null,
  artikkelnr decimal(13,0) null,
  levnr integer null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  bongid integer null,
  bonglinjenr integer null,
  kassanr integer null,
  vg integer null,
  lopnr integer null,
  storl varchar (10) null,
  antall decimal(12,4) null,
  pris decimal(10,2) null,
  rabkr decimal(9,2) null,
  mva decimal(13,6) null,
  plukket tinyint null,
  dato datetime null,
  tid integer null,
  postert tinyint null,
  postertdato datetime null,
  posterttid integer null,
  bestnr integer null,
  ovbutik integer null,
  ovtransnr integer null,
  seqnr integer null,
  feilkode integer null,
  tilstorl varchar (10) null,
  vvarekost decimal(11,4) null,
  sattvvarekost tinyint null,
  medlemsnr decimal(15,2) null,
  kortnr varchar (22) null,
  korttype integer null,
  kundnr decimal(15,2) null,
  kalkylepris decimal(10,2) null,
  profilnr integer null,
  selgernr decimal(15,2) null,
  subtotalrab decimal(9,2) null,
  reftekst varchar (40) null,
  kode varchar (20) null,
  refnr integer null,
  ordreforslag tinyint null,
  linjerab decimal(9,2) null,
  personalrab decimal(9,2) null,
  bongtekst varchar (30) null,
  neglager integer null,
  individnr decimal(12,0) not null,
  mva_ decimal(5,2) null,
  varekost decimal(7,2) null,
  kampid decimal(15,2) null,
  kampeierid integer null,
  kamptilbid integer null,
  kunderabatt decimal(9,2) null,
  medlemsrabatt decimal(9,2) null,
  personalrabatt decimal(9,2) null,
  generellrabatt decimal(9,2) null,
  tilbudsrabatt decimal(9,2) null,
  mixmatchrabatt decimal(9,2) null,
  alternativprisrabatt decimal(9,2) null,
  manuelendretprisrabatt decimal(9,2) null,
  subtotalrabattpersonal decimal(9,2) null,
  linjerabattpersonal decimal(9,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_translogg ON translogg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from translogg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX translogg#_#progress_recid ON translogg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX translogg#_#progress_recid_ident_ ON translogg (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX translogg##artbongtekst ON translogg (artikkelnr, bongtekst, PROGRESS_RECID)
go
CREATE INDEX translogg##artbutstr ON translogg (artikkelnr, butik, storl, PROGRESS_RECID)
go
CREATE INDEX translogg##batchlogg ON translogg (batchnr, butik, transnr, seqnr, PROGRESS_RECID)
go
CREATE INDEX translogg##batchoppdatering ON translogg (batchnr, postert, butik, transnr, seqnr, PROGRESS_RECID)
go
CREATE INDEX translogg##butartstr ON translogg (butik, artikkelnr, storl, PROGRESS_RECID)
go
CREATE INDEX translogg##butdatotid ON translogg (butik, dato, tid, PROGRESS_RECID)
go
CREATE INDEX translogg##butvglopnrstr ON translogg (butik, vg, lopnr, storl, PROGRESS_RECID)
go
CREATE INDEX translogg##dato ON translogg (dato, ttid, butik, PROGRESS_RECID)
go
CREATE INDEX translogg##feilkode ON translogg (feilkode, butik, transnr, seqnr, PROGRESS_RECID)
go
CREATE INDEX translogg##individ ON translogg (individnr, PROGRESS_RECID)
go
CREATE INDEX translogg##kampid ON translogg (kampid, PROGRESS_RECID)
go
CREATE INDEX translogg##kamptilbid ON translogg (kamptilbid, PROGRESS_RECID)
go
CREATE INDEX translogg##kunde ON translogg (kundnr, dato, tid, postert, PROGRESS_RECID)
go
CREATE INDEX translogg##medlem ON translogg (medlemsnr, dato, tid, postert, PROGRESS_RECID)
go
CREATE INDEX translogg##oppslagdatotid ON translogg (artikkelnr, dato, tid, butik, ttid, PROGRESS_RECID)
go
CREATE INDEX translogg##oppslagkunde ON translogg (artikkelnr, kundnr, dato, tid, butik, ttid, tbid, PROGRESS_RECID)
go
CREATE INDEX translogg##oppslagselger ON translogg (artikkelnr, forsnr, dato, tid, butik, ttid, tbid, PROGRESS_RECID)
go
CREATE INDEX translogg##oppslagstr ON translogg (artikkelnr, storl, dato, tid, butik, ttid, PROGRESS_RECID)
go
CREATE INDEX translogg##plukket ON translogg (plukket, butik, vg, lopnr, storl, PROGRESS_RECID)
go
CREATE INDEX translogg##postert ON translogg (postert, butik, transnr, seqnr, PROGRESS_RECID)
go
CREATE INDEX translogg##strekkode ON translogg (kode, PROGRESS_RECID)
go
CREATE UNIQUE INDEX translogg##translogg ON translogg (butik, transnr, seqnr)
go
CREATE INDEX translogg##transtype ON translogg (ttid, PROGRESS_RECID)
go
CREATE INDEX translogg##vglpnrstrbut ON translogg (vg, lopnr, storl, butik, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'transtype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table transtype
go
CREATE TABLE transtype (
  ttid integer not null,
  beskrivelse varchar (30) not null,
  notat varchar (60) null,
  aktiv tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_transtype ON transtype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from transtype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX transtype#_#progress_recid ON transtype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX transtype#_#progress_recid_ident_ ON transtype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX transtype##aktiv ON transtype (aktiv, PROGRESS_RECID)
go
CREATE INDEX transtype##beskrivelse ON transtype (beskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX transtype##transtype ON transtype (ttid)
go
if (select name from sysobjects 
    where name = 'underkategori' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table underkategori
go
CREATE TABLE underkategori (
  underkatnr integer null,
  underkattekst varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_underkategori ON underkategori for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from underkategori t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX underkategori#_#progress_recid ON underkategori (PROGRESS_RECID)
go
CREATE UNIQUE INDEX underkategori#_#progress_recid_ident_ ON underkategori (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX underkategori##underkategori ON underkategori (underkatnr)
go
CREATE INDEX underkategori##underkattekst ON underkategori (underkattekst, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'utbettype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table utbettype
go
CREATE TABLE utbettype (
  utbettid integer null,
  utbbeskrivelse varchar (30) null,
  kontonr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_utbettype ON utbettype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from utbettype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX utbettype#_#progress_recid ON utbettype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX utbettype#_#progress_recid_ident_ ON utbettype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX utbettype##kontonr ON utbettype (kontonr, PROGRESS_RECID)
go
CREATE INDEX utbettype##utbbeskrivelse ON utbettype (utbbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX utbettype##utbettype ON utbettype (utbettid)
go
if (select name from sysobjects 
    where name = 'valuta' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table valuta
go
CREATE TABLE valuta (
  valkod varchar (3) null,
  valkurs decimal(8,6) null,
  valland varchar (15) null,
  valdatum datetime null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  valnr integer null,
  valnavn varchar (30) null,
  indeks integer null,
  retur decimal(7,3) null,
  kassevalkurs decimal(4,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_valuta ON valuta for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from valuta t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX valuta#_#progress_recid ON valuta (PROGRESS_RECID)
go
CREATE UNIQUE INDEX valuta#_#progress_recid_ident_ ON valuta (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX valuta##valland ON valuta (valland, PROGRESS_RECID)
go
CREATE UNIQUE INDEX valuta##valutain ON valuta (valkod)
go
if (select name from sysobjects 
    where name = 'varebehbesthode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varebehbesthode
go
CREATE TABLE varebehbesthode (
  varebehnr decimal(15,2) null,
  clbutikknr integer null,
  hodelinjeid integer null,
  artikkelnr decimal(13,0) null,
  levdato datetime null,
  levuke integer null,
  godkjent tinyint null,
  direktelev tinyint null,
  bestnr integer null,
  levnr integer null,
  storrelser varchar (33) null,
  butikkliste varchar (30) null,
  ordrenr integer not null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  alfafordeling varchar (40) null,
  antlevert decimal(7,2) null,
  verdilevert decimal(9,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varebehbesthode ON varebehbesthode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varebehbesthode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varebehbesthode#_#progress_recid ON varebehbesthode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehbesthode#_#progress_recid_ident_ ON varebehbesthode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX varebehbesthode##artvare ON varebehbesthode (artikkelnr, varebehnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehbesthode##varebbuthodelin ON varebehbesthode (varebehnr, clbutikknr, hodelinjeid)
go
CREATE INDEX varebehbesthode##vareclbutartuke ON varebehbesthode (varebehnr, clbutikknr, artikkelnr, levuke, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'varebehbestlinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varebehbestlinje
go
CREATE TABLE varebehbestlinje (
  varebehnr decimal(15,2) null,
  clbutikknr integer null,
  hodelinjeid integer null,
  storl varchar (10) null,
  bestilt decimal(7,2) null,
  bestiltbutikknr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varebehbestlinje ON varebehbestlinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varebehbestlinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varebehbestlinje#_#progress_recid ON varebehbestlinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehbestlinje#_#progress_recid_ident_ ON varebehbestlinje (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX varebehbestlinje##vbhbs ON varebehbestlinje (varebehnr, clbutikknr, hodelinjeid, bestiltbutikknr, storl)
go
if (select name from sysobjects 
    where name = 'varebehhode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varebehhode
go
CREATE TABLE varebehhode (
  varebehnr decimal(15,2) null,
  varebehbeskrivelse varchar (40) null,
  messenr decimal(10,2) null,
  varebehnotat varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  profilnr integer null,
  oppdatert tinyint null,
  oppdatdato datetime null,
  oppdatav varchar (15) null,
  varebehtype integer null,
  kilde decimal(15,2) null,
  butikkliste varchar (30) null,
  ekstref varchar (20) null,
  frakundeordre tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varebehhode ON varebehhode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varebehhode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varebehhode#_#progress_recid ON varebehhode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehhode#_#progress_recid_ident_ ON varebehhode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX varebehhode##frakundeordre ON varebehhode (frakundeordre, PROGRESS_RECID)
go
CREATE INDEX varebehhode##kilde ON varebehhode (kilde, PROGRESS_RECID)
go
CREATE INDEX varebehhode##messenr ON varebehhode (messenr, PROGRESS_RECID)
go
CREATE INDEX varebehhode##oppdatert ON varebehhode (oppdatert, PROGRESS_RECID)
go
CREATE INDEX varebehhode##profilnr ON varebehhode (profilnr, PROGRESS_RECID)
go
CREATE INDEX varebehhode##varebehbeskrivelse ON varebehhode (varebehbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehhode##varebehhode ON varebehhode (varebehnr)
go
CREATE INDEX varebehhode##varebehtype ON varebehhode (varebehtype, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'varebehlinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varebehlinje
go
CREATE TABLE varebehlinje (
  varebehnr decimal(15,2) null,
  modellfarge decimal(15,2) not null,
  artikkelnr decimal(13,0) null,
  beskr varchar (30) null,
  levkod varchar (30) null,
  levfargkod varchar (15) null,
  vg integer null,
  hg integer null,
  avdelingnr integer null,
  levnr integer null,
  anbefaltpris decimal(8,2) null,
  antall decimal(8,3) null,
  avdelingnavn varchar (30) null,
  brukerid varchar (10) null,
  db_ decimal(7,2) null,
  dbkr decimal(7,2) null,
  edato datetime null,
  etid integer null,
  forhkalkyle decimal(3,2) null,
  forhrab_ decimal(5,2) null,
  hgbeskr varchar (20) null,
  innkjopspris decimal(7,2) null,
  kampanjepris decimal(8,2) null,
  katalogpris decimal(8,2) null,
  kjedeinnkpris decimal(7,2) null,
  kjederab_ decimal(4,2) null,
  levdato1 datetime null,
  levdato2 datetime null,
  levdato3 datetime null,
  levdato4 datetime null,
  levnamn varchar (30) null,
  linjemerknad varchar (40) null,
  mva_ decimal(5,2) null,
  pris decimal(7,2) null,
  prodnr integer null,
  produsentbeskrivelse varchar (50) null,
  rab1kr decimal(7,2) null,
  registrertav varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  supantall decimal(8,3) null,
  supdb_ decimal(7,2) null,
  supdbkr decimal(7,2) null,
  supinnkjopspris decimal(7,2) null,
  supkalkyle decimal(3,2) null,
  suppris decimal(7,2) null,
  suprab_ decimal(5,2) null,
  suprab1kr decimal(7,2) null,
  supvarekost decimal(9,2) null,
  varekost decimal(9,2) null,
  vgbeskr varchar (20) null,
  vpidato datetime null,
  sortering integer null,
  kjedevare tinyint null,
  gjennomfaktureres tinyint null,
  utvidetsok varchar (100) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varebehlinje ON varebehlinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varebehlinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varebehlinje#_#progress_recid ON varebehlinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehlinje#_#progress_recid_ident_ ON varebehlinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX varebehlinje##artikkelnr ON varebehlinje (artikkelnr, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##avdeling ON varebehlinje (varebehnr, avdelingnr, hg, vg, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##huvgr ON varebehlinje (varebehnr, hg, vg, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##levfargkod ON varebehlinje (varebehnr, levfargkod, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##levkod ON varebehlinje (varebehnr, levkod, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##levnamn ON varebehlinje (varebehnr, levnamn, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##levnbeskr ON varebehlinje (varebehnr, levnamn, beskr, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##levnr ON varebehlinje (levnr, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##modellfarge ON varebehlinje (varebehnr, modellfarge, artikkelnr, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##sortering ON varebehlinje (sortering, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##varebehlevnr ON varebehlinje (varebehnr, levnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehlinje##varebehlinje ON varebehlinje (varebehnr, artikkelnr)
go
CREATE INDEX varebehlinje##varebehnrsortering ON varebehlinje (varebehnr, sortering, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##varelinje ON varebehlinje (varebehnr, artikkelnr, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##varetekst ON varebehlinje (varebehnr, beskr, PROGRESS_RECID)
go
CREATE INDEX varebehlinje##vargr ON varebehlinje (varebehnr, vg, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'varebehlinjethode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varebehlinjethode
go
CREATE TABLE varebehlinjethode (
  varebehnr decimal(15,2) null,
  butikknr integer null,
  godkjent tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varebehlinjethode ON varebehlinjethode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varebehlinjethode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varebehlinjethode#_#progress_recid ON varebehlinjethode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehlinjethode#_#progress_recid_ident_ ON varebehlinjethode (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX varebehlinjethode##varebehbut ON varebehlinjethode (varebehnr, butikknr)
go
if (select name from sysobjects 
    where name = 'varebehlinjetrans' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varebehlinjetrans
go
CREATE TABLE varebehlinjetrans (
  varebehnr decimal(15,2) null,
  butikknr integer null,
  artikkelnr decimal(13,0) null,
  kode varchar (20) null,
  levdato1 datetime null,
  levdato2 datetime null,
  levdato3 datetime null,
  levdato4 datetime null,
  bestilt1 decimal(7,2) null,
  bestilt2 decimal(7,2) null,
  bestilt3 decimal(7,2) null,
  bestilt4 decimal(7,2) null,
  levuke1 integer null,
  levuke2 integer null,
  levuke3 integer null,
  levuke4 integer null,
  seqnr integer null,
  utskrnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  godkjentbestilling tinyint null,
  registrertbestilling tinyint null,
  strkode integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varebehlinjetrans ON varebehlinjetrans for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varebehlinjetrans t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varebehlinjetrans#_#progress_recid ON varebehlinjetrans (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehlinjetrans#_#progress_recid_ident_ ON varebehlinjetrans (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX varebehlinjetrans##behbutart ON varebehlinjetrans (varebehnr, butikknr, artikkelnr, PROGRESS_RECID)
go
CREATE INDEX varebehlinjetrans##behbutart2 ON varebehlinjetrans (artikkelnr, varebehnr, butikknr, seqnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehlinjetrans##behbutkode2 ON varebehlinjetrans (varebehnr, butikknr, kode, artikkelnr)
go
CREATE INDEX varebehlinjetrans##godkjentbestilling ON varebehlinjetrans (godkjentbestilling, PROGRESS_RECID)
go
CREATE INDEX varebehlinjetrans##kode ON varebehlinjetrans (kode, PROGRESS_RECID)
go
CREATE INDEX varebehlinjetrans##registrertbestilling ON varebehlinjetrans (varebehnr, artikkelnr, registrertbestilling, butikknr, PROGRESS_RECID)
go
CREATE INDEX varebehlinjetrans##seqnr ON varebehlinjetrans (seqnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'varebehpris' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varebehpris
go
CREATE TABLE varebehpris (
  varebehnr decimal(15,2) null,
  beststat integer null,
  valpris decimal(11,2) null,
  innkjopspris decimal(7,2) null,
  rab1kr decimal(7,2) null,
  rab1_ decimal(5,2) null,
  rab2kr decimal(7,2) null,
  rab2_ decimal(5,2) null,
  frakt decimal(8,2) null,
  frakt_ decimal(5,2) null,
  divkostkr decimal(7,2) null,
  divkost_ decimal(5,2) null,
  rab3kr decimal(7,2) null,
  rab3_ decimal(5,2) null,
  dbkr decimal(7,2) null,
  db_ decimal(7,2) null,
  pris decimal(7,2) null,
  europris decimal(7,2) null,
  euromanuel tinyint null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  profilnr integer null,
  artikkelnr decimal(13,0) null,
  varekost decimal(9,2) null,
  mvakr decimal(9,2) null,
  mva_ decimal(5,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varebehpris ON varebehpris for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varebehpris t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varebehpris#_#progress_recid ON varebehpris (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehpris#_#progress_recid_ident_ ON varebehpris (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX varebehpris##vareart ON varebehpris (varebehnr, artikkelnr)
go
if (select name from sysobjects 
    where name = 'varebehtype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varebehtype
go
CREATE TABLE varebehtype (
  varebehtype integer null,
  beskrivelsevarebehtype varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varebehtype ON varebehtype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varebehtype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varebehtype#_#progress_recid ON varebehtype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehtype#_#progress_recid_ident_ ON varebehtype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX varebehtype##beskrivelsevarebehtype ON varebehtype (beskrivelsevarebehtype, PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebehtype##varebehtype ON varebehtype (varebehtype)
go
if (select name from sysobjects 
    where name = 'varebhbrukergrp' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varebhbrukergrp
go
CREATE TABLE varebhbrukergrp (
  varebehnr decimal(15,2) null,
  brgrpnr integer not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varebhbrukergrp ON varebhbrukergrp for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varebhbrukergrp t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varebhbrukergrp#_#progress_recid ON varebhbrukergrp (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebhbrukergrp#_#progress_recid_ident_ ON varebhbrukergrp (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX varebhbrukergrp##varebhgrpnr ON varebhbrukergrp (varebehnr, brgrpnr)
go
if (select name from sysobjects 
    where name = 'varebokhode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varebokhode
go
CREATE TABLE varebokhode (
  vareboknr decimal(15,2) null,
  varebokbeskrivelse varchar (40) null,
  messenr decimal(10,2) null,
  vareboknotat varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  profilnr integer null,
  oppdatert tinyint null,
  oppdatdato datetime null,
  oppdatav varchar (15) null,
  vareboktype integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varebokhode ON varebokhode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varebokhode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varebokhode#_#progress_recid ON varebokhode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebokhode#_#progress_recid_ident_ ON varebokhode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX varebokhode##messenr ON varebokhode (messenr, PROGRESS_RECID)
go
CREATE INDEX varebokhode##oppdatert ON varebokhode (oppdatert, PROGRESS_RECID)
go
CREATE INDEX varebokhode##profilnr ON varebokhode (profilnr, PROGRESS_RECID)
go
CREATE INDEX varebokhode##varebokbeskrivelse ON varebokhode (varebokbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX varebokhode##varebokhode ON varebokhode (vareboknr)
go
CREATE INDEX varebokhode##vareboktype ON varebokhode (vareboktype, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'vareboklinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vareboklinje
go
CREATE TABLE vareboklinje (
  vareboknr decimal(15,2) null,
  modellfarge decimal(15,2) not null,
  artikkelnr decimal(13,0) null,
  beskr varchar (30) null,
  levkod varchar (30) null,
  levfargkod varchar (15) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  vg integer null,
  hg integer null,
  avdelingnr integer null,
  levnr integer null,
  levnamn varchar (30) null,
  linjemerknad varchar (40) null,
  produsentbeskrivelse varchar (50) null,
  prodnr integer null,
  vgbeskr varchar (20) null,
  hgbeskr varchar (20) null,
  avdelingnavn varchar (30) null,
  levdato1 datetime null,
  levdato2 datetime null,
  levdato3 datetime null,
  levdato4 datetime null,
  innkjopspris decimal(7,2) null,
  rab1kr decimal(7,2) null,
  forhrab_ decimal(5,2) null,
  dbkr decimal(7,2) null,
  db_ decimal(7,2) null,
  pris decimal(7,2) null,
  varekost decimal(9,2) null,
  mva_ decimal(5,2) null,
  supinnkjopspris decimal(7,2) null,
  suprab1kr decimal(7,2) null,
  suprab_ decimal(5,2) null,
  supdbkr decimal(7,2) null,
  supdb_ decimal(7,2) null,
  suppris decimal(7,2) null,
  supvarekost decimal(9,2) null,
  supantall decimal(8,3) null,
  antall decimal(8,3) null,
  forhkalkyle decimal(3,2) null,
  supkalkyle decimal(3,2) null,
  vpidato datetime null,
  kjederab_ decimal(4,2) null,
  kjedeinnkpris decimal(7,2) null,
  anbefaltpris decimal(8,2) null,
  katalogpris decimal(8,2) null,
  kampanjepris decimal(8,2) null,
  sekv integer null,
  kjedevare tinyint null,
  gjennomfaktureres tinyint null,
  sortimentkoder varchar (40) null,
  kampanjeuker varchar (30) null,
  kampanjestotte varchar (30) null,
  lagerkoder varchar (30) null,
  sasong integer null,
  utvidetsok varchar (100) null,
  kjedevalutapris varchar (15) null,
  kjedeprodusent varchar (30) null,
  tilgjengeligfralev datetime null,
  levdatostopp1 datetime null,
  levdatostopp2 datetime null,
  levdatostopp3 datetime null,
  levdatostopp4 datetime null,
  kjedesuprab_ decimal(4,2) null,
  kjedesupinnkpris decimal(7,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vareboklinje ON vareboklinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vareboklinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vareboklinje#_#progress_recid ON vareboklinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vareboklinje#_#progress_recid_ident_ ON vareboklinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vareboklinje##anbefaltpris ON vareboklinje (anbefaltpris, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##antall ON vareboklinje (antall, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##artikkelnr ON vareboklinje (artikkelnr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##avdelingnavn ON vareboklinje (avdelingnavn, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##avdelingsnr ON vareboklinje (avdelingnr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##beskr ON vareboklinje (beskr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##db_ ON vareboklinje (db_, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##dbkr ON vareboklinje (dbkr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##edato ON vareboklinje (edato, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##forhkalkyle ON vareboklinje (forhkalkyle, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##forhrab_ ON vareboklinje (forhrab_, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##hg ON vareboklinje (hg, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##hgbeskr ON vareboklinje (hgbeskr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##innkjopspris ON vareboklinje (innkjopspris, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##jg ON vareboklinje (vg, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##kampanjepris ON vareboklinje (kampanjepris, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##katalogpris ON vareboklinje (katalogpris, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##kjedeinnpris ON vareboklinje (kjedeinnkpris, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##kjederab_ ON vareboklinje (kjederab_, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##levfargkod ON vareboklinje (levfargkod, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##levkod ON vareboklinje (levkod, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##levnavn ON vareboklinje (levnamn, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##levnr ON vareboklinje (levnr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##modellfarge ON vareboklinje (modellfarge, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##mva_ ON vareboklinje (mva_, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##pris ON vareboklinje (pris, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##prodnr ON vareboklinje (prodnr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##produsentbeskrivelse ON vareboklinje (produsentbeskrivelse, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##rab1kr ON vareboklinje (rab1kr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##registrertdato ON vareboklinje (registrertdato, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##sesong ON vareboklinje (sasong, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##supantall ON vareboklinje (supantall, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##supdb_ ON vareboklinje (supdb_, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##supdbkr ON vareboklinje (supdbkr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##supinnkjopspris ON vareboklinje (supinnkjopspris, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##supkalkyle ON vareboklinje (supkalkyle, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##suppris ON vareboklinje (suppris, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##suprab_ ON vareboklinje (suprab_, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##suprab1kr ON vareboklinje (suprab1kr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##suprabi1kr ON vareboklinje (suprab1kr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##supvarekost ON vareboklinje (supvarekost, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##varebokartnrlevnamn ON vareboklinje (vareboknr, artikkelnr, levnamn, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##vareboklevnr ON vareboklinje (vareboknr, levnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vareboklinje##vareboklinje ON vareboklinje (vareboknr, artikkelnr)
go
CREATE INDEX vareboklinje##varekost ON vareboklinje (varekost, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##vgbeskr ON vareboklinje (vgbeskr, PROGRESS_RECID)
go
CREATE INDEX vareboklinje##vpidato ON vareboklinje (vpidato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'vareboktemahode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vareboktemahode
go
CREATE TABLE vareboktemahode (
  varebehnr decimal(15,2) null,
  vbtemenr integer null,
  vbbeskrivelse varchar (40) null,
  vbnotat varchar (40) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  levnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vareboktemahode ON vareboktemahode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vareboktemahode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vareboktemahode#_#progress_recid ON vareboktemahode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vareboktemahode#_#progress_recid_ident_ ON vareboktemahode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vareboktemahode##beskrivelse ON vareboktemahode (varebehnr, levnr, vbbeskrivelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vareboktemahode##vareboktema ON vareboktemahode (varebehnr, levnr, vbtemenr)
go
if (select name from sysobjects 
    where name = 'vareboktemalinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vareboktemalinje
go
CREATE TABLE vareboktemalinje (
  varebehnr decimal(15,2) null,
  vbtemanr integer null,
  levnr integer null,
  seqnr integer null,
  artikkelnr decimal(13,0) not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vareboktemalinje ON vareboktemalinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vareboktemalinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vareboktemalinje#_#progress_recid ON vareboktemalinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vareboktemalinje#_#progress_recid_ident_ ON vareboktemalinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vareboktemalinje##seqnr ON vareboktemalinje (seqnr, PROGRESS_RECID)
go
CREATE INDEX vareboktemalinje##vbtemalinje ON vareboktemalinje (varebehnr, levnr, vbtemanr, artikkelnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'vareboktype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vareboktype
go
CREATE TABLE vareboktype (
  vareboktype integer null,
  beskrivelsevareboktype varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vareboktype ON vareboktype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vareboktype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vareboktype#_#progress_recid ON vareboktype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vareboktype#_#progress_recid_ident_ ON vareboktype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vareboktype##beskrivelsevareboktype ON vareboktype (beskrivelsevareboktype, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vareboktype##vareboktype ON vareboktype (vareboktype)
go
if (select name from sysobjects 
    where name = 'varemerke' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varemerke
go
CREATE TABLE varemerke (
  vmid integer null,
  beskrivelse varchar (30) null,
  merknad varchar (40) null,
  registrertdato datetime null,
  registrerttid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertav varchar (10) null,
  kortnavn varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varemerke ON varemerke for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varemerke t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varemerke#_#progress_recid ON varemerke (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varemerke#_#progress_recid_ident_ ON varemerke (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX varemerke##beskrivelse ON varemerke (beskrivelse, PROGRESS_RECID)
go
CREATE INDEX varemerke##kortnavn ON varemerke (kortnavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX varemerke##vmid ON varemerke (vmid)
go
if (select name from sysobjects 
    where name = 'varetrans' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varetrans
go
CREATE TABLE varetrans (
  butikknr integer not null,
  ean decimal(13,0) not null,
  dato datetime not null,
  tid integer not null,
  tellenr integer not null,
  transaksjonstype integer not null,
  transaksjonstekst varchar (30) null,
  brukerid varchar (12) null,
  antall decimal(8,3) not null,
  kostpris decimal(8,2) not null,
  mvakr decimal(8,2) not null,
  salgssum decimal(9,2) not null,
  nylagant decimal(9,3) not null,
  gmlagant decimal(9,3) not null,
  orsaknr integer not null,
  behandlingskode integer not null,
  tilfrabutikknr integer not null,
  aktivdato datetime not null,
  levnr integer not null,
  ordrenr integer not null,
  radnr integer not null,
  pakkseddelnr integer not null,
  pradnr integer not null,
  c1 varchar (30) null,
  c2 varchar (30) null,
  i1 integer not null,
  i2 integer not null,
  de1 decimal(7,2) not null,
  de2 decimal(7,2) not null,
  da1 datetime null,
  da2 datetime null,
  l1 tinyint not null,
  l2 tinyint not null,
  kode varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varetrans ON varetrans for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varetrans t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varetrans#_#progress_recid ON varetrans (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varetrans#_#progress_recid_ident_ ON varetrans (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX varetrans##datotid ON varetrans (dato, tid, PROGRESS_RECID)
go
CREATE INDEX varetrans##kode ON varetrans (kode, PROGRESS_RECID)
go
CREATE INDEX varetrans##levnr ON varetrans (levnr, PROGRESS_RECID)
go
CREATE INDEX varetrans##ordre ON varetrans (ordrenr, radnr, PROGRESS_RECID)
go
CREATE INDEX varetrans##pakkseddel ON varetrans (pakkseddelnr, pradnr, PROGRESS_RECID)
go
CREATE INDEX varetrans##tellenr ON varetrans (tellenr, PROGRESS_RECID)
go
CREATE INDEX varetrans##transaksjonstype ON varetrans (transaksjonstype, PROGRESS_RECID)
go
CREATE UNIQUE INDEX varetrans##varetransaksjon ON varetrans (butikknr, kode, dato, tid, transaksjonstype)
go
if (select name from sysobjects 
    where name = 'vargr' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vargr
go
CREATE TABLE vargr (
  vg integer null,
  vgbeskr varchar (30) null,
  stoart integer null,
  momskod integer null,
  hg integer null,
  kost_proc decimal(4,1) null,
  kolonne integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  tillatlokalepriser tinyint null,
  bonus_givende tinyint null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vargr ON vargr for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vargr t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vargr#_#progress_recid ON vargr (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vargr#_#progress_recid_ident_ ON vargr (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vargr##bonus_givende ON vargr (bonus_givende, PROGRESS_RECID)
go
CREATE INDEX vargr##hg ON vargr (hg, PROGRESS_RECID)
go
CREATE INDEX vargr##lokalepriser ON vargr (tillatlokalepriser, PROGRESS_RECID)
go
CREATE INDEX vargr##momskod ON vargr (momskod, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vargr##vargrin ON vargr (vg)
go
CREATE INDEX vargr##vgbeskr ON vargr (vgbeskr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'vgakt' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vgakt
go
CREATE TABLE vgakt (
  vg integer null,
  aktnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vgakt ON vgakt for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vgakt t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vgakt#_#progress_recid ON vgakt (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vgakt#_#progress_recid_ident_ ON vgakt (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vgakt##aktnr ON vgakt (aktnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vgakt##vgakt ON vgakt (vg, aktnr)
go
if (select name from sysobjects 
    where name = 'vgkat' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vgkat
go
CREATE TABLE vgkat (
  vg integer null,
  vgkat integer null,
  katnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vgkat ON vgkat for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vgkat t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vgkat#_#progress_recid ON vgkat (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vgkat#_#progress_recid_ident_ ON vgkat (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vgkat##katvg ON vgkat (katnr, vg, vgkat, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vgkat##vgkat ON vgkat (vg, vgkat, katnr)
go
if (select name from sysobjects 
    where name = 'vgkundegrprabatt' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vgkundegrprabatt
go
CREATE TABLE vgkundegrprabatt (
  vg integer null,
  gruppeid integer null,
  rabatt_ decimal(4,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vgkundegrprabatt ON vgkundegrprabatt for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vgkundegrprabatt t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vgkundegrprabatt#_#progress_recid ON vgkundegrprabatt (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vgkundegrprabatt#_#progress_recid_ident_ ON vgkundegrprabatt (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vgkundegrprabatt##gruppe ON vgkundegrprabatt (gruppeid, PROGRESS_RECID)
go
CREATE INDEX vgkundegrprabatt##rabatt_ ON vgkundegrprabatt (rabatt_, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vgkundegrprabatt##vgkundegrprabatt ON vgkundegrprabatt (vg, gruppeid)
go
if (select name from sysobjects 
    where name = 'vpifiltype' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpifiltype
go
CREATE TABLE vpifiltype (
  vpifiltypenr integer null,
  vpifiltypebeskrivelse varchar (30) null,
  vpifiltypeknavn varchar (12) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpifiltype ON vpifiltype for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpifiltype t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpifiltype#_#progress_recid ON vpifiltype (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpifiltype#_#progress_recid_ident_ ON vpifiltype (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vpifiltype##vpifiltypebeskrivelse ON vpifiltype (vpifiltypebeskrivelse, PROGRESS_RECID)
go
CREATE INDEX vpifiltype##vpifiltypeknavn ON vpifiltype (vpifiltypeknavn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpifiltype##vpifiltypenr ON vpifiltype (vpifiltypenr)
go
if (select name from sysobjects 
    where name = 'vpimottak' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpimottak
go
CREATE TABLE vpimottak (
  profilnr integer null,
  artikkelnr decimal(13,0) null,
  innkjopspris decimal(7,2) null,
  rab1_ decimal(5,2) null,
  db_ decimal(7,2) null,
  pris decimal(7,2) null,
  aktiveresdato datetime null,
  gyldigtildato datetime null,
  aktiverestid integer null,
  gyldigtiltid integer null,
  varekost decimal(9,2) null,
  mva_ decimal(5,2) null,
  momskod integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  behstatus integer null,
  etikettskrevet tinyint null,
  orgpris decimal(7,2) null,
  orgdb_ decimal(7,2) null,
  vpitype integer null,
  vpimottakid decimal(15,2) null,
  vg integer null,
  sasong integer null,
  beskr varchar (20) null,
  levnr integer null,
  levfargkod varchar (15) null,
  levkod varchar (20) null,
  orgaktiveresdato datetime null,
  orggyldigtildato datetime null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpimottak ON vpimottak for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpimottak t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpimottak#_#progress_recid ON vpimottak (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpimottak#_#progress_recid_ident_ ON vpimottak (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vpimottak##aktiveres ON vpimottak (aktiveresdato, PROGRESS_RECID)
go
CREATE INDEX vpimottak##beskr ON vpimottak (beskr, PROGRESS_RECID)
go
CREATE INDEX vpimottak##gyldigtil ON vpimottak (gyldigtildato, PROGRESS_RECID)
go
CREATE INDEX vpimottak##levfargkod ON vpimottak (levfargkod, PROGRESS_RECID)
go
CREATE INDEX vpimottak##levkod ON vpimottak (levkod, PROGRESS_RECID)
go
CREATE INDEX vpimottak##sasong ON vpimottak (sasong, PROGRESS_RECID)
go
CREATE INDEX vpimottak##vg ON vpimottak (vg, PROGRESS_RECID)
go
CREATE INDEX vpimottak##vpibehandling ON vpimottak (profilnr, behstatus, aktiveresdato, aktiverestid, vpitype, artikkelnr, PROGRESS_RECID)
go
CREATE INDEX vpimottak##vpimottak ON vpimottak (profilnr, artikkelnr, vpitype, aktiveresdato, aktiverestid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpimottak##vpimottakid ON vpimottak (vpimottakid)
go
CREATE INDEX vpimottak##vpitype ON vpimottak (vpitype, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'z_nummer' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table z_nummer
go
CREATE TABLE z_nummer (
  butikk integer null,
  kasse integer null,
  z_nummer integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_z_nummer ON z_nummer for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from z_nummer t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX z_nummer#_#progress_recid ON z_nummer (PROGRESS_RECID)
go
CREATE UNIQUE INDEX z_nummer#_#progress_recid_ident_ ON z_nummer (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX z_nummer##z_nummer ON z_nummer (butikk, kasse)
go
if (select name from sysobjects where name = '_SEQT_artikkelnr' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_artikkelnr
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'artikkelnr') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'artikkelnr'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_artikkelnr' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_artikkelnr
go
if (select name from sysobjects where name = '_SEQP_artikkelnr' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_artikkelnr
if (select name from sysobjects where name = '_SEQP_REV_artikkelnr' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_artikkelnr
go
create table _SEQT_artikkelnr ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_artikkelnr
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(1, 1, 9223372036854775807, 1, 0) 
 
go
create procedure _SEQP_artikkelnr (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_artikkelnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_artikkelnr set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_artikkelnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_artikkelnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_artikkelnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_artikkelnr) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_artikkelnr)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_artikkelnr set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_artikkelnr set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_batchlogg' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_batchlogg
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'batchlogg') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'batchlogg'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_batchlogg' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_batchlogg
go
if (select name from sysobjects where name = '_SEQP_batchlogg' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_batchlogg
if (select name from sysobjects where name = '_SEQP_REV_batchlogg' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_batchlogg
go
create table _SEQT_batchlogg ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_batchlogg
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 99999999, 0, 1) 
 
go
create procedure _SEQP_batchlogg (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_batchlogg)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_batchlogg set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_batchlogg)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_batchlogg)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_batchlogg)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_batchlogg) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_batchlogg)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_batchlogg set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_batchlogg set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_bestseq' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_bestseq
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'bestseq') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'bestseq'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_bestseq' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_bestseq
go
if (select name from sysobjects where name = '_SEQP_bestseq' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_bestseq
if (select name from sysobjects where name = '_SEQP_REV_bestseq' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_bestseq
go
create table _SEQT_bestseq ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_bestseq
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9999999, 0, 1) 
 
go
create procedure _SEQP_bestseq (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_bestseq)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_bestseq set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_bestseq)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_bestseq)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_bestseq)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_bestseq) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_bestseq)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_bestseq set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_bestseq set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_bongnr' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_bongnr
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'bongnr') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'bongnr'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_bongnr' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_bongnr
go
if (select name from sysobjects where name = '_SEQP_bongnr' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_bongnr
if (select name from sysobjects where name = '_SEQP_REV_bongnr' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_bongnr
go
create table _SEQT_bongnr ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_bongnr
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 99999999, 0, 1) 
 
go
create procedure _SEQP_bongnr (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_bongnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_bongnr set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_bongnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_bongnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_bongnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_bongnr) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_bongnr)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_bongnr set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_bongnr set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_jobbnr' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_jobbnr
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'jobbnr') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'jobbnr'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_jobbnr' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_jobbnr
go
if (select name from sysobjects where name = '_SEQP_jobbnr' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_jobbnr
if (select name from sysobjects where name = '_SEQP_REV_jobbnr' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_jobbnr
go
create table _SEQT_jobbnr ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_jobbnr
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(1, 1, 9999999, 1, 1) 
 
go
create procedure _SEQP_jobbnr (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_jobbnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_jobbnr set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_jobbnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_jobbnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_jobbnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_jobbnr) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_jobbnr)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_jobbnr set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_jobbnr set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_leveringsnr' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_leveringsnr
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'leveringsnr') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'leveringsnr'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_leveringsnr' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_leveringsnr
go
if (select name from sysobjects where name = '_SEQP_leveringsnr' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_leveringsnr
if (select name from sysobjects where name = '_SEQP_REV_leveringsnr' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_leveringsnr
go
create table _SEQT_leveringsnr ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_leveringsnr
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9999999, 0, 1) 
 
go
create procedure _SEQP_leveringsnr (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_leveringsnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_leveringsnr set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_leveringsnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_leveringsnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_leveringsnr)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_leveringsnr) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_leveringsnr)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_leveringsnr set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_leveringsnr set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_next_ientityid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_next_ientityid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'next_ientityid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'next_ientityid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_next_ientityid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_next_ientityid
go
if (select name from sysobjects where name = '_SEQP_next_ientityid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_next_ientityid
if (select name from sysobjects where name = '_SEQP_REV_next_ientityid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_next_ientityid
go
create table _SEQT_next_ientityid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_next_ientityid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_next_ientityid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_next_ientityid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_next_ientityid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_next_ientityid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_next_ientityid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_next_ientityid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_next_ientityid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_next_ientityid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_next_ientityid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_next_ientityid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_next_ilogid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_next_ilogid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'next_ilogid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'next_ilogid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_next_ilogid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_next_ilogid
go
if (select name from sysobjects where name = '_SEQP_next_ilogid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_next_ilogid
if (select name from sysobjects where name = '_SEQP_REV_next_ilogid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_next_ilogid
go
create table _SEQT_next_ilogid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_next_ilogid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_next_ilogid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_next_ilogid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_next_ilogid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_next_ilogid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_next_ilogid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_next_ilogid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_next_ilogid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_next_ilogid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_next_ilogid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_next_ilogid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_ovbuffer' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_ovbuffer
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'ovbuffer') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'ovbuffer'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_ovbuffer' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_ovbuffer
go
if (select name from sysobjects where name = '_SEQP_ovbuffer' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_ovbuffer
if (select name from sysobjects where name = '_SEQP_REV_ovbuffer' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_ovbuffer
go
create table _SEQT_ovbuffer ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_ovbuffer
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9999999, 0, 1) 
 
go
create procedure _SEQP_ovbuffer (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_ovbuffer)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_ovbuffer set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_ovbuffer)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_ovbuffer)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_ovbuffer)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_ovbuffer) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_ovbuffer)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_ovbuffer set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_ovbuffer set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_overforingsordre' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_overforingsordre
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'overforingsordre') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'overforingsordre'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_overforingsordre' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_overforingsordre
go
if (select name from sysobjects where name = '_SEQP_overforingsordre' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_overforingsordre
if (select name from sysobjects where name = '_SEQP_REV_overforingsordre' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_overforingsordre
go
create table _SEQT_overforingsordre ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_overforingsordre
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(1, 1, 99999999, 1, 1) 
 
go
create procedure _SEQP_overforingsordre (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_overforingsordre)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_overforingsordre set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_overforingsordre)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_overforingsordre)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_overforingsordre)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_overforingsordre) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_overforingsordre)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_overforingsordre set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_overforingsordre set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_pakkenr' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_pakkenr
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'pakkenr') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'pakkenr'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_pakkenr' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_pakkenr
go
if (select name from sysobjects where name = '_SEQP_pakkenr' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_pakkenr
if (select name from sysobjects where name = '_SEQP_REV_pakkenr' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_pakkenr
go
create table _SEQT_pakkenr ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_pakkenr
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9999, 0, 1) 
 
go
create procedure _SEQP_pakkenr (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_pakkenr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_pakkenr set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_pakkenr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_pakkenr)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_pakkenr)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_pakkenr) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_pakkenr)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_pakkenr set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_pakkenr set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxappmoduleid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxappmoduleid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxappmoduleid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxappmoduleid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxappmoduleid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxappmoduleid
go
if (select name from sysobjects where name = '_SEQP_seqjboxappmoduleid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxappmoduleid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxappmoduleid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxappmoduleid
go
create table _SEQT_seqjboxappmoduleid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxappmoduleid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxappmoduleid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxappmoduleid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxappmoduleid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxappmoduleid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxappmoduleid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxappmoduleid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxappmoduleid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxappmoduleid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxappmoduleid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxappmoduleid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxappmoduleitemid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxappmoduleitemid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxappmoduleitemid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxappmoduleitemid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxappmoduleitemid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxappmoduleitemid
go
if (select name from sysobjects where name = '_SEQP_seqjboxappmoduleitemid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxappmoduleitemid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxappmoduleitemid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxappmoduleitemid
go
create table _SEQT_seqjboxappmoduleitemid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxappmoduleitemid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxappmoduleitemid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxappmoduleitemid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxappmoduleitemid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxappmoduleitemid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxappmoduleitemid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxappmoduleitemid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxappmoduleitemid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxappmoduleitemid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxappmoduleitemid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxappmoduleitemid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxappprogramid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxappprogramid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxappprogramid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxappprogramid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxappprogramid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxappprogramid
go
if (select name from sysobjects where name = '_SEQP_seqjboxappprogramid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxappprogramid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxappprogramid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxappprogramid
go
create table _SEQT_seqjboxappprogramid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxappprogramid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxappprogramid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxappprogramid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxappprogramid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxappprogramid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxappprogramid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxappprogramid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxappprogramid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxappprogramid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxappprogramid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxappprogramid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxcompanyid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxcompanyid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxcompanyid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxcompanyid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxcompanyid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxcompanyid
go
if (select name from sysobjects where name = '_SEQP_seqjboxcompanyid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxcompanyid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxcompanyid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxcompanyid
go
create table _SEQT_seqjboxcompanyid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxcompanyid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxcompanyid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxcompanyid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxcompanyid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxcompanyid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxcompanyid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxcompanyid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxcompanyid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxcompanyid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxcompanyid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxcompanyid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxcompanyuserid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxcompanyuserid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxcompanyuserid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxcompanyuserid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxcompanyuserid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxcompanyuserid
go
if (select name from sysobjects where name = '_SEQP_seqjboxcompanyuserid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxcompanyuserid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxcompanyuserid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxcompanyuserid
go
create table _SEQT_seqjboxcompanyuserid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxcompanyuserid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxcompanyuserid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxcompanyuserid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxcompanyuserid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxcompanyuserid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxcompanyuserid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxcompanyuserid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxcompanyuserid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxcompanyuserid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxcompanyuserid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxcompanyuserid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxfunctionaccessid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxfunctionaccessid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxfunctionaccessid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxfunctionaccessid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxfunctionaccessid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxfunctionaccessid
go
if (select name from sysobjects where name = '_SEQP_seqjboxfunctionaccessid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxfunctionaccessid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxfunctionaccessid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxfunctionaccessid
go
create table _SEQT_seqjboxfunctionaccessid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxfunctionaccessid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxfunctionaccessid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxfunctionaccessid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxfunctionaccessid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxfunctionaccessid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxfunctionaccessid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxfunctionaccessid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxfunctionaccessid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxfunctionaccessid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxfunctionaccessid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxfunctionaccessid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxfunctionid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxfunctionid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxfunctionid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxfunctionid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxfunctionid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxfunctionid
go
if (select name from sysobjects where name = '_SEQP_seqjboxfunctionid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxfunctionid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxfunctionid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxfunctionid
go
create table _SEQT_seqjboxfunctionid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxfunctionid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxfunctionid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxfunctionid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxfunctionid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxfunctionid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxfunctionid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxfunctionid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxfunctionid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxfunctionid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxfunctionid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxfunctionid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxgencodeid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxgencodeid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxgencodeid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxgencodeid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxgencodeid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxgencodeid
go
if (select name from sysobjects where name = '_SEQP_seqjboxgencodeid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxgencodeid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxgencodeid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxgencodeid
go
create table _SEQT_seqjboxgencodeid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxgencodeid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxgencodeid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxgencodeid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxgencodeid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxgencodeid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxgencodeid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxgencodeid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxgencodeid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxgencodeid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxgencodeid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxgencodeid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxgencodetypeid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxgencodetypeid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxgencodetypeid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxgencodetypeid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxgencodetypeid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxgencodetypeid
go
if (select name from sysobjects where name = '_SEQP_seqjboxgencodetypeid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxgencodetypeid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxgencodetypeid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxgencodetypeid
go
create table _SEQT_seqjboxgencodetypeid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxgencodetypeid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxgencodetypeid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxgencodetypeid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxgencodetypeid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxgencodetypeid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxgencodetypeid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxgencodetypeid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxgencodetypeid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxgencodetypeid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxgencodetypeid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxgencodetypeid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxmenufavoritesid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxmenufavoritesid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxmenufavoritesid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxmenufavoritesid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxmenufavoritesid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxmenufavoritesid
go
if (select name from sysobjects where name = '_SEQP_seqjboxmenufavoritesid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxmenufavoritesid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxmenufavoritesid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxmenufavoritesid
go
create table _SEQT_seqjboxmenufavoritesid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxmenufavoritesid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(1, 1, 9223372036854775807, 1, 0) 
 
go
create procedure _SEQP_seqjboxmenufavoritesid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxmenufavoritesid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxmenufavoritesid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxmenufavoritesid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxmenufavoritesid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxmenufavoritesid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxmenufavoritesid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxmenufavoritesid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxmenufavoritesid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxmenufavoritesid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxmenuid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxmenuid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxmenuid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxmenuid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxmenuid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxmenuid
go
if (select name from sysobjects where name = '_SEQP_seqjboxmenuid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxmenuid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxmenuid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxmenuid
go
create table _SEQT_seqjboxmenuid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxmenuid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(100, 1, 9223372036854775807, 100, 0) 
 
go
create procedure _SEQP_seqjboxmenuid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxmenuid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxmenuid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxmenuid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxmenuid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxmenuid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxmenuid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxmenuid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxmenuid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxmenuid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxtranslationid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxtranslationid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxtranslationid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxtranslationid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxtranslationid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxtranslationid
go
if (select name from sysobjects where name = '_SEQP_seqjboxtranslationid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxtranslationid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxtranslationid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxtranslationid
go
create table _SEQT_seqjboxtranslationid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxtranslationid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxtranslationid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxtranslationid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxtranslationid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxtranslationid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxtranslationid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxtranslationid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxtranslationid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxtranslationid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxtranslationid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxtranslationid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxusergroupid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxusergroupid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxusergroupid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxusergroupid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxusergroupid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxusergroupid
go
if (select name from sysobjects where name = '_SEQP_seqjboxusergroupid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxusergroupid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxusergroupid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxusergroupid
go
create table _SEQT_seqjboxusergroupid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxusergroupid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxusergroupid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxusergroupid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxusergroupid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxusergroupid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxusergroupid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxusergroupid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxusergroupid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxusergroupid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxusergroupid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxusergroupid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxusergroupmembersid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxusergroupmembersid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxusergroupmembersid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxusergroupmembersid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxusergroupmembersid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxusergroupmembersid
go
if (select name from sysobjects where name = '_SEQP_seqjboxusergroupmembersid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxusergroupmembersid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxusergroupmembersid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxusergroupmembersid
go
create table _SEQT_seqjboxusergroupmembersid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxusergroupmembersid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxusergroupmembersid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxusergroupmembersid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxusergroupmembersid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxusergroupmembersid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxusergroupmembersid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxusergroupmembersid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxusergroupmembersid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxusergroupmembersid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxusergroupmembersid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxusergroupmembersid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxusermenuid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxusermenuid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxusermenuid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxusermenuid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxusermenuid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxusermenuid
go
if (select name from sysobjects where name = '_SEQP_seqjboxusermenuid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxusermenuid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxusermenuid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxusermenuid
go
create table _SEQT_seqjboxusermenuid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxusermenuid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxusermenuid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxusermenuid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxusermenuid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxusermenuid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxusermenuid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxusermenuid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxusermenuid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxusermenuid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxusermenuid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxusermenuid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqjboxusersettingid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqjboxusersettingid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxusersettingid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqjboxusersettingid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqjboxusersettingid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqjboxusersettingid
go
if (select name from sysobjects where name = '_SEQP_seqjboxusersettingid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqjboxusersettingid
if (select name from sysobjects where name = '_SEQP_REV_seqjboxusersettingid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqjboxusersettingid
go
create table _SEQT_seqjboxusersettingid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqjboxusersettingid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqjboxusersettingid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqjboxusersettingid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqjboxusersettingid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqjboxusersettingid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqjboxusersettingid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqjboxusersettingid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqjboxusersettingid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqjboxusersettingid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqjboxusersettingid set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqjboxusersettingid set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqpart1' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqpart1
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqpart1') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqpart1'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqpart1' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqpart1
go
if (select name from sysobjects where name = '_SEQP_seqpart1' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqpart1
if (select name from sysobjects where name = '_SEQP_REV_seqpart1' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqpart1
go
create table _SEQT_seqpart1 ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqpart1
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 1) 
 
go
create procedure _SEQP_seqpart1 (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqpart1)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqpart1 set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqpart1)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqpart1)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqpart1)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqpart1) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqpart1)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqpart1 set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqpart1 set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_seqpart2' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_seqpart2
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'seqpart2') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'seqpart2'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_seqpart2' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_seqpart2
go
if (select name from sysobjects where name = '_SEQP_seqpart2' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_seqpart2
if (select name from sysobjects where name = '_SEQP_REV_seqpart2' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_seqpart2
go
create table _SEQT_seqpart2 ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_seqpart2
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(0, 1, 9223372036854775807, 0, 0) 
 
go
create procedure _SEQP_seqpart2 (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_seqpart2)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_seqpart2 set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_seqpart2)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_seqpart2)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_seqpart2)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_seqpart2) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_seqpart2)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_seqpart2 set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_seqpart2 set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_telling' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_telling
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'telling') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'telling'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_telling' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_telling
go
if (select name from sysobjects where name = '_SEQP_telling' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_telling
if (select name from sysobjects where name = '_SEQP_REV_telling' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_telling
go
create table _SEQT_telling ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_telling
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(1, 1, 99999999, 1, 1) 
 
go
create procedure _SEQP_telling (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_telling)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_telling set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_telling)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_telling)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_telling)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_telling) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_telling)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_telling set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_telling set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
if (select name from sysobjects where name = '_SEQT_vpifil' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_vpifil
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'vpifil') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'vpifil'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_vpifil' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_vpifil
go
if (select name from sysobjects where name = '_SEQP_vpifil' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_vpifil
if (select name from sysobjects where name = '_SEQP_REV_vpifil' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_vpifil
go
create table _SEQT_vpifil ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_vpifil
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(1, 1, 9999999, 1, 1) 
 
go
create procedure _SEQP_vpifil (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_vpifil)
        SET @err = @@error 
        if @err <> 0 goto Err 
        commit transaction 
        return 0
    end
    
    /*
     * Next-Value function 
     */
    else if @op = 1
    begin
        declare @cur_val  bigint
        declare @last_val bigint
        declare @inc_val  bigint
 
        begin transaction
 
        /* perform a 'no-op' update to ensure exclusive lock */
        update _SEQT_vpifil set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_vpifil)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_vpifil)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_vpifil)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_vpifil) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_vpifil)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_vpifil set current_value = @val
        SET @err = @@error 
        if @err <> 0 goto Err 
 
 
        commit transaction
        return 0
    end
    else 
    /*
     * Set Current-Value function 
     */
    if @op = 2 
    begin 
      begin transaction 
      update _SEQT_vpifil set current_value = @val
      SET @err = @@error 
      if @err <> 0 goto Err 
      commit transaction 
      return 0 
   end 
    else 
        return -2
   Err: 
       rollback 
       return @err 
end
 
go
exit
