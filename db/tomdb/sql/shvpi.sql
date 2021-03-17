if (select name from sysobjects 
    where name = 'vpialtlevbas' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpialtlevbas
go
CREATE TABLE vpialtlevbas (
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  ekstvpilevnr integer null,
  varenr varchar (20) null,
  levnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpialtlevbas ON vpialtlevbas for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpialtlevbas t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpialtlevbas#_#progress_recid ON vpialtlevbas (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpialtlevbas#_#progress_recid_ident_ ON vpialtlevbas (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX vpialtlevbas##vpialtlevbas ON vpialtlevbas (ekstvpilevnr, varenr, levnr)
go
if (select name from sysobjects 
    where name = 'vpiartbas' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpiartbas
go
CREATE TABLE vpiartbas (
  ekstvpilevnr integer null,
  varenr varchar (20) null,
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
  storrelser tinyint null,
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
  handkode integer null,
  anbefaltpris decimal(8,2) null,
  kunderabatt tinyint null,
  etikett integer null,
  salgsenhet varchar (4) null,
  oppdatert tinyint null,
  lokartikkelnr decimal(13,0) null,
  modellfarge decimal(15,2) not null,
  sentralbestilling tinyint null,
  prisgrpnr integer not null,
  hovedmodellfarge tinyint null,
  kjedevare tinyint null,
  forhrab_##1 decimal(5,2) null,
  forhrab_##2 decimal(5,2) null,
  supprab_##1 decimal(5,2) null,
  supprab_##2 decimal(5,2) null,
  katalogpris##1 decimal(9,2) null,
  katalogpris##2 decimal(9,2) null,
  linjemerknad varchar (40) null,
  levdato3 datetime null,
  levdato4 datetime null,
  vpidato datetime null,
  vpibildekode varchar (30) null,
  linkvarenr decimal(15,2) null,
  mengde decimal(6,3) null,
  manrabikas tinyint null,
  artslag integer null,
  individtype integer null,
  pant tinyint null,
  bestforslag tinyint null,
  garantikl integer null,
  strkode1 integer null,
  strkode2 integer null,
  levvaretekst varchar (30) null,
  antipkn integer null,
  gjennomfaktureres tinyint null,
  behstatus integer null,
  artstatus integer null,
  korrstatus integer null,
  korrartikkelnr decimal(13,0) null,
  utvidetsok varchar (100) null,
  lokasjon varchar (20) null,
  etikettekst1 varchar (30) null,
  etikettekst2 varchar (30) null,
  ravdnr integer null,
  kjedevalutapris varchar (15) null,
  kjedeprodusent varchar (30) null,
  sanertdato datetime null,
  anbrekk tinyint null,
  inkranbrekk integer null,
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
  ekststrtypenavn varchar (40) null,
  kjederab_ decimal(4,2) null,
  kjedesuprab_ decimal(4,2) null,
  kjedeinnkpris decimal(7,2) null,
  kjedesupinnkpris decimal(7,2) null,
  non_sale tinyint null,
  vekt tinyint null,
  sortimentkoder varchar (40) null,
  kampanjeuker varchar (30) null,
  kampanjestotte varchar (30) null,
  lagerkoder varchar (30) null,
  grunnsortiment tinyint null,
  bonus_givende tinyint null,
  link_til_nettside varchar (40) null,
  publiserinettbutikk tinyint null,
  hovedkatnr integer null,
  negvare tinyint null,
  telefonkort tinyint null,
  webbutikkartikkel tinyint null,
  hoylavmva tinyint null,
  salgsstopp integer null,
  linkvareant integer null,
  alfakode2 varchar (4) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpiartbas ON vpiartbas for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpiartbas t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpiartbas#_#progress_recid ON vpiartbas (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpiartbas#_#progress_recid_ident_ ON vpiartbas (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vpiartbas##artikkelnr ON vpiartbas (artikkelnr, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##artstatus ON vpiartbas (artstatus, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##behstatus ON vpiartbas (behstatus, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##beskr ON vpiartbas (beskr, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##bongtekst ON vpiartbas (bongtekst, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##gjfaktureres ON vpiartbas (gjennomfaktureres, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##grunnsortiment ON vpiartbas (grunnsortiment, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##korrartikkelnr ON vpiartbas (korrartikkelnr, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##korrstatus ON vpiartbas (korrstatus, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##levkod ON vpiartbas (levkod, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##levnr ON vpiartbas (levnr, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##lokartikkelnr ON vpiartbas (ekstvpilevnr, lokartikkelnr, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##modellfarge ON vpiartbas (ekstvpilevnr, modellfarge, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##non_sale ON vpiartbas (non_sale, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##oppdatert ON vpiartbas (oppdatert, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##salgstopp ON vpiartbas (salgsstopp, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##sasong ON vpiartbas (sasong, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##settartikkelnr ON vpiartbas (ekstvpilevnr, levkod, beskr, levfargkod, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##settartikkelnrfravpi ON vpiartbas (ekstvpilevnr, levkod, beskr, levfargkod, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##strtypeid ON vpiartbas (strtypeid, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##valkod ON vpiartbas (valkod, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##varenr ON vpiartbas (varenr, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vg ON vpiartbas (vg, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpiartbas##vpiartbas ON vpiartbas (ekstvpilevnr, varenr)
go
CREATE INDEX vpiartbas##vpiartikkelnr ON vpiartbas (ekstvpilevnr, artikkelnr, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vpibeskr ON vpiartbas (ekstvpilevnr, beskr, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vpibildekode ON vpiartbas (ekstvpilevnr, vpibildekode, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vpibongtekst ON vpiartbas (ekstvpilevnr, bongtekst, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vpilevkod ON vpiartbas (ekstvpilevnr, levkod, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vpilevnr ON vpiartbas (ekstvpilevnr, levnr, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vpioppdatert ON vpiartbas (ekstvpilevnr, oppdatert, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vpisasong ON vpiartbas (ekstvpilevnr, sasong, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vpistrtypeid ON vpiartbas (ekstvpilevnr, strtypeid, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vpivalkod ON vpiartbas (ekstvpilevnr, valkod, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vpivarenr ON vpiartbas (ekstvpilevnr, varenr, PROGRESS_RECID)
go
CREATE INDEX vpiartbas##vpivg ON vpiartbas (ekstvpilevnr, vg, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'vpiartbaskarakteristikk' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpiartbaskarakteristikk
go
CREATE TABLE vpiartbaskarakteristikk (
  ekstvpilevnr integer null,
  karakteristikkid varchar (4) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  varenr varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpiartbaskarakteristikk ON vpiartbaskarakteristikk for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpiartbaskarakteristikk t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpiartbaskarakteristikk#_#progress_recid ON vpiartbaskarakteristikk (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpiartbaskarakteristikk#_#progress_recid_ident_ ON vpiartbaskarakteristikk (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vpiartbaskarakteristikk##karakteristikkid ON vpiartbaskarakteristikk (karakteristikkid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpiartbaskarakteristikk##varenrkarakteristikk ON vpiartbaskarakteristikk (ekstvpilevnr, varenr, karakteristikkid)
go
if (select name from sysobjects 
    where name = 'vpiartbestpkt' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpiartbestpkt
go
CREATE TABLE vpiartbestpkt (
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
  varenr varchar (20) null,
  ekstvpilevnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpiartbestpkt ON vpiartbestpkt for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpiartbestpkt t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpiartbestpkt#_#progress_recid ON vpiartbestpkt (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpiartbestpkt#_#progress_recid_ident_ ON vpiartbestpkt (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX vpiartbestpkt##vpiartbestpkt ON vpiartbestpkt (ekstvpilevnr, varenr, butikknr, strkode)
go
if (select name from sysobjects 
    where name = 'vpiartpris' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpiartpris
go
CREATE TABLE vpiartpris (
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
  ekstvpilevnr integer null,
  varenr varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpiartpris ON vpiartpris for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpiartpris t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpiartpris#_#progress_recid ON vpiartpris (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpiartpris#_#progress_recid_ident_ ON vpiartpris (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vpiartpris##vpiartpris ON vpiartpris (ekstvpilevnr, varenr, profilnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'vpibildedata' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpibildedata
go
CREATE TABLE vpibildedata (
  bildnr integer null,
  teller integer null,
  rawdata text null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  ekstvpilevnr integer null,
  varenr varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpibildedata ON vpibildedata for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpibildedata t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpibildedata#_#progress_recid ON vpibildedata (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpibildedata#_#progress_recid_ident_ ON vpibildedata (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX vpibildedata##vpibildedata ON vpibildedata (ekstvpilevnr, varenr, bildnr, teller)
go
if (select name from sysobjects 
    where name = 'vpibilderegister' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpibilderegister
go
CREATE TABLE vpibilderegister (
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
  ekstvpilevnr integer null,
  varenr varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpibilderegister ON vpibilderegister for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpibilderegister t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpibilderegister#_#progress_recid ON vpibilderegister (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpibilderegister#_#progress_recid_ident_ ON vpibilderegister (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX vpibilderegister##vpibilderegister ON vpibilderegister (ekstvpilevnr, varenr, bildnr)
go
if (select name from sysobjects 
    where name = 'vpidatasett' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpidatasett
go
CREATE TABLE vpidatasett (
  ekstvpilevnr integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (10) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (10) null,
  datasettstatus integer null,
  beskrivelse varchar (30) null,
  importdato datetime null,
  importkl integer null,
  oppdatertdato datetime null,
  oppdaterttid integer null,
  filid decimal(15,2) null,
  antallartikler integer null,
  antallkoblet integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpidatasett ON vpidatasett for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpidatasett t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpidatasett#_#progress_recid ON vpidatasett (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpidatasett#_#progress_recid_ident_ ON vpidatasett (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX vpidatasett##vpidatasett ON vpidatasett (ekstvpilevnr)
go
if (select name from sysobjects 
    where name = 'vpierstattningsvare' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpierstattningsvare
go
CREATE TABLE vpierstattningsvare (
  erstattid integer null,
  artikkelnr decimal(13,0) null,
  ekstvpilevnr integer null,
  varenr varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpierstattningsvare ON vpierstattningsvare for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpierstattningsvare t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpierstattningsvare#_#progress_recid ON vpierstattningsvare (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpierstattningsvare#_#progress_recid_ident_ ON vpierstattningsvare (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX vpierstattningsvare##vpierstattningsvare ON vpierstattningsvare (ekstvpilevnr, varenr, artikkelnr)
go
if (select name from sysobjects 
    where name = 'vpipakkelinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpipakkelinje
go
CREATE TABLE vpipakkelinje (
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
  ekstvpilevnr integer null,
  varenr varchar (20) null,
  strkode integer null,
  varekost decimal(9,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpipakkelinje ON vpipakkelinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpipakkelinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpipakkelinje#_#progress_recid ON vpipakkelinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpipakkelinje#_#progress_recid_ident_ ON vpipakkelinje (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX vpipakkelinje##vpipakkelinje ON vpipakkelinje (ekstvpilevnr, varenr, pkartikkelnr, strkode)
go
if (select name from sysobjects 
    where name = 'vpistrekkode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpistrekkode
go
CREATE TABLE vpistrekkode (
  kode varchar (20) null,
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
  ekstvpilevnr integer null,
  varenr varchar (20) null,
  ekststorl varchar (15) null,
  storl varchar (4) null,
  bestillingsnummer varchar (25) null,
  erpnr varchar (30) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpistrekkode ON vpistrekkode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpistrekkode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpistrekkode#_#progress_recid ON vpistrekkode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpistrekkode#_#progress_recid_ident_ ON vpistrekkode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vpistrekkode##bestillingsnummer ON vpistrekkode (ekstvpilevnr, bestillingsnummer, PROGRESS_RECID)
go
CREATE INDEX vpistrekkode##erpnr ON vpistrekkode (ekstvpilevnr, erpnr, PROGRESS_RECID)
go
CREATE INDEX vpistrekkode##globalstrekkode ON vpistrekkode (kode, PROGRESS_RECID)
go
CREATE INDEX vpistrekkode##kode ON vpistrekkode (ekstvpilevnr, kode, PROGRESS_RECID)
go
CREATE INDEX vpistrekkode##vpistrekkode ON vpistrekkode (ekstvpilevnr, varenr, kode, PROGRESS_RECID)
go
exit
