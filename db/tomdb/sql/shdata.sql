if (select name from sysobjects 
    where name = 'analyse' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table analyse
go
CREATE TABLE analyse (
  analyseid integer null,
  knavn varchar (30) null,
  navn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  startdato datetime null,
  sluttdato datetime null,
  aktiv tinyint null,
  notat varchar (40) null,
  aktivertdato datetime null,
  analysetype integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_analyse ON analyse for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from analyse t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX analyse#_#progress_recid ON analyse (PROGRESS_RECID)
go
CREATE UNIQUE INDEX analyse#_#progress_recid_ident_ ON analyse (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX analyse##aktiv ON analyse (aktiv, PROGRESS_RECID)
go
CREATE INDEX analyse##aktivertdato ON analyse (aktivertdato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX analyse##analyse ON analyse (analyseid)
go
CREATE INDEX analyse##analysetype ON analyse (analysetype, PROGRESS_RECID)
go
CREATE INDEX analyse##endret ON analyse (edato, PROGRESS_RECID)
go
CREATE INDEX analyse##knavn ON analyse (knavn, PROGRESS_RECID)
go
CREATE INDEX analyse##navn ON analyse (navn, PROGRESS_RECID)
go
CREATE INDEX analyse##registrert ON analyse (registrertdato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'analyseartikkel' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table analyseartikkel
go
CREATE TABLE analyseartikkel (
  analyseid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  startdato datetime null,
  sluttdato datetime null,
  aktiv tinyint null,
  aktivertdato datetime null,
  artikkelnr decimal(15,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_analyseartikkel ON analyseartikkel for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from analyseartikkel t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX analyseartikkel#_#progress_recid ON analyseartikkel (PROGRESS_RECID)
go
CREATE UNIQUE INDEX analyseartikkel#_#progress_recid_ident_ ON analyseartikkel (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX analyseartikkel##aktiv ON analyseartikkel (analyseid, aktiv, PROGRESS_RECID)
go
CREATE UNIQUE INDEX analyseartikkel##analyseartikkel ON analyseartikkel (analyseid, artikkelnr)
go
CREATE INDEX analyseartikkel##artikkelaktivert ON analyseartikkel (analyseid, aktivertdato, PROGRESS_RECID)
go
CREATE INDEX analyseartikkel##artikkelendret ON analyseartikkel (analyseid, edato, PROGRESS_RECID)
go
CREATE INDEX analyseartikkel##artikkelregistrert ON analyseartikkel (analyseid, registrertdato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'analyselogg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table analyselogg
go
CREATE TABLE analyselogg (
  analyseid integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  artikkelnr decimal(15,2) null,
  bongnr integer not null,
  butikknr integer not null,
  gruppenr integer not null,
  linjenr integer not null,
  dato datetime null,
  b_id decimal(14,2) null,
  kassenr integer not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_analyselogg ON analyselogg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from analyselogg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX analyselogg#_#progress_recid ON analyselogg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX analyselogg#_#progress_recid_ident_ ON analyselogg (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX analyselogg##analyselogg ON analyselogg (analyseid, artikkelnr, b_id, linjenr)
go
CREATE INDEX analyselogg##bongutvalg ON analyselogg (analyseid, butikknr, gruppenr, kassenr, dato, bongnr, linjenr, PROGRESS_RECID)
go
CREATE INDEX analyselogg##dato ON analyselogg (analyseid, dato, butikknr, gruppenr, kassenr, bongnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'bokforingsdag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bokforingsdag
go
CREATE TABLE bokforingsdag (
  butikknr integer not null,
  gruppenr integer not null,
  dato datetime null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  pfflagg integer null,
  bokforingsid decimal(13,0) not null,
  datetime decimal(13,0) null,
  lopnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bokforingsdag ON bokforingsdag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bokforingsdag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bokforingsdag#_#progress_recid ON bokforingsdag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bokforingsdag#_#progress_recid_ident_ ON bokforingsdag (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX bokforingsdag##bfdagtid ON bokforingsdag (butikknr, dato, datetime)
go
CREATE UNIQUE INDEX bokforingsdag##bokforingsid ON bokforingsdag (bokforingsid)
go
CREATE INDEX bokforingsdag##pfflagg ON bokforingsdag (pfflagg, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'bonghode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bonghode
go
CREATE TABLE bonghode (
  bongnr integer not null,
  butikknr integer not null,
  gruppenr integer not null,
  odato datetime null,
  otid integer null,
  oav varchar (15) null,
  edato datetime null,
  etid integer null,
  eav varchar (15) null,
  kassenr integer not null,
  kasserernr decimal(15,2) not null,
  selgernr integer not null,
  kundenr decimal(15,2) null,
  medlemsnr decimal(15,2) null,
  bongstatus integer null,
  kasserernavn varchar (30) null,
  selgernavn varchar (30) null,
  medlemnavn varchar (30) null,
  overforingsnr decimal(10,2) null,
  medlemskort varchar (16) null,
  kundekort varchar (22) null,
  datasettid decimal(16,2) null,
  utskriftskopi varchar (60) null,
  opdkvit tinyint null,
  opdutskkopi tinyint null,
  konvertert tinyint null,
  dato datetime null,
  tid integer null,
  belop decimal(9,2) null,
  kundenavn varchar (30) null,
  logg varchar (60) null,
  korttype integer null,
  gradering integer null,
  b_id decimal(14,2) null,
  flbetalingskort tinyint null,
  flbankkort tinyint null,
  flkreditkort tinyint null,
  flgavekort tinyint null,
  flsjekk tinyint null,
  flrekvisisasjon tinyint null,
  flkupong1 tinyint null,
  flslkort integer null,
  flrabatt tinyint null,
  systemkort varchar (30) null,
  flsystemkort tinyint null,
  eksportertdato datetime null,
  pfflagg integer null,
  kampanje tinyint null,
  skiftnr integer null,
  makulert integer null,
  skiftid decimal(13,0) null,
  kordre_id decimal(15,2) null,
  ttid integer not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bonghode ON bonghode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bonghode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bonghode#_#progress_recid ON bonghode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bonghode#_#progress_recid_ident_ ON bonghode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX bonghode##belop ON bonghode (belop, PROGRESS_RECID)
go
CREATE UNIQUE INDEX bonghode##bong ON bonghode (butikknr, gruppenr, kassenr, dato, bongnr)
go
CREATE INDEX bonghode##bongstatus ON bonghode (bongstatus, PROGRESS_RECID)
go
CREATE INDEX bonghode##butdato ON bonghode (butikknr, dato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX bonghode##b_id ON bonghode (b_id)
go
CREATE INDEX bonghode##datasett ON bonghode (datasettid, butikknr, gruppenr, kassenr, dato, bongnr, PROGRESS_RECID)
go
CREATE INDEX bonghode##eksportert ON bonghode (eksportertdato, PROGRESS_RECID)
go
CREATE INDEX bonghode##fkampanje ON bonghode (kampanje, PROGRESS_RECID)
go
CREATE INDEX bonghode##flbankkort ON bonghode (flbankkort, PROGRESS_RECID)
go
CREATE INDEX bonghode##flbetalingskort ON bonghode (flbetalingskort, PROGRESS_RECID)
go
CREATE INDEX bonghode##flgavekort ON bonghode (flgavekort, PROGRESS_RECID)
go
CREATE INDEX bonghode##flkreditkort ON bonghode (flkreditkort, PROGRESS_RECID)
go
CREATE INDEX bonghode##flkupong1 ON bonghode (flkupong1, PROGRESS_RECID)
go
CREATE INDEX bonghode##flrabatt ON bonghode (flrabatt, PROGRESS_RECID)
go
CREATE INDEX bonghode##flrekvisisasjon ON bonghode (flrekvisisasjon, PROGRESS_RECID)
go
CREATE INDEX bonghode##flsjekk ON bonghode (flsjekk, PROGRESS_RECID)
go
CREATE INDEX bonghode##flsystemkort ON bonghode (flsystemkort, PROGRESS_RECID)
go
CREATE INDEX bonghode##kasserer ON bonghode (butikknr, gruppenr, kassenr, kasserernr, PROGRESS_RECID)
go
CREATE INDEX bonghode##kasserernr ON bonghode (kassenr, PROGRESS_RECID)
go
CREATE INDEX bonghode##kordreid ON bonghode (kordre_id, PROGRESS_RECID)
go
CREATE INDEX bonghode##korttype ON bonghode (korttype, PROGRESS_RECID)
go
CREATE INDEX bonghode##kundekort ON bonghode (kundekort, PROGRESS_RECID)
go
CREATE INDEX bonghode##kundenr ON bonghode (kundenr, PROGRESS_RECID)
go
CREATE INDEX bonghode##makulert ON bonghode (makulert, PROGRESS_RECID)
go
CREATE INDEX bonghode##medlemskort ON bonghode (medlemskort, PROGRESS_RECID)
go
CREATE INDEX bonghode##medlemsnr ON bonghode (medlemsnr, PROGRESS_RECID)
go
CREATE INDEX bonghode##overforingsnr ON bonghode (overforingsnr, PROGRESS_RECID)
go
CREATE INDEX bonghode##pfflagg ON bonghode (pfflagg, PROGRESS_RECID)
go
CREATE INDEX bonghode##selger ON bonghode (butikknr, gruppenr, kassenr, selgernr, PROGRESS_RECID)
go
CREATE INDEX bonghode##selgernr ON bonghode (selgernr, PROGRESS_RECID)
go
CREATE INDEX bonghode##skiftid ON bonghode (skiftid, PROGRESS_RECID)
go
CREATE INDEX bonghode##slkort ON bonghode (flslkort, PROGRESS_RECID)
go
CREATE INDEX bonghode##systemkort ON bonghode (systemkort, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'bonglinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table bonglinje
go
CREATE TABLE bonglinje (
  bongnr integer not null,
  butikknr integer not null,
  gruppenr integer not null,
  odato datetime null,
  otid integer null,
  oav varchar (15) null,
  edato datetime null,
  etid integer null,
  eav varchar (15) null,
  kassenr integer not null,
  linjenr integer not null,
  ttid integer not null,
  artikkelnr varchar (20) not null,
  storrelse varchar (4) null,
  antall decimal(9,3) null,
  tbid integer null,
  varegr integer not null,
  varegruppenavn varchar (30) null,
  bongtekst varchar (30) null,
  linjerab decimal(9,2) null,
  subtotalrab decimal(9,2) null,
  transdato datetime null,
  transtid integer null,
  mvagr integer not null,
  mvagruppenavn varchar (30) null,
  mva_ decimal(7,2) null,
  bongpris decimal(10,2) null,
  linjesum decimal(12,2) null,
  type integer null,
  mbutikknr integer null,
  mvakr decimal(10,2) null,
  originaldata varchar (60) null,
  dato datetime null,
  lopenr integer null,
  notatkode integer null,
  feilkode integer null,
  notatkodetekst varchar (30) null,
  feilkodetekst varchar (30) null,
  makulert tinyint null,
  returbutikk integer null,
  returkasserer decimal(15,2) not null,
  returkasserernavn varchar (30) null,
  hovedgr integer null,
  hovedgrbeskrivelse varchar (30) null,
  transnr integer null,
  seqnr integer null,
  vvarekost decimal(9,2) null,
  reftekst varchar (40) null,
  refnr integer null,
  strekkode varchar (20) null,
  b_id decimal(14,2) null,
  aaaammdd varchar (30) null,
  kunderabatt decimal(9,2) null,
  medlemsrabatt decimal(9,2) null,
  personalrabatt decimal(9,2) null,
  generellrabatt decimal(9,2) null,
  levnr integer null,
  levnavn varchar (30) null,
  forkonvertering varchar (40) null,
  prisprsalgsenhet decimal(12,2) null,
  kampanjeid integer null,
  salgstype tinyint null,
  produkttype integer null,
  skiftnr integer null,
  orgvaregr integer not null,
  kampid decimal(10,2) null,
  kampeierid integer null,
  kamptilbid integer null,
  tilbudsrabatt decimal(9,2) null,
  mixmatchrabatt decimal(9,2) null,
  alternativprisrabatt decimal(9,2) null,
  manuelendretprisrabatt decimal(9,2) null,
  subtotalrabattpersonal decimal(9,2) null,
  linjerabattpersonal decimal(9,2) null,
  individnr decimal(15,2) null,
  divinfo varchar (20) null,
  normalpris decimal(12,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_bonglinje ON bonglinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from bonglinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX bonglinje#_#progress_recid ON bonglinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX bonglinje#_#progress_recid_ident_ ON bonglinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX bonglinje##artikkelnr ON bonglinje (artikkelnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX bonglinje##bonglinje ON bonglinje (butikknr, gruppenr, kassenr, dato, bongnr, linjenr)
go
CREATE UNIQUE INDEX bonglinje##b_id ON bonglinje (b_id, linjenr)
go
CREATE INDEX bonglinje##fsalgstype ON bonglinje (salgstype, PROGRESS_RECID)
go
CREATE INDEX bonglinje##kampanjeid ON bonglinje (kampanjeid, artikkelnr, butikknr, PROGRESS_RECID)
go
CREATE INDEX bonglinje##produkttype ON bonglinje (produkttype, PROGRESS_RECID)
go
CREATE INDEX bonglinje##skiftnr ON bonglinje (butikknr, skiftnr, PROGRESS_RECID)
go
CREATE INDEX bonglinje##strekkode ON bonglinje (strekkode, PROGRESS_RECID)
go
CREATE INDEX bonglinje##ttid ON bonglinje (ttid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'datasett' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table datasett
go
CREATE TABLE datasett (
  butikknr integer not null,
  gruppenr integer not null,
  dato datetime null,
  tid integer null,
  settnr integer null,
  settstatus integer null,
  kassenr integer null,
  filid decimal(15,2) null,
  datasettid decimal(16,2) null,
  filtype integer not null,
  behandlet integer null,
  antalllinjer integer null,
  pfflagg integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_datasett ON datasett for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from datasett t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX datasett#_#progress_recid ON datasett (PROGRESS_RECID)
go
CREATE UNIQUE INDEX datasett#_#progress_recid_ident_ ON datasett (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX datasett##apningsskjema ON datasett (butikknr, dato, PROGRESS_RECID)
go
CREATE INDEX datasett##behandlet ON datasett (behandlet, PROGRESS_RECID)
go
CREATE INDEX datasett##datasett ON datasett (butikknr, gruppenr, kassenr, dato, settnr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX datasett##datasettid ON datasett (datasettid)
go
CREATE INDEX datasett##filid ON datasett (filid, PROGRESS_RECID)
go
CREATE INDEX datasett##pfflagg ON datasett (pfflagg, butikknr, gruppenr, kassenr, dato, settnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'filer' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table filer
go
CREATE TABLE filer (
  filnavn varchar (30) null,
  dato datetime null,
  kl varchar (30) null,
  storrelse integer null,
  katalog varchar (40) null,
  innlest tinyint null,
  feil tinyint null,
  dobbel tinyint null,
  oppdatert tinyint null,
  innlestdato datetime null,
  innlestkl integer null,
  innlestav varchar (15) null,
  oppdatertdato datetime null,
  oppdatertkl integer null,
  oppdatertav varchar (15) null,
  filid decimal(15,2) null,
  backup tinyint null,
  antlinjer integer null,
  slettetdato datetime null,
  slettetav varchar (15) null,
  slettettid integer null,
  slettet tinyint null,
  filtype integer null,
  overfort tinyint null,
  overfortdato datetime null,
  overforttid integer null,
  overfortav varchar (15) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_filer ON filer for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from filer t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX filer#_#progress_recid ON filer (PROGRESS_RECID)
go
CREATE UNIQUE INDEX filer#_#progress_recid_ident_ ON filer (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX filer##backup ON filer (backup, PROGRESS_RECID)
go
CREATE INDEX filer##dobbel ON filer (dobbel, PROGRESS_RECID)
go
CREATE INDEX filer##feil ON filer (feil, PROGRESS_RECID)
go
CREATE UNIQUE INDEX filer##filer ON filer (filnavn, dato, kl, storrelse, katalog)
go
CREATE UNIQUE INDEX filer##filid ON filer (filid)
go
CREATE INDEX filer##filnavn ON filer (filnavn, PROGRESS_RECID)
go
CREATE INDEX filer##filtype ON filer (filtype, PROGRESS_RECID)
go
CREATE INDEX filer##innlest ON filer (innlestdato, innlestkl, PROGRESS_RECID)
go
CREATE INDEX filer##innlestav ON filer (innlestav, innlestdato, innlestkl, PROGRESS_RECID)
go
CREATE INDEX filer##innlestflagg ON filer (innlest, PROGRESS_RECID)
go
CREATE INDEX filer##oppdatert ON filer (oppdatertdato, oppdatertkl, PROGRESS_RECID)
go
CREATE INDEX filer##oppdatertav ON filer (oppdatertav, oppdatertdato, oppdatertkl, PROGRESS_RECID)
go
CREATE INDEX filer##opprettet ON filer (dato, kl, PROGRESS_RECID)
go
CREATE INDEX filer##overfort ON filer (overfort, PROGRESS_RECID)
go
CREATE INDEX filer##slettet ON filer (slettet, PROGRESS_RECID)
go
CREATE INDEX filer##slettetdatotid ON filer (slettetdato, slettettid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'fillinjer' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table fillinjer
go
CREATE TABLE fillinjer (
  filid decimal(15,2) null,
  tekst varchar (60) null,
  datasett tinyint null,
  linjenr integer null,
  behandlet tinyint null,
  datasettid decimal(16,2) null,
  stortekst varchar (80) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_fillinjer ON fillinjer for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from fillinjer t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX fillinjer#_#progress_recid ON fillinjer (PROGRESS_RECID)
go
CREATE UNIQUE INDEX fillinjer#_#progress_recid_ident_ ON fillinjer (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX fillinjer##behandlet ON fillinjer (filid, behandlet, PROGRESS_RECID)
go
CREATE INDEX fillinjer##datasett ON fillinjer (datasettid, linjenr, PROGRESS_RECID)
go
CREATE INDEX fillinjer##datasettid ON fillinjer (filid, datasettid, linjenr, PROGRESS_RECID)
go
CREATE INDEX fillinjer##fillinjer ON fillinjer (filid, linjenr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'fillogg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table fillogg
go
CREATE TABLE fillogg (
  filid decimal(15,2) null,
  linjenr integer null,
  tekst varchar (50) null,
  gradering integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_fillogg ON fillogg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from fillogg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX fillogg#_#progress_recid ON fillogg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX fillogg#_#progress_recid_ident_ ON fillogg (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX fillogg##fillogg ON fillogg (filid, linjenr)
go
CREATE INDEX fillogg##gradering ON fillogg (gradering, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'finansdag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table finansdag
go
CREATE TABLE finansdag (
  butnr integer not null,
  dato datetime not null,
  znr integer not null,
  btovarerkr decimal(11,2) not null,
  btovareant integer not null,
  btovareknd integer not null,
  avslagkr decimal(11,2) not null,
  avslagant integer not null,
  kontorabkr decimal(11,2) not null,
  kontorabant integer not null,
  pantkr decimal(11,2) not null,
  pantant integer not null,
  pros_1kr decimal(11,2) not null,
  pros_1ant integer not null,
  pros_2kr decimal(11,2) not null,
  pros_2ant integer not null,
  pros1kr decimal(11,2) not null,
  pros1ant integer not null,
  pros2kr decimal(11,2) not null,
  pros2ant integer not null,
  kontokr decimal(11,2) not null,
  kontoinnkr decimal(11,2) not null,
  kontoinnant integer not null,
  kontantkr decimal(11,2) not null,
  kontantant integer not null,
  kreditkr decimal(11,2) not null,
  kreditant integer not null,
  media3kr decimal(11,2) not null,
  media3ant integer not null,
  media4kr decimal(11,2) not null,
  media4ant integer not null,
  bankkr decimal(11,2) not null,
  bankant integer not null,
  sjekkr decimal(9,2) not null,
  sjekkant integer not null,
  innbetkr decimal(11,2) not null,
  innbetant integer not null,
  utbetkr decimal(11,2) not null,
  utbetant integer not null,
  vekslekr decimal(11,2) not null,
  veksleant integer not null,
  levkr decimal(11,2) not null,
  levant integer not null,
  negativkr decimal(11,2) not null,
  returkr decimal(11,2) not null,
  returant integer not null,
  feil1kr decimal(11,2) not null,
  feil1ant integer not null,
  feil2kr decimal(11,2) not null,
  feil2ant integer not null,
  avrundkr decimal(6,2) not null,
  nullant integer not null,
  tellkontkr decimal(11,2) not null,
  tellsjekkr decimal(11,2) not null,
  tellmed3kr decimal(11,2) not null,
  tellmed4kr decimal(11,2) not null,
  tellblgkr decimal(11,2) not null,
  levbankkr decimal(11,2) not null,
  tellbankkr decimal(11,2) not null,
  tellkr decimal(11,2) not null,
  varekr decimal(11,2) not null,
  negvarekr decimal(11,2) not null,
  kupongkr decimal(11,2) not null,
  kupongant integer not null,
  nettokr decimal(11,2) not null,
  bruttokr decimal(11,2) not null,
  ikkekredkr decimal(11,2) not null,
  behkontkr decimal(11,2) not null,
  behkredkr decimal(11,2) not null,
  behmed3kr decimal(11,2) not null,
  behmed4kr decimal(11,2) not null,
  behbankkr decimal(11,2) not null,
  behsjekkr decimal(11,2) not null,
  regminkr decimal(11,2) not null,
  regminant integer not null,
  ringcountant integer not null,
  scannant integer not null,
  paragonant integer not null,
  prisant integer not null,
  kassenr integer not null,
  grandtot decimal(14,2) not null,
  dummy1 decimal(11,2) not null,
  dummy2 decimal(11,2) not null,
  dymmy3 decimal(11,2) not null,
  dummy4 decimal(11,2) not null,
  dummy5 decimal(11,2) not null,
  dummy6 decimal(11,2) not null,
  dummy7 decimal(11,2) not null,
  dummy8 decimal(11,2) not null,
  dummy9 decimal(11,2) not null,
  internkr decimal(11,2) not null,
  internant integer not null,
  medinnkr decimal(11,2) not null,
  medinnant integer not null,
  medutkr decimal(11,2) not null,
  medutant integer not null,
  depkr decimal(11,2) not null,
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
 
create trigger _TI_finansdag ON finansdag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from finansdag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX finansdag#_#progress_recid ON finansdag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX finansdag#_#progress_recid_ident_ ON finansdag (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX finansdag##finansdagidx1 ON finansdag (butnr, dato, kassenr)
go
CREATE INDEX finansdag##finansdagidx2 ON finansdag (butnr, kassenr, znr, PROGRESS_RECID)
go
CREATE INDEX finansdag##finansdagidx3 ON finansdag (dato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'hgrdag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table hgrdag
go
CREATE TABLE hgrdag (
  butnr integer not null,
  hgr integer not null,
  dato datetime not null,
  kostpris decimal(9,2) not null,
  mvakr decimal(9,2) not null,
  salgssum decimal(9,2) not null,
  kostkamp decimal(9,2) not null,
  mvakamp decimal(9,2) not null,
  salgkamp decimal(9,2) not null,
  kostmix decimal(9,2) not null,
  mvamix decimal(9,2) not null,
  salgmix decimal(9,2) not null,
  kostmed decimal(9,2) not null,
  mvamed decimal(9,2) not null,
  salgmed decimal(9,2) not null,
  medrabkr decimal(8,2) not null,
  kunrabkr decimal(8,2) not null,
  perrabkr decimal(8,2) not null,
  genrabkr decimal(8,2) not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_hgrdag ON hgrdag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from hgrdag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX hgrdag#_#progress_recid ON hgrdag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX hgrdag#_#progress_recid_ident_ ON hgrdag (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX hgrdag##hgrdagidx1 ON hgrdag (butnr, hgr, dato)
go
CREATE INDEX hgrdag##hgrdagidx2 ON hgrdag (hgr, PROGRESS_RECID)
go
CREATE INDEX hgrdag##hgrdagidx3 ON hgrdag (dato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'jowereceipt' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jowereceipt
go
CREATE TABLE jowereceipt (
  act_no varchar (14) null,
  act_date datetime null,
  logicdate datetime null,
  act_time varchar (5) null,
  our_id varchar (15) null,
  our_no varchar (14) null,
  individ_no varchar (14) null,
  individ_id varchar (15) null,
  name varchar (36) null,
  salesp_id varchar (15) null,
  user_id varchar (15) null,
  eme_no varchar (14) null,
  account_2 varchar (12) null,
  project_id varchar (12) null,
  notes varchar (40) null,
  inserted datetime null,
  updated datetime null,
  i_user_no varchar (14) null,
  user_no varchar (14) null,
  camp_no varchar (14) null,
  cmp_act_no varchar (14) null,
  is_intern varchar (1) null,
  is_done varchar (1) null,
  stockvalue decimal(10,2) null,
  freight decimal(10,2) null,
  inv_fee decimal(10,2) null,
  vat decimal(14,4) null,
  rowsum decimal(14,4) null,
  discount decimal(12,2) null,
  discount_p decimal(10,3) null,
  round_off decimal(12,4) null,
  total decimal(12,2) null,
  curr_rate decimal(10,6) null,
  currencyid varchar (3) null,
  is_approve varchar (1) null,
  is_sent varchar (1) null,
  buyer_type varchar (1) null,
  receipt_no varchar (14) null,
  balance_no varchar (14) null,
  tilltype varchar (2) null,
  tillunitid varchar (15) null,
  s_change decimal(12,2) null,
  print_sap varchar (20) null,
  origin_no varchar (14) null,
  offline_no varchar (40) null,
  custgrp_id varchar (12) null,
  shop_id varchar (15) null,
  cashreg_no integer null,
  signature varchar (6) null,
  supp_price decimal(12,2) null,
  cartypeid varchar (12) null,
  credcardid varchar (16) null,
  legetimat varchar (12) null,
  their_id varchar (15) null,
  eme_name varchar (30) null,
  is_stat varchar (1) null,
  is_report varchar (1) null,
  is_staff varchar (1) null,
  is_locsold varchar (1) null,
  is_printed varchar (1) null,
  is_export varchar (1) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jowereceipt ON jowereceipt for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jowereceipt t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jowereceipt#_#progress_recid ON jowereceipt (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jowereceipt#_#progress_recid_ident_ ON jowereceipt (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jowereceipt##datetimeno ON jowereceipt (act_date, act_time, receipt_no)
go
if (select name from sysobjects 
    where name = 'jowerec_pay' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jowerec_pay
go
CREATE TABLE jowerec_pay (
  receipt_no varchar (14) null,
  paytype_id varchar (12) null,
  payment_id varchar (22) null,
  rate_out decimal(10,6) null,
  amount decimal(10,2) null,
  amount_org decimal(10,2) null,
  paidamount decimal(10,2) null,
  amount_ret decimal(10,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jowerec_pay ON jowerec_pay for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jowerec_pay t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jowerec_pay#_#progress_recid ON jowerec_pay (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jowerec_pay#_#progress_recid_ident_ ON jowerec_pay (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jowerec_pay##rec_pay ON jowerec_pay (receipt_no, paytype_id, payment_id)
go
if (select name from sysobjects 
    where name = 'jowerec_row' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table jowerec_row
go
CREATE TABLE jowerec_row (
  seqno varchar (14) null,
  row_no integer null,
  product_no varchar (14) null,
  unit_id varchar (12) null,
  pricetypeid varchar (12) null,
  quantity decimal(9,3) null,
  stk_convf decimal(8,4) null,
  supp_price decimal(10,2) null,
  unit_price decimal(10,2) null,
  amount decimal(14,4) null,
  discount_p decimal(6,2) null,
  discount decimal(14,4) null,
  vat_p decimal(6,4) null,
  vat decimal(14,4) null,
  contractno varchar (18) null,
  group_no varchar (14) null,
  account_4 varchar (6) null,
  account_5 varchar (6) null,
  is_main_row varchar (1) null,
  is_text varchar (1) null,
  is_mandisc varchar (1) null,
  is_disc_p varchar (1) null,
  is_n_price varchar (1) null,
  product_id varchar (20) null,
  descript varchar (73) null,
  prodgr_id varchar (12) null,
  receipt_no varchar (14) null,
  origin_id varchar (1) null,
  prod_class varchar (20) null,
  disc_cust decimal(14,4) null,
  disc_empl decimal(14,4) null,
  disc_act decimal(14,4) null,
  inserted datetime null,
  cred_qty integer null,
  custord_no varchar (14) null,
  cordrow_no varchar (14) null,
  report_code varchar (1) null,
  stat_code varchar (1) null,
  is_presold varchar (1) null,
  istillunitstock varchar (1) null,
  our_no varchar (14) null,
  unitpricewoact decimal(10,2) null,
  disctypeid varchar (12) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_jowerec_row ON jowerec_row for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from jowerec_row t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX jowerec_row#_#progress_recid ON jowerec_row (PROGRESS_RECID)
go
CREATE UNIQUE INDEX jowerec_row#_#progress_recid_ident_ ON jowerec_row (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX jowerec_row##receipt_row ON jowerec_row (receipt_no, row_no)
go
CREATE UNIQUE INDEX jowerec_row##seqno ON jowerec_row (seqno)
go
if (select name from sysobjects 
    where name = 'kassdag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table kassdag
go
CREATE TABLE kassdag (
  butnr integer not null,
  dato datetime not null,
  kassnr integer not null,
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
 
create trigger _TI_kassdag ON kassdag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from kassdag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX kassdag#_#progress_recid ON kassdag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX kassdag#_#progress_recid_ident_ ON kassdag (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX kassdag##kassdagidx1 ON kassdag (butnr, kassnr, dato)
go
CREATE INDEX kassdag##kassdagidx2 ON kassdag (dato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pbvendor' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pbvendor
go
CREATE TABLE pbvendor (
  vendorid decimal(8,2) null,
  name varchar (30) null,
  slettet tinyint null,
  slettetdato datetime null,
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
 
create trigger _TI_pbvendor ON pbvendor for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pbvendor t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pbvendor#_#progress_recid ON pbvendor (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pbvendor#_#progress_recid_ident_ ON pbvendor (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pbvendor##endret ON pbvendor (edato, etid, PROGRESS_RECID)
go
CREATE INDEX pbvendor##navn ON pbvendor (name, PROGRESS_RECID)
go
CREATE INDEX pbvendor##ny ON pbvendor (registrertdato, registrerttid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pbvendor##vendor ON pbvendor (vendorid)
go
if (select name from sysobjects 
    where name = 'pfdaysales' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pfdaysales
go
CREATE TABLE pfdaysales (
  store_no integer null,
  plu_code varchar (20) null,
  date_ datetime null,
  sales decimal(10,2) null,
  disc decimal(9,2) null,
  qty decimal(8,3) null,
  vendorid integer null,
  vat decimal(9,2) null,
  salescost decimal(10,2) null,
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
 
create trigger _TI_pfdaysales ON pfdaysales for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pfdaysales t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pfdaysales#_#progress_recid ON pfdaysales (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfdaysales#_#progress_recid_ident_ ON pfdaysales (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX pfdaysales##daysales ON pfdaysales (store_no, plu_code, date_)
go
CREATE INDEX pfdaysales##endret ON pfdaysales (edato, etid, PROGRESS_RECID)
go
CREATE INDEX pfdaysales##lev ON pfdaysales (vendorid, store_no, date_, plu_code, PROGRESS_RECID)
go
CREATE INDEX pfdaysales##ny ON pfdaysales (registrertdato, registrerttid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pfdaysales_hourext' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pfdaysales_hourext
go
CREATE TABLE pfdaysales_hourext (
  store_no integer null,
  date_ datetime null,
  tid integer null,
  departmentid integer null,
  retaildate datetime null,
  omsetning decimal(10,2) null,
  disc decimal(9,2) null,
  qty decimal(8,3) null,
  vat decimal(8,2) null,
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
 
create trigger _TI_pfdaysales_hourext ON pfdaysales_hourext for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pfdaysales_hourext t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pfdaysales_hourext#_#progress_recid ON pfdaysales_hourext (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfdaysales_hourext#_#progress_recid_ident_ ON pfdaysales_hourext (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pfdaysales_hourext##butikk ON pfdaysales_hourext (store_no, date_, tid, departmentid, PROGRESS_RECID)
go
CREATE INDEX pfdaysales_hourext##daato ON pfdaysales_hourext (date_, tid, store_no, departmentid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfdaysales_hourext##departmenttid ON pfdaysales_hourext (departmentid, store_no, date_, tid)
go
CREATE INDEX pfdaysales_hourext##endret ON pfdaysales_hourext (edato, etid, PROGRESS_RECID)
go
CREATE INDEX pfdaysales_hourext##ny ON pfdaysales_hourext (registrertdato, registrerttid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pfdepartments' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pfdepartments
go
CREATE TABLE pfdepartments (
  departmentid integer null,
  description varchar (30) null,
  slettet tinyint null,
  slettetdato datetime null,
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
 
create trigger _TI_pfdepartments ON pfdepartments for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pfdepartments t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pfdepartments#_#progress_recid ON pfdepartments (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfdepartments#_#progress_recid_ident_ ON pfdepartments (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX pfdepartments##dep ON pfdepartments (departmentid)
go
CREATE INDEX pfdepartments##descr ON pfdepartments (description, PROGRESS_RECID)
go
CREATE INDEX pfdepartments##endret ON pfdepartments (edato, etid, PROGRESS_RECID)
go
CREATE INDEX pfdepartments##ny ON pfdepartments (registrertdato, registrerttid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pfitemgroup' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pfitemgroup
go
CREATE TABLE pfitemgroup (
  groupid integer null,
  description varchar (30) null,
  slettet tinyint null,
  slettetdato datetime null,
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
 
create trigger _TI_pfitemgroup ON pfitemgroup for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pfitemgroup t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pfitemgroup#_#progress_recid ON pfitemgroup (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfitemgroup#_#progress_recid_ident_ ON pfitemgroup (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pfitemgroup##descript ON pfitemgroup (description, PROGRESS_RECID)
go
CREATE INDEX pfitemgroup##endret ON pfitemgroup (edato, etid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfitemgroup##itemgrp ON pfitemgroup (groupid)
go
CREATE INDEX pfitemgroup##ny ON pfitemgroup (registrertdato, registrerttid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pfiteminfo' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pfiteminfo
go
CREATE TABLE pfiteminfo (
  mainitemid varchar (20) null,
  iteminternalkey decimal(15,2) null,
  subgroupid integer null,
  groupid integer null,
  departmentid integer null,
  mainvendorid integer null,
  fullname varchar (30) null,
  slettet tinyint null,
  slettetdato datetime null,
  manufactureid decimal(8,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  vat1id integer null,
  vat2id integer null,
  vat3id integer null,
  vat4id integer null,
  vat5id integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_pfiteminfo ON pfiteminfo for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pfiteminfo t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pfiteminfo#_#progress_recid ON pfiteminfo (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfiteminfo#_#progress_recid_ident_ ON pfiteminfo (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pfiteminfo##artikkel ON pfiteminfo (iteminternalkey, mainitemid, PROGRESS_RECID)
go
CREATE INDEX pfiteminfo##avd ON pfiteminfo (departmentid, PROGRESS_RECID)
go
CREATE INDEX pfiteminfo##endret ON pfiteminfo (edato, etid, PROGRESS_RECID)
go
CREATE INDEX pfiteminfo##hg ON pfiteminfo (groupid, PROGRESS_RECID)
go
CREATE INDEX pfiteminfo##ny ON pfiteminfo (registrertdato, registrerttid, PROGRESS_RECID)
go
CREATE INDEX pfiteminfo##produsent ON pfiteminfo (manufactureid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfiteminfo##strekkode ON pfiteminfo (mainitemid)
go
CREATE INDEX pfiteminfo##varetekst ON pfiteminfo (fullname, PROGRESS_RECID)
go
CREATE INDEX pfiteminfo##vg ON pfiteminfo (subgroupid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pfitemsubgroup' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pfitemsubgroup
go
CREATE TABLE pfitemsubgroup (
  subgroupid varchar (15) null,
  description varchar (30) null,
  slettet tinyint null,
  slettetdato datetime null,
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
 
create trigger _TI_pfitemsubgroup ON pfitemsubgroup for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pfitemsubgroup t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pfitemsubgroup#_#progress_recid ON pfitemsubgroup (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfitemsubgroup#_#progress_recid_ident_ ON pfitemsubgroup (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pfitemsubgroup##descript ON pfitemsubgroup (description, PROGRESS_RECID)
go
CREATE INDEX pfitemsubgroup##endret ON pfitemsubgroup (edato, etid, PROGRESS_RECID)
go
CREATE INDEX pfitemsubgroup##ny ON pfitemsubgroup (registrertdato, registrerttid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfitemsubgroup##subgroup ON pfitemsubgroup (subgroupid)
go
if (select name from sysobjects 
    where name = 'pfmanefacturer' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pfmanefacturer
go
CREATE TABLE pfmanefacturer (
  manufactureid decimal(8,2) null,
  name varchar (30) null,
  slettet tinyint null,
  slettetdato datetime null,
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
 
create trigger _TI_pfmanefacturer ON pfmanefacturer for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pfmanefacturer t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pfmanefacturer#_#progress_recid ON pfmanefacturer (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfmanefacturer#_#progress_recid_ident_ ON pfmanefacturer (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pfmanefacturer##endret ON pfmanefacturer (edato, etid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfmanefacturer##manefact ON pfmanefacturer (manufactureid)
go
CREATE INDEX pfmanefacturer##name ON pfmanefacturer (name, PROGRESS_RECID)
go
CREATE INDEX pfmanefacturer##ny ON pfmanefacturer (registrertdato, registrerttid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pfmedias' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pfmedias
go
CREATE TABLE pfmedias (
  mediaid integer null,
  description varchar (30) null,
  slettet tinyint null,
  slettetdato datetime null,
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
 
create trigger _TI_pfmedias ON pfmedias for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pfmedias t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pfmedias#_#progress_recid ON pfmedias (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfmedias#_#progress_recid_ident_ ON pfmedias (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pfmedias##dexcript ON pfmedias (description, PROGRESS_RECID)
go
CREATE INDEX pfmedias##endret ON pfmedias (edato, etid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfmedias##media ON pfmedias (mediaid)
go
CREATE INDEX pfmedias##ny ON pfmedias (registrertdato, registrerttid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pfslinfo' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pfslinfo
go
CREATE TABLE pfslinfo (
  salgl_nr integer null,
  salgsl_navn varchar (30) null,
  segmentkode varchar (30) null,
  segmentbeskrivelse varchar (30) null,
  region integer null,
  regionnavn varchar (30) null,
  distriktsnr integer null,
  apnetdato integer null,
  midlstengtfra integer null,
  midlstengttil integer null,
  nedlagtdato integer null,
  slettet tinyint null,
  slettetdato datetime null,
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
 
create trigger _TI_pfslinfo ON pfslinfo for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pfslinfo t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pfslinfo#_#progress_recid ON pfslinfo (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfslinfo#_#progress_recid_ident_ ON pfslinfo (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pfslinfo##apnet ON pfslinfo (apnetdato, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfslinfo##butikk ON pfslinfo (salgl_nr)
go
CREATE INDEX pfslinfo##distrikt ON pfslinfo (distriktsnr, PROGRESS_RECID)
go
CREATE INDEX pfslinfo##endret ON pfslinfo (edato, etid, PROGRESS_RECID)
go
CREATE INDEX pfslinfo##navn ON pfslinfo (salgsl_navn, PROGRESS_RECID)
go
CREATE INDEX pfslinfo##nedlagt ON pfslinfo (nedlagtdato, PROGRESS_RECID)
go
CREATE INDEX pfslinfo##ny ON pfslinfo (registrertdato, registrerttid, PROGRESS_RECID)
go
CREATE INDEX pfslinfo##region ON pfslinfo (region, PROGRESS_RECID)
go
CREATE INDEX pfslinfo##segment ON pfslinfo (segmentkode, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'pfstatisticsrevenue' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pfstatisticsrevenue
go
CREATE TABLE pfstatisticsrevenue (
  store_no integer null,
  date_ datetime null,
  customers_count integer null,
  item_count integer null,
  neg_ticket_amount decimal(9,2) null,
  neg_ticket_count integer null,
  nosale_count integer null,
  refund_amount decimal(9,2) null,
  refund_count integer null,
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
 
create trigger _TI_pfstatisticsrevenue ON pfstatisticsrevenue for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pfstatisticsrevenue t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pfstatisticsrevenue#_#progress_recid ON pfstatisticsrevenue (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfstatisticsrevenue#_#progress_recid_ident_ ON pfstatisticsrevenue (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX pfstatisticsrevenue##dato ON pfstatisticsrevenue (date_, store_no, PROGRESS_RECID)
go
CREATE INDEX pfstatisticsrevenue##endret ON pfstatisticsrevenue (edato, etid, PROGRESS_RECID)
go
CREATE INDEX pfstatisticsrevenue##ny ON pfstatisticsrevenue (registrertdato, registrerttid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX pfstatisticsrevenue##statbut ON pfstatisticsrevenue (store_no, date_)
go
if (select name from sysobjects 
    where name = 'pftenderrevenue' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table pftenderrevenue
go
CREATE TABLE pftenderrevenue (
  store_no integer null,
  date_ datetime null,
  media_no integer null,
  sale_amount decimal(9,2) null,
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
 
create trigger _TI_pftenderrevenue ON pftenderrevenue for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from pftenderrevenue t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX pftenderrevenue#_#progress_recid ON pftenderrevenue (PROGRESS_RECID)
go
CREATE UNIQUE INDEX pftenderrevenue#_#progress_recid_ident_ ON pftenderrevenue (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX pftenderrevenue##butikk ON pftenderrevenue (store_no, date_, media_no)
go
CREATE INDEX pftenderrevenue##dato ON pftenderrevenue (date_, store_no, media_no, PROGRESS_RECID)
go
CREATE INDEX pftenderrevenue##endret ON pftenderrevenue (edato, etid, PROGRESS_RECID)
go
CREATE INDEX pftenderrevenue##media ON pftenderrevenue (media_no, store_no, date_, PROGRESS_RECID)
go
CREATE INDEX pftenderrevenue##ny ON pftenderrevenue (registrertdato, registrerttid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prbmdata' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prbmdata
go
CREATE TABLE prbmdata (
  butikknr integer not null,
  dato datetime null,
  betalingstype integer not null,
  bettypebeskrivelse varchar (30) null,
  subtype integer null,
  subtypenavn varchar (30) null,
  subsubtype integer null,
  belop decimal(10,2) null,
  kassadiff decimal(8,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  pfflagg integer null,
  konto integer null,
  datotid decimal(16,2) null,
  organisationsnr varchar (15) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prbmdata ON prbmdata for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prbmdata t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prbmdata#_#progress_recid ON prbmdata (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prbmdata#_#progress_recid_ident_ ON prbmdata (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prbmdata##datotid ON prbmdata (datotid, PROGRESS_RECID)
go
CREATE INDEX prbmdata##konto ON prbmdata (konto, PROGRESS_RECID)
go
CREATE INDEX prbmdata##pfflagg ON prbmdata (pfflagg, PROGRESS_RECID)
go
CREATE UNIQUE INDEX prbmdata##prbmdata ON prbmdata (butikknr, dato, betalingstype, subtype, subsubtype)
go
if (select name from sysobjects 
    where name = 'prbsdata' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prbsdata
go
CREATE TABLE prbsdata (
  butikknr integer null,
  dato datetime null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  antkvittoinne integer null,
  antkvittoute integer null,
  totsalg decimal(10,2) null,
  mvakr decimal(10,2) null,
  totdrvm decimal(11,3) null,
  drivminne decimal(11,3) null,
  mvadrm decimal(10,2) null,
  bm01 decimal(10,2) null,
  bm02 decimal(10,2) null,
  bm03 decimal(10,2) null,
  bm04 decimal(10,2) null,
  bm05 decimal(10,2) null,
  bm06 decimal(10,2) null,
  bm07 decimal(10,2) null,
  bm08 decimal(10,2) null,
  bm09 decimal(10,2) null,
  bm10 decimal(10,2) null,
  bm11 decimal(10,2) null,
  bm12 decimal(10,2) null,
  bm13 decimal(10,2) null,
  bm14 decimal(10,2) null,
  bm15 decimal(10,2) null,
  bm16 decimal(10,2) null,
  bm17 decimal(10,2) null,
  bm18 decimal(10,2) null,
  bm19 decimal(10,2) null,
  bm20 decimal(10,2) null,
  bm01ant decimal(10,2) null,
  bm02ant decimal(10,2) null,
  bm03ant decimal(10,2) null,
  bm04and decimal(10,2) null,
  bm05ant decimal(10,2) null,
  bm06ant decimal(10,2) null,
  bm07ant decimal(10,2) null,
  bm08ant decimal(10,2) null,
  bm09ant decimal(10,2) null,
  bm10ant decimal(10,2) null,
  bm11ant decimal(10,2) null,
  bm12ant decimal(10,2) null,
  bm13ant decimal(10,2) null,
  bm14ant decimal(10,2) null,
  bm15ant decimal(10,2) null,
  bm16ant decimal(10,2) null,
  bm17ant decimal(10,2) null,
  bm18ant decimal(10,2) null,
  bm19ant decimal(10,2) null,
  bm20ant decimal(10,2) null,
  datotid decimal(16,2) null,
  antkundrivstoff integer null,
  antkunvare integer null,
  antblandet integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prbsdata ON prbsdata for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prbsdata t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prbsdata#_#progress_recid ON prbsdata (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prbsdata#_#progress_recid_ident_ ON prbsdata (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prbsdata##datotid ON prbsdata (datotid, PROGRESS_RECID)
go
CREATE INDEX prbsdata##pfbsdata ON prbsdata (butikknr, dato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prbudget' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prbudget
go
CREATE TABLE prbudget (
  aar integer null,
  station integer null,
  hg integer null,
  sumfgaar decimal(11,2) null,
  p01 decimal(10,2) null,
  p02 decimal(10,2) null,
  p03 decimal(10,2) null,
  p04 decimal(10,2) null,
  p05 decimal(10,2) null,
  p06 decimal(10,2) null,
  p07 decimal(10,2) null,
  p08 decimal(10,2) null,
  p09 decimal(10,2) null,
  p10 decimal(10,2) null,
  p11 decimal(10,2) null,
  p12 decimal(10,2) null,
  datotid decimal(16,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prbudget ON prbudget for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prbudget t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prbudget#_#progress_recid ON prbudget (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prbudget#_#progress_recid_ident_ ON prbudget (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prbudget##idx1 ON prbudget (aar, station, hg, PROGRESS_RECID)
go
CREATE INDEX prbudget##idx2 ON prbudget (datotid, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prchdata' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prchdata
go
CREATE TABLE prchdata (
  butikknr integer null,
  dato datetime null,
  hg integer null,
  vg integer null,
  antsolgt decimal(9,3) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  artikkelnr varchar (20) not null,
  datotid decimal(16,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prchdata ON prchdata for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prchdata t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prchdata#_#progress_recid ON prchdata (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prchdata#_#progress_recid_ident_ ON prchdata (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prchdata##datotid ON prchdata (datotid, PROGRESS_RECID)
go
CREATE INDEX prchdata##prchdata ON prchdata (butikknr, dato, hg, vg, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'preemanalyse' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table preemanalyse
go
CREATE TABLE preemanalyse (
  analyseid integer null,
  navn varchar (30) null,
  teamnr integer null,
  startdato datetime null,
  sluttdato datetime null,
  aktiv tinyint null,
  notat varchar (40) null,
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
 
create trigger _TI_preemanalyse ON preemanalyse for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from preemanalyse t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX preemanalyse#_#progress_recid ON preemanalyse (PROGRESS_RECID)
go
CREATE UNIQUE INDEX preemanalyse#_#progress_recid_ident_ ON preemanalyse (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX preemanalyse##aktiv ON preemanalyse (aktiv, PROGRESS_RECID)
go
CREATE INDEX preemanalyse##navn ON preemanalyse (navn, PROGRESS_RECID)
go
CREATE UNIQUE INDEX preemanalyse##preemanalyse ON preemanalyse (analyseid)
go
if (select name from sysobjects 
    where name = 'preemanalysembr' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table preemanalysembr
go
CREATE TABLE preemanalysembr (
  analyseid integer null,
  radnr integer null,
  kampid decimal(15,2) null,
  kamptilbid integer null,
  artikkelnr decimal(15,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_preemanalysembr ON preemanalysembr for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from preemanalysembr t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX preemanalysembr#_#progress_recid ON preemanalysembr (PROGRESS_RECID)
go
CREATE UNIQUE INDEX preemanalysembr#_#progress_recid_ident_ ON preemanalysembr (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX preemanalysembr##radnr ON preemanalysembr (analyseid, radnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'preemanalyserad' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table preemanalyserad
go
CREATE TABLE preemanalyserad (
  analyseid integer null,
  radnr integer null,
  beskr varchar (30) null,
  typ integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_preemanalyserad ON preemanalyserad for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from preemanalyserad t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX preemanalyserad#_#progress_recid ON preemanalyserad (PROGRESS_RECID)
go
CREATE UNIQUE INDEX preemanalyserad#_#progress_recid_ident_ ON preemanalyserad (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX preemanalyserad##rad ON preemanalyserad (analyseid, radnr)
go
if (select name from sysobjects 
    where name = 'preemanalyseresult' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table preemanalyseresult
go
CREATE TABLE preemanalyseresult (
  analyseid integer null,
  butikknr integer null,
  dato datetime null,
  radnr integer null,
  antall decimal(8,2) null,
  summa decimal(10,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_preemanalyseresult ON preemanalyseresult for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from preemanalyseresult t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX preemanalyseresult#_#progress_recid ON preemanalyseresult (PROGRESS_RECID)
go
CREATE UNIQUE INDEX preemanalyseresult#_#progress_recid_ident_ ON preemanalyseresult (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX preemanalyseresult##rad ON preemanalyseresult (analyseid, butikknr, dato, radnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prfsdata' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prfsdata
go
CREATE TABLE prfsdata (
  butikknr integer null,
  dato datetime null,
  avdelingsnr integer null,
  hg integer null,
  vg integer null,
  antsolgtnto decimal(9,3) null,
  antsolgtplu decimal(9,3) null,
  fsgnto decimal(9,3) null,
  dbkr decimal(8,2) null,
  mvakr decimal(8,2) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  manrabatter decimal(8,2) null,
  centkamprabattok decimal(8,2) null,
  centkamprabattnok decimal(8,2) null,
  lokkamprabatt decimal(8,2) null,
  manrabatter_vol decimal(9,3) null,
  centkamprabattok_vol decimal(9,3) null,
  centkamprabattnok_vol decimal(9,3) null,
  lokkamprabatt_vol decimal(9,3) null,
  datotid decimal(16,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prfsdata ON prfsdata for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prfsdata t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prfsdata#_#progress_recid ON prfsdata (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prfsdata#_#progress_recid_ident_ ON prfsdata (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prfsdata##datotid ON prfsdata (datotid, PROGRESS_RECID)
go
CREATE INDEX prfsdata##pffsdata ON prfsdata (butikknr, dato, avdelingsnr, hg, vg, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'priskontroll' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table priskontroll
go
CREATE TABLE priskontroll (
  butikknr integer not null,
  gruppenr integer not null,
  kassenr integer not null,
  dato datetime null,
  artikkelnr decimal(15,2) null,
  bongtekst varchar (20) null,
  feilkode integer null,
  b_id decimal(14,2) null,
  sevarekost decimal(9,2) null,
  kassevarekost decimal(9,2) null,
  sepris decimal(10,2) null,
  bongpris decimal(10,2) null,
  antall decimal(9,3) null,
  strekkode varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_priskontroll ON priskontroll for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from priskontroll t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX priskontroll#_#progress_recid ON priskontroll (PROGRESS_RECID)
go
CREATE UNIQUE INDEX priskontroll#_#progress_recid_ident_ ON priskontroll (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX priskontroll##butartdat ON priskontroll (butikknr, artikkelnr, dato, PROGRESS_RECID)
go
CREATE INDEX priskontroll##butdatart ON priskontroll (butikknr, dato, artikkelnr, PROGRESS_RECID)
go
CREATE INDEX priskontroll##feilkode ON priskontroll (butikknr, gruppenr, kassenr, dato, artikkelnr, feilkode, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prkddata' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prkddata
go
CREATE TABLE prkddata (
  butikknr integer not null,
  dato datetime null,
  subtype integer null,
  subtypenavn varchar (30) null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  pfflagg integer null,
  volum decimal(10,3) null,
  vg integer null,
  antall integer null,
  datotid decimal(16,2) null,
  belopp decimal(10,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prkddata ON prkddata for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prkddata t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prkddata#_#progress_recid ON prkddata (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prkddata#_#progress_recid_ident_ ON prkddata (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prkddata##datotid ON prkddata (datotid, PROGRESS_RECID)
go
CREATE INDEX prkddata##pfflagg ON prkddata (pfflagg, PROGRESS_RECID)
go
CREATE INDEX prkddata##prddata ON prkddata (butikknr, vg, dato, PROGRESS_RECID)
go
CREATE INDEX prkddata##prddata2 ON prkddata (butikknr, dato, vg, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prksdata' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prksdata
go
CREATE TABLE prksdata (
  butikknr integer not null,
  gruppenr integer not null,
  dato datetime null,
  kundenr decimal(15,2) null,
  sumkreditsalg decimal(10,2) null,
  antallkreditsalg integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  pfflagg integer null,
  datotid decimal(16,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prksdata ON prksdata for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prksdata t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prksdata#_#progress_recid ON prksdata (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prksdata#_#progress_recid_ident_ ON prksdata (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prksdata##datotid ON prksdata (datotid, PROGRESS_RECID)
go
CREATE INDEX prksdata##pfflagg ON prksdata (pfflagg, PROGRESS_RECID)
go
CREATE UNIQUE INDEX prksdata##prksdata ON prksdata (butikknr, kundenr, dato)
go
CREATE INDEX prksdata##prksdata2 ON prksdata (butikknr, dato, kundenr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prn9hgdata' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prn9hgdata
go
CREATE TABLE prn9hgdata (
  hg integer not null,
  butikknr integer not null,
  dato datetime null,
  konto integer null,
  sumvaresalg decimal(10,2) null,
  mvakr decimal(9,2) null,
  sumvolumantall decimal(11,3) null,
  pfflagg integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  sumbruttofsg decimal(10,2) null,
  sumrab decimal(10,2) null,
  datotid decimal(16,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prn9hgdata ON prn9hgdata for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prn9hgdata t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prn9hgdata#_#progress_recid ON prn9hgdata (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prn9hgdata#_#progress_recid_ident_ ON prn9hgdata (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prn9hgdata##datotid ON prn9hgdata (datotid, PROGRESS_RECID)
go
CREATE INDEX prn9hgdata##pfflagg ON prn9hgdata (pfflagg, PROGRESS_RECID)
go
CREATE INDEX prn9hgdata##prn9hgdata ON prn9hgdata (butikknr, hg, dato, PROGRESS_RECID)
go
CREATE INDEX prn9hgdata##prn9hgdata2 ON prn9hgdata (butikknr, dato, hg, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prpgdata' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prpgdata
go
CREATE TABLE prpgdata (
  varegr integer not null,
  butikknr integer not null,
  dato datetime null,
  konto integer null,
  sumvaresalg decimal(10,2) null,
  mvakr decimal(9,2) null,
  sumvolumantall decimal(11,3) null,
  pfflagg integer null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  sumbruttofsg decimal(10,2) null,
  sumrab decimal(10,2) null,
  datotid decimal(16,2) null,
  organisationsnr varchar (15) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prpgdata ON prpgdata for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prpgdata t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prpgdata#_#progress_recid ON prpgdata (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prpgdata#_#progress_recid_ident_ ON prpgdata (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prpgdata##datotid ON prpgdata (datotid, PROGRESS_RECID)
go
CREATE INDEX prpgdata##pfflagg ON prpgdata (pfflagg, PROGRESS_RECID)
go
CREATE INDEX prpgdata##prpgdata ON prpgdata (butikknr, varegr, dato, PROGRESS_RECID)
go
CREATE INDEX prpgdata##prpgdata2 ON prpgdata (butikknr, dato, varegr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'prvsdata' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table prvsdata
go
CREATE TABLE prvsdata (
  butikknr integer not null,
  dato datetime null,
  produktkod decimal(13,0) null,
  betalingstype integer not null,
  subtype integer null,
  volym decimal(8,2) null,
  belop decimal(10,2) null,
  datotid decimal(16,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_prvsdata ON prvsdata for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from prvsdata t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX prvsdata#_#progress_recid ON prvsdata (PROGRESS_RECID)
go
CREATE UNIQUE INDEX prvsdata#_#progress_recid_ident_ ON prvsdata (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX prvsdata##datotid ON prvsdata (datotid, PROGRESS_RECID)
go
CREATE UNIQUE INDEX prvsdata##subtype ON prvsdata (butikknr, dato, produktkod, betalingstype, subtype)
go
if (select name from sysobjects 
    where name = 'skift' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table skift
go
CREATE TABLE skift (
  butikknr integer not null,
  kasserernr decimal(15,2) not null,
  dato datetime null,
  edato datetime null,
  etid integer null,
  brukerid varchar (15) null,
  registrertdato datetime null,
  registrerttid integer null,
  registrertav varchar (15) null,
  pfflagg integer null,
  skiftid decimal(13,0) null,
  bokforingsid decimal(13,0) not null,
  skiftnr decimal(9,0) null,
  aar integer null,
  terminert tinyint null,
  n9skiftnr decimal(9,0) null,
  datetime decimal(13,0) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_skift ON skift for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from skift t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX skift#_#progress_recid ON skift (PROGRESS_RECID)
go
CREATE UNIQUE INDEX skift#_#progress_recid_ident_ ON skift (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX skift##aapneskift ON skift (butikknr, bokforingsid, skiftnr, PROGRESS_RECID)
go
CREATE INDEX skift##bokforingsid ON skift (bokforingsid, PROGRESS_RECID)
go
CREATE INDEX skift##pfflagg ON skift (pfflagg, PROGRESS_RECID)
go
CREATE UNIQUE INDEX skift##skift ON skift (butikknr, aar, skiftnr)
go
CREATE INDEX skift##skiftdatetime ON skift (butikknr, skiftnr, datetime, PROGRESS_RECID)
go
CREATE INDEX skift##skiftid ON skift (skiftid, PROGRESS_RECID)
go
CREATE INDEX skift##terminert ON skift (butikknr, terminert, skiftnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'timedag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table timedag
go
CREATE TABLE timedag (
  butnr integer not null,
  dato datetime not null,
  vareant##1 integer not null,
  vareant##2 integer not null,
  vareant##3 integer not null,
  vareant##4 integer not null,
  vareant##5 integer not null,
  vareant##6 integer not null,
  vareant##7 integer not null,
  vareant##8 integer not null,
  vareant##9 integer not null,
  vareant##10 integer not null,
  vareant##11 integer not null,
  vareant##12 integer not null,
  vareant##13 integer not null,
  vareant##14 integer not null,
  vareant##15 integer not null,
  vareant##16 integer not null,
  vareant##17 integer not null,
  vareant##18 integer not null,
  vareant##19 integer not null,
  vareant##20 integer not null,
  vareant##21 integer not null,
  vareant##22 integer not null,
  vareant##23 integer not null,
  vareant##24 integer not null,
  kundeant##1 integer not null,
  kundeant##2 integer not null,
  kundeant##3 integer not null,
  kundeant##4 integer not null,
  kundeant##5 integer not null,
  kundeant##6 integer not null,
  kundeant##7 integer not null,
  kundeant##8 integer not null,
  kundeant##9 integer not null,
  kundeant##10 integer not null,
  kundeant##11 integer not null,
  kundeant##12 integer not null,
  kundeant##13 integer not null,
  kundeant##14 integer not null,
  kundeant##15 integer not null,
  kundeant##16 integer not null,
  kundeant##17 integer not null,
  kundeant##18 integer not null,
  kundeant##19 integer not null,
  kundeant##20 integer not null,
  kundeant##21 integer not null,
  kundeant##22 integer not null,
  kundeant##23 integer not null,
  kundeant##24 integer not null,
  salgssum##1 decimal(9,2) not null,
  salgssum##2 decimal(9,2) not null,
  salgssum##3 decimal(9,2) not null,
  salgssum##4 decimal(9,2) not null,
  salgssum##5 decimal(9,2) not null,
  salgssum##6 decimal(9,2) not null,
  salgssum##7 decimal(9,2) not null,
  salgssum##8 decimal(9,2) not null,
  salgssum##9 decimal(9,2) not null,
  salgssum##10 decimal(9,2) not null,
  salgssum##11 decimal(9,2) not null,
  salgssum##12 decimal(9,2) not null,
  salgssum##13 decimal(9,2) not null,
  salgssum##14 decimal(9,2) not null,
  salgssum##15 decimal(9,2) not null,
  salgssum##16 decimal(9,2) not null,
  salgssum##17 decimal(9,2) not null,
  salgssum##18 decimal(9,2) not null,
  salgssum##19 decimal(9,2) not null,
  salgssum##20 decimal(9,2) not null,
  salgssum##21 decimal(9,2) not null,
  salgssum##22 decimal(9,2) not null,
  salgssum##23 decimal(9,2) not null,
  salgssum##24 decimal(9,2) not null,
  kostpris##1 decimal(9,2) not null,
  kostpris##2 decimal(9,2) not null,
  kostpris##3 decimal(9,2) not null,
  kostpris##4 decimal(9,2) not null,
  kostpris##5 decimal(9,2) not null,
  kostpris##6 decimal(9,2) not null,
  kostpris##7 decimal(9,2) not null,
  kostpris##8 decimal(9,2) not null,
  kostpris##9 decimal(9,2) not null,
  kostpris##10 decimal(9,2) not null,
  kostpris##11 decimal(9,2) not null,
  kostpris##12 decimal(9,2) not null,
  kostpris##13 decimal(9,2) not null,
  kostpris##14 decimal(9,2) not null,
  kostpris##15 decimal(9,2) not null,
  kostpris##16 decimal(9,2) not null,
  kostpris##17 decimal(9,2) not null,
  kostpris##18 decimal(9,2) not null,
  kostpris##19 decimal(9,2) not null,
  kostpris##20 decimal(9,2) not null,
  kostpris##21 decimal(9,2) not null,
  kostpris##22 decimal(9,2) not null,
  kostpris##23 decimal(9,2) not null,
  kostpris##24 decimal(9,2) not null,
  mvakr##1 decimal(9,2) not null,
  mvakr##2 decimal(9,2) not null,
  mvakr##3 decimal(9,2) not null,
  mvakr##4 decimal(9,2) not null,
  mvakr##5 decimal(9,2) not null,
  mvakr##6 decimal(9,2) not null,
  mvakr##7 decimal(9,2) not null,
  mvakr##8 decimal(9,2) not null,
  mvakr##9 decimal(9,2) not null,
  mvakr##10 decimal(9,2) not null,
  mvakr##11 decimal(9,2) not null,
  mvakr##12 decimal(9,2) not null,
  mvakr##13 decimal(9,2) not null,
  mvakr##14 decimal(9,2) not null,
  mvakr##15 decimal(9,2) not null,
  mvakr##16 decimal(9,2) not null,
  mvakr##17 decimal(9,2) not null,
  mvakr##18 decimal(9,2) not null,
  mvakr##19 decimal(9,2) not null,
  mvakr##20 decimal(9,2) not null,
  mvakr##21 decimal(9,2) not null,
  mvakr##22 decimal(9,2) not null,
  mvakr##23 decimal(9,2) not null,
  mvakr##24 decimal(9,2) not null,
  aktivant##1 integer not null,
  aktivant##2 integer not null,
  aktivant##3 integer not null,
  aktivant##4 integer not null,
  aktivant##5 integer not null,
  aktivant##6 integer not null,
  aktivant##7 integer not null,
  aktivant##8 integer not null,
  aktivant##9 integer not null,
  aktivant##10 integer not null,
  aktivant##11 integer not null,
  aktivant##12 integer not null,
  aktivant##13 integer not null,
  aktivant##14 integer not null,
  aktivant##15 integer not null,
  aktivant##16 integer not null,
  aktivant##17 integer not null,
  aktivant##18 integer not null,
  aktivant##19 integer not null,
  aktivant##20 integer not null,
  aktivant##21 integer not null,
  aktivant##22 integer not null,
  aktivant##23 integer not null,
  aktivant##24 integer not null,
  medkr##1 decimal(11,2) not null,
  medkr##2 decimal(11,2) not null,
  medkr##3 decimal(11,2) not null,
  medkr##4 decimal(11,2) not null,
  medkr##5 decimal(11,2) not null,
  medkr##6 decimal(11,2) not null,
  medkr##7 decimal(11,2) not null,
  medkr##8 decimal(11,2) not null,
  medkr##9 decimal(11,2) not null,
  medkr##10 decimal(11,2) not null,
  medkr##11 decimal(11,2) not null,
  medkr##12 decimal(11,2) not null,
  medkr##13 decimal(11,2) not null,
  medkr##14 decimal(11,2) not null,
  medkr##15 decimal(11,2) not null,
  medkr##16 decimal(11,2) not null,
  medkr##17 decimal(11,2) not null,
  medkr##18 decimal(11,2) not null,
  medkr##19 decimal(11,2) not null,
  medkr##20 decimal(11,2) not null,
  medkr##21 decimal(11,2) not null,
  medkr##22 decimal(11,2) not null,
  medkr##23 decimal(11,2) not null,
  medkr##24 decimal(11,2) not null,
  medant##1 integer not null,
  medant##2 integer not null,
  medant##3 integer not null,
  medant##4 integer not null,
  medant##5 integer not null,
  medant##6 integer not null,
  medant##7 integer not null,
  medant##8 integer not null,
  medant##9 integer not null,
  medant##10 integer not null,
  medant##11 integer not null,
  medant##12 integer not null,
  medant##13 integer not null,
  medant##14 integer not null,
  medant##15 integer not null,
  medant##16 integer not null,
  medant##17 integer not null,
  medant##18 integer not null,
  medant##19 integer not null,
  medant##20 integer not null,
  medant##21 integer not null,
  medant##22 integer not null,
  medant##23 integer not null,
  medant##24 integer not null,
  medvare##1 integer not null,
  medvare##2 integer not null,
  medvare##3 integer not null,
  medvare##4 integer not null,
  medvare##5 integer not null,
  medvare##6 integer not null,
  medvare##7 integer not null,
  medvare##8 integer not null,
  medvare##9 integer not null,
  medvare##10 integer not null,
  medvare##11 integer not null,
  medvare##12 integer not null,
  medvare##13 integer not null,
  medvare##14 integer not null,
  medvare##15 integer not null,
  medvare##16 integer not null,
  medvare##17 integer not null,
  medvare##18 integer not null,
  medvare##19 integer not null,
  medvare##20 integer not null,
  medvare##21 integer not null,
  medvare##22 integer not null,
  medvare##23 integer not null,
  medvare##24 integer not null,
  kampkr##1 decimal(11,2) not null,
  kampkr##2 decimal(11,2) not null,
  kampkr##3 decimal(11,2) not null,
  kampkr##4 decimal(11,2) not null,
  kampkr##5 decimal(11,2) not null,
  kampkr##6 decimal(11,2) not null,
  kampkr##7 decimal(11,2) not null,
  kampkr##8 decimal(11,2) not null,
  kampkr##9 decimal(11,2) not null,
  kampkr##10 decimal(11,2) not null,
  kampkr##11 decimal(11,2) not null,
  kampkr##12 decimal(11,2) not null,
  kampkr##13 decimal(11,2) not null,
  kampkr##14 decimal(11,2) not null,
  kampkr##15 decimal(11,2) not null,
  kampkr##16 decimal(11,2) not null,
  kampkr##17 decimal(11,2) not null,
  kampkr##18 decimal(11,2) not null,
  kampkr##19 decimal(11,2) not null,
  kampkr##20 decimal(11,2) not null,
  kampkr##21 decimal(11,2) not null,
  kampkr##22 decimal(11,2) not null,
  kampkr##23 decimal(11,2) not null,
  kampkr##24 decimal(11,2) not null,
  kampant##1 integer not null,
  kampant##2 integer not null,
  kampant##3 integer not null,
  kampant##4 integer not null,
  kampant##5 integer not null,
  kampant##6 integer not null,
  kampant##7 integer not null,
  kampant##8 integer not null,
  kampant##9 integer not null,
  kampant##10 integer not null,
  kampant##11 integer not null,
  kampant##12 integer not null,
  kampant##13 integer not null,
  kampant##14 integer not null,
  kampant##15 integer not null,
  kampant##16 integer not null,
  kampant##17 integer not null,
  kampant##18 integer not null,
  kampant##19 integer not null,
  kampant##20 integer not null,
  kampant##21 integer not null,
  kampant##22 integer not null,
  kampant##23 integer not null,
  kampant##24 integer not null,
  kampvare##1 integer not null,
  kampvare##2 integer not null,
  kampvare##3 integer not null,
  kampvare##4 integer not null,
  kampvare##5 integer not null,
  kampvare##6 integer not null,
  kampvare##7 integer not null,
  kampvare##8 integer not null,
  kampvare##9 integer not null,
  kampvare##10 integer not null,
  kampvare##11 integer not null,
  kampvare##12 integer not null,
  kampvare##13 integer not null,
  kampvare##14 integer not null,
  kampvare##15 integer not null,
  kampvare##16 integer not null,
  kampvare##17 integer not null,
  kampvare##18 integer not null,
  kampvare##19 integer not null,
  kampvare##20 integer not null,
  kampvare##21 integer not null,
  kampvare##22 integer not null,
  kampvare##23 integer not null,
  kampvare##24 integer not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_timedag ON timedag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from timedag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX timedag#_#progress_recid ON timedag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX timedag#_#progress_recid_ident_ ON timedag (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX timedag##timedagidx1 ON timedag (butnr, dato)
go
CREATE INDEX timedag##timedagidx2 ON timedag (dato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'varedag' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varedag
go
CREATE TABLE varedag (
  butnr integer not null,
  ean decimal(13,0) not null,
  dato datetime not null,
  antall decimal(8,3) not null,
  kostpris decimal(8,2) not null,
  mvakr decimal(8,2) not null,
  salgssum decimal(8,2) not null,
  antkamp decimal(8,3) not null,
  kostkamp decimal(8,2) not null,
  mvakamp decimal(8,2) not null,
  salgkamp decimal(8,2) not null,
  antmix decimal(8,3) not null,
  kostmix decimal(8,2) not null,
  mvamix decimal(8,2) not null,
  salgmix decimal(8,2) not null,
  antmed decimal(8,3) not null,
  kostmed decimal(8,2) not null,
  mvamed decimal(8,2) not null,
  salgmed decimal(8,2) not null,
  medrabkr decimal(8,2) not null,
  kunrabkr decimal(8,2) not null,
  perrabkr decimal(8,2) not null,
  genrabkr decimal(8,2) not null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varedag ON varedag for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varedag t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varedag#_#progress_recid ON varedag (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varedag#_#progress_recid_ident_ ON varedag (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX varedag##varedagidx1 ON varedag (butnr, ean, dato)
go
CREATE INDEX varedag##varedagidx2 ON varedag (ean, PROGRESS_RECID)
go
CREATE INDEX varedag##varedagidx3 ON varedag (dato, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'varehierarki' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table varehierarki
go
CREATE TABLE varehierarki (
  avdelingnr integer null,
  avdelingnavn varchar (30) null,
  hg integer null,
  hgbeskr varchar (20) null,
  vg integer null,
  vgbeskr varchar (20) null,
  kost_proc decimal(4,1) null,
  momskod integer null,
  beskrivelse varchar (30) null,
  momsproc decimal(4,2) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_varehierarki ON varehierarki for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from varehierarki t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX varehierarki#_#progress_recid ON varehierarki (PROGRESS_RECID)
go
CREATE UNIQUE INDEX varehierarki#_#progress_recid_ident_ ON varehierarki (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX varehierarki##ahv ON varehierarki (avdelingnr, hg, vg)
go
if (select name from sysobjects 
    where name = 'vpifilhode' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpifilhode
go
CREATE TABLE vpifilhode (
  filnavn varchar (30) null,
  dato datetime null,
  kl varchar (30) null,
  storrelse integer null,
  katalog varchar (40) null,
  filid decimal(15,2) null,
  antlinjer integer null,
  vpifiltype integer null,
  odato datetime null,
  otid integer null,
  oav varchar (15) null,
  edato datetime null,
  etid integer null,
  eav varchar (15) null,
  vpifilstatus integer null,
  ekstvpilevnr integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpifilhode ON vpifilhode for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpifilhode t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpifilhode#_#progress_recid ON vpifilhode (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpifilhode#_#progress_recid_ident_ ON vpifilhode (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vpifilhode##antlinjer ON vpifilhode (antlinjer, PROGRESS_RECID)
go
CREATE INDEX vpifilhode##fildato ON vpifilhode (dato, kl, PROGRESS_RECID)
go
CREATE INDEX vpifilhode##filnavn ON vpifilhode (filnavn, PROGRESS_RECID)
go
CREATE INDEX vpifilhode##katalog ON vpifilhode (katalog, PROGRESS_RECID)
go
CREATE INDEX vpifilhode##storrelse ON vpifilhode (storrelse, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpifilhode##vpifilhode ON vpifilhode (filid)
go
CREATE INDEX vpifilhode##vpifilstatus ON vpifilhode (vpifilstatus, PROGRESS_RECID)
go
CREATE INDEX vpifilhode##vpifiltype ON vpifilhode (vpifiltype, PROGRESS_RECID)
go
CREATE INDEX vpifilhode##vpilev ON vpifilhode (ekstvpilevnr, PROGRESS_RECID)
go
if (select name from sysobjects 
    where name = 'vpifillinje' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpifillinje
go
CREATE TABLE vpifillinje (
  filid decimal(15,2) null,
  tekst varchar (60) null,
  linjenr integer null,
  behandlet tinyint null,
  stortekst varchar (80) null,
  odato datetime null,
  otid integer null,
  oav varchar (15) null,
  edato datetime null,
  etid integer null,
  eav varchar (15) null,
  varenr varchar (20) null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpifillinje ON vpifillinje for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpifillinje t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpifillinje#_#progress_recid ON vpifillinje (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpifillinje#_#progress_recid_ident_ ON vpifillinje (PROGRESS_RECID_IDENT_ )
go
CREATE INDEX vpifillinje##behandlet ON vpifillinje (filid, behandlet, PROGRESS_RECID)
go
CREATE INDEX vpifillinje##tekst ON vpifillinje (filid, tekst, PROGRESS_RECID)
go
CREATE INDEX vpifillinje##varenr ON vpifillinje (varenr, filid, linjenr, PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpifillinje##vpifillinje ON vpifillinje (filid, linjenr)
go
if (select name from sysobjects 
    where name = 'vpifillogg' and type = 'U' and 
    uid = (select uid from sysusers 
            where sid = (select sid from master.dbo.syslogins
                         where UPPER(name) = UPPER('tny'))))
   is not NULL
    drop table vpifillogg
go
CREATE TABLE vpifillogg (
  filid decimal(15,2) null,
  linjenr integer null,
  tekst varchar (50) null,
  gradering integer null,
  PROGRESS_RECID bigint null,
  PROGRESS_RECID_IDENT_ bigint identity
)
go
 
create trigger _TI_vpifillogg ON vpifillogg for insert as
 RAISERROR ('PSC-init',0,1) 
 SET XACT_ABORT ON 
 SET LOCK_TIMEOUT -1 
    if  ( select PROGRESS_RECID from inserted) is NULL
    begin
        update t set PROGRESS_RECID = i.IDENTITYCOL 
         from vpifillogg t  JOIN inserted i ON 
         t.PROGRESS_RECID_IDENT_ = i.PROGRESS_RECID_IDENT_
        select convert (bigint, @@identity)
    end
 SET XACT_ABORT OFF 
 RAISERROR ('PSC-end',0,1) 
go
CREATE INDEX vpifillogg#_#progress_recid ON vpifillogg (PROGRESS_RECID)
go
CREATE UNIQUE INDEX vpifillogg#_#progress_recid_ident_ ON vpifillogg (PROGRESS_RECID_IDENT_ )
go
CREATE UNIQUE INDEX vpifillogg##fillogg ON vpifillogg (filid, linjenr)
go
CREATE INDEX vpifillogg##gradering ON vpifillogg (gradering, PROGRESS_RECID)
go
CREATE INDEX vpifillogg##tekst ON vpifillogg (filid, tekst, PROGRESS_RECID)
go
if (select name from sysobjects where name = '_SEQT_bokforingsid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_bokforingsid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'bokforingsid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'bokforingsid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_bokforingsid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_bokforingsid
go
if (select name from sysobjects where name = '_SEQP_bokforingsid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_bokforingsid
if (select name from sysobjects where name = '_SEQP_REV_bokforingsid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_bokforingsid
go
create table _SEQT_bokforingsid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_bokforingsid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(1, 1, 9223372036854775807, 1, 1) 
 
go
create procedure _SEQP_bokforingsid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_bokforingsid)
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
        update _SEQT_bokforingsid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_bokforingsid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_bokforingsid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_bokforingsid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_bokforingsid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_bokforingsid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_bokforingsid set current_value = @val
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
      update _SEQT_bokforingsid set current_value = @val
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
if (select name from sysobjects where name = '_SEQT_bongid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_bongid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'bongid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'bongid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_bongid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_bongid
go
if (select name from sysobjects where name = '_SEQP_bongid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_bongid
if (select name from sysobjects where name = '_SEQP_REV_bongid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_bongid
go
create table _SEQT_bongid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_bongid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(1, 1, 99999999, 1, 1) 
 
go
create procedure _SEQP_bongid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_bongid)
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
        update _SEQT_bongid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_bongid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_bongid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_bongid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_bongid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_bongid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_bongid set current_value = @val
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
      update _SEQT_bongid set current_value = @val
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
if (select name from sysobjects where name = '_SEQT_datasettid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_datasettid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'datasettid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'datasettid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_datasettid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_datasettid
go
if (select name from sysobjects where name = '_SEQP_datasettid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_datasettid
if (select name from sysobjects where name = '_SEQP_REV_datasettid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_datasettid
go
create table _SEQT_datasettid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_datasettid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(1, 1, 99999999, 1, 1) 
 
go
create procedure _SEQP_datasettid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_datasettid)
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
        update _SEQT_datasettid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_datasettid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_datasettid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_datasettid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_datasettid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_datasettid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_datasettid set current_value = @val
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
      update _SEQT_datasettid set current_value = @val
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
if (select name from sysobjects where name = '_SEQT_skiftid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_skiftid
if (select seq_name from _SEQT_REV_SEQTMGR where seq_name = 'skiftid') 
 is not NULL delete from _SEQT_REV_SEQTMGR where seq_name = 'skiftid'
 if (select seq_name from _SEQT_REV_SEQTMGR) is NULL drop table _SEQT_REV_SEQTMGR 
if (select name from sysobjects where name = '_SEQT_REV_skiftid' and
    uid = (select uid from sysusers 
           where sid = (select sid from master.dbo.syslogins
                        where UPPER(name) = UPPER('tny'))))
is not NULL
drop table _SEQT_REV_skiftid
go
if (select name from sysobjects where name = '_SEQP_skiftid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_skiftid
if (select name from sysobjects where name = '_SEQP_REV_skiftid' and
           uid = (select uid from sysusers 
                  where sid = (select sid from master.dbo.syslogins
                               where UPPER(name) = UPPER('tny'))))
 is not NULL
    drop procedure _SEQP_REV_skiftid
go
create table _SEQT_skiftid ( 
    initial_value    bigint null,
    increment_value  bigint null,
    upper_limit      bigint null,
    current_value    bigint null,
    cycle            bit not null) 
  
insert into _SEQT_skiftid
       (initial_value, increment_value, upper_limit, current_value, cycle)
       values(1, 1, 9223372036854775807, 1, 1) 
 
go
create procedure _SEQP_skiftid (@op int, @val bigint output) as 
begin
    /* 
     * Current-Value function 
     */
   SET XACT_ABORT ON 
    declare @err int 
    if @op = 0 
    begin
        begin transaction
        select @val = (select current_value from _SEQT_skiftid)
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
        update _SEQT_skiftid set initial_value = initial_value
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        select @cur_val = (select current_value from _SEQT_skiftid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @last_val = (select upper_limit from _SEQT_skiftid)
        SET @err = @@error 
        if @err <> 0 goto Err 
        select @inc_val  = (select increment_value from _SEQT_skiftid)
        SET @err = @@error 
        if @err <> 0 goto Err 
 
        /*
         * if the next value will pass the upper limit, then either
         * wrap or return a range violation
         */ 
        if  @inc_val > 0 and @cur_val > @last_val - @inc_val  or @inc_val < 0 and @cur_val < @last_val - @inc_val 
        begin
            if (select cycle from _SEQT_skiftid) = 0 /* non-cycling sequence */
            begin 
                SET @err = @@error 
                if @err <> 0 goto Err 
                select @val = @cur_val
                commit transaction
                return -1
            end
            else 
            BEGIN 
                 select @val = (select initial_value from _SEQT_skiftid)
                 SET @err = @@error 
                 if @err <> 0 goto Err 
            END 
        end
        else 
             select @val = @cur_val + @inc_val
 
 
        update _SEQT_skiftid set current_value = @val
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
      update _SEQT_skiftid set current_value = @val
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
