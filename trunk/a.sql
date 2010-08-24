USE [БД заказов];
CREATE USER admin2 FOR LOGIN admin2 
    WITH DEFAULT_SCHEMA = [dbo];
GRANT AUTHENTICATE TO admin2;
GRANT SELECT TO admin2;
GRANT INSERT TO admin2;
GRANT UPDATE TO admin2;
GRANT DELETE TO admin2;
GRANT EXECUTE TO admin2;
exec sp_addrolemember db_datareader, admin2;
exec sp_addrolemember db_datawriter, admin2;
exec sp_addrolemember db_securityadmin, admin2;
exec sp_addrolemember db_backupoperator, admin2;
exec sp_addrolemember db_accessadmin, admin2;
exec sp_addrolemember db_owner, admin2;
