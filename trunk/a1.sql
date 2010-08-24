USE master;
GO
DROP DATABASE [БД заказов]
GO
RESTORE FILELISTONLY
FROM DISK = 'C:\gccfd\БД заказов 2000.bak'
GO
----Make Database to single user Mode
--ALTER DATABASE [БД заказов 2000.bak
--SET SINGLE_USER WITH
--ROLLBACK IMMEDIATE

----Restore Database
RESTORE DATABASE [БД заказов]
FROM DISK = 'C:\gccfd\БД заказов 2000.bak'
--WITH MOVE 'YourMDFLogicalName' TO 'D:DataYourMDFFile.mdf',
--MOVE 'YourLDFLogicalName' TO 'D:DataYourLDFFile.ldf'

/*If there is no error in statement before database will be in multiuser
mode.
If error occurs please execute following command it will convert
database in multi user.*/
ALTER DATABASE [БД заказов] SET MULTI_USER
GO
CREATE LOGIN admin WITH PASSWORD = 'letmein', DEFAULT_DATABASE = [БД заказов], DEFAULT_LANGUAGE = Russian 
GO
--CREATE USER admin FOR LOGIN admin 
--    WITH DEFAULT_SCHEMA = [dbo];
GO
exec sp_addsrvrolemember admin, securityadmin;
exec sp_addsrvrolemember admin, setupadmin;
exec sp_addsrvrolemember admin, serveradmin;
exec sp_addsrvrolemember admin, sysadmin;
--exec sp_addrolemember db_datareader, admin;
--exec sp_addrolemember db_datawriter, admin;
--exec sp_addrolemember db_securityadmin, admin;
--exec sp_addrolemember db_accessadmin, admin;
--exec sp_addrolemember db_owner, admin;
GO
USE [БД заказов]
GO
CREATE USER admin FOR LOGIN admin 
    WITH DEFAULT_SCHEMA = [dbo];
GRANT AUTHENTICATE TO admin;
GRANT SELECT TO admin;
GRANT INSERT TO admin;
GRANT UPDATE TO admin;
GRANT DELETE TO admin;
GRANT EXECUTE TO admin;
exec sp_addrolemember db_datareader, admin;
exec sp_addrolemember db_datawriter, admin;
exec sp_addrolemember db_securityadmin, admin;
exec sp_addrolemember db_backupoperator, admin;
exec sp_addrolemember db_accessadmin, admin;
exec sp_addrolemember db_owner, admin;
GO
