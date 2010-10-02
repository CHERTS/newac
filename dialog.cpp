#include <iostream>
#include "dialog.h"
#include "ui_dialog.h"
#include <QMessageBox>
#include <QSqlDatabase>
#include <QSqlTableModel>
#include <QSqlError>
#include <QTableView>
#include <QSettings>
#include <QSqlQuery>
#include <QItemDelegate>
#include <QSqlRelationalDelegate>
#include <QListView>

using namespace std;
Dialog::Dialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Dialog)
{
    ui->setupUi(this);
}

Dialog::~Dialog()
{
    delete ui;
}

void Dialog::on_pushButton_clicked()
{
    connectDB();
    createModels();
}

bool Dialog::connectDB()
{
    //QSettings::setDefaultFormat(QSettings::IniFormat);
    settings = new QSettings("NPPGamma", "Junkie", this);
//    QMessageBox mb(0);
//    mb.setText(settings->fileName());
//    mb.exec();


    QString server = settings->value("server").toString();
    QString database = settings->value("database").toString();
    QString user = settings->value("user").toString();
    QString password = settings->value("password").toString();
    /*QString server = settings->value("server").toString();
    QString database = settings->value("database").toString();
    QString user = settings->value("user").toString();
    QString password = settings->value("password").toString();*/

    settings->value("server", server);
    settings->value("database", database);
    settings->value("user", user);
    settings->value("password", password);
    settings->sync();
    if (settings->status() != QSettings::NoError){
        QMessageBox mb(0);
        mb.setText(QString("Settings error %1").arg(settings->status()));
        mb.exec();
    }
    QString connect_string = QString("Driver={SQL SERVER};Server=%1;Database={%2};UID=%3;PWD=%4").arg(server, database, user, password);
    db = new QSqlDatabase(QSqlDatabase::addDatabase("QODBC"));
    if (!db->isValid()) {
        QMessageBox mb(0);
        mb.setText(db->lastError().text());
        mb.exec();
        return false;
    }
//    db->setHostName("WIN-IY8J8WSLK8O");

    //db->setDatabaseName(QString::fromLocal8Bit("Driver={SQL SERVER};Server=WIN-IY8J8WSLK8O;Database={БД заказов};UID=sa;PWD=Qx653223"));
    db->setDatabaseName(connect_string);
//    db->setUserName("sa");
   // db->setConnectOptions();
    //db->setPassword("Qx653223");
    if (!db->open())  {
        QMessageBox mb(0);
        mb.setText(db->lastError().text());
        mb.exec();
        return false;
    }
    return true;
}

void Dialog::createModels()
{
    main_model = new QSqlTableModel(0);
    //QSqlQuery q(QString::fromLocal8Bit("SELECT * from [t_konstr_obj]"), *db);
    //QString q = QString::fromLocal8Bit("SELECT * from [t_konstr_obj]");
    main_model->setTable("t_konstr_obj");
    QTableView * view = new QTableView(0);
    main_model->setEditStrategy(QSqlTableModel::OnManualSubmit);
    main_model->select();
    view->setWindowTitle(QString::fromLocal8Bit("Стены и перекрытия"));
    view->setModel(main_model);
    view->setItemDelegate(new QSqlRelationalDelegate());
    view->show();

    //view->setWindowTitle(QObject::trUtf8("Музыкальный каталог"));
    //this->ui->textEdit->setText("kkk");
}
