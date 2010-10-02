#ifndef DIALOG_H
#define DIALOG_H

#include <QDialog>

namespace Ui {
    class Dialog;
}
class QSqlDatabase;
class QSqlTableModel;
class QSettings;

class Dialog : public QDialog
{
    Q_OBJECT

public:
    explicit Dialog(QWidget *parent = 0);
    ~Dialog();
    bool connectDB();
    void createModels();
private:
    Ui::Dialog *ui;
    QSqlDatabase * db;
    QSqlTableModel * main_model;
    QSettings * settings;
private slots:
    void on_pushButton_clicked();
};

#endif // DIALOG_H
