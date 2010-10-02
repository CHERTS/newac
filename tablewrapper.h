#ifndef TABLEWRAPPER_H
#define TABLEWRAPPER_H
#include <QObject>
#include <QWidget>
#include <QString>

class QSqlTableModel;
class QSqlDatabase;
class QTableView;
class QSqlRecord;
class QStringList;
class TableWrapper : public QObject
{
    Q_OBJECT

public:
    TableWrapper(QSqlDatabase * db, const QString &TableName);
    ~TableWrapper();
    bool openTable();
    void showTable(QWidget * parent);
    bool selectRow(int num);
    void selectFirstRow(){selectRow(0);}
    void selectLastRow() {selectRow(rowsCount()-1);}
    int rowsCount();
    bool setTableValue(const QString &FieldName, QWidget * control);
    bool setTableValue(const QString &FieldName, const QString &value);
    bool setTableValue(int column, QWidget * control);
    bool setTableValue(int column, const QString &value);
    void getTableValue(const QString &FieldName, QWidget * control);
    void getTableValue(int column, QWidget * control);
    QString getTableValue(const QString &FieldName);
    QString getTableValue(int column);
    void renameColumn(const QString &OldName, const QString &NewName);
    void save();
    void newRow();
    void deleteRow(int row);
    void setTableDisplayName(const QString &name);
    void setTableColumnName(int index, const QString &name);
    void setColumnHidden(int index, bool hidden);
private:
    QSqlTableModel * m_model;
    QTableView * m_view;
    int current_row;
    QStringList * names;
};

#endif // TABLEWRAPPER_H
