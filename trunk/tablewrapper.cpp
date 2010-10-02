#include "tablewrapper.h"
#include <QSqlTableModel>
#include <QSqlDatabase>
#include <QTableView>
#include <QSqlRecord>
#include <QSqlRelationalDelegate>
#include <QStringList>
#include <QSpinBox>
#include <QLineEdit>
#include <QComboBox>

TableWrapper::TableWrapper(QSqlDatabase *db, const QString &TableName)
{
    m_model = new QSqlTableModel(0);
    m_model->setTable(TableName);
    m_view = new QTableView(0);
    m_model->setEditStrategy(QSqlTableModel::OnManualSubmit);
    m_view->setModel(m_model);
    m_view->setItemDelegate(new QSqlRelationalDelegate());
    names = new QStringList();
}

bool TableWrapper::selectRow(int num)
{
    this->current_row = num;
    return true;
}

bool TableWrapper::openTable()
{
     m_model->select();
     names->clear();
     for (int i =0; i < m_model->columnCount(); i++)
         names->append(m_model->headerData(i, Qt::Horizontal).toString());
     return true;
}

void TableWrapper::newRow()
{

}

void TableWrapper::deleteRow(int row)
{
        m_model->removeRow(row);
}

bool TableWrapper::setTableValue(int column, const QString &value)
{
    if (current_row >= 0)
        m_model->record(current_row).setValue(column, value);
    else
        m_model->record(rowsCount()).setValue(column, value);
    return true;
}

bool TableWrapper::setTableValue(const QString &FieldName, const QString &value)
{
    if (current_row >= 0)
        m_model->record(current_row).setValue(FieldName, value);
    else
        m_model->record(rowsCount()).setValue(FieldName, value);
    return true;
}

bool TableWrapper::setTableValue(const QString &FieldName, QWidget *control)
{
  if (control->metaObject()->className() ==  QString("QSpinBox"))
    return setTableValue(FieldName, ((QSpinBox*)control)->text());
   if (control->metaObject()->className() ==  QString("QLineEdit"))
       return setTableValue(FieldName, ((QLineEdit*)control)->text());
   if (control->metaObject()->className() ==  QString("QComboBox"))
       return setTableValue(FieldName, ((QComboBox*)control)->currentText());
   return false;
}

bool TableWrapper::setTableValue(int column, QWidget *control)
{
    return setTableValue(names->value(column), control);
}

QString TableWrapper::getTableValue(const QString &FieldName)
{
    if (current_row >= 0)
        return m_model->record(current_row).value(FieldName).toString();
    else
        return m_model->record().value(FieldName).toString();
}

QString TableWrapper::getTableValue(int column)
{
    if (current_row >= 0)
        return m_model->record(current_row).value(column).toString();
    else
        return m_model->record().value(column).toString();
}

void TableWrapper::getTableValue(const QString &FieldName, QWidget *control)
{
    QString text = getTableValue(FieldName);
    if (control->metaObject()->className() ==  QString("QSpinBox")) {
        ((QSpinBox*)control)->setValue(text.toInt());
        return;
    }
    if (control->metaObject()->className() ==  QString("QLineEdit")) {
        ((QLineEdit*)control)->setText(text);
        return;
    }
    if (control->metaObject()->className() ==  QString("QComboBox")) {
        ((QComboBox*)control)->setCurrentIndex(((QComboBox*)control)->findText(text));
        return;
    }
}

void TableWrapper::getTableValue(int column, QWidget *control)
{
    getTableValue(names->value(column), control);
}

void TableWrapper::renameColumn(const QString &OldName, const QString &NewName)
{
    this->setTableColumnName(names->indexOf(OldName), NewName);
}

void TableWrapper::save()
{
    m_model->submitAll();
}

void TableWrapper::showTable(QWidget *parent)
{
    m_view->show();
}

void TableWrapper::setTableDisplayName(const QString &name)
{
    m_view->setWindowTitle(name);
}

void TableWrapper::setTableColumnName(int index, const QString &name)
{
    m_model->setHeaderData(index, Qt::Horizontal, name);
}

void TableWrapper::setColumnHidden(int index, bool hidden)
{
    m_view->setColumnHidden(index, hidden);
}

int TableWrapper::rowsCount()
{
    return m_model->rowCount();
}

TableWrapper::~TableWrapper()
{
    delete m_view;
    delete m_model;
    delete names;
}
