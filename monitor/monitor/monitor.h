#ifndef MONITOR_H
#define MONITOR_H

#include <QMainWindow>

class Monitor : public QMainWindow
{
    Q_OBJECT

public:
    Monitor(QWidget *parent = nullptr);
    ~Monitor();
};
#endif // MONITOR_H
