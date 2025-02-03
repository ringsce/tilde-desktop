#include <QApplication>
#include <QWidget>
#include <QVBoxLayout>
#include <QTextEdit>
#include <QPushButton>
#include <QFile>
#include <QTimer>
#include <QProcess>
#include <QDebug>
#include <QDir>

class AppMonitor : public QWidget {
    Q_OBJECT

public:
    AppMonitor(QWidget *parent = nullptr) : QWidget(parent) {
        // Set up the UI
        QVBoxLayout *layout = new QVBoxLayout(this);

        textEdit = new QTextEdit(this);
        textEdit->setReadOnly(true);
        layout->addWidget(textEdit);

        QPushButton *refreshButton = new QPushButton("Refresh", this);
        layout->addWidget(refreshButton);

        // Connect the refresh button to update the log display
        connect(refreshButton, &QPushButton::clicked, this, &AppMonitor::updateLogDisplay);

        // Set up a timer to periodically update the log display
        QTimer *timer = new QTimer(this);
        connect(timer, &QTimer::timeout, this, &AppMonitor::updateLogDisplay);
        timer->start(1000); // Update every 1 second

        // Start the monitor script
        startMonitorScript();
    }

    ~AppMonitor() {
        // Stop the monitor script when the application exits
        if (process && process->state() == QProcess::Running) {
            process->terminate();
            process->waitForFinished();
        }
    }

private slots:
    void updateLogDisplay() {
        QFile logFile(QDir::homePath() + "/app_open_times.txt");
        if (logFile.open(QIODevice::ReadOnly | QIODevice::Text)) {
            QTextStream in(&logFile);
            textEdit->setPlainText(in.readAll());
            logFile.close();
        } else {
            textEdit->setPlainText("Log file not found or cannot be opened.");
        }
    }

private:
    void startMonitorScript() {
        process = new QProcess(this);
        QString scriptPath = QDir::homePath() + "/monitor_apps.sh";

        if (QFile::exists(scriptPath)) {
            process->start(scriptPath);
            if (!process->waitForStarted()) {
                qWarning() << "Failed to start the monitor script.";
            }
        } else {
            qWarning() << "Monitor script not found at" << scriptPath;
        }
    }

    QTextEdit *textEdit;
    QProcess *process = nullptr;
};

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    AppMonitor window;
    window.setWindowTitle("Application Monitor");
    window.resize(400, 300);
    window.show();

    return app.exec();
}

#include "main.moc"
