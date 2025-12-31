#include <QApplication>
#include <QMainWindow>
#include <QWidget>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QLabel>
#include <QTextEdit>
#include <QProgressBar>
#include <QTimer>
#include <QPainter>
#include <QVector>
#include <QProcess>
#include <QFile>
#include <QTextStream>
#include <QStackedWidget>
#include <cmath>

#ifdef Q_OS_WIN
#include <windows.h>
#endif

#ifdef Q_OS_LINUX
#include <sys/types.h>
#include <sys/sysinfo.h>
#endif

class CPUGraphWidget : public QWidget {
    Q_OBJECT
private:
    QVector<int> cpuHistory;
    int maxHistory;

public:
    CPUGraphWidget(QWidget *parent = nullptr) : QWidget(parent), maxHistory(100) {
        cpuHistory.resize(maxHistory);
        cpuHistory.fill(0);
        setMinimumHeight(300);
    }

    void addCPUValue(int value) {
        cpuHistory.pop_front();
        cpuHistory.append(qBound(0, value, 100));
        update();
    }

protected:
    void paintEvent(QPaintEvent *) override {
        QPainter painter(this);
        painter.setRenderHint(QPainter::Antialiasing);

        // Background
        painter.fillRect(rect(), Qt::black);

        // Grid lines
        painter.setPen(QPen(QColor(40, 40, 40), 1));
        for (int i = 0; i <= 4; i++) {
            int y = height() * i / 4;
            painter.drawLine(0, y, width(), y);
        }

        // CPU line graph
        if (cpuHistory.size() < 2) return;

        painter.setPen(QPen(Qt::green, 2));
        
        double xScale = (double)width() / (maxHistory - 1);
        double yScale = (double)height() / 100.0;

        for (int i = 0; i < cpuHistory.size() - 1; i++) {
            int x1 = i * xScale;
            int y1 = height() - (cpuHistory[i] * yScale);
            int x2 = (i + 1) * xScale;
            int y2 = height() - (cpuHistory[i + 1] * yScale);
            painter.drawLine(x1, y1, x2, y2);
        }

        // Labels
        painter.setPen(Qt::green);
        painter.drawText(10, 20, "CPU History Graph");
        
        painter.setPen(Qt::white);
        QFont font = painter.font();
        font.setPointSize(8);
        painter.setFont(font);
        painter.drawText(10, height() - 10, "0%");
        painter.drawText(10, 20, "100%");
        painter.drawText(width() - 60, height() - 10, "Time →");
    }
};

class SystemMonitor : public QObject {
    Q_OBJECT
private:
#ifdef Q_OS_WIN
    ULONGLONG lastIdleTime, lastKernelTime, lastUserTime;
#else
    unsigned long long lastTotalTime, lastIdleTime;
#endif

public:
    SystemMonitor(QObject *parent = nullptr) : QObject(parent) {
#ifdef Q_OS_WIN
        lastIdleTime = 0;
        lastKernelTime = 0;
        lastUserTime = 0;
#else
        lastTotalTime = 0;
        lastIdleTime = 0;
#endif
    }

    int getCPUUsage() {
#ifdef Q_OS_WIN
        FILETIME idleTime, kernelTime, userTime;
        if (!GetSystemTimes(&idleTime, &kernelTime, &userTime)) {
            return 0;
        }

        auto fileTimeToULongLong = [](const FILETIME& ft) -> ULONGLONG {
            return ((ULONGLONG)ft.dwHighDateTime << 32) | ft.dwLowDateTime;
        };

        ULONGLONG sysIdle = fileTimeToULongLong(idleTime);
        ULONGLONG sysKernel = fileTimeToULongLong(kernelTime);
        ULONGLONG sysUser = fileTimeToULongLong(userTime);

        if (lastIdleTime != 0) {
            ULONGLONG deltaIdle = sysIdle - lastIdleTime;
            ULONGLONG deltaKernel = sysKernel - lastKernelTime;
            ULONGLONG deltaUser = sysUser - lastUserTime;
            ULONGLONG deltaTotal = deltaKernel + deltaUser;

            if (deltaTotal > 0) {
                int usage = (int)(100.0 - (deltaIdle * 100.0 / deltaTotal));
                lastIdleTime = sysIdle;
                lastKernelTime = sysKernel;
                lastUserTime = sysUser;
                return qBound(0, usage, 100);
            }
        }

        lastIdleTime = sysIdle;
        lastKernelTime = sysKernel;
        lastUserTime = sysUser;
        return 0;
#else
        // Unix/Linux/macOS
        QFile file("/proc/stat");
        if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
            return 0;
        }

        QTextStream in(&file);
        QString line = in.readLine();
        file.close();

        QStringList parts = line.split(' ', Qt::SkipEmptyParts);
        if (parts.size() < 5 || parts[0] != "cpu") {
            return 0;
        }

        unsigned long long user = parts[1].toULongLong();
        unsigned long long nice = parts[2].toULongLong();
        unsigned long long system = parts[3].toULongLong();
        unsigned long long idle = parts[4].toULongLong();
        unsigned long long iowait = parts.size() > 5 ? parts[5].toULongLong() : 0;
        unsigned long long irq = parts.size() > 6 ? parts[6].toULongLong() : 0;
        unsigned long long softirq = parts.size() > 7 ? parts[7].toULongLong() : 0;

        unsigned long long total = user + nice + system + idle + iowait + irq + softirq;
        unsigned long long currentIdle = idle;

        if (lastTotalTime > 0) {
            unsigned long long totalDelta = total - lastTotalTime;
            unsigned long long idleDelta = currentIdle - lastIdleTime;

            if (totalDelta > 0) {
                int usage = (int)(100.0 * (1.0 - ((double)idleDelta / totalDelta)));
                lastTotalTime = total;
                lastIdleTime = currentIdle;
                return qBound(0, usage, 100);
            }
        }

        lastTotalTime = total;
        lastIdleTime = currentIdle;
        return 0;
#endif
    }

    int getMemoryUsage() {
#ifdef Q_OS_WIN
        MEMORYSTATUSEX memStatus;
        memStatus.dwLength = sizeof(memStatus);
        if (GlobalMemoryStatusEx(&memStatus)) {
            return (int)memStatus.dwMemoryLoad;
        }
        return 0;
#else
        // Unix/Linux/macOS
        QFile file("/proc/meminfo");
        if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
            return 50;
        }

        QTextStream in(&file);
        unsigned long long memTotal = 0, memAvailable = 0, memFree = 0, buffers = 0, cached = 0;

        while (!in.atEnd()) {
            QString line = in.readLine();
            QStringList parts = line.split(' ', Qt::SkipEmptyParts);
            if (parts.size() < 2) continue;

            if (parts[0] == "MemTotal:")
                memTotal = parts[1].toULongLong();
            else if (parts[0] == "MemAvailable:")
                memAvailable = parts[1].toULongLong();
            else if (parts[0] == "MemFree:")
                memFree = parts[1].toULongLong();
            else if (parts[0] == "Buffers:")
                buffers = parts[1].toULongLong();
            else if (parts[0] == "Cached:")
                cached = parts[1].toULongLong();
        }
        file.close();

        if (memTotal > 0) {
            if (memAvailable > 0) {
                return (int)(100.0 * (1.0 - ((double)memAvailable / memTotal)));
            } else {
                unsigned long long usedMem = memTotal - memFree - buffers - cached;
                return (int)(100.0 * ((double)usedMem / memTotal));
            }
        }
        return 50;
#endif
    }

    QString getMemoryStats() {
#ifdef Q_OS_WIN
        MEMORYSTATUSEX memStatus;
        memStatus.dwLength = sizeof(memStatus);
        if (GlobalMemoryStatusEx(&memStatus)) {
            double totalGB = memStatus.ullTotalPhys / (1024.0 * 1024.0 * 1024.0);
            double usedGB = (memStatus.ullTotalPhys - memStatus.ullAvailPhys) / (1024.0 * 1024.0 * 1024.0);
            return QString("%1 GB / %2 GB").arg(usedGB, 0, 'f', 1).arg(totalGB, 0, 'f', 1);
        }
        return "N/A";
#else
        // Unix/Linux/macOS
        QFile file("/proc/meminfo");
        if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
            return "N/A";
        }

        QTextStream in(&file);
        unsigned long long memTotal = 0, memAvailable = 0;

        while (!in.atEnd()) {
            QString line = in.readLine();
            QStringList parts = line.split(' ', Qt::SkipEmptyParts);
            if (parts.size() < 2) continue;

            if (parts[0] == "MemTotal:")
                memTotal = parts[1].toULongLong();
            else if (parts[0] == "MemAvailable:")
                memAvailable = parts[1].toULongLong();
        }
        file.close();

        if (memTotal > 0) {
            double totalGB = memTotal / (1024.0 * 1024.0);
            double usedGB = (memTotal - memAvailable) / (1024.0 * 1024.0);
            return QString("%1 GB / %2 GB").arg(usedGB, 0, 'f', 1).arg(totalGB, 0, 'f', 1);
        }
        return "N/A";
#endif
    }

    QStringList getProcessList() {
        QStringList processes;
        QProcess process;

#ifdef Q_OS_WIN
        process.start("tasklist", QStringList());
#else
        // Unix/Linux/macOS
        process.start("/bin/ps", QStringList() << "aux");
#endif

        if (!process.waitForFinished(5000)) {
            processes << "Error: Process list timeout";
            return processes;
        }

        QString output = process.readAllStandardOutput();
        QStringList lines = output.split('\n');

        for (const QString& line : lines) {
            if (line.trimmed().isEmpty()) continue;

#ifndef Q_OS_WIN
            // Filter out kernel threads on Unix/Linux/macOS
            if (line.contains("[kworker") || line.contains("[migration") ||
                line.contains("[ksoftirqd") || line.contains("[rcu_") ||
                line.contains("[watchdog") || line.contains("[cpuhp") ||
                line.contains("[kthreadd")) {
                continue;
            }
#endif
            processes << line;
            if (processes.size() >= 200) break;
        }

        return processes;
    }
};

class MainWindow : public QMainWindow {
    Q_OBJECT
private:
    QStackedWidget *stackedWidget;
    QWidget *overviewWidget;
    QWidget *processesWidget;
    
    QLabel *cpuLabel;
    QLabel *cpuPercentLabel;
    QProgressBar *cpuProgressBar;
    QLabel *memoryLabel;
    QLabel *memoryPercentLabel;
    QLabel *memoryStatsLabel;
    QProgressBar *memoryProgressBar;
    CPUGraphWidget *cpuGraph;
    
    QTextEdit *processTextEdit;
    
    QPushButton *overviewButton;
    QPushButton *processesButton;
    QPushButton *refreshButton;
    
    SystemMonitor *monitor;
    QTimer *updateTimer;

public:
    MainWindow(QWidget *parent = nullptr) : QMainWindow(parent) {
        setWindowTitle("System Activity Monitor");
        resize(900, 700);

        monitor = new SystemMonitor(this);

        // Central widget
        QWidget *centralWidget = new QWidget(this);
        setCentralWidget(centralWidget);

        QVBoxLayout *mainLayout = new QVBoxLayout(centralWidget);
        mainLayout->setContentsMargins(10, 10, 10, 10);

        // Top button panel
        QHBoxLayout *buttonLayout = new QHBoxLayout();
        overviewButton = new QPushButton("Overview", this);
        processesButton = new QPushButton("Processes", this);
        refreshButton = new QPushButton("Refresh", this);
        
        overviewButton->setStyleSheet("font-weight: bold; padding: 10px;");
        processesButton->setStyleSheet("padding: 10px;");
        refreshButton->setStyleSheet("padding: 10px;");
        
        buttonLayout->addWidget(overviewButton);
        buttonLayout->addWidget(processesButton);
        buttonLayout->addWidget(refreshButton);
        buttonLayout->addStretch();

        mainLayout->addLayout(buttonLayout);

        // Stacked widget for different views
        stackedWidget = new QStackedWidget(this);
        mainLayout->addWidget(stackedWidget);

        // Overview page
        createOverviewPage();
        
        // Processes page
        createProcessesPage();

        // Connect buttons
        connect(overviewButton, &QPushButton::clicked, this, &MainWindow::showOverview);
        connect(processesButton, &QPushButton::clicked, this, &MainWindow::showProcesses);
        connect(refreshButton, &QPushButton::clicked, this, &MainWindow::refreshData);

        // Setup timer for auto-update
        updateTimer = new QTimer(this);
        connect(updateTimer, &QTimer::timeout, this, &MainWindow::updateStats);
        updateTimer->start(2000); // Update every 2 seconds

        // Initial update
        monitor->getCPUUsage(); // First call to establish baseline
        QTimer::singleShot(500, this, &MainWindow::updateStats);
        
        showOverview();
    }

private:
    void createOverviewPage() {
        overviewWidget = new QWidget();
        QVBoxLayout *layout = new QVBoxLayout(overviewWidget);
        layout->setSpacing(15);

        // CPU Usage section
        QWidget *cpuWidget = new QWidget();
        cpuWidget->setStyleSheet("QWidget { background-color: #f0f0f0; border-radius: 5px; padding: 15px; }");
        QVBoxLayout *cpuLayout = new QVBoxLayout(cpuWidget);
        
        QLabel *cpuTitle = new QLabel("CPU Usage");
        cpuTitle->setStyleSheet("font-size: 16px; font-weight: bold; color: #333;");
        cpuLayout->addWidget(cpuTitle);
        
        QHBoxLayout *cpuInfoLayout = new QHBoxLayout();
        cpuLabel = new QLabel("Current CPU Usage:");
        cpuLabel->setStyleSheet("font-size: 12px;");
        cpuPercentLabel = new QLabel("0%");
        cpuPercentLabel->setStyleSheet("font-size: 14px; font-weight: bold; color: #0066cc;");
        cpuInfoLayout->addWidget(cpuLabel);
        cpuInfoLayout->addWidget(cpuPercentLabel);
        cpuInfoLayout->addStretch();
        cpuLayout->addLayout(cpuInfoLayout);
        
        cpuProgressBar = new QProgressBar();
        cpuProgressBar->setMinimum(0);
        cpuProgressBar->setMaximum(100);
        cpuProgressBar->setTextVisible(false);
        cpuProgressBar->setStyleSheet(R"(
            QProgressBar {
                border: 2px solid #ccc;
                border-radius: 5px;
                text-align: center;
                height: 25px;
            }
            QProgressBar::chunk {
                background-color: #4CAF50;
                border-radius: 3px;
            }
        )");
        cpuLayout->addWidget(cpuProgressBar);
        
        layout->addWidget(cpuWidget);

        // Memory Usage section
        QWidget *memWidget = new QWidget();
        memWidget->setStyleSheet("QWidget { background-color: #f0f0f0; border-radius: 5px; padding: 15px; }");
        QVBoxLayout *memLayout = new QVBoxLayout(memWidget);
        
        QLabel *memTitle = new QLabel("Memory Usage");
        memTitle->setStyleSheet("font-size: 16px; font-weight: bold; color: #333;");
        memLayout->addWidget(memTitle);
        
        QHBoxLayout *memInfoLayout = new QHBoxLayout();
        memoryLabel = new QLabel("Current Memory Usage:");
        memoryLabel->setStyleSheet("font-size: 12px;");
        memoryPercentLabel = new QLabel("0%");
        memoryPercentLabel->setStyleSheet("font-size: 14px; font-weight: bold; color: #0066cc;");
        memInfoLayout->addWidget(memoryLabel);
        memInfoLayout->addWidget(memoryPercentLabel);
        memInfoLayout->addStretch();
        memLayout->addLayout(memInfoLayout);
        
        memoryProgressBar = new QProgressBar();
        memoryProgressBar->setMinimum(0);
        memoryProgressBar->setMaximum(100);
        memoryProgressBar->setTextVisible(false);
        memoryProgressBar->setStyleSheet(R"(
            QProgressBar {
                border: 2px solid #ccc;
                border-radius: 5px;
                text-align: center;
                height: 25px;
            }
            QProgressBar::chunk {
                background-color: #2196F3;
                border-radius: 3px;
            }
        )");
        memLayout->addWidget(memoryProgressBar);
        
        memoryStatsLabel = new QLabel("0.0 GB / 0.0 GB");
        memoryStatsLabel->setStyleSheet("font-size: 11px; color: #666; margin-top: 5px;");
        memLayout->addWidget(memoryStatsLabel);
        
        layout->addWidget(memWidget);

        // CPU History Graph
        QWidget *graphWidget = new QWidget();
        graphWidget->setStyleSheet("QWidget { background-color: #f0f0f0; border-radius: 5px; padding: 15px; }");
        QVBoxLayout *graphLayout = new QVBoxLayout(graphWidget);
        
        QLabel *graphTitle = new QLabel("CPU History Graph");
        graphTitle->setStyleSheet("font-size: 16px; font-weight: bold; color: #333;");
        graphLayout->addWidget(graphTitle);
        
        cpuGraph = new CPUGraphWidget();
        graphLayout->addWidget(cpuGraph);
        
        layout->addWidget(graphWidget);

        stackedWidget->addWidget(overviewWidget);
    }

    void createProcessesPage() {
        processesWidget = new QWidget();
        QVBoxLayout *layout = new QVBoxLayout(processesWidget);

        QLabel *title = new QLabel("Running Processes");
        title->setStyleSheet("font-size: 18px; font-weight: bold; color: #333; padding: 10px;");
        layout->addWidget(title);

        processTextEdit = new QTextEdit();
        processTextEdit->setReadOnly(true);
        processTextEdit->setFont(QFont("Courier New", 9));
        processTextEdit->setStyleSheet("background-color: white; border: 1px solid #ccc;");
        layout->addWidget(processTextEdit);

        stackedWidget->addWidget(processesWidget);
    }

private slots:
    void showOverview() {
        stackedWidget->setCurrentWidget(overviewWidget);
        overviewButton->setStyleSheet("font-weight: bold; padding: 10px;");
        processesButton->setStyleSheet("padding: 10px;");
    }

    void showProcesses() {
        stackedWidget->setCurrentWidget(processesWidget);
        overviewButton->setStyleSheet("padding: 10px;");
        processesButton->setStyleSheet("font-weight: bold; padding: 10px;");
        refreshProcessList();
    }

    void refreshData() {
        if (stackedWidget->currentWidget() == processesWidget) {
            refreshProcessList();
        } else {
            updateStats();
        }
    }

    void updateStats() {
        int cpuUsage = monitor->getCPUUsage();
        int memUsage = monitor->getMemoryUsage();
        QString memStats = monitor->getMemoryStats();

        cpuPercentLabel->setText(QString("%1%").arg(cpuUsage));
        cpuProgressBar->setValue(cpuUsage);
        
        // Update progress bar color based on usage
        QString cpuColor = cpuUsage > 75 ? "#f44336" : (cpuUsage > 50 ? "#ff9800" : "#4CAF50");
        cpuProgressBar->setStyleSheet(QString(R"(
            QProgressBar {
                border: 2px solid #ccc;
                border-radius: 5px;
                text-align: center;
                height: 25px;
            }
            QProgressBar::chunk {
                background-color: %1;
                border-radius: 3px;
            }
        )").arg(cpuColor));

        memoryPercentLabel->setText(QString("%1%").arg(memUsage));
        memoryProgressBar->setValue(memUsage);
        memoryStatsLabel->setText(memStats);
        
        QString memColor = memUsage > 75 ? "#f44336" : (memUsage > 50 ? "#ff9800" : "#2196F3");
        memoryProgressBar->setStyleSheet(QString(R"(
            QProgressBar {
                border: 2px solid #ccc;
                border-radius: 5px;
                text-align: center;
                height: 25px;
            }
            QProgressBar::chunk {
                background-color: %1;
                border-radius: 3px;
            }
        )").arg(memColor));

        cpuGraph->addCPUValue(cpuUsage);
    }

    void refreshProcessList() {
        processTextEdit->setPlainText("Loading processes...");
        QApplication::processEvents();

        QStringList processes = monitor->getProcessList();
        processTextEdit->setPlainText(processes.join('\n'));
    }
};

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    MainWindow window;
    window.show();

    return app.exec();
}

#include "main.moc"
