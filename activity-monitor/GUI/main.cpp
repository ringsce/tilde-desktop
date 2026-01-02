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

#ifdef Q_OS_MACOS
#include <mach/mach.h>
#include <mach/mach_host.h>
#include <mach/host_info.h>
#include <sys/sysctl.h>
#include <sys/types.h>
#include <libproc.h>
#include <IOKit/IOKitLib.h>
#include <CoreFoundation/CoreFoundation.h>
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

class GPUGraphWidget : public QWidget {
    Q_OBJECT
private:
    QVector<int> gpuHistory;
    int maxHistory;

public:
    GPUGraphWidget(QWidget *parent = nullptr) : QWidget(parent), maxHistory(100) {
        gpuHistory.resize(maxHistory);
        gpuHistory.fill(0);
        setMinimumHeight(300);
    }

    void addGPUValue(int value) {
        gpuHistory.pop_front();
        gpuHistory.append(qBound(0, value, 100));
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

        // GPU line graph
        if (gpuHistory.size() < 2) return;

        painter.setPen(QPen(QColor(255, 165, 0), 2)); // Orange
        
        double xScale = (double)width() / (maxHistory - 1);
        double yScale = (double)height() / 100.0;

        for (int i = 0; i < gpuHistory.size() - 1; i++) {
            int x1 = i * xScale;
            int y1 = height() - (gpuHistory[i] * yScale);
            int x2 = (i + 1) * xScale;
            int y2 = height() - (gpuHistory[i + 1] * yScale);
            painter.drawLine(x1, y1, x2, y2);
        }

        // Labels
        painter.setPen(QColor(255, 165, 0));
        painter.drawText(10, 20, "GPU History Graph");
        
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
#endif
#ifdef Q_OS_MACOS
    unsigned long long lastTotalTicks, lastIdleTicks;
#endif
#ifdef Q_OS_LINUX
    unsigned long long lastTotalTime, lastIdleTime;
#endif

public:
    SystemMonitor(QObject *parent = nullptr) : QObject(parent) {
#ifdef Q_OS_WIN
        lastIdleTime = 0;
        lastKernelTime = 0;
        lastUserTime = 0;
#endif
#ifdef Q_OS_MACOS
        lastTotalTicks = 0;
        lastIdleTicks = 0;
#endif
#ifdef Q_OS_LINUX
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
#endif

#ifdef Q_OS_MACOS
        host_cpu_load_info_data_t cpuInfo;
        mach_msg_type_number_t count = HOST_CPU_LOAD_INFO_COUNT;
        
        if (host_statistics(mach_host_self(), HOST_CPU_LOAD_INFO,
                           (host_info_t)&cpuInfo, &count) != KERN_SUCCESS) {
            return 0;
        }

        unsigned long long totalTicks = 0;
        for (int i = 0; i < CPU_STATE_MAX; i++) {
            totalTicks += cpuInfo.cpu_ticks[i];
        }
        unsigned long long idleTicks = cpuInfo.cpu_ticks[CPU_STATE_IDLE];

        if (lastTotalTicks > 0) {
            unsigned long long totalDelta = totalTicks - lastTotalTicks;
            unsigned long long idleDelta = idleTicks - lastIdleTicks;

            if (totalDelta > 0) {
                int usage = (int)(100.0 * (1.0 - ((double)idleDelta / totalDelta)));
                lastTotalTicks = totalTicks;
                lastIdleTicks = idleTicks;
                return qBound(0, usage, 100);
            }
        }

        lastTotalTicks = totalTicks;
        lastIdleTicks = idleTicks;
        return 0;
#endif

#ifdef Q_OS_LINUX
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
        return 0;
    }

    int getMemoryUsage() {
#ifdef Q_OS_WIN
        MEMORYSTATUSEX memStatus;
        memStatus.dwLength = sizeof(memStatus);
        if (GlobalMemoryStatusEx(&memStatus)) {
            return (int)memStatus.dwMemoryLoad;
        }
        return 0;
#endif

#ifdef Q_OS_MACOS
        vm_size_t page_size;
        vm_statistics64_data_t vm_stats;
        mach_msg_type_number_t count = HOST_VM_INFO64_COUNT;
        
        if (host_page_size(mach_host_self(), &page_size) != KERN_SUCCESS) {
            return 0;
        }
        
        if (host_statistics64(mach_host_self(), HOST_VM_INFO64,
                             (host_info64_t)&vm_stats, &count) != KERN_SUCCESS) {
            return 0;
        }

        // Get physical memory size
        int mib[2] = {CTL_HW, HW_MEMSIZE};
        uint64_t physical_memory;
        size_t length = sizeof(physical_memory);
        if (sysctl(mib, 2, &physical_memory, &length, NULL, 0) != 0) {
            return 0;
        }

        // Calculate used memory
        uint64_t used_memory = ((uint64_t)vm_stats.active_count +
                                (uint64_t)vm_stats.inactive_count +
                                (uint64_t)vm_stats.wire_count) * (uint64_t)page_size;

        if (physical_memory > 0) {
            return (int)((double)used_memory / physical_memory * 100.0);
        }
        return 0;
#endif

#ifdef Q_OS_LINUX
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
        return 0;
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
#endif

#ifdef Q_OS_MACOS
        vm_size_t page_size;
        vm_statistics64_data_t vm_stats;
        mach_msg_type_number_t count = HOST_VM_INFO64_COUNT;
        
        if (host_page_size(mach_host_self(), &page_size) != KERN_SUCCESS) {
            return "N/A";
        }
        
        if (host_statistics64(mach_host_self(), HOST_VM_INFO64,
                             (host_info64_t)&vm_stats, &count) != KERN_SUCCESS) {
            return "N/A";
        }

        // Get physical memory size
        int mib[2] = {CTL_HW, HW_MEMSIZE};
        uint64_t physical_memory;
        size_t length = sizeof(physical_memory);
        if (sysctl(mib, 2, &physical_memory, &length, NULL, 0) != 0) {
            return "N/A";
        }

        // Calculate used memory
        uint64_t used_memory = ((uint64_t)vm_stats.active_count +
                                (uint64_t)vm_stats.inactive_count +
                                (uint64_t)vm_stats.wire_count) * (uint64_t)page_size;

        double totalGB = physical_memory / (1024.0 * 1024.0 * 1024.0);
        double usedGB = used_memory / (1024.0 * 1024.0 * 1024.0);
        
        return QString("%1 GB / %2 GB").arg(usedGB, 0, 'f', 1).arg(totalGB, 0, 'f', 1);
#endif

#ifdef Q_OS_LINUX
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
        return "N/A";
    }

    int getGPUUsage() {
#ifdef Q_OS_MACOS
        // Get IOService for AGXAccelerator (Apple Silicon GPU)
        io_iterator_t iterator;
        kern_return_t result = IOServiceGetMatchingServices(
            kIOMasterPortDefault,
            IOServiceMatching("AGXAccelerator"),
            &iterator
        );

        if (result != kIOReturnSuccess) {
            // Try for AMD/Intel GPU on older Macs
            result = IOServiceGetMatchingServices(
                kIOMasterPortDefault,
                IOServiceMatching("IOAccelerator"),
                &iterator
            );
            if (result != kIOReturnSuccess) {
                return 0;
            }
        }

        io_registry_entry_t service;
        int gpuUsage = 0;
        
        while ((service = IOIteratorNext(iterator))) {
            CFMutableDictionaryRef properties = NULL;
            result = IORegistryEntryCreateCFProperties(
                service,
                &properties,
                kCFAllocatorDefault,
                kNilOptions
            );

            if (result == kIOReturnSuccess && properties) {
                // Try to get GPU utilization
                CFNumberRef utilization = (CFNumberRef)CFDictionaryGetValue(
                    properties,
                    CFSTR("PerformanceStatistics")
                );
                
                if (utilization) {
                    // This is a simplified approach
                    // Real GPU usage requires parsing PerformanceStatistics dictionary
                    CFDictionaryRef perfStats = (CFDictionaryRef)CFDictionaryGetValue(
                        properties,
                        CFSTR("PerformanceStatistics")
                    );
                    
                    if (perfStats) {
                        CFNumberRef deviceUtil = (CFNumberRef)CFDictionaryGetValue(
                            perfStats,
                            CFSTR("Device Utilization %")
                        );
                        
                        if (deviceUtil) {
                            int util;
                            CFNumberGetValue(deviceUtil, kCFNumberIntType, &util);
                            gpuUsage = qMax(gpuUsage, util);
                        }
                    }
                }
                
                CFRelease(properties);
            }
            
            IOObjectRelease(service);
        }
        
        IOObjectRelease(iterator);
        return qBound(0, gpuUsage, 100);
#else
        // For non-macOS platforms, return 0 or implement platform-specific code
        return 0;
#endif
    }

    QString getGPUInfo() {
#ifdef Q_OS_MACOS
        io_iterator_t iterator;
        kern_return_t result = IOServiceGetMatchingServices(
            kIOMasterPortDefault,
            IOServiceMatching("AGXAccelerator"),
            &iterator
        );

        if (result != kIOReturnSuccess) {
            result = IOServiceGetMatchingServices(
                kIOMasterPortDefault,
                IOServiceMatching("IOAccelerator"),
                &iterator
            );
            if (result != kIOReturnSuccess) {
                return "GPU: N/A";
            }
        }

        io_registry_entry_t service;
        QString gpuInfo = "GPU: ";
        
        while ((service = IOIteratorNext(iterator))) {
            CFMutableDictionaryRef properties = NULL;
            result = IORegistryEntryCreateCFProperties(
                service,
                &properties,
                kCFAllocatorDefault,
                kNilOptions
            );

            if (result == kIOReturnSuccess && properties) {
                // Try to get model name
                CFStringRef model = (CFStringRef)CFDictionaryGetValue(
                    properties,
                    CFSTR("model")
                );
                
                if (!model) {
                    model = (CFStringRef)CFDictionaryGetValue(
                        properties,
                        CFSTR("IOClass")
                    );
                }
                
                if (model) {
                    char modelStr[256];
                    CFStringGetCString(model, modelStr, sizeof(modelStr), kCFStringEncodingUTF8);
                    gpuInfo = QString("GPU: %1").arg(modelStr);
                }
                
                CFRelease(properties);
            }
            
            IOObjectRelease(service);
            break; // Just get first GPU
        }
        
        IOObjectRelease(iterator);
        return gpuInfo;
#else
        return "GPU: N/A";
#endif
    }

    QStringList getProcessList() {
        QStringList processes;

#ifdef Q_OS_WIN
        QProcess process;
        process.start("tasklist", QStringList());

        if (!process.waitForFinished(5000)) {
            processes << "Error: Process list timeout";
            return processes;
        }

        QString output = process.readAllStandardOutput();
        QStringList lines = output.split('\n');

        for (const QString& line : lines) {
            if (line.trimmed().isEmpty()) continue;
            processes << line;
            if (processes.size() >= 200) break;
        }
#endif

#ifdef Q_OS_MACOS
        // Get number of processes
        int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_ALL, 0};
        size_t size;
        
        if (sysctl(mib, 4, NULL, &size, NULL, 0) < 0) {
            processes << "Error: Could not get process count";
            return processes;
        }

        // Allocate buffer for process info
        struct kinfo_proc *procs = (struct kinfo_proc *)malloc(size);
        if (!procs) {
            processes << "Error: Memory allocation failed";
            return processes;
        }

        if (sysctl(mib, 4, procs, &size, NULL, 0) < 0) {
            free(procs);
            processes << "Error: Could not get process list";
            return processes;
        }

        int proc_count = size / sizeof(struct kinfo_proc);

        // Structure to hold process info with CPU usage
        struct ProcessInfo {
            pid_t pid;
            pid_t ppid;
            uid_t uid;
            QString name;
            QString status;
            double cpuPercent;
        };
        
        QVector<ProcessInfo> procInfos;

        // Process each entry
        for (int i = 0; i < proc_count; i++) {
            pid_t pid = procs[i].kp_proc.p_pid;
            
            // Skip zombie processes
            if (procs[i].kp_proc.p_stat == SZOMB) {
                continue;
            }

            ProcessInfo info;
            info.pid = pid;
            info.ppid = procs[i].kp_eproc.e_ppid;
            info.uid = procs[i].kp_eproc.e_ucred.cr_uid;
            
            // Get process name
            char pathbuf[PROC_PIDPATHINFO_MAXSIZE];
            char name[PROC_PIDPATHINFO_MAXSIZE];
            
            if (proc_pidpath(pid, pathbuf, sizeof(pathbuf)) > 0) {
                const char *slash = strrchr(pathbuf, '/');
                strncpy(name, slash ? slash + 1 : pathbuf, sizeof(name) - 1);
                name[sizeof(name) - 1] = '\0';
            } else {
                strncpy(name, procs[i].kp_proc.p_comm, sizeof(name) - 1);
                name[sizeof(name) - 1] = '\0';
            }
            info.name = QString(name);

            // Get process state
            switch (procs[i].kp_proc.p_stat) {
                case SIDL:   info.status = "IDLE";   break;
                case SRUN:   info.status = "RUN";    break;
                case SSLEEP: info.status = "SLEEP";  break;
                case SSTOP:  info.status = "STOP";   break;
                case SZOMB:  info.status = "ZOMBIE"; break;
                default:     info.status = "?";      break;
            }

            // Get CPU usage for process
            struct proc_taskinfo taskInfo;
            int ret = proc_pidinfo(pid, PROC_PIDTASKINFO, 0, &taskInfo, sizeof(taskInfo));
            if (ret == sizeof(taskInfo)) {
                // Calculate CPU percentage (simplified)
                info.cpuPercent = (taskInfo.pti_total_user + taskInfo.pti_total_system) / 1000000.0;
            } else {
                info.cpuPercent = 0.0;
            }

            procInfos.append(info);
        }

        free(procs);

        // Sort by CPU usage (highest first)
        std::sort(procInfos.begin(), procInfos.end(),
                  [](const ProcessInfo& a, const ProcessInfo& b) {
                      return a.cpuPercent > b.cpuPercent;
                  });

        // Add header
        processes << QString("%-8s %-8s %-6s %-7s %-6s %-30s")
                     .arg("PID")
                     .arg("PPID")
                     .arg("UID")
                     .arg("CPU%")
                     .arg("STATUS")
                     .arg("NAME");
        processes << QString("-").repeated(75);

        // Add sorted processes
        for (int i = 0; i < procInfos.size() && processes.size() < 200; i++) {
            const ProcessInfo& info = procInfos[i];
            
            QString line = QString("%-8d %-8d %-6d %-7.1f %-6s %s")
                          .arg(info.pid)
                          .arg(info.ppid)
                          .arg(info.uid)
                          .arg(info.cpuPercent)
                          .arg(info.status)
                          .arg(info.name);
            
            processes << line;
        }

        processes << "";
        processes << QString("Total processes: %1 (showing %2, sorted by CPU usage)")
                     .arg(proc_count)
                     .arg(qMin(procInfos.size(), 200));
#endif

#ifdef Q_OS_LINUX
        QProcess process;
        process.start("/bin/ps", QStringList() << "aux");

        if (!process.waitForFinished(5000)) {
            processes << "Error: Process list timeout";
            return processes;
        }

        QString output = process.readAllStandardOutput();
        QStringList lines = output.split('\n');

        for (const QString& line : lines) {
            if (line.trimmed().isEmpty()) continue;

            // Filter out kernel threads
            if (line.contains("[kworker") || line.contains("[migration") ||
                line.contains("[ksoftirqd") || line.contains("[rcu_") ||
                line.contains("[watchdog") || line.contains("[cpuhp") ||
                line.contains("[kthreadd")) {
                continue;
            }
            
            processes << line;
            if (processes.size() >= 200) break;
        }
#endif

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
    QLabel *gpuLabel;
    QLabel *gpuPercentLabel;
    QLabel *gpuInfoLabel;
    QProgressBar *gpuProgressBar;
    CPUGraphWidget *cpuGraph;
    GPUGraphWidget *gpuGraph;
    
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

        // GPU Usage section
        QWidget *gpuWidget = new QWidget();
        gpuWidget->setStyleSheet("QWidget { background-color: #f0f0f0; border-radius: 5px; padding: 15px; }");
        QVBoxLayout *gpuLayout = new QVBoxLayout(gpuWidget);
        
        QLabel *gpuTitle = new QLabel("GPU Usage");
        gpuTitle->setStyleSheet("font-size: 16px; font-weight: bold; color: #333;");
        gpuLayout->addWidget(gpuTitle);
        
        QHBoxLayout *gpuInfoLayout = new QHBoxLayout();
        gpuLabel = new QLabel("Current GPU Usage:");
        gpuLabel->setStyleSheet("font-size: 12px;");
        gpuPercentLabel = new QLabel("0%");
        gpuPercentLabel->setStyleSheet("font-size: 14px; font-weight: bold; color: #0066cc;");
        gpuInfoLayout->addWidget(gpuLabel);
        gpuInfoLayout->addWidget(gpuPercentLabel);
        gpuInfoLayout->addStretch();
        gpuLayout->addLayout(gpuInfoLayout);
        
        gpuProgressBar = new QProgressBar();
        gpuProgressBar->setMinimum(0);
        gpuProgressBar->setMaximum(100);
        gpuProgressBar->setTextVisible(false);
        gpuProgressBar->setStyleSheet(R"(
            QProgressBar {
                border: 2px solid #ccc;
                border-radius: 5px;
                text-align: center;
                height: 25px;
            }
            QProgressBar::chunk {
                background-color: #FF9800;
                border-radius: 3px;
            }
        )");
        gpuLayout->addWidget(gpuProgressBar);
        
        gpuInfoLabel = new QLabel("GPU: N/A");
        gpuInfoLabel->setStyleSheet("font-size: 11px; color: #666; margin-top: 5px;");
        gpuLayout->addWidget(gpuInfoLabel);
        
        layout->addWidget(gpuWidget);

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

        // GPU History Graph
        QWidget *gpuGraphWidget = new QWidget();
        gpuGraphWidget->setStyleSheet("QWidget { background-color: #f0f0f0; border-radius: 5px; padding: 15px; }");
        QVBoxLayout *gpuGraphLayout = new QVBoxLayout(gpuGraphWidget);
        
        QLabel *gpuGraphTitle = new QLabel("GPU History Graph");
        gpuGraphTitle->setStyleSheet("font-size: 16px; font-weight: bold; color: #333;");
        gpuGraphLayout->addWidget(gpuGraphTitle);
        
        gpuGraph = new GPUGraphWidget();
        gpuGraphLayout->addWidget(gpuGraph);
        
        layout->addWidget(gpuGraphWidget);

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
        int gpuUsage = monitor->getGPUUsage();
        QString memStats = monitor->getMemoryStats();
        QString gpuInfo = monitor->getGPUInfo();

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

        gpuPercentLabel->setText(QString("%1%").arg(gpuUsage));
        gpuProgressBar->setValue(gpuUsage);
        gpuInfoLabel->setText(gpuInfo);
        
        QString gpuColor = gpuUsage > 75 ? "#f44336" : (gpuUsage > 50 ? "#ff9800" : "#FF9800");
        gpuProgressBar->setStyleSheet(QString(R"(
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
        )").arg(gpuColor));

        cpuGraph->addCPUValue(cpuUsage);
        gpuGraph->addGPUValue(gpuUsage);
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
