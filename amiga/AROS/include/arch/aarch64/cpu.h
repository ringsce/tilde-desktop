#ifndef AROS_CPU_H
#define AROS_CPU_H

#include <stdint.h>  // For standard integer types
#include <stddef.h>  // For standard definitions

/* CPU Architecture Specific Definitions */
#define CPU_ARCH "aarch64"
#define CPU_VENDOR "Apple Silicon"
#define CPU_MODEL "M1/M2/M3"

#ifdef __APPLE__
#include <sys/sysctl.h>

/* MacOS Specific CPU Information Retrieval */
static inline const char *get_cpu_name() {
    static char cpu_name[256];
    size_t size = sizeof(cpu_name);
    if (sysctlbyname("machdep.cpu.brand_string", &cpu_name, &size, NULL, 0) == 0) {
        return cpu_name;
    }
    return "Unknown ARM CPU";
}
#endif

/* CPU Feature Flags */
#define CPU_HAS_NEON 1
#define CPU_HAS_SVE  0  // Update if Scalable Vector Extensions are supported

/* CPU Cache Line Size */
#define CPU_CACHE_LINE_SIZE 64

/* Synchronization Primitives */
static inline void cpu_memory_barrier() {
    __asm__ __volatile__("dmb sy" ::: "memory");
}

static inline void cpu_data_sync_barrier() {
    __asm__ __volatile__("dsb sy" ::: "memory");
}

static inline void cpu_instruction_sync_barrier() {
    __asm__ __volatile__("isb" ::: "memory");
}

/* CPU Core Count */
static inline int get_cpu_core_count() {
#ifdef __APPLE__
    int core_count = 1;
    size_t size = sizeof(core_count);
    sysctlbyname("hw.ncpu", &core_count, &size, NULL, 0);
    return core_count;
#else
    return 1;  // Default to 1 if unknown
#endif
}

#endif /* AROS_CPU_H */
