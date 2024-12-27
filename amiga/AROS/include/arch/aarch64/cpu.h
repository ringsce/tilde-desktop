#ifndef AROS_CPU_H
#define AROS_CPU_H

#include <stdint.h>  // For standard integer types
#include <stddef.h>  // For standard definitions

/* CPU Architecture Specific Definitions */
#define CPU_ARCH "aarch64"
#define CPU_VENDOR "Apple Silicon"
#define CPU_MODEL "M1/M2/M3"


#define __ARM64_ARCH_8__ 1
#define __ARM_64BIT_STATE 1
#define __ARM_ACLE 200
#define __ARM_ALIGN_MAX_STACK_PWR 4
#define __ARM_ARCH 8
#define __ARM_ARCH_8_3__ 1
#define __ARM_ARCH_8_4__ 1
#define __ARM_ARCH_8_5__ 1
#define __ARM_ARCH_ISA_A64 1
#define __ARM_ARCH_PROFILE 'A'
#define __ARM_FEATURE_AES 1
#define __ARM_FEATURE_ATOMICS 1
#define __ARM_FEATURE_BTI 1
#define __ARM_FEATURE_CLZ 1
#define __ARM_FEATURE_COMPLEX 1
#define __ARM_FEATURE_CRC32 1
#define __ARM_FEATURE_CRYPTO 1
#define __ARM_FEATURE_DIRECTED_ROUNDING 1
#define __ARM_FEATURE_DIV 1
#define __ARM_FEATURE_DOTPROD 1
#define __ARM_FEATURE_FMA 1
#define __ARM_FEATURE_FP16_FML 1
#define __ARM_FEATURE_FP16_SCALAR_ARITHMETIC 1
#define __ARM_FEATURE_FP16_VECTOR_ARITHMETIC 1
#define __ARM_FEATURE_FRINT 1
#define __ARM_FEATURE_IDIV 1
#define __ARM_FEATURE_JCVT 1
#define __ARM_FEATURE_LDREX 0xF
#define __ARM_FEATURE_NUMERIC_MAXMIN 1
#define __ARM_FEATURE_PAUTH 1
#define __ARM_FEATURE_QRDMX 1
#define __ARM_FEATURE_RCPC 1
#define __ARM_FEATURE_SHA2 1
#define __ARM_FEATURE_SHA3 1
#define __ARM_FEATURE_SHA512 1
#define __ARM_FEATURE_UNALIGNED 1
#define __ARM_FP 0xE
#define __ARM_FP16_ARGS 1
#define __ARM_FP16_FORMAT_IEEE 1
#define __ARM_NEON 1
#define __ARM_NEON_FP 0xE
#define __ARM_NEON__ 1
#define __ARM_PCS_AAPCS64 1
#define __ARM_SIZEOF_MINIMAL_ENUM 4
#define __ARM_SIZEOF_WCHAR_T 4
#define __arm64 1
#define __arm64__ 1


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
