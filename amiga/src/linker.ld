ENTRY(main)

SECTIONS
{
    . = 0x80000000; /* Base address for the kernel */

    .text : {
        *(.text*)
    }

    .data : {
        *(.data*)
    }

    .bss : {
        *(.bss*)
    }
}
