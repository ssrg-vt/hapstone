/* An attempt to map all struct sizes to account for the shitty C11 support
 * C2HS exposes. We just print them in a structured fashion to account for
 * the anonymous unions present in most of the code. That way, currently
 * broken Storable instances can be fixed. - This is what happens when you
 * start writing proper property-based tests ;)
 */

#include <stdio.h>

#include <capstone/arm64.h>

// print all struct sizes
void print_sizes(void){
    puts("arm64.h");
    printf("[*] arm64_op_mem: %d\n", sizeof(arm64_op_mem));
    printf("[*] cs_arm64_op: %d\n", sizeof(cs_arm64_op));
    printf("[*] cs_arm64: %d\n", sizeof(cs_arm64));
}

// print all offsets and member sizes
void print_alignment(void){
    cs_arm64_op test = {0, 1, 2, {3, 4}, 5, 6, 7};
    void *base = &test;

    printf("cs_arm64_op: %d\n", sizeof(cs_arm64_op));
    puts("cs_arm64_op\toffset\tsize");
    printf("vector_index:\t%d\t%d\n", (void *)&test.vector_index - base,
            sizeof(int));
    printf("vas:\t\t%d\t%d\n", (void *)&test.vas - base,
            sizeof(arm64_vas));
    printf("vess:\t\t%d\t%d\n", (void *)&test.vess - base,
            sizeof(arm64_vess));
    printf("shift.type:\t%d\t%d\n", (void *)&test.shift.type - base,
            sizeof(arm64_shifter));
    printf("shift.value:\t%d\t%d\n", (void *)&test.shift.value - base,
            sizeof(unsigned int));
    printf("ext:\t\t%d\t%d\n", (void *)&test.ext - base,
            sizeof(arm64_extender));
    printf("type:\t\t%d\t%d\n", (void *)&test.type - base,
            sizeof(arm64_op_type));
    printf("union.reg:\t%d\t%d\n", (void *)&test.reg - base,
            sizeof(unsigned int));
    printf("union.imm:\t%d\t%d\n", (void *)&test.imm - base,
            sizeof(int64_t));
    printf("union.fp:\t%d\t%d\n", (void *)&test.fp - base,
            sizeof(double));
    printf("union.mem:\t%d\t%d\n", (void *)&test.mem - base,
            sizeof(arm64_op_mem));
    printf("union.pstate:\t%d\t%d\n", (void *)&test.pstate - base,
            sizeof(arm64_pstate));
    printf("union.sys:\t%d\t%d\n", (void *)&test.sys - base,
            sizeof(unsigned int));
    printf("union.prefetch:\t%d\t%d\n", (void *)&test.prefetch - base,
            sizeof(arm64_prefetch_op));
    printf("union.barrier:\t%d\t%d\n", (void *)&test.barrier - base,
            sizeof(arm64_barrier_op));
    printf("access:\t%d\t%d\n", (void *)&test.access - base,
            sizeof(uint8_t));
}

int main(void){
    print_sizes();
    print_alignment();

    return 0;
}
