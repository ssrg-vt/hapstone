/* An attempt to map all struct sizes to account for the shitty C11 support
 * C2HS exposes. We just print them in a structured fashion to account for
 * the anonymous unions present in most of the code. That way, currently
 * broken Storable instances can be fixed. - This is what happens when you
 * start writing proper property-based tests ;)
 */

#include <stdio.h>
#include <stdbool.h>

#include <capstone/x86.h>

// print all struct sizes
void print_sizes(void){
    puts("x86.h");
    printf("[*] x86_op_mem: %d\n", sizeof(x86_op_mem));
    printf("[*] cs_x86_op: %d\n", sizeof(cs_x86_op));
    printf("[*] cs_x86: %d\n", sizeof(cs_x86));
}

// print all offsets and member sizes
void print_alignment(void){
    cs_x86_op test = {0, 1};
    void *base = &test;

    printf("cs_x86_op: %d\n", sizeof(cs_x86_op));
    puts("cs_x86_op\toffset\tsize");
    printf("type:\t\t%d\t%d\n", (void *)&test.type - base,
            sizeof(x86_op_type));
    printf("union.reg:\t%d\t%d\n", (void *)&test.reg - base,
            sizeof(x86_reg));
    printf("union.imm:\t%d\t%d\n", (void *)&test.imm - base,
            sizeof(int64_t));
    printf("union.fp:\t%d\t%d\n", (void *)&test.fp - base,
            sizeof(double));
    printf("union.mem:\t%d\t%d\n", (void *)&test.mem - base,
            sizeof(x86_op_mem));
    printf("size:\t\t%d\t%d\n", (void *)&test.size - base,
            sizeof(uint8_t));
    printf("avx_bcast:\t%d\t%d\n", (void *)&test.avx_bcast - base,
            sizeof(x86_avx_bcast));
    printf("avx_zero_opmask:%d\t%d\n", (void *)&test.avx_zero_opmask - base,
            sizeof(bool));
}

int main(void){
    print_sizes();
    print_alignment();

    return 0;
}
