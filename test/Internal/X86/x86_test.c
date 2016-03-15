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
    cs_x86_op test_1 = {0, 1};
    cs_x86 test_2 = {0};
    void *base = &test_1;

    printf("cs_x86_op: %d\n", sizeof(cs_x86_op));
    puts("cs_x86_op\toffset\tsize");
    printf("type:\t\t%d\t%d\n", (void *)&test_1.type - base,
            sizeof(x86_op_type));
    printf("union.reg:\t%d\t%d\n", (void *)&test_1.reg - base,
            sizeof(x86_reg));
    printf("union.imm:\t%d\t%d\n", (void *)&test_1.imm - base,
            sizeof(int64_t));
    printf("union.fp:\t%d\t%d\n", (void *)&test_1.fp - base,
            sizeof(double));
    printf("union.mem:\t%d\t%d\n", (void *)&test_1.mem - base,
            sizeof(x86_op_mem));
    printf("size:\t\t%d\t%d\n", (void *)&test_1.size - base,
            sizeof(uint8_t));
    printf("avx_bcast:\t%d\t%d\n", (void *)&test_1.avx_bcast - base,
            sizeof(x86_avx_bcast));
    printf("avx_zero_opmask:%d\t%d\n\n",
            (void *)&test_1.avx_zero_opmask - base, sizeof(bool));

    base = &test_2;

    printf("cs_x86:\t%d\n", sizeof(cs_x86));
    puts("cs_x86\t\toffset\tsize");
    printf("prefix:\t\t%d\t%d\n", (void *)&test_2.prefix - base,
            4*sizeof(uint8_t));
    printf("opcode:\t\t%d\t%d\n", (void *)&test_2.opcode - base,
            4*sizeof(uint8_t));
    printf("rex:\t\t%d\t%d\n", (void *)&test_2.rex - base,
            sizeof(uint8_t));
    printf("addr_size:\t%d\t%d\n", (void *)&test_2.addr_size - base,
            sizeof(uint8_t));
    printf("modrm:\t\t%d\t%d\n", (void *)&test_2.modrm - base,
            sizeof(uint8_t));
    printf("sib:\t\t%d\t%d\n", (void *)&test_2.sib - base,
            sizeof(uint8_t));
    printf("disp:\t\t%d\t%d\n", (void *)&test_2.disp - base,
            sizeof(int32_t));
    printf("sib_index:\t%d\t%d\n", (void *)&test_2.sib_index - base,
            sizeof(x86_reg));
    printf("sib_scale:\t%d\t%d\n", (void *)&test_2.sib_scale - base,
            sizeof(int8_t));
    printf("sib_base:\t%d\t%d\n", (void *)&test_2.sib_base - base,
            sizeof(x86_reg));
    printf("sse_cc:\t\t%d\t%d\n", (void *)&test_2.sse_cc - base,
            sizeof(x86_sse_cc));
    printf("avx_cc:\t\t%d\t%d\n", (void *)&test_2.avx_cc - base,
            sizeof(x86_avx_cc));
    printf("avx_sae:\t%d\t%d\n", (void *)&test_2.avx_sae - base,
            sizeof(bool));
    printf("avx_rm:\t\t%d\t%d\n", (void *)&test_2.avx_rm - base,
            sizeof(x86_avx_rm));
    printf("op_count:\t%d\t%d\n", (void *)&test_2.op_count - base,
            sizeof(uint8_t));
    printf("operands:\t%d\t%d\n", (void *)&test_2.operands - base,
            8*sizeof(cs_x86_op));
}

int main(void){
    print_sizes();
    print_alignment();

    return 0;
}
